module GitRunner ( GitRepo(..)
                 , GitCommand(..)
                 , gitIsDirtyTree
                 , buildGitRevisionString
                 , runGit
  ) where
import System.IO
import System.Process
import System.Exit

data GitRepo = GitRepo { gitRepoPath :: FilePath
                       } deriving (Show, Eq)

data GitCommand = GitRevisionInfoCmd
                | GitUpdateIndexCmd
                | GitDiffIndexNamesCmd
                deriving (Show, Eq)

gitCommandArgs :: GitCommand -> [String]
gitCommandArgs GitUpdateIndexCmd = ["update-index", "-q", "--refresh"]
gitCommandArgs GitDiffIndexNamesCmd = ["diff-index", "--name-only", "HEAD", "--"]
gitCommandArgs GitRevisionInfoCmd = ["describe", "--tags"]

buildGitRevisionString :: GitRepo -> IO String
buildGitRevisionString repo = do
  isDirty <- gitIsDirtyTree repo
  (_, revStr) <- runGit repo GitRevisionInfoCmd
  if isDirty
    then return $ revStr ++ "-dirty"
    else return revStr

gitIsDirtyTree :: GitRepo -> IO Bool
gitIsDirtyTree repo = do
  (_, _) <- runGit repo GitUpdateIndexCmd
  (_, output) <- runGit repo GitDiffIndexNamesCmd
  let changedFiles = length output
  return $ changedFiles > 0

runGit :: GitRepo -> GitCommand -> IO (ExitCode, String)
runGit repo command = do
  let gitArgs = gitCommandArgs command
      repoDir = gitRepoPath repo
      out = CreatePipe
      err = CreatePipe
      d = Just repoDir
  (_, Just gitOutputStream, Just gitErrorStream, procHandle) <- createProcess (proc "git" gitArgs) {std_out = out, std_err = err,cwd = d}
  exitCode <- waitForProcess procHandle
  case exitCode of
    ExitSuccess -> do
      gitOutputStr <- hGetContents gitOutputStream
      let trimmedOutput | length gitOutputStr > 0 = init gitOutputStr
                        | otherwise               = gitOutputStr
      return (ExitSuccess, trimmedOutput)
    (ExitFailure errorCode) -> do
      gitErrorStr  <- hGetContents gitErrorStream
      let msg = "Git failed with code " ++ (show errorCode) ++ " and message:\n" ++ gitErrorStr
      return (exitCode, msg)
