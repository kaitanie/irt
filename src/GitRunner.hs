module GitRunner ( GitRepo(..)
                 , GitCommand(..)
                 , gitIsDirtyTree
                 , buildGitRevisionString
                 , runGit
  ) where
import System.IO
import System.Process

data GitRepo = GitRepo { gitRepoPath :: FilePath
                       } deriving (Show, Eq)

data GitCommand = GitRevisionInfoCmd
                | GitUpdateIndexCmd
                | GitDiffIndexNamesCmd

gitCommandArgs GitUpdateIndexCmd = ["update-index", "-q", "--refresh"]
gitCommandArgs GitDiffIndexNamesCmd = ["diff-index", "--name-only", "HEAD", "--"]
gitCommandArgs GitRevisionInfoCmd = ["describe", "--tags"]

buildGitRevisionString :: GitRepo -> IO String
buildGitRevisionString repo = do
  isDirty <- gitIsDirtyTree repo
  revStr <- runGit repo GitRevisionInfoCmd
  if isDirty
    then return (revStr ++ "-dirty")
    else return revStr

gitIsDirtyTree :: GitRepo -> IO Bool
gitIsDirtyTree repo = do
  runGit repo GitUpdateIndexCmd
  output <- runGit repo GitDiffIndexNamesCmd
  let changedFiles = length output
  if changedFiles > 0
    then return True
    else return False

runGit :: GitRepo -> GitCommand -> IO String
runGit repo command = do
  let gitArgs = gitCommandArgs command
      repoDir = gitRepoPath repo
  (_, Just gitOutputStream, _, _) <- createProcess (proc "git" gitArgs) {std_out = CreatePipe,cwd = Just repoDir}
  gitOutputStr <- hGetContents gitOutputStream
  let trimmedOutput | length gitOutputStr > 0 = init gitOutputStr
                    | otherwise               = gitOutputStr
  return trimmedOutput
