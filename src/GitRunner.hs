module GitRunner (
  ) where
import System.IO
import System.Process

data GitRepo = GitRepo { gitRepoPath :: FilePath
                       } deriving (Show, Eq)

data GitCommand = GitRevisionInfo
                | GitIsDirtyTree

gitCommandArgs GitIsDirtyTree = ["diff-index", "--name-only", "HEAD", "--"]
gitCommandArgs GitRevisionInfo = ["describe", "--tags"]

runGit :: GitRepo -> GitCommand -> IO String
runGit repo command = do
  let gitArgs = gitCommandArgs command
  (_, Just gitOutputStream, _, _) <- createProcess (proc "git" gitArgs) { std_out = CreatePipe }
  gitOutputStr <- hGetContents gitOutputStream
  let trimmedOutput = init gitOutputStr
  return trimmedOutput
