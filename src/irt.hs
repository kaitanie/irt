module Main where
import System.Environment (getArgs)
import System.IO
import System.Process

data SourceConfig = SourceConfig { rootDir :: FilePath }

data SourceTransform = FileTransform { transformFileName :: FilePath
                                     , transformFileContents :: [String]
                                     }
                     | TreeTransform { transformTreeDirectory :: FilePath
                                     }
                       deriving (Show, Eq)

getGitRevisionInfo = do
  (_, Just gitDescribeOutputStream, _, _) <- createProcess (proc "git" ["describe", "--tags"]) {std_out = CreatePipe }
  createProcess (proc "git" ["update-index", "-q", "--refresh"])
  (_, Just gitDiffIndexStream, _, _) <-createProcess (proc "git" ["diff-index", "--name-only", "HEAD", "--"]) {std_out = CreatePipe }
  gitRevision <- hGetContents gitDescribeOutputStream
  gitDiffIndex <- hGetContents gitDiffIndexStream
  let treeIsDirty = length (lines gitDiffIndex) > 0
      trimmedRevision = init gitRevision
  if treeIsDirty
    then return (trimmedRevision ++ "-dirty")
    else return trimmedRevision

parseConfig = undefined

performCommand = undefined

usage = do
  rev <- getGitRevisionInfo
  putStrLn ("usage: irt version " ++ rev ++ " command [args]")
  putStrLn "Commands: help, release"

main = do
  args <- getArgs
  if length args <= 0
    then usage
    else do let config = parseConfig args
            performCommand config
