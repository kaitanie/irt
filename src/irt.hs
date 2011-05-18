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
  (_, Just gitOutputStream, _, _) <- createProcess (proc "git" ["describe"]) {std_out = CreatePipe }
  gitRevision <- hGetContents gitOutputStream
  return gitRevision

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
