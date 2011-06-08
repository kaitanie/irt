{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NamedFieldPuns #-}
module Main where
import System.Environment (getArgs)
import System.IO
import System.Process
import System.Console.CmdArgs
import GitRunner

data CmdOptions = AboutIncl {
                            }
                | G4Release { gitRepositoryPath :: String
                            , ignoreGitModifications :: Bool
                            , geant4SourcePath :: String
                            }
                deriving (Show, Eq, Data, Typeable)

data SourceConfig = SourceConfig { rootDir :: FilePath }

data SourceTransform = FileTransform { transformFileName :: FilePath
                                     , transformFileContents :: [String]
                                     }
                     | TreeTransform { transformTreeDirectory :: FilePath
                                     }
                       deriving (Show, Eq)

parseConfig = undefined

performCommand = undefined

g4release = G4Release { gitRepositoryPath = def &= help "INCL++ Git repository path"
                      , ignoreGitModifications = False &= help "Ignore modifications in the Git tree"
                      , geant4SourcePath = def &= help "Geant4 checkout (main level)"
                      }  &= help "Release INCL++ to Geant4"

info = AboutIncl {} &= help "Help"

mode = cmdArgsMode $ modes [info,g4release] &= help "Make an INCL release" &= program "irt" &= summary "irt v0.1"

abortG4Release = do
  putStrLn "Error! Git tree must not contain uncommitted changes!"

performG4Release conf = undefined
--performG4Release conf = do
--  srcs <- collectF
--  srcs <- filterSource conf
  

runIrtCommand conf@(G4Release gitpath ignoregitmodif g4sourcepath) = do
  let inclRepository = GitRepo gitpath
  inclDirtyTree <- gitIsDirtyTree inclRepository
  inclRev <- buildGitRevisionString inclRepository
  putStrLn $ "INCL++ repository path is: " ++ gitpath
  putStrLn $ "INCL++ revision is: " ++ inclRev
  putStrLn $ "Geant4 path is: " ++ g4sourcepath
  if inclDirtyTree && (not ignoregitmodif)
    then abortG4Release
    else performG4Release conf

--runIrtCommand :: CmdOptions -> IO ()
--runIrtCommand G4Release{..} = undefined
--runIrtCommand _ = undefined

main = do
  conf <- cmdArgsRun mode
  runIrtCommand conf