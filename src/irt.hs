{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.FilePath
import System.Console.CmdArgs
import GitRunner
import G4Release

data CmdOptions = AboutIrt {
                            }
                | G4Release { gitRepositoryPath :: String
                            , ignoreGitModifications :: Bool
                            , g4options :: [G4ReleaseOption]
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

g4release :: CmdOptions
g4release = G4Release { gitRepositoryPath = def &= help "INCL++ Git repository path"
                      , ignoreGitModifications = False &= help "Ignore modifications in the Git tree"
                      , geant4SourcePath = def &= help "Geant4 checkout (main level)"
                      , g4options = [] &= help "Geant4 release options"
                      }  &= help "Release INCL++ to Geant4"

info :: CmdOptions
info = AboutIrt {} &= help "Help"

mode :: Mode (CmdArgs CmdOptions)
mode = cmdArgsMode $ modes [info,g4release] &= help "Make an INCL release" &= program "irt" &= summary "irt v0.3"

abortG4Release :: IO ()
abortG4Release = do
  putStrLn "Error! Git tree must not contain uncommitted changes!"

performG4Release :: GitRepo -> FilePath -> [G4ReleaseOption] -> IO ()
performG4Release repo targetDir g4opts = do
  let inclDir = gitRepoPath repo
  g4inclxxUtilsModule <- mkModuleDefinition inclDir "utils" []
  g4inclxxPhysicsModule <- mkModuleDefinition inclDir "incl_physics" [g4inclxxUtilsModule]
  g4inclxxInterfaceModule <- mkModuleDefinition inclDir "interface" [g4inclxxUtilsModule, g4inclxxPhysicsModule]
  let g4modules = [g4inclxxUtilsModule, g4inclxxPhysicsModule, g4inclxxInterfaceModule]
  releaseG4 repo targetDir g4modules g4opts

runIrtCommand :: CmdOptions -> IO ()
runIrtCommand (G4Release gitpath ignoregitmodif g4opts g4sourcepath) = do
  let inclRepository = GitRepo gitpath
      targetDir = g4sourcepath </> "source/processes/hadronic/models/inclxx/"
  inclDirtyTree <- gitIsDirtyTree inclRepository
  inclRev <- buildGitRevisionString inclRepository
  putStrLn $ "INCL++ repository path is: " ++ gitpath
  putStrLn $ "INCL++ revision is: " ++ inclRev
  putStrLn $ "Geant4 path is: " ++ g4sourcepath
  putStrLn $ "G4 release options: " ++ (show g4opts)
  if inclDirtyTree && (not ignoregitmodif)
    then abortG4Release
    else performG4Release inclRepository targetDir g4opts

runIrtCommand AboutIrt = do
  putStrLn "About irt"

main :: IO ()
main = do
  conf <- cmdArgsRun mode
  runIrtCommand conf

