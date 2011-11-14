{-# LANGUAGE RankNTypes, KindSignatures #-}
module G4Release (
  G4Module(..),
  mkModuleDefinition,
  toCMakeSources,
  releaseG4
  ) where

import System.FilePath
import System.Directory
import FindFile
import GitRunner
import SedRunner

data G4Module = G4Module {
  g4moduleName :: String,
  g4moduleHeaders :: [FilePath],
  g4moduleSources :: [FilePath],
  g4moduleGranularDependencies :: [String],
  g4moduleGlobalDependencies :: [String]
  } deriving (Show, Eq)

-- identityTransform :: String -> IO String
-- identityTransform code = do
--   return code

releaseG4 :: GitRepo -> FilePath -> [G4Module] -> IO ()
releaseG4 repo targetdir modules = do
--  let transformFn code = (useG4Types code) >>= (appendRevisionInfo repo) >>= (appendLicense licenseBoilerplate)
  let transformFn code = (useG4Types code) >>= disableAssertions >>= appendDefines >>= (appendRevisionInfo repo) >>= (appendLicense licenseBoilerplate)
--  let transformFn code = (appendDefines code) >>= (appendRevisionInfo repo) >>= (appendLicense licenseBoilerplate)
      releaseFn = releaseModule targetdir transformFn
  mapM_ releaseFn modules

-- Apply transform (String -> IO String) to a code file:
releaseFile :: FilePath -> (String -> IO String) -> FilePath -> IO ()
releaseFile destinationDir transform file = do
  code <- readFile file
  code' <- transform code
  let fileName = takeFileName file
      targetFileName = destinationDir </> fileName
  writeFile targetFileName code'

releaseModule :: FilePath -> (String -> IO String) -> G4Module -> IO ()
releaseModule targetRootDir transform g4module = do
  createDirectoryIfMissing True targetRootDir
  let modName = g4moduleName g4module
      headers = g4moduleHeaders g4module      
      sources = g4moduleSources g4module      
  createDirectoryIfMissing True (targetRootDir </> modName </> "include")
  createDirectoryIfMissing True (targetRootDir </> modName </> "src")
--  createGNUmakefile targetRootDir g4module
--  createCMakeFile targetRootDir g4module
--  createCMakeSources targetRootDir g4module
  mapM_ (releaseFile (targetRootDir </> modName </> "include") transform) headers
  mapM_ (releaseFile (targetRootDir </> modName </> "src") transform) sources

createCMakeSources :: forall t (m :: * -> *) a. Monad m => t -> G4Module -> a -> m a
createCMakeSources targetRootDir g4module = do
  let modname = g4moduleName g4module
      libname = "G4hadronic_inclxx" ++ modname
      cmakeSources = toCMakeSources g4module
  return

createGNUmakefile :: FilePath -> G4Module -> FilePath -> String -> IO ()
createGNUmakefile targetRootDir g4module = do
  let name = g4moduleName g4module
      fname = targetRootDir </> name </> "GNUmakefile"
      libraryName = "name := G4hadronic_inclxx_" ++ name ++ "\n"
      content = gnumakefileHeader ++ libraryName ++ gnumakefileBody
  writeFile 

-- Build system boilerplate

gnumakefileHeader :: String
gnumakefileHeader = "#-----------------------------------------------------------\n\
\# GNUmakefile for INCL++.  Pekka Kaitaniemi (26.08.2011).\n\
\# -----------------------------------------------------------\n"

gnumakefileBody :: String
gnumakefileBody = "CPPFLAGS += -I$(G4BASE)/global/management/include \\n\
\             -I$(G4BASE)/global/HEPRandom/include \\n\
\             -I$(G4BASE)/global/HEPGeometry/include \\n\
\             -I$(G4BASE)/track/include \\n\
\             -I$(G4BASE)/geometry/volumes/include \\n\
\             -I$(G4BASE)/geometry/management/include \\n\
\             -I$(G4BASE)/processes/management/include \\n\
\             -I$(G4BASE)/processes/hadronic/management/include/ \\n\
\             -I$(G4BASE)/processes/hadronic/util/include \\n\
\             -I$(G4BASE)/processes/hadronic/cross_sections/include/ \\n\
\             -I$(G4BASE)/particles/management/include \\n\
\             -I$(G4BASE)/particles/leptons/include \\n\
\             -I$(G4BASE)/particles/bosons/include \\n\
\             -I$(G4BASE)/particles/hadrons/mesons/include \\n\
\             -I$(G4BASE)/particles/hadrons/barions/include \\n\
\             -I$(G4BASE)/particles/hadrons/ions/include \\n\
\             -I$(G4BASE)/materials/include \\n\
\\n\
\/include $(G4INSTALL)/config/common.gmk\n"

-- Boilerplate to be added to each file

appendDefines :: String -> IO String
appendDefines code = do
  let codeText = lines code
      defines = ["#define INCLXX_IN_GEANT4_MODE 1\n", "#include \"globals.hh\"\n"]
      codeText' = concat [defines, codeText]
      code' = unlines codeText'
  return code'

appendLicense :: String -> String -> IO String
appendLicense license code = do
  let licenseText = lines license
      codeText = lines code
      combinedTexts = concat [licenseText, codeText]
      code' = unlines combinedTexts
  return code'

appendRevisionInfo :: GitRepo -> String -> IO String
appendRevisionInfo repo code = do
  revStr <- buildGitRevisionString repo
  let revText = lines $ "// INCL++ revision: " ++ revStr ++ "\n//\n"
      codeText = lines code
      combinedTexts = concat [revText, codeText]
      code' = unlines combinedTexts
  return code'

licenseBoilerplate :: String
licenseBoilerplate = "//\n\
\// ********************************************************************\n\
\// * License and Disclaimer                                           *\n\
\// *                                                                  *\n\
\// * The  Geant4 software  is  copyright of the Copyright Holders  of *\n\
\// * the Geant4 Collaboration.  It is provided  under  the terms  and *\n\
\// * conditions of the Geant4 Software License,  included in the file *\n\
\// * LICENSE and available at  http://cern.ch/geant4/license .  These *\n\
\// * include a list of copyright holders.                             *\n\
\// *                                                                  *\n\
\// * Neither the authors of this software system, nor their employing *\n\
\// * institutes,nor the agencies providing financial support for this *\n\
\// * work  make  any representation or  warranty, express or implied, *\n\
\// * regarding  this  software system or assume any liability for its *\n\
\// * use.  Please see the license in the file  LICENSE  and URL above *\n\
\// * for the full disclaimer and the limitation of liability.         *\n\
\// *                                                                  *\n\
\// * This  code  implementation is the result of  the  scientific and *\n\
\// * technical work of the GEANT4 collaboration.                      *\n\
\// * By using,  copying,  modifying or  distributing the software (or *\n\
\// * any work based  on the software)  you  agree  to acknowledge its *\n\
\// * use  in  resulting  scientific  publications,  and indicate your *\n\
\// * acceptance of all terms of the Geant4 Software license.          *\n\
\// ********************************************************************\n\
\//\n\
\// INCL++ intra-nuclear cascade model\n\
\// Pekka Kaitaniemi, CEA and Helsinki Institute of Physics\n\
\// Davide Mancusi, CEA\n\
\// Alain Boudard, CEA\n\
\// Sylvie Leray, CEA\n\
\// Joseph Cugnon, University of Liege\n\
\//\n"

-- G4 build system code generator

defaultGlobDeps :: [String]
defaultGlobDeps = ["G4geometry", "G4global", "G4materials",
                   "G4particles", "G4track"]

defaultGranDeps :: [String]
defaultGranDeps = ["G4baryons", "G4bosons", "G4geometrymng",
                   "G4globman", "G4hadronic_mgt", "G4hadronic_util",
                   "G4hadronic_xsect", "G4ions", "G4leptons",
                   "G4materials", "G4mesons", "G4partman",
                   "G4procman", "G4track", "G4volumes"]

mkModuleDefinition :: FilePath -> String -> [G4Module] -> IO G4Module
mkModuleDefinition basedir pkgname granularDeps = do
  let  name = pkgname
  moduleFiles <- getRecursiveContents (basedir </> pkgname)
  let headers = headerFilesOnly moduleFiles
      sources = sourceFilesOnly moduleFiles
      granDeps = concat [defaultGranDeps, (map g4moduleName granularDeps)]
      globDeps = defaultGlobDeps
      newModule = G4Module name headers sources granDeps globDeps
  return newModule

indentation :: String
indentation = "   "

concatStrsToLines :: String -> String -> String
concatStrsToLines "" s = indentation ++ s
concatStrsToLines acc s = acc ++ "\n" ++ indentation ++ s

combineStrListToLines :: [String] -> String
combineStrListToLines l = foldl concatStrsToLines "" l

toCMakeSources :: G4Module -> String
toCMakeSources m = sourcesCMakeHeader ++ moduleDefStr
  where moduleDefStr = moduleNameDefStr ++ headerDefs ++ "\n" ++ srcDefs ++ "\n" ++ depDefs ++ moduleEndStr
        depDefs = granularDefs ++ "\n" ++ globalDefs
        granularDefs = combineStrListToLines $ g4moduleGranularDependencies m
        globalDefs = combineStrListToLines $ g4moduleGlobalDependencies m
        headerDefs = combineStrListToLines $ g4moduleHeaders m
        srcDefs = combineStrListToLines $ g4moduleSources m
        moduleName = g4moduleName m
        libName = "G4hadronic_inclxx" ++ moduleName
        moduleNameDefStr = "GEANT4_DEFINE_MODULE(NAME " ++ libName ++ "\n"
        moduleEndStr = "\n)\n"

sourcesCMakeHeader :: String
sourcesCMakeHeader = "#-----------------\
\-------------------------------------------------------------\n\
\# sources.cmake\n\
\# Module : G4hadronic_hetcpp_utils\n\
\# Package: Geant4.src.G4processes.G4hadronic.G4hadronic_models.G4had_hetcpp.G4hadronic_hetcpp_utils\n\
\#\n\
\# Sources description for a library.\n\
\# Lists the sources and headers of the code explicitely.\n\
\# Lists include paths needed.\n\
\# Lists the internal granular and global dependencies of the library.\n\
\# Source specific properties should be added at the end.\n\
\#\n\
\# Generated on : 24/9/2010\n\
\#\n\
\# $Id: sources.cmake,v 1.1 2010-09-29 18:57:01 bmorgan Exp $\n\
\#\n\
\#------------------------------------------------------------------------------\n\
\\n\
\# List external includes needed.\n\
\include_directories(${CLHEP_INCLUDE_DIRS})\n\
\\n\
\# List internal includes needed.\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/geometry/management/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/geometry/volumes/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/global/HEPGeometry/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/global/HEPRandom/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/global/management/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/materials/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/bosons/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/hadrons/barions/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/hadrons/ions/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/hadrons/mesons/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/leptons/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/particles/management/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/processes/hadronic/cross_sections/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/processes/hadronic/management/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/processes/hadronic/util/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/processes/management/include)\n\
\include_directories(${CMAKE_SOURCE_DIR}/source/track/include)\n\
\\n\
\#\n\
\# Define the Geant4 Module.\n\
\#\n\
\include(Geant4MacroDefineModule)\n"

