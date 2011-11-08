module FindFile ( getRecursiveContents
                , filterFilesWithSuffix
                , headerFilesOnly
                , sourceFilesOnly
                ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension, takeFileName)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

filterFilesWithSuffix :: String -> [FilePath] -> [FilePath]
filterFilesWithSuffix suffix files = filter (\fname -> takeExtension fname == suffix) files

data ProjectFileType = HeaderFile
                     | SourceFile
                     deriving (Show, Eq)

fileExtension :: ProjectFileType -> String
fileExtension HeaderFile = ".hh"
fileExtension SourceFile = ".cc"

filterFilesByType :: ProjectFileType -> [FilePath] -> [FilePath]
filterFilesByType = filterFilesWithSuffix . fileExtension

headerFilesOnly :: [FilePath] -> [FilePath]
headerFilesOnly = filterFilesByType HeaderFile

sourceFilesOnly :: [FilePath] -> [FilePath]
sourceFilesOnly = filterFilesByType SourceFile