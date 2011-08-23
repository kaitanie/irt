module SedRunner (
  useG4Types
  ) where

import System.IO
import System.Process
import System.Exit

data SedCommand = SedIntToG4Int
                | SedFloatToG4Float
                | SedDoubleToG4Double
                | SedBoolToG4Bool
                deriving (Show, Eq)

sedCommandArgs :: SedCommand -> [String]
sedCommandArgs SedIntToG4Int = ["s/int/G4int/g"]
sedCommandArgs SedFloatToG4Float = ["s/float/G4float/g"]
sedCommandArgs SedDoubleToG4Double = ["s/double/G4double/g"]
sedCommandArgs SedBoolToG4Bool = ["s/bool/G4bool/g"]

useG4Int :: String -> IO String
useG4Int = runSed SedIntToG4Int

useG4Float :: String -> IO String
useG4Float = runSed SedFloatToG4Float

useG4Double :: String -> IO String
useG4Double = runSed SedDoubleToG4Double

useG4Bool :: String -> IO String
useG4Bool = runSed SedBoolToG4Bool

useG4Types :: String -> IO String
useG4Types code = (useG4Int code) >>= useG4Float >>= useG4Double >>= useG4Bool

runSed :: SedCommand -> String -> IO String
runSed command inputData = do
  let sedArgs = sedCommandArgs command
      inPipe = CreatePipe
      outPipe = CreatePipe
      errPipe = CreatePipe
  (Just hInput, Just hOutput, Just hError, procHandle) <- createProcess (proc "sed" sedArgs) {std_in = inPipe, std_out = outPipe, std_err = errPipe}
  hPutStr hInput inputData
  hClose hInput
  exitCode <- waitForProcess procHandle
  outputStr <- hGetContents hOutput
  return outputStr

