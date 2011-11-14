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
                | SedCommentAsserts
                | SedFixG4G4
                | SedFixUnsignedG4Int
                | SedFixG4boolalpha
                deriving (Show, Eq)

--toG4TypeRegexp t = ["\\'s/\\b\\(" ++ t ++"\\)\\b/G4\\1/g\'"]
toG4TypeRegexp :: String -> [String]
toG4TypeRegexp t = ["s/" ++ t ++ "/G4" ++ t ++ "/g"]

--let substitute x y s = subRegex (mkRegex x) s y
-- substitute "int" "G4int"

sedCommandArgs :: SedCommand -> [String]
sedCommandArgs SedIntToG4Int = ["s/int/G4int/g"]
--sedCommandArgs SedIntToG4Int = toG4TypeRegexp "int"
sedCommandArgs SedFloatToG4Float = toG4TypeRegexp "float"
sedCommandArgs SedDoubleToG4Double = toG4TypeRegexp "double"
sedCommandArgs SedBoolToG4Bool = toG4TypeRegexp "bool"
sedCommandArgs SedCommentAsserts = ["s/^ *assert/\\/\\/ assert/g"]
sedCommandArgs SedFixG4G4 = ["s/G4G4/G4/g"]
sedCommandArgs SedFixUnsignedG4Int = ["s/unsigned\\ G4int/unsigned\\ int/g"]
sedCommandArgs SedFixG4boolalpha = ["s/G4boolalpha/boolalpha/g"]

useG4Int :: String -> IO String
useG4Int = runSed SedIntToG4Int

useG4Float :: String -> IO String
useG4Float = runSed SedFloatToG4Float

useG4Double :: String -> IO String
useG4Double = runSed SedDoubleToG4Double

useG4Bool :: String -> IO String
useG4Bool = runSed SedBoolToG4Bool

commentAsserts :: String -> IO String
commentAsserts = runSed SedCommentAsserts

fixG4G4 :: String -> IO String
fixG4G4 = runSed SedFixG4G4

fixUnsignedG4int :: String -> IO String
fixUnsignedG4int = runSed SedFixUnsignedG4Int

fixG4boolalpha :: String -> IO String
fixG4boolalpha = runSed SedFixG4boolalpha

-- Chain the useG4<type> functions together.
useG4Types :: String -> IO String
--useG4Types code = (useG4Int code)
useG4Types code = (useG4Int code) >>= useG4Float >>= useG4Double >>= useG4Bool >>= fixG4G4 >>= fixUnsignedG4int >>= fixG4boolalpha

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
  if exitCode /= ExitSuccess
    then do msg <- hGetContents hError
            hClose hOutput
            error $ "Sed reported an error: " ++ msg
    else do outputStr <- hGetContents hOutput
            hClose hError
            return outputStr

