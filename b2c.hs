import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit (exitFailure)
import Control.Monad (foldM, mzero)
import Data.Char (toUpper)
import Text.Printf (PrintfArg, printf)
import Data.List.Split (chunksOf)

type Error = String
type OutArrayLen = Integer

data Flag = File String
          | Var String
  deriving (Show)

data Config = Config { cfile :: FilePath, hfile :: FilePath, variable :: String, files :: [FilePath] }
  deriving (Show)

usage :: String -> String
usage name = "usage: " ++ name ++ " -o <filename> -v <varname> [files...]"

description :: String -> String
description name = name ++ " -- Converts input data into a C array."

usageError :: String -> IO ()
usageError e = do
  name <- getProgName
  let desc = description name
  let use  = usageInfo (usage name) arguments
  hPutStr stderr $ unlines [e, desc, use]
  exitFailure

arguments :: [OptDescr Flag]
arguments =
  [ Option ['o'] ["out"] (ReqArg File "filename") "name of the output files (extension .c and .h will be added)"
  , Option ['v'] ["var"] (ReqArg Var "varname"  ) "name of the variable in the output files"
  ]

main :: IO ()
main = do
  args <- readArgs
  case parseOptions args of
    Left err -> usageError err
    Right config -> runConfig config
  where readArgs = fmap (getOpt Permute arguments) getArgs

parseFileOpt :: [Flag] -> Either Error FilePath
parseFileOpt fs =
  case [n | File n <- fs] of 
    [n] -> Right n
    []  -> Left "No file name option provided."
    _   -> Left "Multiple file name options provided."

parseVarOpt :: [Flag] -> Either Error String
parseVarOpt fs =
  case [n | Var n <- fs] of
    [n] -> parseVarName n
    []  -> Left "No file name option provided."
    _   -> Left "Multiple file name options provided."
  where parseVarName "" = Left "The variable name cannot be blank."
        parseVarName n  = Right n

parseInputFiles :: [String] -> Either Error [String]
parseInputFiles [] = Left "No input files specified."
parseInputFiles ns =
  case filter (=="-") ns of
    []    -> Right ns
    ["-"] -> Right ns
    _     -> Left "STDIN '-' option can only be provided once."

handleOptErrors :: [String] -> Either Error ()
handleOptErrors [] = Right ()
handleOptErrors s  = Left . concat $ s

parseOptions :: ([Flag], [String], [String]) -> Either Error Config
parseOptions (opts, files, errs) = do
  handleOptErrors errs
  file <- parseFileOpt opts
  var <- parseVarOpt opts
  ins <- parseInputFiles files
  return $ buildConfig file var ins
  where buildConfig file var ins = Config {
            cfile = file ++ ".c"
          , hfile = file ++ ".h"
          , variable = var
          , files = ins
          }

runConfig :: Config -> IO ()
runConfig c = createCFile c >>= createHFile c

createCFile :: Config -> IO OutArrayLen
createCFile c = withFile (cfile c) WriteMode $ writeCFile c

writeCFile :: Config -> Handle -> IO OutArrayLen
writeCFile c outh = do
  hPutStrLn outh $ "#include \"" ++ hfile c ++ "\""
  hPutStrLn outh $ ""
  hPutStrLn outh $ "const uint8_t " ++ variable c ++ "[] = {"
  total <- writeArrayFromFiles outh $ files c
  hPutStrLn outh $ "};"
  return total

writeArrayFromFiles :: Handle -> [FilePath] -> IO OutArrayLen
writeArrayFromFiles outh = foldM (\acc fname -> (+acc) <$> withInFile fname (writeArrayBytes outh fname)) 0
  where withInFile "-" f = f stdin
        withInFile n   f = withFile n ReadMode f

writeArrayBytes :: Handle -> FilePath -> Handle -> IO OutArrayLen
writeArrayBytes outh fname inh = do
  hSetBinaryMode inh True
  length <- hFileSize inh
  contents <- hGetContents inh
  hPutStrLn outh $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  toArray contents
  hPutStrLn outh $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return length
  where toArray = mapM_ (hPutStrLn outh) . fmap toArrayLine . chunksOf 16
        formatByte = printf "0x%02X,"
        toArrayLine = ("  "++) . concatMap formatByte

createHFile :: Config -> OutArrayLen -> IO ()
createHFile c arrlen = withFile (hfile c) WriteMode $ writeHFile c arrlen

writeHFile :: Config -> OutArrayLen -> Handle -> IO ()
writeHFile c arrlen outh = hPutStr outh . unlines $ hlines
  where vname = variable c
        upvname = map toUpper vname
        vguard = upvname ++ "_H"
        vlength = upvname ++ "_LEN"
        hlines = [ "#ifndef " ++ vguard
                 , "#define " ++ vguard
                 , "#include <stdint.h>"
                 , ""
                 , "#define " ++ vlength ++ " (uint32_t)(" ++ show arrlen ++ "UL)"
                 , "extern const uint8_t " ++ vname ++ "["++ vlength ++ "];"
                 , ""
                 , "#endif /* " ++ vguard ++ " */"]
