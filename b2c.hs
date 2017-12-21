import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Data.Monoid
import Data.List
import Data.Char
import Text.Printf

type Error = String

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
  putError e
  putError $ description name
  putError $ usageInfo (usage name) arguments
  exitFailure
  where putError = hPutStrLn stderr

arguments :: [OptDescr Flag]
arguments =
  [ Option ['o'] ["out"] (ReqArg File "filename") "name of the output files (extension .c and .h will be added)"
  , Option ['v'] ["var"] (ReqArg Var "varname"  ) "name of the variable in the output files"
  ]

main :: IO ()
main = do
  args <- fmap (getOpt Permute arguments) getArgs
  case parseOptions args of
    Left err -> usageError err
    Right config -> processConfig config
  where readArgs = fmap (getOpt Permute arguments) getArgs

parseFileOpt :: [Flag] -> Either Error FilePath
parseFileOpt fs = case [n | File n <- fs] of 
  [n] -> Right n
  []  -> Left "No file name option provided."
  _   -> Left "Multiple file name options provided."

parseVarOpt :: [Flag] -> Either Error String
parseVarOpt fs = case [n | Var n <- fs] of
  [n] -> parseVarName n
  []  -> Left "No file name option provided."
  _   -> Left "Multiple file name options provided."
  where parseVarName "" = Left "The variable name cannot be blank."
        parseVarName n  = Right n

parseInputFiles :: [String] -> Either Error [String]
parseInputFiles [] = Left "No input files specified."
parseInputFiles ns = Right ns

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
  where buildConfig file var ins = Config { cfile = file ++ ".c", hfile = file ++ ".h", variable = var, files = ins }

processConfig :: Config -> IO ()
processConfig c = writeCFile c >> writeHFile c

writeCFile :: Config -> IO ()
writeCFile c =
  withFile (cfile c) WriteMode $ \outh -> do
    hPutStrLn outh $ "#include \"" ++ hfile c ++ "\""
    hPutStrLn outh $ ""
    hPutStrLn outh $ "const uint8_t " ++ variable c ++ "[] = {"
    processInputFiles outh $ files c
    hPutStrLn outh $ "};"

processInputFiles :: Handle -> [FilePath] -> IO ()
processInputFiles outh fs =
  forM_ fs $ \fname -> withFile fname ReadMode $ writeFileBytes outh

writeFileBytes :: Handle -> Handle -> IO ()
writeFileBytes outh inh = do
  eof <- hIsEOF inh
  unless eof $ do
    cs <- hRead 16 inh
    dumpLine cs
    writeFileBytes outh inh
    where dumpLine cs = do
            hPutStr outh "  "
            hPutStrLn outh . concatMap formatByte $ cs
          formatByte :: (PrintfArg a) => a -> String
          formatByte = printf "0x%02X, "

hTryGetChar :: Handle -> IO (Maybe Char)
hTryGetChar h = do
   eof <- hIsEOF h
   if eof then return mzero else Just <$> hGetChar h

hRead :: (Integral a) => a -> Handle -> IO String
hRead 0 _ = return []
hRead n handle = do
  mc <- hTryGetChar handle
  case mc of
    Nothing -> return []
    Just c  -> (c :) <$> hRead (n-1) handle

writeHFile :: Config -> IO ()
writeHFile c =
  withFile (hfile c) WriteMode $ \outh -> do
    hPutStrLn outh $ "#ifndef " ++ guardV
    hPutStrLn outh $ "#define " ++ guardV
    hPutStrLn outh $ "#include <stdint.h>"
    hPutStrLn outh $ ""
    -- TODO: Need to determine the output array length
    hPutStrLn outh $ "#define " ++ lengthV ++ " (uint32_t)(" ++ show 1000 ++ "UL)"
    hPutStrLn outh $ "extern const uint8_t " ++ variable c ++ "["++ lengthV ++ "];"
    hPutStrLn outh $ ""
    hPutStrLn outh $ "#endif /* " ++ guardV ++ " */"
      where upVname = map toUpper . variable $ c
            guardV = upVname ++ "_H"
            lengthV = upVname ++ "_LEN"
