import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Control.Monad (foldM, mzero)
import Data.Char (toUpper)
import Text.Printf (PrintfArg, printf)
import Data.List.Split (chunksOf)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer (tell, execWriterT)
import Data.Monoid (Sum(..))

type OutArrayLen = Integer

data InputSource = Stdin
                 | InFiles [String]

data Options = Options 
  { fileName :: String
  , varName :: String
  , source :: InputSource
  }

cFileName :: Options -> String
cFileName = (++".c") . fileName

hFileName :: Options -> String
hFileName = (++".h") . fileName

fileNameParser :: Parser String
fileNameParser = strOption
  (  short 'o'
  <> long "out"
  <> metavar "TARGET"
  <> help "Name of the output files (extensions .c and .h will be added)" 
  )

varNameParser :: Parser String
varNameParser = strOption
  (  short 'v'
  <> long "var"
  <> metavar "VAR_NAME"
  <> help "Name of the array variable" 
  )

sourceParser :: Parser InputSource
sourceParser = fileParser <|> stdinParser
  where fileParser :: Parser InputSource
        fileParser = InFiles <$> (some . strArgument $ metavar "FILES...")
        stdinParser :: Parser InputSource
        stdinParser = flag' Stdin
          (  long "stdin"
          <> help "Read from stdin"
          )

optionsParser :: Parser Options
optionsParser = Options
  <$> fileNameParser
  <*> varNameParser
  <*> sourceParser

main :: IO ()
main = b2c =<< execParser opts
  where opts = info (optionsParser <**> helper)
                    (fullDesc <> desc <> hdr)
        desc = progDesc " Create TARGET.c and TARGET.h containing an array \
                        \ named VAR_NAME consisting of the contents of \
                        \ FILES..."
        hdr  = header "b2c - converts input data into a C array"

b2c :: Options -> IO()
b2c opt = createCFile opt >>= createHFile opt

createCFile :: Options -> IO OutArrayLen
createCFile opt = withFile (cFileName opt) WriteMode (writeCFile opt)

writeCFile :: Options -> Handle -> IO OutArrayLen
writeCFile opt outh = do
  hPutStrLn outh $ "#include \"" ++ hFileName opt ++ "\""
  hPutStrLn outh $ ""
  hPutStrLn outh $ "const uint8_t " ++ varName opt ++ "[] = {"
  total <- writeArray outh (source opt)
  hPutStrLn outh $ "};"
  return total

writeArray :: Handle -> InputSource -> IO OutArrayLen
writeArray outh Stdin = writeBytes outh stdin
writeArray outh (InFiles fs) = foldM sumWrites 0 fs
  where sumWrites acc fname = (+acc) <$> writeFileBytes fname
        writeFileBytes fname = withFile fname ReadMode (writeFileArray outh fname)

writeFileArray :: Handle -> FilePath -> Handle -> IO OutArrayLen
writeFileArray outh fname inh = do
  hPutStrLn outh $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  length <- writeHandleBytes outh inh
  hPutStrLn outh $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return length

writeHandleBytes :: Handle -> Handle -> IO OutArrayLen
writeHandleBytes outh inh = do
  hSetBinaryMode inh True
  contents <- hGetContents inh
  writeArrayLines outh . chunksOf 16 $ contents

writeArrayLines :: Handle -> [String] -> IO OutArrayLen
writeArrayLines outh ls = do
  writes <- execWriterT . mapM_ writeLine $ ls
  return (getSum writes)
  where writeLine l = do
          tell $ Sum (toInteger . length $ l)
          liftIO . hPutStrLn outh . toLine $ l
        toLine = ("  "++) . concatMap formatByte
        formatByte = printf "0x%02X,"

createHFile :: Options -> OutArrayLen -> IO ()
createHFile opt arrlen = withFile (hFileName opt) WriteMode (writeHFile opt arrlen)

writeHFile :: Options -> OutArrayLen -> Handle -> IO ()
writeHFile opt arrlen outh = hPutStr outh . unlines $ hlines
  where vname = varName opt
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
