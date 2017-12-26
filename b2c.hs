{-# LANGUAGE BangPatterns #-}
import           Control.Monad             (foldM, mapM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (execStateT, modify')
import           Data.Char                 (toUpper)
import           Data.List.Split           (chunksOf)
import           Data.Monoid               ((<>))
import qualified Options.Applicative       as OA
import qualified System.IO                 as IO
import           Text.Printf               (printf)

type OutArrayLen = Int

data InputSource = Stdin
                 | InFiles [String]

data Options = Options
  { fileName :: String
  , varName  :: String
  , source   :: InputSource
  }

cFileName :: Options -> String
cFileName = (++".c") . fileName

hFileName :: Options -> String
hFileName = (++".h") . fileName

fileNameParser :: OA.Parser String
fileNameParser = OA.strOption
  (  OA.short 'o'
  <> OA.long "out"
  <> OA.metavar "TARGET"
  <> OA.help "Name of the output files (extensions .c and .h will be added)"
  )

varNameParser :: OA.Parser String
varNameParser = OA.strOption
  (  OA.short 'v'
  <> OA.long "var"
  <> OA.metavar "VAR_NAME"
  <> OA.help "Name of the array variable"
  )

sourceParser :: OA.Parser InputSource
sourceParser = fileParser OA.<|> stdinParser
  where fileParser :: OA.Parser InputSource
        fileParser = InFiles <$> (OA.some . OA.strArgument $ OA.metavar "FILES...")
        stdinParser :: OA.Parser InputSource
        stdinParser = OA.flag' Stdin
          (  OA.long "stdin"
          <> OA.help "Read from stdin"
          )

optionsParser :: OA.Parser Options
optionsParser = Options
  <$> fileNameParser
  <*> varNameParser
  <*> sourceParser

main :: IO ()
main = b2c =<< OA.execParser opts
  where opts = OA.info (OA.helper <*> optionsParser) $
                        OA.fullDesc <> desc <> hdr
        desc = OA.progDesc " Create TARGET.c and TARGET.h containing an array \
                           \ named VAR_NAME consisting of the contents of \
                           \ FILES..."
        hdr  = OA.header "b2c - converts input data into a C array"

b2c :: Options -> IO()
b2c opt = createCFile opt >>= createHFile opt

createCFile :: Options -> IO OutArrayLen
createCFile opt = IO.withFile (cFileName opt) IO.WriteMode (writeCFile opt)

writeCFile :: Options -> IO.Handle -> IO OutArrayLen
writeCFile opt outh = do
  IO.hPutStrLn outh $ "#include \"" ++ hFileName opt ++ "\""
  IO.hPutStrLn outh $ ""
  IO.hPutStrLn outh $ "const uint8_t " ++ varName opt ++ "[] = {"
  total <- writeArray outh (source opt)
  IO.hPutStrLn outh $ "};"
  return total

writeArray :: IO.Handle -> InputSource -> IO OutArrayLen
writeArray outh Stdin = writeHandleBytes outh IO.stdin
writeArray outh (InFiles fs) = foldM sumWrites 0 fs
  where sumWrites !acc fname = (+acc) <$> writeFileBytes fname
        writeFileBytes fname = IO.withFile fname IO.ReadMode (writeFileArray outh fname)

writeFileArray :: IO.Handle -> FilePath -> IO.Handle -> IO OutArrayLen
writeFileArray outh fname inh = do
  IO.hPutStrLn outh $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  len <- writeHandleBytes outh inh
  IO.hPutStrLn outh $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return len

writeHandleBytes :: IO.Handle -> IO.Handle -> IO OutArrayLen
writeHandleBytes outh inh = do
  IO.hSetBinaryMode inh True
  contents <- IO.hGetContents inh
  writeArrayLines outh . chunksOf 16 $ contents

writeArrayLines :: IO.Handle -> [String] -> IO OutArrayLen
writeArrayLines outh ls = execStateT (mapM_ writeLine ls) 0
  where writeLine l = do
          modify' (+ length l)
          liftIO . IO.hPutStrLn outh . toLine $ l
        toLine = ("  "++) . concatMap formatByte
        formatByte = printf "0x%02X,"

createHFile :: Options -> OutArrayLen -> IO ()
createHFile opt arrlen = IO.withFile (hFileName opt) IO.WriteMode (writeHFile opt arrlen)

writeHFile :: Options -> OutArrayLen -> IO.Handle -> IO ()
writeHFile opt arrlen outh = IO.hPutStr outh . unlines $ hlines
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
