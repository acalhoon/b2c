{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
{- |
 - Program     : b2c
 - Description : Converts input into a C array.
 - Copyright   : (c) A.Calhoon, 2017
 - License     : BSD3
 - Maintainer  : a.calhoon@gmail.com
 - Stability   : experimental
 - Portability : POSIX
 - 
 - Converts input sources into a C file containing an array of uint8_t bytes
 - and an H file with the necessary defines and externs so that array can be
 - referenced from other program files.
 -}
--------------------------------------------------------------------------------
module Main where
import           Control.Monad              (foldM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import           Control.Monad.Trans.State  (execStateT, modify')
import           Data.Char                  (toUpper)
import           Data.List.Split            (chunksOf)
import           Data.Monoid                ((<>))
import qualified Options.Applicative        as OA
import qualified System.IO                  as IO
import           Text.Printf                (printf)

--------------------------------------------------------------------------------
-- | The length of the output C array.
type CArrayLen = Int

--------------------------------------------------------------------------------
-- | Environment Reader in IO.
type EnvReader = ReaderT Options IO

--------------------------------------------------------------------------------
-- | Input data source.
data InputSource = Stdin              -- ^ Input coming from stdin.
                 | InFiles [FilePath] -- ^ Input coming from file(s).

--------------------------------------------------------------------------------
-- | Configurable options from the command line.
data Options = Options
  { fileName  :: String      -- ^ Output filename w/o extension.
  , varName   :: String      -- ^ C array name.
  , source    :: InputSource -- ^ Where to retrieve input from.
  , chunkSize :: Int         -- ^ Maximum number of array bytes per line.
  }

--------------------------------------------------------------------------------
-- | Build the C file name from command line args.
cFileName :: Options -> String
cFileName = (++".c") . fileName

--------------------------------------------------------------------------------
-- | Build the H file name from command line args.
hFileName :: Options -> String
hFileName = (++".h") . fileName

--------------------------------------------------------------------------------
-- | A parser for the filename command line arg.
fileNameParser :: OA.Parser String
fileNameParser = OA.strOption
  (  OA.short 'o'
  <> OA.long "out"
  <> OA.metavar "TARGET"
  <> OA.help "Name of the output files (extensions .c and .h will be added)"
  )

--------------------------------------------------------------------------------
-- | A parser for the variable name command line arg.
varNameParser :: OA.Parser String
varNameParser = OA.strOption
  (  OA.short 'v'
  <> OA.long "var"
  <> OA.metavar "VAR_NAME"
  <> OA.help "Name of the array variable"
  )

--------------------------------------------------------------------------------
-- | A parser for the input source command line args.
sourceParser :: OA.Parser InputSource
sourceParser = fileParser OA.<|> stdinParser
  where fileParser :: OA.Parser InputSource
        -- ^ A parser for  when one or more filenames is provided.
        fileParser = InFiles <$> (OA.some . OA.strArgument $ OA.metavar "FILES...")
        stdinParser :: OA.Parser InputSource
        -- ^ A parser for the stdin flag.
        stdinParser = OA.flag' Stdin
          (  OA.long "stdin"
          <> OA.help "Read input from stdin"
          )

--------------------------------------------------------------------------------
-- | A parser for the optional chunk size command line arg.
chunkSizeParser :: OA.Parser Int
chunkSizeParser = OA.option (OA.auto >>= readPositive)
  (  OA.short 'c'
  <> OA.long "chunk"
  <> OA.metavar "INT"
  <> OA.value 16
  <> OA.help "Maximum number of array bytes per line."
  )
  where readPositive n | n <=0     = OA.readerAbort $ OA.ErrorMsg "This option must have a positive value."
                       | otherwise = return n

--------------------------------------------------------------------------------
-- | A parser for all of the command line args.
optionsParser :: OA.Parser Options
optionsParser = Options
  <$> fileNameParser
  <*> varNameParser
  <*> sourceParser
  <*> chunkSizeParser

--------------------------------------------------------------------------------
-- | Parse command line options and report usage on failure.
main :: IO ()
main = b2c =<< OA.execParser opts
  where opts = OA.info (OA.helper <*> optionsParser) $
                        OA.fullDesc <> desc <> hdr
        desc = OA.progDesc " Create TARGET.c and TARGET.h containing an array \
                           \ named VAR_NAME consisting of the contents of \
                           \ FILES..."
        hdr  = OA.header "b2c - converts input data into a C array"

--------------------------------------------------------------------------------
-- | Turn command line arguments into the target C and H files.
b2c :: Options -- ^ command line arguments
    -> IO()
b2c opt = createCFile opt >>= createHFile opt

--------------------------------------------------------------------------------
-- | Creates the target C file.
createCFile :: Options      -- ^ command line arguments
            -> IO CArrayLen -- ^ total length of input read
createCFile o = IO.withFile (cFileName o) IO.WriteMode (\h -> runReaderT (writeCFile h) o)

--------------------------------------------------------------------------------
-- | Writes the C file with include, array definition and array contents.
writeCFile :: IO.Handle           -- ^ output file handle
           -> EnvReader CArrayLen -- ^ total length of input read
writeCFile outh = do
  o <- ask
  liftIO $ IO.hPutStrLn outh $ "#include \"" ++ hFileName o ++ "\""
  liftIO $ IO.hPutStrLn outh $ ""
  liftIO $ IO.hPutStrLn outh $ "const uint8_t " ++ varName o ++ "[] = {"
  total <- writeArray outh (source o)
  liftIO $ IO.hPutStrLn outh $ "};"
  return total

--------------------------------------------------------------------------------
-- | Writes the contents of all of the input sources to the output file as
-- array contents.
writeArray :: IO.Handle           -- ^ output file handle
           -> InputSource         -- ^ input data source
           -> EnvReader CArrayLen -- ^ total length of input read
writeArray outh Stdin = writeHandleBytes outh IO.stdin
writeArray outh (InFiles fs) = foldM sumWrites 0 fs
  where sumWrites !acc fname = (+acc) <$> writeFileBytes fname
        writeFileBytes fname = ask >>= \o -> 
          liftIO $ IO.withFile fname IO.ReadMode (\h -> runReaderT (writeFileArray outh fname h) o)

--------------------------------------------------------------------------------
-- | Writes the contents of a named input file to the output file as array
-- contents.  The array contents are pre- and post-fixed with comments
-- including the input file's full path.
writeFileArray :: IO.Handle           -- ^ output file handle
               -> FilePath            -- ^ input file path
               -> IO.Handle           -- ^ input file handle
               -> EnvReader CArrayLen -- ^ length of input read from the input file
writeFileArray outh fname inh = do
  liftIO $ IO.hPutStrLn outh $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  len <- writeHandleBytes outh inh
  liftIO $ IO.hPutStrLn outh $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return len

--------------------------------------------------------------------------------
-- | Writes the contents of an input file to the output file as array contents
-- in @maxChunkSize@ byte chunks.
writeHandleBytes :: IO.Handle           -- ^ output file handle
                 -> IO.Handle           -- ^ input file handle
                 -> EnvReader CArrayLen -- ^ length of input read from the input file
writeHandleBytes outh inh = do
  liftIO $ IO.hSetBinaryMode inh True
  contents <- liftIO $ IO.hGetContents inh
  sz <- asks chunkSize
  writeArrayLines outh . chunksOf sz $ contents

--------------------------------------------------------------------------------
-- | Writes the contents of a list of strings to the output file as comma
-- separated hexidecimal byte values.
writeArrayLines :: IO.Handle           -- ^ output file handle
                -> [String]            -- ^ input lines
                -> EnvReader CArrayLen -- ^ sum of the lengths of the input lines
writeArrayLines outh ls = execStateT (mapM_ writeLine ls) 0
  where writeLine l = do
          modify' (+ length l)
          liftIO . IO.hPutStrLn outh . toLine $ l
        toLine = ("  "++) . concatMap formatByte
        formatByte = printf "0x%02X,"

--------------------------------------------------------------------------------
-- | Creates the target H file.
createHFile :: Options   -- ^ command line arguments
            -> CArrayLen -- ^ length of the C array
            -> IO ()
createHFile o arrlen = IO.withFile (hFileName o) IO.WriteMode (writeHFile o arrlen)

--------------------------------------------------------------------------------
-- | Write the H file include guards, length #define, extern variable name
-- reference, and a trailing include guard.
writeHFile :: Options   -- ^ command line arguments
           -> CArrayLen -- ^ total length of the array
           -> IO.Handle -- ^ output file handle
           -> IO ()
writeHFile o arrlen outh = IO.hPutStr outh . unlines $ hlines
  where vname = varName o
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
