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
import           Control.Monad             (foldM, mapM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (execStateT, modify')
import           Data.Char                 (toUpper)
import           Data.List.Split           (chunksOf)
import           Data.Monoid               ((<>))
import qualified Options.Applicative       as OA
import qualified System.IO                 as IO
import           Text.Printf               (printf)

--------------------------------------------------------------------------------
-- | The length of the output C array.
type CArrayLen = Int

--------------------------------------------------------------------------------
-- | The maximum number of bytes in the array output.
maxChunkSize :: (Num a) => a
maxChunkSize = 16

--------------------------------------------------------------------------------
-- | Input data source.
data InputSource = Stdin              -- ^ Input coming from stdin.
                 | InFiles [FilePath] -- ^ Input coming from file(s).

--------------------------------------------------------------------------------
-- | Configurable options from the command line.
data Options = Options
  { fileName :: String      -- ^ Output filename w/o extension.
  , varName  :: String      -- ^ Variable name.
  , source   :: InputSource -- ^ Where to retrieve input from.
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
          <> OA.help "Read from stdin"
          )

--------------------------------------------------------------------------------
-- | A parser for all of the command line args.
optionsParser :: OA.Parser Options
optionsParser = Options
  <$> fileNameParser
  <*> varNameParser
  <*> sourceParser

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
b2c :: Options -> IO()
b2c opt = createCFile opt >>= createHFile opt

--------------------------------------------------------------------------------
-- | Creates the target C file.
createCFile :: Options -> IO CArrayLen
createCFile opt = IO.withFile (cFileName opt) IO.WriteMode (writeCFile opt)

--------------------------------------------------------------------------------
-- | Writes the C file with include, array definitition and array contents.
writeCFile :: Options      -- ^ command line arguments
           -> IO.Handle    -- ^ output file handle
           -> IO CArrayLen -- ^ total length of input read
writeCFile opt outh = do
  IO.hPutStrLn outh $ "#include \"" ++ hFileName opt ++ "\""
  IO.hPutStrLn outh $ ""
  IO.hPutStrLn outh $ "const uint8_t " ++ varName opt ++ "[] = {"
  total <- writeArray outh (source opt)
  IO.hPutStrLn outh $ "};"
  return total

--------------------------------------------------------------------------------
-- | Writes the contents of all of the input sources to the output file as
-- array contents.
writeArray :: IO.Handle    -- ^ output file handle
           -> InputSource  -- ^ input data source
           -> IO CArrayLen -- ^ total length of input read
writeArray outh Stdin = writeHandleBytes outh IO.stdin
writeArray outh (InFiles fs) = foldM sumWrites 0 fs
  where sumWrites !acc fname = (+acc) <$> writeFileBytes fname
        writeFileBytes fname = IO.withFile fname IO.ReadMode (writeFileArray outh fname)

--------------------------------------------------------------------------------
-- | Writes the contents of a named input file to the output file as array
-- contents.  The array contents are pre- and post-fixed with comments
-- including the input file's full path.
writeFileArray :: IO.Handle    -- ^ output file handle
               -> FilePath     -- ^ input file path
               -> IO.Handle    -- ^ input file handle
               -> IO CArrayLen -- ^ length of input read from the input file
writeFileArray outh fname inh = do
  IO.hPutStrLn outh $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  len <- writeHandleBytes outh inh
  IO.hPutStrLn outh $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return len

--------------------------------------------------------------------------------
-- | Writes the contents of an input file to the output file as array contents
-- in @maxChunkSize@ byte chunks.
writeHandleBytes :: IO.Handle    -- ^ output file handle
                 -> IO.Handle    -- ^ input file handle
                 -> IO CArrayLen -- ^ length of input read from the input file
writeHandleBytes outh inh = do
  IO.hSetBinaryMode inh True
  contents <- IO.hGetContents inh
  writeArrayLines outh . chunksOf maxChunkSize $ contents

--------------------------------------------------------------------------------
-- | Writes the contents of a list of strings to the output file as comma
-- separated hexidecimal byte values.
writeArrayLines :: IO.Handle    -- ^ output file handle
                -> [String]     -- ^ input lines
                -> IO CArrayLen -- ^ sum of the lengths of the input lines
writeArrayLines outh ls = execStateT (mapM_ writeLine ls) 0
  where writeLine l = do
          modify' (+ length l)
          liftIO . IO.hPutStrLn outh . toLine $ l
        toLine = ("  "++) . concatMap formatByte
        formatByte = printf "0x%02X,"

--------------------------------------------------------------------------------
-- | Creates the target H file.
createHFile :: Options -> CArrayLen -> IO ()
createHFile opt arrlen = IO.withFile (hFileName opt) IO.WriteMode (writeHFile opt arrlen)

--------------------------------------------------------------------------------
-- | Write the H file include guards, length #define, extern variable name
-- reference, and a trailing include guard.
writeHFile :: Options   -- ^ command line arguments
           -> CArrayLen -- ^ total length of the array
           -> IO.Handle -- ^ output file handle
           -> IO ()
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
