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
import           Control.Arrow              ((&&&))
import           Control.Monad              (foldM, (>=>))
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
-- | Input data source.
data InputSource = Stdin              -- ^ Input coming from stdin.
                 | InFiles [FilePath] -- ^ Input coming from file(s).

--------------------------------------------------------------------------------
-- | Configurable options from the command line.
data Options =
  Options
  { fileName  :: String      -- ^ Output filename w/o extension.
  , varName   :: String      -- ^ C array name.
  , source    :: InputSource -- ^ Where to retrieve input from.
  , chunkSize :: Int         -- ^ Maximum number of array bytes per line.
  }

--------------------------------------------------------------------------------
-- | Environment for creating files.
data Env =
  Env
  { handle  :: IO.Handle -- ^ Output file handle.
  , options :: Options   -- ^ Command line options
  }

--------------------------------------------------------------------------------
-- | Environment Reader in IO.
type EnvReader = ReaderT Env IO

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
fileNameParser =
  OA.strOption
  (  OA.short 'o'
  <> OA.long "out"
  <> OA.metavar "TARGET"
  <> OA.help "Name of the output files (extensions .c and .h will be added)"
  )

--------------------------------------------------------------------------------
-- | A parser for the variable name command line arg.
varNameParser :: OA.Parser String
varNameParser =
  OA.strOption
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
chunkSizeParser = 
  let readPositive n | n <= 0    = OA.readerAbort $
                                     OA.ErrorMsg "This option must have a positive value."
                     | otherwise = return n
  in OA.option (OA.auto >>= readPositive)
       (  OA.short 'c'
       <> OA.long "chunk"
       <> OA.metavar "INT"
       <> OA.value 16
       <> OA.help "Maximum number of array bytes per line."
       )

--------------------------------------------------------------------------------
-- | A parser for all of the command line args.
optionsParser :: OA.Parser Options
optionsParser =
  Options <$> fileNameParser
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
createCFile o = 
  let doWrite h = runReaderT writeCFile Env { handle=h, options=o }
  in IO.withFile (cFileName o) IO.WriteMode doWrite

--------------------------------------------------------------------------------
-- | Writes the C file with include, array definition and array contents.
writeCFile :: EnvReader CArrayLen -- ^ total length of input read
writeCFile = do
  (h,o) <- asks $ handle &&& options
  putLn h $ "#include \"" ++ hFileName o ++ "\""
  putLn h $ ""
  putLn h $ "const uint8_t " ++ varName o ++ "[] = {"
  total <- writeArrayFromInputs $ source o
  putLn h $ "};"
  return total
  where putLn h = liftIO . IO.hPutStrLn h

--------------------------------------------------------------------------------
-- | Writes the contents of all of the input sources to the output file as
-- array contents.
writeArrayFromInputs :: InputSource         -- ^ input data source
                     -> EnvReader CArrayLen -- ^ total length of input read
writeArrayFromInputs Stdin = writeArrayFromStdin
writeArrayFromInputs (InFiles fs) = writeArrayFromFiles fs

--------------------------------------------------------------------------------
-- | Write the array using stdin as the input source.
writeArrayFromStdin :: EnvReader CArrayLen
writeArrayFromStdin = liftIO (getBinaryContents IO.stdin) >>= writeArray

--------------------------------------------------------------------------------
-- | Write the array using files as the input source.
writeArrayFromFiles :: [FilePath]
                    -> EnvReader CArrayLen
writeArrayFromFiles = foldM sumWrite 0
  where sumWrite !acc fname = (+acc) <$> (ask >>= writeSubArray fname)
        writeSubArray fname env =
          liftIO . withInputFile fname $ \s ->
          runReaderT (writeFileArray fname s) env

--------------------------------------------------------------------------------
-- | Perform @action@ on @file@ with that file open for reading in binary mode.
withInputFile :: FilePath         -- ^ file path
              -> (String -> IO a) -- ^ action to perform
              -> IO a
withInputFile name action =
  IO.withBinaryFile name IO.ReadMode $ IO.hGetContents >=> action

--------------------------------------------------------------------------------
-- | Get the contents of a file handle in binary mode.
getBinaryContents :: IO.Handle -> IO String
getBinaryContents h = IO.hSetBinaryMode h True >> IO.hGetContents h

--------------------------------------------------------------------------------
-- | Writes the contents of a named input file to the output file as array
-- contents.  The array contents are pre- and post-fixed with comments
-- including the input file's full path.
writeFileArray :: FilePath            -- ^ input file path
               -> String              -- ^ file contents
               -> EnvReader CArrayLen -- ^ length of input read from the input file
writeFileArray fname s = do
  h <- asks handle
  putLn h $ "  /* -- Start of File: \"" ++ fname ++ "\" -- */"
  len <- writeArray s
  putLn h $ "  /* -- End of File: \"" ++ fname ++ "\" -- */"
  return len
  where putLn h = liftIO . IO.hPutStrLn h

--------------------------------------------------------------------------------
-- | Writes the contents of an input file to the output file as array contents
-- in @maxChunkSize@ byte chunks.
writeArray :: String              -- ^ file contents
           -> EnvReader CArrayLen -- ^ length of input read from the input file
writeArray s = do
  sz <- asks (chunkSize . options)
  writeArrayLines . chunksOf sz $ s

--------------------------------------------------------------------------------
-- | Writes the contents of a list of strings to the output file as comma
-- separated hexidecimal byte values.
writeArrayLines :: [String]            -- ^ input lines
                -> EnvReader CArrayLen -- ^ sum of the lengths of the input lines
writeArrayLines ls = do
  h <- asks handle
  execStateT (mapM_ (writeLine h) ls) 0
  where writeLine h l = modify' (+ length l) >> putLn h l
        putLn h = liftIO . IO.hPutStrLn h . ("  "++) . toArrayLine

--------------------------------------------------------------------------------
-- | Converts a string of characters into a comma separated list of hexidecimal
-- values suitable for a C array body.
toArrayLine :: String -> String
toArrayLine = concatMap (printf "0x%02X,")

--------------------------------------------------------------------------------
-- | Creates the target H file.
createHFile :: Options   -- ^ command line arguments
            -> CArrayLen -- ^ length of the C array
            -> IO ()
createHFile o len =
  let doWrite h = runReaderT (writeHFile len) Env { handle=h, options=o }
  in IO.withFile (hFileName o) IO.WriteMode doWrite

--------------------------------------------------------------------------------
-- | Write the H file include guards, length #define, extern variable name
-- reference, and a trailing include guard.
writeHFile :: CArrayLen    -- ^ total length of the array
           -> EnvReader ()
writeHFile len = do
  (h,o) <- asks $ handle &&& options
  liftIO . IO.hPutStr h . unlines $ getHFileLines o len

--------------------------------------------------------------------------------
-- | A list of lines to create the H file.
getHFileLines :: Options   -- ^ command line arguments
              -> CArrayLen -- ^ length of the C array
              -> [String]  -- ^ lines of the H file
getHFileLines opt len =
  [ "#ifndef " ++ guardDef
  , "#define " ++ guardDef
  , "#include <stdint.h>"
  , ""
  , "#define " ++ lenDef ++ " (uint32_t)(" ++ show len ++ "UL)"
  , "extern const uint8_t " ++ varName opt ++ "["++ lenDef ++ "];"
  , ""
  , "#endif /* " ++ guardDef ++ " */"
  ] where upvname  = map toUpper (varName opt)
          guardDef = upvname ++ "_H"
          lenDef   = upvname ++ "_LEN"
