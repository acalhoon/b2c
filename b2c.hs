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
import           Control.Exception          (IOException, try)
import           Control.Monad              (foldM, (>=>), void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import           Control.Monad.Trans.State  (execStateT, modify')
import           Data.Char                  (toUpper)
import           Data.List.Split            (chunksOf)
import           Data.Monoid                ((<>))
import qualified Options.Applicative        as OA
import           System.Directory           (removeFile)
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
-- | A parser for when one or more input filenames is provided.
fileParser :: OA.Parser InputSource
fileParser = InFiles <$> (OA.some . OA.strArgument $ OA.metavar "FILES...")

--------------------------------------------------------------------------------
-- | A parser for the stdin flag.
stdinParser :: OA.Parser InputSource
stdinParser =
  OA.flag' Stdin
  (  OA.long "stdin"
  <> OA.help "Read input from stdin"
  )

--------------------------------------------------------------------------------
-- | A parser for the optional chunk size command line arg.
chunkSizeParser :: OA.Parser Int
chunkSizeParser = 
  OA.option (OA.auto >>= readPositive)
  (  OA.short 'c'
  <> OA.long "chunk"
  <> OA.metavar "INT"
  <> OA.value 16
  <> OA.help "Maximum number of array bytes per line."
  )
  where readPositive n | n <= 0    = errMsg
                       | otherwise = return n
        errMsg = OA.readerError "must be a positive value"

--------------------------------------------------------------------------------
-- | A parser for all of the command line args.
optionsParser :: OA.Parser Options
optionsParser =
  Options <$> fileNameParser
          <*> varNameParser
          <*> (fileParser OA.<|> stdinParser)
          <*> chunkSizeParser

--------------------------------------------------------------------------------
-- | Parse command line options and report usage on failure.
main :: IO ()
main = b2c =<< OA.execParser opts
  where opts = OA.info (OA.helper <*> optionsParser)
                       (OA.fullDesc <> desc <> hdr)
        desc = OA.progDesc " Create TARGET.c and TARGET.h containing an array \
                           \ named VAR_NAME consisting of the contents of \
                           \ FILES..."
        hdr  = OA.header "b2c - converts input data into a C array"

--------------------------------------------------------------------------------
-- | Try to execute an IO action that may throw an exception.
tryIOE :: IO a -> IO (Either IOException a)
tryIOE = try

--------------------------------------------------------------------------------
-- | Turn command line arguments into the target C and H files.
b2c :: Options -- ^ command line arguments
    -> IO()
b2c opt = tryIOE (createCFile opt >>= createHFile opt) >>= cleanUp
  where cleanUp (Left exc) = do tryDelete (cFileName opt)
                                tryDelete (hFileName opt)
                                print exc
        cleanUp (Right _)  = return ()
        tryDelete name = void $ tryIOE $ removeFile name

--------------------------------------------------------------------------------
-- | Creates the target C file.
createCFile :: Options      -- ^ command line arguments
            -> IO CArrayLen -- ^ total length of input read
createCFile o = withOutputFile (cFileName o) o writeCFile 

--------------------------------------------------------------------------------
-- | Perform @m@ on @file@ with that file open for reading in binary mode.
withOutputFile :: FilePath    -- ^ file to be created
               -> Options     -- ^ command line arguments
               -> EnvReader a -- ^ computation to perform within an Env
               -> IO a        -- ^ result of performed action
withOutputFile name o ma =
  IO.withFile name IO.WriteMode $ \h ->
  runReaderT ma Env { handle=h, options=o }

--------------------------------------------------------------------------------
-- | Writes the C file with include, array definition and array contents.
writeCFile :: EnvReader CArrayLen -- ^ total length of input read
writeCFile = do
  (h,o) <- asks $ handle &&& options
  liftIO $ do IO.hPutStrLn h $ "#include \"" ++ hFileName o ++ "\""
              IO.hPutStrLn h $ ""
              IO.hPutStrLn h $ "const uint8_t " ++ varName o ++ "[] = {"
  total <- sourcePutArray $ source o
  liftIO $    IO.hPutStrLn h $ "};"
  return total

--------------------------------------------------------------------------------
-- | Writes the contents of all of the input sources to the output file as
-- array contents.
sourcePutArray :: InputSource         -- ^ input data source
               -> EnvReader CArrayLen -- ^ total length of input read
sourcePutArray Stdin = liftIO (IO.hSetBinaryMode IO.stdin True >> IO.getContents) >>= putArray
sourcePutArray (InFiles fs) = foldM (\ !acc name -> (+acc) <$> filePutArray name) 0 fs

--------------------------------------------------------------------------------
-- | Writes the contents of a named input file to the output file as array
-- contents.  The array contents are pre- and post-fixed with comments
-- including the input file's full path.
filePutArray :: FilePath            -- ^ input file path
             -> EnvReader CArrayLen -- ^ length of input read from the input file
filePutArray name = ask >>= liftIO . putLines
  where putLines env = do 
          let h = handle env
          IO.hPutStrLn h $ "  /* -- Start of File: \"" ++ name ++ "\" -- */"
          len <- withInputFile name $ \s -> runReaderT (putArray s) env
          IO.hPutStrLn h $ "  /* -- End of File: \"" ++ name ++ "\" -- */"
          return len

--------------------------------------------------------------------------------
-- | Perform @action@ on @name@ with that file open for reading in binary mode.
withInputFile :: FilePath         -- ^ file to be read
              -> (String -> IO a) -- ^ action to perform on file contents
              -> IO a
withInputFile name action =
  IO.withBinaryFile name IO.ReadMode $ IO.hGetContents >=> action

--------------------------------------------------------------------------------
-- | Writes the contents of an input file to the output file as array contents
-- in @maxChunkSize@ byte chunks.
putArray :: String              -- ^ file contents
         -> EnvReader CArrayLen -- ^ length of input read from the input file
putArray s = do
  size <- asks (chunkSize . options)
  h <- asks handle
  let writeCount l = modify' (+ length l) >> liftIO (putAsArrayLn h l)
  execStateT (mapM_ writeCount $ chunksOf size s) 0

--------------------------------------------------------------------------------
-- | Writes a string of bytes to @handle@ as a line of uint8_t array bytes
putAsArrayLn :: IO.Handle -> String -> IO ()
putAsArrayLn h = IO.hPutStrLn h . ("  "++) . concatMap (printf "0x%02X,")

--------------------------------------------------------------------------------
-- | Creates the target H file.
createHFile :: Options   -- ^ command line arguments
            -> CArrayLen -- ^ length of the C array
            -> IO ()
createHFile o len = withOutputFile (hFileName o) o $ writeHFile len

--------------------------------------------------------------------------------
-- | Write the H file include guards, length #define, extern variable name
-- reference, and a trailing include guard.
writeHFile :: CArrayLen    -- ^ total length of the array
           -> EnvReader ()
writeHFile len = do
  (h,o) <- asks (handle &&& options)
  liftIO $ IO.hPutStr h . unlines $ getHFileLines o len

--------------------------------------------------------------------------------
-- | A list of lines to create the H file.
getHFileLines :: Options   -- ^ command line arguments
              -> CArrayLen -- ^ length of the C array
              -> [String]  -- ^ lines of the H file
getHFileLines opt len =
  [ "#ifndef " ++ guardDef
  , "#define " ++ guardDef
  , ""
  , "#ifdef __cplusplus"
  , "extern \"C\" {"
  , "#endif /* _cplusplus */"
  , ""
  , "#include <stdint.h>"
  , ""
  , "#define " ++ lengthDef ++ " (uint32_t)(" ++ show len ++ "UL)"
  , "extern const uint8_t " ++ varName opt ++ "["++ lengthDef ++ "];"
  , ""
  , "#ifdef __cplusplus"
  , "}"
  , "#endif /* _cplusplus */"
  , ""
  , "#endif /* " ++ guardDef ++ " */"
  ] where guardDef  = map toUpper (fileName opt) ++ "_H"
          lengthDef = map toUpper (varName opt) ++ "_LEN"
