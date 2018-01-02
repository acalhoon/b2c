{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
{- |
 - Program     : b2c
 - Description : Converts input into a C array.
 - Copyright   : (c) A.Calhoon, 2017-2018
 - License     : BSD3
 - Maintainer  : a.calhoon@gmail.com
 - Stability   : experimental
 - Portability : POSIX
 - 
 - Converts input sources into a C file containing an array of uint8_t bytes
 - and an H file with the necessary defines and externs so that array can be
 - referenced from C and C++ program files.
 -}
--------------------------------------------------------------------------------
module Main where
import           Control.Exception          (IOException, try)
import           Control.Monad              (foldM, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.State  (execStateT, modify')
import           Data.Char                  (toUpper)
import           Data.List.Split            (chunksOf)
import           Data.Monoid                ((<>))
import qualified Options.Applicative        as OA
import           System.Directory           (removeFile)
import           System.IO
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
-- | Turn command line arguments @opt@ into the target C and H files.
b2c :: Options -> IO ()
b2c opt = tryIOE (createCFile opt >>= createHFile opt) >>= cleanUp
  where cleanUp (Left exc) = do tryDelete (cFileName opt)
                                tryDelete (hFileName opt)
                                print exc
        cleanUp (Right _)  = return ()
        tryDelete name = void $ tryIOE $ removeFile name

--------------------------------------------------------------------------------
-- | Creates the target C file from @opts@.
createCFile :: Options
            -> IO CArrayLen -- ^ total length of input read
createCFile opts = withFile (cFileName opts) WriteMode $ writeCFile opts

--------------------------------------------------------------------------------
-- | Writes @outfile@ as a C file containing an include, array definition, and
-- array contents derived from @opts@ and returns the total length of the C
-- array.
writeCFile :: Options
           -> Handle       -- ^ output file handle
           -> IO CArrayLen -- ^ total length of the C file array
writeCFile opts outfile = do
  hPutStr outfile . unlines $ [ "#include \"" ++ hFileName opts ++ "\""
                              , ""
                              , "const uint8_t " ++ varName opts ++ "[] = {" ]

  let maxbytes = chunkSize opts
  total <- case source opts of
    (InFiles fs) -> let sum' !acc f = (+acc) <$> putFileArray outfile f maxbytes
                    in foldM sum' 0 fs
    Stdin        -> putArray outfile maxbytes stdin

  hPutStrLn outfile $ "};"
  return total

--------------------------------------------------------------------------------
-- | Writes the contents of @infile@ to @outfile@ as the lines of an array. The
-- maximum number of bytes in a given line is @maxbytes@.
putArray :: Handle       -- ^ output file handle
         -> Int          -- ^ maximum number of bytes per line
         -> Handle       -- ^ input file handle
         -> IO CArrayLen -- ^ total length of input read from @infile@
putArray outfile maxbytes infile =
  getArrayBytes infile maxbytes >>= putArrayBytes outfile

--------------------------------------------------------------------------------
-- | Read binary input from @infile@ in chunks of length @maxbytes@.
getArrayBytes :: Handle      -- ^ input file handle
              -> Int         -- ^ length of input chunks
              -> IO [String] -- ^ byte-string chunks
getArrayBytes infile maxbytes = do
  hSetBinaryMode infile True
  chunksOf maxbytes <$> hGetContents infile

--------------------------------------------------------------------------------
-- | Writes to @outfile@ each byte-string in @xs@ as array lines of uint8_t
-- bytes.
putArrayBytes :: Handle       -- ^ output file handle
              -> [String]     -- ^ byte-strings
              -> IO CArrayLen -- ^ total number of bytes written
putArrayBytes outfile xs = do
  let writeCount x = modify' (+ length x) >> liftIO (putLine x)
  execStateT (mapM_ writeCount xs) 0
  where putLine = hPutStrLn outfile . ("  "++) . concatMap (printf "0x%02X,")

--------------------------------------------------------------------------------
-- | Writes to @outfile@ the contents of the file at path @inpath@ as an array
-- contents. The maximum number of bytes on each line of the array is
-- @maxbytes@.  The array contents are pre- and post-fixed with comments
-- including the @inpath@.
putFileArray :: Handle       -- ^ output file handle
             -> FilePath     -- ^ input file path
             -> Int          -- ^ maximum number of bytes for an array line
             -> IO CArrayLen -- ^ total length of input read from @infile@
putFileArray outfile inpath maxbytes = do
  hPutStrLn outfile $ "  /* -- Start of File: \"" ++ inpath ++ "\" -- */"
  total <- withFile inpath ReadMode $ putArray outfile maxbytes
  hPutStrLn outfile $ "  /* -- End of File: \"" ++ inpath ++ "\" -- */"
  return total

--------------------------------------------------------------------------------
-- | Creates the target H file.
createHFile :: Options
            -> CArrayLen -- ^ length of the C array
            -> IO ()
createHFile opts len =
  withFile (hFileName opts) WriteMode $ writeHFile opts len

--------------------------------------------------------------------------------
-- | Writes @outfile@ as an H file containing include guards, length #define,
-- and extern reference to the C array.
writeHFile :: Options
           -> CArrayLen    -- ^ length of the array
           -> Handle       -- ^ output file handle
           -> IO ()
writeHFile opts len outfile = hPutStr outfile . unlines $ getHFileLines opts len

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
  , "extern const uint8_t " ++ var ++ "["++ lengthDef ++ "];"
  , ""
  , "#ifdef __cplusplus"
  , "}"
  , "#endif /* _cplusplus */"
  , ""
  , "#endif /* " ++ guardDef ++ " */"
  ] where var       = varName opt
          guardDef  = map toUpper (fileName opt) ++ "_H"
          lengthDef = map toUpper var ++ "_LEN"
