module Main where

import ClassyPrelude.Conduit
import Data.Digest.Pure.MD5
import System.Process
import Options.Applicative
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.FileLock
import Control.Category ((>>>))
import Data.List (nub)

data Opts = Opts {
  optsClean :: Bool,
  optsExprs :: [String],
  optsFiles :: [FilePath],
  optsTemplate :: String,
  optsGhcO :: String,
  optsGhcOpts :: String,
  optsPackages :: [String],
  optsModules :: [String],
  optsExec :: Bool,
  optsVerbose :: Bool,
  optsArgs :: [String]
  } deriving (Show)

optsParser :: Parser Opts
optsParser = do
  optsExprs <- many $ option str (short 'e' ++ long "expr" ++ help "Expression")
  optsFiles <- many $ option str (short 'f' ++ long "file" ++ help "Use declarations from file")
  optsGhcO <- option str (short 'O' ++ value "" ++ help "Optimization level")
  optsGhcOpts <- option str (long "ghc-options" ++ help "Extra GHC options" ++ value "")
  optsTemplate <- option str (short 't' ++ long "template" ++ help "Template to use" ++ value "default")
  optsExec <- not <$> switch (short 'n' ++ help "Build only, don't execute")
  optsClean <- switch (long "clean" ++ help "Force code generation and recompilation")
  optsVerbose <- switch (long "verbose" ++ short 'v' ++ help "Verbose mode")
  optsPackages <- many $ option str (long "package" ++ short 'p' ++ help "Use package")
  optsModules <- many $ option str (long "module" ++ short 'm' ++ help "Import module (unqualified)")
  optsArgs <- many (strArgument (metavar "ARGS") )
  return (Opts{..})

runOptParser :: Parser a -> [String] -> Maybe a
runOptParser p = getParseResult . execParserPure defaultPrefs (info p mempty)

defaultOpts :: Opts
defaultOpts = fromMaybe (error "defaultOpts: no default value for options") $ runOptParser optsParser []

main :: IO ()
main = do
  allArgs <- map unpack <$> getArgs
  opts@Opts{..} <- if
    -- if the first arg is file, then we're likely invoked from shebang; get options from the file
    | file:args <- allArgs, not ("-" `isPrefixOf` file) ->
      -- rewrite using lens?
      (\o -> o {optsFiles = optsFiles o `snoc` file, optsArgs = args}) <$> parseOptsFromFile file
    | otherwise -> execParser $ info (helper <*> optsParser) $ progDesc ""
  when optsVerbose $ print opts
  -- print opts
  exec <- compileExpr opts
  if
    | optsExec -> do
      _ <- callProcess exec optsArgs
      return ()
    | otherwise -> do
      putStrLn $ pack exec

parseOptsFromFile :: MonadIO m => FilePath -> m Opts
parseOptsFromFile file = BL8.lines <$> readFile file >>= \rows -> if
  | r1:r2:_ <- rows, "#!" `isPrefixOf` r1, ("--" : args') <- words (BL8.unpack r2) ->
    return $ fromMaybe (error $ "Can't parse options from file: " ++ unwords args') $ runOptParser optsParser args'
  | otherwise -> return defaultOpts

combineExprs :: [String] -> LByteString
combineExprs = unsnoc >>> \case
  Nothing -> ""
  Just (exprs, lastExpr) -> BL8.pack $ unlines $ exprs `snoc` ("main = " ++ lastExpr)

compileExpr :: Opts -> IO FilePath
compileExpr opts@Opts{..} = do
  cacheDir <- getCacheDir
  template <- readFile . (</> optsTemplate ++ ".hs") =<< getTemplatesDir
  body <- (++ combineExprs optsExprs) . concat . map (`BL8.snoc` '\n') <$> traverse readFile optsFiles
  let progSource = generateSource opts body template
  stackGhcArgs <- getStackGhcArgs opts
  let progId = progHash stackGhcArgs progSource
  let progDir = cacheDir </> progId
  let progSourceFile = progDir </> basename ++ ".hs"
  let progExecFile = progDir </> basename
  let unlessCached = whenM ((optsClean ||) . not <$> doesFileExist progExecFile)
  unlessCached $ do
    createDirectoryIfMissing True progDir
    -- Create a lock in case other `adlib` process is doing the same job.
    -- By the time we get the lock, other process may have done the job.
    withFileLock (progDir </> "lock") Exclusive $ \_lock -> unlessCached $ do
      writeFile progSourceFile progSource
      when optsVerbose $ do
        putStrLn $ "Wrote source file: " ++ pack progSourceFile ++ "\n"
        putStr =<< readFile progSourceFile
      compileSourceFile opts stackGhcArgs progSourceFile
  return progExecFile
  where
  basename = "main"

progHash :: [String] -> LByteString -> String
progHash buildArgs progSource = show $ md5 $ BL8.pack (unwords buildArgs) ++ progSource

generateSource :: Opts -> LByteString -> LByteString -> LByteString
generateSource Opts{..} expr = BL8.unlines . (++ dropShebang (BL8.lines expr)) .
  insertImports . dropShebang . BL8.lines
  where
  dropShebang = dropWhile (prefixOneOf ["#!"])
  insertImports rows = before ++ ["import " ++ BL8.pack m | m <- optsModules] ++ after
    where
    (before, after) = span (emptyOrStartsWith ["module", "{-", "--"]) rows

getStackGhcArgs :: Opts -> IO [String]
getStackGhcArgs Opts{..} = do
  includeDir <- getIncludeDir
  let includePackagesFile = includeDir </> "packages"
  let grepPackages = unwords ["find", includeDir, "-name '*.hs'",
        "| xargs cat |",
        "sed -n 's/^import .*\"\\([a-Z0-9-]*\\)\".*/\\1/p' > ", includePackagesFile]
  when optsVerbose $ putStrLn $ pack grepPackages
  callCommand grepPackages
  includePackages <- lines <$> readFile includePackagesFile
  -- FIXME: sometimes it's .stack/global (for older stack versions)
  stackYaml <- (</> ".stack/global-project/stack.yaml") <$> getHomeDirectory
  return $
    ["--stack-yaml=" ++ stackYaml] ++
    map ("--package=" ++) (nub $ optsPackages ++ includePackages) ++
    ["--", "-v0", "-XPackageImports", "-XApplicativeDo", "-O" ++ optsGhcO, "-i" ++ includeDir] ++ words optsGhcOpts

compileSourceFile :: Opts -> [String] -> FilePath -> IO ()
compileSourceFile Opts{..} args file = do
  when optsVerbose $ putStrLn $ pack $ "Running: stack " ++ unwords stackArgs
  out <- readProcess "stack" stackArgs ""
  when optsVerbose $ putStrLn $ pack out
  where
  stackArgs = "ghc" : args `snoc` file
getRootDir, getCacheDir, getIncludeDir, getTemplatesDir :: IO FilePath
getRootDir = (</> ".adlib") <$> getHomeDirectory
getCacheDir = (</> "cache") <$> getRootDir
getIncludeDir = (</> "include") <$> getRootDir
getTemplatesDir = (</> "templates") <$> getRootDir

-- * Parsing helpers

prefixOneOf :: (MonoFoldable mono, IsSequence (Element mono), Eq (Element (Element mono))) =>
  mono -> Element mono -> Bool
prefixOneOf prefixes x = any (`isPrefixOf` x) prefixes
emptyOrStartsWith :: (Element mono ~ LByteString, MonoFoldable mono) =>
  mono -> LByteString -> Bool
emptyOrStartsWith prefixes = maybe True (prefixOneOf prefixes ) . listToMaybe . BL8.words
