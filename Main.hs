
{-# LANGUAGE   LambdaCase
             , ScopedTypeVariables
             , OverloadedStrings
             , RecordWildCards #-}

module Main (main) where

import System.Exit
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Console.GetOpt
import System.Console.ANSI
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Exception
import Control.Concurrent.Async
import Data.Function
import Data.List
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Attoparsec.Text hiding (try)
import Text.Printf
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)
import Distribution.Simple.Utils (findPackageDesc)
import Distribution.ModuleName (toFilePath, components)
import Language.Haskell.Exts
import Language.Preprocessor.Cpphs
import Network.HTTP

main :: IO ()
main = do
    -- Process command line arguments
    (package, verA, verB, flags) <-
        runExcept <$> (getCmdOpt <$> getProgName <*> getArgs) >>= either die return
    when (verA == verB) $
        die "Need to specify different package versions for comparison"
    let disableColor = FlagDisableColor `elem` flags
        silentFlag   = FlagSilent       `elem` flags
    mode <- case foldr (\f r -> case f of FlagMode m -> m; _ -> r) "downloaddb" flags of
                "downloaddb" -> return ModeDownloadDB
                "builddb"    -> return ModeBuildDB
                "parsehs"    -> return ModeParseHS
                m            -> die $ printf "'%s' is not a valid mode" m
    diff <- withTmpDirectory $ \tmpDir -> do
        -- Need to download packages?
        when (mode `elem` [ModeBuildDB, ModeParseHS]) $
            forM_ [package ++ "-" ++ verA, package ++ "-" ++ verB] $ \pkg -> do
                unless silentFlag . putStrLn $ "Downloading " ++ pkg ++ "..."
                runExceptT (downloadPackage pkg tmpDir) >>= either die return
        -- Parse, compute difference
        either die return =<<
            ( runExceptT $
                  let cp = ComputeParams tmpDir package verA verB silentFlag
                   in case mode of
                          ModeDownloadDB -> computeDiffDownloadHoogleDB cp
                          ModeBuildDB    -> computeDiffBuildHoogleDB    cp
                          ModeParseHS    -> computeDiffParseHaskell     cp
            )
    -- Output results
    unless silentFlag $ printf "\n--- Diff for | %s → %s | ---\n\n" verA verB
    outputDiff diff disableColor silentFlag
  where die :: String -> IO a
        die str = putStrLn str >> exitFailure

data FlagMode = ModeDownloadDB | ModeBuildDB | ModeParseHS
                deriving (Eq)

data CmdFlag = FlagDisableColor
             | FlagSilent
             | FlagMode String
               deriving (Eq)

getCmdOpt :: String -> [String] -> Except String (String, String, String, [CmdFlag])
getCmdOpt progName args =
    case getOpt RequireOrder opt args of
        (flags, (pkg:verA:verB:[]), []) -> do
            -- Sanity check package version string
            forM_ [verA, verB] $ \ver ->
                case parseOnly pkgVerParser (T.pack ver) of
                    Left  _ -> throwError $ "'" ++ ver ++
                                            "' is not a valid package version string " ++
                                            "(expecting: 1.0[.0[.0]])"
                    Right _ -> return ()
            return (pkg, verA, verB, flags)
        (_, _, [] ) -> throwError usage
        (_, _, err) -> throwError (concat err ++ "\n" ++ usage)
  where
    header =
      "hackage-diff | Compare the public API of different versions of a Hackage library\n" ++
      "github.com/blitzcode/hackage-diff | www.blitzcode.net | (C) 2014 Tim C. Schroeder\n\n" ++
      "Usage: " ++ progName ++ " [options] <package-name> <old-version> <new-version>"
    usage  = usageInfo header opt
    opt    = [ Option []
                      ["mode"]
                      (ReqArg FlagMode "[downloaddb|builddb|parsehs]")
                      ( "what to download, how to compare\n" ++
                        "  downloaddb - download Hoogle DBs and diff (Default)\n" ++
                        "  builddb    - download packages, build Hoogle DBs and diff\n" ++
                        "  parsehs    - download packages, directly diff .hs exports"
                      )
             , Option ['c']
                      ["disable-color"]
                      (NoArg FlagDisableColor)
                      "disable color output"
             , Option ['s']
                      ["silent"]
                      (NoArg FlagSilent)
                      "disable progress output"
             ]

-- Check a package version string, i.e. name-1.0[.0[.0]]
pkgVerParser :: Parser ()
pkgVerParser = (nDigits 4 <|> nDigits 3 <|> nDigits 2) *> endOfInput
  where digitInt  = void (decimal :: Parser Int)
        nDigits n = count (n - 1) (digitInt *> char '.') *> digitInt

-- Create and clean up temporary working directory
withTmpDirectory :: (FilePath -> IO a) -> IO a
withTmpDirectory = bracket
    ( do sysTmpDir <- getTemporaryDirectory
         let tmpDir = addTrailingPathSeparator $ sysTmpDir </> "hackage-diff"
         createDirectoryIfMissing True tmpDir
         return tmpDir
    )
    ( removeDirectoryRecursive )

cabalInstall :: [String] -> ExceptT String IO ()
cabalInstall args = do
    (cabalExit, _, cabalStdErr) <- liftIO $ readProcessWithExitCode "cabal" args []
    unless (cabalExit == ExitSuccess) . throwError $ cabalStdErr

-- Use cabal-install to download a package from hackage
downloadPackage :: String -> FilePath -> ExceptT String IO ()
downloadPackage pkg destination = cabalInstall [ "get", pkg, "--destdir=" ++ destination ]

-- TODO: Also compare the exports by their type and report changes
data ExportCmp = EAdded | ERemoved | EModified String {- Old signature -} | EUnmodified
                 deriving (Show, Eq, Ord)

data ModuleCmp = MAdded [String]                 -- Module was added
               | MAddedParseError                -- Like above, but we couldn't parse the new one
               | MRemoved [String]               -- Module was removed
               | MRemovedParseError              -- Like above, but we couldn't parse the old one
               | MNotSureIfModifiedParseError    -- New and/or old didn't parse, can't tell
               | MModified [(ExportCmp, String)] -- Modified
               | MUnmodifed                      -- Changed
                 deriving (Show, Eq, Ord)

type Diff = [(ModuleCmp, String)]

-- Print our the computed difference, optionally with ANSI colors
outputDiff :: Diff -> Bool -> Bool -> IO ()
outputDiff diff disableColor disableLengend = do
    let putStrCol color str
            | disableColor = liftIO $ putStr str
            | otherwise    = liftIO . putStr $ setSGRCode [SetColor Foreground Vivid color] ++
                                               str ++ setSGRCode [Reset]
        putStrLnCol color str = liftIO $ putStrCol color str >> putStrLn ""
    breakingChanges <- flip execStateT (0 :: Int) . forM_ diff $ \case
        (MAdded exps                 , mname) -> do
            putStrLnCol Green $ "+ " ++ mname
            mapM_ (putStrLnCol Green . printf "  + %s") exps
        (MAddedParseError            , mname) ->
            putStrLnCol Green $
                printf " + %s (ERROR: failed to parse new version, exports not available)" mname
        (MRemoved exps               , mname) -> do
            putStrLnCol Red $ "- " ++ mname
            mapM_ (\e -> modify' (+ 1) >> putStrLnCol Red (printf "  - %s" e)) exps
        (MRemovedParseError          , mname) -> do
            modify' (+ 1)
            putStrLnCol Red $
                " - " ++ mname ++ " (ERROR: failed to parse old version, exports not available)"
        (MNotSureIfModifiedParseError, mname) -> do
            putStrLnCol Yellow $ "× " ++ mname ++
                " (Potentially modified, ERROR: failed to parse new and/or old version)"
        (MModified exps              , mname) -> do
            putStrLnCol Yellow $ "× " ++ mname
            forM_ exps $ \(cmp, expname) -> case cmp of
                EAdded        ->    putStrLnCol Green  $ "  + "      ++ expname
                ERemoved      -> do modify' (+ 1)
                                    putStrLnCol Red    $ "  - "      ++ expname
                EModified old -> do modify' (+ 1)
                                    putStrLnCol Yellow $ "  × New: " ++ expname ++ "\n" ++
                                                         "    Old: " ++ old
                EUnmodified   -> return ()
        (MUnmodifed                  , mname) -> putStrLnCol White $ "· " ++ mname
    unless disableLengend $ do
        putStrLn ""
        putStrCol Green  "[+ Added] "
        putStrCol Red    "[- Removed] "
        putStrCol Yellow "[× Modified] "
        putStrCol White  "[· Unmodified]\n"
    unless (breakingChanges == 0) $
        putStrLnCol Red $ printf "\n%i potential breaking changes found" breakingChanges

-- All the parameters required by the various compute* functions that actually prepare the
-- data and compute the difference
data ComputeParams = ComputeParams { cpTmpDir     :: FilePath
                                   , cpPackage    :: String
                                   , cpVerA       :: String
                                   , cpVerB       :: String
                                   , cpSilentFlag :: Bool
                                   } deriving (Eq, Show)

-- Compute a Diff by comparing the package's Hoogle DB downloaded from Hackage
computeDiffDownloadHoogleDB :: ComputeParams -> ExceptT String IO Diff
computeDiffDownloadHoogleDB ComputeParams { .. } = do
    -- Download databases. Network.HTTP is kinda crummy, but pulling in http-client/conduit
    -- just for downloading two small text files is probably not worth it
    putS "Downloading Hoogle DBs..."
    (dbA, dbB) <-
      either (\(e :: IOException) -> throwError $ "Download Error: " ++ show e ++ tip) return =<<
        ( liftIO . try $ concurrently (downloadURL $ getHoogleDBURL cpVerA)
                                      (downloadURL $ getHoogleDBURL cpVerB)
        )
    -- Parse
    putS "Parsing Hoogle DBs..."
    [parsedDBA, parsedDBB] <- forM [dbA, dbB] $ \db ->
        either throwError return $ parseOnly hoogleDBParser db
    -- Compare
    putS "Comparing Hoogle DBs..."
    return $ diffHoogleDB parsedDBA parsedDBB
  where getHoogleDBURL ver = "http://hackage.haskell.org/package" </> cpPackage ++ "-" ++ ver </>
                             "docs" </> cpPackage <.> "txt"
        downloadURL url = T.pack <$> do
                              req  <- simpleHTTP (getRequest url)
                              -- HTTP will throw an IOException for any connection error,
                              -- also examine the response code and throw one for every
                              -- non-200 one we get
                              code <- getResponseCode req
                              unless (code == (2, 0, 0)) . throwIO . userError $
                                  "Status code " ++ show code ++ " for request " ++ url
                              getResponseBody req
        tip = "\nYou can try building missing Hoogle DBs yourself by running with --mode=builddb"
        putS = unless cpSilentFlag . liftIO . putStrLn

-- Compute a Diff by comparing the package's Hoogle DB build through Haddock. Unfortunately,
-- running Haddock requires to have the package configured with all dependencies
-- installed. This can often be very slow and frequently fails for older packages, on top
-- of any Haddock failures that might happen
computeDiffBuildHoogleDB :: ComputeParams -> ExceptT String IO Diff
computeDiffBuildHoogleDB ComputeParams { .. } =
  flip catchError (\e -> throwError $ e ++ tip) $ do
    let packageA = cpPackage ++ "-" ++ cpVerA
        packageB = cpPackage ++ "-" ++ cpVerB
    forM_ [packageA, packageB] $ \pkg -> do
       putS $ "Processing " ++ pkg ++ "..."
       -- TODO: This is rather ugly. Cabal does not allow us to specify the target
       --       directory, and the current directory is not a per-thread property.
       --       While createProcess allows the specification of a working directory, our
       --       preferred wrapper readProcessWithExitCode does not expose that.
       --       Duplicating that function and its web of private helpers here would be
       --       quite some overhead. For now we simply change the working directory of
       --       the process
       --
       --       https://ghc.haskell.org/trac/ghc/ticket/9322#ticket
       --
       liftIO . setCurrentDirectory $ cpTmpDir </> pkg
       -- All the steps required to get the Hoogle DB
       putS "  Creating Sandbox"        >> cabalInstall [ "sandbox", "init"                      ]
       putS "  Installing Dependencies" >> cabalInstall [ "install", "--dependencies-only", "-j" ]
       putS "  Configuring"             >> cabalInstall [ "configure"                            ]
       putS "  Building Haddock"        >> cabalInstall [ "haddock", "--hoogle"                  ]
    -- Read DBs from disk
    [dbA, dbB] <-
        forM [packageA, packageB] $ \pkg ->
            (liftIO . try . TI.readFile $ getHoogleDBPath pkg)
                >>= either (\(e :: IOException) -> throwError $ show e) return
    -- Parse
    [parsedDBA, parsedDBB] <- forM [dbA, dbB] $ \db ->
        either throwError return $ parseOnly hoogleDBParser db
    -- Compare
    return $ diffHoogleDB parsedDBA parsedDBB
  where putS = unless cpSilentFlag . liftIO . putStrLn
        getHoogleDBPath ver = cpTmpDir </> cpPackage ++ "-" ++ ver </> "dist/doc/html" </>
                              cpPackage </> cpPackage <.> "txt"
        tip = "\nIf downloading / building Hoogle DBs fails, you can try directly parsing " ++
              "the source files by running with --mode=parsehs"

-- Compare two packages made up of readily parsed Hoogle DBs
diffHoogleDB :: [DBEntry] -> [DBEntry] -> Diff
diffHoogleDB dbA dbB = do
    let [verA, verB] = flip map [dbA, dbB]
            (   -- Sort exports by name
                map (\(nm, exps) -> (nm, sortBy (compare `on` dbeName) exps))
                -- Sort modules by name
              . sortBy (compare `on` fst)
                -- Extract module name, put into (name, exports) pair
              . map (\case ((DBModule nm):exps) -> (nm         , exps)
                           exps                 -> ("(Unknown)", exps)
                    )
                -- Group by module
              . groupBy (\a b -> or $ (\case DBModule _ -> False
                                             _          -> True
                                      ) <$> [a, b]
                        )
                -- Filter out comments and package information
              . filter (\case (DBPkgInfo _ _) -> False
                              (DBComment _  ) -> False
                              _               -> True
                       )
            )
        modulesAdded   = allANotInBBy ((==) `on` fst) verB verA
        modulesRemoved = allANotInBBy ((==) `on` fst) verA verB
        modulesKept    = intersectBy  ((==) `on` fst) verA verB
        resAdded       = flip map modulesAdded $ \(nm, exps) ->
                             (MAdded . map (show) $ exps, T.unpack nm)
        resRemoved     = flip map modulesRemoved $ \(nm, exps) ->
                             (MRemoved . map (show) $ exps, T.unpack nm)
        resKept        =
            sortBy compareKept . flip map modulesKept $ \(mname, modA') ->
                -- Did the exports change?
                case (modA', snd <$> find ((== mname) . fst) verB) of
                    (_   , Nothing  )  -> -- This really should not ever happen here
                                          (MNotSureIfModifiedParseError, T.unpack mname)
                    (modA, Just modB)
                        | modA == modB -> (MUnmodifed                  , T.unpack mname)
                        | otherwise    -> (MModified expCmp            , T.unpack mname)
                      where -- Which exports were added / removed / modified?
                        expCmp        = expAdded ++ expRemoved ++ expKept
                        expAdded      =
                            [(EAdded  , show x) | x <- allANotInBBy compareDBEName modB modA]
                        expRemoved    =
                            [(ERemoved, show x) | x <- allANotInBBy compareDBEName modA modB]
                        expKept       =
                            -- We don't sort by modified / unmodified here as we currently
                            -- don't list the unmodified ones
                            flip map (intersectBy compareDBEName modA modB) $ \eOld ->
                                case find (compareDBEName eOld) modB of
                                    Nothing -> error "intersectBy / find is broken..."
                                    -- TODO: The comparison we do here is pretty stupid, i.e.
                                    --       'id :: a -> a' and 'id :: x -> x' would be different
                                    Just eNew | ((==) `on` dbeType) eOld eNew ->
                                                    (EUnmodified, show eOld)
                                              | otherwise                     ->
                                                    (EModified $ show eOld, show eNew)
        -- Sort everything by modification type, but make sure we sort
        -- modified modules by their name, not their export list
        compareKept a b = case (a, b) of
                              ((MModified _, nameA), (MModified _, nameB)) -> compare nameA nameB
                              _ -> compare a b
     in resAdded ++ resRemoved ++ resKept

-- Stupid helper to build module / export lists. Should probably switch to using
-- Data.Set for all of these operations to stop having O(n*m) everywhere
allANotInBBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
allANotInBBy f a b = filter (\m -> not $ any (f m) b) a

data DBEntry = DBModule   !T.Text
             | DBPkgInfo  !T.Text !T.Text
             | DBComment  !T.Text
             | DBType     !T.Text !T.Text
             | DBNewtype  !T.Text !T.Text
             | DBData     !T.Text !T.Text
             | DBCtor     !T.Text !T.Text
             | DBClass    !T.Text !T.Text
             | DBInstance !T.Text !T.Text
             | DBFunction !T.Text !T.Text
               deriving (Eq)

-- When comparing names we have to take the kind of the export into account, i.e.
-- type and value constructors may have the same name without being identical
compareDBEName :: DBEntry -> DBEntry -> Bool
compareDBEName a b = case (a, b) of
    (DBModule _    , DBModule _    ) -> cmp; (DBPkgInfo _ _ , DBPkgInfo _ _ ) -> cmp;
    (DBComment _   , DBComment _   ) -> cmp; (DBType _ _    , DBType _ _    ) -> cmp;
    (DBNewtype _ _ , DBNewtype _ _ ) -> cmp; (DBData _ _    , DBData _ _    ) -> cmp;
    (DBCtor _ _    , DBCtor _ _    ) -> cmp; (DBClass _ _   , DBClass _ _   ) -> cmp;
    (DBInstance _ _, DBInstance _ _) -> cmp; (DBFunction _ _, DBFunction _ _) -> cmp;
    _ -> False
  where cmp = ((==) `on` dbeName) a b

-- Extract a database entry's "name" (i.e. a function name vs its type)
dbeName :: DBEntry -> T.Text
dbeName = \case
    DBModule nm     -> nm; DBPkgInfo k _   -> k ; DBComment _     -> "";
    DBType nm _     -> nm; DBNewtype nm _  -> nm; DBData nm _     -> nm;
    DBCtor nm _     -> nm; DBClass nm _    -> nm; DBInstance nm _ -> nm;
    DBFunction nm _ -> nm

-- Extract a database entry's "type" (i.e. a function type vs its name)
dbeType :: DBEntry -> T.Text
dbeType = \case
    DBModule _      -> "";  DBPkgInfo _ v   -> v ; DBComment _     -> "";
    DBType _ ty     -> ty;  DBNewtype _ ty  -> ty; DBData _ ty     -> ty;
    DBCtor _ ty     -> ty;  DBClass _ ty    -> ty; DBInstance _ ty -> ty;
    DBFunction _ ty -> ty

instance Show DBEntry where
  show = \case
    DBModule nm      -> "module " ++ T.unpack nm
    DBPkgInfo k v    -> "@" ++ T.unpack k ++ T.unpack v
    DBComment txt    -> "-- " ++ T.unpack txt
    DBType nm ty     -> "type " ++ T.unpack nm ++ " " ++ T.unpack ty
    DBNewtype nm ty  -> "newtype " ++ T.unpack nm ++ " " ++ T.unpack ty
    DBData nm ty     -> "data " ++ T.unpack nm ++ (if T.null ty then "" else " " ++ T.unpack ty)
    DBCtor nm ty     -> T.unpack nm ++ " :: " ++ T.unpack ty
    DBClass _ ty     -> "class " ++ T.unpack ty
    DBInstance _ ty  -> "instance " ++ T.unpack ty
    DBFunction nm ty -> T.unpack nm ++ " :: " ++ T.unpack ty

-- Parse a Hoogle text database
hoogleDBParser :: Parser [DBEntry]
hoogleDBParser = many parseLine
  where
    parseLine     = (*>) skipEmpty $ parseComment  <|> parseData  <|> parsePkgInfo  <|>
                                     parseDBModule <|> parseCtor  <|> parseNewtype  <|>
                                     parseDBType   <|> parseClass <|> parseInstance <|>
                                     parseFunction
    parseComment  = string "-- " *> (DBComment <$> tillEoL)
    parsePkgInfo  = char '@' *> (DBPkgInfo <$> takeTill (== ' ') <*> tillEoL)
    parseData     = string "data " *>
                    ( (DBData <$> takeTill (`elem` [ ' ', '\n' ]) <* endOfLine <*> "") <|>
                      (DBData <$> takeTill (== ' ') <* skipSpace <*> tillEoL)
                    )
    parseNewtype  = string "newtype " *>
                    ( (DBNewtype <$> takeTill (`elem` [ ' ', '\n' ]) <* endOfLine <*> "") <|>
                      (DBNewtype <$> takeTill (== ' ') <* skipSpace <*> tillEoL)
                    )
    parseCtor     = do peekChar' >>= flip unless (fail "lowercase") . isAsciiUpper
                       DBCtor <$> takeTill (== ' ') <* string " :: " <*> tillEoL
    parseFunction = do peekChar' >>=
                           flip unless (fail "uppercase") . (\c -> isAsciiLower c || c == '(')
                       DBFunction <$> takeTill (== ' ') <* string " :: " <*> tillEoL
    parseInstance = do void $ string "instance "
                       line <- T.words <$> tillEoL
                       -- The name of an instance is basically everything
                       -- after the typeclass requirements
                       let nm = case break (== "=>") line of
                                    (xs, [])    -> T.unwords xs
                                    (_, (_:xs)) -> T.unwords xs
                       return . DBInstance nm $ T.unwords line
    parseClass    = do void $ string "class "
                       line <- T.words <$> tillEoL
                       let nm = case break (== "=>") line of
                                    ((n:_), [])  -> n
                                    (_, (_:n:_)) -> n
                                    _            -> ""
                           -- TODO: Sometimes typeclasses have all their default method
                           --       implementations listed right after the 'where' part,
                           --       just cut all of this off for now
                           trunc = fst . break (== "where") $ line
                        in return . DBClass nm $ T.unwords trunc
    parseDBType   = string "type " *> (DBType <$> takeTill (== ' ') <* skipSpace <*> tillEoL)
    parseDBModule = string "module " *> (DBModule <$> takeTill (== '\n')) <* endOfLine
    skipEmpty     = many endOfLine
    tillEoL       = takeTill (== '\n') <* endOfLine

-- Compute a Diff by processing Haskell files directly. We use the Cabal API to locate and
-- parse the package .cabal file, extract a list of modules from it, and then pre-process
-- each module with cpphs and finally parse it with haskell-src-exts. The principal issue
-- with this approach is the often complex use of the CPP inside Haskell packages, making
-- this fail fairly often. This method also currently does not look at type signatures and
-- has various other limitations, like not working with modules that do not have an
-- export list
computeDiffParseHaskell :: ComputeParams -> ExceptT String IO Diff
computeDiffParseHaskell ComputeParams { .. } = do
    [mListA, mListB] <- forM [cpPackage ++ "-" ++ cpVerA, cpPackage ++ "-" ++ cpVerB] $ \pkg -> do
       unless cpSilentFlag . liftIO . putStrLn $ "Processing " ++ pkg ++ "..."
       -- Find .cabal file
       dotCabal <- (liftIO . findPackageDesc $ cpTmpDir ++ pkg) >>= either throwError return
       -- Parse .cabal file, extract exported modules
       exports  <- condLibrary <$> (liftIO $ readPackageDescription normal dotCabal) >>= \case
           Nothing   -> throwError $ pkg ++ " is not a library"
           Just node -> return $ exposedModules . condTreeData $ node
       -- Build module name / module source file list
       --
       -- TODO: Some packages have a more complex source structure, need to look at the
       --       cabal file some more to locate the files
       let modules = flip map exports $
               \m -> ( concat . intersperse "." . components $ m
                     , cpTmpDir ++ pkg </> toFilePath m <.> "hs" -- TODO: Also .lhs?
                     )
       -- Parse modules
       liftIO . forM modules $ \(modName, modPath) -> do
           unless cpSilentFlag . putStrLn $ "  Parsing " ++ modName
           Main.parseModule modPath >>= either
               -- Errors only affecting single modules are recoverable, just
               -- print them instead of throwing
               (\e -> putStrLn ("    " ++ e) >> return (modName, Nothing))
               (\r ->                           return (modName, Just r ))
    -- Compute difference
    return $ comparePackageModules mListA mListB

-- Parse a Haskell module interface using haskell-src-exts and cpphs
parseModule :: FilePath -> IO (Either String Module)
parseModule modPath = runExceptT $ do
    (liftIO $ doesFileExist modPath) >>= flip unless
        (throwError $ "Can't open source file '" ++ modPath ++ "'")
    -- Run cpphs as pre-processor over our module
    --
    -- TODO: This obviously doesn't have the same defines and include paths set like
    --       when compiling with GHC, major source of failures right now
    modSrcCPP <- liftIO $ readFile modPath >>= runCpphs defaultCpphsOptions modPath
    -- Parse pre-processed Haskell source. This pure parsing function unfortunately throws
    -- exceptions for things like encountering an '#error' directive in the code, so we
    -- also have to handle those as well
    (liftIO . try . evaluate $
        parseFileContentsWithMode defaultParseMode { parseFilename = modPath } modSrcCPP)
            >>= \case Left (e :: ErrorCall) ->
                          throwError $ "Haskell Parse Exception - " ++ show e
                      Right (Language.Haskell.Exts.ParseFailed (SrcLoc fn ln cl) err) ->
                          throwError $ printf "Haskell Parse Error - %s:%i:%i: %s" fn ln cl err
                      Right (Language.Haskell.Exts.ParseOk parsedModule) ->
                          return parsedModule

type PackageModuleList = [(String, Maybe Module)]

-- Compare two packages made up of readily parsed Haskell modules
comparePackageModules :: PackageModuleList -> PackageModuleList -> Diff
comparePackageModules verA verB = do
    let -- Compare lists of modules
        modulesAdded   = allANotInBBy ((==) `on` fst) verB verA
        modulesRemoved = allANotInBBy ((==) `on` fst) verA verB
        modulesKept    = intersectBy  ((==) `on` fst) verA verB
        -- Build result Diff of modules
        resAdded       = flip map modulesAdded $ \case
                             (mname, Just m ) ->
                                 (MAdded . map (prettyPrint) $ moduleExports m, mname)
                             (mname, Nothing) ->
                                 (MAddedParseError, mname)
        resRemoved     = flip map modulesRemoved $ \case
                             (mname, Just m ) ->
                                 (MRemoved . map (prettyPrint) $ moduleExports m, mname)
                             (mname, Nothing) ->
                                 (MRemovedParseError, mname)
                         -- TODO: This doesn't sort correctly by type of change + name
        resKept        = sortBy (compare `on` fst) . flip map modulesKept $ \(mname, modA') ->
                           -- Did the exports change?
                           case (modA', findModule verB mname) of
                             (_, Nothing) -> (MNotSureIfModifiedParseError, mname)
                             (Nothing, _) -> (MNotSureIfModifiedParseError, mname)
                             (Just modA, Just modB)
                                 | moduleExports modA == moduleExports modB
                                                -> (MUnmodifed      , mname)
                                 | otherwise    -> (MModified expCmp, mname)
                               where -- Which exports were added / removed?
                                     expCmp        =
                                       [(EAdded     , prettyPrint x) | x <- expAdded     ] ++
                                       [(ERemoved   , prettyPrint x) | x <- expRemoved   ] ++
                                       [(EUnmodified, prettyPrint x) | x <- expUnmodified]
                                       -- TODO: We do not look for type changes, no EModified
                                     expAdded      = allANotInBBy (==) (moduleExports modB)
                                                                       (moduleExports modA)
                                     expRemoved    = allANotInBBy (==) (moduleExports modA)
                                                                       (moduleExports modB)
                                     expUnmodified = intersectBy  (==) (moduleExports modA)
                                                                       (moduleExports modB)
        -- TODO: If the module does not have an export spec, we assume it exports nothing
        moduleExports (Module _ _ _ _ (Just exportSpec) _ _) = exportSpec
        moduleExports _                                      = []
        findModule mlist mname = maybe Nothing snd $ find ((== mname) . fst) mlist
     in resAdded ++ resRemoved ++ resKept
