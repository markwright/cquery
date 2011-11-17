-- Copyright (c) 2011, Mark Wright.  All rights reserved.

{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main (main) where

import Control.Exception as E
import qualified Data.List as L
import Data.Char (toLower)
import Distribution.Compiler
import Distribution.ModuleName (components, ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.System (buildPlatform)
import Distribution.Verbosity
import Distribution.Version
import System.Console.CmdArgs.Implicit
import System.IO
import System.Process (readProcess)
import Data.String.Utils
import Text.Regex.TDFA

deriving instance Data CompilerFlavor
deriving instance Data Version

deriving instance Typeable CompilerFlavor

instance Default CompilerFlavor where
  def = buildCompilerFlavor

data VersionInfo = NotDisplayed
                   | Displayed
                   | DisplayedInSquareBrackets
                   deriving (Data, Enum, Typeable, Show)

data ModuleInfo = ExposedAndNonExposed
                  | Exposed
                  | NonExposed
                  deriving (Data, Enum, Typeable, Show)

data CmdOpts = Depends
               { delim :: String
               , prefix :: String
               , compilerFlavor :: CompilerFlavor
               , compilerVersion :: Version
               , versionInfo :: VersionInfo
               , cabalFilePath :: FilePath
               }
               | Modules
               { delim :: String
               , prefix :: String
               , compilerFlavor :: CompilerFlavor
               , compilerVersion :: Version
               , moduleInfo :: ModuleInfo
               , cabalFilePath :: FilePath
               } deriving (Data, Typeable, Show)

lowercase :: String -> String
lowercase = map toLower

depends_ :: CmdOpts
depends_ = Depends { delim = " "
                   , prefix = ""
                   , compilerFlavor = enum [ GHC &= help "ghc"
                                           , NHC &= help "nhc"
                                           , YHC &= help "yhc"
                                           , Hugs &= help "hugs"
                                           , HBC &= help "hbc"
                                           , Helium &= help "helium"
                                           , JHC &= help "jhc"
                                           , LHC &= help "lhc"
                                           , UHC &= help "uhc"
                                           , OtherCompiler "" &= help ""
                                           ]
                   , compilerVersion = Version { versionBranch = []
                                               , versionTags = [] }
                   , versionInfo = enum [ NotDisplayed &= help "Version info not displayed"
                                        , Displayed &= help "Version info displayed"
                                        , DisplayedInSquareBrackets &= help "Version info displayed in square brackets"
                                        ]
                   , cabalFilePath = "" &= typFile &= args
                   } &= help "List dependencies"
          
modules :: CmdOpts
modules = Modules { delim = " "
                  , prefix = ""
                  , compilerFlavor = enum [ GHC &= help "ghc"
                                          , NHC &= help "nhc"
                                          , YHC &= help "yhc"
                                          , Hugs &= help "hugs"
                                          , HBC &= help "hbc"
                                          , Helium &= help "helium"
                                          , JHC &= help "jhc"
                                          , LHC &= help "lhc"
                                          , UHC &= help "uhc"
                                          , OtherCompiler "" &= help ""
                                          ]
                  , compilerVersion = Version { versionBranch = []
                                              , versionTags = []
                                              }
                  , moduleInfo = enum [ ExposedAndNonExposed &= help "Both exposed and non-exposed modules"
                                      , Exposed &= help "Exposed modules"
                                      , NonExposed &= help "Non-exposed modules"
                                      ]
                  , cabalFilePath = "" &= typFile &= args
                  } &= help "List modules"

-- | display dependency and optionally version information
depVerStr :: VersionInfo -> -- ^ controls the optional display of dependency version information
             Dependency ->  -- ^ the dependency
             String        -- ^ output dependency string and optionally dependency version information 
depVerStr vi (Dependency (PackageName pn) vr) = case vi of
  NotDisplayed -> pn
  Displayed -> pn ++ " " ++ show vr
  DisplayedInSquareBrackets -> pn ++ "[" ++ show vr ++ "]"

-- | list dependencies in the cabal file
lsdeps :: VersionInfo -> -- ^ optionally list dependency version information
          String ->      -- ^ delimiter string between dependencies
          String ->      -- ^ optional prefix string before each dependency
          Library ->     -- ^ library
          IO ()          -- ^ return nothing
lsdeps vi d p l = 
  let ds = (map (depVerStr vi) .  targetBuildDepends . libBuildInfo) l
      prefixedDeps = map (p ++) ds
      deps = L.intercalate d prefixedDeps
  in
   do
     putStrLn deps
     return ()

nonExposedModules :: Library -> 
                     [ModuleName] 
nonExposedModules = otherModules . libBuildInfo

mods :: String ->                   -- ^ delimiter string between dependencies
        String ->                   -- ^ optional prefix string before each dependency
        Library ->                  -- ^ library
        (Library -> [ModuleName]) -> -- ^ function to obtain list of modules from Library
        IO ()                       -- ^ return nothing
mods d p l f =
  let ds = map (L.intercalate "." . components) (f l)
      prefixedMods = map (p ++) ds
      ms = L.intercalate d prefixedMods
  in
   putStrLn ms

-- | list modules in the cabal file
lsmods :: ModuleInfo ->  -- ^ optionally list exposed and/or non-exposed modules
          String ->      -- ^ delimiter string between dependencies
          String ->      -- ^ optional prefix string before each dependency
          Library ->     -- ^ library
          IO ()          -- ^ return nothing
lsmods mi d p l =
  case mi of
    ExposedAndNonExposed -> 
      do
        mods d p l exposedModules
        mods d p l nonExposedModules
        return ()
    Exposed -> mods d p l exposedModules
    NonExposed -> mods d p l nonExposedModules
    
-- | Replace all occurrances of escaped line feed, carriage return and 
-- tab characters.
escapeReplace :: String -> -- ^ input string
                 String   -- ^ output string with escaped characters replaced with corresponding characters
escapeReplace = replace "\\n" "\n" . replace "\\r" "\r" . replace "\\t" "\t"

compVer :: CompilerFlavor ->
           Version ->
           IO CompilerId
compVer c v@(Version [] []) =
  do
    let comp = (lowercase . show) c
    eitherH <- (E.try :: IO String -> IO (Either SomeException String)) $ readProcess comp ["--version"] []
    case eitherH of
      Left err ->
        do
          putStrLn $ "Failed to obtain " ++ (lowercase . show) c ++ " version: " ++ show err
          return $ CompilerId c v
      Right vs ->
        do
          let vw = words (vs =~ "[Vv]ersion [0-9]+([.][0-9])+" :: String)
          let ns = if length vw == 2
                   then (words . replace "." " " . last) vw
                   else []
          eitherR <- (E.try :: IO [Int] -> IO (Either SomeException [Int])) $ return $ map (read :: String -> Int) ns
          case eitherR of
            Left _ ->
              return $ CompilerId c Version { versionBranch = [] 
                                            , versionTags = ns
                                            }
            Right is ->
              return $ CompilerId c Version { versionBranch = is 
                                            , versionTags = ns
                                            }
compVer c v@Version {} =
  return $ CompilerId c v

-- | Process command line options
processOpts :: CmdOpts -> -- ^ command line options
               IO Int    -- ^ return 0 on success, 1 on failure
processOpts cmdOpts =
  do
    let inputFilePath = cabalFilePath cmdOpts
    cv <- compVer (compilerFlavor cmdOpts) (compilerVersion cmdOpts)
    eitherH <- E.try $ readPackageDescription verbose inputFilePath
    case eitherH of
      Left err ->
        putStrLn "Failed to open output file " >> ioError err >> return 1
      Right genericPackageDescription ->
        let eitherP = finalizePackageDescription
                      [ (FlagName "small_base", True)
                      ]
                      (\_ -> True)
                      buildPlatform
                      cv
                      [] 
                      genericPackageDescription
        in
         case eitherP of
           Left ds ->
             let missingDeps = (L.unwords . map show) ds
             in
              do
                putStrLn $ "Missing deps: " ++ missingDeps
                return 1
           Right (pd, _) ->
             case cmdOpts of
               Depends {} ->
                 do
                   let
                     d = escapeReplace $ delim cmdOpts
                     p = escapeReplace $ prefix cmdOpts
                     vi = versionInfo cmdOpts
                   withLib pd (lsdeps vi d p)
                   return 0
               Modules {} ->
                 do
                   let
                     d = escapeReplace $ delim cmdOpts
                     p = escapeReplace $ prefix cmdOpts
                     mi = moduleInfo cmdOpts
                   withLib pd (lsmods mi d p)
                   return 0

main :: IO Int
main = do
  cmdOpts <- cmdArgs $ modes [ depends_
                            , modules
                            ] 
             &= program "cquery"
             &= summary "cquery"
  processOpts cmdOpts
