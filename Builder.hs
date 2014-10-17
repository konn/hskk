{-# LANGUAGE RecordWildCards #-}
module Main where
import           Control.Applicative                   ((<$>))
import           Control.Monad                         (when)
import           Data.List                             (intersperse, nub)
import           Data.Maybe
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Distribution.Cab.Sandbox
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import qualified GHC.Paths                             as Path
import qualified System.Directory                      as D
import           System.Environment                    (getArgs)
import           System.Environment                    (withArgs)

build :: FilePath
build = "_build"

srcDirs :: [FilePath]
srcDirs = ["src", "cocoa"]

data AppSetting = AppSetting { app      :: String
                             , ldFlags  :: [String]
                             , packDBs  :: [FilePath]
                             , includes :: [FilePath]
                             } deriving (Read, Show, Eq, Ord)

main :: IO ()
main = do
  cab <- head . filter ((== ".cabal") . takeExtension) <$> (D.getDirectoryContents =<< D.getCurrentDirectory)
  packdesc <- readPackageDescription silent cab
  msandbox <- getSandbox
  let deps0 = buildDepends $ packageDescription packdesc
      exes  = map (condTreeData . snd) $ condExecutables packdesc
      name  = fst $ last $ condExecutables packdesc
      deps1 = condTreeConstraints $ snd $ last $ condExecutables packdesc
      dbs   = maybeToList msandbox
      incs  = map (</> "include") [Path.libdir, Path.libdir ++ "/../.."]
      targ = last exes
      deps = deps0 ++ deps1 ++ targetBuildDepends (buildInfo targ)
      packs = prefixing "-package" $ nub $ map getPackageName deps
      frams = prefixing "-framework" $ frameworks $ buildInfo targ
      opts  = ["-optl-ObjC", "-threaded"]
      setting = AppSetting { app = name
                           , ldFlags = packs ++ frams ++ opts
                           , packDBs = dbs
                           , includes = incs
                           }
    in shakeArgs shakeOptions{shakeFiles=build} $ buildWith setting

prefixing :: a -> [a] -> [a]
prefixing _   [] = []
prefixing str xs = str : intersperse str xs

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName name) _) = name

buildWith :: AppSetting -> Rules ()
buildWith AppSetting {..} = do
  let dbs  = prefixing "-package-db" packDBs
      dest = app <.> "app"
      skeleton = build </> app <.> "app"
      exe = build </> app
      xcode = "xcode_proj" </> app </> app <.> "xcodeproj"
  want [dest]

  phony "clean" $ do
    putNormal "cleaning..."
    removeFilesAfter "" ["//*_objc.h","//*_objc.m","//*_stub.h"]
    xcodeRemove <- doesDirectoryExist $ "xcode_proj" </> app </> "build"
    when xcodeRemove $
      removeFilesAfter ("xcode_proj" </> app </> "build") ["//*"]
    remove <- doesDirectoryExist build
    when remove $
      removeFilesAfter build ["//*"]

    remove' <- doesDirectoryExist dest
    when remove $
      removeFilesAfter dest ["//*"]

  dest *> \out -> do
    putNormal "setting up for bundle..."
    need [skeleton, exe]
    remove <- doesDirectoryExist dest
    when remove $ removeFilesAfter dest ["//*"]
    () <- cmd "mv" "-f" skeleton out
    copyFile' exe (out </> "Contents" </> "MacOS" </> app)
    copyFile' ("data/AppIcon.icns") (out </> "Contents" </> "Resources" </> "AppIcon.icns")
    putNormal "Application bundle successfully compiled."

  build </> app <.> "app" *> \out -> do
    need [xcode]
    putNormal "compiling xcodeproj..."
    () <- cmd "xcodebuild -project" xcode
    cmd "mv" "-f" ("xcode_proj" </> app </> "build/Release" </> app <.> "app") out

  build </> app *> \out -> do
    hss0 <- getDirectoryFiles "cocoa" ["//*.hs"]
    hss1 <- getDirectoryFiles "src" ["//*.hs"]
    let objs = [build </> hs -<.> "o" | hs <- hss0 ++ hss1, hs `notElem` ["Setup.hs", "Builder.hs"]]
    need objs
    addObs <- getDirectoryFiles "" ["_build//*_objc.o"]
    putNormal $ "linking executable... "
    command_ [] "ghc" $ map ("-i"++) srcDirs ++
      [ "-o", out, "-odir", build, "-hidir", build, "-stubdir"
      , build, "-outputdir", build] ++ objs ++ addObs ++ ldFlags ++ dbs

  ["_build//*.o", "_build//*.hi"] &*> \ [out, hi] -> do
    putNormal $ "building object: " ++ out
    let hs0 = dropDirectory1 $ out -<.> "hs"
    isSrc <- doesFileExist $ "src" </> hs0
    let hs | isSrc     = "src" </> hs0
           | otherwise = "cocoa" </> hs0
        dep = out -<.> "dep"
        obcBase = dropExtension hs ++ "_objc"
        obcm = obcBase <.> "m"
        obch = obcBase <.> "h"
    command_ [] "ghc" $ map ("-i"++) srcDirs ++ [ "-M", "-odir", build, "-hidir", build
                        , "-dep-suffix", "", "-dep-makefile", dep, hs] ++ dbs
    needMakefileDependencies dep
    command_ [] "ghc" $ map ("-i"++) srcDirs ++ ["-c", hs, "-dynamic"
                      , "-o", out] ++ dbs ++ [ "-odir", build, "-hidir"
                      , build, ("-i" ++ build)]
    gen'd <- doesFileExist obcm
    when gen'd $ do
      putNormal $ "compiling gen'd file: " ++ obcm
      () <- cmd "mv" obcm (build </> dropDirectory1 obcm)
      () <- cmd "mv" obch (build </> dropDirectory1 obch)
      () <- cmd "cc" "-fobjc-arc"
        (map ("-I"++) includes)
        "-c -o" (build </> dropDirectory1 obcBase <.> "o") (build </> dropDirectory1 obcm)
      cmd "rm" "-f" $ dropExtension (dropDirectory1 out) ++ "_stub" <.> "h"
    putNormal $ "built: " ++ out
