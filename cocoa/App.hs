{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, QuasiQuotes, TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                    #-}
-- HSApp: a simple Cocoa app in Haskell
--
-- Main application module entering AppKit's application framework

module App (main, objc_initialise) where
import Constants
import Messaging

import Language.C.Inline.ObjC
import Language.C.Quote.ObjC
import System.IO.Unsafe

objc_import ["<Cocoa/Cocoa.h>", "<InputMethodKit/InputMethodKit.h>"]

defineClass "NSObject" Nothing
idMarshaller ''NSObject

defineClass "NSApplication" (Just ''NSObject)
idMarshaller ''NSApplication

defineClass "NSBundle" (Just ''NSObject)
idMarshaller ''NSBundle

defineClass "IMKServer" (Just ''NSObject)
idMarshaller ''IMKServer

defineClass "NSDictionary" (Just ''NSObject)
idMarshaller ''NSDictionary

defineClass "NSString" (Just ''NSObject)
idMarshaller ''NSString

mainBundle :: NSBundle
mainBundle = unsafePerformIO $(objc [] $ Class ''NSBundle <: [cexp| [NSBundle mainBundle] |] )
{-# NOINLINE mainBundle #-}

defineSelector newSelector { selector = "bundleIdentifier"
                           , reciever = (''NSBundle, "bundle")
                           , returnType = Just [t| String |]
                           , definition = [cexp| [bundle bundleIdentifier] |]
                           }

defineSelector newSelector { selector = "bundle"
                           , reciever = (''IMKServer, "server")
                           , returnType = Just [t| NSBundle |]
                           , definition = [cexp| [server bundle] |]
                           }

serverWithNameBundleIdentifier :: String -> String -> IO IMKServer
serverWithNameBundleIdentifier name ident
  = $(objc ['name :> ''String, 'ident :> ''String] $
      Class ''IMKServer <: [cexp| [[IMKServer alloc] initWithName: name bundleIdentifier: ident] |])

defineSelector newSelector { selector = "loadNibNamedOwner"
                           , reciever = (''NSBundle, "bundle")
                           , arguments = ["nib" :>>: ''String, "owner" :>.>: ''NSObject]
                           , definition = [cexp| [bundle loadNibNamed: nib
                                                                owner: owner
                                                      topLevelObjects: nil] |]
                           }

sharedApplication :: NSApplication
sharedApplication = unsafePerformIO $
  $(objc [] $ Class ''NSApplication <:
    [cexp| [NSApplication sharedApplication] |])
{-# NOINLINE sharedApplication #-}

defineSelector newSelector { selector = "run"
                           , reciever = (''NSApplication, "app")
                           , definition = [cexp| [app run] |]
                           }

defineSelector newSelector { selector = "description"
                           , reciever = (''NSObject, "obj")
                           , returnType = Just [t| String |]
                           , definition = [cexp| [obj description] |]
                           }

nsLog :: String -> IO ()
nsLog str = $(objc ['str :> ''String] $ void [cexp| NSLog(@"%@", str) |])

main :: IO ()
main = do
  ident <- mainBundle # bundleIdentifier
  nsLog $ "identifier: " ++ ident
  server <- serverWithNameBundleIdentifier connName ident
  mainBundle # loadNibNamedOwner "MainMenu" sharedApplication
  nsLog . ("bundle: " ++ ) =<< server # bundle #. description
  sharedApplication # run
  return ()

objc_emit
