-- HSApp: a simple Cocoa app in Haskell
--
-- Tying all components together

import qualified App            as App
import qualified AppDelegate    as Delegate
import qualified HSKKController as Controller

main :: IO ()
main
  = do
    { App.objc_initialise
    ; Delegate.objc_initialise
    ; Controller.objc_initialise
    ; App.main
    }
