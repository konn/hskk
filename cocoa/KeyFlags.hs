{-# LANGUAGE DeriveDataTypeable, FlexibleContexts                          #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies                                                  #-}
module KeyFlags
       (Masked(..), testCode, compatible, Keyboard(..),
        decodeKeyboard, decodeModifiers, Modifier(..), encodeMask,
        decodeFunctionKey, functionKeys, isAlphabeticModifier, _Char, _JIS) where
import KeyFlags.Macros

import           Control.Applicative ((<$>))
import           Control.Lens        (makePrisms)
import           Data.Bits
import qualified Data.Text.IO        as T
import           Foreign.C.Types
import           Language.Haskell.TH (runIO)

do table <- runIO $ procLR . parse <$> T.readFile "data/keycodes.dat"
   defineKeyCode "Keyboard" [t| CLong |] table

makePrisms ''Keyboard

decodeModifiers :: Mask Modifier -> [Modifier]
decodeModifiers b = [m | m <- modifiers, testCode m b]

encodeMask :: (Masked a, Num (Mask a)) => [a] -> Mask a
encodeMask = foldl (.|.) 0 . map toMask

data Modifier = AlphaShift
              | Shift
              | Control
              | Alternate
              | Command
              | NumericPad
              | HelpM
              | Function
              | Independent
              deriving (Read, Show, Eq, Ord, Enum)

instance Masked Modifier where
  type Mask Modifier = CULong
  toMask AlphaShift  = alphaShiftMask
  toMask Alternate  = alternateMask
  toMask Shift       = shiftMask
  toMask Control = controlMask
  toMask Command = commandMask
  toMask NumericPad = numericPadMask
  toMask HelpM = helpMask
  toMask Function = functionMask
  toMask Independent = independentMask

isAlphabeticModifier :: Modifier -> Bool
isAlphabeticModifier a = compatible Shift a || compatible AlphaShift a

modifiers :: [Modifier]
modifiers = [ AlphaShift
            , Shift
            , Control
            , Alternate
            , Command
            , NumericPad
            , HelpM
            , Function
            , Independent
            ]
alphaShiftMask :: Mask Modifier
alphaShiftMask = 1 `shiftL` 16

shiftMask :: Mask Modifier
shiftMask = 1 `shiftL` 17

controlMask :: Mask Modifier
controlMask = 1 `shiftL` 18

alternateMask :: Mask Modifier
alternateMask = 1 `shiftL` 19

commandMask :: Mask Modifier
commandMask = 1 `shiftL` 20

numericPadMask :: Mask Modifier
numericPadMask = 1 `shiftL` 21

helpMask :: Mask Modifier
helpMask = 1 `shiftL` 22

functionMask :: Mask Modifier
functionMask = 1 `shiftL` 23

independentMask :: Mask Modifier
independentMask = 0xffff0000

functionKeys :: [Keyboard]
functionKeys = case break (==Home) funs0 of
  (as, bs) -> as ++ head bs : drop 2 bs

funs0 :: [Keyboard]
funs0 = [CursorUp
       , CursorDown
       , CursorLeft
       , CursorRight
       ] ++ [ Fn n | n <- [1..35]] ++
       [ PcInsert
       , Delete
       , Home
       , undefined
       , End
       , Pageup
       , Pagedown
       , PcPrintscreen
       , PcScrolllock
       ]

decodeFunctionKey :: Char -> Maybe Keyboard
decodeFunctionKey = flip lookup functionKeyDic

functionKeyDic :: [(Char, Keyboard)]
functionKeyDic =
  case break ((== Home) . snd) $ zip ['\xF700'..] funs0 of
    (as, bs) -> as ++ head bs : drop 2 bs

