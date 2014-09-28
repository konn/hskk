module KeyFlags where
import Control.Applicative ((<$>))
import Data.Bits
import Data.Char           (Char)
import Data.Char           (isAscii)
import Data.Char           (isControl)
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)
import Foreign.C.Types

type Modifiers = CULong
type Mask = CULong

data ModifierKey = AlphaShift
                 | Shift
                 | Control
                 | Alternate
                 | Command
                 | NumericPad
                 | HelpM
                 | Function
                 | Independent
                 deriving (Read, Show, Eq, Ord, Enum)

maskDic :: [(ModifierKey, Modifiers)]
maskDic = [(AlphaShift, alphaShiftMask)
          ,(Shift, shiftMask)
          ,(Control, controlMask)
          ,(AlphaShift, alternateMask)
          ,(Command, commandMask)
          ,(NumericPad, numericPadMask)
          ,(HelpM, helpMask)
          ,(Function, functionMask)
          ,(Independent, independentMask)
          ]

alphaShiftMask :: Modifiers
alphaShiftMask = 1 `shiftL` 16

shiftMask :: Modifiers
shiftMask = 1 `shiftL` 17

controlMask :: Modifiers
controlMask = 1 `shiftL` 18

alternateMask :: Modifiers
alternateMask = 1 `shiftL` 19

commandMask :: Modifiers
commandMask = 1 `shiftL` 20

numericPadMask :: Modifiers
numericPadMask = 1 `shiftL` 21

helpMask :: Modifiers
helpMask = 1 `shiftL` 22

functionMask :: Modifiers
functionMask = 1 `shiftL` 23

independentMask :: Modifiers
independentMask = 0xffff0000

hasBit :: Mask -> Modifiers -> Bool
hasBit a b = a .&. b == a

decodeModifiers :: Modifiers -> [ModifierKey]
decodeModifiers b = [k | (k, m) <- maskDic, hasBit m b]

decodeFunctionKey :: Char -> Maybe FunctionKey
decodeFunctionKey = flip lookup functionKeyDic

data Key = Special FunctionKey | Normal Char
         deriving (Read, Show, Eq, Ord)

decodeKey :: Char -> Maybe Key
decodeKey c
  | not (isControl c) || c == '\t' = Just $ Normal c
  | otherwise = Special <$> decodeFunctionKey c

data FunctionKey = UpArrow
                 | DownArrow
                 | LeftArrow
                 | RightArrow
                 | Fn Int
                 | Insert
                 | Delete
                 | Home
                 | Begin
                 | End
                 | PageUp
                 | PageDown
                 | PrintScreen
                 | ScrollLock
                 | Pause
                 | SysReq
                 | Break
                 | Reset
                 | Stop
                 | Menu
                 | User
                 | System
                 | Print
                 | ClearLine
                 | ClearDisplay
                 | InsertLine
                 | DeleteLine
                 | InsertChar
                 | DeleteChar
                 | Prev
                 | Next
                 | Select
                 | Execute
                 | Undo
                 | Redo
                 | Find
                 | HelpF
                 | ModeSwitch
                 deriving (Read, Show, Eq, Ord)

funs :: [FunctionKey]
funs = [UpArrow
       , DownArrow
       , LeftArrow
       , RightArrow
       ] ++ [ Fn n | n <- [1..35]] ++
       [ Insert
       , Delete
       , Home
       , Begin
       , End
       , PageUp
       , PageDown
       , PrintScreen
       , ScrollLock
       , Pause
       , SysReq
       , Break
       , Reset
       , Stop
       , Menu
       , User
       , System
       , Print
       , ClearLine
       , ClearDisplay
       , InsertLine
       , DeleteLine
       , InsertChar
       , DeleteChar
       , Prev
       , Next
       , Select
       , Execute
       , Undo
       , Redo
       , Find
       , HelpF
       , ModeSwitch
       ]

instance Enum FunctionKey where
  toEnum = (funs !!)
  fromEnum = fromJust . flip elemIndex funs
  enumFrom s = dropWhile (/= s) funs
  enumFromTo s t = takeWhile (/= t) (dropWhile (/= s) funs) ++ [t]
  enumFromThen s t =
    map toEnum $ enumFromThenTo (fromJust (elemIndex s funs)) (fromJust $ elemIndex t funs) 71
  enumFromThenTo s t t' =
    map toEnum $ enumFromThenTo
      (fromJust $ elemIndex s funs)
      (fromJust $ elemIndex t funs)
      (min 71 $ fromJust $ elemIndex t' funs)

functionKeyDic :: [(Char, FunctionKey)]
functionKeyDic = [('\r', InsertLine), ('\b', Delete), ('\a', Undo)] ++ zip ['\xF700'..] funs
