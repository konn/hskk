module Constants where
import Data.Maybe           (fromMaybe)
import Text.InputMethod.SKK hiding (lookup)

connName :: String
connName = "HSKK_Connection"

candidateKeys :: String
candidateKeys = "asdfjkl"

lookupMode :: String -> Maybe ConvMode
lookupMode = flip lookup $ [(hiraganaModeKey, Hiragana)
                           ,(katakanaModeKey, Katakana)
                           ,(hankakuModeKey, HankakuKatakana)
                           ,(asciiModeKey, Ascii)
                           ]

modeToString :: ConvMode -> String
modeToString Hiragana = hiraganaModeKey
modeToString Katakana = katakanaModeKey
modeToString HankakuKatakana = hankakuModeKey
modeToString Ascii = asciiModeKey

hiraganaModeKey, katakanaModeKey, hankakuModeKey, asciiModeKey :: String
hiraganaModeKey = "com.apple.inputmethod.Japanese.Hiragana"
katakanaModeKey = "com.apple.inputmethod.Japanese.Katakana"
hankakuModeKey  = "com.apple.inputmethod.Japanese.HalfWidthKana"
asciiModeKey = "com.apple.inputmethod.Roman"

otherDicsKey :: String
otherDicsKey = "otherDics"

inlineCountKey :: String
inlineCountKey = "inlineCandidateCount"

candidateLabelKey :: String
candidateLabelKey = "candidateLabel"

userDicKey :: String
userDicKey = "userDicPath"

userDefaultName :: String
userDefaultName = "group.konn-san.com.inputmethod.hSKK"

miscDicKindKey :: String
miscDicKindKey = "kind"

miscDicLocationKey :: String
miscDicLocationKey = "location"
