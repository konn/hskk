module Constants where
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

hiraganaModeKey, katakanaModeKey, hankakuModeKey, asciiModeKey :: String
hiraganaModeKey = "com.apple.inputmethod.Japanese.Hiragana"
katakanaModeKey = "com.apple.inputmethod.Japanese.Katakana"
hankakuModeKey  = "com.apple.inputmethod.Japanese.HalfWidthKana"
asciiModeKey = "com.apple.inputmethod.Roman"
