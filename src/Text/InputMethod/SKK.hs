{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs              #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, ViewPatterns                                #-}
module Text.InputMethod.SKK
       (module Text.InputMethod.SKK.Dictionary,
        toInput, parseDictionary,
        -- * Converters
        romanConv, defKanaTable, convertTest,
        -- * Data-types and lenses
        KanaEntry(..),ConvMode(..), ConvState(..),
        _Converted, _NoHit, _InProgress, newInput,
        hiraConv, kataConv, hanKataConv, nextState,
        KanaTable(..), kanaDic) where
import           Control.Applicative             ((<$>))
import           Control.Arrow                   (second)
import           Control.Lens                    (makeLenses, makePrisms)
import           Control.Lens                    (makeWrapped, (^.))
import           Control.Lens                    ((%~), (^?), _head, _tail)
import           Control.Lens                    (each)
import           Control.Lens                    (view)
import           Control.Monad                   (liftM)
import           Data.Attoparsec.Text            (parseOnly)
import qualified Data.ByteString.Char8           as BS
import           Data.Char                       (isAlpha, isAscii)
import           Data.Data                       (Data, Typeable)
import           Data.Monoid                     ((<>))
import           Data.Monoid                     (Monoid (..))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Trie
import qualified Data.Trie                       as Trie
import           FRP.Ordrea                      (start)
import           FRP.Ordrea                      (Event, SignalGen)
import           FRP.Ordrea                      (eventFromList,
                                                  newExternalEvent,
                                                  triggerExternalEvent)
import           FRP.Ordrea                      (eventToBehavior, justE)
import           FRP.Ordrea                      (mapAccumE, networkToList)
import           FRP.Ordrea                      (externalE)
import           Text.InputMethod.SKK.Dictionary

data KanaEntry = KanaEntry { _hiraConv    :: T.Text
                           , _kataConv    :: T.Text
                           , _hanKataConv :: T.Text
                           , _nextState   :: Maybe Char
                           } deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaEntry

data ConvState = Converted T.Text | NoHit | InProgress BS.ByteString
               deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''ConvState
makePrisms ''ConvState

instance Monoid ConvState where
  mempty = NoHit
  mappend NoHit a = a
  mappend a NoHit = a
  mappend c@Converted{} Converted{}  = c
  mappend c@Converted{} InProgress{} = c
  mappend InProgress{} c@Converted{} = c
  mappend (InProgress c) (InProgress d) = InProgress (c <> d)

data ConvMode = Hiragana | Katakana | HankakuKatakana
              deriving (Read, Show, Eq, Ord, Data, Typeable)
makeLenses ''ConvMode
makePrisms ''ConvMode

newtype KanaTable = KanaTable { _kanaDic :: Trie KanaEntry }
                  deriving (Show, Eq, Typeable)
makeLenses ''KanaTable
makeWrapped ''KanaTable

toInput :: T.Text -> Input
toInput "" = Input "" Nothing
toInput str
  | T.length str == 1
    = Input str Nothing
  | isAlpha (T.last str) && isAscii (T.last str)
    = Input (T.init str) (Just $ T.last str)
  | otherwise =  Input str Nothing

parseDictionary :: T.Text -> Either String Dictionary
parseDictionary = parseOnly dictionary

convChar :: Functor f => ConvMode
         -> (T.Text -> f T.Text) -> KanaEntry -> f KanaEntry
convChar Hiragana = hiraConv
convChar Katakana = kataConv
convChar HankakuKatakana = hanKataConv

romanConv :: KanaTable -> ConvMode -> Event Char -> SignalGen (Event [ConvState])
romanConv (KanaTable dic) mode input = mapAccumE ("", dic) (flip upd <$> input)
  where
    upd (pre, ps) (BS.singleton -> inp) =
      let (mans, ps') = lookupBy (,) inp ps
          addPre | T.null pre = id
                 | otherwise  = (Converted pre :)
      in case mans of
        Just a ->
          let (ps'', out) = case a ^. nextState of
                Nothing -> (ps', a ^. convChar mode)
                Just st -> (prefixes (BS.singleton st) dic,
                            a ^. convChar mode)
          in if Trie.null ps'
             then  (("", ps''), [Converted out])
             else ((out, ps''), [InProgress inp])
        Nothing | Trie.null ps' ->
                    if Trie.null (submap inp dic)
                    then ((pre, dic), addPre [NoHit])
                    else second addPre $ upd (pre, dic) $ BS.head inp
                | otherwise     -> (("", ps'), [InProgress inp])

prefixes :: BS.ByteString -> Trie a -> Trie a
prefixes = lookupBy ((snd .) . (,))

convertTest :: ConvMode -> String -> IO T.Text
convertTest mode inp = liftM (T.concat . head) . networkToList (length inp) $ eventToBehavior <$> inputText mode inp

inputText :: ConvMode -> String -> SignalGen (Event T.Text)
inputText mode inp = do
  inps <- eventFromList [inp]
  ev <- romanConv defKanaTable mode inps
  return $ justE $ (^? _Converted) . mconcat <$> ev

newInput :: IO (Char -> IO [ConvState])
newInput = do
  eev <- newExternalEvent
  step <- start $ do
    input <- externalE eev
    fmap head . eventToBehavior <$> romanConv defKanaTable Katakana input
  return $ \inp -> triggerExternalEvent eev inp >> step

defKanaTable :: KanaTable
defKanaTable = KanaTable $ fromList
               [(",", KanaEntry "、" "、" "､" Nothing)
               ,("-", KanaEntry "ー" "ー" "-" Nothing)
               ,(".", KanaEntry "。" "。" "｡" Nothing)
               ,("[", KanaEntry "「" "「" "｢" Nothing)
               ,("]", KanaEntry "」" "」" "｣" Nothing)
               ,("a", KanaEntry "あ" "ア" "ｱ" Nothing)
               ,("ba", KanaEntry "ば" "バ" "ﾊﾞ" Nothing)
               ,("bb", KanaEntry "っ" "ッ" "ｯ" $ Just 'b')
               ,("be", KanaEntry "べ" "ベ" "ﾍﾞ" Nothing)
               ,("bi", KanaEntry "び" "ビ" "ﾋﾞ" Nothing)
               ,("bo", KanaEntry "ぼ" "ボ" "ﾎﾞ" Nothing)
               ,("bu", KanaEntry "ぶ" "ブ" "ﾌﾞ" Nothing)
               ,("bya", KanaEntry "びゃ" "ビャ" "ﾋﾞｬ" Nothing)
               ,("bye", KanaEntry "びぇ" "ビェ" "ﾋﾞｪ" Nothing)
               ,("byi", KanaEntry "びぃ" "ビィ" "ﾋﾞｨ" Nothing)
               ,("byo", KanaEntry "びょ" "ビョ" "ﾋﾞｮ" Nothing)
               ,("byu", KanaEntry "びゅ" "ビュ" "ﾋﾞｭ" Nothing)
               ,("cc", KanaEntry "っ" "ッ" "ｯ" $ Just 'c')
               ,("cha", KanaEntry "ちゃ" "チャ" "ﾁｬ" Nothing)
               ,("che", KanaEntry "ちぇ" "チェ" "ﾁｪ" Nothing)
               ,("chi", KanaEntry "ち" "チ" "ﾁ" Nothing)
               ,("cho", KanaEntry "ちょ" "チョ" "ﾁｮ" Nothing)
               ,("chu", KanaEntry "ちゅ" "チュ" "ﾁｭ" Nothing)
               ,("cya", KanaEntry "ちゃ" "チャ" "ﾁｬ" Nothing)
               ,("cye", KanaEntry "ちぇ" "チェ" "ﾁｪ" Nothing)
               ,("cyi", KanaEntry "ちぃ" "チィ" "ﾁｨ" Nothing)
               ,("cyo", KanaEntry "ちょ" "チョ" "ﾁｮ" Nothing)
               ,("cyu", KanaEntry "ちゅ" "チュ" "ﾁｭ" Nothing)
               ,("da", KanaEntry "だ" "ダ" "ﾀﾞ" Nothing)
               ,("dd", KanaEntry "っ" "ッ" "ｯ" $ Just 'd')
               ,("de", KanaEntry "で" "デ" "ﾃﾞ" Nothing)
               ,("dha", KanaEntry "でゃ" "デャ" "ﾃﾞｬ" Nothing)
               ,("dhe", KanaEntry "でぇ" "デェ" "ﾃﾞｪ" Nothing)
               ,("dhi", KanaEntry "でぃ" "ディ" "ﾃﾞｨ" Nothing)
               ,("dho", KanaEntry "でょ" "デョ" "ﾃﾞｮ" Nothing)
               ,("dhu", KanaEntry "でゅ" "デュ" "ﾃﾞｭ" Nothing)
               ,("di", KanaEntry "ぢ" "ヂ" "ﾁﾞ" Nothing)
               ,("do", KanaEntry "ど" "ド" "ﾄﾞ" Nothing)
               ,("du", KanaEntry "づ" "ヅ" "ﾂﾞ" Nothing)
               ,("dya", KanaEntry "ぢゃ" "ヂャ" "ﾁﾞｬ" Nothing)
               ,("dye", KanaEntry "ぢぇ" "ヂェ" "ﾁﾞｪ" Nothing)
               ,("dyi", KanaEntry "ぢぃ" "ヂィ" "ﾁﾞｨ" Nothing)
               ,("dyo", KanaEntry "ぢょ" "ヂョ" "ﾁﾞｮ" Nothing)
               ,("dyu", KanaEntry "ぢゅ" "ヂュ" "ﾁﾞｭ" Nothing)
               ,("e", KanaEntry "え" "エ" "ｴ" Nothing)
               ,("fa", KanaEntry "ふぁ" "ファ" "ﾌｧ" Nothing)
               ,("fe", KanaEntry "ふぇ" "フェ" "ﾌｪ" Nothing)
               ,("ff", KanaEntry "っ" "ッ" "ｯ" $ Just 'f')
               ,("fi", KanaEntry "ふぃ" "フィ" "ﾌｨ" Nothing)
               ,("fo", KanaEntry "ふぉ" "フォ" "ﾌｫ" Nothing)
               ,("fu", KanaEntry "ふ" "フ" "ﾌ" Nothing)
               ,("fya", KanaEntry "ふゃ" "フャ" "ﾌｬ" Nothing)
               ,("fye", KanaEntry "ふぇ" "フェ" "ﾌｪ" Nothing)
               ,("fyi", KanaEntry "ふぃ" "フィ" "ﾌｨ" Nothing)
               ,("fyo", KanaEntry "ふょ" "フョ" "ﾌｮ" Nothing)
               ,("fyu", KanaEntry "ふゅ" "フュ" "ﾌｭ" Nothing)
               ,("ga", KanaEntry "が" "ガ" "ｶﾞ" Nothing)
               ,("ge", KanaEntry "げ" "ゲ" "ｹﾞ" Nothing)
               ,("gg", KanaEntry "っ" "ッ" "ｯ" $ Just 'g')
               ,("gi", KanaEntry "ぎ" "ギ" "ｷﾞ" Nothing)
               ,("go", KanaEntry "ご" "ゴ" "ｺﾞ" Nothing)
               ,("gu", KanaEntry "ぐ" "グ" "ｸﾞ" Nothing)
               ,("gya", KanaEntry "ぎゃ" "ギャ" "ｷﾞｬ" Nothing)
               ,("gye", KanaEntry "ぎぇ" "ギェ" "ｷﾞｪ" Nothing)
               ,("gyi", KanaEntry "ぎぃ" "ギィ" "ｷﾞｨ" Nothing)
               ,("gyo", KanaEntry "ぎょ" "ギョ" "ｷﾞｮ" Nothing)
               ,("gyu", KanaEntry "ぎゅ" "ギュ" "ｷﾞｭ" Nothing)
               ,("ha", KanaEntry "は" "ハ" "ﾊ" Nothing)
               ,("he", KanaEntry "へ" "ヘ" "ﾍ" Nothing)
               ,("hh", KanaEntry "っ" "ッ" "ｯ" $ Just 'h')
               ,("hi", KanaEntry "ひ" "ヒ" "ﾋ" Nothing)
               ,("ho", KanaEntry "ほ" "ホ" "ﾎ" Nothing)
               ,("hu", KanaEntry "ふ" "フ" "ﾌ" Nothing)
               ,("hya", KanaEntry "ひゃ" "ヒャ" "ﾋｬ" Nothing)
               ,("hye", KanaEntry "ひぇ" "ヒェ" "ﾋｪ" Nothing)
               ,("hyi", KanaEntry "ひぃ" "ヒィ" "ﾋｨ" Nothing)
               ,("hyo", KanaEntry "ひょ" "ヒョ" "ﾋｮ" Nothing)
               ,("hyu", KanaEntry "ひゅ" "ヒュ" "ﾋｭ" Nothing)
               ,("i", KanaEntry "い" "イ" "ｲ" Nothing)
               ,("ja", KanaEntry "じゃ" "ジャ" "ｼﾞｬ" Nothing)
               ,("je", KanaEntry "じぇ" "ジェ" "ｼﾞｪ" Nothing)
               ,("ji", KanaEntry "じ" "ジ" "ｼﾞ" Nothing)
               ,("jj", KanaEntry "っ" "ッ" "ｯ" $ Just 'j')
               ,("jo", KanaEntry "じょ" "ジョ" "ｼﾞｮ" Nothing)
               ,("ju", KanaEntry "じゅ" "ジュ" "ｼﾞｭ" Nothing)
               ,("jya", KanaEntry "じゃ" "ジャ" "ｼﾞｬ" Nothing)
               ,("jye", KanaEntry "じぇ" "ジェ" "ｼﾞｪ" Nothing)
               ,("jyi", KanaEntry "じぃ" "ジィ" "ｼﾞｨ" Nothing)
               ,("jyo", KanaEntry "じょ" "ジョ" "ｼﾞｮ" Nothing)
               ,("jyu", KanaEntry "じゅ" "ジュ" "ｼﾞｭ" Nothing)
               ,("ka", KanaEntry "か" "カ" "ｶ" Nothing)
               ,("ke", KanaEntry "け" "ケ" "ｹ" Nothing)
               ,("ki", KanaEntry "き" "キ" "ｷ" Nothing)
               ,("kk", KanaEntry "っ" "ッ" "ｯ" $ Just 'k')
               ,("ko", KanaEntry "こ" "コ" "ｺ" Nothing)
               ,("ku", KanaEntry "く" "ク" "ｸ" Nothing)
               ,("kya", KanaEntry "きゃ" "キャ" "ｷｬ" Nothing)
               ,("kye", KanaEntry "きぇ" "キェ" "ｷｪ" Nothing)
               ,("kyi", KanaEntry "きぃ" "キィ" "ｷｨ" Nothing)
               ,("kyo", KanaEntry "きょ" "キョ" "ｷｮ" Nothing)
               ,("kyu", KanaEntry "きゅ" "キュ" "ｷｭ" Nothing)
               ,("ma", KanaEntry "ま" "マ" "ﾏ" Nothing)
               ,("me", KanaEntry "め" "メ" "ﾒ" Nothing)
               ,("mi", KanaEntry "み" "ミ" "ﾐ" Nothing)
               ,("mm", KanaEntry "っ" "ッ" "ｯ" $ Just 'm')
               ,("mo", KanaEntry "も" "モ" "ﾓ" Nothing)
               ,("mu", KanaEntry "む" "ム" "ﾑ" Nothing)
               ,("mya", KanaEntry "みゃ" "ミャ" "ﾐｬ" Nothing)
               ,("mye", KanaEntry "みぇ" "ミェ" "ﾐｪ" Nothing)
               ,("myi", KanaEntry "みぃ" "ミィ" "ﾐｨ" Nothing)
               ,("myo", KanaEntry "みょ" "ミョ" "ﾐｮ" Nothing)
               ,("myu", KanaEntry "みゅ" "ミュ" "ﾐｭ" Nothing)
               ,("n", KanaEntry "ん" "ン" "ﾝ" Nothing)
               ,("n'", KanaEntry "ん" "ン" "ﾝ" Nothing)
               ,("na", KanaEntry "な" "ナ" "ﾅ" Nothing)
               ,("ne", KanaEntry "ね" "ネ" "ﾈ" Nothing)
               ,("ni", KanaEntry "に" "ニ" "ﾆ" Nothing)
               ,("nn", KanaEntry "ん" "ン" "ﾝ" Nothing)
               ,("no", KanaEntry "の" "ノ" "ﾉ" Nothing)
               ,("nu", KanaEntry "ぬ" "ヌ" "ﾇ" Nothing)
               ,("nya", KanaEntry "にゃ" "ニャ" "ﾆｬ" Nothing)
               ,("nye", KanaEntry "にぇ" "ニェ" "ﾆｪ" Nothing)
               ,("nyi", KanaEntry "にぃ" "ニィ" "ﾆｨ" Nothing)
               ,("nyo", KanaEntry "にょ" "ニョ" "ﾆｮ" Nothing)
               ,("nyu", KanaEntry "にゅ" "ニュ" "ﾆｭ" Nothing)
               ,("o", KanaEntry "お" "オ" "ｵ" Nothing)
               ,("pa", KanaEntry "ぱ" "パ" "ﾊﾟ" Nothing)
               ,("pe", KanaEntry "ぺ" "ペ" "ﾍﾟ" Nothing)
               ,("pi", KanaEntry "ぴ" "ピ" "ﾋﾟ" Nothing)
               ,("po", KanaEntry "ぽ" "ポ" "ﾎﾟ" Nothing)
               ,("pp", KanaEntry "っ" "ッ" "ｯ" $ Just 'p')
               ,("pu", KanaEntry "ぷ" "プ" "ﾌﾟ" Nothing)
               ,("pya", KanaEntry "ぴゃ" "ピャ" "ﾋﾟｬ" Nothing)
               ,("pye", KanaEntry "ぴぇ" "ピェ" "ﾋﾟｪ" Nothing)
               ,("pyi", KanaEntry "ぴぃ" "ピィ" "ﾋﾟｨ" Nothing)
               ,("pyo", KanaEntry "ぴょ" "ピョ" "ﾋﾟｮ" Nothing)
               ,("pyu", KanaEntry "ぴゅ" "ピュ" "ﾋﾟｭ" Nothing)
               ,("ra", KanaEntry "ら" "ラ" "ﾗ" Nothing)
               ,("re", KanaEntry "れ" "レ" "ﾚ" Nothing)
               ,("ri", KanaEntry "り" "リ" "ﾘ" Nothing)
               ,("ro", KanaEntry "ろ" "ロ" "ﾛ" Nothing)
               ,("rr", KanaEntry "っ" "ッ" "ｯ" $ Just 'r')
               ,("ru", KanaEntry "る" "ル" "ﾙ" Nothing)
               ,("rya", KanaEntry "りゃ" "リャ" "ﾘｬ" Nothing)
               ,("rye", KanaEntry "りぇ" "リェ" "ﾘｪ" Nothing)
               ,("ryi", KanaEntry "りぃ" "リィ" "ﾘｨ" Nothing)
               ,("ryo", KanaEntry "りょ" "リョ" "ﾘｮ" Nothing)
               ,("ryu", KanaEntry "りゅ" "リュ" "ﾘｭ" Nothing)
               ,("sa", KanaEntry "さ" "サ" "ｻ" Nothing)
               ,("se", KanaEntry "せ" "セ" "ｾ" Nothing)
               ,("sha", KanaEntry "しゃ" "シャ" "ｼｬ" Nothing)
               ,("she", KanaEntry "しぇ" "シェ" "ｼｪ" Nothing)
               ,("shi", KanaEntry "し" "シ" "ｼ" Nothing)
               ,("sho", KanaEntry "しょ" "ショ" "ｼｮ" Nothing)
               ,("shu", KanaEntry "しゅ" "シュ" "ｼｭ" Nothing)
               ,("si", KanaEntry "し" "シ" "ｼ" Nothing)
               ,("so", KanaEntry "そ" "ソ" "ｿ" Nothing)
               ,("ss", KanaEntry "っ" "ッ" "ｯ" $ Just 's')
               ,("su", KanaEntry "す" "ス" "ｽ" Nothing)
               ,("sya", KanaEntry "しゃ" "シャ" "ｼｬ" Nothing)
               ,("sye", KanaEntry "しぇ" "シェ" "ｼｪ" Nothing)
               ,("syi", KanaEntry "しぃ" "シィ" "ｼｨ" Nothing)
               ,("syo", KanaEntry "しょ" "ショ" "ｼｮ" Nothing)
               ,("syu", KanaEntry "しゅ" "シュ" "ｼｭ" Nothing)
               ,("ta", KanaEntry "た" "タ" "ﾀ" Nothing)
               ,("te", KanaEntry "て" "テ" "ﾃ" Nothing)
               ,("tha", KanaEntry "てぁ" "テァ" "ﾃｧ" Nothing)
               ,("the", KanaEntry "てぇ" "テェ" "ﾃｪ" Nothing)
               ,("thi", KanaEntry "てぃ" "ティ" "ﾃｨ" Nothing)
               ,("tho", KanaEntry "てょ" "テョ" "ﾃｮ" Nothing)
               ,("thu", KanaEntry "てゅ" "テュ" "ﾃｭ" Nothing)
               ,("ti", KanaEntry "ち" "チ" "ﾁ" Nothing)
               ,("to", KanaEntry "と" "ト" "ﾄ" Nothing)
               ,("tsu", KanaEntry "つ" "ツ" "ﾂ" Nothing)
               ,("tt", KanaEntry "っ" "ッ" "ｯ" $ Just 't')
               ,("tu", KanaEntry "つ" "ツ" "ﾂ" Nothing)
               ,("tya", KanaEntry "ちゃ" "チャ" "ﾁｬ" Nothing)
               ,("tye", KanaEntry "ちぇ" "チェ" "ﾁｪ" Nothing)
               ,("tyi", KanaEntry "ちぃ" "チィ" "ﾁｨ" Nothing)
               ,("tyo", KanaEntry "ちょ" "チョ" "ﾁｮ" Nothing)
               ,("tyu", KanaEntry "ちゅ" "チュ" "ﾁｭ" Nothing)
               ,("u", KanaEntry "う" "ウ" "ｳ" Nothing)
               ,("va", KanaEntry "う゛ぁ" "ヴァ" "ｳﾞｧ" Nothing)
               ,("ve", KanaEntry "う゛ぇ" "ヴェ" "ｳﾞｪ" Nothing)
               ,("vi", KanaEntry "う゛ぃ" "ヴィ" "ｳﾞｨ" Nothing)
               ,("vo", KanaEntry "う゛ぉ" "ヴォ" "ｳﾞｫ" Nothing)
               ,("vu", KanaEntry "う゛" "ヴ" "ｳﾞ" Nothing)
               ,("vv", KanaEntry "っ" "ッ" "ｯ" $ Just 'v')
               ,("wa", KanaEntry "わ" "ワ" "ﾜ" Nothing)
               ,("we", KanaEntry "うぇ" "ウェ" "ｳｪ" Nothing)
               ,("wi", KanaEntry "うぃ" "ウィ" "ｳｨ" Nothing)
               ,("wo", KanaEntry "を" "ヲ" "ｦ" Nothing)
               ,("wu", KanaEntry "う" "ウ" "ｳ" Nothing)
               ,("ww", KanaEntry "っ" "ッ" "ｯ" $ Just 'w')
               ,("xa", KanaEntry "ぁ" "ァ" "ｧ" Nothing)
               ,("xe", KanaEntry "ぇ" "ェ" "ｪ" Nothing)
               ,("xi", KanaEntry "ぃ" "ィ" "ｨ" Nothing)
               ,("xka", KanaEntry "ヵ" "ヵ" "ｶ" Nothing)
               ,("xke", KanaEntry "ヶ" "ヶ" "ｹ" Nothing)
               ,("xo", KanaEntry "ぉ" "ォ" "ｫ" Nothing)
               ,("xtsu", KanaEntry "っ" "ッ" "ｯ" Nothing)
               ,("xtu", KanaEntry "っ" "ッ" "ｯ" Nothing)
               ,("xu", KanaEntry "ぅ" "ゥ" "ｩ" Nothing)
               ,("xwa", KanaEntry "ゎ" "ヮ" "ﾜ" Nothing)
               ,("xwe", KanaEntry "ゑ" "ヱ" "ｴ" Nothing)
               ,("xwi", KanaEntry "ゐ" "ヰ" "ｲ" Nothing)
               ,("xx", KanaEntry "っ" "ッ" "ｯ" $ Just 'x')
               ,("xya", KanaEntry "ゃ" "ャ" "ｬ" Nothing)
               ,("xyo", KanaEntry "ょ" "ョ" "ｮ" Nothing)
               ,("xyu", KanaEntry "ゅ" "ュ" "ｭ" Nothing)
               ,("ya", KanaEntry "や" "ヤ" "ﾔ" Nothing)
               ,("ye", KanaEntry "いぇ" "イェ" "ｲｪ" Nothing)
               ,("yi", KanaEntry "い" "イ" "ｲ" Nothing)
               ,("yo", KanaEntry "よ" "ヨ" "ﾖ" Nothing)
               ,("yu", KanaEntry "ゆ" "ユ" "ﾕ" Nothing)
               ,("yy", KanaEntry "っ" "ッ" "ｯ" $ Just 'y')
               ,("z ", KanaEntry "　" "　" "　" Nothing)
               ,("z,", KanaEntry "‥" "‥" "‥" Nothing)
               ,("z-", KanaEntry "〜" "〜" "〜" Nothing)
               ,("z.", KanaEntry "…" "…" "…" Nothing)
               ,("z/", KanaEntry "・" "・" "･" Nothing)
               ,("z:", KanaEntry "：" "：" ":" Nothing)
               ,("z[", KanaEntry "『" "『" "『" Nothing)
               ,("z]", KanaEntry "』" "』" "』" Nothing)
               ,("za", KanaEntry "ざ" "ザ" "ｻﾞ" Nothing)
               ,("ze", KanaEntry "ぜ" "ゼ" "ｾﾞ" Nothing)
               ,("zh", KanaEntry "←" "←" "←" Nothing)
               ,("zi", KanaEntry "じ" "ジ" "ｼﾞ" Nothing)
               ,("zj", KanaEntry "↓" "↓" "↓" Nothing)
               ,("zk", KanaEntry "↑" "↑" "↑" Nothing)
               ,("zl", KanaEntry "→" "→" "→" Nothing)
               ,("zo", KanaEntry "ぞ" "ゾ" "ｿﾞ" Nothing)
               ,("zu", KanaEntry "ず" "ズ" "ｽﾞ" Nothing)
               ,("zw", KanaEntry "ｗ" "ｗ" "ｗ" Nothing)
               ,("zya", KanaEntry "じゃ" "ジャ" "ｼﾞｬ" Nothing)
               ,("zye", KanaEntry "じぇ" "ジェ" "ｼﾞｪ" Nothing)
               ,("zyi", KanaEntry "じぃ" "ジィ" "ｼﾞｨ" Nothing)
               ,("zyo", KanaEntry "じょ" "ジョ" "ｼﾞｮ" Nothing)
               ,("zyu", KanaEntry "じゅ" "ジュ" "ｼﾞｭ" Nothing)
               ,("zz", KanaEntry "っ" "ッ" "ｯ" $ Just 'z')
               ]

parseKanaTable :: T.Text -> KanaTable
parseKanaTable =
  KanaTable . fromList. map (build . T.splitOn ",") . filter (not . T.null) . T.lines
  where
    build [a, b, c, d] = (T.encodeUtf8 $ T.replace "&comma;" "," a, KanaEntry b c d Nothing)
    build [a, b, c, d, e] = (T.encodeUtf8 $ T.replace "&comma;" "," a, KanaEntry b c d $ Just $ T.head e)

prettyKanaTable :: KanaTable -> T.Text
prettyKanaTable =
  T.unlines . (++ ["]"]) . (_tail.each %~ (T.cons ',')) .
  (_head  %~ (T.cons '[')) . map prettyEntry . toList . view kanaDic

show' :: Show a => a -> T.Text
show' = T.pack . show

prettyEntry :: Show a => (a, KanaEntry) -> T.Text
prettyEntry (mid, KanaEntry a b c d) =
  T.concat ["(", show' mid, ", KanaEntry ", T.unwords $ map prettyText [a, b, c], " $ ", show' d, ")"]

prettyText :: T.Text -> T.Text
prettyText str = "\"" <> str <> "\""

prettyState :: ConvState -> String
prettyState (Converted a) = T.unpack $ "Converted " <> prettyText a
prettyState w             = show w
