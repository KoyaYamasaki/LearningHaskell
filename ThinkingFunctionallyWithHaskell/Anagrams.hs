import Data.List (sort, words)
import System.IO
import Prelude hiding (Word)

type Label = [Char]
type Word = [Char]

-- arg1: 文字列の文字数
-- arg2: 文字列のリスト
-- 第一引数にマッチする文字数の文字列のみを返却
getWords :: Int -> [Word] -> [Word]
getWords n = filter ((==n) . length)

-- addLabel :: Word -> (Label, Word)

-- sortLabels :: [(Label, Word)] -> [(Label, Word)]

-- groupByLabel :: [(Label, Word)] -> [(Label, [Word])]

-- showEntry :: (Label, [Word]) -> String


-- anagrams n = concat . map showEntry . groupByLabel
--     . sortLabels . map addLabel . getWords n