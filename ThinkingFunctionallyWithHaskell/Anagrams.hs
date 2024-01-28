import Data.List (sort, words)
import System.IO
import Prelude hiding (Word)
import System.Environment (getArgs)

type Label = [Char]
type Word = [Char]

-- arg1: 文字列の文字数
-- arg2: 文字列のリスト
-- 第一引数にマッチする文字数の文字列のみを返却
getWords :: Int -> [Word] -> [Word]
getWords n = filter ((==n) . length)

-- arg1: 文字列
-- 引数の文字列と、その単語の文字をアルファベット順にソートしたラベルをタプルで返却
addLabel :: Word -> (Label, Word)
addLabel w = (sort w, w)

-- sortLabels :: [(Label, Word)] -> [(Label, Word)]
-- sortLabels [] = []
-- sortLabels x = sort . map x

groupByLabel :: [(Label, Word)] -> [(Label, [Word])]
groupByLabel [] = []
groupByLabel (lbl:kws) = insert lbl (groupByLabel kws)
  where
    insert (l,w) [] = [(l, [w])]
    insert (l,w) ((l', ws):kws) = 
      if l == l' then (l, w:ws):kws
                 else (l, [w]):(l', ws):kws

-- arg1: ラベル、文字列のリスト
-- 文字列のリストを「,」で連結。ラベルを「: 」で連結。文字列の最後に「\n」付与。
showEntry :: [(Label, [Word])] -> String
showEntry = concat . map showLine
  where showLine (k,ws) = k ++ ": " ++ addcommas ws ++ "\n"

addcommas :: [Word] -> String
addcommas [] = []
addcommas [w] = w
addcommas (w:ws) = w ++ "," ++ addcommas ws

main :: IO ()
main = do {
  [n] <- getArgs;
  text <- getContents;
  putStr (anagrams (read n) (words text))
}
anagrams n = showEntry . groupByLabel . map addLabel . getWords n