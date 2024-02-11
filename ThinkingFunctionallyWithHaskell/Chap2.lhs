Practice C
1. toUpper
2. 
 unwords:: [Word] -> String 
  -> 単語を空白を１つ挟んで連結する
 words:: [String] -> [Word]
  -> 空白くぎりでStringをリスト化する
3.
 head -> リストの先頭要素取得
 tail -> リストの先頭以外の要素取得

> import Data.List (words, unwords)
> import Data.Char (toUpper,toLower,isAlpha)
> type Word = [Char]
> type CIN = [Char]

> returnIdentical :: String -> String
> returnIdentical (x:xs) = [x] ++ xs

> modernise :: String -> String
> modernise = unwords . map capitalize . words

> capitalize :: String -> String
> capitalize xs = [toUpper (head xs)] ++ tail xs

> addSum :: CIN -> CIN
> addSum cin = cin ++ show (n `div` 10) ++ show (n `mod` 10) where n = sum (map fromDigit cin)

> fromDigit :: Char -> Int
> fromDigit c = read [c]

> palindrome :: IO()
> palindrome 
>  = do {putStrLn "Enter a string:";
>        xs <- getLine;
>        if isPalindrome xs then putStrLn "Yes!"
>        else putStrLn "No!"}

> isPalindrome :: String -> Bool
> isPalindrome xs = (ys == reverse ys)
>   where ys = map toLower (filter isAlpha xs)
