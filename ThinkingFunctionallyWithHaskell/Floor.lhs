Floor値の2分探索

1. m <= x < n を満たすmとnを見つける
2. xを含んだまま[m,n)を単位区間(m+1=nになる区間)になるまで挟める
3. 区間の左側境界を結果として返す


> type Interval = (Integer, Integer)

> leq :: Integer -> Float -> Bool
> m `leq` x = fromInteger m <= x

> lt :: Float -> Integer -> Bool
> x `lt` n = x < fromInteger n

shrink x (bound x) の返却値(m,n)が、 (m + 1 == n)であれば、その時のmの値が、底値(floor)。そうでなければ、shrink x (m, n)で再度実行。

> floor :: Float -> Integer
> floor x = fst (until unit (shrink x) (bound x)) where unit (m,n) = m + 1 == n

> floor' :: Float -> (Integer, Integer)
> floor' x = until unit (shrink x) (bound x) where unit (m,n) = m + 1 == n

shrink x (m,n):
 mとnの平均値がx以下の場合 -> (mとnの平均値, n)
 mとnの平均値がxより大きい場合 -> (m, mとnの平均値)

> shrink :: Float -> Interval -> Interval
> shrink x (m,n) = if p `leq` x then (p, n) else (m, p) where p = choose (m, n)

choose (m,n): mとnの平均値

> choose :: Interval -> Integer
> choose (m, n) = (m + n) `div` 2

(m + 1) != n の場合、m < (m + n) div 2 < n であることを確認

bound x: m<= x < nを満たす対(m,n)を返却

> bound :: Float -> Interval
> bound x = (lower x, upper x)

lower xの値はx以下の整数のうちのどれか
-- -1, -2,-4,-8,-16,-32,-64,-128....

> lower :: Float -> Integer
> lower x = until (`leq` x) (*2) (-1)

upper xの値はxより大きい整数のどれか
-- 1,2,4,8,16,32,64,128....

> upper :: Float -> Integer
> upper x = until (x `lt`) (*2) 1