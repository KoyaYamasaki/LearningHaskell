-------------------------------
Practice E

> isSqrt :: Float -> Integer
> isSqrt x = floor $ sqrt x

-------------------------------

Practice F
引数の平方根を返却する関数
・yが√xの近似であれば、x/yも√xも近似する。
  → y<=√x<=x/y or x/y<=√x<=y のどちらかとなる。
・| y^2 - x | < ε　もしくは、| y^2 - x | < ε*x　の条件で近似を判断する

> sqrt' :: Float -> Integer
> sqrt' x = floor $ sqrt x

> isApproximate :: Integer -> Integer -> Bool
> isApproximate x y = abs < 1
>  where abs = if (y^2 - x) < 0 then negate (y^2 - x) else y^2 - x

> findXApproximate :: Integer -> Integer
> findXApproximate x = until (\y -> isApproximate x y) (+1) 1

-------------------------------