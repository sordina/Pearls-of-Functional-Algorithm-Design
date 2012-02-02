
Countdown attempts to construct a number as near as possible to a target number
from a list of positive numbers using only additoin, subtraction,
multiplication, and division:

> countdown1 :: Int -> [Int] -> EV
> countdown1 n = nearest n . concatMap mkExprs . subseqs

> prop_acceptance = snd (countdown1 831 [1,3,7,10,25,50]) == 832


Subseqs returns a list of all non-empty subsequences of a non-empty list.

> subseqs [x]      = [[x]]
> subseqs (x : xs) = xss ++ [x] : map (x:) xss where xss = subseqs xs


The data-types are:

> data Expr  = Num Int | App Op Expr Expr
> data Op    = Add | Sub | Mul | Div

> type Value = Int
> type EV = (Expr, Value)

> value :: Expr -> Value
> value (Num x)      = x
> value (App op x y) = apply op (value x) (value y)

> apply :: Op -> Int -> Int -> Value
> apply Add = (+)
> apply Sub = (-)
> apply Mul = (*)
> apply Div = div

> legal :: Op -> Value -> Value -> Bool
> legal Add _ _ = True
> legal Mul _ _ = True
> legal Sub x y = x > y          -- No negative results
> legal Div x y = x `mod` y == 0 -- Only divide evenly


mkExprs creates a list of all legal expressions that can be built using the given subsequence:

> mkExprs :: [Int] -> [EV]
> mkExprs [x] = [(Num x, x)]
> mkExprs xs  = [ ev | (ys, zs) <- unmerges xs
>                    , ev1      <- mkExprs  ys
>                    , ev2      <- mkExprs  zs
>                    , ev       <- combine ev1 ev2 ]


unmerges xs is a list of all pairs (ys, zs) of nonempy lists such that merge ys
zs = xs, where merge merges two ordered lists into one (it is exploited that
the inputs are ordered)

> unmerges :: [a] -> [([a],[a])]
> unmerges  [x,y]  = [([x],[y]),([y],[x])]
> unmerges (x:xs)  = [([x],xs), (xs,[x])]
>                 ++ concatMap (add x) (unmerges xs)
>                     where
>                       add x (ys, zs) = [(x: ys, zs), (ys, x:zs)]


> ops = [Add, Sub, Mul, Div]

> combine :: EV -> EV -> [EV]
> combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]


nearest x xs returns the closest EV to x in xs or the first EV that equals x:

> nearest n ((e, v) : evs) = if d == 0 then (e, v)
>                                      else search n d (e,v) evs
>                                      where d = abs (n - v)

> search n d ev [] = ev
> search n d ev ((e, v) : evs) | d' ==  0 = (e, v)
>                              | d' <   d = search n d' (e, v) evs
>                              | d' >=  d = search n d ev evs
>                              where
>                                d' = abs (n - v)
