> module Lib
>     (
>       isSubstringOf2
>     , isSubstringOf
>     ) where

> someFunc :: IO ()
> someFunc = putStrLn "someFunc"

> data KMP a = KMP 
>     {
>       done :: Bool
>     , next :: (a -> KMP a)
>     }

> isSubstringOf2 :: Eq a => [a] -> [a] -> Bool
> isSubstringOf2 as bs = match (makeTable as) bs
>    where  match table []     = done table
>           match table (b:bs) = done table || match (next table b) bs

> isSubstringOf as bs = any done $ scanl next (makeTable as) bs

> makeTable :: Eq a => [a] -> KMP a
> makeTable xs = table
>        where 
>           table = makeTable' xs (const table)

> makeTable' []     failure = KMP True failure
> makeTable' (x:xs) failure = KMP False test
>    where  test  c = if c == x then success else failure c
>           success = makeTable' xs (next (failure x))


table :: KMP Char
const table :: b -> KMP Char

This is from stack overflow: http://stackoverflow.com/questions/16694306/knuth-morris-pratt-algorithm-in-haskell

makeTable "shoeshop" = table0

table0 = makeTable' "shoeshop" (const table0)
       = KMP False test0

test0 c = if c == 's' then success1 else const table0 c
        = if c == 's' then success1 else table0

success1 = makeTable' "hoeshop" (next (const table0 's'))
         = makeTable' "hoeshop" (next table0)
         = makeTable' "hoeshop" test0
         = KMP False test1

test1 c = if c == 'h' then success2 else test0 c

success2 = makeTable' "oeshop" (next (test0 'h'))
         = makeTable' "oeshop" (next table0)
         = makeTable' "oeshop" test0
         = makeTable' "oeshop" test0
         = KMP False test2

test2 c = if c == 'o' then success3 else test0 c

success3 = makeTable' "eshop" (next (test0 'o'))
         = makeTable' "eshop" (next table0)
         = makeTable' "eshop" test0
         = KMP False test3

test3 c = if c == 'e' then success4 else test0 c

success4 = makeTable' "shop" (next (test0 'e'))
         = makeTable' "shop" (next table0)
         = makeTable' "shop" test0
         = KMP False test4

test4 c = if c == 's' then success5 else test0 c

success5 = makeTable' "hop" (next (test0 's'))
         = makeTable' "hop" (next success1)
         = makeTable' "hop" test1
         = KMP False test5

test5 c = if c == 'h' then success6 else test1 c

success6 = makeTable' "op" (next (test1 'h'))
         = makeTable' "op" (next success2)
         = makeTable' "op" test2
         = KMP False test6

test6 c = if c == 'o' then success7 else test2 c

success7 = makeTable' "p" (next (test2 'o'))
         = makeTable' "p" (next success3)
         = makeTable' "p" test3
         = KMP False test7

test7 c = if c == 'p' then success8 else test3 c

success8 = makeTable' "" (next (test3 'p'))
         = makeTable' "" (next (test0 'p'))
         = makeTable' "" (next table0)
         = makeTable' "" test0
         = KMP True test0
