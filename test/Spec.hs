import qualified Data.List as List
import Lib(isSubstringOf)
import Test.QuickCheck(quickCheck)

isSubStr as bs = any (as `List.isPrefixOf`) (List.tails bs)

prop_isSubstringOf :: [Bool] -> [Bool] -> Bool
prop_isSubstringOf as bs = (isSubStr as bs) == (isSubstringOf as bs)

main :: IO ()
main = do
        quickCheck prop_isSubstringOf
