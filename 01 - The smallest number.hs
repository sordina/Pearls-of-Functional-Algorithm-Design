import Control.Arrow
import Data.List (delete)
import Test.QuickCheck

{- This is my Naive attempt at solvng the problem,
 - However, this makes some incorrect assumptions.
 - Still, I'm keeping it as it is a fun solution.
 -}

missing :: [Int] -> Int
missing = uncurry (-) . (((`div` 2) . uncurry (*) . (id &&& (+1)) . length) &&& sum)

prop_missing (Positive n) = missing ( delete deleted [0..n] ) == deleted where deleted = n `div` 2
