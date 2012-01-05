import Control.Arrow

missing :: [Int] -> Int
missing = uncurry (-) . (((`div` 2) . uncurry (*) . (id &&& (+1)) . length) &&& sum)
