module Utils (splitWhen) where

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f xs = (takeWhile f xs, dropWhile f xs)
