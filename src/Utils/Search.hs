module Utils.Search (binaryMax) where

-- Assumes min <= max
-- Assumes there is an a for which p holds.
binaryMax :: (Integral a) => (a -> Bool) -> a -> a -> a
binaryMax = generalMax binaryPivot

binaryPivot :: (Integral a) => a -> a -> a
binaryPivot min' max'
  | min' == max' - 1 = max'
  | otherwise = (min' + max') `div` 2

-- Assumes min' <= max'
-- p returns true iff the interval should be shrunk from the top.
generalMax :: (Eq a, Enum a) => (a -> a -> a) -> (a -> Bool) -> a -> a -> a
generalMax pivotF p min' max'
  | min' == max' = min'
  | otherwise = let
    pivot = pivotF min' max'
    (min'', max'') = if p pivot
      then (pivot, max')
      else (min', pred pivot)
    in generalMax pivotF p min'' max''
