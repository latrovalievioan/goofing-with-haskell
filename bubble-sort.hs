step xs i 
  | i + 2 > length xs = xs
  | xs !! i <= xs !! (i + 1) = step xs (i + 1)     
  | otherwise = step (take i xs ++ [xs !! (i + 1), xs !! i] ++ drop (i + 2) xs) (i + 1)

checkIsSorted xs i
  | i >= (length xs) - 1 = True
  | xs !! i <= xs !! (i + 1) = checkIsSorted xs (i + 1)
  | otherwise = False

bubbleSort xs 
  | checkIsSorted xs 0 == True = xs
  | otherwise = bubbleSort (step xs 0) 
