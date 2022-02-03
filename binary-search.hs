getMiddle xs =
  xs !! (div (length xs) 2)


binarySearch xs n
  | length xs < 1 = "Not Found"
  | getMiddle xs == n = "Found"
  | getMiddle xs < n = binarySearch (drop (getMiddle xs) xs) n  
  | otherwise = binarySearch (take (getMiddle xs) xs) n
