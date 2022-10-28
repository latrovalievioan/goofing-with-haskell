fibToN n xs = if length xs == n
  then reverse xs
  else fibToN n ([xs!!0 + xs!!1] ++ xs)

