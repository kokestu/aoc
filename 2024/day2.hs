main = do
  ls <- map (getInts . words) <$> lines <$> readFile "inputs/day2.txt"
  -- Part one.
  putStrLn "Part one:"
  print $ foldl (\acc l -> acc + validate Any l) 0 ls
  -- Part two.
  putStrLn "Part two:"
  print $ foldl (\acc l -> acc + validateDampBF l) 0 ls
  putStrLn "Check non-BF:"
  print $ foldl (\acc l -> acc + validateDamp Any l) 0 ls
  putStrLn "Mistakes:"
  sequence_ [if validateDampBF l > validateDamp Any l then print l else return () | l <- ls]
  where
    getInts :: [String] -> [Int]
    getInts = map read

-- Track the direction.
data Direction = Any | Inc | Dec 
dir x y = if x < y then Inc else Dec

-- Helper to check the distance and direction.
check Any x y =
  (abs $ x - y) >= 1 && (abs $ x - y) <= 3
check Inc x y =
  (y - x) >= 1 && (y - x) <= 3
check Dec x y =
  (x - y) >= 1 && (x - y) <= 3

-- Validate without the dampener.
validate :: Direction -> [Int] -> Int
validate _ [_] = 1
validate Any (x:y:xs) =
  if check Any x y then validate (dir x y) (y:xs) else 0
validate d (x:y:xs) =
  if check d x y then validate d (y:xs) else 0

-- Brute force validation with the dampener: try removing the entries
-- one at a time until it works.
validateDampBF :: [Int] -> Int
validateDampBF xs = validateDampBF' xs [0..length xs]
  where
    validateDampBF' _ [] = 0
    validateDampBF' xs (i:is) =
      if validate Any (removeN xs i) == 1 then 1 else validateDampBF' xs is
    removeN :: [a] -> Int -> [a]
    removeN [] _ = []
    removeN (x:xs) i
      | i == 0    = x:xs
      | i == 1    = xs
      | otherwise = x : removeN xs (i-1)
    
-- Validate with the dampener. This was my non-brute force attempt,
-- but it needs backtracking. If we go into what seems like a valid
-- branch, (e.g. dropping the second element because it looks like the
-- first will work) but later find a mistake, we don't backtrack to try
-- dropping the first. This means we make mistakes in cases like:
--    [20,17,18,21,23,25]
-- It's not clear what the best method is to fix this, so I'm going to
-- leave it alone.
validateDamp :: Direction -> [Int] -> Int
validateDamp _ [x, y] = 1   -- if they don't work, use the dampener
validateDamp Any (x:y:z:xs)
  -- If the first two work, continue.
  | check Any x y = validateDamp (dir x y) (y:z:xs)
  -- Try dropping y (fallback to `validate` since we used the dampener)
  | check Any x z = validate (dir x z) (z:xs)
  -- Try dropping x -- this is okay at the start
  | check Any y z = validate (dir y z) (z:xs)
  -- Otherwise, fail.
  | otherwise     = 0
validateDamp d (x:y:z:xs)
  -- If the first two work, continue.
  | check d x y = validateDamp d (y:z:xs)
  -- Try dropping y (fallback to `validate` since we used the dampener)
  | check d x z = validate d (z:xs)
  -- Otherwise, fail.
  | otherwise   = 0
