import qualified Data.Map.Strict as M

main = do
  ls <- lines <$> readFile "inputs/day1.txt"
  -- Part one.
  let (as, bs) = getOrdLists ([], []) ls
  putStrLn "Part one: "
  print $ sum $ zipWith (\a b -> abs $ a-b) as bs
  -- Part two.
  let hm = countEntries bs
  putStrLn "Part two: "
  print $ fst $ foldl f (0,hm) as
  where
    f (tot, hm) a = (tot + (maybe 0 (\x -> a * x) (hm M.!? a)), hm)

-- Get two ordered lists, one for the first column and one for the second.
getOrdLists :: ([Int], [Int]) -> [String] -> ([Int], [Int])
getOrdLists res [] = res
getOrdLists (as, bs) (l:ls) = getOrdLists (ordIns a as, ordIns b bs) ls
  where [a, b] = map read . words $ l

-- Ordered insert.
ordIns :: Ord a => a -> [a] -> [a]
ordIns a [] = [a]
ordIns a bs'@(b:bs)
  | a <= b    = a:bs'
  | otherwise = b : ordIns a bs

-- Given a list, produce a count of how many times each number appears.
countEntries :: Ord a => [a] -> M.Map a Int
countEntries = foldl (\hm x -> M.insertWith (+) x 1 hm) M.empty
