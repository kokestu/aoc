import Text.Parsec
import Data.Map.Strict (Map, empty, alter, (!?))
import Data.Set (Set, singleton, insert, member)
import Data.List.Split (splitOn)
import Data.List (sortBy)

main = do
  content <- readFile "inputs/day5.txt"
  case parseIt content of
    Left b -> do print content; print b
    Right (back, rem) -> do
      let lists = map read <$> splitOn "," <$> lines rem
      putStrLn "Part one:"
      print $ sum $ map listval $ [x | x <- lists, verify back x]
      putStrLn "Part two:"
      let fixed = [sortBy (order back) x | x <- lists, not $ verify back x]
      print $ sum $ map listval fixed
  where parseIt = runParser
          (parseLookups *> ((,) <$> getState <*> getInput))
          empty ""

type Lookup = Map Int (Set Int)
type Parser = Parsec String Lookup

-- Build a list of rules looking backwards, since the only way
-- to fail is to occur after when you should be before. We could
-- use forward-looking rules instead, but then we would want to
-- iterate through the list backwards (inefficient in Haskell).
parseLookups :: Parser Char
parseLookups = try (newline) <|> do
  a <- parseInt
  char '|'
  b <- parseInt
  newline
  updateState (\back -> alter (addOrInsert a) b back)
  parseLookups
    where
      parseInt = read <$> many1 digit
      addOrInsert x Nothing = Just $ singleton x
      addOrInsert x (Just s) = Just $ insert x s

-- For every element of the list, check that it shouldn't come
-- before any of the later ones.
verify :: Lookup -> [Int] -> Bool
verify _ [] = True
verify back (x:xs) =
  case (back !? x) of
    Nothing -> verify back xs
    Just s -> all (\y -> not (y `member` s)) xs && verify back xs

listval :: [Int] -> Int
listval l = l !! (length l `div` 2)

order :: Lookup -> Int -> Int -> Ordering
order back x y =
  case back !? x of
    Nothing -> EQ
    Just sx -> if not (y `member` sx) then LT else GT
