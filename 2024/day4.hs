import qualified Data.Matrix as M
import Text.Parsec

main = do
  content <- readFile "inputs/day4.txt"
  -- Find all the Xs and count XMASs.
  putStrLn "Part one: "
  doTask content 'X' countXmass
  putStrLn "Part two: "
  doTask content 'A' countCrossMAS

doTask content letter f = 
  case runParser (parseLetters letter) ([],(1,1)) "" content of
    Left _ -> return ()
    Right (xlocs, _) -> print $ f xlocs m
    where m = M.fromLists $ lines content
  
-- Find locations of Xs to check.
type Location = (Int, Int)
type ParseState = ([Location], Location)
type Parser = Parsec String ParseState

-- Find the locations of a specific letter.
parseLetters :: Char -> Parser ParseState
parseLetters letter =
  -- Note the location of the specific letter.
  try (char letter *> logLocation *> cont) <|>
  -- Keep track of location, and terminate at end of file.
  try (char '\n' *> bumpRow *> cont) <|>
  try (eof *> getState) <|>
  (anyChar *> bumpCol *> cont)
  where
    logLocation = do
      modifyState (\(locs, loc) -> (loc:locs, loc))
      bumpCol
    bumpRow = modifyState (\(_x, (r,_)) -> (_x, (r+1,1)))
    bumpCol = modifyState (\(_x, (r,c)) -> (_x, (r,c+1)))
    cont = parseLetters letter

data Direction = L | R | U | UL | UR | D | DL | DR
dir L (x,y) = (x-1, y)
dir R (x,y) = (x+1, y)
dir U (x,y) = (x, y-1)
dir UL (x,y) = (x-1, y-1)
dir UR (x,y) = (x+1, y-1)
dir D (x,y) = (x, y+1)
dir DL (x,y) = (x-1, y+1)
dir DR (x,y) = (x+1, y+1)

opp UL = DR
opp UR = DL
opp _ = undefined

countXmass :: [Location] -> M.Matrix Char -> Int
countXmass [] _ = 0
countXmass (loc:locs) m =
  checkXmas L m loc + checkXmas R m loc +
  checkXmas U m loc + checkXmas UL m loc + checkXmas UR m loc +
  checkXmas D m loc + checkXmas DL m loc + checkXmas DR m loc +
  countXmass locs m

checkXmas :: Direction -> M.Matrix Char -> Location -> Int
checkXmas d m loc = checkXmas' "MAS" m (f loc)
  where
    f = dir d
    checkXmas' [] _ _ = 1
    checkXmas' (x:xs) m loc =
      case m !? loc of
        Nothing -> 0
        Just y -> if y == x then checkXmas' xs m (f loc) else 0

countCrossMAS :: [Location] -> M.Matrix Char -> Int
countCrossMAS [] _ = 0
countCrossMAS (loc:locs) m =
  checked + countCrossMAS locs m
  where
    checked = if (checkCrossMAS UL m loc && checkCrossMAS UR m loc) then 1 else 0

checkCrossMAS d m loc =
  case (,) <$> m !? (dir d loc ) <*> m !? (dir (opp d) loc) of
    Just (a, b) ->
      (a == 'M' && b == 'S') || (b == 'M' && a == 'S')
    Nothing -> False

-- Safe indexing into matrices
(!?) = flip $ uncurry M.safeGet
