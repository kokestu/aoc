import Text.Parsec
import qualified Data.Set as S

main = do
  content <- readFile "inputs/day6-test.txt"
  case runParser (parseLocations (1,1)) state0 "" content of
    Left e -> print e
    Right ((w,h),(obs, guard)) -> do
      putStrLn "Part one: "
      case moveGuard (outside w h) obs guard of
        Nothing -> error "loop in part one"
        Just dlocs -> do
          print $ S.size $ S.map fst dlocs
          putStrLn "Part two: "
          let obs_locs = findPlacements (outside w h) obs (S.toList dlocs)
          print $ S.size $ S.delete (fst guard) $ S.fromList obs_locs
  where state0 = (S.empty, ((-1,-1), U))

-- Find locations of obstacles and the guard.
type Location = (Int, Int)
type DirLocation = ((Int, Int), Direction)
type Locations = S.Set Location
type ParseState = (Locations, DirLocation)
type Parser = Parsec String ParseState

parseLocations :: Location -> Parser (Location, ParseState)
parseLocations loc@(row,col) =
  -- Note the location of obstacles
  try (char '#' *> logObstacle *> parseLocations nextCol) <|>
  -- Note the location of the guard
  try (char 'v' *> logGuard D *> parseLocations nextCol) <|>
  try (char '>' *> logGuard R *> parseLocations nextCol) <|>
  try (char '<' *> logGuard L *> parseLocations nextCol) <|>
  try (char '^' *> logGuard U *> parseLocations nextCol) <|>
  -- Track the current location and stop at end of file.
  try (char '\n' *> parseLocations nextRow) <|>
  try (eof *> ((,) loc <$> getState)) <|>
  (anyChar *> parseLocations nextCol)
  where
    logObstacle = modifyState (\(locs, _x) -> (S.insert loc locs, _x))
    logGuard d = modifyState (\(_x, _) -> (_x, (loc, d)))
    nextRow = (row+1, 1)
    nextCol = (row, col+1)

data Direction = L | R | U | D deriving (Show, Eq, Ord)
dir U (x,y) = (x-1, y)
dir D (x,y) = (x+1, y)
dir L (x,y) = (x, y-1)
dir R (x,y) = (x, y+1)

rot L = U
rot U = R
rot R = D
rot D = L

moveGuard :: (Location -> Bool) -> Locations -> DirLocation -> Maybe (S.Set DirLocation)
moveGuard = moveGuard' S.empty
moveGuard' dlocs isOutside obs (loc,d)
  -- Join the path in the same position again -- loop!
  | (loc, d) `S.member` dlocs = Nothing
  -- Collide with an obstacle.
  | target `S.member` obs = moveGuard' dlocs isOutside obs (loc, rot d)
  -- Leave the map.
  | isOutside target      = Just modDlocs
  -- Continue.
  | otherwise             = moveGuard' modDlocs isOutside obs (target, d)
  where
    target = dir d loc
    modDlocs = S.insert (loc,d) dlocs

-- For each point on the path, try placing an obstacle and continuing. Track
-- where this creates loops.
findPlacements :: (Location -> Bool) -> Locations -> [DirLocation] -> [Location]
findPlacements _ _ [] = []
findPlacements f obs (dloc@(loc,d):dlocs) =
  case moveGuard f (S.insert placement obs) dloc of
    -- Found a loop!
    Nothing -> placement : findPlacements f obs dlocs
    -- No loop, continue.
    Just _ -> findPlacements f obs dlocs
  where placement = dir d loc

outside :: Int -> Int -> Location -> Bool
outside w h (x,y) = x < 1 || x > w || y < 1 || y > h
