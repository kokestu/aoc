import Text.Parsec 

main = do
  contents <- readFile "inputs/day3.txt"
  -- Part one.
  putStrLn "Part one: "
  print $ runParser (parseMuls *> getState) (True, 0) "day3" contents
  -- Part two.
  putStrLn "Part two: "
  print $ runParser (parseMulAndDos *> getState) (True, 0) "day3" contents

type Parser = Parsec String (Bool, Int)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseMuls :: Parser ()
parseMuls =
  try (parseMul *> parseMuls) <|>
  try eof <|>
  (anyChar *> parseMuls)

parseMulAndDos :: Parser ()
parseMulAndDos =
  try (parseMul *> parseMulAndDos) <|>
  try (parseDo *> parseMulAndDos) <|>
  try (parseDont *> parseMulAndDos) <|>
  try eof <|>
  (anyChar *> parseMulAndDos)

parseMul :: Parser ()
parseMul = do
  string "mul("
  a <- parseInt
  char ','
  b <- parseInt
  char ')'
  should_do <- fst <$> getState
  if should_do
  then modifyState (\(x,y) -> (x, y+(a*b)))
  else return ()

parseDo :: Parser ()
parseDo = string "do()" *> modifyState (\(_,y) -> (True, y))

parseDont :: Parser ()
parseDont = string "don't()" *> modifyState (\(_,y) -> (False, y))
