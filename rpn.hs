module RPNcalc (rpn) where
import qualified Text.ParserCombinators.Parsec as P
import Control.Monad.State

data Content a = Number a | Addition | Multiplication | Subtraction | Division
  deriving Show
type Stack a = [a]


-- Parser
run :: P.Parser a -> String -> a
run p input = case (P.parse p "" input) of
                Left err -> error $ show err
                Right x -> x
line :: P.Parser [String]
line = P.sepBy (P.many (P.noneOf " ")) (P.char ' ')

parse_input :: String -> [Content Int]
parse_input = (map interpret) . (run line) . cleanInput

interpret :: String -> Content Int
interpret x = case x of
                ('+' : _) -> Addition
                ('-' : _) -> Subtraction
                ('*' : _) -> Multiplication
                ('/' : _) -> Division
                x -> Number ( read x )

cleanInput :: String -> String
cleanInput = filter (`elem` "01234567890+*/- ")


-- Calculator
initialState :: Num a => Stack a
initialState = []

pop :: Num a => State (Stack a) a
pop = do
  stack <- get
  case stack of
    [] -> error $ "Empty Stack."
    (x: xs) -> do
      put xs
      return x

push :: Num a => a -> State (Stack a) ()
push x = do
  xs <- get
  put (x:xs)

evaluate :: [Content Int] -> State (Stack Int) Int
evaluate [] = do
  stack <- get
  return $ head stack
evaluate (x:xs) = do
  case x of
    Addition -> do
      a <- pop
      b <- pop
      push (a + b)
    Subtraction -> do
      a <- pop
      b <- pop
      push (b - a)
    Division -> do
      a <- pop
      b <- pop
      push (b `div` a)
    Multiplication -> do
      a <- pop
      b <- pop
      push (a * b)
    Number n -> push n
  evaluate xs

rpn :: String -> Int
rpn x = evalState (evaluate (parse_input x) ) initialState
