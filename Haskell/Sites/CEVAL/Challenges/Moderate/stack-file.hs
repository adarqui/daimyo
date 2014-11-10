import Data.List
import Control.Monad
import System.Environment

data Stack a = Empty | Node a (Stack a) deriving (Show)

push :: a -> Stack a -> Stack a
push a Empty = Node a Empty
push a st = Node a st

pop :: Stack a -> Stack a
pop Empty = Empty
pop (Node a r) = r

top :: Stack a -> a
top Empty = error "top of an empty stack"
top (Node a r) = a

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (path:[]) -> do
   v <- readFile path
   mapM_ (\line -> putStrLn $ ints2printableString $ cyclePop $ pushLine line) $ lines v
  otherwise -> error "usage: ./stack-file <path>"

pushLine :: String -> Stack Integer
pushLine l = foldl' (\acc e -> push (read e :: Integer) acc) Empty $ words l

cyclePop :: Stack a -> [a]
cyclePop = cyclePop' 0

cyclePop' :: Int -> Stack a -> [a]
cyclePop' _ Empty = []
cyclePop' oe st =
 case (even oe) of
  True -> (top st) : cyclePop' 1 (pop st)
  False -> cyclePop' 0 (pop st)

ints2printableString :: [Integer] -> String
ints2printableString l = concat $ intersperse " " $ map show l

t1 = foldl' (\acc e -> push e acc) Empty [1..10]
t2 = pushLine "1 2 3 -1 4 9 0 10 99 3 2000"
t3 = cyclePop t2
t4 = [cyclePop $ pushLine "1 2 3 4", cyclePop $ pushLine "10 -2 3 4"]
