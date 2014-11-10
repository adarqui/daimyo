import Data.Ord
import Data.Char
import Data.List
import Control.Monad
import System.Environment

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (path:[]) -> do
            v <- readFile path
            mapM_ (putStrLn . solve) $ lines v
        otherwise -> error "usage: ./string-permutations <file>"

solve s = concat $ intersperse "," $ map (map chr) $ sort $ map (map ord) $ permutations s
