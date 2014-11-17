import Data.List
import System.Environment

echo = do
    argv <- getArgs
    let argv' = concat $ intersperse " " argv
    putStrLn argv'

main :: IO ()
main = do
    echo
