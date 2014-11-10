import Data.Char
import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 strings <- replicateM _T getLine
 mapM_ (putStrLn . show . makePalindrome) strings

makePalindrome s = makePalindrome' s (reverse s)

makePalindrome' _ [] = 0
makePalindrome' s@(sx:ss) rs@(rsx:rss) = (diff sx rsx) + makePalindrome' ss rss

diff sc rc
 | sc >= rc = 0
 | sc < rc = (ord rc - ord sc)
