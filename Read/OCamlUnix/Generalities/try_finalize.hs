import Control.Exception
import System.Environment
import System.IO

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [] -> error "supply a filename"
        (h:_) -> do
            -- two examples

            -- ex1: try
            r <- try $ readFile h :: IO (Either SomeException String)
            case r of
                Left e -> putStrLn "exception!"
                Right _ -> putStrLn "success"

            -- ex2: bracket
            bracket
                (openFile h ReadMode)
                (\fh -> putStrLn "CLEANUP!" >> hClose fh)
                (\fh -> do
                    v <- hGetLine fh
                    putStrLn $ head $ lines v)
