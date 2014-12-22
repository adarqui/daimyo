module Daimyo.Crypto.Frequency (
    FrequencyTable,
    new,
    bigrams,
    unigrams
) where

import Data.List
import qualified Data.Map as M

data FrequencyTable k = FrequencyTable {
    sz :: Int,
    uni :: M.Map k (Int, Double),
    bi :: M.Map [k] (Int, Double)
} deriving (Show)

new :: (Ord a) => [a] -> FrequencyTable a
new l =
    let
        sz' = fromIntegral (length l) :: Double
    in
        FrequencyTable {
            sz = length l,
            uni =
                M.fromListWith
                    (\k (v,v') -> let v'' = (v+1) in (v'',(fromIntegral v'' :: Double)/sz'))
                    $ zipWith (\x y -> (x,(y,1.0/sz'))) l (repeat 1),
            bi =
                M.fromListWith
                    (\k (v,v') -> let v'' = (v+1) in (v'',(fromIntegral v'' :: Double)/sz'))
                    $ zipWith (\x y -> (x,(y,1.0/sz'))) (bigrams l) (repeat 1)
        }

unigrams l = l

bigrams [] = []
bigrams (x:y:[]) = [(x:y:[])]
bigrams (x:y:xs) = (x:y:[]) : bigrams (y : xs)

t_new'1 = new [1,2,3,4]
t_new'2 = new [1,2,3,4,5]
t_new'3 = new [1,2,3,4,5,6,7,1,1,1,1,1,1,1,1,1]
t_new'4 = do
    new
--    putStrLn $ M.showTree $ table $ new
        $ filter (/= ' ')
            "LOJUMYLJMEPDYVJQXTDVSVJNLDMTJZWMJGGYSNDLUYLEOSKDVC"++
            "GEPJSMDIPDNEJSKDNJTJLSKDLOSVDVDNGYNVSGLLOSCIOLGOYG"++
            "ESNEPCGYSNGUJMJDGYNKDPPYXPJDGGSVDNTWMSWSGYLYSNGSKJ"++
            "CEPYQGSGLDMLPYNIUSCPQOYGMJGCPLGDWWJDMLSLOJCNYNYLYD"++
            "LJQLODLCNLYPLOJTPJDMNJQLOJWMSEJGGJGXTUOYEOOJODQDMM"++
            "YBJQDLLOJVLOJTVYIOLUJPPESNGYQJMOYVDGDNJEMSVDNEJM"
