module Daimyo.Cryptography.Frequency (
  FrequencyTable,
  newFrequencyTable,
  bigrams,
  unigrams
) where

import qualified Data.Map  as M

data FrequencyTable k = FrequencyTable {
    ftSize     :: Int,
    ftUnigrams :: M.Map k (Int, Double),
    ftBigrams  :: M.Map [k] (Int, Double)
} deriving (Show)

-- | newFrequencyTable
--
-- >>> newFrequencyTable ([1,2,3,4,5,6,7,1,1,1,1,1,1,1,1,1] :: [Integer])
-- FrequencyTable {ftSize = 16, ftUnigrams = fromList [(1,(10,0.625)),(2,(1,6.25e-2)),(3,(1,6.25e-2)),(4,(1,6.25e-2)),(5,(1,6.25e-2)),(6,(1,6.25e-2)),(7,(1,6.25e-2))], ftBigrams = fromList [([1,1],(8,0.5)),([1,2],(1,6.25e-2)),([2,3],(1,6.25e-2)),([3,4],(1,6.25e-2)),([4,5],(1,6.25e-2)),([5,6],(1,6.25e-2)),([6,7],(1,6.25e-2)),([7,1],(1,6.25e-2))]}
--
-- newFrequencyTable "LOJUMYLJMEPDYVJQXTDVSVJNLDMTJZWMJGGYSNDLUYLEOSKDVCGEPJSMDIPDNEJSKDNJTJLSKDLOSVDVDNGYNVSGLLOSCIOLGOYGESNEPCGYSNGUJMJDGYNKDPPYXPJDGGSVDNTWMSWSGYLYSNGSKJCEPYQGSGLDMLPYNIUSCPQOYGMJGCPLGDWWJDMLSLOJCNYNYLYDLJQLODLCNLYPLOJTPJDMNJQLOJWMSEJGGJGXTUOYEOOJODQDMMYBJQDLLOJVLOJTVYIOLUJPPESNGYQJMOYVDGDNJEMSVDNEJM"
--
newFrequencyTable :: (Ord a) => [a] -> FrequencyTable a
newFrequencyTable l =
  FrequencyTable {
    ftSize = length l,
    ftUnigrams =
      M.fromListWith (\_ (x,_) -> let r = (x+1) in (r,(fromIntegral r)/sz)) $ zipWith (\x y -> (x,(y,1.0/sz))) l (repeat 1),
    ftBigrams =
      M.fromListWith (\_ (x,_) -> let r = (x+1) in (r,(fromIntegral r)/sz)) $ zipWith (\x y -> (x,(y,1.0/sz))) (bigrams l) (repeat 1)
  }
  where
    sz = fromIntegral $ length l

-- | unigrams
--
-- >> unigrams "hi"
-- "hi"
--
unigrams :: t -> t
unigrams = id

-- | bigrams
--
-- >>> bigrams "hello world"
-- ["he","el","ll","lo","o "," w","wo","or","rl","ld"]
--
bigrams :: [t] -> [[t]]
bigrams []          = []
bigrams (_:[])      = []
bigrams bi@(_:_:[]) = [bi]
bigrams (x:y:xs)     = (x:y:[]) : bigrams (y : xs)
