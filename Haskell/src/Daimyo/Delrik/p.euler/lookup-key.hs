module
    ElkStore  (elkLookUp, t_elkLookUp)
where

newtype Key = MakeKey String
    deriving (Ord, Eq, Show)

toKey :: String -> Key
toKey = MakeKey

-- interesting, so this toKey function does the same thing my MakeKey type constructor above does.  Takes a string and wraps it as a Key

runMakeKey :: Key -> String
runMakeKey (MakeKey i) = i

elkLookUp :: Key -> [(Key,v)] -> Maybe v
elkLookUp (MakeKey k) env = foldr crunch Nothing env
    where
        crunch (MakeKey k',v') acc
            | k' == k = Just v'
            | otherwise = acc
t_elkLookUp = elkLookUp (toKey "Toby") [(toKey "Dante", 5), (toKey "Toby", 6)]
