module
    ElkStore  (elkLookUp, t_elkLookUp)
where

import qualified Data.Map as M

newtype Key = MakeKey String
    deriving (Ord, Eq, Show)

newtype Env = Env (M.Map Key Integer)
    deriving (Show)

toKey :: String -> Key
toKey = MakeKey

-- interesting, so this toKey function does the same thing 
-- my MakeKey type constructor above does.  Takes a string and wraps it as a Key

runMakeKey :: Key -> String
runMakeKey (MakeKey i) = i

runEnv :: Env -> (M.Map Key Integer)
runEnv (Env m) = m

augEnv :: Key -> Integer -> Env -> Env
augEnv k v (Env m) = Env $ M.insert k v m

elkLookUp :: Key -> Env -> Maybe Integer
elkLookUp k env= M.lookup k $ runEnv env

t_elkLookUp = elkLookUp (toKey "Dante") t_data'
t_data = Env $ M.fromList [(toKey "Dante", 6), (toKey "Toby", 8)]
t_data'= augEnv (toKey "Alex") 10 t_data
