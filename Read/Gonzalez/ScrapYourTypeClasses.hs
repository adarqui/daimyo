{- source: http://www.haskellforall.com/2012/05/scrap-your-type-classes.html -}

{-# LANGUAGE RankNTypes #-}

import Prelude hiding (fmap, (>>=), return, sum, product)

data CategoryI cat = CategoryI {
    _compose :: forall a b c . cat b c -> cat a b -> cat a c,
    _id      :: forall a . cat a a
}

data Isomorphism a b = Isomorphism {
    fw :: a -> b,
    bw :: b -> a
}

data FunctorI f = FunctorI {
    _fmap :: forall a b . (a -> b) -> f a -> f b
}

data MonadI m = MonadI {
    _return :: forall a . a -> m a,
    _bind   :: forall a b . m a -> (a -> m b) -> m b
}

data PointedI m = PointedI { _pure :: forall a . a -> m a }

data MonoidI m = MonoidI {
    _mempty  :: m,
    _mappend :: m -> m -> m }

monoidSum :: MonoidI Int
monoidSum = MonoidI {
    _mempty  = 0,
    _mappend = (+) }

monoidProduct :: MonoidI Int
monoidProduct = MonoidI {
    _mempty  = 1,
    _mappend = (*) }

monoidProduct' :: (Num a) => MonoidI a
monoidProduct' = MonoidI {
    _mempty  = 1,
    _mappend = (*) }

mconcat :: MonoidI a -> [a] -> a
mconcat i = foldr (_mappend i) (_mempty i)

sum     = mconcat monoidSum
product = mconcat monoidProduct


{-
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
-}

{-
instance Monad Maybe where
    return = Just
    m >>= f = case m of
        Nothing -> Nothing
        Just x  -> f x
-}

monad'Maybe :: MonadI Maybe
monad'Maybe = MonadI {
    _return = Just,
    _bind = \m f ->
        case m of
            Nothing -> Nothing
            Just x -> f x
    }

{-
sequence :: (Monad m) => [m a] -> m [a]
-}

sequence' :: MonadI m -> [m a] -> m [a]
sequence' mi l =
    case l of
        [] -> _return mi []
        (m:ms) ->
            _bind mi m (\x ->
                _bind mi (sequence' mi ms) (\xs ->
                    _return mi (x:xs)))
            

combine :: Isomorphism b c -> Isomorphism a b -> Isomorphism a c
combine (Isomorphism fw1 bw1) (Isomorphism fw2 bw2) = Isomorphism (fw1 . fw2) (bw2 . bw1)

{- Categories -}

category'Function :: CategoryI (->)
category'Function = CategoryI {
    _compose = \f g x -> f (g x),
    _id = \x -> x
}

category'Isomorphism :: CategoryI Isomorphism
category'Isomorphism = CategoryI {
    _compose =
        let (.) = _compose category'Function
                                                                       -- FIXME
        in \(Isomorphism fw1 bw1) (Isomorphism fw2 bw2) -> Isomorphism { }, --Isomorphism (fw1 . fw2) (bw1 . bw2),
    _id = let id = _id category'Function in Isomorphism id id
}


{-
iso1 :: Isomorphism ((a, b), c) (a, (b, c))
iso1 = Isomorphism {
    _fw = \((a, b), c) = (a, (b, c)), -- = is typo
    _bw = \(a, (b, c)) = ((a, b), c) -- = is typo
    _fw = \((a, b), c) -> (a, (b, c)),
    _bw = \(a, (b, c)) -> ((a, b), c)
}

FIXME
iso2 :: Isomorphism ((), a) a
    _fw = \((), a) -> a,
    _bw = \a -> ((), a)
}

(.) = _compose category'Function
iso1 . iso2 :: Isomorphism (((), b), c) (b, c)
-}


{-
category'Isomorphism :: CategoryI Isomorphism
category'Isomorphism = CategoryI {
    _compose = let (.) = _compose category'Function
                in \(Isomorphism fw1 bw1)
                    (Isomorphism fw2 bw2) ->
                       Isomorphism (fw1 . fw2) (bw1 . bw2),
    _id      = let id = _id category'Function
                in Isomorphism id id }
-}

-- Pointed is a super-class of Monad
pointed'Super'Monad :: MonadI m -> PointedI m
-- FIXME
--pointed'Super'Monad i = PointedI (_return i} <-- } = typo
pointed'Super'Monad i = PointedI (_return i)

{- FIXME
monad'Pointed'Bind :: PointedI m -> (m a -> (a -> m b) -> m b) -> MonadI m
monad'Pointed'Bind i b = MonadI (_pure i) b
-}


{- FIX
functor'Monad :: MonadI m -> FunctorI m
functor'Monad mi = FunctorI { _fmap = \f x -> x >>= return . f }
  where
    (.)    = _compose category'Function
    (>>=)  = _bind   mi
    return = _return mi
-}

-- Examples

t_sequence'1 = sequence' monad'Maybe [Just 1, Just 2, Just 3, Nothing, Just 4]
t_sequence'2 = sequence' monad'Maybe [Just 1, Just 2, Just 3, Just 4]
t_combine = (fw comb 0, bw comb 0)
    where
        iso1 = Isomorphism {
                fw = \a -> a+1,
                bw = \b -> b-1
            }
        iso2 = Isomorphism {
                fw = \a -> a+10,
                bw = \b -> b-10
            }
        comb = combine iso2 iso1

t_sum = sum [1..5]
t_product = product [1..5]



--

newtype StateT  s m a = StateT   { runStateT   :: s -> m (a, s) }
newtype ReaderT r m a = ReaderT  { runReaderT  :: r -> m a      }
newtype Identity    a = Identity { runIdentity :: a             }

{- class Monad m where
       return :: a -> m a
       (>>=) :: m a -> (a -> m b) -> m b -}
{-
data MonadI m = MonadI {
    _return :: forall a . a -> m a,
    _bind   :: forall a b . m a -> (a -> m b) -> m b }
-}

{- class MonadTrans t where
       lift :: Monad m => m a -> t m a -}
data MonadTransI t = MonadTransI {
    _lift :: forall a m . MonadI m -> m a -> t m a }

{- class Monad m => MonadState s m | m -> s where
       get :: m s
       put :: s -> m ()
       state :: (s -> (a, s)) -> m a -}
data MonadStateI s m = MonadStateI {
    -- This next line is the secret sauce
    _monad'Super'MonadState :: MonadI m,
    _put :: s -> m (),
    _get :: m s,
    _state :: forall a . (s -> (a, s)) -> m a }

{- class Monad m => Monadreader r m | m -> r where
       ask    :: m r
       local  :: (r -> r) -> m a -> m a
       reader :: (r -> a) -> m a -}
data MonadReaderI r m = MonadReaderI {
    _monad'Super'MonadReader :: MonadI m,
    _ask    :: m r,
    _local  :: forall a . (r -> r) -> m a -> m a,
    _reader :: forall a . (r -> a) -> m a }

{- get :: (Monad m) => StateT s m s
   get = StateT $ \s -> return (s, s) -}
get :: MonadI m -> StateT s m s
get i = StateT $ \s -> (_return i) (s, s)

{- put :: (Monad m) => s -> StateT s m ()
   put s = StateT $ \_ -> return ((), s) -}
put :: MonadI m -> s -> StateT s m ()
put i s = StateT $ \_ -> (_return i) ((), s)

{- state :: (Monad m) => (s -> (a, s)) -> StateT s m a
   state f = StateT (return . f) -}
state :: MonadI m -> (s -> (a, s)) -> StateT s m a
state i f = StateT ((_return i) . f)

{- ask :: (Monad m) => ReaderT r m r
   ask = ReaderT return -}
ask :: MonadI m -> ReaderT r m r
ask i = ReaderT (_return i)

{- local :: (Monad m) =>
       (r -> r) -> ReaderT r m a -> ReaderT r m a
   local f m = ReaderT $ runReaderT m . f -}
local :: MonadI m -> (r -> r) -> ReaderT r m a -> ReaderT r m a
local _ f m = ReaderT $ runReaderT m . f

{- reader :: (Monad m) => (r -> a) -> ReaderT r m a
   reader f = ReaderT (return . f) -}
reader :: MonadI m -> (r -> a) -> ReaderT r m a
reader i f = ReaderT ((_return i) . f)

{- instance Monad (Identity) where
       return = Identity
       m >>= k = k $ runIdentity m -}
monad'Identity :: MonadI Identity
monad'Identity = MonadI {
    _return = Identity,
    _bind = \m k -> k $ runIdentity m }

{- instance (Monad m) => Monad (StateT s m) where
       return a = state $ \s -> (a, s)
       m >>= k  = StateT $ \s -> do
           (a, s') <- runStateT m s
           runStateT (k a) s' -}
monad'StateT :: MonadI m -> MonadI (StateT s m)
monad'StateT i =
    let (>>=) = _bind i
     in MonadI {
            _return = \a -> state i $ \s -> (a, s),
            _bind   = \m k -> StateT $ \s ->
                          runStateT m s >>= \(a, s') ->
                          runStateT (k a) s' }

{- instance (Monad m) => Monad (ReaderT s m) where
       return = lift . return
       m >>= k = ReaderT $ \r -> do
           a <- runReaderT m r
           runReaderT (k a) r -}
monad'ReaderT :: MonadI m -> MonadI (ReaderT s m )
monad'ReaderT i =
    let return = _return i
        (>>=) = _bind i
        lift = _lift monadTrans'ReaderT i
     in MonadI {
            _return = lift . (_return i),
            _bind = \m k -> ReaderT $ \r ->
                runReaderT m r >>= \a ->
                runReaderT (k a) r }

{- instance MonadTrans StateT where
       lift m = StateT $ \s -> do
           a <- m
           return (a, s) -}
monadTrans'StateT :: MonadTransI (StateT s)
monadTrans'StateT = MonadTransI {
    _lift = \i m ->
        let return = _return i
            (>>=)  = _bind   i
         in StateT $ \s ->
                m >>= \a ->
                return (a, s) }

{- instance MonadTrans ReaderT where
       lift m = ReaderT (const m) -}
monadTrans'ReaderT :: MonadTransI (ReaderT r)
monadTrans'ReaderT = MonadTransI {
    _lift = \_ m -> ReaderT (const m) }

{- instance (Monad m) => MonadState s (StateT s m) where
       get = get -- from Control.Monad.Trans.State
       put = put
       state = state -}
monadState'StateT :: MonadI m -> MonadStateI s (StateT s m)
monadState'StateT i = MonadStateI {
    _monad'Super'MonadState = monad'StateT i,
    _get   = get   i,
    _put   = put   i,
    _state = state i }

{- instance (MonadState s m) => MonadState s (ReaderT r m) where
       get   = lift get
       put   = lift . put
       state = lift . state -}
monadState'ReaderT ::
    MonadStateI s m -> MonadStateI s (ReaderT r m)
monadState'ReaderT i =
    let monad'm = _monad'Super'MonadState i
        lift = _lift monadTrans'ReaderT monad'm
     in MonadStateI {
            _monad'Super'MonadState = monad'ReaderT monad'm,
            _get   = lift $ _get   i,
            _put   = lift . _put   i,
            _state = lift . _state i }

{- instance Monad m => MonadReader r (ReaderT r m) where
       ask = ask
       local = local
       reader = reader -}
monadReader'ReaderT :: MonadI m -> MonadReaderI r (ReaderT r m )
monadReader'ReaderT i = MonadReaderI {
    _monad'Super'MonadReader = monad'ReaderT i,
    _ask    = ask    i,
    _local  = local  i,
    _reader = reader i }

{- instance (MonadReader r m) => MonadReader r (StateT s m) where
       ask = lift ask
       local = \f m -> StateT $ local f . runStateT m
       reader = lift . reader -}
monadReader'StateT ::
    MonadReaderI r m -> MonadReaderI r (StateT s m)
monadReader'StateT i =
    let monad'm = _monad'Super'MonadReader i
        lift = _lift monadTrans'StateT monad'm
     in MonadReaderI {
            _monad'Super'MonadReader = monad'StateT monad'm,
            _ask = lift $ _ask i,
            _local = \f m -> StateT $ (_local i f) . runStateT m,
            _reader = lift . (_reader i) }

{- test :: (MonadState a m, MonadReader a m) => m ()
   test = ask >>= put -}
test :: MonadStateI a m -> MonadReaderI a m -> m ()
test = \is ir -> let (>>=)   = _bind (_monad'Super'MonadState is)
                  in (_ask ir) >>= (_put is)

example1 :: ReaderT a (StateT a Identity) ()
example1 = test
    (monadState'ReaderT $ monadState'StateT $ monad'Identity)
    (monadReader'ReaderT $ monad'StateT $ monad'Identity)

example2 :: StateT a (ReaderT a Identity) ()
example2 = test
    (monadState'StateT $ monad'ReaderT $ monad'Identity)
    (monadReader'StateT $ monadReader'ReaderT $ monad'Identity)

run1, run2 :: ((), Char)
run1 = runIdentity $ runStateT (runReaderT example1 'A') 'B'
run2 = runIdentity $ runReaderT (runStateT example2 'B') 'A'


