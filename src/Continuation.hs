module Continuation(Continuation(..), ($$), continuize, eval) where

--Define Continuations and make Continuation b a monad

--The continuation data type is just a wrapper around functions of a certain type
--It allows us to define new instances for type classes
data Continuation b a = Continuation ((a -> b) -> b)

--Monads need to be functors
--We can define fmap with respect to the monad definitions (which we do)
--but belowm, commented out, is the explicit definition as well
instance Functor (Continuation b) where
        --fmap f (Continuation g) = Continuation $ \c -> g (c . f)
        fmap = (=<<) . (return .)

--Monads need to be applicatives
--Similarly, we can define the applicative functions with respect to the monad operations but
--below is an explicit definition (commented out
instance Applicative (Continuation b) where
    pure x  = Continuation $ \c -> c x
    --(Continuation f) <*> (Continuation v) = Continuation $ \c -> f $ \g -> v (c . g)
    cf <*> cv = (cf >>=) $ (cv >>=) . (return .)

--Monad definition
--return is what is referred to in the paper as eta, and >>= is *
instance Monad (Continuation b) where
        return = pure
        (Continuation m) >>= k = Continuation $ \c -> m $ \a -> let (Continuation f) = (k a) in f c

--mapply is the lifted version of function application defined in the paper as A_M(f)(x)
--intermediate lines of code give equivalent expressions that may be more readable
--notice that this is just defined as the applicative <*> operator, but we give it a new name
--for clarity purposes
($$) :: (Monad m) => (m (a ->b)) -> m a -> m b
($$) = (<*>)

--return can be used for any monad, it is useful to have an explict function to make continuations
continuize :: a -> Continuation b a
continuize x = return x

--evaluate continuation to get final result of computation
eval :: Continuation b b -> b
eval (Continuation  f) = f id
