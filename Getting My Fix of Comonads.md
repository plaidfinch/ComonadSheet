% Getting My Fix of Comonads
% How to `extract` computation<br/>and not `duplicate` work
% Kenneth Foner — June 13, 2014

# (Co)monads: a brief refresher

```Haskell
class Functor m => Monad m where
   return :: a       -> m a
   join   :: m (m a) -> m a
```

(Yes, I know that `Functor` isn't actually a superclass of `Monad`... yet.)

```Haskell
class Functor w => Comonad w where
   extract   :: w a -> a       -- a.k.a. coreturn
   duplicate :: w a -> w (w a) -- a.k.a. cojoin
```

(from Edward Kmett's `Control.Comonad`)

### Monad laws:

```Haskell
join . return      == id
join . fmap return == id  
join . join        == join . fmap join 
```

### Comonad laws:

```Haskell
extract      . duplicate == id
fmap extract . duplicate == id
duplicate    . duplicate == fmap duplicate . duplicate
```

# A particular flavor of comonad

```Haskell
data Stream a = Cons a (Stream a) -- no nil!
```

(from Wouter Swierstra's `Data.Stream`)

```Haskell
head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))
```

(Notice that `head` and `tail` are total functions!)

```Haskell
instance Comonad Stream where
   extract   = head
   duplicate = iterate tail
```

Lots of other kinds of comonads, but the ones I'm talking about today are based on streams in various ways, so we'll focus on them.

# And forth from the streams...

```Haskell
import qualified Data.Stream as S
import           Data.Stream ( Stream(..) )

data Tape a = (Stream a) a (Stream a)

moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c         rs)  = Tape         ls  l (Cons c rs)
moveR (Tape         ls  c (Cons r rs)) = Tape (Cons c ls) r         rs

iterate :: (a -> a) -> (a -> a) -> a -> Tape a
iterate prev next = Tape <$> S.iterate prev <*> id <*> S.iterate next
```

It's infinite in both directions, like a Turing-machine `Tape`.

# Still a comonad?

Yep.

```Haskell
instance Comonad Tape where
   extract (Tape _ c _) = c
   duplicate = iterate moveL moveR
```

Duplicate ends up being a kind of "diagonalization." This means that movement and duplication commute:

```Haskell
moveL . duplicate == duplicate . moveL
moveR . duplicate == duplicate . moveR
```

I have a hand-wavy proof that this is the only definition of `duplicate` which satisfies the comonad laws for `Tape`.

(Proofs using more category theory and less gesticulation are welcome.)

# Fixed That For You

```Haskell
loeb :: (Functor f) => f (f a -> a) -> f a
loeb fs = fix $ \xs -> ($ xs) <$> fs
```

```Haskell
evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ \xs -> fs <@> duplicate xs
```
