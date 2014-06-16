\title{Getting My Fix of Comonads}
\subtitle{A quest to \texttt{extract} computation and not \texttt{duplicate} work}
\author{Kenneth Foner}
\institute{Brandeis University / Galois, Inc.}
\date{\today}

\titlepage

# A tale of two blog articles

Dan Piponi, 2006 (<http://blog.sigfpe.com>):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
löb :: Functor f => f (f a -> a) -> f a
löb x = xs where xs = fmap ($ xs) x
```

. . .

### "Evaluating Cellular Automata is Comonadic"

> I want to work on 'universes' that extend to infinity in both directions. And I want this universe to be constructed lazily on demand.

> We can think of a universe with the cursor pointing at a particular element as being an element with a neighbourhood on each side.

# An unexpected journey

```Haskell
löb :: Functor f => f (f a -> a) -> f a
löb x = xs where xs = fmap ($ xs) x
```

- `löb` is a fixed-point where each element of the structure refers to the whole structure in terms of *absolute position*

- Comonadic computations are often described as computations in the context of a *relative position* within a larger structure

. . .

These articles are talking about almost the same thing!

This talk is about my quest to find that missing *je ne sais quoi*, and where that journey took me.

# (Co)monads: a brief summary

### Monads:

Most Haskellers define monads via `return` and `>>=`{.haskell}. Today, we'll use `return` and `join`. Note: `x >>= f == join (fmap f x)`{.haskell}.

```Haskell
class Functor m => Monad m where
   return :: a       -> m a
   join   :: m (m a) -> m a
```

. . .

### Comonads:

```Haskell
class Functor w => Comonad w where
   extract   :: w a -> a       -- a.k.a. coreturn
   duplicate :: w a -> w (w a) -- a.k.a. cojoin
```

(from Edward Kmett's `Control.Comonad`)

<!-- # (Co)monads (co)ntinued: laws

### Monad laws:

```Haskell
join . return      == id
join . fmap return == id  
join . join        == join . fmap join 
```

. . .

### Comonad laws:

```Haskell
extract      . duplicate == id
fmap extract . duplicate == id
duplicate    . duplicate == fmap duplicate . duplicate
``` -->

# A particular flavor of comonad

```Haskell
data Stream a = Cons a (Stream a) -- no nil!
```

(from Wouter Swierstra's `Data.Stream`)

. . .

```Haskell
head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))
```

(Notice that `head` and `tail` are total functions!)

<!-- . . .

```Haskell
instance Comonad Stream where
   extract   = head
   duplicate = iterate tail
``` -->

# A particular flavor of comonad

```Haskell
data Tape a = (Stream a) a (Stream a)
```

It's infinite in both directions, like a Turing-machine's... `Tape`.

. . .

```Haskell
moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) =
       Tape         ls  l (Cons c rs)
moveR (Tape ls          c (Cons r rs)) = 
       Tape (Cons c ls) r         rs
```

. . .

```Haskell
iterate :: (a -> a) -> (a -> a) -> a -> Tape a
iterate prev next x =
   Tape (Stream.iterate prev x) x (Stream.iterate next x)
```

# A particular flavor of comonad

```Haskell
instance Comonad Tape where
   extract (Tape _ c _) = c
   duplicate = iterate moveL moveR
```

. . .

Duplicate ends up being a kind of "diagonalization." This means that movement and duplication commute:

```Haskell
moveL . duplicate == duplicate . moveL
moveR . duplicate == duplicate . moveR
```

. . .

I have a hand-wavy proof that this is the only definition of `duplicate` which satisfies the comonad laws for `Tape`.

(I welcome proofs using more category theory than gesticulation.)

# Back to Piponi's `löb`

Löb's theorem: $\Box(\Box P \to P) \to \Box P$

> I'm going to take that as my theorem from which I'll derive a type. But what should $\Box$ become in Haskell?

> We'll defer that decision until later and assume as little as possible. Let's represent $\Box$ by a type that is a Functor.

(Piponi, 2006)

```Haskell
löb :: Functor f => f (f a -> a) -> f a
löb fs = xs where xs = fmap ($ xs) fs
```

. . .

But $\Box$ could also have more structure...

# Fixed That For You

```Haskell
löb :: Functor f => f (f a -> a) -> f a
löb fs  = fix $ \xs -> fmap ($ xs) fs
```

We want to find:

`???? :: Comonad w => w (w a -> a) -> w a`{.haskell}

. . .

\vspace*{.45\baselineskip}

`cfix :: Comonad w =>   (w a -> a) -> w a`{.haskell}

\vspace*{1.43\baselineskip} 

. . .

`wfix :: Comonad w => w (w a -> a) ->   a`{.haskell}

\vspace*{1.51\baselineskip} 

# Fixed That For You

```Haskell
löb :: Functor f => f (f a -> a) -> f a
löb fs  = fix $ \xs -> fmap ($ xs) fs
```

We want to find:

`???? :: Comonad w => w (w a -> a) -> w a`{.haskell}

```Haskell
cfix :: Comonad w =>   (w a -> a) -> w a
cfix f = fix (fmap f . duplicate)
```

```Haskell
wfix :: Comonad w => w (w a -> a) ->   a
wfix w = extract w (fmap wfix (duplicate w))
```

# A possible candidate

```Haskell
possibility :: Comonad w => w (w a -> a) -> w a
possibility = fmap wfix . duplicate
```

. . .

It type-checks, so it has to be right! Right?

# Well, sort of\dots

. . .

Let's try to count to 10000!

```Haskell
main = print . S.take 10000 . viewR . possibility $
   Tape (S.repeat (const 0)) -- zero left of origin
        (const 0)            -- zero at origin
        (S.repeat                    -- right of origin:
           (succ . extract . moveL)) -- 1 + leftward value
```

(Don't worry, this syntax gets much more elegant later.)

# Well, sort of\dots

`$ time ./possibility`

. . .

```
[0,1,2,3,4 ... some time later ... 9998, 9999, 10000]
       39.49 real        38.87 user         0.38 sys
```

. . .

256 increment operations per second.

(And this gets worse—it's not linear\dots)

# Sharing is caring

```Haskell
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix (duplicate w))
```

```Haskell
possibility :: Comonad w => w (w a -> a) -> w a
possibility = fmap wfix . duplicate
```

. . .

- No sharing: computation is shaped like a tree, not a DAG

- Count all the way up from zero for each number, so $O(n^2)$

      + possibly worse than that, as I saw when experimenting with it—likely due to the garbage collector

. . .

That really `succ`s.

# Sharing is caring

```Haskell
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix (duplicate w))
```

The root of the problem: `wfix` can't be expressed in terms of `fix`.

. . .

For the wise-guys in the audience, the following is *not* what I mean by "in terms of `fix`":

```Haskell
notWhatI'mTalkingAbout :: Comonad w => w (w a -> a) -> a
notWhatI'mTalkingAbout =
   fix $ \wfix ->
      \w -> extract w (fmap wfix (duplicate w))
```

. . .

So more specifically: `wfix` can't be expressed in terms of `fix` on its *argument*—there's always that extra `extract w` on the outside which can't be folded into the recursion.

# Filling in the holes to fix our problem

**Epiphany**: If there exists an efficient (optimally-sharing) version of our "evaluation" function, it has to be expressible as:

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _
```

GHC 7.8 gave us typed holes—let's use them to fill in the blank!

# Filling in the holes to fix our problem

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _
```

. . .

```
Found hole ‘_’ with type: w a -> w a
    Relevant bindings include
      fs       :: w (w a -> a)
      evaluate :: w (w a -> a) -> w a
```

(Error messages have been cleaned for your viewing enjoyment.)

\vspace*{1.5\baselineskip}

# Filling in the holes to fix our problem

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _ . duplicate
```

. . .

```
Found hole ‘_’ with type: w (w a) -> w a
    Relevant bindings include
      fs       :: w (w a -> a)
      evaluate :: w (w a -> a) -> w a
```

\vspace*{3\baselineskip}

<!-- All of the previous comonadic fixed-points we were using duplicated their argument, so that each location can consume a version of the final structure located at its own position, so let's stick a duplicate in here. -->

# Filling in the holes to fix our problem

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _ fs . duplicate
```

. . .

```
Found hole ‘_’ with type: w (w a -> a) -> w (w a) -> w a
    Relevant bindings include
      fs       :: w (w a -> a)
      evaluate :: w (w a -> a) -> w a
```

\vspace*{3\baselineskip}

<!-- So we've got something of type w (w a -> a) -- our input -- and we'll *be given* something of type w (w a) -- by the fixed-point operator -- and we need to synthesize something of type (w a) from those two. -->

<!-- Hoogling this exact type signature doesn't work, but if we replace (w a) in the above with some arbitrary type b, this looks exactly like the Applicative pattern! f (b -> a) -> f b -> f a. And Hoogle will now happily tell us that there's a version of Applicative for comonads: ComonadApply. -->

# Filling in the holes to fix our problem

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
```

. . .

```
Could not deduce (ComonadApply w)
   arising from a use of ‘<@>’
   from the context (Comonad w)
   bound by the type signature for evaluate
   Possible fix:
      add (ComonadApply w) to the context
      of the type signature for evaluate.
```     

# Filling in the holes to fix our problem

\vspace*{2.1\baselineskip}

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
```

. . .

\begin{center}
   \includegraphics[height=10\baselineskip]{Rainbow.jpg}
\end{center}

# But will it blend?

Let's try to count to 10000 (again)!

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

main = print . S.take 10000 . viewR . evaluate $
   Tape (S.repeat (const 0)) -- zero left of origin
        (const 0)            -- zero at origin
        (S.repeat                    -- right of origin:
           (succ . extract . moveL)) -- 1 + leftward value
```

# But will it blend?

`$ time ./evaluate`

. . .

```
[0,1,2,3,4 ... a blink of an eye later ... 9998, 9999, 10000]
       0.01 real        0.00 user         0.00 sys
```

. . .

Still very slightly slower than `take 10000 [1..]`{.haskell}, almost certainly because GHC fuses away the intermediate list.

<!-- \vspace*{8.65\baselineskip} -->

<!-- We just derived our comonadic fixed point, in the process discovering a necessary and sufficient condition for its efficiency. Thanks, type inference! -->

# Zippy comonads $\to$ zippy computation


