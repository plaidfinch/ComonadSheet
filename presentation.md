\title{Getting A Quick Fix on Comonads}
\subtitle{A quest to \texttt{extract} computation and not \texttt{duplicate} work}
\author{Kenneth Foner}
\institute{Brandeis University / Galois, Inc.}
\date{\today}

\titlepage

# A tale of two blog articles

Dan Piponi, November 2006 (<http://blog.sigfpe.com>):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

. . .

Example:

`> loeb [length, (!! 0), \x -> x !! 0 + x !! 1]`{.haskell}
\vspace*{1\baselineskip}

# A tale of two blog articles

Dan Piponi, November 2006 (<http://blog.sigfpe.com>):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

Example:

`> loeb [length, (!! 0), \x -> x !! 0 + x !! 1]`{.haskell}
\newline
`[3,3,6]`{.haskell}

# A tale of two blog articles

Dan Piponi, November 2006 (<http://blog.sigfpe.com>):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

Example:

`> loeb [length, sum]`{.haskell}
\vspace*{1\baselineskip}

# A tale of two blog articles

Dan Piponi, November 2006 (<http://blog.sigfpe.com>):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

Example:

`> loeb [length, sum]`{.haskell}
\newline
$\bot$

# A tale of two blog articles

Dan Piponi, December 2006 (<http://blog.sigfpe.com>):

### "Evaluating Cellular Automata is Comonadic"

> I want to work on 'universes' that extend to infinity in both directions. And I want this universe to be constructed lazily on demand.

> We can think of a universe with the cursor pointing at a particular element as being an element with a neighbourhood on each side.

# An unexpected journey

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

> - `loeb` is a fixed-point where each element of the structure refers to the whole structure in terms of *absolute position*
> - Comonadic computations are often described as computations in the context of a *relative position* within a larger structure

These articles are talking about almost the same thing!

\note{This talk is about my quest to find that missing *je ne sais quoi* and unify these two notions.}

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

\note{Why am I going to ignore extend? Same reason as why I'm ignoring bind: it 's the less clear presentation of the same ideas for our purposes today.}

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

\note{Note that `head` and `tail` are total functions.}

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

\note{It's infinite in both directions, like a Turing-machine's... `Tape`.}

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

Duplicate is "diagonalization." Movement and duplication commute:

```Haskell
moveL . duplicate == duplicate . moveL
moveR . duplicate == duplicate . moveR
```

<!-- . . . -->

\note{I have a hand-wavy proof that this is the only definition of `duplicate` which satisfies the comonad laws for `Tape`.}

\note{(I welcome proofs using more category theory than gesticulation.)}

# Back to Piponi's `loeb`

Löb's theorem: $\Box(\Box P \to P) \to \Box P$

> I'm going to take that as my theorem from which I'll derive a type. But what should $\Box$ become in Haskell?

> We'll defer that decision until later and assume as little as possible. Let's represent $\Box$ by a type that is a Functor.

(Piponi, 2006)

. . .

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
```

. . .

But $\Box$ could also have more structure...

# Fixed That For You

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

. . .

```Haskell
fix :: (a -> a) -> a
fix f = let x = f x in x
```

. . .

We can redefine Piponi's `loeb` in terms of `fix`:

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = fix $ \xs -> fmap ($ xs) fs
```

I'll use this one from now on.

# Fixed That For You

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = fix $ \xs -> fmap ($ xs) fs
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
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = fix $ \xs -> fmap ($ xs) fs
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
       (S.repeat                   -- right of origin:
         (succ . extract . moveL)) -- 1 + leftward value
```

(This syntax gets more elegant later.)

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

# Sharing is caring

```Haskell
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix (duplicate w))
```

So more specifically: `wfix` can't be expressed in terms of `fix` on its *argument*—there's always that extra `extract w` on the outside which can't be folded into the recursion.

Why does this mean it's inefficient?

\vspace*{1.5\baselineskip}

. . .

We can't hold onto a singular (lazy) reference to the eventual future of the computation.

# Filling in the holes to fix our problem

**Epiphany**: Any optimally sharing "evaluation" function has to be expressible in the form:

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

\note{All of the previous comonadic fixed-points we were using duplicated their argument, so that each location can consume a version of the final structure located at its own position. Let's stick a duplicate in here.}

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

\note{So we've got something of type w (w a $\to$ a) -- our input -- and we'll *be given* something of type w (w a) -- by the fixed-point operator -- and we need to synthesize something of type (w a) from those two.}

\note{Let's put our fs argument in as an argument to our hole: we know it has to be used somewhere, and it's gotta be there.}

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

\note{Hoogling this exact type signature doesn't work, but if we replace (w a) in the above with some arbitrary type b, this looks exactly like the Applicative pattern! f (b $\to$ a) $\to$ f b $\to$ f a. And Hoogle will now happily tell us that there's a version of Applicative for comonads: ComonadApply.}

# Filling in the holes to fix our problem

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
```

`(<@>) :: ComonadApply w => w (a -> b) -> w a -> w b`{.haskell}

. . .

```
Could not deduce (ComonadApply w)
   arising from a use of ‘<@>’
   Possible fix:
      add (ComonadApply w) to the context
      of the type signature for evaluate.
```

\note{Oh, obviously: we need to fix that constraint now.}

# Filling in the holes to fix our problem

\vspace*{3.4\baselineskip}

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
```

`(<@>) :: ComonadApply w => w (a -> b) -> w a -> w b`{.haskell}

. . .

\begin{center}
   \includegraphics[height=10\baselineskip]{Rainbow.jpg}
\end{center}

\note{We just derived our comonadic fixed point, in the process discovering a necessary and sufficient condition for its efficiency. Thanks, type inference!}

# But will it blend?

Let's try to count to 10000\dots again!

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

main = print . S.take 10000 . viewR . evaluate $
  Tape (S.repeat (const 0)) -- zero left of origin
       (const 0)            -- zero at origin
       (S.repeat                   -- right of origin:
         (succ . extract . moveL)) -- 1 + leftward value
```

# But will it blend?

`$ time ./evaluate`

. . .

```
[0,1,2,3,4 ... a blur on the screen ... 9999, 10000]
       0.01 real        0.00 user         0.00 sys
```

. . .

Still very slightly slower than `take 10000 [1..]`{.haskell}, almost certainly because GHC fuses away the intermediate list.

. . .

**Aside**: list fusion in `evaluate`: reducible to halting problem?

# Wait just a second!

"Hang on, Kenny! We don't know why `Tape`s are instances of `ComonadApply`!"

. . .

Okay, here you go:

```Haskell
instance ComonadApply Tape where
   (Tape ls c rs) <@> (Tape ls' c' rs') =
      Tape (ls <@> ls') (c c') (rs <@> rs')
```

. . .

"But that relies on the `ComonadApply` instance for `Stream`s!"

\note{How are you doing that thing where you speak in monospace fonts, anonymous interrogator? Anyhow...}

. . .

```Haskell
instance ComonadApply Stream where (<@>) = (<*>)
```

. . .

Just kidding; here's the `Applicative` instance for `Stream`:

```Haskell
instance Applicative Stream where
  pure  = repeat
  (<*>) = zipWith ($)
```

\note{If something is both an Applicative and a ComonadApply, we have to make the two 'apply' operations equivalent. Edward Kmett says so.}

# But what *is* a `ComonadApply` anyhow?

\note{Speaking of other things Edward Kmett says:}

. . .

*It is a strong lax symmetric semi-monoidal comonad on the category Hask of Haskell types. That it to say that w is a strong lax symmetric semi-monoidal functor on Hask, where both extract and duplicate are symmetric monoidal natural transformations.*
\newline
\hspace*{.5em}
—Edward Kmett

\vspace*{1\baselineskip}

. . .

*ComonadApply is to Comonad like Applicative is to Monad.*
\newline
\hspace*{.5em}
—Edward Kmett

# But what *is* a `ComonadApply` anyhow?

### The laws of `ComonadApply`:

```Haskell
(.) <$> u <@> v <@> w == u <@> (v <@> w)
extract   (p <@> q)   == extract p (extract q)
duplicate (p <@> q)   == (<@>) <$> duplicate p <@> duplicate q
```

. . .

These laws (among other things) imply that `(<@>)`{.haskell} *must* behave in a "zippy" fashion, lining up structure and preserving cardinality.

. . .

In Uustalu and Vene's *The Essence of Dataflow Programming*, `ComonadApply` was called `ComonadZip`, and defined in terms of the function:

\ \ \ \ \ `czip :: (ComonadZip d) => d a -> d b -> d (a,b)`{.haskell}

. . .

**Aside**: Enlightening exercise: for an arbitrary `Functor f`, given `pair :: forall a b. f a -> f b -> f (a,b)`{.haskell}, show that `pair` and `(<*>)`{.haskell} can be defined in terms of one another, modulo `fmap`.

# Zippy comonads $\to$ zippy computation

\large The "zippiness" required by the laws of `ComonadApply` is also the source of `evaluate`'s *computational* "zippiness."

# Do you want to build a comonad?

\note{Now that we have an efficient evaluation function, the next step is to see what things we can evaluate!}

. . .

Nesting `Tape`s inside one another leads us into to higher-dimensional (discrete) spaces to explore.
\newline

`Tape a`\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ $\cong$ `Integer` $\to$ a

`Tape (Tape a)`\ \ \ \ \ \ \ \ \ \ \ \ $\cong$ `(Integer,Integer)` $\to$ a

`Tape (Tape (Tape a))`\ $\cong$ `(Integer,Integer,Integer)` $\to$ a

Etcetera, ad infinitum!

# Do you want to build a comonad?

We could define a `newtype` for each added dimension, but this carries an overhead of between $O(n^2)$ and $O(n^3)$ boilerplate per dimension.

. . .

```Haskell
newtype Tape2 a = Tape (Tape a)
newtype Tape3 a = Tape (Tape (Tape a))
...
```

```Haskell
instance Functor Tape2 where ...
instance Comonad Tape2 where ...
instance ComonadApply Tape2 where ...

instance Functor Tape3 where ...
instance Comonad Tape3 where ...
instance ComonadApply Tape3 where ...
...
```

That also `succ`s a lot.

# Do you want to build a comonad?

There has to be a better solution—there's a pattern here.

. . .

Composition of functors (from `Data.Functor.Compose`):

```Haskell
newtype Compose f g a = Compose { getCompose :: f (g a) }

(Functor f, Functor g)         => Functor (Compose f g)
(Applicative f, Applicative g) => Applicative (Compose f g)
```

. . .

```Haskell
instance (Comonad f, Comonad g) => Comonad (Compose f g) where
  extract   = extract . extract . getCompose
  duplicate = ...
```

Hmm. What *does* go there?

# Do you want to build a comonad?

(N.B. In this section, I've specialized many type signatures.)

**What can you do with a `(Compose f g a)`{.haskell}?**

. . .

Not much; you have to unwrap it. So, what can you do with something of type `(Comonad f, Comonad g) => f (g a)`{.haskell}?

. . .

**You can duplicate the outer layer:**

`duplicate :: f (g a) -> f (f (g a))`{.haskell}

. . .

**You can duplicate the inner layer:**

`fmap duplicate :: f (g a) -> f (g (g a))`{.haskell}

. . .

**You can duplicate both:**

`duplicate . fmap duplicate :: f (g a) -> f (f (g (g a)))`{.haskell}

. . .

**And that's about it.**

# Do you want to build a comonad?

If only we had `??? :: f (f (g (g a))) -> f (g (f (g a)))`{.haskell}, then we could wrap its result back up inside *two* `Compose`{.haskell}s:

```Haskell
Compose . fmap Compose
    :: f (g (f (g a))) -> Compose f g (Compose f g a)
```

. . .

Putting it together:

```Haskell
Compose . fmap Compose -- wrap again
. ???                  -- swap middle two layers
. duplicate            -- duplicate outside
. fmap duplicate       -- duplicate inside
. getCompose           -- unwrap
    :: Compose f g a -> Compose f g (Compose f g a)
```

And that's our `duplicate`!

\note{But what's the mystery function\dots?}

# Type sleuth vs. the mysterious functor-swapper

Whatever `???` is, it likely has a more generic type.

\note{Let's guess that we're looking for something which swaps just two layers.}

. . .

```Haskell
???      ::    f (g x)      ->    g (f x)
fmap ??? :: f (f (g (g a))) -> f (g (f (g a)))
```

. . .

```Haskell
Compose . fmap Compose -- wrap again
. fmap ???             -- swap middle two layers
. duplicate            -- duplicate outside
. fmap duplicate       -- duplicate inside
. getCompose           -- unwrap
    :: Compose f g a -> Compose f g (Compose f g a)
```

. . .

Two candidates:

```Haskell
sequenceA    -- from Data.Traversable
  :: (Traversable t, Applicative f) => t (f a) -> f (t a)
distribute   -- from Data.Distributive
  :: (Distributive g, Functor f)    => f (g a) -> g (f a)
```

# Type sleuth vs. the mysterious functor-swapper

```Haskell
sequenceA    -- from Data.Traversable
  :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

Initially promising—I know and love `Traversable`.

. . .

Requires two constraints:

- `Applicative f` means our outer comonad has to have both `(<*>)`{.haskell} and `pure`—somewhat acceptable.
    + But `pure` is a hard pill to swallow.
- `Traversable t`—that's a deal-breaker!
    + Jaskelioff & Rypacek, MSFP 2012, "An Investigation of the Laws of Traversals": "We are not aware of any functor that is traversable and is not a finitary container."
    + Moreover, infinite streams are definitely not traversable.

# Type sleuth vs. the mysterious functor-swapper

```Haskell
distribute   -- from Data.Distributive
  :: (Distributive g, Functor f) => f (g a) -> g (f a)
```

But what does `Distributive` mean?

. . .

What can you do underneath a `Functor`?

. . .

"Touch, don't look."

. . .

A strategy/intuition for `distribute`:

- Fill a `g`-shaped structure with copies of `f (g a)`: `g (f (g a))`.
- For each `f (g a)` on the inside, navigate to a particular focus (using `fmap`) and `fmap extract` to eliminate the inner `g`.

\note{What do we need to have a Distributive?

We already know f is a functor; it's a comonad! And Distributive only has Functor as a superclass, so we're good there too.

You can think of the Functor interface as a one-way membrane which lets us "send commands" to the thing inside the box, but not ask it anything about itself.

So we can't get information out: all things of type `g` must have identical structure. Streams, length-indexed lists, and functions are all distributive, but not lists or finite trees.}

# Mystery solved

```Haskell
instance (Comonad f, Comonad g, Distributive g)
  => Comonad (Compose f g) where
extract   = extract . extract . getCompose
duplicate = Compose . fmap Compose -- wrap again
            . distribute           -- swap middle two layers
            . duplicate            -- duplicate outside
            . fmap duplicate       -- duplicate inside
            . getCompose           -- unwrap
```

# Mystery solved

```Haskell
unfold prev center next x =
   Tape (S.unfold prev x) (center x) (S.unfold next x)

instance Distributive Tape where
   distribute =
      unfold (fmap (focus . moveL) &&& fmap moveL)
             (fmap focus)
             (fmap (focus . moveR) &&& fmap moveR)
```

\note{All the comonads we want to talk about (Tapes and Streams and their ilk) are of fixed cardinality (i.e. countable infinity), with no extra information.

  The triple-ampersand operator is from Control.Arrow; it makes a function which takes something and returns a pair consisting of the results of applying each of its two function arguments to that something.}

# The story so far

. . .

Efficient evaluation:

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
```

. . .

Elegant composition:

```Haskell
(Comonad f, Comonad g, Distributive g) => Comonad (Compose f g)
```

. . .

I could make a library out of this!

\note{First, though, some ugly truths have to be brought to light.}

# Baby, there's a shark in the water

\begin{center}
\includegraphics[width=\textwidth]{shark-fin-ocean.jpg}
\end{center}

# GADTs to the rescue!

Previously:

```Haskell
newtype Compose f g a = { getCompose :: f (g a) }
```

. . .

What if we put *depth of nesting* in the types?

```Haskell
data Flat x
data Nest o i

data Nested fs a where
   Flat :: f a -> Nested (Flat f) a
   Nest :: Nested fs (f a) -> Nested (Nest fs f) a
```

. . .

```Haskell
            Just [1]   :: Maybe [Int]
      Flat (Just [1])  :: Nested (Flat Maybe) [Int]
Nest (Flat (Just [1])) :: Nested (Nest (Flat Maybe) []) Int
```
