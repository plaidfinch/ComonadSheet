\title{Getting a Quick Fix on Comonads}
\subtitle{A quest to \texttt{extract} computation and not \texttt{duplicate} work}
\author{Kenneth Foner}
\institute{Brandeis University / Galois, Inc.}
\date{\today}

\titlepage

\note{
Hi, I'm Kenny Foner\dots

This talk is about a journey I've been on\dots

And in the telling of a journey, the best place to start is often the beginning.
}

# A tale of two blog articles

Dan Piponi, November 2006 (`blog.sigfpe.com`):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

. . .

Example:

`> loeb [length, (!! 0), \x -> x !! 0 + x !! 1]`{.haskell}
\vspace*{.9\baselineskip}

\note{Beginning of journey: reading Dan Piponi's excellent blog.

Explain loeb.
}

# A tale of two blog articles

Dan Piponi, November 2006 (`blog.sigfpe.com`):

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

Dan Piponi, November 2006 (`blog.sigfpe.com`):

### "From Löb's Theorem to Spreadsheet Evaluation"

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

Example:

`> loeb [length, sum]`{.haskell}
\vspace*{.9\baselineskip}

\note{But what if we try to evaluate this expression?}

# A tale of two blog articles

Dan Piponi, November 2006 (`blog.sigfpe.com`):

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

Dan Piponi, December 2006 (`blog.sigfpe.com`):

### "Evaluating Cellular Automata is Comonadic"

> I want to work on 'universes' that extend to infinity in both directions. And I want this universe to be constructed lazily on demand.

> We can think of a universe with the cursor pointing at a particular element as being an element with a neighbourhood on each side.

\note{All that was fascinating. But the true beginning was when I read a second post, also by Dan Piponi.

Thank you, Dan.}

# An unexpected journey

```Haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
```

> - `loeb`: each element refers to *absolute positions* in a structure
> - comonads: computations in context of *relative position* in a structure

These are almost the same thing!

\note{This talk is about my quest to find that missing \emph{je ne sais quoi} and unify these two notions.}

# (Co)monads: a brief summary

### Monads:

Most Haskellers define monads via `return` and `(>>=)`{.haskell}. Today, we'll use `return` and `join`. *Note:* `x >>= f == join (fmap f x)`{.haskell}.

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

\note{Before I go any further, I want to give a quick summary of comonads. They're the star of this show.

  Perhaps I should say coproductary instead of summary, for you category theory folk.

  I'm ignoring extend (cobind) for the same reason as why I'm ignoring bind: things today will make more sense in terms of return/coreturn and join/cojoin.}

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

\note{In particular, most of the comonadic structures I'll be talking about are based off a very simple data structure.

  Note that `head` and `tail` are total functions.}

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

\note{Using the stream type, we can create something more interesting.

Streams have no notion of the past, only the future; they can only look in one direction.

Tapes are a bidirectional stream, also known as a stream zipper.}

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

\note{They have a more satisfying comonad instance than streams, IMO.

  I have a hand-wavy proof that this is the only definition of `duplicate` which satisfies the comonad laws for `Tape`.

(I welcome proofs using more category theory than gesticulation.)}

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

\note{
Now let's get back to Piponi's loeb function. He takes Lob's theorem from modal logic, and decides to find a computational interpretation of it.
}

# Fixed that for you

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

# Fixed that for you

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

# Fixed that for you

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

# Is this our fix?

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

# Sharing is caring (as well as polynomial complexity)

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

- In a higher-dimensional space with $> 1$ reference per cell, would be exponential or worse.

<!-- + possibly worse than that, as I saw when experimenting with it—likely due to the garbage collector -->

. . .

That really `succ`s.

# Sharing is caring (as well as polynomial complexity)

```Haskell
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix (duplicate w))
```

The root of the problem: `wfix` can't be expressed in terms of `fix`.

. . .

<!-- For the wise-guys in the audience, the following is *not* what I mean by "in terms of `fix`": -->

```Haskell
notWhatI'mTalkingAbout :: Comonad w => w (w a -> a) -> a
notWhatI'mTalkingAbout =
   fix $ \wfix ->
      \w -> extract w (fmap wfix (duplicate w))
```

# Holding on to the future

```Haskell
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (fmap wfix (duplicate w))
```

More specifically: `wfix` is inexpressible in terms of `fix` on its *argument*.

Why does this mean it's inefficient?

\vspace*{1.5\baselineskip}

. . .

No single reference to the eventual future of the computation.

# Holding on to the future

**Epiphany**: Any efficient "evaluation" function looks like:

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _
```

<!-- and by efficient, I mean optimally sharing -->

<!-- GHC 7.8 gave us typed holes—let's use them to fill in the blank! -->

# Filling in the holes

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _
```

. . .

\texttt{Found hole with type: \color{purple}w a -> w a}

(Error messages have been cleaned for your viewing enjoyment.)

\vspace*{5.75\baselineskip}

\note{All of the previous comonadic fixed-points we were using duplicated their argument, so that each location can consume a version of the final structure located at its own position. Let's stick a duplicate in here.}

# Filling in the holes

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _ . duplicate
```

. . .

\texttt{Found hole with type: \color{purple}w (w a) -> w a}

\vspace*{7.25\baselineskip}

\note{So we've got something of type w (w a $\to$ a) -- our input -- and we'll *be given* something of type w (w a) -- by the fixed-point operator -- and we need to synthesize something of type (w a) from those two.}

\note{Let's put our fs argument in as an argument to our hole: we know it has to be used somewhere, and it's gotta be there.}

# Filling in the holes

```Haskell
evaluate :: Comonad w => w (w a -> a) -> w a
evaluate fs = fix $ _ fs . duplicate
```

. . .

\texttt{Found hole with type: \color{purple}w (w a -> a) -> w (w a) -> w a}

\vspace*{7.25\baselineskip}

\note{Hoogling this exact type signature doesn't work, but if we replace (w a) in the above with some arbitrary type b, this looks exactly like the Applicative pattern! f (b $\to$ a) $\to$ f b $\to$ f a. And Hoogle will now happily tell us that there's a version of Applicative for comonads: ComonadApply.}

# Filling in the holes

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

# Filling in the holes

\vspace*{3.4\baselineskip}

```Haskell
evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
```

`(<@>) :: ComonadApply w => w (a -> b) -> w a -> w b`{.haskell}

\vspace*{12.5\baselineskip}

# 

\begin{center}
   \includegraphics[width=\textwidth]{Rainbow.jpg}
\end{center}

\note{We just derived our comonadic fixed point, in the process discovering a necessary and sufficient condition for its efficiency. Thanks, type inference!}

# Will it blend?

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

# Will it blend?

`$ time ./evaluate`

. . .

```
[0,1,2,3,4 ... a blur on the screen ... 9999, 10000]
       0.01 real        0.00 user         0.00 sys
```

. . .

Still very slightly slower than `take 10000 [1..]`{.haskell}, almost certainly because GHC fuses away the intermediate list.

**Aside**: list fusion in `evaluate`: reducible to halting problem?

# Wait just a minute!

"Hang on, Kenny! We don't know why `Tape`s are `ComonadApply`!"

. . .

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

```Haskell
instance Applicative Stream where
  pure  = repeat
  (<*>) = zipWith ($)
```

\note{If something is both an Applicative and a ComonadApply, we have to make the two 'apply' operations equivalent. Edward Kmett says so.}

# What *is* a `ComonadApply` anyhow?

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

# What *is* a `ComonadApply` anyhow?

### The laws of `ComonadApply`:

```Haskell
(.) <$> u <@> v <@> w == u <@> (v <@> w)
extract   (p <@> q)   == extract p (extract q)
duplicate (p <@> q)   == (<@>) <$> duplicate p <@> duplicate q
```

. . .

These laws mean `(<@>)`{.haskell} *must* be "zippy."

<!-- lining up structure and preserving cardinality -->

. . .

Uustalu and Vene's *The Essence of Dataflow Programming* calls it:

\ \ \ \ \ `czip :: (ComonadZip d) => d a -> d b -> d (a,b)`{.haskell}

. . .

**Enlightening exercise:** for an arbitrary `Functor f`, show how `czip` and `(<@>)`{.haskell} can be defined in terms of each other and `fmap`.

# Zippy comonads $\to$ zippy computation

\large The "zippiness" required by the laws of `ComonadApply` is also the source of `evaluate`'s *computational* "zippiness."

<!-- talk here about why you can't have dynamic cycle detection if you want asymptotic efficiency, also about how static checking is a dead-end, and why (laziness makes static reference analysis overpredict) -->

# Can going fast be total(ly safe)?

. . .

\large Short answer: no.

\large Long answer: not in ways we would care about.

# But I'm more than a one-dimensional character

\note{Now that we have an efficient evaluation function, the next step is to see what things we can evaluate!}

. . .

Nesting `Tape`s inside one another leads us into to higher-dimensional (discrete) spaces to explore.
\newline

`Tape a`\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ $\cong$ `Integer` $\to$ a

`Tape (Tape a)`\ \ \ \ \ \ \ \ \ \ \ \ $\cong$ `(Integer,Integer)` $\to$ a

`Tape (Tape (Tape a))`\ $\cong$ `(Integer,Integer,Integer)` $\to$ a

Etcetera, ad infinitum!

# But I'm more than a one-dimensional character

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

That also really `succ`s.

# But I'm more than a one-dimensional character

<!-- There has to be a better solution—there's a pattern here. -->

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

<!-- Hmm. What *does* go there? -->

# Do you want to build a comonad?

(N.B. In this section, I've specialized many type signatures.)

**What can you do with `(Compose f g a)`{.haskell}?**

. . .

Equivalent: what can you do with `(Comonad f, Comonad g) => f (g a)`{.haskell}?

. . .

**Duplicate outer layer:**

`duplicate :: f (g a) -> f (f (g a))`{.haskell}

. . .

**Duplicate inner layer:**

`fmap duplicate :: f (g a) -> f (g (g a))`{.haskell}

. . .

**Duplicate both:**

`duplicate . fmap duplicate :: f (g a) -> f (f (g (g a)))`{.haskell}

# Do you want to build a comonad?

If only we had `f (f (g (g a))) -> f (g (f (g a)))`{.haskell}\dots

```Haskell
Compose . fmap Compose -- wrap again
. ???                  -- swap middle two layers
. duplicate            -- duplicate outside
. fmap duplicate       -- duplicate inside
. getCompose           -- unwrap
    :: Compose f g a -> Compose f g (Compose f g a)
```

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

# Type sleuth vs. the mysterious functor-swapper

Two candidates (thanks Hoogle!):

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

> - `Applicative f`: outer layer has to have `(<*>)`{.haskell} and `pure`—`pure` is a hard pill to swallow.
> - `Traversable t`—that's a deal-breaker!

. . .

Jaskelioff & Rypacek, MSFP 2012, "An Investigation of the Laws of Traversals": "We are not aware of any functor that is traversable and is not a finitary container."

- Infinite streams are definitely not `Traversable`.

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

# Type sleuth vs. the mysterious functor-swapper

```Haskell
distribute   -- from Data.Distributive
  :: (Distributive g, Functor f) => f (g a) -> g (f a)
```

Strategy/intuition for `distribute`:

> - Start with `f (g a)`
> - Create a `g` with `f (g a)` in each 'hole': `g (f (g a))`
> - For each `f (g a)` on the inside of `g`:
>     + navigate to a particular focus (using `fmap`)
>     + `fmap extract` to eliminate the inner `g`
> - Result: `g (f a)`

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

# 

\begin{center}
\includegraphics[width=\textwidth]{shark-fin-ocean.jpg}
\end{center}

# Baby, there's a shark in the water

```Haskell
type family ComposeCount f where
  ComposeCount (Compose f g a) = Succ (ComposeCount (f (g a)))
  ComposeCount x               = Zero

class CountCompose f where
  countCompose :: f -> ComposeCount f
```

\vspace{3.75\baselineskip}

# Baby, there's a shark in the water

```Haskell
{-# LANGUAGE FeelBadAboutYourself #-}

type family ComposeCount f where
  ComposeCount (Compose f g a) = Succ (ComposeCount (f (g a)))
  ComposeCount x               = Zero

class CountCompose f where
  countCompose :: f -> ComposeCount f
```

\vspace{6.75\baselineskip}

# Baby, there's a shark in the water

```Haskell
{-# LANGUAGE OverlappingInstances #-}

type family ComposeCount f where
  ComposeCount (Compose f g a) = Succ (ComposeCount (f (g a)))
  ComposeCount x               = Zero

class CountCompose f where
  countCompose :: f -> ComposeCount f
```

. . .

```Haskell
instance (CountCompose (f (g a)))
  => CountCompose (Compose f g a) where
  countCompose (Compose x) = Succ (countCompose x)
```

. . .

```Haskell
instance (ComposeCount f ~ Zero) => CountCompose f where
  countCompose _ = Zero
```

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

# Nest it / `fmap` it / quick rewrap it

Two cases for each instance (base case/recursive case):

```Haskell
instance (Functor f) => Functor (Nested (Flat f)) where
   fmap f (Flat x) = Flat $ fmap f x

instance (Functor f, Functor (Nested fs))
   => Functor (Nested (Nest fs f)) where
   fmap f (Nest x) = Nest $ fmap (fmap f) x
```

The rest of the instances for look similar.

# Nest it / `fmap` it / quick rewrap it

> - Match on types, not constraints
> - Base case is `Flat`, not every type, so no "universal instance"
> - See ya later, `OverlappingInstances`!

# Drag and drop it / zip - unzip it

### What's in a reference?

. . .

```Haskell
{-# LANGUAGE DataKinds #-}

data RefType = Relative | Absolute

data Ref (t :: RefType) where
   Rel :: Int -> Ref Relative
   Abs :: Int -> Ref Absolute
```

# Drag and drop it / zip - unzip it

```Haskell
type family Combine a b where
   Combine Relative Absolute = Absolute
   Combine Absolute Relative = Absolute
   Combine Relative Relative = Relative
```

. . .

```Haskell
class CombineRefs a b where ...
instance CombineRefs Absolute Relative where ...
instance CombineRefs Relative Absolute where ...
instance CombineRefs Relative Relative where ...
```

. . .

```Haskell
... combine :: Ref a -> Ref b -> Ref (Combine a b)
... combine (Abs a) (Rel b) = Abs (a + b)
... combine (Rel a) (Abs b) = Abs (a + b)
... combine (Rel a) (Rel b) = Rel (a + b)
```

. . .

- Split presentation style due to Conor McBride, JFP 2001:\newline\emph{Faking It: Simulating Dependent Types in Haskell}

# He's making a list and checking it statically

```Haskell
data x :-: y
data Nil

data ConicList f ts where
   (:-:) :: f x -> ConicList f xs -> ConicList f (x :-: xs)
   ConicNil  :: ConicList f Nil

type RefList = ConicList Ref
```

It's called a conic list because category theory:\newline
`forall a. f a -> x`{.haskell} is a co-cone, and this looks like that.

# He's making a list and checking it statically

```Haskell
type family a & b where
   (a :-: as) & (b :-: bs) = Combine a b :-: (as & bs)
   Nil        & bs         = bs
   as         & Nil        = as
```

. . .

```Haskell
class CombineRefLists as bs where ...
instance (CombineRefs a b, CombineRefLists as bs)
      => CombineRefLists (a :-: as) (b :-: bs) where ...
instance CombineRefLists Nil        (b :-: bs) where ...
instance CombineRefLists (a :-: as) Nil        where ...
instance CombineRefLists Nil        Nil        where ...
```

. . .

```Haskell
... (&) :: RefList as -> RefList bs -> RefList (as & bs)
... (a :-: as) & (b :-: bs) = combine a b :-: (as & bs)
... ConicNil   & bs         = bs
... as         & ConicNil   = as
... ConicNil   & ConicNil   = ConicNil
```

<!-- Again, note the pattern of using closed type families to effectively close a nominally open typeclass -->

# He's making a list and checking it statically

With suitable definition of names\dots

```Haskell
a :: RefList (Relative :-: Relative :-: Nil)
a = belowBy 3 & rightBy 14
```

. . .

```Haskell
b :: RefList (Relative :-: Absolute :-: Nil)
b = columnAt 9000 & aboveBy 1
```

. . .

\color{red}\texttt{c = columnAt 5 \& columnAt 10}

# Take it / view it / go - insert it

```Haskell
class Take r t where
   type ListFrom t a
   take :: RefList r -> t a -> ListFrom t a

class View r t where
   type StreamFrom t a
   view :: RefList r -> t a -> StreamFrom t a

class Go r t where
   go :: RefList r -> t a -> t a
```

. . .

\dots and `insert` --- I have discovered a truly marvelous type signature for this, which this margin is too narrow to contain.

# What have we learned?

> - Efficient comonadic fixed-point requires zipping
> - Distributive comonads compose
> - Dimension polymorphism needs type-indexed composition
> - Heterogeneous lists unify absolute and relative references

. . .

- (Co)monads are (co)ol!

<!-- But what does it all look like? -->

# With great power comes code snippets for a tech talk

```Haskell
fibonacci :: Sheet1 Integer
fibonacci = evaluate . sheet 1 $
  repeat $ cell (leftBy 2) + cell left
```

(I told you the syntax would get nicer!)

. . .

```Haskell
> slice (leftBy 2) (rightBy 17) fibonacci
[1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]
```

# With great power comes code snippets for a tech talk

<!-- This is where we are now: -->

```Haskell
pascal :: Sheet2 Integer
pascal = evaluate . sheet 0 $
   repeat 1 <:> repeat (1 <:> pascalRow)
   where pascalRow = repeat $ cell above + cell left
```

. . .

```Haskell
> take (belowBy 9 & rightBy 9) pascal
```

. . .

\vspace*{-\baselineskip}
```Haskell
[[1,  1,  1,   1,   1,    1,    1,     1,     1,     1], 
 [1,  2,  3,   4,   5,    6,    7,     8,     9,    10], 
 [1,  3,  6,  10,  15,   21,   28,    36,    45,    55], 
 [1,  4, 10,  20,  35,   56,   84,   120,   165,   220], 
 [1,  5, 15,  35,  70,  126,  210,   330,   495,   715], 
 [1,  6, 21,  56, 126,  252,  462,   792,  1287,  2002], 
 [1,  7, 28,  84, 210,  462,  924,  1716,  3003,  5005], 
 [1,  8, 36, 120, 330,  792, 1716,  3432,  6435, 11440], 
 [1,  9, 45, 165, 495, 1287, 3003,  6435, 12870, 24310], 
 [1, 10, 55, 220, 715, 2002, 5005, 11440, 24310, 48620]]
```

# With great power comes code snippets for a tech talk

```Haskell
data Cell = X | O deriving ( Eq )

life :: ([Int],[Int]) -> [[Cell]] -> Sheet3 Cell
life ruleset seed =
   evaluate $ insert [map (map const) seed] blank where
     blank = sheet (const X) (repeat . tapeOf . tapeOf $ rule)
     rule place =
       case (neighbors place `elem`) `onBoth` ruleset of
            (True,_) -> O
            (_,True) -> cell inward place
            _        -> X
     neighbors = length . filter (O ==) . cells bordering
     bordering = map (inward &) (diag ++ vert ++ horz)
     diag = (&) <$> horizontals <*> verticals
     vert =        [above, below]
     horz = map d2 [right, left]

conway :: [[Cell]] -> Sheet3 Cell
conway = life ([3],[2,3])
```

# With great power comes code snippets for a tech talk

```Haskell
glider :: Sheet3 Cell
glider = conway [[X,X,O],
                 [O,X,O],
                 [X,O,O]]
```

. . .

```
> printLife glider
```

. . .

\vspace*{-\baselineskip}
\includegraphics[width=6em]{glider.png}

# 

\vspace*{2\baselineskip}

\begin{center}
\LARGE\texttt{cabal install ComonadSheet}
\vspace*{\baselineskip}

\texttt{github.com/kwf/ComonadSheet}

\Large Suggestions, bug reports, pull requests welcome!
\end{center}
