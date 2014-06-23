The great package reorganization of 2014!

Package: Tape
Tape        -> Data.Stream.Tape

Package: NestedFunctor
Nested      -> Data.Functor.Nested

Package: IndexedList
IndexedList -> Data.List.Indexed (conversion functions and re-export)
               Data.List.Indexed.Counted
               Data.List.Indexed.Counted.Tuple (doesn't exist yet)
               Data.List.Indexed.Conic (rename TaggedList to ConicList)

Package: PeanoWitness
Peano       -> Data.Numeric.Witness.Peano

Package: ComonadSheet (note the rename... again)
Evaluate    -> Control.Comonad.Sheet (re-exports everything like All)
Indexed     -> Control.Comonad.Sheet.Indexed
Names       -> Control.Comonad.Sheet.Names
Reference   -> Control.Comonad.Sheet.Reference
Manipulate  -> Control.Comonad.Sheet.Manipulate
(none)      -> Control.Comonad.Sheet.TH (doesn't exist yet; TH for names)
