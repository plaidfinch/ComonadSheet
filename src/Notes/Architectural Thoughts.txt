A separate closed type family in conjunction with an (open, as always) typeclass can simulate a "closed" typeclass, in that it is impossible to define new non-bottom instances once all the possibilities of the type family have been exhausted. This might be slightly untrue in the case of OverlappingInstances...

The InsertNested and InsertBase typeclasses have the same signature, but mean different things. The user of the library, if they want to add a new thing to be inserted or a new thing to be inserted into, should only have to modify InsertBase.

While (as noted above) the Insert module can be easily extended to insert new types of list-like things, or to insert *into* new types of tape-like things, Slice is (at present 05-26-2014) not so parameterized; it's specific to Tapes and nested tapes. New instances can be provided for new tape-like types, but it's a fair amount of boilerplate to do so.


