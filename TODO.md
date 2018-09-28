#TODO

1) ~Add compatibility with Lists/Arrays/Etc...~ Improve compatibility
2) ~Add compatibility with Class/Struct type properties~ Improve compatibility
3) Add compatibility with Foreign Keys (?)
4) ~Add compatibility with Nullable types~ Improve compatibility
5) Add compatibility with ValueTuple
6) Improve cache
7) Improve performance

#Possible implementation of Lists/Arrays/Etc...

Currently, I will only plan to support:

T[] // All kind of matrixes hopefully, initial support will be probably upto [][][]

List(Of T)

ValueTuple(Of T1, T2, ..., T7) // I will also support TRest As Structure later

Dictionary(Of TKey, TValue)

And all variants of IEnumerable(Of T)
