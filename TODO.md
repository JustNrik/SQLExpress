#TODO

1) Add compatibility with Lists/Arrays/Etc...
2) Add compatibility with Class/Struct type properties
3) Add compatibility with Foreign Keys (?)
4) ~Add compatibility with Nullable types~ Improve compatibility with Nullable types
5) Improve cache
6) Improve performance

#Possible implementation of Lists/Arrays/Etc...

Currently, I will only plan to support:

T[]

T[][]

T[][][]

List(Of T)

ValueTuple(Of T1, T2)

ValueTuple(Of T1, T2, T3)

Dictionary(Of TKey, TValue)


And all variants of IEnumerable(Of T)/(Of T1, T2)/(Of T1, T2, T3)
