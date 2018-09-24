Imports System.Runtime.CompilerServices

Friend Module Extensions
    <Extension>
    Friend Function ToDictionary(Of TKey, TValue)(dict As IEnumerable(Of KeyValuePair(Of TKey, TValue))) As IDictionary(Of TKey, TValue)
        Return dict.ToDictionary(Function(x) x.Key, Function(x) x.Value)
    End Function
End Module
