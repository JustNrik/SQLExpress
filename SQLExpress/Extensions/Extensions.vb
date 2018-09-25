Imports System.Runtime.CompilerServices

Public Module Extensions
    <Extension>
    Public Function ToDictionary(Of TKey, TValue)(dict As IEnumerable(Of KeyValuePair(Of TKey, TValue))) As IDictionary(Of TKey, TValue)
        Return dict.ToDictionary(Function(x) x.Key, Function(x) x.Value)
    End Function
    <Extension>
    Public Function Unawait(Of T)(task As Task(Of T)) As ConfiguredTaskAwaitable(Of T)
        Return task.ConfigureAwait(False)
    End Function
    <Extension>
    Public Function Unawait(task As Task) As ConfiguredTaskAwaitable
        Return task.ConfigureAwait(False)
    End Function
End Module
