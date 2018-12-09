Imports System.Collections.Immutable
Imports System.Convert
Imports System.Data.SqlClient
Imports System.Reflection
Imports System.Runtime.CompilerServices

Friend Module Helpers

    Friend Iterator Function GetTypes(Of T As {IStoreableObject})(obj As T) As IEnumerable(Of IStoreableObject)
        Dim properties = GetAllStoreablePropierties(obj.GetType.GetProperties)
        For Each prop In properties.Where(Function(x) IsClassOrStruct(x))
            Yield DirectCast(prop.PropertyType, IStoreableObject)
            For Each inprop In prop.PropertyType.GetProperties.Where(Function(x) IsClassOrStruct(x))
                Yield DirectCast(inprop.PropertyType, IStoreableObject)
                For Each intype In GetTypes(DirectCast(inprop.PropertyType, IStoreableObject))
                    If IsClassOrStruct(intype.GetType) Then Yield intype
                Next
            Next
        Next
        For Each prop In properties.Where(Function(x) IsCollection(x))
            For Each arg In prop.PropertyType.GenericTypeArguments
                If IsClassOrStruct(arg) Then
                    Dim instance = DirectCast(Activator.CreateInstance(arg), IStoreableObject)
                    Yield instance
                    For Each type In GetTypes(instance)
                        If IsClassOrStruct(type.GetType) Then Yield type
                    Next
                End If
            Next
        Next
    End Function

    Friend Function GetAllStoreablePropierties(properties As PropertyInfo()) As ImmutableArray(Of PropertyInfo)
        Return (From [property] In properties
                Where [property].GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                Order By [property].GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray
    End Function

    Friend Function GetPrivitimes(properties As ImmutableArray(Of PropertyInfo)) As PropertyInfo()
        Return (From [property] In properties
                Where Not IsCollection([property]) AndAlso
                      Not IsClassOrStruct([property]) AndAlso
                      Not IsTuple([property])).ToArray()
    End Function

    Friend Function IsTuple([property] As PropertyInfo) As Boolean
        Return GetType(ITuple).IsAssignableFrom([property].PropertyType)
    End Function

    Friend Function IsCollection([property] As PropertyInfo) As Boolean
        Return GetType(IEnumerable).IsAssignableFrom([property].PropertyType) AndAlso Not GetType(String).IsAssignableFrom([property].PropertyType)
    End Function

    Friend Function IsClassOrStruct([property] As PropertyInfo) As Boolean
        Dim type = [property].PropertyType

        If IsCollection([property]) OrElse
           IsTuple([property]) Then Return False

        Return IsClassOrStruct(type)
    End Function

    Friend Function IsClassOrStruct(type As Type) As Boolean
        If type.IsPrimitive Then Return False

        If type.IsGenericType AndAlso
           type.GetGenericTypeDefinition = GetType(Nullable(Of)) Then Return False

        If type Is GetType(Decimal) OrElse
           type Is GetType(String) OrElse
           type Is GetType(Date) OrElse
           type Is GetType(DateTimeOffset) OrElse
           type Is GetType(TimeSpan) OrElse
           type Is GetType([Enum]) Then Return False

        Return type.IsClass OrElse type.IsValueType
    End Function

    Friend Function ParseTupleType(value As Object, propType As Type, fieldNumber As Integer) As Object
        Dim stringValue = value.ToString
        Select Case propType.GenericTypeArguments(fieldNumber - 1).Name
            Case "UInt64" : Return ULong.Parse(stringValue)
            Case "Int64" : Return Long.Parse(stringValue)
            Case "UInt32" : Return UInteger.Parse(stringValue)
            Case "Int32" : Return Integer.Parse(stringValue)
            Case "UInt16" : Return UShort.Parse(stringValue)
            Case "Int16", "UInt16" : Return Short.Parse(stringValue)
            Case "Boolean" : Return Boolean.Parse(stringValue)
            Case "String" : Return stringValue
            Case "Byte" : Return Byte.Parse(stringValue)
            Case "SByte" : Return SByte.Parse(stringValue)
            Case "Decimal" : Return Decimal.Parse(stringValue)
            Case "Double" : Return Double.Parse(stringValue)
            Case "Single" : Return Single.Parse(stringValue)
            Case "DateTime" : Return Date.Parse(stringValue)
            Case "DateTimeOffset" : Return DateTimeOffset.Parse(stringValue)
            Case "TimeSpan" : Return TimeSpan.Parse(stringValue)
        End Select
        Return Nothing
    End Function

    Friend Function ParseSQLDecimal(obj As Object) As String
        If obj Is Nothing Then Return Nothing

        If TypeOf obj Is Decimal OrElse
           TypeOf obj Is Double OrElse
           TypeOf obj Is Single Then Return obj.ToString.Replace(","c, "."c)

        Return obj.ToString
    End Function

    Friend Async Function GetCollectionAsync(id As ULong, name As String, con As SqlConnection) As Task(Of ICollection(Of KeyValuePair(Of ULong, String)))
        Dim dict As New Dictionary(Of ULong, String)
        Using command As New SqlCommand($"SELECT* FROM _enumerablesOfT WHERE Id = {id} And PropName = '{name}'", con)
            Using r = Await command.ExecuteReaderAsync.ConfigureAwait(False)
                While Await r.ReadAsync.ConfigureAwait(False)
                    dict.TryAdd(ULong.Parse($"{r.Item(3)}"), ParseSQLDecimal(r.Item(4)))
                End While
            End Using
        End Using
        Return dict
    End Function

    Friend Async Function GetTuple(Of T As {IStoreableObject})(obj As T, prop As PropertyInfo, con As SqlConnection) As Task(Of Object)
        Dim type = prop.PropertyType
        Using cmd As New SqlCommand($"SELECT* FROM _tuplesOfT WHERE Id = {obj.Id} AND PropName = '{prop.Name}'", con)
            Using r = Await cmd.ExecuteReaderAsync
                While Await r.ReadAsync
                    Dim tupleValues As New List(Of Object)
                    For i = 1 To 7
                        If Not IsDBNull(r.Item($"Item{i}")) Then tupleValues.Add(ParseTupleType(r.Item($"Item{i}"), type, i))
                    Next
                    Return type.GetConstructor(type.GetGenericArguments)?.Invoke(tupleValues.ToArray)
                End While
            End Using
        End Using
        Return Nothing
    End Function

    Friend Function UnsignedFix(Of T As {IStoreableObject})(obj As T, name As String, value As Object) As Object
        If IsDBNull(value) Then Return Nothing

        Dim [property] = obj.GetType.GetProperty(name)
        Dim propertyType = GetNullableTypeName([property].PropertyType)

        If TypeOf value Is Decimal Then
            Select Case propertyType
                Case "UInt64" : Return Decimal.ToUInt64(DirectCast(value, Decimal))
                Case "UInt32" : Return Decimal.ToUInt32(DirectCast(value, Decimal))
                Case "UInt16" : Return Decimal.ToUInt16(DirectCast(value, Decimal))
                Case "Byte" : Return Decimal.ToByte(DirectCast(value, Decimal))
                Case Else : Return value
            End Select
        End If

        Return value
    End Function

    Friend Function GetNullableTypeName(propType As Type) As String
        If propType.Name.Contains("[]") Then Return propType.Name.Replace("[]", "")
        Return If(propType.IsGenericType AndAlso propType.GetGenericTypeDefinition = GetType(Nullable(Of)),
              propType.GetGenericArguments.First.Name,
              propType.Name)
    End Function

    Friend Function GetGenericEnumerable(obj As Object) As IEnumerable(Of Type)
        Return From o In obj.GetType.GetInterfaces
               Where o.IsGenericType AndAlso
                 o.GetGenericTypeDefinition = GetType(IEnumerable(Of))
               Select o.GetGenericArguments.First
    End Function

End Module
