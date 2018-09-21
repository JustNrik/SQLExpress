Option Compare Text
#Region "Imports"
Imports Newtonsoft.Json.Linq
Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Data.SqlClient
Imports System.Reflection
Imports System.Text
Imports System.Xml
Imports System.Runtime.InteropServices
#End Region
Public NotInheritable Class SQLExpressClient
#Region "Fields"
    Private _usingService As Boolean
    Private _connectionString As String
    Private _database As String
    Private _sqlServer As String
    Private _username As String
    Private _password As String
    Private _stringLimit As Integer
#End Region
#Region "Properties"
    Public Property Cache As New ConcurrentDictionary(Of ULong, SQLObject)
    ''' <summary>
    ''' Sets the database where the data will be gathered.
    ''' </summary>
    Public WriteOnly Property Database As String
        Set
            _database = Value
            _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
        End Set
    End Property
    ''' <summary>
    ''' Sets the instance where it will be connected.
    ''' </summary>
    Public WriteOnly Property Server As String
        Set
            _sqlServer = Value
            _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
        End Set
    End Property
    ''' <summary>
    ''' Sets the username required to log in the database
    ''' </summary>
    Public WriteOnly Property Username As String
        Set
            _username = Value
            _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
        End Set
    End Property
    ''' <summary>
    ''' Sets the password required to log in the database
    ''' </summary>
    Public WriteOnly Property Password As String
        Set
            _password = Value
            _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
        End Set
    End Property
    ''' <summary>
    ''' Sets the ConnectionString that will be used to connect the database.
    ''' </summary>
    Public WriteOnly Property ConnectionString As String
        Set
            _connectionString = Value
        End Set
    End Property
#End Region
#Region "Constructors"
    ''' <summary>
    ''' Creates a new instance of this class
    ''' </summary>
    Sub New(Optional StringLimit As Integer = 20)
        _stringLimit = StringLimit
    End Sub
    ''' <summary>
    ''' Provide the full connection string.
    ''' </summary>
    ''' <param name="ConnectionString"></param>
    Sub New(ConnectionString As String, Optional StringLimit As Integer = 20)
        _connectionString = ConnectionString
        _stringLimit = StringLimit
    End Sub
    ''' <summary>
    ''' Reads the config from a XML Document. You must load the XMlDocument before using this method.
    ''' </summary>
    ''' <param name="xmlConfig"></param>
    Sub New(xmlConfig As XmlDocument, Optional StringLimit As Integer = 20)
        ReadConfig(xmlConfig)
        _stringLimit = StringLimit
    End Sub
    ''' <summary>
    ''' Reads the config from a JSON file. You must parse the JObject before using this method.
    ''' </summary>
    ''' <param name="jConfig"></param>
    Sub New(jConfig As JObject, Optional StringLimit As Integer = 20)
        ReadConfig(jConfig)
        _stringLimit = StringLimit
    End Sub
#End Region
#Region "Config"
    ''' <summary>
    ''' Reads the SQL Configuration from an XML Document. Remember to load the document before executing this method.
    ''' </summary>
    ''' <param name="xmlConfig"></param>
    Public Sub ReadConfig(xmlConfig As XmlDocument)
        _database = xmlConfig.SelectSingleNode("*/database").InnerXml
        _sqlServer = xmlConfig.SelectSingleNode("*/server").InnerXml
        _username = xmlConfig.SelectSingleNode("*/username").InnerXml
        _password = xmlConfig.SelectSingleNode("*/password").InnerXml
        _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
    End Sub
    ''' <summary>
    ''' Reads the SQL Configuration from an JSON file. Remember to parse the document before executing this method.
    ''' </summary>
    ''' <param name="jConfig"></param>
    Public Sub ReadConfig(jConfig As JObject)
        _database = $"{jConfig("database")}"
        _sqlServer = $"{jConfig("server")}"
        _username = $"{jConfig("username")}"
        _password = $"{jConfig("password")}"
        _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
    End Sub
#End Region
#Region "InitialiseObjects"
    ''' <summary>
    ''' This is the Object initialisation, it will automatically create the objects that don't exists.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function InitialiseObjectsAsync(Of T As {SQLObject})(ParamArray objs As T()) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync()
            For Each obj In objs
                If Await CheckObjectExistence(obj, con) = 0 Then Await SendQueryAsync(BuildTable(obj), con)
            Next
        End Using
    End Function
    ''' <summary>
    ''' This is the Object initialisation, it will automatically create the objects that don't exists.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    Public Sub InitialiseObjects(Of T As {SQLObject})(ParamArray objs As T())
        InitialiseObjectsAsync(objs).Wait()
    End Sub
#End Region
#Region "LoadObjectCache"
    ''' <summary>
    ''' Loads the cache with all objects stored in the database of the type provided.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectCacheAsync(Of T As {New, SQLObject})(ParamArray objs As T()) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            For Each obj In objs
                Select Case Await SendScalarAsync(Of Integer)($"SELECT COUNT(Id) From {obj.Name};", con)
                    Case 0 : Continue For
                    Case Else
                        Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.Name};", con).ToImmutableArray
                        For Each id In ids
                            Dim newObj = Await LoadObjectAsync(New T With {.Id = id})
                            If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj)
                        Next
                End Select
            Next
        End Using
    End Function
    ''' <summary>
    ''' Loads the cache with all objects stored in the database of the type provided.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    Public Sub LoadObjectCache(Of T As {New, SQLObject})(ParamArray objs As T())
        LoadObjectCacheAsync(objs).Wait()
    End Sub
#End Region
#Region "CreateNewObject"
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Async Function CreateNewObjectAsync(Of T As {SQLObject})(obj As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync()

            Dim properties = (From prop In obj.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            If properties.Count = 0 Then Throw New EmptyObjectException

            Await SendQueryAsync($"INSERT INTO {obj.Name} ({properties.Select(Function(x) x.Name).Aggregate(Function(x, y) x & ", " & y)})" &
                                 $"VALUES ({properties.Select(Function(x) $"{GetSqlValue(x, obj)}").Aggregate(Function(x, y) x & ", " & y)});", con)

            Dim newObj = Await LoadObjectAsync(obj)
            If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj)
            Return newObj
        End Using
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function CreateNewObjectAsync(Of T As {New, SQLObject})(id As ULong) As Task(Of T)
        Dim obj As New T With {.Id = id}
        Return Await CreateNewObjectAsync(obj)
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    Public Sub CreateNewObject(Of T As {New, SQLObject})(<Out> ByRef obj As T)
        obj = CreateNewObjectAsync(obj).Result
    End Sub
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function CreateNewObject(Of T As {New, SQLObject})(id As ULong) As T
        Dim obj As New T With {.Id = id}
        Return CreateNewObjectAsync(obj).Result
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="obj"></param>
    Public Sub CreateNewObject(Of T As {New, SQLObject})(id As ULong, <Out> ByRef obj As T)
        Dim newObj As New T With {.Id = id}
        obj = CreateNewObjectAsync(newObj).Result
    End Sub
#End Region
#Region "LoadObject"
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toLoad"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectAsync(Of T As {SQLObject})(toLoad As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            If Not Await CheckExistenceAsync(toLoad, con) Then Return Await CreateNewObjectAsync(toLoad)

            Dim propertyNames = (From prop In toLoad.GetType.GetProperties
                                 Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                                 Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending
                                 Select prop.Name).ToImmutableArray

            If propertyNames.Count = 0 Then Throw New EmptyObjectException
            Using cmd As New SqlCommand($"SELECT* FROM {toLoad.Name} WHERE Id = {toLoad.Id};", con)
                Using r = Await cmd.ExecuteReaderAsync
                    While Await r.ReadAsync
                        For x = 0 To r.FieldCount - 1
                            toLoad.GetType.GetProperty(propertyNames(x)).SetValue(toLoad, UnsignedFix(toLoad, propertyNames(x), r.Item(propertyNames(x))))
                        Next
                    End While
                    Return toLoad
                End Using
            End Using
        End Using
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectAsync(Of T As {New, SQLObject})(id As ULong) As Task(Of T)
        Dim toLoad As New T With {.Id = id}
        Return Await LoadObjectAsync(toLoad)
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {SQLObject})(obj As T) As T
        Return LoadObjectAsync(obj).Result
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {New, SQLObject})(id As ULong) As T
        Dim newObj As New T With {.Id = id}
        Return LoadObject(newObj)
    End Function

    Public Sub LoadObject(Of T As {New, SQLObject})(id As ULong, <Out> ByRef obj As T)
        Dim newObj As New T With {.Id = id}
        obj = LoadObject(newObj)
    End Sub
#End Region
#Region "UpdateObject"
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toUpdate"></param>
    ''' <returns></returns>
    Public Async Function UpdateObjectAsync(Of T As {SQLObject})(toUpdate As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            If Not Await CheckExistenceAsync(toUpdate, con) Then Return Await CreateNewObjectAsync(toUpdate)

            Dim properties = (From prop In toUpdate.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing AndAlso prop.GetCustomAttribute(Of PrimaryKeyAttribute)(True) Is Nothing
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            If properties.Count = 0 Then Throw New EmptyObjectException
            Await SendQueryAsync(BuildUpdate(toUpdate, properties), con)

            Dim newObj = Await LoadObjectAsync(toUpdate)
            If Cache.ContainsKey(newObj.Id) Then Cache(newObj.Id) = newObj Else Cache.TryAdd(newObj.Id, newObj)
            Return newObj
        End Using
    End Function
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function UpdateObjectAsync(Of T As {New, SQLObject})(id As ULong) As Task(Of T)
        Dim toUpdate As New T With {.Id = id}
        Return Await UpdateObjectAsync(toUpdate)
    End Function
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    Public Sub UpdateObject(Of T As {SQLObject})(<Out> ByRef obj As T)
        obj = UpdateObjectAsync(obj).Result
    End Sub
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    Public Sub UpdateObject(Of T As {New, SQLObject})(id As ULong, <Out> ByRef obj As T)
        Dim newObj As New T With {.Id = id}
        obj = UpdateObjectAsync(newObj).Result
    End Sub
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function UpdateObject(Of T As {New, SQLObject})(id As ULong) As T
        Dim newObj As New T With {.Id = id}
        Return UpdateObjectAsync(newObj).Result
    End Function
#End Region
#Region "RemoveObject"
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toRemove"></param>
    ''' <returns></returns>
    Public Async Function RemoveObjectAsync(Of T As {SQLObject})(toRemove As T) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            If Not Await CheckExistenceAsync(toRemove, con) Then Return
            Await SendQueryAsync($"DELETE FROM {toRemove.Name} WHERE Id = {toRemove.Id};", con)
            If Cache.ContainsKey(toRemove.Id) Then Cache.TryRemove(toRemove.Id, Nothing)
        End Using
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function RemoveObjectAsync(Of T As {New, SQLObject})(id As ULong) As Task
        Dim toRemove As New T With {.Id = id}
        Await RemoveObjectAsync(toRemove)
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toRemove"></param>
    Public Sub RemoveObject(Of T As {SQLObject})(toRemove As T)
        Using con As New SqlConnection(_connectionString) : con.Open()
            If Not CheckExistence(toRemove, con) Then Return
            SendQuery($"DELETE FROM {toRemove.Name} WHERE Id = {toRemove.Id}", con)
            If Cache.ContainsKey(toRemove.Id) Then Cache.TryRemove(toRemove.Id, Nothing)
        End Using
    End Sub
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    Public Sub RemoveObject(Of T As {New, SQLObject})(id As ULong)
        Dim toRemove As New T With {.Id = id}
        RemoveObject(toRemove)
    End Sub
#End Region
#Region "SendQuery"
    ''' <summary>
    ''' Sends a query to the database.
    ''' </summary>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function SendQueryAsync(query As String, Optional con As SqlConnection = Nothing) As Task
        If con Is Nothing Then
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync
                Using cmd As New SqlCommand(query, conn)
                    Await cmd.ExecuteNonQueryAsync
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Await cmd.ExecuteNonQueryAsync
            End Using
        End If
    End Function
    ''' <summary>
    ''' Sends a query to the database.
    ''' </summary>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    Public Sub SendQuery(query As String, Optional con As SqlConnection = Nothing)
        SendQueryAsync(query, con).Wait()
        Dim x = SendScalarAsync(Of ULong)("")
    End Sub
#End Region
#Region "SendScalar"
    ''' <summary>
    ''' Sends a query to the database and returns the first value retrieved of the type provided. Returns null if cast fails.
    ''' </summary>
    ''' <typeparam name="TResult"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function SendScalarAsync(Of TResult)(query As String, Optional con As SqlConnection = Nothing) As Task(Of TResult)
        If con Is Nothing Then
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync
                Using cmd As New SqlCommand(query, conn)
                    Dim result = Await cmd.ExecuteScalarAsync
                    Return If(TypeOf result Is TResult, DirectCast(result, TResult), Nothing)
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Dim result = Await cmd.ExecuteScalarAsync
                Return If(TypeOf result Is TResult, DirectCast(result, TResult), Nothing)
            End Using
        End If
    End Function
    ''' <summary>
    ''' Sends a query to the database and returns the first value retrieved of the type provided. Returns null if cast fails.
    ''' </summary>
    ''' <typeparam name="TResult"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Function SendScalar(Of TResult)(query As String, Optional con As SqlConnection = Nothing) As TResult
        Return SendScalarAsync(Of TResult)(query, con).Result
    End Function
#End Region
#Region "YieldData"
    ''' <summary>
    ''' Yields the data of the resulting column into an IEnumerable, if there are other rows they will be ignored.
    ''' </summary>
    ''' <typeparam name="TResult"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Iterator Function YieldData(Of TResult)(query As String, Optional con As SqlConnection = Nothing) As IEnumerable(Of TResult)
        If con Is Nothing Then
            Using conn As New SqlConnection(_connectionString) : conn.Open()
                Using cmd As New SqlCommand(query, conn)
                    Using r = cmd.ExecuteReader
                        While r.Read
                            Yield If(TypeOf r.Item(0) Is TResult, DirectCast(r.Item(0), TResult), CType(r.Item(0), TResult))
                        End While
                    End Using
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Using r = cmd.ExecuteReader
                    While r.Read
                        Yield If(TypeOf r.Item(0) Is TResult, DirectCast(r.Item(0), TResult), CType(r.Item(0), TResult))
                    End While
                End Using
            End Using
        End If
    End Function
#End Region
#Region "CheckExistence"
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function CheckExistenceAsync(Of T As {SQLObject})(obj As T, Optional con As SqlConnection = Nothing) As Task(Of Boolean)
        If con Is Nothing Then
            Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {obj.Name} WHERE Id = {obj.Id};", con)
                Return DirectCast(Await cmd.ExecuteScalarAsync, Integer) = 1
            End Using
        Else
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync
                Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {obj.Name} WHERE Id = {obj.Id};", con)
                    Return DirectCast(Await cmd.ExecuteScalarAsync, Integer) = 1
                End Using
            End Using
        End If
    End Function
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Function CheckExistence(Of T As {SQLObject})(obj As T, Optional con As SqlConnection = Nothing) As Boolean
        Return CheckExistenceAsync(obj, con).Result
    End Function
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function CheckExistenceAsync(Of T As {New, SQLObject})(id As ULong, Optional con As SqlConnection = Nothing) As Task(Of Boolean)
        Dim obj As New T With {.Id = id}
        Return Await CheckExistenceAsync(obj, con)
    End Function
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function CheckExistence(Of T As {New, SQLObject})(id As ULong, Optional con As SqlConnection = Nothing) As Boolean
        Dim obj As New T With {.Id = id}
        Return CheckExistenceAsync(obj, con).Result
    End Function
#End Region
#Region "Private Methods"
    Private Function BuildUpdate(Of T As {SQLObject})(obj As T, properties As ImmutableArray(Of PropertyInfo)) As String
        Dim sb As New StringBuilder
        With sb
            .AppendLine($"UPDATE {obj.Name}")
            .Append("SET ")
            For Each [Property] In properties
                .AppendLine($"{[Property].Name} = {GetSqlValue([Property], obj)}{If([Property] IsNot properties.Last, ",", "")}")
            Next
            .AppendLine($"WHERE Id = {obj.Id};")
        End With
        Return sb.ToString
    End Function
    Private Function BuildTable(Of T As {SQLObject})(obj As T) As String
        Dim sb As New StringBuilder
        With sb
            .AppendLine($"CREATE TABLE {obj.Name} (")

            Dim properties = (From prop In obj.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            If properties.Count = 0 Then Throw New EmptyObjectException
            For Each [Property] In properties
                .Append($"{[Property].Name} {ParseType([Property].PropertyType.Name, [Property].GetCustomAttribute(Of StringLengthAttribute)(True)?.Length)} ")
                .Append($"{If([Property].GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing, "NOT NULL", "")} ")
                .Append($"{If([Property].GetCustomAttribute(Of PrimaryKeyAttribute)(True) IsNot Nothing, "PRIMARY KEY", "")}")

                If [Property] Is properties.Last() Then .AppendLine(");") Else .AppendLine(",")
            Next
        End With
        Return sb.ToString
    End Function
    Private Async Function CheckObjectExistence(Of T As {SQLObject})(obj As T, con As SqlConnection) As Task(Of Integer)
        Return Await SendScalarAsync(Of Integer)($"IF OBJECT_ID('{obj.Name}') IS NULL SELECT 0" & vbCrLf &
                                                  "ELSE SELECT 1;", con)
    End Function
    Private Function ParseType(typeName As String, Optional stringLimit As Byte? = Nothing) As String
        With typeName
            Select Case True
                Case .Contains("Int64") : Return "BIGINT"
                Case .Contains("Int32") : Return "INT"
                Case .Contains("Int16") : Return "SMALLINT"
                Case .Contains("Boolean") : Return "BIT"
                Case .Contains("String") : Return $"VARCHAR({If(stringLimit, _stringLimit)})"
                Case .Contains("Byte") : Return "TINYINT"
                Case .Contains("Decimal") : Return "DECIMAL"
                Case .Contains("Double") : Return "FLOAT"
                Case .Contains("Single") : Return "REAL"
                Case .Contains("DateTime") : Return "DATE"
                Case .Contains("TimeSpan") : Return "TIME"
                Case .Contains("DateTimeOffset") : Return "DATETIMEOFFSET"
            End Select
        End With
        Throw New UnsupportedTypeException
        Return Nothing
    End Function

    Private Function GetSqlValue(Of T As {SQLObject})([Property] As PropertyInfo, ByRef obj As T) As String
        Dim value = [Property].GetValue(obj)
        Dim typeName = ParseType([Property].PropertyType.Name)
        Select Case typeName
            Case "BIGINT", "SMALLINT", "INT", "TINYINT", "DECIMAL", "FLOAT", "REAL" : Return $"{value}"
            Case "DATE", "TIME", "DATETIMEOFFSET", $"VARCHAR({If([Property].GetCustomAttribute(Of StringLengthAttribute)(True)?.Length, _stringLimit)})" : Return $"'{value}'"
            Case "BIT" : Return $"{If(DirectCast(value, Boolean) = True, 1, 0)}"
            Case Else : Throw New UnsupportedTypeException
        End Select
        Return Nothing
    End Function

    Private Function UnsignedFix(Of T As {SQLObject})(obj As T, name As String, value As Object) As Object
        Dim propertyType = obj.GetType.GetProperty(name).PropertyType.Name
        Select Case True
            Case TypeOf value Is Int64 : Return If(propertyType = "UInt64", Convert.ToUInt64(value), value)
            Case TypeOf value Is Int32 : Return If(propertyType = "UInt32", Convert.ToUInt32(value), value)
            Case TypeOf value Is Int16 : Return If(propertyType = "UInt16", Convert.ToUInt16(value), value)
            Case TypeOf value Is SByte : Return If(propertyType = "Byte", Convert.ToByte(value), value)
        End Select
        Return value
    End Function
#End Region
End Class