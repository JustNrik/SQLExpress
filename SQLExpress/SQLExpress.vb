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

    Public Async Function InstallDatabase() As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            If Await SendScalarAsync(Of Integer)($"IF OBJECT_ID('_enumerablesOfT') IS NULL SELECT 0" & vbCrLf &
                                                  "ELSE SELECT 1;", con) = 0 Then

                Await SendQueryAsync("CREATE TABLE _enumerablesOfT (" & vbCrLf &
                                     "    Id BIGINT NOT NULL," & vbCrLf &
                                     "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    RawKey INT NOT NULL," & vbCrLf &
                                     "    RawValue VARCHAR(50));", con)
            End If
        End Using
    End Function
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
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending)

            If properties.Count = 0 Then Throw New EmptyObjectException
            If properties.Any(Function(x) x.GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing AndAlso
                                  x.GetValue(obj) Is Nothing) Then Throw New NullPropertyException

            Await SendQueryAsync(Await BuildInsertAsync(obj, properties, con), con)

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

            Dim properties = (From prop In toLoad.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            Dim propertyNames = (From prop In properties
                                 Where GetType(ICollection).IsAssignableFrom(prop.PropertyType)
                                 Select prop.Name).ToImmutableArray

            If propertyNames.Count = 0 Then Throw New EmptyObjectException

            Dim collectionNames = (From prop In toLoad.GetType.GetProperties
                                   Where GetType(ICollection).IsAssignableFrom(prop.PropertyType)
                                   Select prop.Name).ToImmutableArray

            Dim flag = Await SendScalarAsync(Of Integer)("SELECT COUNT(Id) FROM _enumerablesOfT", con) > 0
            If collectionNames.Count > 0 AndAlso flag Then
                Dim objs As New List(Of ICollection(Of KeyValuePair(Of Integer, String)))
                For Each name In collectionNames
                    objs.Add(Await GetCollection(Of Integer, String)(toLoad.Id, name, con))
                Next

                If objs.Count > 0 Then
                    Dim index = 0
                    For Each name In collectionNames
                        toLoad.GetType.GetProperty(name).SetValue(toLoad, ParseObject(objs(index), toLoad, name))
                        index += 1
                    Next
                End If
            End If

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
                            Yield If(IsDBNull(r.Item(0)), Nothing, (If(TypeOf r.Item(0) Is TResult, DirectCast(r.Item(0), TResult), CType(r.Item(0), TResult))))
                        End While
                    End Using
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Using r = cmd.ExecuteReader
                    While r.Read
                        Yield If(IsDBNull(r.Item(0)), Nothing, (If(TypeOf r.Item(0) Is TResult, DirectCast(r.Item(0), TResult), CType(r.Item(0), TResult))))
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

    Private Function ParseObject([Enum] As ICollection(Of KeyValuePair(Of Integer, String)), obj As SQLObject, name As String) As Object
        Dim propType = obj.GetType.GetProperty(name).PropertyType
        Dim type = GetNullableTypeName(propType.GetGenericArguments(0))
        Dim propName = propType.Name
        Dim typeName = If(propName.Contains("`"c), propName.Substring(0, propName.IndexOf("`"c)), propName)
        typeName = If(propName.Contains("[]"), "Array", typeName)
        Select Case True
            Case typeName.Contains("List")
                Select Case type
                    Case "String" : Return [Enum].ToList
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToList
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToList
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToList
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToList
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToList
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToList
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToList
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToList
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToList
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToList
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToList
                    Case "DateTime" : Return [Enum].Select(Function(x) DateTime.Parse(x.Value)).ToList
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToList
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToList
                End Select
            Case typeName.Contains("Collection")
                Select Case type
                    Case "String" : Return [Enum]
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value))
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value))
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value))
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value))
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value))
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value))
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value))
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value))
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value))
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value))
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value))
                    Case "DateTime" : Return [Enum].Select(Function(x) DateTime.Parse(x.Value))
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value))
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value))
                End Select
            Case typeName.Contains("Enumerable")
                Select Case type
                    Case "String" : Return [Enum].AsEnumerable
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).AsEnumerable
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).AsEnumerable
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).AsEnumerable
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).AsEnumerable
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).AsEnumerable
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).AsEnumerable
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).AsEnumerable
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).AsEnumerable
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).AsEnumerable
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).AsEnumerable
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).AsEnumerable
                    Case "DateTime" : Return [Enum].Select(Function(x) DateTime.Parse(x.Value)).AsEnumerable
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).AsEnumerable
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).AsEnumerable
                End Select
            Case typeName = "Array"
                Select Case type
                    Case "String" : Return [Enum].ToArray
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToArray
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToArray
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToArray
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToArray
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToArray
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToArray
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToArray
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToArray
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToArray
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToArray
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToArray
                    Case "DateTime" : Return [Enum].Select(Function(x) DateTime.Parse(x.Value)).ToArray
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToArray
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToArray
                End Select
            Case typeName.Contains("Dictionary")
                Select Case type
                    Case "String" : Return [Enum].ToDictionary
                    Case "UInt64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, ULong)(x.Key, ULong.Parse(x.Value))).ToDictionary
                    Case "Int64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Long)(x.Key, Long.Parse(x.Value))).ToDictionary
                    Case "UInt32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UInteger)(x.Key, UInteger.Parse(x.Value))).ToDictionary
                    Case "Int32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Integer)(x.Key, Integer.Parse(x.Value))).ToDictionary
                    Case "UInt16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UShort)(x.Key, UShort.Parse(x.Value))).ToDictionary
                    Case "Int16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Short)(x.Key, Short.Parse(x.Value))).ToDictionary
                    Case "Boolean" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Boolean)(x.Key, Boolean.Parse(x.Value))).ToDictionary
                    Case "Byte" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Byte)(x.Key, Byte.Parse(x.Value))).ToDictionary
                    Case "Decimal" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Decimal)(x.Key, Decimal.Parse(x.Value))).ToDictionary
                    Case "Double" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Double)(x.Key, Double.Parse(x.Value))).ToDictionary
                    Case "Single" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Single)(x.Key, Single.Parse(x.Value))).ToDictionary
                    Case "DateTime" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, DateTime)(x.Key, DateTime.Parse(x.Value))).ToDictionary
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToDictionary
                    Case "TimeSpan" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToDictionary
                End Select
        End Select
        Throw New UnsupportedTypeException
    End Function

    Private Async Function GetCollection(Of TKey, TValue)(id As ULong, name As String, con As SqlConnection) As Task(Of ICollection(Of KeyValuePair(Of TKey, TValue)))
        Dim dict As New Dictionary(Of TKey, TValue)
        Using command As New SqlCommand($"SELECT* FROM _enumerablesOfT WHERE Id = {id} AND PropName = '{name}'", con)
            Using r = Await command.ExecuteReaderAsync
                While Await r.ReadAsync
                    dict.Add(DirectCast(r.Item(3), TKey), DirectCast(r.Item(4), TValue))
                End While
            End Using
        End Using
        Return dict
    End Function

    Private Async Function BuildInsertAsync(Of T As {SQLObject})(obj As T, properties As IOrderedEnumerable(Of PropertyInfo), con As SqlConnection) As Task(Of String)
        Dim collections = properties.Where(Function(x) GetType(ICollection).IsAssignableFrom(x.PropertyType)).ToImmutableArray
        If collections.Count > 0 Then
            For Each collection In collections
                Await InsertCollectionAsync(obj, collection, con)
            Next
        End If

        Dim props = If(collections.Count > 0, properties.Except(collections), properties).ToImmutableArray

        Return $"INSERT INTO {obj.Name} ({props.Select(Function(x) x.Name).Aggregate(Function(x, y) x & ", " & y)})" & vbCrLf &
               $"VALUES ({props.Select(Function(x) $"{GetSqlValue(x, obj)}").Aggregate(Function(x, y) x & ", " & y)});"
    End Function

    Private Function InsertCollectionAsync(Of T As {SQLObject})(obj As T, [Property] As PropertyInfo, con As SqlConnection) As Task
        Dim generic = [Property].GetValue(obj)
        Dim enumerable = GetGenericEnumerable(generic).ToImmutableArray
        Dim index = 0
        Dim values = DirectCast([Property].GetValue(obj), ICollection)
        For Each element In values
            SendQuery($"INSERT INTO _enumerablesOfT (Id, ObjName, PropName, RawKey, RawValue)" & vbCrLf &
                      $"VALUES ({obj.Id}, '{obj.Name}', '{[Property].Name}', {index}, '{element}');", con)
            index += 1
        Next
        Return Task.CompletedTask
    End Function

    Private Function GetGenericEnumerable(obj As Object) As IEnumerable(Of Type)
        Return From o In obj.GetType.GetInterfaces
               Where o.IsGenericType AndAlso
                     o.GetGenericTypeDefinition = GetType(IEnumerable(Of))
               Select o.GetGenericArguments(0)
    End Function

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
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing AndAlso
                                  Not GetType(ICollection).IsAssignableFrom(prop.PropertyType)
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            If properties.Count = 0 Then Throw New EmptyObjectException
            For Each [Property] In properties
                .Append($"{[Property].Name} {ParseType(GetNullableTypeName([Property].PropertyType), [Property].GetCustomAttribute(Of StringLengthAttribute)(True)?.Length)} ")
                .Append($"{If([Property].GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing, "NOT NULL", "")} ")
                .Append($"{If([Property].GetCustomAttribute(Of PrimaryKeyAttribute)(True) IsNot Nothing, "PRIMARY KEY", "")}")

                If [Property] Is properties.Last Then .AppendLine(");") Else .AppendLine(",")
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

    Private Function GetNullableTypeName(propType As Type) As String
        Return If(propType.IsGenericType AndAlso propType.GetGenericTypeDefinition = GetType(Nullable(Of)),
                  propType.GetGenericArguments(0).Name,
                  propType.Name)
    End Function

    Private Function GetSqlValue(Of T As {SQLObject})([Property] As PropertyInfo, ByRef obj As T) As String
        Dim value = [Property].GetValue(obj)
        Dim typeName = ParseType(GetNullableTypeName([Property].PropertyType))

        Select Case typeName
            Case "BIGINT", "SMALLINT", "INT", "TINYINT", "DECIMAL", "FLOAT", "REAL" : Return $"{If(value, "NULL")}"
            Case "DATE", "TIME", "DATETIMEOFFSET", $"VARCHAR({If([Property].GetCustomAttribute(Of StringLengthAttribute)(True)?.Length, _stringLimit)})" : Return $"'{If(value, "NULL")}'"
            Case "BIT" : Return $"{If(DirectCast(value, Boolean), 1, 0)}"
            Case Else : Throw New UnsupportedTypeException
        End Select
        Return Nothing
    End Function

    Private Function UnsignedFix(Of T As {SQLObject})(obj As T, name As String, value As Object) As Object
        If IsDBNull(value) Then Return Nothing

        Dim [property] = obj.GetType.GetProperty(name)
        Dim propertyType = GetNullableTypeName([property].PropertyType)

        Select Case True
            Case TypeOf value Is Long : Return If(propertyType = "UInt64", CULng(value), value)
            Case TypeOf value Is Integer : Return If(propertyType = "UInt32", CUInt(value), value)
            Case TypeOf value Is Short : Return If(propertyType = "UInt16", CUShort(value), value)
            Case TypeOf value Is SByte : Return If(propertyType = "Byte", CByte(value), value)
        End Select
        Return value
    End Function
#End Region
End Class