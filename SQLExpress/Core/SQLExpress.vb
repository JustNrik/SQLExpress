Option Compare Text
#Region "Imports"
Imports Newtonsoft.Json.Linq
Imports System.Collections.Concurrent
Imports System.Collections.Immutable
Imports System.Convert
Imports System.Data.SqlClient
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Xml
Imports SQLExpress.Extensions
Imports System.Collections.ObjectModel
#End Region
Public NotInheritable Class SQLExpressClient
#Region "Fields"
    Private _connectionString As String
    Private _database As String
    Private _sqlServer As String
    Private _username As String
    Private _password As String
    Private _stringLimit As Integer
#End Region
#Region "Properties"
    Public Property Cache As New ConcurrentDictionary(Of ULong, IStoreableObject)
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
    Public Async Function InitialiseObjectsAsync(Of T As {IStoreableObject})(ParamArray objs As T()) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            For Each obj In objs
                If Not Await CheckObjectExistenceAsync(obj, con).Unawait Then Await SendQueryAsync(BuildTable(obj), con).Unawait
            Next
        End Using
    End Function
    ''' <summary>
    ''' This is the Object initialisation, it will automatically create the objects that don't exists.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    Public Sub InitialiseObjects(Of T As {IStoreableObject})(ParamArray objs As T())
        Using con As New SqlConnection(_connectionString) : con.Open()
            For Each obj In objs
                If Not CheckObjectExistence(obj, con) Then SendQuery(BuildTable(obj), con)
            Next
        End Using
    End Sub

    Public Async Function InstallDatabaseAsync() As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            If Await SendScalarAsync(Of Integer)($"IF OBJECT_ID('_enumerablesOfT') IS NULL SELECT 0" & vbCrLf &
                                                  "ELSE SELECT 1;", con).Unawait = 0 Then

                Await SendQueryAsync("CREATE TABLE _enumerablesOfT (" & vbCrLf &
                                     "    Id BIGINT NOT NULL," & vbCrLf &
                                     "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    RawKey INT NOT NULL," & vbCrLf &
                                     "    RawValue VARCHAR(50));", con).Unawait
            End If
        End Using
    End Function

    Public Sub InstallDatabase()
        Using con As New SqlConnection(_connectionString) : con.Open()
            If SendScalar(Of Integer)($"If OBJECT_ID('_enumerablesOfT') IS NULL SELECT 0" & vbCrLf &
                                       "ELSE SELECT 1;", con) = 0 Then

                SendQuery("CREATE TABLE _enumerablesOfT (" & vbCrLf &
                          "    Id BIGINT NOT NULL," & vbCrLf &
                          "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    RawKey INT NOT NULL," & vbCrLf &
                          "    RawValue VARCHAR(50));", con)
            End If
        End Using
    End Sub
#End Region
#Region "LoadObjectCache"
    ''' <summary>
    ''' Loads the cache with all objects stored in the database of the type provided.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectCacheAsync(Of T As {New, IStoreableObject})(ParamArray objs As T()) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            For Each obj In objs
                Select Case Await SendScalarAsync(Of Integer)($"SELECT COUNT(Id) From {obj.TableName};", con).Unawait
                    Case 0 : Continue For
                    Case Else
                        Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.TableName};", con).ToImmutableArray
                        For Each id In ids
                            Dim newObj = Await LoadObjectAsync(New T With {.Id = id}).Unawait
                            If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
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
    Public Sub LoadObjectCache(Of T As {New, IStoreableObject})(ParamArray objs As T())
        Using con As New SqlConnection(_connectionString) : con.Open()
            For Each obj In objs
                Select Case SendScalar(Of Integer)($"Select COUNT(Id) From {obj.TableName};", con)
                    Case 0 : Continue For
                    Case Else
                        Dim ids = YieldData(Of ULong)($"Select Id FROM {obj.TableName};", con).ToImmutableArray
                        For Each id In ids
                            Dim newObj = LoadObject(New T With {.Id = id})
                            If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj)
                        Next
                End Select
            Next
        End Using
    End Sub
#End Region
#Region "CreateNewObject"
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Async Function CreateNewObjectAsync(Of T As {IStoreableObject})(obj As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait

            Dim properties = (From prop In obj.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing AndAlso
                                      Not IsClassOrStruct(prop.PropertyType)
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending)

            Dim objProperties = (From prop In obj.GetType.GetProperties
                                 Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing AndAlso
                                         IsClassOrStruct(prop.PropertyType)
                                 Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending)

            If properties.Count = 0 Then Throw New EmptyObjectException
            If properties.Any(Function(x) x.GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing AndAlso
                                  x.GetValue(obj) Is Nothing) Then Throw New NullPropertyException

            Await SendQueryAsync(Await BuildInsertAsync(obj, properties.Except(objProperties), con).Unawait, con).Unawait

            If objProperties.Count > 0 Then
                For Each prop In objProperties
                    Dim propObj = TryCast(prop.GetValue(obj), IStoreableObject)
                    If propObj IsNot Nothing Then If Not Await CheckObjectExistenceAsync(propObj, con).Unawait Then Await SendQueryAsync(BuildTable(propObj), con).Unawait
                Next
            End If

            Dim newObj = Await LoadObjectAsync(obj).Unawait
            If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
            Return newObj
        End Using
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function CreateNewObjectAsync(Of T As {New, IStoreableObject})(id As ULong) As Task(Of T)
        Dim obj As New T With {.Id = id}
        Return Await CreateNewObjectAsync(obj)
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    Public Sub CreateNewObject(Of T As {New, IStoreableObject})(<Out> ByRef obj As T)
        obj = CreateNewObjectAsync(obj).Result
    End Sub
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function CreateNewObject(Of T As {New, IStoreableObject})(id As ULong) As T
        Dim obj As New T With {.Id = id}
        CreateNewObject(obj)
        Return obj
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="obj"></param>
    Public Sub CreateNewObject(Of T As {New, IStoreableObject})(id As ULong, <Out> ByRef obj As T)
        obj = CreateNewObject(Of T)(id)
    End Sub
#End Region
#Region "LoadObject"
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toLoad"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectAsync(Of T As {IStoreableObject})(toLoad As T) As Task(Of T)
        If Cache.ContainsKey(toLoad.Id) Then Return DirectCast(Cache(toLoad.Id), T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            If Not Await CheckExistenceAsync(toLoad, con).Unawait Then Return Await CreateNewObjectAsync(toLoad).Unawait

            Dim properties = (From prop In toLoad.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing
                              Order By prop.GetCustomAttribute(Of StoreAttribute)(True).Priority Descending).ToImmutableArray

            Dim propertyNames = (From prop In properties
                                 Where Not GetType(ICollection).IsAssignableFrom(prop.PropertyType) AndAlso
                                 Not IsClassOrStruct(prop.PropertyType)
                                 Select prop.Name).ToImmutableArray

            If properties.Count = 0 Then Throw New EmptyObjectException

            Dim collectionNames = (From prop In toLoad.GetType.GetProperties
                                   Where GetType(ICollection).IsAssignableFrom(prop.PropertyType) AndAlso
                                   Not IsClassOrStruct(prop.PropertyType)
                                   Select prop.Name).ToImmutableArray

            Dim types = (From prop In toLoad.GetType.GetProperties
                         Where IsClassOrStruct(prop.PropertyType)).ToImmutableArray

            Dim flag = Await SendScalarAsync(Of Integer)("SELECT COUNT(Id) FROM _enumerablesOfT", con).Unawait > 0
            If collectionNames.Count > 0 AndAlso flag Then
                Dim objs As New List(Of ICollection(Of KeyValuePair(Of Integer, String)))
                For Each name In collectionNames
                    objs.Add(Await GetCollection(Of Integer, String)(toLoad.Id, name, con).Unawait)
                Next

                If objs.Count > 0 Then
                    Dim index = 0
                    For Each name In collectionNames
                        toLoad.GetType.GetProperty(name).SetValue(toLoad, ParseObject(objs(index), toLoad, name))
                        index += 1
                    Next
                End If
            End If

            If types.Count > 0 Then
                For Each obj In types
                    Dim refObj = TryCast(obj.GetValue(toLoad), IStoreableObject)
                    If refObj IsNot Nothing Then
                        refObj.Id = If(refObj.Id = 0, toLoad.Id, refObj.Id)
                        Dim loadObj = Await LoadObjectAsync(refObj).Unawait
                        toLoad.GetType.GetProperty(obj.Name).SetValue(toLoad, refObj)
                    End If
                Next
            End If

            Using cmd As New SqlCommand($"SELECT* FROM {toLoad.TableName} WHERE Id = {toLoad.Id};", con)
                Using r = Await cmd.ExecuteReaderAsync.Unawait
                    While Await r.ReadAsync.Unawait
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
    Public Async Function LoadObjectAsync(Of T As {New, IStoreableObject})(id As ULong) As Task(Of T)
        If Cache.ContainsKey(id) Then Return DirectCast(Cache(id), T)
        Dim toLoad As New T With {.Id = id}
        Return Await LoadObjectAsync(toLoad).Unawait
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {IStoreableObject})(obj As T) As T
        If Cache.ContainsKey(obj.Id) Then Return DirectCast(Cache(obj.Id), T)
        Return LoadObjectAsync(obj).Result
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {New, IStoreableObject})(id As ULong) As T
        If Cache.ContainsKey(id) Then Return DirectCast(Cache(id), T)
        Dim newObj As New T With {.Id = id}
        Return LoadObject(newObj)
    End Function

    Public Sub LoadObject(Of T As {New, IStoreableObject})(id As ULong, <Out> ByRef obj As T)
        If Cache.ContainsKey(id) Then
            obj = DirectCast(Cache(id), T)
        Else
            Dim newObj As New T With {.Id = id}
            obj = LoadObject(newObj)
        End If
    End Sub
#End Region
#Region "UpdateObject"
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toUpdate"></param>
    ''' <returns></returns>
    Public Async Function UpdateObjectAsync(Of T As {IStoreableObject})(toUpdate As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            If Not Await CheckExistenceAsync(toUpdate, con).Unawait Then
                Return Await CreateNewObjectAsync(toUpdate).Unawait
            Else
                Await RemoveObjectAsync(toUpdate).Unawait
                Dim newObj = Await CreateNewObjectAsync(toUpdate).Unawait
                If Cache.ContainsKey(toUpdate.Id) Then Cache(toUpdate.Id) = toUpdate Else Cache.TryAdd(toUpdate.Id, toUpdate)
                Return newObj
            End If
        End Using
    End Function
    ''' <summary>
    ''' Updates the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toUpdate"></param>
    Public Sub UpdateObject(Of T As {New, IStoreableObject})(ByRef toUpdate As T)
        Using con As New SqlConnection(_connectionString) : con.Open()
            If Not CheckExistence(toUpdate, con) Then
                CreateNewObject(toUpdate)
            Else
                RemoveObject(toUpdate)
                CreateNewObject(toUpdate)
                If Cache.ContainsKey(toUpdate.Id) Then Cache(toUpdate.Id) = toUpdate Else Cache.TryAdd(toUpdate.Id, toUpdate)
            End If
        End Using
    End Sub
#End Region
#Region "RemoveObject"
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toRemove"></param>
    ''' <returns></returns>
    Public Async Function RemoveObjectAsync(Of T As {IStoreableObject})(toRemove As T) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.Unawait
            If Not Await CheckExistenceAsync(toRemove, con).Unawait Then Return
            Await SendQueryAsync($"DELETE FROM {toRemove.TableName} WHERE Id = {toRemove.Id};", con).Unawait
            Await SendQueryAsync($"DELETE FROM _enumerablesOfT WHERE Id = {toRemove.Id};", con).Unawait
            If Cache.ContainsKey(toRemove.Id) Then Cache.TryRemove(toRemove.Id, Nothing)
        End Using
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function RemoveObjectAsync(Of T As {New, IStoreableObject})(id As ULong) As Task
        Dim toRemove As New T With {.Id = id}
        Await RemoveObjectAsync(toRemove).Unawait
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toRemove"></param>
    Public Sub RemoveObject(Of T As {IStoreableObject})(toRemove As T)
        Using con As New SqlConnection(_connectionString) : con.Open()
            If Not CheckExistence(toRemove, con) Then Return
            SendQuery($"DELETE FROM {toRemove.TableName} WHERE Id = {toRemove.Id}", con)
            SendQuery($"DELETE FROM _enumerablesOfT WHERE Id = {toRemove.Id}", con)
            If Cache.ContainsKey(toRemove.Id) Then Cache.TryRemove(toRemove.Id, Nothing)
        End Using
    End Sub
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    Public Sub RemoveObject(Of T As {New, IStoreableObject})(id As ULong)
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
            Using conn = New SqlConnection(_connectionString) : Await conn.OpenAsync.Unawait
                Using cmd As New SqlCommand(query, conn)
                    Await cmd.ExecuteNonQueryAsync.Unawait
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Await cmd.ExecuteNonQueryAsync.Unawait
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
    ''' <typeparam name="T"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function SendScalarAsync(Of T)(query As String, Optional con As SqlConnection = Nothing) As Task(Of T)
        If con Is Nothing Then
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync.Unawait
                Using cmd As New SqlCommand(query, conn)
                    Dim result = Await cmd.ExecuteScalarAsync.Unawait
                    Return If(TypeOf result Is T, DirectCast(result, T), Nothing)
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Dim result = Await cmd.ExecuteScalarAsync.Unawait
                Return If(TypeOf result Is T, DirectCast(result, T), Nothing)
            End Using
        End If
    End Function
    ''' <summary>
    ''' Sends a query to the database and returns the first value retrieved of the type provided. Returns null if cast fails.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Function SendScalar(Of T)(query As String, Optional con As SqlConnection = Nothing) As T
        Return SendScalarAsync(Of T)(query, con).Result
    End Function
#End Region
#Region "YieldData"
    ''' <summary>
    ''' Yields the data of the resulting column into an IEnumerable, if there are other rows they will be ignored.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="query"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Iterator Function YieldData(Of T)(query As String, Optional con As SqlConnection = Nothing) As IEnumerable(Of T)
        If con Is Nothing Then
            Using conn As New SqlConnection(_connectionString) : conn.Open()
                Using cmd As New SqlCommand(query, conn)
                    Using r = cmd.ExecuteReader
                        While r.Read
                            Yield If(IsDBNull(r.Item(0)), Nothing, (If(TypeOf r.Item(0) Is T, DirectCast(r.Item(0), T), CType(r.Item(0), T))))
                        End While
                    End Using
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Using r = cmd.ExecuteReader
                    While r.Read
                        Yield If(IsDBNull(r.Item(0)), Nothing, (If(TypeOf r.Item(0) Is T, DirectCast(r.Item(0), T), CType(r.Item(0), T))))
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
    Public Async Function CheckExistenceAsync(Of T As {IStoreableObject})(obj As T, Optional con As SqlConnection = Nothing) As Task(Of Boolean)
        If con Is Nothing Then
            Using cmd As New SqlCommand($"Select COUNT(Id) FROM {obj.TableName} WHERE Id = {obj.Id};", con)
                Return DirectCast(Await cmd.ExecuteScalarAsync.Unawait, Integer) = 1
            End Using
        Else
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync.Unawait
                Using cmd As New SqlCommand($"Select COUNT(Id) FROM {obj.TableName} WHERE Id = {obj.Id};", con)
                    Return DirectCast(Await cmd.ExecuteScalarAsync.Unawait, Integer) = 1
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
    Public Function CheckExistence(Of T As {IStoreableObject})(obj As T, Optional con As SqlConnection = Nothing) As Boolean
        Return CheckExistenceAsync(obj, con).Result
    End Function
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="con"></param>
    ''' <returns></returns>
    Public Async Function CheckExistenceAsync(Of T As {New, IStoreableObject})(id As ULong, Optional con As SqlConnection = Nothing) As Task(Of Boolean)
        Dim obj As New T With {.Id = id}
        Return Await CheckExistenceAsync(obj, con).Unawait
    End Function
    ''' <summary>
    ''' Checks the existence of an Object, optionally you can provide an Open connection.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function CheckExistence(Of T As {New, IStoreableObject})(id As ULong, Optional con As SqlConnection = Nothing) As Boolean
        Dim obj As New T With {.Id = id}
        Return CheckExistenceAsync(obj, con).Result
    End Function
#End Region
#Region "Private Methods"
    Private Function IsClassOrStruct(Type As Type) As Boolean
        If Type.IsPrimitive Then Return False
        If Type Is GetType(Decimal) OrElse
           Type Is GetType(Date) OrElse
           Type Is GetType(DateTimeOffset) OrElse
           Type Is GetType(TimeSpan) Then Return False

        Return Not Type.IsClass
    End Function
    Private Function ParseObject([Enum] As ICollection(Of KeyValuePair(Of Integer, String)), obj As IStoreableObject, name As String) As Object
        Dim propType = obj.GetType.GetProperty(name).PropertyType
        Dim type = GetNullableTypeName(If(propType.GenericTypeArguments.Count = 0, propType, propType.GetGenericArguments.First))
        Dim typeForDict = If(propType.GenericTypeArguments.Count > 1, GetNullableTypeName(propType.GetGenericArguments.ElementAt(1)), Nothing)
        Dim propName = propType.Name
        Dim typeName = If(propName.Contains("`"c), propName.Substring(0, propName.IndexOf("`"c)), propName)
        typeName = If(propName.Contains("[]"), "Array", typeName)
        Select Case typeName
            Case "List", "IList"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToList
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToList
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToList
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToList
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToList
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToList
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToList
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToList
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToList
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToList
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToList
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToList
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToList
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToList
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToList
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToList
                End Select
            Case "ImmutableList", "IImmutableList"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToImmutableList
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToImmutableList
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToImmutableList
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToImmutableList
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToImmutableList
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToImmutableList
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToImmutableList
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToImmutableList
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToImmutableList
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToImmutableList
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToImmutableList
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToImmutableList
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToImmutableList
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToImmutableList
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToImmutableList
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToImmutableList
                End Select
            Case "Collection", "ICollection"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value)
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value))
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value))
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value))
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value))
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value))
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value))
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value))
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value))
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value))
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value))
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value))
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value))
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value))
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value))
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value))
                End Select
            Case "ReadOnlyCollection", "IReadOnlyCollection"
                Select Case type
                    Case "String" : Return New ReadOnlyCollection(Of String)([Enum].Select(Function(x) x.Value).ToArray)
                    Case "UInt64" : Return New ReadOnlyCollection(Of ULong)([Enum].Select(Function(x) ULong.Parse(x.Value)).ToArray)
                    Case "Int64" : Return New ReadOnlyCollection(Of Long)([Enum].Select(Function(x) Long.Parse(x.Value)).ToArray)
                    Case "UInt32" : Return New ReadOnlyCollection(Of UInteger)([Enum].Select(Function(x) UInteger.Parse(x.Value)).ToArray)
                    Case "Int32" : Return New ReadOnlyCollection(Of Integer)([Enum].Select(Function(x) Integer.Parse(x.Value)).ToArray)
                    Case "UInt16" : Return New ReadOnlyCollection(Of UShort)([Enum].Select(Function(x) UShort.Parse(x.Value)).ToArray)
                    Case "Int16" : Return New ReadOnlyCollection(Of Short)([Enum].Select(Function(x) Short.Parse(x.Value)).ToArray)
                    Case "Boolean" : Return New ReadOnlyCollection(Of Boolean)([Enum].Select(Function(x) Boolean.Parse(x.Value)).ToArray)
                    Case "Byte" : Return New ReadOnlyCollection(Of Byte)([Enum].Select(Function(x) Byte.Parse(x.Value)).ToArray)
                    Case "SByte" : Return New ReadOnlyCollection(Of SByte)([Enum].Select(Function(x) SByte.Parse(x.Value)).ToArray)
                    Case "Decimal" : Return New ReadOnlyCollection(Of Decimal)([Enum].Select(Function(x) Decimal.Parse(x.Value)).ToArray)
                    Case "Double" : Return New ReadOnlyCollection(Of Double)([Enum].Select(Function(x) Double.Parse(x.Value)).ToArray)
                    Case "Single" : Return New ReadOnlyCollection(Of Single)([Enum].Select(Function(x) Single.Parse(x.Value)).ToArray)
                    Case "DateTime" : Return New ReadOnlyCollection(Of Date)([Enum].Select(Function(x) Date.Parse(x.Value)).ToArray)
                    Case "DateTimeOffset" : Return New ReadOnlyCollection(Of DateTimeOffset)([Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToArray)
                    Case "TimeSpan" : Return New ReadOnlyCollection(Of TimeSpan)([Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToArray)
                End Select
            Case "Enumerable", "IEnumerable"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).AsEnumerable
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).AsEnumerable
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).AsEnumerable
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).AsEnumerable
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).AsEnumerable
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).AsEnumerable
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).AsEnumerable
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).AsEnumerable
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).AsEnumerable
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).AsEnumerable
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).AsEnumerable
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).AsEnumerable
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).AsEnumerable
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).AsEnumerable
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).AsEnumerable
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).AsEnumerable
                End Select
            Case "Array"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToArray
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToArray
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToArray
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToArray
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToArray
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToArray
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToArray
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToArray
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToArray
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToArray
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToArray
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToArray
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToArray
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToArray
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToArray
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToArray
                End Select
            Case "ImmutableArray", "IImmutableArray"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToImmutableArray
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToImmutableArray
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToImmutableArray
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToImmutableArray
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToImmutableArray
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToImmutableArray
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToImmutableArray
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToImmutableArray
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToImmutableArray
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToImmutableArray
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToImmutableArray
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToImmutableArray
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToImmutableArray
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToImmutableArray
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToImmutableArray
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToImmutableArray
                End Select
            Case "HashSet", "ISet"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToHashSet
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToHashSet
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToHashSet
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToHashSet
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToHashSet
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToHashSet
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToHashSet
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToHashSet
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToHashSet
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToHashSet
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToHashSet
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToHashSet
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToHashSet
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToHashSet
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToHashSet
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToHashSet
                End Select
            Case "ImmutableHashSet", "IImmutableSet"
                Select Case type
                    Case "String" : Return [Enum].Select(Function(x) x.Value).ToImmutableHashSet
                    Case "UInt64" : Return [Enum].Select(Function(x) ULong.Parse(x.Value)).ToImmutableHashSet
                    Case "Int64" : Return [Enum].Select(Function(x) Long.Parse(x.Value)).ToImmutableHashSet
                    Case "UInt32" : Return [Enum].Select(Function(x) UInteger.Parse(x.Value)).ToImmutableHashSet
                    Case "Int32" : Return [Enum].Select(Function(x) Integer.Parse(x.Value)).ToImmutableHashSet
                    Case "UInt16" : Return [Enum].Select(Function(x) UShort.Parse(x.Value)).ToImmutableHashSet
                    Case "Int16" : Return [Enum].Select(Function(x) Short.Parse(x.Value)).ToImmutableHashSet
                    Case "Boolean" : Return [Enum].Select(Function(x) Boolean.Parse(x.Value)).ToImmutableHashSet
                    Case "Byte" : Return [Enum].Select(Function(x) Byte.Parse(x.Value)).ToImmutableHashSet
                    Case "SByte" : Return [Enum].Select(Function(x) SByte.Parse(x.Value)).ToImmutableHashSet
                    Case "Decimal" : Return [Enum].Select(Function(x) Decimal.Parse(x.Value)).ToImmutableHashSet
                    Case "Double" : Return [Enum].Select(Function(x) Double.Parse(x.Value)).ToImmutableHashSet
                    Case "Single" : Return [Enum].Select(Function(x) Single.Parse(x.Value)).ToImmutableHashSet
                    Case "DateTime" : Return [Enum].Select(Function(x) Date.Parse(x.Value)).ToImmutableHashSet
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) DateTimeOffset.Parse(x.Value)).ToImmutableHashSet
                    Case "TimeSpan" : Return [Enum].Select(Function(x) TimeSpan.Parse(x.Value)).ToImmutableHashSet
                End Select
            Case "Dictionary", "IDictionary"
                Select Case typeForDict
                    Case "String" : Return [Enum].ToDictionary
                    Case "UInt64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, ULong)(x.Key, ULong.Parse(x.Value))).ToDictionary
                    Case "Int64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Long)(x.Key, Long.Parse(x.Value))).ToDictionary
                    Case "UInt32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UInteger)(x.Key, UInteger.Parse(x.Value))).ToDictionary
                    Case "Int32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Integer)(x.Key, Integer.Parse(x.Value))).ToDictionary
                    Case "UInt16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UShort)(x.Key, UShort.Parse(x.Value))).ToDictionary
                    Case "Int16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Short)(x.Key, Short.Parse(x.Value))).ToDictionary
                    Case "Boolean" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Boolean)(x.Key, Boolean.Parse(x.Value))).ToDictionary
                    Case "Byte" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Byte)(x.Key, Byte.Parse(x.Value))).ToDictionary
                    Case "SByte" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, SByte)(x.Key, SByte.Parse(x.Value))).ToDictionary
                    Case "Decimal" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Decimal)(x.Key, Decimal.Parse(x.Value))).ToDictionary
                    Case "Double" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Double)(x.Key, Double.Parse(x.Value))).ToDictionary
                    Case "Single" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Single)(x.Key, Single.Parse(x.Value))).ToDictionary
                    Case "DateTime" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Date)(x.Key, Date.Parse(x.Value))).ToDictionary
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToDictionary
                    Case "TimeSpan" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToDictionary
                End Select
            Case "ImmutableDictionary", "IImmutableDictionary"
                Select Case typeForDict
                    Case "String" : Return [Enum].ToImmutableDictionary
                    Case "UInt64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, ULong)(x.Key, ULong.Parse(x.Value))).ToImmutableDictionary
                    Case "Int64" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Long)(x.Key, Long.Parse(x.Value))).ToImmutableDictionary
                    Case "UInt32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UInteger)(x.Key, UInteger.Parse(x.Value))).ToImmutableDictionary
                    Case "Int32" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Integer)(x.Key, Integer.Parse(x.Value))).ToImmutableDictionary
                    Case "UInt16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, UShort)(x.Key, UShort.Parse(x.Value))).ToImmutableDictionary
                    Case "Int16" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Short)(x.Key, Short.Parse(x.Value))).ToImmutableDictionary
                    Case "Boolean" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Boolean)(x.Key, Boolean.Parse(x.Value))).ToImmutableDictionary
                    Case "Byte" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Byte)(x.Key, Byte.Parse(x.Value))).ToImmutableDictionary
                    Case "SByte" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, SByte)(x.Key, SByte.Parse(x.Value))).ToImmutableDictionary
                    Case "Decimal" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Decimal)(x.Key, Decimal.Parse(x.Value))).ToImmutableDictionary
                    Case "Double" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Double)(x.Key, Double.Parse(x.Value))).ToImmutableDictionary
                    Case "Single" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Single)(x.Key, Single.Parse(x.Value))).ToImmutableDictionary
                    Case "DateTime" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, Date)(x.Key, Date.Parse(x.Value))).ToImmutableDictionary
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToImmutableDictionary
                    Case "TimeSpan" : Return [Enum].Select(Function(x) New KeyValuePair(Of Integer, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToImmutableDictionary
                End Select
        End Select
        Throw New UnsupportedTypeException
    End Function
    Private Async Function GetCollection(Of TKey, TValue)(id As ULong, name As String, con As SqlConnection) As Task(Of ICollection(Of KeyValuePair(Of TKey, TValue)))
        Dim dict As New Dictionary(Of TKey, TValue)
        Using command As New SqlCommand($"Select* FROM _enumerablesOfT WHERE Id = {id} And PropName = '{name}'", con)
            Using r = Await command.ExecuteReaderAsync.Unawait
                While Await r.ReadAsync.Unawait
                    dict.Add(DirectCast(r.Item(3), TKey), DirectCast(r.Item(4), TValue))
                End While
            End Using
        End Using
        Return dict
    End Function
    Private Async Function BuildInsertAsync(Of T As {IStoreableObject})(obj As T, properties As IEnumerable(Of PropertyInfo), con As SqlConnection) As Task(Of String)
        Dim collections = properties.Where(Function(x) GetType(ICollection).IsAssignableFrom(x.PropertyType)).ToImmutableArray
        If collections.Count > 0 Then
            For Each collection In collections
                Await InsertCollectionAsync(obj, collection, con).Unawait
            Next
        End If

        Dim props = If(collections.Count > 0, properties.Except(collections), properties).ToImmutableArray

        Return $"INSERT INTO {obj.TableName} ({props.Select(Function(x) x.Name).Aggregate(Function(x, y) x & ", " & y)})" & vbCrLf &
           $"VALUES ({props.Select(Function(x) $"{GetSqlValue(x, obj)}").Aggregate(Function(x, y) x & ", " & y)});"
    End Function
    Private Function InsertCollectionAsync(Of T As {IStoreableObject})(obj As T, [Property] As PropertyInfo, con As SqlConnection) As Task
        Dim generic = [Property].GetValue(obj)
        If generic Is Nothing Then Return Task.CompletedTask
        Dim enumerable = GetGenericEnumerable(generic).ToImmutableArray
        Dim index = 0
        Dim values = DirectCast(generic, ICollection)
        For Each element In values
            SendQuery($"INSERT INTO _enumerablesOfT (Id, ObjName, PropName, RawKey, RawValue)" & vbCrLf &
                  $"VALUES ({obj.Id}, '{obj.TableName}', '{[Property].Name}', {index}, '{element}');", con)
            index += 1
        Next
        Return Task.CompletedTask
    End Function

    Private Function GetGenericEnumerable(obj As Object) As IEnumerable(Of Type)
        Return From o In obj.GetType.GetInterfaces
               Where o.IsGenericType AndAlso
                 o.GetGenericTypeDefinition = GetType(IEnumerable(Of))
               Select o.GetGenericArguments.First
    End Function
    Private Function BuildTable(Of T As {IStoreableObject})(obj As T) As String
        Dim sb As New StringBuilder
        With sb
            .AppendLine($"CREATE TABLE {obj.TableName} (")

            Dim properties = (From prop In obj.GetType.GetProperties
                              Where prop.GetCustomAttribute(Of StoreAttribute)(True) IsNot Nothing AndAlso
                              Not GetType(ICollection).IsAssignableFrom(prop.PropertyType) AndAlso
                              Not IsClassOrStruct(prop.PropertyType)
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
    Private Async Function CheckObjectExistenceAsync(Of T As {IStoreableObject})(obj As T, con As SqlConnection) As Task(Of Boolean)
        Return (Await SendScalarAsync(Of Integer)($"IF OBJECT_ID('{obj.TableName}') IS NULL SELECT 0" & vbCrLf &
                                                   "ELSE SELECT 1;", con).Unawait) = 1
    End Function

    Private Function CheckObjectExistence(Of T As {IStoreableObject})(obj As T, con As SqlConnection) As Boolean
        Return (SendScalar(Of Integer)($"IF OBJECT_ID('{obj.TableName}') IS NULL SELECT 0" & vbCrLf &
                                        "ELSE SELECT 1;", con)) = 1
    End Function
    Private Function ParseType(typeName As String, Optional stringLimit As Integer? = Nothing) As String
        Select Case typeName
            Case "UInt64" : Return "DECIMAL(20,0)"
            Case "Int64" : Return "BIGINT"
            Case "UInt32" : Return "DECIMAL(10,0)"
            Case "Int32" : Return "INT"
            Case "UInt16" : Return "DECIMAL(5,0)"
            Case "Int16", "UInt16" : Return "SMALLINT"
            Case "Boolean" : Return "BIT"
            Case "String" : Return $"VARCHAR({If(stringLimit Is Nothing, $"{_stringLimit}", If(stringLimit = -1, "MAX", $"{stringLimit}"))})"
            Case "Byte" : Return "DECIMAL(3,0)"
            Case "SByte" : Return "TINYINT"
            Case "Decimal" : Return "DECIMAL"
            Case "Double" : Return "FLOAT"
            Case "Single" : Return "REAL"
            Case "DateTime" : Return "DATE"
            Case "DateTimeOffset" : Return "DATETIMEOFFSET"
            Case "TimeSpan" : Return "TIME"
        End Select
        Throw New UnsupportedTypeException
        Return Nothing
    End Function

    Private Function GetNullableTypeName(propType As Type) As String
        If propType.Name.Contains("[]") Then Return propType.Name.Replace("[]", "")
        Return If(propType.IsGenericType AndAlso propType.GetGenericTypeDefinition = GetType(Nullable(Of)),
              propType.GetGenericArguments.First.Name,
              propType.Name)
    End Function

    Private Function GetSqlValue(Of T As {IStoreableObject})([Property] As PropertyInfo, ByRef obj As T) As String
        Dim value = [Property].GetValue(obj)
        Dim typeName = ParseType(GetNullableTypeName([Property].PropertyType))

        Select Case typeName
            Case "DECIMAL(20,0)", "BIGINT", "DECIMAL(10,0)", "SMALLINT", "DECIMAL(5,0)", "INT", "DECIMAL(3,0)", "TINYINT", "DECIMAL", "FLOAT", "REAL" : Return $"{If(value, "NULL")}"
            Case "DATE", "TIME", "DATETIMEOFFSET", $"VARCHAR({If([Property].GetCustomAttribute(Of StringLengthAttribute)(True)?.Length, _stringLimit)})" : Return $"'{If(value, "NULL")}'"
            Case "BIT" : Return $"{If(DirectCast(value, Boolean), 1, 0)}"
            Case Else : Throw New UnsupportedTypeException
        End Select
        Return Nothing
    End Function

    Private Function UnsignedFix(Of T As {IStoreableObject})(obj As T, name As String, value As Object) As Object
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
#End Region
End Class