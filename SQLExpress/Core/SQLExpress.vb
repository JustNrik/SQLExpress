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
    Private _enumFlag As Boolean
    Private _tupleFlag As Boolean
    Private _stringLimit As Integer
    Private _useCache As Boolean
    Private _logEnable As Boolean
#End Region
#Region "Properties"
    ''' <summary>
    ''' This is the cache of objects, will be used if enabled.
    ''' </summary>
    ''' <returns></returns>
    Public Property Cache As New ConcurrentDictionary(Of ULong, IStoreableObject)
    ''' <summary>
    ''' Sets the limit of the length of string type properties to be stored in the database. Only values between 1 and 8000 are accepted, use -1 for MAX.
    ''' </summary>
    ''' <returns></returns>
    Public Property StringLimit As Integer
        Get
            Return _stringLimit
        End Get
        Set
            Select Case Value
                Case -1, 1 To 8000 : _stringLimit = Value
                Case Else : Throw New ArgumentOutOfRangeException("Length must be between 1 and 8000, or -1 if you want to use MAX length instead")
            End Select
        End Set
    End Property
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
    ''' Provide the full connection string.
    ''' </summary>
    ''' <param name="ConnectionString"></param>
    Sub New(config As SQLExpressConfig)
        With config
            StringLimit = .StringLimit
            _connectionString = .ConnectionString
            _logEnable = .Logging
            _useCache = .UseCache
        End With
    End Sub
    ''' <summary>
    ''' Reads the config from a XML Document. You must load the XMlDocument before using this method.
    ''' </summary>
    ''' <param name="xmlConfig"></param>
    Sub New(xmlConfig As XmlDocument)
        ReadConfig(xmlConfig)
    End Sub
    ''' <summary>
    ''' Reads the config from a JSON file. You must parse the JObject before using this method.
    ''' </summary>
    ''' <param name="jConfig"></param>
    Sub New(jConfig As JObject)
        ReadConfig(jConfig)
    End Sub
#End Region
#Region "Config"
    Private Sub ReadConfig(xmlConfig As XmlDocument)
        _database = xmlConfig.SelectSingleNode("*/database").InnerXml
        _sqlServer = xmlConfig.SelectSingleNode("*/server").InnerXml
        _username = xmlConfig.SelectSingleNode("*/username").InnerXml
        _password = xmlConfig.SelectSingleNode("*/password").InnerXml
        _logEnable = Boolean.Parse(xmlConfig.SelectSingleNode("*/enablelogging").InnerXml)
        _useCache = Boolean.Parse(xmlConfig.SelectSingleNode("*/usecache").InnerXml)
        _stringLimit = Integer.Parse(xmlConfig.SelectSingleNode("*/stringlimit").InnerXml)
        _connectionString = $"Server={_sqlServer};Database={_database};User Id={_username};Password={_password};"
    End Sub

    Private Sub ReadConfig(jConfig As JObject)
        _database = $"{jConfig("database")}"
        _sqlServer = $"{jConfig("server")}"
        _username = $"{jConfig("username")}"
        _password = $"{jConfig("password")}"
        _logEnable = Boolean.Parse($"{jConfig("enablelogging")}")
        _useCache = Boolean.Parse($"{jConfig("usecache")}")
        _stringLimit = Integer.Parse($"{jConfig("stringlimit")}")
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
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            For Each obj In objs
                If Not Await CheckObjectExistenceAsync(obj, con).ConfigureAwait(False) Then
                    Await SendQueryAsync(BuildTable(obj), con).ConfigureAwait(False)
                    Dim innerObjs = GetTypes(obj).Distinct.ToImmutableArray
                    For Each innerObj In innerObjs
                        If Not Await CheckObjectExistenceAsync(innerObj, con) Then _
                            Await SendQueryAsync(BuildTable(innerObj), con).ConfigureAwait(False)
                    Next
                End If
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
                If Not CheckObjectExistence(obj, con) Then
                    SendQuery(BuildTable(obj), con)
                    Dim innerObjs = GetTypes(obj).Distinct.ToImmutableArray
                    For Each innerObj In innerObjs
                        If Not CheckObjectExistence(innerObj, con) Then SendQuery(BuildTable(innerObj), con)
                    Next
                End If
            Next
        End Using
    End Sub
#End Region
#Region "InstallDatabase"
    Public Async Function InstallDatabaseAsync() As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            If Await SendScalarAsync(Of Integer)("IF OBJECT_ID('_enumerablesOfT') IS NULL SELECT 0" & vbCrLf &
                                                 "ELSE SELECT 1;", con).ConfigureAwait(False) = 0 Then

                Await SendQueryAsync("CREATE TABLE _enumerablesOfT (" & vbCrLf &
                                     "    Id BIGINT NOT NULL," & vbCrLf &
                                     "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    RawKey INT NOT NULL," & vbCrLf &
                                     "    RawValue VARCHAR(50));", con).ConfigureAwait(False)

            End If

            If Await SendScalarAsync(Of Integer)("IF OBJECT_ID('_tuplesOfT') IS NULL SELECT 0" & vbCrLf &
                                                 "ELSE SELECT 1;", con).ConfigureAwait(False) = 0 Then

                Await SendQueryAsync("CREATE TABLE _tuplesOfT (" & vbCrLf &
                                     "    Id BIGINT NOT NULL," & vbCrLf &
                                     "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                                     "    Item1 VARCHAR(50)," & vbCrLf &
                                     "    Item2 VARCHAR(50)," & vbCrLf &
                                     "    Item3 VARCHAR(50)," & vbCrLf &
                                     "    Item4 VARCHAR(50)," & vbCrLf &
                                     "    Item5 VARCHAR(50)," & vbCrLf &
                                     "    Item6 VARCHAR(50)," & vbCrLf &
                                     "    Item7 VARCHAR(50));", con).ConfigureAwait(False)
            End If
        End Using
    End Function

    Public Sub InstallDatabase()
        Using con As New SqlConnection(_connectionString) : con.Open()
            If SendScalar(Of Integer)("IF OBJECT_ID('_enumerablesOfT') IS NULL SELECT 0" & vbCrLf &
                                      "ELSE SELECT 1;", con) = 0 Then

                SendQuery("CREATE TABLE _enumerablesOfT (" & vbCrLf &
                          "    Id BIGINT NOT NULL," & vbCrLf &
                          "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    RawKey INT NOT NULL," & vbCrLf &
                          "    RawValue VARCHAR(50));", con)

            End If

            If SendScalar(Of Integer)("IF OBJECT_ID('_tuplesOfT') IS NULL SELECT 0" & vbCrLf &
                                      "ELSE SELECT 1;", con) = 0 Then

                SendQuery("CREATE TABLE _tuplesOfT (" & vbCrLf &
                          "    Id BIGINT NOT NULL," & vbCrLf &
                          "    ObjName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    PropName VARCHAR(50) NOT NULL," & vbCrLf &
                          "    Item1 VARCHAR(50)," & vbCrLf &
                          "    Item2 VARCHAR(50)," & vbCrLf &
                          "    Item3 VARCHAR(50)," & vbCrLf &
                          "    Item4 VARCHAR(50)," & vbCrLf &
                          "    Item5 VARCHAR(50)," & vbCrLf &
                          "    Item6 VARCHAR(50)," & vbCrLf &
                          "    Item7 VARCHAR(50),", con)
            End If
        End Using
    End Sub
#End Region
#Region "LoadObjectCache"
    ''' <summary>
    ''' Loads the cache of the provided object.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectCacheAsync(Of T As {New, IStoreableObject})(obj As T) As Task
        If Not _useCache Then Throw New CacheDisabledException
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            If Await SendScalarAsync(Of Integer)($"SELECT COUNT(Id) FROM {obj.TableName};", con).ConfigureAwait(False) > 0 Then
                Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.TableName};", con).ToImmutableArray
                For Each id In ids
                    Dim newObj = Await LoadObjectAsync(New T With {.Id = id}).ConfigureAwait(False)
                    If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
                Next
            End If
        End Using
    End Function
    ''' <summary>
    ''' Loads the cache of the provided object.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    Public Sub LoadObjectCache(Of T As {New, IStoreableObject})(obj As T)
        If Not _useCache Then Throw New CacheDisabledException
        Using con As New SqlConnection(_connectionString) : con.Open()
            If SendScalar(Of Integer)($"SELECT COUNT(Id) FROM {obj.TableName};", con) > 0 Then
                Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.TableName};", con).ToImmutableArray
                For Each id In ids
                    Dim newObj = LoadObject(New T With {.Id = id})
                    If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj)
                Next
            End If
        End Using
    End Sub
    ''' <summary>
    ''' Loads the cache with all objects stored in the database of the type provided.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function LoadObjectsCacheAsync(Of T As {IStoreableObject})(ParamArray objs As T()) As Task
        If Not _useCache Then Throw New CacheDisabledException
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            For Each obj In objs
                If Await SendScalarAsync(Of Integer)($"SELECT COUNT(Id) FROM {obj.TableName};", con).ConfigureAwait(False) > 0 Then
                    Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.TableName};", con).ToImmutableArray
                    For Each id In ids
                        Dim instance = DirectCast(Activator.CreateInstance(obj.GetType), T) : instance.Id = id
                        Dim newObj = Await LoadObjectAsync(instance).ConfigureAwait(False)
                        If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
                    Next
                End If
            Next
        End Using
    End Function
    ''' <summary>
    ''' Loads the cache with all objects stored in the database of the type provided.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    Public Sub LoadObjectsCache(Of T As {IStoreableObject})(ParamArray objs As T())
        If Not _useCache Then Throw New CacheDisabledException
        Using con As New SqlConnection(_connectionString) : con.Open()
            For Each obj In objs
                If SendScalar(Of Integer)($"SELECT COUNT(Id) FROM {obj.TableName};", con) > 0 Then
                    Dim ids = YieldData(Of ULong)($"SELECT Id FROM {obj.TableName};", con).ToImmutableArray
                    For Each id In ids
                        Dim instance = DirectCast(Activator.CreateInstance(obj.GetType), T) : instance.Id = id
                        Dim newObj = LoadObject(instance)
                        If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
                    Next
                End If
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
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)

            Dim properties = GetAllStoreablePropierties(obj.GetType.GetProperties)
            Dim primitives = GetPrivitimes(properties).ToImmutableArray
            Dim collections = properties.Where(Function(x) IsCollection(x)).ToImmutableArray
            Dim types = properties.Where(Function(x) IsClassOrStruct(x)).ToImmutableArray
            Dim tuples = properties.Where(Function(x) IsTuple(x)).ToImmutableArray

            If properties.Length = 0 Then Throw New EmptyObjectException
            If properties.Any(Function(x) x.GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing AndAlso
                                          x.GetValue(obj) Is Nothing) Then Throw New NullPropertyException

            Await SendQueryAsync(BuildInsert(obj, primitives, con), con).ConfigureAwait(False)

            If types.Length > 0 Then
                For Each prop In types
                    Dim propObj = TryCast(prop.GetValue(obj), IStoreableObject)
                    If propObj IsNot Nothing AndAlso Not Await CheckObjectExistenceAsync(propObj, con).ConfigureAwait(False) Then _
                        Await SendQueryAsync(BuildTable(propObj), con).ConfigureAwait(False)
                Next
            End If

            If collections.Length > 0 Then
                For Each collection In collections
                    Await InsertCollectionAsync(obj, collection, con)
                Next
            End If

            If tuples.Length > 0 Then
                For Each tuple In tuples
                    Await InsertTupleAsync(obj, tuple, con)
                Next
            End If

            Log(obj, LogType.Create)
            Dim newObj = Await LoadObjectAsync(obj).ConfigureAwait(False)
            If _useCache Then If Not Cache.ContainsKey(newObj.Id) Then Cache.TryAdd(newObj.Id, newObj) Else Cache(newObj.Id) = newObj
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
        Return Await CreateNewObjectAsync(New T With {.Id = id})
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
        Return CreateNewObjectAsync(New T With {.Id = id}).Result
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="obj"></param>
    Public Sub CreateNewObject(Of T As {New, IStoreableObject})(id As ULong, <Out> ByRef obj As T)
        obj = CreateNewObjectAsync(Of T)(id).Result
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
        If _useCache AndAlso Cache.ContainsKey(toLoad.Id) Then Return DirectCast(Cache(toLoad.Id), T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            If Not Await CheckExistenceAsync(toLoad, con).ConfigureAwait(False) Then Return Await CreateNewObjectAsync(toLoad).ConfigureAwait(False)

            Dim properties = GetAllStoreablePropierties(toLoad.GetType.GetProperties)
            Dim primitives = GetPrivitimes(properties).ToImmutableArray
            Dim primitivesName = primitives.Select(Function(x) x.Name).ToImmutableArray
            Dim collections = properties.Where(Function(x) IsCollection(x)).ToImmutableArray
            Dim collectionNames = collections.Select(Function(x) x.Name).ToImmutableArray
            Dim types = properties.Where(Function(x) IsClassOrStruct(x)).ToImmutableArray
            Dim tuples = properties.Where(Function(x) IsTuple(x)).ToImmutableArray

            If properties.Length = 0 Then Throw New EmptyObjectException
            If properties.Any(Function(x) x.GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing AndAlso
                                          x.GetValue(toLoad) Is Nothing) Then Throw New NullPropertyException

            If Not _enumFlag Then _enumFlag = Await SendScalarAsync(Of Integer)("SELECT COUNT(Id) FROM _enumerablesOfT", con).ConfigureAwait(False) > 0
            If Not _tupleFlag Then _tupleFlag = Await SendScalarAsync(Of Integer)("SELECT COUNT(Id) FROM _tuplesOfT", con).ConfigureAwait(False) > 0

            If collectionNames.Length > 0 AndAlso _enumFlag Then
                Dim collection As New List(Of ICollection(Of KeyValuePair(Of ULong, String)))
                For Each collectionName In collectionNames
                    collection.Add(Await GetCollectionAsync(toLoad.Id, collectionName, con).ConfigureAwait(False))
                Next

                collection.RemoveAll(Function(x) x Is Nothing)

                If collection.Count > 0 Then
                    For x = 0 To collectionNames.Length - 1
                        Dim [property] = toLoad.GetType.GetProperty(collectionNames(x))
                        Dim temp = [property].PropertyType.GenericTypeArguments.Where(Function(y) IsClassOrStruct(y))

                        If temp.Count > 0 Then
                            For Each item In temp
                                Dim targetType = GetType(List(Of)).MakeGenericType(item)
                                Dim newCol = DirectCast(Activator.CreateInstance(targetType), IList)
                                Dim keys = collection(x).Select(Function(kvp) kvp.Key)
                                For Each key In keys
                                    Dim instance = DirectCast(Activator.CreateInstance(item), IStoreableObject)
                                    If instance Is Nothing Then Continue For Else instance.Id = key
                                    newCol.Add(Await LoadObjectAsync(instance))
                                Next
                                [property].SetValue(toLoad, ParseCollection(newCol, toLoad, [property].Name))
                            Next
                        Else
                            [property].SetValue(toLoad, ParseObject(collection(x), toLoad, collectionNames(x)))
                        End If
                    Next
                End If
            End If

            If types.Length > 0 Then
                For Each obj In types
                    Dim refObj = TryCast(obj.GetValue(toLoad), IStoreableObject)
                    If refObj IsNot Nothing Then
                        refObj.Id = If(refObj.Id = 0, toLoad.Id, refObj.Id)
                        Dim loadObj = Await LoadObjectAsync(refObj).ConfigureAwait(False)
                        toLoad.GetType.GetProperty(obj.Name).SetValue(toLoad, refObj)
                    End If
                Next
            End If

            If tuples.Length > 0 AndAlso _tupleFlag Then
                For Each tuple In tuples
                    tuple.SetValue(toLoad, Await GetTuple(toLoad, tuple, con))
                Next
            End If

            Using cmd As New SqlCommand($"SELECT* FROM {toLoad.TableName} WHERE Id = {toLoad.Id};", con)
                Using r = Await cmd.ExecuteReaderAsync.ConfigureAwait(False)
                    While Await r.ReadAsync.ConfigureAwait(False)
                        For x = 0 To r.FieldCount - 1
                            toLoad.GetType.GetProperty(primitivesName(x)).
                                SetValue(toLoad, UnsignedFix(toLoad, primitivesName(x), r.Item(primitivesName(x))))
                        Next
                    End While
                    Log(toLoad, LogType.Load)
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
        Return If(_useCache AndAlso Cache.ContainsKey(id),
            DirectCast(Cache(id), T),
            Await LoadObjectAsync(New T With {.Id = id}).ConfigureAwait(False))
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {IStoreableObject})(obj As T) As T
        Return If(_useCache AndAlso Cache.ContainsKey(obj.Id),
            DirectCast(Cache(obj.Id), T),
            LoadObjectAsync(obj).Result)
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Function LoadObject(Of T As {New, IStoreableObject})(id As ULong) As T
        Return If(_useCache AndAlso Cache.ContainsKey(id),
            DirectCast(Cache(id), T),
            LoadObjectAsync(New T With {.Id = id}).Result)
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <param name="obj"></param>
    Public Sub LoadObject(Of T As {New, IStoreableObject})(id As ULong, <Out> ByRef obj As T)
        If _useCache AndAlso Cache.ContainsKey(id) Then _
            obj = DirectCast(Cache(id), T) Else _
            obj = LoadObjectAsync(New T With {.Id = id}).Result
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
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            If Not Await CheckExistenceAsync(toUpdate, con).ConfigureAwait(False) Then
                Return Await CreateNewObjectAsync(toUpdate).ConfigureAwait(False)
            Else
                Await RemoveObjectAsync(toUpdate).ConfigureAwait(False)
                If _useCache AndAlso Cache.ContainsKey(toUpdate.Id) Then Cache.TryRemove(toUpdate.Id, Nothing)
                Dim newObj = Await CreateNewObjectAsync(toUpdate).ConfigureAwait(False)
                If _useCache AndAlso Cache.ContainsKey(toUpdate.Id) Then Cache(toUpdate.Id) = newObj Else Cache.TryAdd(newObj.Id, newObj)
                Log(newObj, LogType.Update)
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
        toUpdate = UpdateObjectAsync(toUpdate).Result
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
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync.ConfigureAwait(False)
            If Not Await CheckExistenceAsync(toRemove, con).ConfigureAwait(False) Then Return
            Await SendQueryAsync($"DELETE FROM {toRemove.TableName} WHERE Id = {toRemove.Id};", con).ConfigureAwait(False)
            Await SendQueryAsync($"DELETE FROM _enumerablesOfT WHERE Id = {toRemove.Id};", con).ConfigureAwait(False)
            Await SendQueryAsync($"DELETE FROM _tuplesOfT WHERE Id = {toRemove.Id};", con).ConfigureAwait(False)
            If _useCache AndAlso Cache.ContainsKey(toRemove.Id) Then Cache.TryRemove(toRemove.Id, Nothing)
            Log(toRemove, LogType.Delete)
        End Using
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    ''' <returns></returns>
    Public Async Function RemoveObjectAsync(Of T As {New, IStoreableObject})(id As ULong) As Task
        Await RemoveObjectAsync(New T With {.Id = id}).ConfigureAwait(False)
    End Function
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toRemove"></param>
    Public Sub RemoveObject(Of T As {IStoreableObject})(toRemove As T)
        RemoveObjectAsync(toRemove).Wait()
    End Sub
    ''' <summary>
    ''' Removes the Object if exists
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="id"></param>
    Public Sub RemoveObject(Of T As {New, IStoreableObject})(id As ULong)
        RemoveObjectAsync(New T With {.Id = id}).Wait()
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
            Using conn = New SqlConnection(_connectionString) : Await conn.OpenAsync.ConfigureAwait(False)
                Using cmd As New SqlCommand(query, conn)
                    Await cmd.ExecuteNonQueryAsync.ConfigureAwait(False)
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Await cmd.ExecuteNonQueryAsync.ConfigureAwait(False)
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
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync.ConfigureAwait(False)
                Using cmd As New SqlCommand(query, conn)
                    Dim result = Await cmd.ExecuteScalarAsync.ConfigureAwait(False)
                    Return If(TypeOf result Is T, DirectCast(result, T), Nothing)
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Dim result = Await cmd.ExecuteScalarAsync.ConfigureAwait(False)
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
                            Yield If(IsDBNull(r.Item(0)), Nothing, If(TypeOf r.Item(0) Is T, DirectCast(r.Item(0), T), CType(r.Item(0), T)))
                        End While
                    End Using
                End Using
            End Using
        Else
            Using cmd As New SqlCommand(query, con)
                Using r = cmd.ExecuteReader
                    While r.Read
                        Yield If(IsDBNull(r.Item(0)), Nothing, If(TypeOf r.Item(0) Is T, DirectCast(r.Item(0), T), CType(r.Item(0), T)))
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
            Using conn As New SqlConnection(_connectionString) : Await conn.OpenAsync.ConfigureAwait(False)
                Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {obj.TableName} WHERE Id = {obj.Id};", con)
                    Return DirectCast(Await cmd.ExecuteScalarAsync.ConfigureAwait(False), Integer) = 1
                End Using
            End Using
        Else
            Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {obj.TableName} WHERE Id = {obj.Id};", con)
                Return DirectCast(Await cmd.ExecuteScalarAsync.ConfigureAwait(False), Integer) = 1
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
        Return Await CheckExistenceAsync(obj, con).ConfigureAwait(False)
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
    Private Function ParseCollection(Of T As {IStoreableObject})(col As IList, obj As T, name As String) As Object
        Dim propType = obj.GetType.GetProperty(name).PropertyType
        Dim propName = propType.Name
        Dim typeName = If(propName.Contains("`"c), propName.Substring(0, propName.IndexOf("`"c)), propName)
        typeName = If(propName.Contains("[]"), "Array", typeName)
        Select Case typeName
            Case "List", "IList" : Return col
            Case "ImmutableList", "IImmutableList" : Return col.OfType(Of T).ToImmutableList
            Case "Collection", "ICollection" : Return New Collection(Of T)(col.OfType(Of T).ToList)
            Case "ReadOnlyCollection", "IReadOnlyCollection" : Return New ReadOnlyCollection(Of T)(col.OfType(Of T).ToList)
            Case "Enumerable", "IEnumerable" : Return col.OfType(Of T).AsEnumerable
            Case "Array" : Return col.OfType(Of T).ToArray
            Case "ImmutableArray", "IImmutableArray" : Return col.OfType(Of T).ToImmutableArray
            Case "ImmutableList", "IImmutableList" : Return col.OfType(Of T).ToImmutableList
            Case "HashSet", "ISet" : Return col.OfType(Of T).ToHashSet
            Case "ImmutableHashSet", "IImmutableSet" : Return col.OfType(Of T).ToImmutableHashSet
            Case "Dictionary", "IDictionary" : Return col.OfType(Of T).ToDictionary(Function(x) x.Id, Function(x) x)
            Case "ReadOnlyDictionary", "IReadOnlyDictionary" : Return New ReadOnlyDictionary(Of ULong, T)(col.OfType(Of T).ToDictionary(Function(x) x.Id, Function(x) x))
            Case "ImmutableDictionary", "IImmutableDictionary" : Return col.OfType(Of T).ToImmutableDictionary(Function(x) x.Id, Function(x) x)
        End Select
        Throw New UnsupportedTypeException
    End Function

    Private Function ParseObject([Enum] As ICollection(Of KeyValuePair(Of ULong, String)), obj As IStoreableObject, name As String) As Object
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
                    Case "UInt64" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, ULong)(x.Key, ULong.Parse(x.Value))).ToDictionary
                    Case "Int64" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Long)(x.Key, Long.Parse(x.Value))).ToDictionary
                    Case "UInt32" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, UInteger)(x.Key, UInteger.Parse(x.Value))).ToDictionary
                    Case "Int32" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Integer)(x.Key, Integer.Parse(x.Value))).ToDictionary
                    Case "UInt16" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, UShort)(x.Key, UShort.Parse(x.Value))).ToDictionary
                    Case "Int16" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Short)(x.Key, Short.Parse(x.Value))).ToDictionary
                    Case "Boolean" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Boolean)(x.Key, Boolean.Parse(x.Value))).ToDictionary
                    Case "Byte" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Byte)(x.Key, Byte.Parse(x.Value))).ToDictionary
                    Case "SByte" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, SByte)(x.Key, SByte.Parse(x.Value))).ToDictionary
                    Case "Decimal" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Decimal)(x.Key, Decimal.Parse(x.Value))).ToDictionary
                    Case "Double" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Double)(x.Key, Double.Parse(x.Value))).ToDictionary
                    Case "Single" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Single)(x.Key, Single.Parse(x.Value))).ToDictionary
                    Case "DateTime" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Date)(x.Key, Date.Parse(x.Value))).ToDictionary
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToDictionary
                    Case "TimeSpan" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToDictionary
                End Select
            Case "ReadOnlyDictionary", "IReadOnlyDictionary"
                Select Case typeForDict
                    Case "String" : Return New ReadOnlyDictionary(Of ULong, String)([Enum].ToDictionary)
                    Case "UInt64" : Return New ReadOnlyDictionary(Of ULong, ULong)([Enum].Select(Function(x) New KeyValuePair(Of ULong, ULong)(x.Key, ULong.Parse(x.Value))).ToDictionary)
                    Case "Int64" : Return New ReadOnlyDictionary(Of ULong, Long)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Long)(x.Key, Long.Parse(x.Value))).ToDictionary)
                    Case "UInt32" : Return New ReadOnlyDictionary(Of ULong, UInteger)([Enum].Select(Function(x) New KeyValuePair(Of ULong, UInteger)(x.Key, UInteger.Parse(x.Value))).ToDictionary)
                    Case "Int32" : Return New ReadOnlyDictionary(Of ULong, Integer)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Integer)(x.Key, Integer.Parse(x.Value))).ToDictionary)
                    Case "UInt16" : Return New ReadOnlyDictionary(Of ULong, UShort)([Enum].Select(Function(x) New KeyValuePair(Of ULong, UShort)(x.Key, UShort.Parse(x.Value))).ToDictionary)
                    Case "Int16" : Return New ReadOnlyDictionary(Of ULong, Short)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Short)(x.Key, Short.Parse(x.Value))).ToDictionary)
                    Case "Boolean" : Return New ReadOnlyDictionary(Of ULong, Boolean)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Boolean)(x.Key, Boolean.Parse(x.Value))).ToDictionary)
                    Case "Byte" : Return New ReadOnlyDictionary(Of ULong, Byte)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Byte)(x.Key, Byte.Parse(x.Value))).ToDictionary)
                    Case "SByte" : Return New ReadOnlyDictionary(Of ULong, SByte)([Enum].Select(Function(x) New KeyValuePair(Of ULong, SByte)(x.Key, SByte.Parse(x.Value))).ToDictionary)
                    Case "Decimal" : Return New ReadOnlyDictionary(Of ULong, Decimal)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Decimal)(x.Key, Decimal.Parse(x.Value))).ToDictionary)
                    Case "Double" : Return New ReadOnlyDictionary(Of ULong, Double)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Double)(x.Key, Double.Parse(x.Value))).ToDictionary)
                    Case "Single" : Return New ReadOnlyDictionary(Of ULong, Single)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Single)(x.Key, Single.Parse(x.Value))).ToDictionary)
                    Case "DateTime" : Return New ReadOnlyDictionary(Of ULong, Date)([Enum].Select(Function(x) New KeyValuePair(Of ULong, Date)(x.Key, Date.Parse(x.Value))).ToDictionary)
                    Case "DateTimeOffset" : Return New ReadOnlyDictionary(Of ULong, DateTimeOffset)([Enum].Select(Function(x) New KeyValuePair(Of ULong, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToDictionary)
                    Case "TimeSpan" : Return New ReadOnlyDictionary(Of ULong, TimeSpan)([Enum].Select(Function(x) New KeyValuePair(Of ULong, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToDictionary)
                End Select
            Case "ImmutableDictionary", "IImmutableDictionary"
                Select Case typeForDict
                    Case "String" : Return [Enum].ToImmutableDictionary
                    Case "UInt64" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, ULong)(x.Key, ULong.Parse(x.Value))).ToImmutableDictionary
                    Case "Int64" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Long)(x.Key, Long.Parse(x.Value))).ToImmutableDictionary
                    Case "UInt32" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, UInteger)(x.Key, UInteger.Parse(x.Value))).ToImmutableDictionary
                    Case "Int32" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Integer)(x.Key, Integer.Parse(x.Value))).ToImmutableDictionary
                    Case "UInt16" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, UShort)(x.Key, UShort.Parse(x.Value))).ToImmutableDictionary
                    Case "Int16" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Short)(x.Key, Short.Parse(x.Value))).ToImmutableDictionary
                    Case "Boolean" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Boolean)(x.Key, Boolean.Parse(x.Value))).ToImmutableDictionary
                    Case "Byte" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Byte)(x.Key, Byte.Parse(x.Value))).ToImmutableDictionary
                    Case "SByte" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, SByte)(x.Key, SByte.Parse(x.Value))).ToImmutableDictionary
                    Case "Decimal" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Decimal)(x.Key, Decimal.Parse(x.Value))).ToImmutableDictionary
                    Case "Double" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Double)(x.Key, Double.Parse(x.Value))).ToImmutableDictionary
                    Case "Single" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Single)(x.Key, Single.Parse(x.Value))).ToImmutableDictionary
                    Case "DateTime" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, Date)(x.Key, Date.Parse(x.Value))).ToImmutableDictionary
                    Case "DateTimeOffset" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, DateTimeOffset)(x.Key, DateTimeOffset.Parse(x.Value))).ToImmutableDictionary
                    Case "TimeSpan" : Return [Enum].Select(Function(x) New KeyValuePair(Of ULong, TimeSpan)(x.Key, TimeSpan.Parse(x.Value))).ToImmutableDictionary
                End Select
        End Select
        Throw New UnsupportedTypeException
    End Function

    Private Function BuildTable(Of T As {IStoreableObject})(obj As T) As String
        Dim sb As New StringBuilder
        With sb
            .AppendLine($"CREATE TABLE {obj.TableName} (")

            Dim properties = GetPrivitimes(GetAllStoreablePropierties(obj.GetType.GetProperties))

            If properties.Count = 0 Then Throw New EmptyObjectException
            For Each [Property] In properties
                .Append($"{[Property].Name} {ParseType([Property])} ")
                .Append($"{If([Property].GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing, "NOT NULL", "")} ")
                .Append($"{If([Property].GetCustomAttribute(Of PrimaryKeyAttribute)(True) IsNot Nothing, "PRIMARY KEY", "")}")

                If [Property] Is properties.Last Then .AppendLine(");") Else .AppendLine(",")
            Next
        End With
        Return sb.ToString
    End Function

    Private Function BuildInsert(Of T As {IStoreableObject})(obj As T, properties As ImmutableArray(Of PropertyInfo), con As SqlConnection) As String
        Return $"INSERT INTO {obj.TableName} ({properties.Select(Function(x) x.Name).Aggregate(Function(x, y) $"{x}, {y}")})" & vbCrLf &
               $"VALUES ({properties.Select(Function(x) GetSqlValue(x, obj)).Aggregate(Function(x, y) $"{x}, {y}")});"
    End Function

    Friend Async Function InsertTupleAsync(Of T As {IStoreableObject})(obj As T, prop As PropertyInfo, con As SqlConnection) As Task
        Dim sb As New StringBuilder
        Dim pivot = Integer.Parse(prop.PropertyType.Name.Substring(prop.PropertyType.Name.IndexOf("`"c) + 1))
        With sb
            .AppendLine($"INSERT INTO _tuplesOfT (Id, ObjName, PropName, Item1, Item2, Item3, Item4, Item5, Item6, Item7)")
            .Append($"VALUES ({obj.Id}, '{obj.TableName}', '{prop.Name}'")
            For x = 1 To 7
                .Append(If(x <= pivot, $", '{ParseSQLDecimal(prop.GetValue(obj).GetType.GetField($"Item{x}").GetValue(prop.GetValue(obj)))}'", ", NULL"))
            Next
            .Append(");")
        End With
        Await SendQueryAsync(sb.ToString)
    End Function

    Private Async Function InsertCollectionAsync(Of T As {IStoreableObject})(obj As T, prop As PropertyInfo, con As SqlConnection) As Task
        Dim generic = TryCast(prop.GetValue(obj), ICollection)
        If generic Is Nothing Then Return

        Dim collectionType = prop.PropertyType.GetGenericArguments()(0)
        If IsClassOrStruct(collectionType) Then
            For Each gen In generic
                Dim toLoad = TryCast(gen, IStoreableObject)
                If toLoad IsNot Nothing Then
                    If Not Await CheckExistenceAsync(toLoad, con) Then Await CreateNewObjectAsync(toLoad)
                Else : Continue For : End If
                Await SendQueryAsync($"INSERT INTO _enumerablesOfT (Id, ObjName, PropName, RawKey, RawValue)" & vbCrLf &
                                     $"VALUES ({obj.Id}, '{obj.TableName}', '{prop.Name}', {toLoad.Id}, '{toLoad.TableName}');", con)
            Next
        Else
            For x = 0 To generic.Count - 1
                Await SendQueryAsync($"INSERT INTO _enumerablesOfT (Id, ObjName, PropName, RawKey, RawValue)" & vbCrLf &
                                     $"VALUES ({obj.Id}, '{obj.TableName}', '{prop.Name}', {x}, '{ParseSQLDecimal(generic(x))}');", con)
            Next
        End If

    End Function


    Private Async Function CheckObjectExistenceAsync(Of T As {IStoreableObject})(obj As T, con As SqlConnection) As Task(Of Boolean)
        Return (Await SendScalarAsync(Of Integer)($"IF OBJECT_ID('{obj.TableName}') IS NULL SELECT 0" & vbCrLf &
                                                   "ELSE SELECT 1;", con).ConfigureAwait(False)) = 1
    End Function

    Private Function CheckObjectExistence(Of T As {IStoreableObject})(obj As T, con As SqlConnection) As Boolean
        Return SendScalar(Of Integer)($"IF OBJECT_ID('{obj.TableName}') IS NULL SELECT 0" & vbCrLf &
                                        "ELSE SELECT 1;", con) = 1
    End Function

    Private Function ParseType([property] As PropertyInfo, Optional stringLimit As Integer? = Nothing) As String
        Select Case GetNullableTypeName([property].PropertyType)
            Case "UInt64" : Return "DECIMAL(20,0)"
            Case "Int64" : Return "BIGINT"
            Case "UInt32" : Return "DECIMAL(10,0)"
            Case "Int32" : Return "INT"
            Case "UInt16" : Return "DECIMAL(5,0)"
            Case "Int16", "UInt16" : Return "SMALLINT"
            Case "Boolean" : Return "BIT"
            Case "String" : Return $"VARCHAR({GetVarcharLength([property])})"
            Case "Byte" : Return "DECIMAL(3,0)"
            Case "SByte" : Return "TINYINT"
            Case "Decimal" : Return "DECIMAL"
            Case "Double" : Return "FLOAT"
            Case "Single" : Return "REAL"
            Case "DateTime" : Return "DATETIME2"
            Case "DateTimeOffset" : Return "DATETIMEOFFSET"
            Case "TimeSpan" : Return "TIME"
        End Select
        Throw New UnsupportedTypeException
        Return Nothing
    End Function

    Private Function GetSqlValue(Of T As {IStoreableObject})([property] As PropertyInfo, ByRef obj As T) As String
        Dim value = [property].GetValue(obj)
        Dim typeName = ParseType([property])

        Select Case typeName
            Case "DECIMAL(20,0)", "BIGINT", "DECIMAL(10,0)", "SMALLINT", "DECIMAL(5,0)", "INT", "DECIMAL(3,0)", "TINYINT", "DECIMAL", "FLOAT", "REAL" : Return $"{If(ParseSQLDecimal(value), "NULL")}"
            Case "DATETIME2", "TIME", "DATETIMEOFFSET" : Return $"{If(value Is Nothing, "NULL", FixSQLDate(value))}"
            Case $"VARCHAR({GetVarcharLength([property])})" : Return $"'{If(value, "NULL")}'"
            Case "BIT" : Return $"{If(DirectCast(value, Boolean), 1, 0)}"
            Case Else : Throw New UnsupportedTypeException
        End Select
        Return Nothing
    End Function

    Private Function GetVarcharLength([Property] As PropertyInfo) As String
        If [Property].GetCustomAttribute(Of StringLengthAttribute)(True) IsNot Nothing Then
            Dim length = [Property].GetCustomAttribute(Of StringLengthAttribute)(True).Length
            Return If(length = -1, "MAX", $"{length}")
        Else
            Return $"{_stringLimit}"
        End If
    End Function

    Private Sub Log(Of T As {IStoreableObject})(obj As T, logSource As LogType)
        If Not _logEnable Then Return
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write($"[{Date.UtcNow}] [{CenterLog(logSource),-6}] ")
        Console.ResetColor()
        Console.WriteLine($"The object of {obj.TableName} ({obj.Id}) has been {ToPast(logSource)}")
    End Sub

    Private Enum LogType
        Create
        Load
        Update
        Delete
    End Enum

    Private Function CenterLog(logSource As LogType) As String
        Return If(logSource = LogType.Load, " Load ", $"{logSource}")
    End Function

    Private Function ToPast(logSource As LogType) As String
        Return If(logSource = LogType.Load, "Loaded", $"{logSource}d")
    End Function

#End Region
End Class