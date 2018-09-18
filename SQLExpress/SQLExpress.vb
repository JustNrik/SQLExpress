Option Compare Text

Imports Newtonsoft.Json.Linq
Imports System.Collections.Concurrent
Imports System.Data.SqlClient
Imports System.Reflection
Imports System.Text
Imports System.Xml
Public NotInheritable Class SQLExpress
    Implements IDisposable

    Public Property Cache As New ConcurrentDictionary(Of ULong, SQLObject)

    Private _usingService As Boolean
    Private _connectionString As String
    Private _database As String
    Private _sqlServer As String
    Private _username As String
    Private _password As String
    Private _stringLimit As Integer
    ''' <summary>
    ''' Sets the database where the data will be gathered.
    ''' </summary>
    Public WriteOnly Property Database As String
        Set(value As String)
            _database = value
        End Set
    End Property
    ''' <summary>
    ''' Sets the instance where it will be connected.
    ''' </summary>
    Public WriteOnly Property SQLServer As String
        Set(value As String)
            _sqlServer = value
        End Set
    End Property
    ''' <summary>
    ''' Sets the username required to log in the database
    ''' </summary>
    Public WriteOnly Property Username As String
        Set(value As String)
            _username = value
        End Set
    End Property
    ''' <summary>
    ''' Sets the password required to log in the database
    ''' </summary>
    Public WriteOnly Property Password As String
        Set(value As String)
            _password = value
        End Set
    End Property
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
    ''' Manually loads the config from an XML Document. You must load the XMlDocument before using this method.
    ''' </summary>
    ''' <param name="xmlConfig"></param>
    Sub New(xmlConfig As XmlDocument, Optional StringLimit As Integer = 20)
        ReadConfig(xmlConfig)
        _stringLimit = StringLimit
    End Sub
    ''' <summary>
    ''' Manually loads the config from a JSON file. You must parse the JObject before using this method.
    ''' </summary>
    ''' <param name="jConfig"></param>
    Sub New(jConfig As JObject, Optional StringLimit As Integer = 20)
        ReadConfig(jConfig)
        _stringLimit = StringLimit
    End Sub
    ''' <summary>
    ''' Reads the SQL Configuration from an XML Document. Remember to load the document before executing this method.
    ''' </summary>
    ''' <param name="xmlConfig"></param>
    Public Sub ReadConfig(xmlConfig As XmlDocument)
        _database = $"{xmlConfig("database")}"
        _sqlServer = $"{xmlConfig("server")}"
        _username = $"{xmlConfig("username")}"
        _password = $"{xmlConfig("password")}"
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
    ''' <summary>
    ''' This is the Object initialisation, it will automatically create the objects that don't exists.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="objs"></param>
    ''' <returns></returns>
    Public Async Function SetupObjects(Of T As {SQLObject})(ParamArray objs As T()) As Task
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync()
            For Each obj In objs
                Dim result As Integer
                Using cmd As New SqlCommand($"IF OBJECT_ID('{obj.Name}') IS NULL" & vbCrLf &
                                             "    SELECT 0" & vbCrLf &
                                             "ELSE" & vbCrLf &
                                             "    SELECT 1", con)
                    result = DirectCast(Await cmd.ExecuteScalarAsync, Integer)
                End Using
                Select Case result
                    Case 0
                        Dim sb As New StringBuilder
                        With sb
                            .AppendLine($"CREATE TABLE {obj.Name} (")
                            Dim properties = obj.GetType.GetProperties.
                                Where(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True) IsNot Nothing).
                                OrderByDescending(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True).Priority)
                            If properties.Count = 0 Then Throw New EmptyObjectException
                            For Each [Property] In properties
                                .Append($"{[Property].Name} {ParseType([Property].PropertyType.Name)} ")
                                .Append($"{If([Property].GetCustomAttribute(Of NotNullAttribute)(True) IsNot Nothing, "NOT NULL", "")} ")
                                .Append($"{If([Property].GetCustomAttribute(Of PrimaryKeyAttribute)(True) IsNot Nothing, "PRIMARY KEY", "")}")
                                If [Property] Is properties.Last() Then .AppendLine(")") Else .AppendLine(",")
                            Next
                        End With
                        Using newCmd As New SqlCommand($"{sb}", con)
                            Await newCmd.ExecuteNonQueryAsync
                        End Using
                    Case 1 : Continue For
                End Select
            Next
        End Using
    End Function
    ''' <summary>
    ''' Adds a new Object to the database. Throws when fail.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="obj"></param>
    ''' <returns></returns>
    Public Async Function NewObject(Of T As {SQLObject})(obj As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync()
            Dim properties = obj.GetType.GetProperties.
                Where(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True) IsNot Nothing).
                OrderBy(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True).Priority)
            If properties.Count = 0 Then Throw New EmptyObjectException
            Using cmd As New SqlCommand($"INSERT INTO {obj.Name} ({properties.Select(Function(x) x.Name).Aggregate(Function(x, y) x & ", " & y)})" &
                                        $"VALUES ({properties.Select(Function(x) $"{GetSqlValue(x, obj)}").Aggregate(Function(x, y) x & ", " & y)})", con)
                Await cmd.ExecuteNonQueryAsync
                Return obj
            End Using
        End Using
    End Function
    ''' <summary>
    ''' Loads the Object, creates a new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toLoad"></param>
    ''' <returns></returns>
    Public Async Function LoadObject(Of T As {New, SQLObject})(toLoad As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            Dim obj As New T
            Dim result As Integer
            Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {toLoad.Name} WHERE Id = {toLoad.Id}", con)
                result = DirectCast(Await cmd.ExecuteScalarAsync, Integer)
            End Using
            If result = 0 Then Return Await NewObject(toLoad)
            Dim propertyNames = obj.GetType.GetProperties.
                Where(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True) IsNot Nothing).
                OrderBy(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True).Priority).
                Select(Function(x) x.Name)
            If propertyNames.Count = 0 Then Throw New EmptyObjectException
            Using cmd As New SqlCommand($"SELECT* FROM {obj.Name} WHERE Id = {obj.Id}", con)
                Using r = Await cmd.ExecuteReaderAsync
                    While Await r.ReadAsync
                        For x = 0 To r.FieldCount - 1
                            obj.GetType.GetProperty(propertyNames(x)).SetValue(obj, r.Item(x))
                        Next
                    End While
                    Return obj
                End Using
            End Using
        End Using
    End Function
    ''' <summary>
    ''' Sabes the Object, createsa new one if it doesn't exist.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="toSave"></param>
    ''' <returns></returns>
    Public Async Function SaveObject(Of T As {New, SQLObject})(toSave As T) As Task(Of T)
        Using con As New SqlConnection(_connectionString) : Await con.OpenAsync
            Dim result As Integer
            Dim obj As New T
            Using cmd As New SqlCommand($"SELECT COUNT(Id) FROM {toSave.Name} WHERE Id = {toSave.Id}", con)
                result = DirectCast(Await cmd.ExecuteScalarAsync, Integer)
            End Using
            If result = 0 Then Return Await NewObject(toSave)
            Dim properties = obj.GetType.GetProperties.
                Where(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True) IsNot Nothing).
                OrderBy(Function(x) x.GetCustomAttribute(Of PriorityAttribute)(True).Priority)
            If properties.Count = 0 Then Throw New EmptyObjectException
            Dim sb As New StringBuilder
            With sb
                .AppendLine($"UPDATE {obj.Name}")
                For Each [Property] In properties
                    .AppendLine($"SET {[Property].Name} = {[Property].GetValue(obj)}")
                Next
                .AppendLine($"WHERE Id = {obj.Id}")
            End With
            Using cmd As New SqlCommand($"{sb}", con)
                Await cmd.ExecuteNonQueryAsync
            End Using
            Return obj
        End Using
    End Function

    Private Function ParseType(typeName As String) As String
        With typeName
            Select Case True
                Case .Contains("Int64") : Return "BIGINT"
                Case .Contains("Int32") : Return "INT"
                Case .Contains("Int16") : Return "SMALLINT"
                Case .Contains("Byte") : Return "TINYINT"
                Case .Contains("Decimal") : Return "DECIMAL"
                Case .Contains("Double") : Return "FLOAT"
                Case .Contains("Single") : Return "REAL"
                Case .Contains("DateTime") : Return "DATE"
                Case .Contains("TimeSpan") : Return "TIME"
                Case .Contains("DateTimeOffset") : Return "DATETIMEOFFSET"
                Case .Contains("String") : Return $"VARCHAR({_stringLimit})"
            End Select
        End With
        Throw New UnsupportedTypeException
        Return ""
    End Function

    Private Function GetSqlValue(Of T As {SQLObject})([Property] As PropertyInfo, ByRef obj As T) As String
        Dim value = [Property].GetValue(obj)
        Dim typeName = ParseType([Property].PropertyType.Name)
        Select Case typeName
            Case "BIGINT", "SMALLINT", "INT", "TINYINT", "DECIMAL", "FLOAT", "REAL" : Return $"{value}"
            Case "DATE", "TIME", "DATETIMEOFFSET", $"VARCHAR({_stringLimit})" : Return $"'{value}'"
            Case Else : Throw New UnsupportedTypeException
        End Select
        Return ""
    End Function

    Public Sub Dispose() Implements IDisposable.Dispose
        Finalize()
    End Sub
End Class