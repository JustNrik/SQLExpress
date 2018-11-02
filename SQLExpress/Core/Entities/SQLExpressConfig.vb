''' <summary>
''' This structure is used to provide the configuration the client needs.
''' </summary>
Public Structure SQLExpressConfig
    ''' <summary>
    ''' Defines if you want to use the cache to greatly improve the performance at the cost of some RAM
    ''' </summary>
    ''' <returns></returns>
    Public Property UseCache As Boolean
    ''' <summary>
    ''' Defines if you want events to be raised whenever an object is Created, Loaded, Updated or Removed.
    ''' </summary>
    ''' <returns></returns>
    Public Property Logging As Boolean
    ''' <summary>
    ''' Defines the string limit used for Varchar. Default one is 20, the value must be between 1 and 8000 or -1 if you want to use Varchar(MAX).
    ''' </summary>
    ''' <returns></returns>
    Public Property StringLimit As Integer
    ''' <summary>
    ''' Defines the connection string to log in the database.
    ''' </summary>
    ''' <returns></returns>
    Public Property ConnectionString As String
    ''' <summary>
    ''' Defines the format used for dates.
    ''' </summary>
    ''' <returns></returns>
    Public Property DateFormat As String

    Sub New(connectionString As String, dateFormat As String, Optional stringLimit As Integer = 20)
        Me.ConnectionString = connectionString
        Me.StringLimit = stringLimit
        Me.DateFormat = dateFormat
    End Sub

    Sub New(useCache As Boolean, logging As Boolean, stringLimit As Integer, dateFormat As String, connectionString As String)
        Me.UseCache = useCache
        Me.Logging = logging
        Me.StringLimit = stringLimit
        Me.ConnectionString = connectionString
        Me.DateFormat = dateFormat
    End Sub
End Structure