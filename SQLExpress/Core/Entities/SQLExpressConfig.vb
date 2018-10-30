''' <summary>
''' This class is used to provide the configuration the client needs.
''' </summary>
Public Structure SQLExpressConfig
    Public Property UseCache As Boolean
    Public Property Logging As Boolean
    Public Property StringLimit As Integer
    Public Property ConnectionString As String
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