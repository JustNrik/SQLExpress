''' <summary>
''' This class is used to provide the configuration the client needs.
''' </summary>
Public NotInheritable Class SQLExpressConfig
    Public Property UseCache As Boolean
    Public Property Logging As Boolean
    Public Property StringLimit As Integer
    Public Property ConnectionString As String

    Sub New(connectionString As String, Optional stringLimit As Integer = 20)
        Me.ConnectionString = connectionString
        Me.StringLimit = stringLimit
    End Sub

    Sub New(useCache As Boolean, logging As Boolean, stringLimit As Integer, connectionString As String)
        Me.UseCache = useCache
        Me.Logging = logging
        Me.StringLimit = stringLimit
        Me.ConnectionString = connectionString
    End Sub
End Class