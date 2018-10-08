''' <summary>
''' This class is used to provide the configuration the client needs.
''' </summary>
Public NotInheritable Class SQLExpressConfig
    Public Property UseCache As Boolean
    Public Property Logging As Boolean
    Public Property StringLimit As Integer
    Public Property ConnectionString As String

    Sub New()
        StringLimit = 20
    End Sub

    Sub New(connectionString As String)
        StringLimit = 20
        Me.ConnectionString = connectionString
    End Sub

    Sub New(stringLimit As Integer, connectionString As String)
        Me.StringLimit = stringLimit
        Me.ConnectionString = connectionString
    End Sub

    Sub New(useCache As Boolean, logging As Boolean, stringLimit As Integer, connectionString As String)
        Me.UseCache = useCache
        Me.Logging = logging
        Me.StringLimit = stringLimit
        Me.ConnectionString = connectionString
    End Sub
End Class