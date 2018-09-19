Public MustInherit Class SQLObject
    ''' <summary>
    ''' Sets the Id of the object. Used as a Primary Key for querying.
    ''' </summary>
    ''' <returns></returns>
    <NotNull>
    <PrimaryKey>
    <Priority(Integer.MaxValue)>
    Public Property Id As ULong
    ''' <summary>
    ''' Sets the name of the object. This name is used to create a table in the database.
    ''' </summary>
    ''' <returns></returns>
    Public MustOverride ReadOnly Property Name As String

    Sub New(Id As ULong)
        Me.Id = Id
    End Sub

    Sub New()
    End Sub

End Class