Public MustInherit Class SQLObject
    Implements IStoreableObject
    ''' <summary>
    ''' Sets the Id of the object. Used as a Primary Key for querying.
    ''' </summary>
    ''' <returns></returns>
    <NotNull>
    <PrimaryKey>
    <Store(255)>
    Public Property Id As ULong Implements IStoreableObject.Id
    ''' <summary>
    ''' Sets the name of the object. This name is used to create a table in the database.
    ''' </summary>
    ''' <returns></returns>
    Public MustOverride ReadOnly Property TableName As String Implements IStoreableObject.TableName

    Sub New(Id As ULong)
        Me.Id = Id
    End Sub

    Sub New()
    End Sub

End Class