Friend Structure Dummy
    Implements IStoreableObject

    <Store(255)>
    <NotNull>
    <PrimaryKey>
    Public Property Id As ULong Implements IStoreableObject.Id

    Public ReadOnly Property TableName As String Implements IStoreableObject.TableName

    Sub New(id As ULong, tableName As String)
        Me.Id = id
        Me.TableName = tableName
    End Sub

End Structure