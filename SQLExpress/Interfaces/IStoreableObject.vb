Public Interface IStoreableObject
    ''' <summary>
    ''' Sets the Id of the object. Used as a Primary Key for querying.
    ''' </summary>
    ''' <returns></returns>
    Property Id As ULong
    ''' <summary>
    ''' Sets the name of the object. This name is used to create a table in the database.
    ''' </summary>
    ''' <returns></returns>
    ReadOnly Property Name As String
End Interface
