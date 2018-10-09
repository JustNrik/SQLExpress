''' <summary>
''' This Exception is thrown when the object doesn't have any property with Store Attribute.
''' </summary>
Public Class EmptyObjectException
    Inherits Exception

    Private Const DEFAULT_EXCEPTION_MESSAGE = "The object doesn't have any property with Store Attribute"

    Sub New()
        MyBase.New(DEFAULT_EXCEPTION_MESSAGE)
    End Sub
End Class