''' <summary>
''' This Exception is thrown when you attempt to store an object with an unsupported type.
''' </summary>
Public Class UnsupportedTypeException
        Inherits Exception

    Private Const DEFAULT_EXCEPTION_MESSAGE = "The object has a property with an unsupported type"
    Sub New()
        MyBase.New(DEFAULT_EXCEPTION_MESSAGE)
    End Sub
End Class