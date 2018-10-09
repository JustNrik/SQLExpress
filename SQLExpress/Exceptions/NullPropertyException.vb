''' <summary>
''' This Exception is throw when your property flagged with NotNullAttribute has a Null value.
''' </summary>
Public Class NullPropertyException
        Inherits Exception

    Private Const DEFAULT_EXCEPTION_MESSAGE = "The object has a property with NotNull attribute, but the value of that property is Null"

    Sub New()
        MyBase.New(DEFAULT_EXCEPTION_MESSAGE)
    End Sub
End Class
