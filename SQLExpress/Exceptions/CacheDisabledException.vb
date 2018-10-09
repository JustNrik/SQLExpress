''' <summary>
''' This Exception is thrown when the object doesn't have any property with Store Attribute.
''' </summary>
Public Class CacheDisabledException
    Inherits Exception

    Private Const DEFAULT_EXCEPTION_MESSAGE = "Cache disabled. You must manually add/remove/remove all entries in the cache."

    Sub New()
        MyBase.New(DEFAULT_EXCEPTION_MESSAGE)
    End Sub
End Class