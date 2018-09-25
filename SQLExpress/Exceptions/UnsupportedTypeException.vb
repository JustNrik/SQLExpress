Namespace Exceptions
    Public Class UnsupportedTypeException
        Inherits Exception

        Private Const _defaul = "The object has a property with an unsupported type"
        Sub New()
            MyBase.New(_defaul)
        End Sub
    End Class
End Namespace