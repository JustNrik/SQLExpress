''' <summary>
''' This Exception is throw when your property flagged with NotNullAttribute has a Null value.
''' </summary>
Public Class NullPropertyException
        Inherits Exception

        Private Const _defaul = "The object has a property with NotNull attribute, but the value of that property is Null"
        Sub New()
            MyBase.New(_defaul)
        End Sub
    End Class
