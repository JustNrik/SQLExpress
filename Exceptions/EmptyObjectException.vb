﻿''' <summary>
''' This Exception is thrown when the object doesn't have any property with Store Attribute.
''' </summary>
Public Class EmptyObjectException
    Inherits Exception

    Private Const _defaul = "The object doesn't have any property with Store Attribute"
    Sub New()
        MyBase.New(_defaul)
    End Sub
End Class