﻿Public Class EmptyObjectException
    Inherits Exception
    Private Const _defaul = "The object doesn't have any property with Priority Attribute"
    Sub New()
        MyBase.New(_defaul)
    End Sub
End Class