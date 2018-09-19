<AttributeUsage(AttributeTargets.Property)>
Public Class StringLengthAttribute
    Inherits Attribute

    Public Property Length As Byte
    Sub New(length As Byte)
        Me.Length = length
    End Sub
End Class
