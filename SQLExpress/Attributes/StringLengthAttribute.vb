Option Compare Text
Namespace Attributes
    <AttributeUsage(AttributeTargets.Property)>
    Public Class StringLengthAttribute
        Inherits Attribute

        Public Property Length As Integer
        Sub New(length As Integer)
            Select Case length
                Case 1 To 8000 : Me.Length = length
                Case -1 : Me.Length = length
                Case Else : Throw New ArgumentException("Length must be between 1 and 8000, or -1 if you want to use MAX length instead")
            End Select
        End Sub
    End Class
End Namespace