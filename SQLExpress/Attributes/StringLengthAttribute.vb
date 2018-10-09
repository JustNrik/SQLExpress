''' <summary>
''' Flags the String property to be stored with the defined length. Use -1 for MAX value.
''' </summary>
<AttributeUsage(AttributeTargets.Property)>
Public Class StringLengthAttribute
    Inherits Attribute

    ''' <summary>
    ''' Sets the Length of the string that will be stored. The maximun is 8000. Use -1 for MAX value
    ''' </summary>
    ''' <returns></returns>
    Public Property Length As Integer

    Sub New(length As Integer)
        Select Case length
            Case -1, 1 To 8000 : Me.Length = length
            Case Else : Throw New ArgumentException("Length must be between 1 and 8000, or -1 if you want to use MAX length instead")
        End Select
    End Sub
End Class