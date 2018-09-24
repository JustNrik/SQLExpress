<AttributeUsage(AttributeTargets.Property)>
Public NotInheritable Class StoreAttribute
    Inherits Attribute
    Public Property Priority As Integer
    ''' <summary>
    ''' Makes this property to be stored. Priority orders the colums from highest priority to lowest
    ''' </summary>
    ''' <param name="Priority"></param>
    Sub New(Optional priority As Integer = 0)
        Me.Priority = priority
    End Sub
End Class