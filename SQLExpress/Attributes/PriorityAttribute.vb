<AttributeUsage(AttributeTargets.Property)>
Public NotInheritable Class PriorityAttribute
    Inherits Attribute
    Public Property Priority As Integer
    ''' <summary>
    ''' Makes this property to be stored. Priority orders the colums from highest priority to lowest
    ''' </summary>
    ''' <param name="Priority"></param>
    Sub New(Priority As Integer)
        Me.Priority = Priority
    End Sub
End Class
