''' <summary>
''' Makes this property to be stored. Priority orders the colums from highest priority to lowest
''' </summary>
<AttributeUsage(AttributeTargets.Property)>
    Public NotInheritable Class StoreAttribute
        Inherits Attribute
    ''' <summary>
    ''' Sets the priority of the property. Ordered by descending.
    ''' </summary>
    ''' <returns></returns>
    Public Property Priority As Integer

    Sub New(Optional priority As Integer = 0)
            Me.Priority = priority
        End Sub
    End Class
