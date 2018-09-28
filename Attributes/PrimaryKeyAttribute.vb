''' <summary>
''' Flags the property as a Primary Key, don't use this if you inherit from SQLObject.
''' </summary>
<AttributeUsage(AttributeTargets.Property)>
Public NotInheritable Class PrimaryKeyAttribute
    Inherits Attribute

End Class