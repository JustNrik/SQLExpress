# How to Use?

1) Add a **Singleton** of SQLExpressClient.

```vb
Function ServiceBuilder() As IServiceProvider
    ' JSON
    Dim jString = File.ReadAllText("config.json")
    Dim jObj = jObject.Parse(jString)
    ' XML
    Dim xDoc As New XmlDocument
    xDoc.Load("config.xml")
    Return New ServiceCollection().
        AddSingleton(New SQLExpressClient(New SQLExpressConfig(YourConnectionString))).
        ' AddSingleton(New SQLExpressClient(xDoc))
        ' AddSingleton(New SQLExpressClient(jObj))
        ' Any will work
        BuildServiceProvider()
End Function
```

2) Initialise the service.

```vb
Async Function Initialise() As Task
    Dim db = _services.GetService(Of SQLExpressClient)
    ' This is a way
    Dim objs As IStoreableObject = {New Person, New Employee, New Derp}
    Await db.InitialiseObjectsAsync(objs)
    ' You can also load the Cache along with the initialisation
    Await db.LoadObjectsCacheAsync(objs) ' This will throw if you disable cache
End Function
```

# How do I make an Object?

You must create a `Class` that inherits from `SQLObject` or implement `IStoreableObject` interface and add `Store` Attribute on the properties you want to store.  Optionally, you can provide an index to Store, this will affect the order they are organized. It doesn't really matter, just for organization. If you implement `IStoreableObject` it is **strongly** suggested to flag ID property with `PrimaryKey` attribute.

```vb
Public Class Person
    Inherits SQLObject

    Public Overrides ReadOnly Property TableName As String 
        Get
            Return "persons"
        End Get
    End Property
    <Store(1)>
    Public Property Address As String
    <Store(2)>
    Public Property TelephoneNumber As String
    
    Sub New()
    End Sub
    
    Sub New(id As ULong)
        MyBase.New(id)
    End Sub
End Class

Public Structure Employee 
    Implements IStoreableObject
    
    Public ReadOnly Property TableName As String Implements IStoreableObject.TableName
        Get
            Return "employees"
        End Get
    End Property
    <Store(255)>
    <PrimaryKey>
    <NotNull>
    Public Property Id As ULong Implements IStoreableObject.Id
    <Store>
    Public Property Salary As Decimal
    <Store>
    Public Property HoursOfWork As Integer
    
    Sub New(Id As ULong)
        _id = Id
    End Sub
End Structure
```
