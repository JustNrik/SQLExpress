# How to Use?

1) Add a **Singleton** of SQLExpressClient.

```vb
Function ServiceBuilder() As IServiceProvider
    Return New ServiceCollection().
        AddSingleton(New SQLExpressClient).
        ' Others
        BuildServiceProvider()
End Function
```

2) Read the config from an XML or JSON file.

```vb
Sub LoadConfig()
    Dim db = _services.GetService(Of SQLEXpressClient)
    ' JSON
    Dim jString = File.ReadAllText("config.json")
    Dim jObj = jObject.Parse(jString)
    db.ReadConfig(jObj)
    ' XML
    Dim xDoc As New XmlDocument
    xDoc.Load("config.xml")
    db.ReadConfig(xDoc)
End Sub
```
3) Initialise the service.

```vb
Async Function Initialise() As Task
    Dim db = _services.GetService(Of SQLExpressClient)
    ' This is a way
    Dim objs As IStoreableObject = {New Person, New Employee, New Derp}
    Await db.InitialiseObjectsAsync(objs)
    ' You can also load the Cache along with the initialisation
    Await db.LoadObjectCacheAsync(New Person)
    Await db.LoadObjectCacheAsync(New Employee)
    Await db.LoadObjectCacheAsync(New Derp)
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
