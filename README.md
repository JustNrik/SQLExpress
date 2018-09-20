# SQLExpress

This is an Object Oriented SQL Wrapper to Save/Load data from objects.

Available on NuGet.

# How to Use?

1) Not mandatory but **strongly** suggested to add a Singleton of SQLExpressClient.

```cs
IServiceProvider ServiceBuilder()
    => new ServiceCollection().
        AddSingleton<SQLExpressClient>().
        // Others
        BuildServiceProvider();
```
```vb
Function ServiceBuilder() AS IServiceProvider
    Return New ServiceCollection().
        AddSingleton(Of SQLExpressClient).
        ' Others
        BuildServiceProvider()
End Function
```
2) Load the config from a .JSON or .XML file

```cs
void LoadConfig()
{
    // JSON
    var jString = File.ReadAllText("config.json");
    var jObj = jObject.Parse(jString);
    dbo.ReadConfig(jObj);
    // XML
    var xDoc = new XmlDocument();
    xDoc.Load("config.xml");
    dbo.ReadConfig(xDoc);
}
```
```vb
Sub LoadConfig()
    ' JSON
    Dim jString = File.ReadAllText("config.json")
    Dim jObj = jObject.Parse(jString)
    dbo.ReadConfig(jObj)
    ' XML
    Dim xDoc As New XmlDocument
    xDoc.Load("config.xml")
    dbo.ReadConfig(xDoc)
End Sub
```

3) Set it up adding all your objects.

```cs
async Task Initialise()
{
    var dbo = _services.GetService<SQLExpressClient>();
    // This is a way
    var objs = new SQLObject[] { new Person(), new Employee(), new Derp() };
    await dbo.InitialiseObjectsAsync(objs);
    // This is another way
    await dbo.InitialiseObjectsAsync<GuildObject>(new Person(), new Employee(), new Derp());
    // You can also load the Cache along with the initialisation
    await dbo.LoadObjectCacheAsync(New Person());
    await dbo.LoadObjectCacheAsync(new Employee());
    await dbo.LoadObjectCacheAsync(new Derp());
}
```
```vb
Async Function Initialise() As Task
    Dim dbo = _services.GetService(Of SQLExpressClient)
    ' This is a way
    Dim objs = New SQLObject() {New Person, New Employee, New Derp}
    Await dbo.InitialiseObjectsAsync(objs)
    ' This is another way
    Await dbo.InitialiseObjectsAsync(Of GuildObject)(New Person, New Employee, New Derp)
    ' You can also load the Cache along with the initialisation
    Await dbo.LoadObjectCacheAsync(New Person)
    Await dbo.LoadObjectCacheAsync(New Employee)
    Await dbo.LoadObjectCacheAsync(New Derp)
End Function
```

And you're done. You can Save, Load or Create new objects as you please. They will be automaticaly stored in the database.

# How do I make an Object?

You must create a `Class` that inherits from `SQLObject` and add `Store` Attribute on the properties you want to store.  Optionally, you can provide an index to Store, this will affect the order they are organized. It doesn't really matter, just for organization.

```cs
public class Person : SQLObject
{
    public override string Name { get; } => "persons";
    [Store(1)]
    public string Address { get; set; }
    [Store(2)]
    public string TelephoneNumber { get; set; }
    
    void Person() { }
    void Person(ulong id) : base(id) { }
}
```
```vb
Public Class Person
    Inherits SQLObject

    Public Overrides ReadOnly Property Name As String
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
```
You can also user `NotNull` Attribute to prevent null data to be stored. Keep in mind that it will throw if you attempt to Save null data.

**Note**: The `Name` of the object will be the name of the Table that will be stored in the database.
