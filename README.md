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
    await dbo.SetupObjects(objs);
    // This is another way
    await dbo.SetupObjects<GuildObject>(new Person(), new Employee(), new Derp());
}
```
```vb
Async Function Initialise() As Task
    Dim dbo = _services.GetService(Of SQLExpressClient)
    ' This is a way
    Dim objs = New SQLObject() {New Person, New Employee, New Derp}
    Await dbo.SetupObjects(objs)
    ' This is another way
    Await dbo.SetupObjects(Of GuildObject)(New Person, New Employee, New Derp)
End Function
```

And you're done. You can Save, Load or Create new objects as you please. They will be automaticaly stored in the database.

**Note**: All methods are Asynchronous. They are: CreateNewObjectAsync, LoadObjectAsync And SaveObjectAsync.

# How do I make an Object?

You must create a `Class` that inherits from `SQLObject` and add `Priority` Attribute on the properties you want to store. 

```cs
public class Person : SQLObject
{
    public overrides string Name { get; } => "persons";
    [Priority(1)]
    public string Address { get; set; }
    [Priority(2)]
    public string TelephoneNumber { get; set; }
    
    void Person() { }
    void Person(ulong id) : base(id) { }
}
```
```vb
Public Class Person
    Inherits SQLObject

    Private Overrides ReadOnly Property Name As String
    Get
        Return "persons"
    End Get
    <Priority(1)>
    Public Property Address As String
    <Priority(2)>
    Public Property TelephoneNumber As String

    Sub New()
    End Sub

    Sub New(id As ULong)
        MyBase.New(id)
    End Sub
End Class
```
You can also user `NotNull` Attribute to prevent null data to be stored. Keep in mind that it will throw if you attempt to Save null data.
