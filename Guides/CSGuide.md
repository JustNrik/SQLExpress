# How to Use

1) Add a **Singleton** of SQLExpressClient

```cs
IServiceProvider ServiceBuilder()
    => new ServiceCollection().
    AddSingleton(new SQLExpressClient(New SQLExpressConfig(YourConnectionString))).
    // Others
    BuildServiceProvider();
```

2) Read the config from a XML or JSON file (You can also provide it directly to the client ctor)

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

3) Initialise the service

```cs
async Task Initialise()
{
    var dbo = _services.GetService<SQLExpressClient>();
    // This is a way
    IStoreableObject[] objs = new[] { new Person(), new Employee(), new Derp() };
    await dbo.InitialiseObjectsAsync(objs);
    // You can also load the Cache along with the initialisation
    await dbo.LoadObjectsCacheAsync(objs); // This will throw if you disable cache
}
```

# How do I make an Object?

You must create a `Class` that inherits from `SQLObject` or implement `IStoreableObject` interface and add `Store` Attribute on the properties you want to store.  Optionally, you can provide an index to Store, this will affect the order they are organized. It doesn't really matter, just for organization. If you implement `IStoreableObject` it is **strongly** suggested to flag ID property with `PrimaryKey` attribute.

```cs
public class Person : SQLObject
{
    public override string TableName => "persons";
    [Store(1)]
    public string Address { get; set; }
    [Store(2)]
    public string TelephoneNumber { get; set; }
    
    public Person() { }
    public Person(ulong id) : base(id) { }
}

public struct Employee : IStoreableObject
{
    public string TableName => "employees";
    [Store(255)]
    [PrimaryKey]
    [NotNull]
    public ulong Id { get; set; }
    [Store]
    public decimal Salary { get; set; }
    [Store]
    public int HoursOfWork { get; set; }
    
    public Employee(ulong id)
        => Id = id;
}
```
