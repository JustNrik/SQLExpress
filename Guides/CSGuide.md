# How to Use

1) Add a **Singleton** of SQLExpressClient

```cs
IServiceProvider ServiceBuilder()
    => new ServiceCollection().
        AddSingleton<SQLExpressClient>().
        // Others
        BuildServiceProvider();
```

2) Read the config from a XML or JSON file

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
    await dbo.LoadObjectCacheAsync(new Person());
    await dbo.LoadObjectCacheAsync(new Employee());
    await dbo.LoadObjectCacheAsync(new Derp());
}
```

# How do I make an Object?

You must create a `Class` that inherits from `SQLObject` or implement `IStoreableObject` interface and add `Store` Attribute on the properties you want to store.  Optionally, you can provide an index to Store, this will affect the order they are organized. It doesn't really matter, just for organization. If you implement `IStoreableObject` it is **strongly** suggested to flag ID property with `PrimaryKey` attribute.

```cs
public class Person : SQLObject
{
    public override readonly string TableName => "persons";
    [Store(1)]
    public string Address { get; set; }
    [Store(2)]
    public string TelephoneNumber { get; set; }
    
    void Person() { }
    void Person(ulong id) : base(id) { }
}

public struct Employee : IStoreableObject
{
    public readonly string TableName => "employees";
    [Store(int.MaxValue)]
    [PrimaryKey]
    [NotNull]
    public ulong Id { get; set; }
    [Store]
    public decimal Salary { get; set; }
    [Store]
    public int HoursOfWork { get; set; }
    
    void Employee(ulong id)
        => Id = id;
}
```