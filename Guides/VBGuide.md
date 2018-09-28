# How to Use?

1) Add a **Singleton** of SQLExpressClient.

```vb
Function ServiceBuilder() As IServiceProvider
    Return New ServiceCollection().
        AddSingleton(Of SQLExpressClient).
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
