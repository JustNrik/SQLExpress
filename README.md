# SQLExpress

This is an Object Oriented SQL Wrapper to Save/Load data from objects.

Available on NuGet.

`Install-Package SQLExpress -Version 1.2.6`

# Attributes

`NotNull`: Prevents Null data to be stored. Useful to keep the integrity of the database.

`PrimaryKey`: Sets the primary key of the Table that references your object. If you inherit from `SQLObject`, the primary key is already set to be Id property. If you implement `IStoreableObject`, you will have to add the attribute to the Id manually. Also, keep in mind Primary Keys cannot be Null so you should add `NotNull` attribute too.

`Store`: This is the filter attribute to store properties in the database. Optionally, you can set a priority. Only for organization though.

`StringLength`: Sets the maximun Length of a string (VARCHAR) to be stored in the database. The maximun length you can set is 8000. You can also use -1 to store it as VARCHAR(MAX).

# Exceptions

`EmptyObject`: This exception will be thrown if the object doesn't have any property with Store attribute.

`NullProperty`: This exception will be thrown if a property with NotNull attribute has a Null value.

`UnsupportedTypeException`: This exception will be thrown if you add Store attribute to a property whose property type is unsupported. Examples: `Object`, `Func<T>`, `Stack`, etc...

`CacheDisabledException`: This exception will be thrown if you attempt to load objects to the cache if it was disabled in your configuration.

# Methods

`InitialiseObjects`: Checks the existence of the provided objects, if they don't exist, their tables will be created in the database.

`InstallDatabase`: For now, it only adds the table required to store `List<T>`, `IEnumerable<T>`, `IDictionary<TKey, TValue>`, etc...

`LoadObjectCache`: Loads the Cache for the specified object.

`LoadObjectsCache`: Loads the Cache for the group of objects.

`CreateNewObject`: Creates a new Object in the database. LoadObject and UpdateObject will automatically create objects that don't exist so you don't actually need this at all.

`LoadObject`: Loads the Object from the database, creates a new one if it doesn't exists.

`UpdateObject`: Deletes and Re-add the Object in the database (it will be reworked later), creates a new Object if it doesn't exist.

`RemoveObject`: Removes the Object from the database.

`SendQuery`: Executes a NonQuery in the database. (Ironic, yeah)

`SendScalar`: Executes a Scalar and returns the first value.

`YieldData`: Executes a Reader to yield an IEnumerable of the data retrieved. Only returns the first **column**.

`CheckExistence`: Checks if a Table exists.
