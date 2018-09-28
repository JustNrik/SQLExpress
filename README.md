# SQLExpress

This is an Object Oriented SQL Wrapper to Save/Load data from objects.

Available on NuGet.

# Attributes

`NotNull`: Prevents Null data to be stored. Useful to keep the integrity of the database.

`PrimaryKey`: Sets the primary key of the Table that references your object. If you inherit from `SQLObject`, the primary key is already set to be Id property. If you implement `IStoreableObject`, you will have to add the attribute to the Id manually. Also, keep in mind Primary Keys cannot be Null so you should add `NotNull` attribute too.

`Store`: This is the filter attribute to store properties in the database. Optionally, you can set a priority. Only for organization though.

`StringLength`: Sets the maximun Length of a string (VARCHAR) to be stored in the database. The maximun length you can set is 8000. You can also use -1 to store it as VARCHAR(MAX).

# Exceptions

`EmptyObject`: This exception will be thrown if the object doesn't have any property with Store attribute.

`NullProperty`: This exception will be thrown if a property with NotNull attribute has a Null value.

`UnsupportedTypeException`: This exception will be thrown if you add Store attribute to a property whose property type is unsupported. Examples: `Object`, `Func<T>`, `Stack`, etc...

# Methods

`InitialiseObjects`: Checks the existence of the provided objects, if they don't exist, their tables will be created in the database.

`InstallDatabase`: For now, it only adds the table required to store `List<T>`, `IEnumerable<T>`, `IDictionary<TKey, TValue>`, etc...

`LoadObjectCache`: Loads the Cache for the specified object.
