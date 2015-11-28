##ExtensionPack Object

Reads Sandcat Extension (.scx) pack files

###Creation

* **expansionpack:new** (): Creates and returns a new extension pack object.

###Methods

* **dofile** ( scriptfilename ): Loads and runs a Lua script that is inside the extension package
* **fileexists** ( textfilename ): Returns true if a file is present inside the extension package. Otherwise, returns false. Returns `boolean`
* **getfile** ( textfilename ): Gets the contents of a text file that is inside the extension package. Returns `string`
* **require** ( modulename ): Loads and returns the Lua module that is inside the extension package. Similar to Lua's require().
* **release** (): Frees the extension pack object

###Properties
name | type | description
--- | --- | ---
**filename** | string | Gets or sets the extension pack filename (eg: MyPack.scx)