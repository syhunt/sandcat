##Task Object

Sandcat can launch Lua scripts to perform operations in an isolated process. The tasks can be monitored and managed from the Tasks page.

###Methods

Methods available from within an isolated process of a Sandcat Task:

* **task:browserdostring** ( luacode ): Runs a Lua script in the browser process.

* **task:dopackfile** ( pakfilename, scriptfilename): Loads and runs a Lua script that is inside an extension package

* **task:logrequest** ( json ) Can be used to manually log a request from a custom HTTP client class to the Live Headers list. Accepts the same keys as the tab:logrequest() method.

* **task:getpackfile** ( pakfilename, filename): Gets the contents of a file that is inside the specified extension package. Returns `string`

* **task:sendrequest** ( json ): Performs a custom HTTP request in the tab that launched the task and adds the request to the Live Headers list. Accepts the same keys as the tab:loadrequest() method, but here they must be provided as a JSON object.

* **task:setprogress** ( position [, max] ): Updates the progress bar from the task progress monitor panel. If the second parameter is omitted, 100 is used as maximum value

* **task:setscript** ( event, luacode ): Sets a Lua code to be executed during specific task progress monitor panel events (eg: onclick, ondblclick, onstop)

* **task:showmessage** ( s ): Displays a message to the user

* **task:stop** ( reason ): Manually stops the task. Sandcat will automatically call this method at the end of the task execution (if the method has not been called before).

###Properties

name | type | description
--- | --- | ---
caption	| string | Gets or sets the task caption
id | string | Gets the unique ID of the task
pid | string | Gets the process ID of the task
status | string | Gets or sets the task status text

###Additional Functions

In addition to the functions above, all functions above and all functions from the Catarinka library are also available from within a Sandcat Task.

* **getappdir** (): Returns the Sandcat installation directory. Returns `string`
* **parambool** ( key, defaultvalue ): Returns the value of a parameter key as a boolean value. If the supplied key is not found and a default value has been supplied, returns the default value. Returns `boolean`
* **paramint** ( key, defaultvalue ): Returns the value of a parameter key as an integer value. If the supplied key is not found and a default value has been supplied, returns the default value. Returns `integer`
* **paramstr** ( key, defaultvalue ): Returns the value of a parameter key as a string. If the supplied key is not found and a default value has been supplied, returns the default value. Returns `string`
* **print** ( v ): Prints a line to the task log
* **printfailure** ( v ): Prints a line to the task log and paints the task progress monitor panel in red color
* **printsuccess** ( v ): Prints a line to the task log and paints the task progress monitor panel in green color