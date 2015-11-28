##Prefs library

Can be used to read or change Sandcat preferences

* **prefs.get** ( key, novalue ): Gets the value of a browser setting. If key is not found, returns the value supplied in the second parameter; otherwise, returns nil. Returns `variant`

* **prefs.getdefault** ( key ): Gets the default value of a browser setting. Returns `variant`

* **prefs.getall( )**: Returns the value of all settings as JSON string. Returns `json string`

* **prefs.getalldefault** (): Returns the default value of all settings as JSON string. Returns `json string`

* **prefs.load** ( json ): Loads the browser settings from a JSON string

* **prefs.regdefault** ( key, value): Registers the default value of a browser setting.

* **prefs.save** (): Saves the browser settings to the disk.

* **prefs.set** ( key, value ): Sets the value of a browser setting.

* **prefs.update** (): Notifies all open tabs about a setting or multiple settings update