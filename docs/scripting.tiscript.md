##TIScript

TIScript is the scripting engine used by Sandcat for some of its user interface operations. TIScript uses JavaScript as a base with some Python features added: classes and namespaces, properties, decorators, etc.

More details can be found at:
http://code.google.com/p/tiscript/wiki/LanguageAndRuntime

### Custom TIScript functions available in Sandcat

Sandcat TIS Object:

* **Sandcat.Debug** ( s ): Outputs a debug string
* **Sandcat.GoToURL** ( url [, newtab] ): Goes to the supplied URL. The second parameter is optional. If supplied and is true, opens the URL in a new tab
* **Sandcat.PrefsSet** ( key, value ): Sets the value of a browser setting
* **Sandcat.ShowMessage** ( s ): Shows a message to the user
* **Sandcat.RunLua** ( luacode ): Runs a Lua script
* **Sandcat.Write**  ( s ): Writes the parameter to the Sandcat Console without creating a new line.
* **Sandcat.WriteLn** ( s ): Writes a new line to the Sandcat Console
* **Sandcat.WriteValue** ( key , value ): Writes a value that can be later read using the browser.jsvalues Lua table