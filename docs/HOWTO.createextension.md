##The Sandcat Extension System

Syhunt Sandcat extensions are developed using a combination of **HTML**, **CSS** and **Lua** script. Sandcat Extensions, Tasks and Console commands can execute Lua code with all its standard functions, including file IO support. TIScript (a programming language derived from JavaScript) can also be used to extend and manipulate the Sandcat user interface.

Sandcat Extensions are deployed as ZIP files (renamed **.scx**) under [SandcatDir]\Packs\Extensions\

###Extension Manifest

During startup Sandcat will check all extensions for a Manifest.json file contained within the extension's ZIP package. The following is an example of a manifest file:
 
```
{
 "name": "Hello World Extension",
 "version": "1.0",
 "author": "Me",
 "script": {
  "filename": "hello.lua",
  "init": "Hello:register()",
 }
}
```

###Manifest Keys

* **name***: The extension name
* **version***: The extension version. If 'file:[dllfilename]' is used as version, the browser will read the version from the supplied DLL file.
* **description**: The extension description
* **script***: The Lua code
 * filename: The main Lua script file contained within the extension's ZIP package. This script will be loaded and executed during the browser startup
 * init: Can contain a Lua script to be executed during the browser startup. If script.filename has been provided, executes after the script file has been loaded.
 * shutdown: A Lua script to be executed during the browser shutdown
 
(*) = required keys

###HelloWorld Example

How to make a simple hello world extension:

* Create a folder somewhere on your system to contain your extension's files.
2. Inside the folder, create Manifest.json with the following content:

```
{
 "name": "Hello World Extension",
 "version": "1.0",
 "author": "Me",
 "script": {
  "filename": "hello.lua",
  "init": "Hello:register()",
 }
}
```

* Create a Lua script called hello.lua with the following content:

```lua
Hello = {}

function Hello:showmsg()
 app.showmessage('Hello World!')
end

function Hello:register() -- will be called during startup
 local html = [[
 <div
 class="button"
 onclick="Hello:showmsg()"
 style="foreground-image: url(Hello.scx#world.png);"
 />
 ]]
 browser.navbar:inserthtml(1,'#toolbar',html)
end
```

* Place ![this](http://www.syhunt.com/sandcat/uploads/EDK/world.png "") icon file in the same folder
* Zip the files and change the extension to .scx. The zip file can be called Hello.scx.
* Copy the Hello.scx file to the **Packs\Extensions** folder, which is located in the root of the Sandcat Browser folder.
* Launch the Sandcat Browser. Now in the navigation bar, you should see the globe icon. Now try clicking on it.

###More Examples

* [Syhunt Community Extensions](https://github.com/syhunt/community)