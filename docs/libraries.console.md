##Console library

Sandcat Browser comes with Sandcat Console, a command console with several useful commands and extension possibilities. 

###Functions

* **console.addcmd** (name,luacode,description) : Adds a custom command to the Sandcat Console

* **console.clear** (): Clears the contents of the Sandcat Console

* **console.gethandler** (): Gets the current command handler. Returns `string`

* **console.reset** (): Restores user settings, default handler and clears the console.

* **console.setcolor** ( htmlcolor ) : Sets the background color
 
* **console.setfontcolor** ( htmlcolor ) : Sets the font color

* **console.sethandler** ( cmdname ): Redirects all future commands to the supplied cmd name

* **console.write** ( v ) : Writes the parameter to the Sandcat Console without creating a new line

* **console.writeln** ( v ): Writes a new line to the Sandcat Console

###Cmd Object

The cmd object allows you to get the parameters of a Sandcat Console command. The object is automatically created when Sandcat starts. Its available methods and properties are described below.

name | type | description
--- | --- | ---
cmd.name | string | Gets the name of the last command
cmd.params | string | Gets the parameters of the last command

###Adding Commands

Below you can find how to extend the console with custom commands. Currently, there are two methods for adding new commands.

####Method 1: As part of a Sandcat Extension

New commands can be added using the Lua language via the **console.addcmd()** function during the Sandcat extension initialization or at any moment after the initialization. The first parameter must contain the command name and its arguments (if any), the second parameter the Lua code to be executed, and the third parameter must contain a simple description of the command. See below a few examples.

```lua
MyCommands = {}

function MyCommands:Google(query)
 if query ~= '' then
  browser.newtab('https://www.google.com/search?q='..query)
 end
end

function MyExtension:init()
 -- Google Search command
 console.addcmd('search [query]','MyCommands:Google(cmd.params)','Searches Google')
 -- Simple print command
 console.addcmd('say [str]','print(cmd.params)','Prints a string of text')
end
```

####Method 2: As an external Command file

New commands can also be added by creating a Lua script file which must be placed in the Scripts/Commands directory. The first line of the script must be a comment containing the command name and its arguments (if any). The second line should contain a simple description of the command. See below an example of a command file.

say.lua:

```lua
-- say [str]
-- Prints a string of text
if cmd.params ~= '' then
 print(cmd.params)
end
```