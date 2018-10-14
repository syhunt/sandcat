##Sandcat UI Manipulation

###Method 1: Using a Lua or TIScript script tag

Sandcat can associate the HTML elements of a Sandcat extension user interface with a Lua table. In order to enable this, just add a `SandcatUIX` meta tag to the extension UI code, as exemplified below.

```html
<meta name="SandcatUIX" content="MyExtension">
<script type="text/lua">
function MyExtension:changeit()
  self.plaintext1.value = 'New text!'
end
</script>
<plaintext id="plaintext1"></plaintext>
<button onclick="MyExtension:changeit()">Demo</button>
<button onclick="MyExtension.plaintext1.value = 'Another value'">Demo 2</button>
```

As seen above, elements that have an id attribute can be manipulated via **[tablename].[yourelementid]**, which returns a Sandcat UI element object.

Alternatively, you can use a TIScript script tag:

```html
<script type="text/tiscript">
$(#btndemo).onControlEvent = function(evt) {
 if (evt.type == Event.BUTTON_CLICK ) {
  $("#myplaintext").value = "New text!";
  }
}
</script>
<plaintext id="myplaintext"></plaintext>
<button #btndemo>Demo</button>
```

###Method 2: Using Lua

You can associate the HTML elements of a Sandcat extension user interface with a Lua table by supplying a table name when calling the browser.loadpagex() method, as exemplified below.

interface.html:
```html
<plaintext id="myplaintext"></plaintext>
<button onclick="MyExtension:changeit()">Demo</button>
```

interface.lua:
```lua
MyExtension = extensionpack:new()
MyExtension.filename = 'Demo.scx'

function MyExtension:show()
  local html = self:getfile('interface.html')
  browser.loadpagex({name='My Extension',html=html,table='MyExtension'})
end

function MyExtension:changeit()
  self.myplaintext.value = 'New text!'
end
```

###Method 3: Using TIScript and the UI zone's eval() method

interface.html:
```html
<plaintext id="myplaintext"></plaintext>
<button onclick="MyExtension:changeit()">Demo</button>
```

interface.lua:

```lua
MyExtension = extensionpack:new()
MyExtension.filename = 'Demo.scx'
MyExtension.zone = browser.pagex

function MyExtension:show()
  local html = self:getfile('interface.html')
  browser.loadpagex({name='My Extension',html=html,table='MyExtension'})
end

function MyExtension:changeit()
  self.zone:eval('$("#myplaintext").value = "New text!"')
end
```