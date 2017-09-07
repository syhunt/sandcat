##OSR Object

Creates Off-Screen Chromium Renderer processes

###Creation

* **osr:new** ()
Creates and returns a new Sandcat Chromium OSR object

####Example

```lua
local cef = osr:new()
cef.onsetsource = function(s) app.showmessage(s) end
cef.onrequestdone = function(t) app.showmessage(t.url) end
cef:loadurl('http://www.syhunt.com/sandcat/')
```

###Methods

* **loadrequest** ( method, url[,postdata] ): Sends a HTTP request and loads its response. The third parameter is optional; can be used in POST requests. This method can alternatively be called with a single table parameter (see below)

* **loadrequest** ( luatable ): Sends a custom HTTP request and loads its response. Accepts the same keys as the tab:loadrequest() method.

* **loadsource** ( source, url ): Renders a source code string

* **loadurl** ( url ): Goes to the supplied url

* **reload** ( ignorecache ): Reloads the page. If the first parameter is supplied and is true, the cache will be ignored

* **runjs** ( jscode [,url, startline] ): Executes a JavaScript code in the loaded page. The last two parameters are optional

* **runjs** ( luatable ): Performs a custom JavaScript call in the loaded page. Accepts the same keys as the tab:runjs() method.

* **sendrequest** ( luatable ): Sends a custom HTTP request. See Request for details. Accepts the same keys as the tab:loadrequest() method.

* **showauthdialog** ( [username,password] ): Displays the basic authentication dialog. The username and password parameters are optional. If supplied, they are used as default values in the dialog

* **stop** (): Stops loading a page

* **release** (): Frees the OSR object

###Properties

name | type | description
--- | --- | ---
captureurls | boolean | Enables or disables the capture of URLs during operation (Default: false)
downloadfiles | boolean | Enables or disables the download of files during operation (Default: false)
getsourceastext	| boolean | Defines if the source must be returned as text
reslist| string | Gets the list of resources (images, CSS files, etc) of the loaded page
url	| string | Gets or sets the URL of the renderer
urllist	| string | Gets a list of URLs captured during operation (requires captureurls enabled)

###Events
* **onaddresschange**: If set, called when the URL of the renderer changes (eg, after a 302 redirect). Receives a string containing the new URL

* **onbeforepopup**:If set, called before creating a popup window
Receives a table parameter containing the keys: url

* **onconsolemessage**: If set, called when there is a JavaScript execution error or when console.log() is called. Receives a table parameter containing the keys: message, source and line

* **ondownloadstart**: If set, called before starting a file download
Receives a table parameter containing the keys: id, suggestedname

* **onloadend**: If set, called when the page finishes loading
Receives a table parameter containing the keys: statuscode

* **onloaderror**: If set, called when there is a error during operations
Receives a table parameter containing the keys: errorcode, errortext and failedurl

* **onloadstatechange**: If set, called when the loading state of a page changes. Receives a table parameter containing the keys: isloading, cangoback and cangoforward

* **onrequestdone**: If set, called after every request
Receives a table as first parameter and a JSON string as second parameter. They contain request details in the form of keys

* **onresourcefound**: If set, called when resouce files (images, CSS, etc) are loaded. Receives a string containing the resource URL

* **onsetsource**: If set, called when the page finishes loading
Receives a string containing the page source code