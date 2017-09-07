##Tab Object

The tab object is automatically created when Sandcat starts and is always connected to the active tab.

###Methods

* **tab:clearheaders** (): Clears the live headers

* **tab:clearlog** (): Clears the contents of the Log tab

* **tab:goback** (): Goes back in history

* **tab:goforward** (): Goes forward in history

* **tab:gotosrcline** ( int ): Goes to the source page and scrolls to a given line number

* **tab:gotourl** ( url [, source] ): Goes to the supplied url. The source parameter is optional - if supplied, the page will be loaded from it.

* **tab:loadrequest** ( method, url [,postdata]): Sends a HTTP request, loads its response and adds the request to the Live Headers list. The third parameter is optional; can be used in POST requests. This method can alternatively be called with a single table parameter (see below).

* **tab:loadrequest** ( luatable ): Sends a custom HTTP request, loads its response and adds the request to the Live Headers list. The following keys can be provided:
 * method - the HTTP method (GET, POST, etc). Default GET
url - the request URL (if not supplied, the tab URL will be used as URL)
 * postdata - the request data
 * headers - custom request headers
 * ignorecache - If supplied, and is false, may load the page from the cache. Default true.
 * useauth - If supplied, and is true, uses cached authentication credentials. Default true.
 * usecookies - If supplied, and is false, ignores cookies. Default true.
details - a short description for the request

* **tab:loadsourcefile** ( filename ): Loads the contents of source page from a file.

* **tab:loadx** ( html [,tablename] ): Loads an extension page. The second parameter is optional - if supplied, associates the HTML elements of the page with a Lua table. See UI Manipulation for details.

* **tab:log** ( s ): Prints a line to the Log page.

* **tab:logerror** ( componenttitle, linenumber, msg ): Manually adds an error item to the Script Errors screen.

* **tab:logrequest** ( json [,responsetext] ): Manually logs a request from a custom HTTP client class to the Live Headers. The second parameter is optional and can contain the response text. The following JSON keys can be provided:
 * method (required): the request method
 * url (required): the request URL. In case this is a low level request you can supply a host and a port key instead of an URL key.
 * postdata: the request data
 * headers: the request headers
 * responseheaders: the response headers
 * responsefilename: the name of a temporary file that must contain the response text or stream. The file will be automatically deleted after its contents are added to the Live Headers cache.
 * status: if not supplied, the status code will be read from the response headers
 * details: a short description for the request

* **tab:reload** ( ignorecache ): Reloads the page. If the first parameter is supplied and is true, the cache will be ignored.

* **tab:runjs** ( jscode [,url, startline]): Executes a JavaScript code in the loaded page. The last two parameters are optional.

* **tab:runjs** ( luatable ): Performs a custom JavaScript call in the loaded page. The following keys can be provided:
 * code (required): the JavaScript code
 * url: the JS URL
 * startln: the start line number (default is 0)
 * silent: If supplied, and is true, JS execution errors are not reported. Default false.

* **tab:runluaonlog** (msg,luacode): Sets a Lua script to be executed after a certain message is received through the JS console.log() method. This method should be used with caution.

* **tab:runtask** ( luacode [,json, menuhtml] ): Executes a Lua code in an isolated process. The second parameter is optional and can be used to pass parameters to the task process. Returns an unique task ID. Returns `string`

* **tab:search** ( s ): Performs a search using the active search engine

* **tab:sendrequest** ( method, url [,postdata] ): Sends a HTTP request and adds the request to the Live Headers list. The third parameter is optional. This method can alternatively be called with a single table parameter as explained below.

* **tab:sendrequest** ( luatable ): Sends a custom HTTP request and adds the request to the Live Headers list. Supports the same table keys as the tab:loadrequest() method (see above).

* **tab:showauthdialog** ( [username, password] ): Displays the basic authentication dialog. The username and password parameters are optional. If supplied, they are used as default values in the dialog.

* **tab:stopload** (): Stops loading a page.

* **tab:viewsource** (): Views the page source in an external editor.

###Properties

name | type | description
--- | --- | ---
capture|boolean|Enables or disables the live headers capture
capturebrowser|boolean|Enables or disables the live headers capture just for browser requests
downloadfiles|boolean|Enables or disables the download capability
headersfilter|string|Gets or sets the live headers filter
icon|string|Gets or sets the tab icon
lastjslogmsg|string|Gets the last message from a JS console.log() call
loadend|string|Sets a Lua script to be executed after the page finishes loading
loadendjs|string|Sets a JavaScript to be executed after the page finishes loading
logtext|string|Gets or sets the contents of the Log page
name|string|Gets the unique ID of the tab
reslist|string|Gets the list of resources (images, CSS files, etc) of the loaded page
screenshot|string|Takes a screenshot of the loaded page and returns its temporary filename
source|string|Gets or sets the source of the loaded page
status|string|Gets or sets the status bar text
title|string|Gets or sets the tab title
url|string|Gets the URL of the loaded page
zoomlevel|double|Gets or sets the zoom level