##Browser Library

###Core browser methods

* **browser.addlibinfo** ( name, version, author [,luacode] ): Adds information about a library to the About Sandcat screen. Should be called by extensions during startup
 * *version* - the library version. If 'file:[dllfilename]' is used as version, the browser will read the version from the DLL file.
 * *luacode* - A code to be executed when the library information icon is clicked. This parameter is optional.

* **browser.bookmark** ( [title,url] ): Adds a page to the bookmarks. This function bookmarks the page of the active tab if no parameters are provided

* **browser.cleardata** ( type ): Clears browsing data. The type parameter can be: cache, appcache, cookies, databases, history, settings

* **browser.closepage** ( pagename ): Closes an extension page by its name

* **browser.closetab** ( tabname ): Closes a tab by its name. If no param is supplied, closes the active tab

* **browser.closetabs** ( except ): Closes all tabs. If a tab name is supplied as parameter, closes all tabs except the specified tab. E.g. browser.closetabs(tab.name) will close all but active tab

* **browser.dostring** (luacode): Runs a Lua script

* **browser.exit** (): Closes the browser

* **browser.getpackfile** ( pakfilename, filename): Gets the contents of a file that is inside an extension package. Returns `string`

* **browser.gettaskinfo** ( taskid ): Gets details about a Sandcat Task, launched using the tab:runtask() function. Returns `table`

* **browser.gototab** ( tabname ): Goes to the specified tab

* **browser.loadpagex** ( pagename, html [,tablename]): Loads an extension page in a separate page. The third parameter is optional. If supplied, associates the HTML elements of the page with a Lua table. See UI Manipulation for details.

* **browser.newtab** ( url [,source] ): Opens an URL in a new tab. The source parameter is optional - if specified, the page will be loaded from it. Returns the tab number. Returns `integer`

* **browser.newtabx** ( luatable ): Creates a custom extension tab. Returns the tab number. Returns `integer` The following keys can be provided (all of them optional):
 * *activepage* - the active tab page (ex: browser, source, log...)
 * *icon* - an icon url
 * *html* - page contents
 * *shownavbar* - if false, hides the navigation bar.
 * *table* - the name of a Lua table. Associates the HTML elements of the page with the table. See UI Manipulation for details.
 * *tag* - an unique string to identify the tab. If newtabx() is called again and a tab with the supplied tag has been already created, Sandcat goes to the tab instead of creating a new one. Additionaly, if a *loadnew* key is supplied and is true, the tab page is reloaded from the HTML string supplied as the second parameter.
 * *title* - the tab title
 * *toolbar* - a custom toolbar source (eg: MyExtension.scx#MyToolbar.html)
 
* **browser.newwindow** ( [url] ): Opens a new browser window. If a URL is provided as parameter, opens the URL

* **browser.removetask** ( taskid ): Stops and removes a Sandcat Task from the tasks list

* **browser.setactivepage** ( pagename ): Sets the active page by its name (eg, browser, source, log, etc).

* **browser.setsearcheng** ( name, queryurl, iconurl): Sets a new search engine

* **browser.showreqbuilder** (): Loads the Request Builder bar

* **browser.showurl** ( url [,source] ): Loads an URL in the bottom bar. The source parameter is optional - if supplied, the page will be loaded from it

* **browser.stoptask** ( taskid [,reason] ): Stops a Sandcat Task. The second parameter is optional, can be used to explain the reason of the stop

* **browser.suspendtask** ( taskid [,resume] ): Suspends a Sandcat Task. The second parameter is optional - if supplied and true, resumes the task.

### Meta Tables

#### **browser.options**

Allows to set the visibility of special UI parts

name | type | description
--- | --- | ---
showbottombar | boolean | Sets the visibility of the bottom bar
showconsole	| boolean | Sets the visibility of the Sandcat Console
showheaders	| boolean | Sets the visibility of the live headers
showpagestrip |	boolean	| Sets the visibility of the page strip
showsidebar | boolean | Sets the visibility of the sidebar

#### **browser.info**

Returns various details about the browser

* abouturl
* cachedir
* configdir
* commands
* downloads
* errorlog
* exefilename
* extensions
* iconfilename
* libraries
* name
* options
* proxy
* tasks
* useragent
* version

#### **browser.jsvalues**

Stores temporary JSON values