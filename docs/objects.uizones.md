##UI Zone Object

Used to extend the Sandcat user interface

###Available UI Zones
name | description
--- | ---
browser.navbar|Navigation bar
browser.pagebar|Page tab strip
browser.statbar|Status bar
browser.tabbar|Tab bar
tab.engine|Main engine of the active tab
tab.toolbar|Custom tab toolbar
reqbuilder.toolbar|Request Builder toolbar
dlg.prefs|Preferences dialog
dlg.about|About Sandcat dialog

###UI Zone Methods

* **addhtml** ( target, html ): Extends the UI zone with custom HTML elements. This method should be called by extensions during startup
 * target: must be a CSS selector string
 * html: the HTML string you want to add

* **addhtmlfile** ( target, html): Extends the UI zone with custom HTML elements from a file. This method should be called by extensions during startup
 * target: must be a CSS selector string
 * htmlfilename: the name of the HTML file you want to add

* **addtiscript** ( tiscode ): Extends the UI zone with custom TIScript. This method should be called by extensions during startup

* **eval** ( tiscode ): Evaluates a TIScript code. Returns `variant`

* **inserthtml** ( index, target, html ): Extends the UI zone with custom HTML elements. This method should be called by extensions during startup
 * index: insertion position (zero-based).
 * target: must be a CSS selector string
 * html: the HTML string you want to insert

* **inserthtmlfile** ( index, target, htmlfilename): Extends the UI zone with custom HTML from a file. This method should be called by extensions during startup

* **loadx** ( html [, tablename] ): Loads the UI zone interface from a string. The second parameter is optional. If supplied, associates the HTML elements of the page with a Lua table. See UI Manipulation for details.