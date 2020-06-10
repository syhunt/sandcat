##Security considerations when developing extensions

Sandcat Extensions, built using HTML and Lua, can execute Lua code with file IO support. For this reason, you should not load a string as part of the HTML of an extension user interface without filtering or validating the content before.

This applies to the following functions:
* **app.showdialogx()**
* **app.showmessagex()**
* **app_showalerttext()**
* **browser.loadpagex()**
* **browser.newtabx()**
* **tab:loadx()**
* **[anyuizone]:loadx()**

###Never use the extension user interface for regular browsing

You can use http for displaying media files, such as images, but do not try to load web sites using supported protocols. You will be loading an entirely new user interface, which as mentioned before has desktop application privileges.

If you need to load a web page, use the function **tab:gotourl()**, which will load the web page using the standard, safe for browsing Chromium navigator.

###Security Bug Reporting

Security bugs should be reported directly to security@syhunt.com. Low risk security bugs can be reported by opening an issue here. If you are unsure about the risk level, please report it via email.