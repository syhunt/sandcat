##App library

Contains mostly application and window related functions

* **app.ask_yn** (msg [,caption]) : Asks a question. Returns true if the Yes button is pressed and false if No is pressed. The second parameter is optional. Returns `boolean`

* **app.bringtofront** (): Brings the application window to the front.

* **app.gettitle** (): Gets the application window title. Returns `string`

* **app.openfile** ( [filter, default_ext, default_filename] ): Displays the open file dialog. Returns the entered filename or an empty string if the Cancel button has been pressed. All parameters are optional. Returns `string`

* **app.savefile** ( [filter, default_ext, suggested_filename] ): Displays the save file dialog. Returns the entered filename or an empty string if the Cancel button has been pressed. All parameters are optional. Returns `string`

* **app.selectdir** ( [caption]): Displays the select directory dialog. Returns the selected directory or an empty string if the Cancel button has been pressed. All parameters are optional. Returns `string`

* **app.settitle** ( s ): Sets the application window title

* **app.showalert** ( s ): Displays a warning message to the user
 
* **app.showinputdialog** ( prompt [, default_value, caption]): Displays a simple input dialog. Returns the entered string or the default value string if the Cancel button has been pressed.

* **app.showdialogx** ( html [, id] ): Displays a custom Sandcat extension dialog. The id parameter is optional, can be used to assign an unique identifier to the dialog.

* **app.showmessage** ( s ): Displays a message to the user

* **app.showmessagesmpl** ( s ): Displays a message to the user using a simple dialog

* **app.showmessagex** ( html ): Displays a HTML formatted message to the user.

* **app.update** (): Updates the screen; Ensures that the window is completely drawn.

### Variables

* **app.dir**: stores the name of the Sandcat application directory
* **app.datadir**: stores the name of the Sandcat application data directory