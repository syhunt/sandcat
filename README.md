# Sandcat Browser

Sandcat is a lightweight multi-tabbed web browser that combines the speed and power of Chromium and Lua. Sandcat comes with built-in live headers, an extensible user interface and command line console, resource viewer, and many other features that are useful for web developers and pen-testers and when you need to examine live web applications. For more details, visit [http://www.syhunt.com/sandcat/](http://www.syhunt.com/sandcat/). See also the docs directory and credits section below for a few more details about the Sandcat architecture.

## Directories

* `/docs` - Lua API documentation
* `/packs` - contents of uncompressed pack files
 * `/Common` - common CSS, widgets and scripts package (Common.pak)
 * `/Resources` - resources package (Resources.pak)
* `/src` - the main executable source and built-in resource files
 * `/core` - user interface source
 * `/html` - user interface resources (HTML)
 * `/lua` - Lua API source
 
## Download

Compiled binaries for Windows can be downloaded from the links below.

* [6.0 RC2 64-bit](https://dl.syhunt.net/tools/sandcat/sandcat-6.0rc2-win64.exe)
* [6.0 RC2 32-bit](https://dl.syhunt.net/tools/sandcat/sandcat-6.0rc2-win32.exe)
* [6.0 RC2 32-bit with Pen-Tester Tools](http://www.syhunt.com/en/?n=Syhunt.DownloadHybridCE) (included as part of Syhunt Community)

## Compiling

For compiling Sandcat, you will just need [Catarinka](https://github.com/felipedaragon/catarinka) and [pLua](https://github.com/felipedaragon/pLua-XE).
 
The entire Sandcat user interface is created during runtime, so there is no need to install third-party components in the IDE - you can just add the dependencies listed above to the library path and hit compile. It compiles under Delphi 10 Seattle down to XE2.

## License & Credits

Sandcat was developed by Felipe Daragon, [Syhunt](http://www.syhunt.com/).

This code is licensed under a 3-clause BSD license - see the LICENSE file for details.

Third-party software used in Sandcat include:

* [libcef](http://code.google.com/p/chromiumembedded), based on [Chromium](http://www.chromium.org/), is the engine at the heart of the Sandcat Browser. Sandcat can use the [WACEF3](https://bitbucket.org/WaspAce/wacef) (Chromium Embedded Framework) component or the [DCEF3](https://github.com/hgourvest/dcef3) library). Because we want Sandcat to use the latest Chromium binaries, the most up-to-date and stable wrapper is used for official Sandcat releases.
* [Lua](http://www.lua.org/) - Developed by a small team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Lua is the core language used to develop Sandcat extensions as well as portions of the browser itself.
* [TIScript](http://code.google.com/p/tiscript/) is an extended version of ECMAScript (JavaScript 1.x) developed by Terra Informatica Software and [@AndrewTerra](https://github.com/AndrewTerra), the developers of Sciter. It is used by Sandcat for some of its user interface operations.
* **Sciter** is the engine currently used by Sandcat for rendering its user interface. Sciter supports TIScript execution and, in Sandcat, it can also execute Lua scripts and be manipulated from Lua scripts.
* Icons are derived from: [Fugue Icons](https://github.com/yusukekamiyamane/fugue-icons) (by [@yusukekamiyamane](https://github.com/yusukekamiyamane/)) and [FatCow Icons](http://www.fatcow.com/free-icons).
* The [custom Lua icon](http://maurits.tv/data/garrysmod/wiki/wiki.garrysmod.com/indexbf0b.html) is the work of Yarin Kaul, and used in Sandcat with his permission.
* For syntax highlighting, Sandcat currently uses [SynEdit](http://sourceforge.net/projects/synedit/) and [@Krystian-Bigaj](https://github.com/Krystian-Bigaj)'s [SynWeb](https://code.google.com/p/synweb/) with a color scheme adapted from [@korny](https://github.com/korny)'s [CodeRay](https://github.com/rubychan/coderay).

The license for each component listed above can be found in the `/packs/Resources/docs/` directory of this repository.

## Contact

Twitter: [@felipedaragon](https://twitter.com/felipedaragon), [@syhunt](https://twitter.com/syhunt)

Email: felipe _at_ syhunt.com

If you want to report a security bug, please see the `docs\SECURITY.md` file.