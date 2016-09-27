## Catarinka

Catarinka is a multi-purpose set of Lua extensions developed to be used in the Sandcat Browser. Currently, this library extends Lua with [over 60 functions](https://github.com/felipedaragon/catarinka/blob/master/lualib/docs/functions.md) and some useful classes.

In Sandcat, Catarinka is available as the **ctk** global. For a list of available functions, see [functions.md](https://github.com/felipedaragon/catarinka/blob/master/lualib/docs/functions.md).

### Classes

All Catarinka classes (described in `classes.*` under the [Catarinka Docs](https://github.com/felipedaragon/catarinka/tree/master/lualib/docs) are available.

Each class has a "new" method that must be used for creating the object and a "release" method for freeing it.