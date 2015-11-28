## Selenite

Selenite is a multi-purpose set of Lua extensions developed to be used in the Sandcat Browser. Currently, this library extends Lua with [over 60 functions](https://github.com/felipedaragon/selenite/blob/master/docs/functions.md) and some useful classes.

In Sandcat, Selenite is available as the **slx** global. For a list of available functions, see [functions.md](https://github.com/felipedaragon/selenite/blob/master/docs/functions.md).

### Classes

All Selenite classes (described in `classes.*` under the [Selenite Docs](https://github.com/felipedaragon/selenite/tree/master/docs) are available.

Each class has a "new" method that must be used for creating the object and a "release" method for freeing it.