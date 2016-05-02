# The Haskell backend

Here's how the Haskell code is organized:

  * src/
    * templates/ contains the templates for the pages
    * Main runs the Warp webserver and does routing.
    * ParseCNF parses the configuration file
    * Kiss contains the KiSS data types (and Aeson instances)
    * Shell converts the files (hmmm... maybe it should be called "Convert" instead?)
    * Upload processes uploaded sets
  * tests/ contains spec tests
  * static/ contains the HTML, CSS, and JavaScript resources and all the kiss sets that are uploaded
  * smooch.cabal is the cabal file (lists all the libraries and modules that are used)
  * stack.yaml is the stack file (lets us avoid Cabal Hell)

To work on this codebase, you need to install [Haskell](https://www.haskell.org/downloads)
and [Stack](https://github.com/commercialhaskell/stack/wiki/Downloads). You also need to compile `cel2pnm` and put it in your PATH.

To test, type `stack test` in your command line while you're in this directory.

Build: `stack build`.

Run: `stack exec smooch`. This serves the app on localhost:8000.

## What it does

Right now, a user is greeted with an invitation to upload a file. The uploaded file unzipped, the configuration is parsed into JSON, and all the ".cel" images are converted to pngs. Then a page with all the images, the JSON, and the KiSS javascript is served to the user.

There are lots of problems and weirdness about this that I'm still figuring out.

## TODO

  * Add offsets from cels (now)
  * Get every one of some test set of non-FKiSS dolls to work correctly (next)
  * Add FKiSS (after that)
  * Figure out how users find/view dolls (someday)

## Sample dolls

A few sample dolls are now included in the repo in
"/haskell/tests/samples/". These dolls were mostly chosen for lack of
advanced FKiSS features, non-graphic nudity or no nudity at all (for
ease of working on this in public ;) ), and (with the exceptions noted
below) small file size.

  * arwen.lzh - [Arwen](http://otakuworld.com/kiss/dolls/pages/a/arwen.htm) by
    Anime Craze and Aragonite. (A larger doll.)
  * boredom.lzh --
    [Boredom](http://otakuworld.com/kiss/dolls/pages/b/boredom.htm) by
    Jade Gordon.
  * lina.lzh -- [Lina
    Inverse](http://otakuworld.com/kiss/dolls/pages/l/lina.htm) by
    Anna.
  * lucca.lzh -- [Lucca](http://otakuworld.com/kiss/dolls/pages/l/lucca1r.htm)
    by Bryan O'Malley. (Another larger doll.)
  * washua.lzh -- [Washu](http://otakuworld.com/kiss/dolls/pages/w/washua.htm)
    by Alison.
  * sk_kimux.lzh -- [Mini-Skuld](http://otakuworld.com/kiss/dolls/pages/s/sk_kimux.htm)
    by MUX. (A Japanese doll.)

It bothers me that I don't have permission from the artists to use
these dolls, but if I only used the dolls of artists I knew how to
reach, I would have no non-FKiSS dolls to use for testing. :/ If any
of these artists finds this repo and would like their work removed, I
am happy to do so. Email me at libby@daydrea.me.