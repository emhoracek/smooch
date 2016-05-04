# The Haskell backend

*Windows users: The Haskell web app will not build on Windows because
of network library problems. :( If you use Windows, you'll need to
work in a Linux VM to get this code to run.*

Here's how the Haskell code is organized:

  * src/
    * Main runs the Warp webserver and does routing.
    * ParseCNF parses the configuration file
    * Kiss contains the KiSS data types (and Aeson instances)
    * Shell converts the files (hmmm... maybe it should be
      called "Convert" instead?)
    * Upload processes uploaded sets
  * tests/ contains spec tests
  * templates/ contains the templates for the pages
  * static/ contains the CSS and JavaScript resources. These
    are symlinks to the top level "javascript" directory, so
    that's where you should work on them.
    * static/sets/ is where the uploaded kiss sets are stored.
  * smooch.cabal is the Cabal file (lists all the libraries and
    modules that are used)
  * stack.yaml is the Stack file (helps manage dependencies)

To work on this codebase, you need to install
[Stack](https://github.com/commercialhaskell/stack/wiki/Downloads). You
also need to compile `cel2pnm` and put it in your PATH.

If you haven't installed Haskell before, run `stack setup` on your
command line, in this directory.

To test, run `stack test`.

Build: `stack build`.

Run: `stack exec smooch`. This serves the app on localhost:8000.

## What it does

Right now, a user is greeted with an invitation to upload a file. The
uploaded file unzipped, the configuration is parsed into JSON, and all
the ".cel" images are converted to pngs. Then a response with all the
images, the JSON, and the KiSS javascript is served to the user.

It does not work very well.

## TODO

Roughly in order, except the last!!

  * First JSON generated for cel data so that cel and offset are a single
    object, not elements in a nested array.
  * Use the correct background color for a set.
  * Get a whole bunch of non-FKiSS, single-palette dolls to work correctly.
  * Figure out how NOT to use shell commands to unzip LHAs or convert cels.
      * Serve cels as binary blobs and let JavaScript display them?
      * Inline the C code for cel2pnm?
      * Write a Haskell library for decompressing LHAs? (O.O)
  * Support multiple palettes (dependent on how the above situation
    with cels as blobs vs pngs turns out)
  * Figure out how users view dolls, whether dolls are stored on
    server, etc.
  * Release a usable vanilla KiSS viewer.
  * Add FKiSS1, FKiSS2, FKiSS3, CherryKiSS, Enhanced Palettes, FKiSS4,
    FKiSS5...
  * Add creation and editing tools.
  * Refactoring, better tests, and better documentation

## How you can contribute

If you would like to help with Smooch, I am SO EXCITED!! Let me tell you
some specific things that would be super helpful.

The first would be: tackle an issue! But right now I haven't written
up many issues. :)

Another fun possibility is to try out KiSS dolls from
[Okatuworld](http://www.otakuworld.com/kiss) and see which ones work
and which don't. I would look for dolls that are as simple as possible
-- a single palette, a small wardrobe, no animations or special
effects (no "FKiSS").

You can also use the samples included in the repo (see below)!

Try the doll you've chosen in a KiSS doll viewer like [GnomeKiSS for
Linux](http://devel.tlrmx.org/kiss/), [Direct KiSS or PlayKiSS for
Windows](http://otakuworld.com/index.html?/kiss/viewers.htm) or
[UltraKiSS for anything with
Java](http://www.wmiles.com/projects/ultrakiss). Then, fire up Smooch
and try the doll in Smooch. The doll probably won't look right -- it
might cause an error in the Haskell or Javascript!

If you can add an issue describing the problems with the doll and
perhaps even start investigating what is causing the problem that
would be super helpful <3 <3 <3

If the doll is small and has lots of interesting problems, it
may be a good candidate for being included in the repo as a
sample doll.

AND if you wanted to make a pull request to solve a problem or
two that would be amazing!! Please do!

It's also super helpful to have more and better documentation.
If anything here is confusing, please create an issue. If
you can improve any documentation, please create a pull request!

I also appreciate any sort of  ideas and feedback!

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