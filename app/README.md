# The Haskell backend

Here's how the Haskell code is organized:

  * exe/Main runs the Warp webserver.
  * src/
    * Web does routing and request handling
    * Ctxt defines the request context and provides helper functions
    * ParseCNF parses the configuration file
    * Kiss contains the KiSS data types (and Aeson instances)
    * Shell converts the files (hmmm... maybe it should be
      called "Convert" instead?)
    * Upload processes uploaded sets
    * Users/
       * Model has the data type declaration and functions for querying users
       * View has functions for displaying user data
       * Controller has handlers and routes for users.
  * tests/ contains spec tests
  * templates/ contains the templates for the pages
  * static/ contains the CSS and JavaScript resources. These
    are symlinks to the top level "javascript" directory, so
    that's where you should work on them.
    * static/sets/ is where the uploaded kiss sets are stored.
  * smooch.cabal is the Cabal file (lists all the libraries and
    modules that are used)
  * stack.yaml is the Stack file (helps manage dependencies)

To work on this codebase, follow the directions in the [CONTRIBUTING](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md#run-smooch-on-your-computer) file.

If you have *any issues at all* getting started with Smooch, it would be
extremely helpful to me if you filed a ticket in the "Issues" section on
Github and tagged me (@emhoracek)! I want to know about any
obstacles anyone is facing in using Smooch -- if you're having trouble,
someone else probably will, too!

## Current state of the app

Right now, a user is greeted with an invitation to create an account,
login, or upload a file. Only uploading a file does anything interesting.
The uploaded file is unzipped, the configuration is parsed into JSON, and
all the ".cel" images are converted to pngs. Then a response with all the
images, the JSON, and the KiSS javascript is served to the user.

## Skipping login

It can be kinda annoying to log in every time you want to test a set.
If you would like to skip logging in, just add the following line
to `devel.cfg`:

```
  skip-login-user = "TEST_ACCOUNT_USERNAME_HERE"
```
Then whenever you want to test a set, you can go directly to
"localhost:8000/users/TEST_ACCOUNT_USERNAME_HERE" to "upload" it,
or to "localhost:8000/users/TEST_ACCOUNT_USERNAME_HERE/sets/SET_NAME_HERE"
to view a set that was already "uploaded".

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
