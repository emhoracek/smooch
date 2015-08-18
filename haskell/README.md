Here's how the Haskell code is organized:

  * src/
    * templates/ contains the templates for the pages
    * Main runs the Scotty webserver
    * ParseCNF parses the configuration file
    * Kiss contains the KiSS data types (and Aeson instances)
    * Shell converts the files (hmmm... maybe it should be called "Convert" instead?)
    * Upload processes uploaded sets
  * tests/ contains unit tests
  * static/ contains the HTML, CSS, and JavaScript resources and all the kiss sets that are uploaded
  * smooch.cabal is the cabal file (lists all the libraries and modules that are used)
  * stack.yaml is the stack file (lets us avoid Cabal Hell)

To work on this codebase, you need to install [Haskell](https://www.haskell.org/downloads) 
and [Stack](https://github.com/commercialhaskell/stack/wiki/Downloads). 

To test, type `stack test` in your command line while you're in this directory.

Build: `stack build`.

Install: `stack install`

Run: `smooch`. This serves the app on localhost:3000.

If you have any questions or comments, please let me know! 
