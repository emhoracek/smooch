# smooch

Smooch is a web app for viewing and eventually creating KiSS
dolls. KiSS stands for Kisekae (Japanese for "dress up") Set System.

A typical KiSS doll: [Spark by Kimiki](http://otakuworld.com/kiss/dolls/pages/k/ki_spark.htm)

![Playing with a KiSS doll](http://i.imgur.com/UnxpRmL.gif)

Check out Smooch's [GitHub wiki](https://github.com/emhoracek/smooch/wiki) if you want to learn more about KiSS.

We're making a KiSS doll viewer for the browser -- and you can help! Check
out the [CONTRIBUTING.md](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md) file for details.

The static [Smooch demo](http://emhoracek.github.io/smooch/index.html) has examples of working KiSS dolls.

The [Smooch alpha test](http://67.205.172.104:8000/) is now live! Try out some dolls!

![first-timers-only-friendly](http://img.shields.io/badge/first--timers--only-friendly-blue.svg?style=flat-square)
[![Discord](https://badgen.net/badge/icon/discord?icon=discord&label)](https://disboard.org/server/950519299364237312)

## Roadmap

  * Users can save the dolls they uploaded in a personal library
  * Support multiple CNFs
      * A single doll can have multiple configuration files - this was
        often used to have one version with FKiSS effects and another
        without
  * Add CherryKiSS support
  * Allow users to create sets from scratch
      * Users can upload a PSD file.
          * If the PSD is indexed to a single palette:
               * The PSD palette becomes a KCF file
          * If the PSD is 32-bit color:
               * A standard 256-color KCF is created for the doll
          * Each layer becomes a separate cel with the name of the layer
          * All the cels are listed in a CNF that is editable in the browser
      * A user can download their own sets as LHA archives viewable in any
        KiSS viewer
  * Allow users to edit their own dolls
      * Users can click a button to enter "edit" mode
      * Users can modify the position and layering of cels in sets
      * Users can make cels fixed or unfixed
  * Add FKiSS, FKiSS2, FKiSS3, FKiSS4

### Someday, maybes

These are features I don't plan to support. If someone else is excited about one of these
features and wants to add it, then we can talk about it, but these are on the back-burner
for my own efforts.

  * Enhanced Palettes
  * Palette groups
  * FKiSS5 (neither the UltraKiSS or DirectKiSS versions)
  * "Fix" (an object can be "fixed" at first, but clicking on the object reduces the fix value)
  * Expansion dolls (Users can open one doll, then add a second doll to "expand" the original doll)

## Thanks

I (Libby/emhoracek/horrorcheck) started and maintain this project but it wouldn't have gotten where it is now without the efforts of many other people! That includes:

 * the international KiSS community
   * the Japanese KiSS community that developed KiSS as an open standard in the first place
   * Dov Sherman, who maintained Otakuworld's [Big KiSS Page](https://www.otakuworld.com/kiss) (an incredible resource) for many years
   * Stephan Lepisto, who has kept the Big KiSS Page going even after the community died out
   * Nick Lamb, who wrote GnomeKiSS, which I learned and copied a lot from for cel2pnm
   * William Miles, who wrote UltraKiSS, the Java app that I use as a benchmark for compatibility
   * all the incredible artists who made over 4,500 sets for this system
 * Github contributors
   * @ear/bpaf helped with Haskell refactoring
   * Darius improved cel2pnm and added tests
   * @huggablemonad has helped debug several sets and wrote a cel parser in Haskell
   * @lisaychuang improved documentation for contributors
   * Alexis (@aklap) found many many documentation bugs for installing the Haskell app
   * Alice (@wuworkshop) added issue and PR templates
 * mentors
   * Daniel Patterson (dbp) taught me about monad transformers and error handling in Haskell
   * Mark Dominus helped me write cel2pmn

## Feedback

I would very much appreciate input on how to make this program better.

Smooch should display any plain KiSS/GS doll correctly. FKiSS and Cherry KiSS
aren't supported yet. If you find a plain KiSS doll that Smooch doesn't display
right, please submit an issue along with an error message, so we can figure out why!

If you would like to help with this or other tools for KiSS, please let me know!
Email me at libby@daydrea.me, submit an issue, or send a pull request. THANKS!! :D
