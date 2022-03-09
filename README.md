# smooch

Smooch is a set of tools for viewing, converting, and eventually creating KiSS
dolls. KiSS stands for Kisekae (Japanese for "dress up") Set System.

KiSS dolls have been around for a long time, but there's only one modern viewer
(and it has some drawbacks) and no way to play with these dolls online. Many
very talented artists spent countless hours making these fun digital toys. Some
are wonderful examples of pixel artistry. It would be a shame for them to be
lost just because technology has moved on.

A typical KiSS doll: [Spark by Kimiki](http://otakuworld.com/kiss/dolls/pages/k/ki_spark.htm)

![Playing with a KiSS doll](http://i.imgur.com/UnxpRmL.gif)

Check out Smooch's [GitHub wiki](https://github.com/emhoracek/smooch/wiki) if you want to learn more about KiSS.

Now, we're making a KiSS doll viewer for the browser -- and you can help! Check
out the [CONTRIBUTING.md](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md) file for details.

Checkout the static [Smooch demo](http://emhoracek.github.io/smooch/index.html)!

![first-timers-only-friendly](http://img.shields.io/badge/first--timers--only-friendly-blue.svg?style=flat-square)
![Discord](https://badgen.net/badge/icon/discord?icon=discord&label)](https://disboard.org/server/950519299364237312)

## Roadmap

  * ~~Get a whole bunch of non-FKiSS, single-palette dolls to work correctly~~
  * ~~Support multiple palettes~~
  * Users can view dolls
      * ~~Users can create accounts and log in~~
      * ~~Users can upload an LZH file to the server~~
      * LZH archive is decompressed and files are put in S3 bucket
      * ~~Smooch converts directory contents to PNGs and JSON~~
      * JSON and S3 location are stored in database
      * User can access previously-uploaded dolls from profile
  * Set up server deployment (probably Docker)
  * Release a usable vanilla KiSS viewer

### Future plans
  * Support multiple CNFs
      * A single doll can have multiple configuration files - this was
        often used to have one version with FKiSS effects and another
        without
  * Allow users to create sets from scratch
      * Users can upload a PSD file.
          * PSDs must be indexed to a single palette.
          * The PSD palette becomes a KCF file.
          * Each layer becomes a separate cel with the name of the layer
          * All the cels are listed in a CNF that is editable in the browser
      * A user can download their own sets as LHA archives viewable in any
        KiSS viewer
  * Allow users to edit their own dolls
      * Users can click a button to enter "edit" mode
      * Users can modify the position and layering of cels in sets
      * Users can make cels fixed or unfixed
  * Add CherryKiSS support
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
