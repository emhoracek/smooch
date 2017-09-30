# smooch

I'm making a set of tools for viewing, converting, and creating KiSS dolls.
KiSS stands for Kisekae (Japanese for "dress up") Set System. KiSS dolls
have been around for a long time, but there's only one modern viewer (and it
has some drawbacks) and no way to play with these dolls online. Many very
talented artists spent countless hours making these fun digital toys. Some are
wonderful examples of pixel artistry. It would be a shame for them to be lost
just because technology has moved on.

I've added information about KiSS to the
[GitHub wiki](https://github.com/emhoracek/smooch/wiki), please check it out
if you want to learn more.

I built a tool for converting KiSS cells (the individual graphics) to pnm
format (which is then simple to convert to lots of other formats).

Now, I'm making a KiSS doll viewer for the browser -- and you can help! Check out the [CONTRIBUTING.md](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md) file for details.

[!http://img.shields.io/badge/first--timers--only-friendly-blue.svg?style=flat-square](http://www.firsttimersonly.com/)

## Structure

To display a KiSS doll, an application has to do four things:
  1. Decompress the old, obsure Japanese compression format and extract the files.
  2. Read the specialized image files (cells and palettes).
  3. Read the configuration file.
  4. Display the sets so the user can interact with them.

For the second part I had kludge together some code from a couple different
open source projects (GNOME KiSS and GIMP) in order produce something I could
understand. [Mark Dominus](http://blog.plover.com) helped me a lot with this
when he was a resident at [Recurse Center](http://www.recurse.com). That part
is `cel2pnm` and it's written in C. You can learn more about it in the "cel2pnm" directory's README.

The third involves parsing text into JSON, so I knew exactly what tool I wanted
to use. Haskell has two awesome libraries for this: Parsec and Aeson.
If you'd like to hack on the Haskell, check out the README in the "haskell" directory. The backend of the web app is also in Haskell.

The fourth part is where JavaScript comes in. If you want to hack on Smooch
but don't want to mess with Haskell, try the "javascript" folder. It's not
actually just JavaScript -- it's also the HTML, CSS, and all the images for a
single doll ("Aurora" by Punky). I don't know JavaScript very well so if you
can help that would be amazing.

## Thanks

I (Libby/emhoracek/horrorcheck) started and maintain this project but it wouldn't have gotten where it is now without the efforts of many other people! That includes:

 * the international KiSS community
   * the Japanese KiSS community that developed KiSS as an open standard in the first place
   * Dov Sherman, who maintained the Big KiSS Page (an incredible resource) for many years
   * Stephan Lepisto, who has kept the Big KiSS Page going even after the community died out
   * Nick Lamb, who wrote GnomeKiSS, which I learned and copied a lot from for cel2pnm
   * William Miles, who wrote UltraKiSS, the Java app that I use as a benchmark for compatibility
   * all the incredible artists who made over 4,500 sets for this system
 * Github contributors
   * ear/bpaf helped with Haskell refactoring
   * Darius improved cel2pnm and added tests
   * huggablemonad has helped debug several sets
 * mentors
   * Daniel Patterson (dbp) taught me about monad transformers and error handling in Haskell
   * Mark Dominus helped me write cel2pmn

## Feedback

I would very much appreciate input on how to make this program better.

Please let me know if cel2pnm doesn't convert a cell correctly and I will
attempt to fix it. 

Smooch should display any plain KiSS/GS doll correctly. FKiSS and Cherry KiSS
aren't supported yet. If you find a plain KiSS doll that Smooch doesn't display
right, please submit an issue along with an error message, so we can figure out why!

If you would like to help with this or other tools for KiSS, please let me know!
Email me at libby@daydrea.me, submit an issue, or send a pull request. THANKS!! :D
