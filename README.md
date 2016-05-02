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

Now, I'm making a KiSS doll viewer for the browser.

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

## Feedback

I would very much appreciate input on how to make this program better.

Please let me know if cel2pnm doesn't convert a cell correctly and I will
attempt to fix it. (If smooch doesn't display a doll correctly, that's because it
doesn't display hardly anything correctly right now.)

If you would like to help with this or other tools for KiSS, please let me know!
Email me at libby@daydrea.me, submit an issue, or send a pull request. THANKS!! :D
