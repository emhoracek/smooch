# smooch

I'm making a set of tools for viewing, converting, and creating KiSS dolls. 
KiSS stands for Kisaeke (Japanese for "dress up") Set System. KiSS dolls 
have been around for a long time, but there's only one modern viewer (and it 
has some drawbacks) and no way to play with these dolls online. Many very 
talented artists spent countless hours making these fun digital toys. Some are 
wonderful examples of pixel artistry. It would be a shame for them to be lost 
just because technology has moved on.

I've added information about KiSS to the [GitHub wiki](https://github.com/emhoracek/smooch/wiki), please check it out if you want to learn more.

I built a tool for converting KiSS cells (the individual graphics) to pnm
format (which is then simple to convert to lots of other formats). Next, I 
want to make a KiSS doll viewer for the browser.

## JavaScript KiSS

My current goals with this project are:

1. Convert CNF to JSON using Haskell
2. Interpret the CNF with JavaScript 
3. Allow dolls to be uploaded to a server to be converted.
4. Allow users to play with dolls in the browser.
5. Enable users to create/edit dolls in the browser. 

This is a very ambitious plan and right now I'm still going back and forth between steps one and two, trying to implement the KiSS specification correctly. I would love help if anyone is interested.

## cel2pnm

The first tool I've made is cel2pnm. It converts the individual pixel
art cells to PNM files. PNM is a human-readable image format that can easily
be converted to other formats like PNG or GIF. Big thanks to [Mark Jason Dominus](http://blog.plover.com/)
 who told me about PNM and helped me code this during his residency at the Recurse 
Center. I also depended on Nick Lamb's GIMP plugin for learning how the cel
formats work.

You can use cel2pnm this way:

```(sh)
cel2pnm <palette file> <cel image> <output file> 
```
or with debugging:
```(sh)
cel2pnm -d <palette file> <cel image> <output file> 
```

Debug mode will print the width, height, bits-per-pixel, and the palette slot
for each pixel in the cell.

KiSS cells always have a transparent color, color 0 in the palette. Add a "-t" 
flag and cel2pnm will print the color instead of converting any cell. The 
color will be printed in the format "rbg:x/x/x" where each is x is a 2-digit 
hex value.

## Converting to png

Sorry, this is ugly. Install pnmtools, then:

```(sh)
cel2pnm examples/smoon.kcf examples/luna.cel examples/luna.pnm
pnmtopng -transparent `./cel2pnm -t examples/smoon.kcf` examples/luna.pnm > luna.png
```

## Feedback

I would very much appreciate input on how to make this program better.

Please let me know if the program doesn't convert a cell correctly and I will 
attempt to fix it.

If you would like to help with this or other tools for KiSS, please let me know!
Email me at libby@daydrea.me, submit an issue, or send a pull request. THANKS!! :D
