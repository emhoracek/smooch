# cel2pnm

This is the first tool I made for Smooch. It converts the individual pixel
art cells to PNM files. PNM is a human-readable image format that can easily
be converted to other formats like PNG or GIF. Big thanks to [Mark Jason Dominus](http://blog.plover.com/)
 who told me about PNM and helped me code this during his residency at the Recurse
Center. I also depended on Nick Lamb's GIMP plugin for learning how the cel
formats work.

You can use cel2pnm this way:

```(sh)
cel2pnm <palette file> <cel image> <output file>
``
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
