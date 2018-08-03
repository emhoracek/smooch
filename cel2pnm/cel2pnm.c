#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned char palette[256 * 3];
static char transparent[14]; // the red, green, blue of the transparent color,
                     // as hex, with slashes between, prefaced by "rgb:"
                     // (for pnmtopng)
static int debug = 0;
static int output_offset = 0;

static int parse_uint16(const unsigned char *bytes) {
    return bytes[0] + 256 * bytes[1];
}

static int convert_cel(const char *celfile, const char *pnmfile) {
    FILE    *fpcel,
            *fppnm,
            *fppgm;
    unsigned char header[32],
             file_mark,
             bpp;
    int      height, width,
             offx, offy;// might need these later
    int      i, j;
    size_t   n_read;

    fpcel = fopen(celfile, "r");
    if (!fpcel) {
        perror(celfile);
        return -1;
    }

    n_read = fread(header, 4, 1, fpcel);
    if (n_read < 1) {
        fprintf(stderr, "Unable to read header.\n");
        return -1;
    }

    if (strncmp ((const char *) header, "KiSS", 4)) {
        // if the header does NOT start with KiSS
        if (debug) {
            fprintf(stderr, "Old style KiSS cel.\n");
        }
        bpp    = 4;
        width  = parse_uint16(header+0);
        height = parse_uint16(header+2);
        offx   = 0;
        offy   = 0;
    }
    else {
        n_read = fread(header, 28, 1, fpcel);
        if (n_read < 1) {
            fprintf(stderr, "Unable to read rest of header.\n");
            return -1;
        }

        file_mark = header[0];
        if (file_mark != 0x20 && file_mark != 0x21) {
            fprintf(stderr, "%s is not a CEL image file.\n", celfile);
            return -1;
        }

        bpp    = header[1];
        width  = parse_uint16(header+4);
        height = parse_uint16(header+6);
        offx   = parse_uint16(header+8);
        offy   = parse_uint16(header+10);
    }

    if (output_offset) {
        fprintf(stdout, "%d %d\n", offx, offy);
    }

    if (debug) {
        fprintf(stderr, "Width: %d, height: %d \n", width, height);
        fprintf(stderr, "Offx: %d, offy: %d \n", offx, offy);
        fprintf(stderr, "Bits-per-pixel: %d \n", bpp);
    }

    if (width < 0 || height < 0) {
        fprintf(stderr, "Invalid width or height.");
        return -1;
    }

    // open fppnm stream
    fppnm = fopen(pnmfile, "w+");
    if (!fppnm) {
        perror(pnmfile);
        return -1;
    }
    // write header
    fprintf(fppnm, "P3\n");
    fprintf(fppnm, "%d %d\n", width, height);
    fprintf(fppnm, "255\n");

    // if it's a Cherry KiSS file, create a grayscale PGM file for alpha transparency
    if (bpp == 32) {
        fppgm = fopen("out.pgm", "w+");
        if (!fppgm) {
            perror("out.pgm");
            return -1;
        }
        fprintf(fppgm, "P5\n");
        fprintf(fppgm, "%d %d\n", width, height);
        fprintf(fppgm, "255\n");
    }

    unsigned char line [width * 4];

    for (i = 0; i < height && !feof(fpcel); ++i) {
    // for each row in the picture

        switch (bpp) {
            case 4:
                for (j = 0; j < (width+1)/2; j++) {
                // for every two pixels in each row

                    // read one byte into the buffer
                    unsigned char buffer;
                    n_read = fread(&buffer, 1, 1, fpcel);
                    if (n_read < 1) {
                        fprintf(stderr,
                                "Error reading pixels at row %d, column %d\n", i, j);
                        return -1;
                    }

                    // split the byte into two numbers
                    int num1 = (buffer & 0xf0) >> 4;
                    int num2 = buffer & 0x0f;

                    // print the numbers if in debug mode
                    if (debug > 1) {
                        fprintf(stderr, "%d ", num1);
                        fprintf(stderr, "%d ", num2);
                    }

                    if ((width / 2 == 0) || j < (width/2)) {
                    // if the width is even, or it's not the end of a line
                        // lookup the R, G, and B values and print
                        // both pixels to the output file
                        fprintf(fppnm, "%u %u %u %u %u %u ",
                            (unsigned) palette[num1*3],
                            (unsigned) palette[num1*3+1],
                            (unsigned) palette[num1*3+2],
                            (unsigned) palette[num2*3],
                            (unsigned) palette[num2*3+1],
                            (unsigned) palette[num2*3+2]);
                    }
                    else {
                    // if this is the end of an uneven line
                        // print just the first pixel
                        fprintf(fppnm, "%u %u %u ",
                            (unsigned) palette[num1*3],
                            (unsigned) palette[num1*3+1],
                            (unsigned) palette[num1*3+2]);
                    }
                }

                break;
            case 8:
                for (j = 0; j < width; j++) {
                // for each pixel in each row

                    // read one byte into the buffer,
                    unsigned char buffer;
                    n_read = fread(&buffer, 1, 1, fpcel);
                    if (n_read < 1) {
                        fprintf(stderr,
                                "Error reading pixels at row %d, column %d\n", i, j);
                        return -1;
                    }
                    // convert the byte to an int
                    int num;
                    num = buffer;

                    // print the number if in debug mode
                    if (debug > 1) {
                        fprintf(stderr, "%d ", num);
                    }

                    // lookup the R, G, and B values and print to file
                    fprintf(fppnm, "%u %u %u ", (unsigned) palette[num*3],
                                                (unsigned) palette[num*3+1],
                                                (unsigned) palette[num*3+2]);

                }
                break;
            case 32:
                // read a line of pixels
                n_read = fread(line, width*4, 1, fpcel);
                if (n_read < 1) {
                    fprintf(stderr,
                            "Error reading pixels at row %d, column %d\n", i, j);
                    return -1;
                }

                for (j = 0; j < width; j++) {

                    // cels are stored in Blue, Green, Red order
                    int blue, green, red;
                    blue = line[j*4];
                    green = line[j*4+1];
                    red = line[j*4+2];

                    // 8-bit alpha channel
                    unsigned char alpha;
                    alpha = line[j*4+3];

                    if (debug > 1) {
                        fprintf(stderr, "%u%u%u-%X ", red, green, blue, alpha);
                    }

                    // Print pixels to file
                    fprintf(fppnm, "%u %u %u ", (unsigned) red,
                                                (unsigned) green,
                                                (unsigned) blue);
                    // Write the one byte from alpha to fppgm
                    fwrite(&alpha, 1, 1, fppgm);
                }
                break;

        }
        // end the row with a newline
        if (debug > 1) {
            fprintf(stderr, " \n");
        }
        fprintf(fppnm, "\n");
    }

    fclose(fpcel);
    fclose(fppnm);

    return 0;
}

static int read_palette(const char *palfile) {
    FILE   *fppal;
    unsigned char header[32],
            file_mark,
            buffer[2],
            bpp;
    int     colors;
    int     i;
    size_t  n_read;

    fppal = fopen(palfile, "r");
    if (!fppal) {
        perror(palfile);
        return -1;
    }

    n_read = fread(header, 4, 1, fppal);
    if (n_read < 1) {
        fprintf(stderr, "Bad palette header.\n");
        return -1;
    }

    if (strncmp ((const char *) header, "KiSS", 4)) {
        if (debug) {
            fprintf(stderr, "Old style palette\n");
        }

        colors = 16;

        // seeks back to the start of the file
        fseek(fppal, 0, SEEK_SET);

        // the rest of this is the same as "case 12" below"
        // DRY?
        // could pull the switch case out of the if and
        // switch on colors?
        for (i = 0; i < colors; i++) {
            n_read = fread(buffer, 1, 2, fppal);
            if (n_read < 2) {
                fprintf(stderr, "Error reading palette.");
                return -1;
            }

            palette[i*3+0] =  buffer[0] & 0xf0;
            palette[i*3+1] = (buffer[1] & 0x0f) << 4;
            palette[i*3+2] = (buffer[0] & 0x0f) << 4;
        }
    }
    else {
        fprintf(stderr, "New style palette\n");

        n_read = fread(header+4, 28, 1, fppal);
        if (n_read < 1) {
            fprintf(stderr, "Can't read palette header after \"KiSS\".\n");
        }

        file_mark = header[4];
        if (file_mark != 0x10) {
            fprintf(stderr, "Filemark should be %c and is actually %c.\n", 0x10, file_mark);
            return -1;
        }

        bpp = header[5];
        colors = parse_uint16(header+8);

        if (debug) {
            fprintf(stderr,"Bits per pixel: %d \n", bpp);
            fprintf(stderr, "Number colors: %d \n", colors);
        }

        switch (bpp) {
            case 12:
                //for each color
                for (i = 0; i < colors; i++) {
                    // read two bytes into the buffer
                    n_read = fread(buffer, 1, 2, fppal);
                    if (n_read < 2) {
                        fprintf(stderr, "Error while reading palette.");
                        return -1;
                    }

                    // three colors are stored in two bytes
                    //     buffer[0]    |    buffer[1]
                    //  0 1 2 3 4 5 6 7 | 0 1 2 3 4 5 6 7
                    //  color 1 color 3 |   ---   color 2
                    palette[i*3+0] =  buffer[0] & 0xf0;
                    palette[i*3+1] = (buffer[1] & 0x0f) * 16;
                    palette[i*3+2] = (buffer[0] & 0x0f) * 16;
                }
                break;
            case 24:
                // goes through fppal grabs 3 bytes colors times
                // and stores it in palette
                n_read = fread(palette, 3, colors, fppal);
                if (n_read < colors) {
                    fprintf(stderr, "Error while reading palette.");
                    return -1;
                }
                break;
            default:
                fprintf(stderr, "Invalid bits-per-pixel of %d", bpp);
                return -1;
        }
    }

    sprintf(transparent, "rgb:%x/%x/%x", (unsigned) palette[0],
                                         (unsigned) palette[1],
                                         (unsigned) palette[2]);

    fclose(fppal);

    return 0;
}

int main (int argc, char *argv[]) {
    int err;

    char *input_file;
    char *palette_file;
    char *output_file;

    if (strcmp(argv[1], "-c") == 0) {
      palette_file = argv[3];
      fprintf(stderr,"Read palette %s \n", palette_file);
      err = read_palette (palette_file);
      if (err) return 1;
      char * color_string;
      int color_number = strtol(argv[2], &color_string, 10);
      int n = color_number *3;
      fprintf(stdout, "#%02x%02x%02x", (unsigned) palette[n],
                                       (unsigned) palette[n+1],
                                       (unsigned) palette[n+2]);
      return 0;
    }

    if (strcmp(argv[1], "-t") == 0) {
        palette_file = argv[2];
        fprintf(stderr,"Read palette %s \n", palette_file);
        err = read_palette (palette_file);
        if (err) return 1;
        fprintf(stdout, "%s", transparent);
        return 0;
    }

    int ap = 1; // arg pointer

    debug = 0;
    output_offset = 0;

    if (ap < argc && strncmp(argv[ap], "-d", 2) == 0) {
        if (strncmp(argv[ap], "-d2", 3) == 0) {
            debug = 2;
        }
        else {
            debug = 1;
        }
        ++ap;
    }

    if (ap < argc && strcmp(argv[ap], "-o") == 0) {
        output_offset = 1;
        ++ap;
    }

    if (ap+3 == argc) {
        palette_file = argv[ap];
        input_file = argv[ap+1];
        output_file = argv[ap+2];
    }
    else {
        fprintf(stderr, "Usage: cel2png (-d) (-t) (-o) <palette file> <cel file> <out file> \n");
        return 1;
    }

    fprintf(stderr,"Read palette %s \n", palette_file);
    err = read_palette (palette_file);
    if (err) return 1;

    fprintf(stderr,"Read cel %s \n", input_file);
    err = convert_cel (input_file, output_file);
    if (err) return 1;

    fprintf(stderr,"Done \n");

    return 0;
}
