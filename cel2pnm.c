#include <stdio.h>
#include <string.h>

// so why does it have to be an unsigned char again?
unsigned char palette[256 * 3];

void convert_cel(const char *celfile, const char *pnmfile) {
    FILE    *fpcel,
            *fppnm;
    char    header[32],
            file_mark,
            bpp;
    int     height, width,
            offx, offy;
    int     i, j, k;

    fpcel = fopen(celfile, "r");

    fread(header, 4, 1, fpcel);

    if (strncmp ((const char *) header, "KiSS", 4)) {
        bpp = 4;
        width = header[0] + (256 * header[1]);
        height = header[2] + (256 * header[3]);
        offx = 0;
        offy = 0;
    }
    else {
        fread(header, 28, 1, fpcel);

        file_mark = header[0];
        
        bpp = header[1];
        width = header[4] + (256 * header[5]);
        height = header[6] + (256 * header[7]);
        offx = header[8] + (256 * header[9]);
        offy = header[10] + (256 * header[11]);
    }

    fppnm = stdout;

    fprintf(fppnm, "P3\n");
    fprintf(fppnm, "%d %d\n", width, height);
    fprintf(fppnm, "255 \n");
    for (i = 0; i < height && !feof(fpcel); ++i) {
    // for each row in the picture
        
        for (j = 0; j < width; j++) {
        // and each pixel in each row
            // get the color from the palette
            int color;
            char buffer;
            fread(&buffer, 1, 1, fpcel);
            color = buffer;
            fprintf(stderr, "Color: %d \n", color);            
            fprintf(fppnm, "%u %u %u ", (unsigned) palette[color*3], 
                                        (unsigned) palette[color*3+1],
                                        (unsigned) palette[color*3+2]);
        }
        // end the row with a newline
        fprintf(fppnm, "\n");
    }

    fclose(fpcel);
    fclose(fppnm);

}

int read_palette(const char *palfile) {
    FILE   *fppal;
    char    header[32],
            file_mark,
            bpp;
    int     colors;
    int     i;
    size_t  n_read;

    fppal = fopen(palfile, "r");

    fread(header, 4, 1, fppal);
    
    if (strncmp ((const char *) header, "KiSS", 4)) {
        fprintf(stderr, "bad header");
        return -1;
    }

    fread(header+4, 28, 1, fppal);

    file_mark = header[4];

    if (file_mark != 0x10) {
        fprintf(stderr, "Filemark should be %c and is actually %c.\n", 0x10, file_mark);
        return -1;
    }

    bpp = header[5];
    fprintf(stderr,"Bits per pixel: %d \n", bpp);
    colors = header[8] + header[9] * 256;
    fprintf(stderr, "Number colors: %d \n", colors);

    //assuming 24-bit colors
    fread(palette, colors, 3, fppal);
        // goes through fppal grabs 3 bytes colors times
        // and stores it in palette

    fclose(fppal);

    return 0;
}


int main (int argc, char *argv[]) {

    fprintf(stderr,"Read palette %s \n", argv[2]);
    read_palette (argv[2]);
    fprintf(stderr,"Read cel \n");
    convert_cel (argv[1], "out.pnm");
    fprintf(stderr,"Done \n");
    return 0;

}
