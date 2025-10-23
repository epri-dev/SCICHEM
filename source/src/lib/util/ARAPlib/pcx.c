/*
======================================================================
pcx.c

PCX load and save routines.

Ernie Wright
26 Oct 95
====================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys\stat.h>
#include <windows.h>

typedef struct {
  unsigned char red, gre, blu;
} PCX_RGB3;

typedef struct {
  char id;
  char version;
  char encoding;
  char planebits;
  short x0;
  short y0;
  short x1;
  short y1;
  short hres;
  short vres;
  PCX_RGB3 pal[16];
  char reserved1;
  char nplanes;
  short rowbytes;
  short palkey;
  short hscreen;
  short vscreen;
  char reserved2[54];
} PCX_HEADER;

typedef struct {
  PCX_HEADER hdr;
  unsigned char *body;
  unsigned long bodysize;
  PCX_RGB3 pal[256];
} PCX_FILE;

static BITMAPINFO *alloc_bmi(int width, int height, int *rowbytes);
static PCX_FILE *get_file(char *filename);
static int get_header(FILE *fp, PCX_HEADER *hdr);
static int put_header(FILE *fp, int w, int h);
static int put_pal(FILE *fp, RGBQUAD *pal);
static int unpack(unsigned char **psrc, unsigned char *dst, int size);
static int pack(unsigned char *src, unsigned char *dst, int size);

/*
======================================================================
pcxSize()

Return the width and height of a 256-color PCX file.

INPUTS
   filename    PCX image file to examine
   width       pixel width
   height      pixel height

RESULTS
   Reads the PCX header and modifies the width and height arguments.
   Returns TRUE if successful.  If the file couldn't be opened, or it
   wasn't a 256-color PCX file, width and height aren't changed, and
   the function returns FALSE.

Use this function to determine the correct dimensions for the bitmap
passed to pcxLoad().  SetDIBits(), used during PCX loading, can cause
addressing errors when the dimensions of the source and destination
don't match.
====================================================================== */

int WINAPI pcxSize(char *filename, int *width, int *height) {
  PCX_HEADER hdr;
  FILE *fp;
  int ok = FALSE;

  if (fp = fopen(filename, "rb")) {
    ok = get_header(fp, &hdr);
    fclose(fp);
    if (ok) {
      *width = hdr.x1 - hdr.x0 + 1;
      *height = hdr.y1 - hdr.y0 + 1;
      return TRUE;
    }
  }
  return FALSE;
}

/*
======================================================================
pcxLoad()

Load a 256-color PCX file into a bitmap referenced by a bitmap handle.

INPUTS
   filename    PCX image file to load
   hdc         device context
   hbm         bitmap handle

RESULTS
   Loads the PCX file and copies the image to the bitmap.  Returns
   TRUE if successful, otherwise FALSE.

The bitmap's palette is the one selected into the device context.

This function uses Windows SetDIBits() to transfer the image pixels to
the bitmap, so the bitmap can't be selected into a device context when
pcxLoad() is called, and the bitmap's dimensions must match those of
the PCX image.  You can use pcxSize() to get the PCX image dimensions.
====================================================================== */

int WINAPI pcxLoad(char *filename, HDC hdc, HBITMAP hbm) {
  PCX_FILE *pcx;
  BITMAPINFO *bmi = NULL;
  unsigned char *src, *dst;
  int i, w, h, rb, y, ok = FALSE;

  if (!(pcx = get_file(filename))) {
    return 0;
  }
  w = pcx->hdr.x1 - pcx->hdr.x0 + 1;
  h = pcx->hdr.y1 - pcx->hdr.y0 + 1;
  if (!(bmi = alloc_bmi(w, h, &rb))) {
    goto Finish;
  }

  src = pcx->body;
  dst = (char *)bmi->bmiColors + 1024 + rb * h;
  for (y = 0; y < h; y++) {
    dst -= rb;
    if (!unpack(&src, dst, pcx->hdr.rowbytes)) {
      goto Finish;
    }
  }
  for (i = 0; i < 256; i++) {
    bmi->bmiColors[i].rgbRed = pcx->pal[i].red;
    bmi->bmiColors[i].rgbGreen = pcx->pal[i].gre;
    bmi->bmiColors[i].rgbBlue = pcx->pal[i].blu;
  }

  SetDIBits(hdc, hbm, 0, h, dst, bmi, DIB_RGB_COLORS);

  ok = TRUE;

Finish:
  free(pcx->body);
  free(pcx);
  if (bmi) {
    free(bmi);
  }
  return ok;
}

/*
======================================================================
pcxSave()

Save a bitmap as a 256-color PCX file.

INPUTS
   filename    PCX image file to create
   hdc         device context
   hbm         bitmap handle

RESULTS
   Saves the bitmap to the PCX file.  Returns TRUE if successful.  If
   something goes wrong, the partial file is deleted and the function
   returns FALSE.

The bitmap's palette is the one selected into the device context.

This function uses Windows GetDIBits() to transfer the image pixels
from the bitmap, so the bitmap can't be selected into a device context
when pcxSave() is called.
====================================================================== */

int WINAPI pcxSave(char *filename, HDC hdc, HBITMAP hbm) {
  BITMAP bm;
  BITMAPINFO *bmi;
  FILE *fp;
  unsigned char *src, *dst;
  int w, rb, y, n, ok = FALSE;

  if (!GetObject(hbm, sizeof(bm), &bm)) {
    return FALSE;
  }
  if (!(bmi = alloc_bmi(bm.bmWidth, bm.bmHeight, &rb))) {
    return FALSE;
  }

  src = (char *)bmi->bmiColors + 1024;
  dst = calloc(1, bm.bmWidth + bm.bmWidth / 32);
  fp = fopen(filename, "wb");
  if (!dst || !fp) {
    goto Finish;
  }

  GetDIBits(hdc, hbm, 0, bm.bmHeight, src, bmi, DIB_RGB_COLORS);

  if (!put_header(fp, bm.bmWidth, bm.bmHeight)) {
    goto Finish;
  }

  w = bm.bmWidth + (bm.bmWidth & 1);
  src = (char *)bmi->bmiColors + 1024 + rb * (bm.bmHeight - 1);
  for (y = 0; y < bm.bmHeight; y++) {
    n = pack(src, dst, w);
    if (1 != fwrite(dst, n, 1, fp)) {
      goto Finish;
    }
    src -= rb;
  }

  if (!put_pal(fp, bmi->bmiColors)) {
    goto Finish;
  }

  ok = TRUE;

Finish:
  if (fp) {
    fclose(fp);
    if (!ok) {
      remove(filename);
    }
  }
  if (dst) {
    free(dst);
  }
  if (bmi) {
    free(bmi);
  }
  return ok;
}

/*
======================================================================
get_file()

Read a 256-color PCX file into memory.

INPUTS
   filename    PCX image file to load

RESULTS
   Loads the PCX image and returns a PCX_FILE, or NULL if something
   goes wrong.
====================================================================== */

static PCX_FILE *get_file(char *filename) {
  FILE *fp;
  PCX_FILE *pcx;
  struct stat s;

  if (pcx = calloc(1, sizeof(PCX_FILE))) {
    if (!stat(filename, &s)) {
      pcx->bodysize = s.st_size - 897;
      if (pcx->body = malloc(pcx->bodysize)) {
        if (fp = fopen(filename, "rb")) {
          if (get_header(fp, &pcx->hdr)) {
            if (1 == fread(pcx->body, pcx->bodysize, 1, fp)) {
              if (0x0C == getc(fp)) {
                if (1 == fread(pcx->pal, 768, 1, fp)) {
                  fclose(fp);
                  return pcx;
                }
              }
            }
          }
          fclose(fp);
        }
        free(pcx->body);
      }
    }
    free(pcx);
  }
  return NULL;
}

/*
======================================================================
alloc_bmi()

Allocate memory for a device-independent bitmap.

INPUTS
   width       image width in pixels
   height      image height in pixels
   rowbytes    storage for number of bytes per row

RESULTS
   Allocates memory for a 256-color image in CF_DIB format.  If row-
   bytes isn't NULL, it's set to the number of bytes per scanline.
   This may be different from the pixel width because DIB scanlines
   must be padded to a longword boundary.  The BITMAPINFOHEADER is
   initialized with values appropriate to a 256-color DIB, and the
   memory block is returned as a pointer to BITMAPINFO.
====================================================================== */

static BITMAPINFO *alloc_bmi(int width, int height, int *rowbytes) {
  BITMAPINFO *bmi;
  int rb;

  rb = ((width + 3) >> 2) << 2;
  if (rowbytes) {
    *rowbytes = rb;
  }
  bmi = malloc(sizeof(BITMAPINFOHEADER) + 1024 + rb * height);
  if (!bmi) {
    return NULL;
  }

  bmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  bmi->bmiHeader.biWidth = width;
  bmi->bmiHeader.biHeight = height;
  bmi->bmiHeader.biPlanes = 1;
  bmi->bmiHeader.biBitCount = 8;
  bmi->bmiHeader.biCompression = BI_RGB;
  bmi->bmiHeader.biSizeImage = rb * height;
  bmi->bmiHeader.biXPelsPerMeter = 3780;
  bmi->bmiHeader.biYPelsPerMeter = 3780;
  bmi->bmiHeader.biClrUsed = 256;
  bmi->bmiHeader.biClrImportant = 0;

  return bmi;
}

/*
======================================================================
get_header()

Read and verify a PCX version 5 file header.

INPUTS
   fp          file to read
   hdr         PCX header to receive file bytes

RESULTS
   Reads sizeof( PCX_HEADER ) bytes and verifies that they belong to a
   PCX file that we can interpret.  If this is a PCX file and we know
   how to load it, the function returns TRUE, with the contents of the
   file header in hdr, otherwise FALSE.
====================================================================== */

static int get_header(FILE *fp, PCX_HEADER *hdr) {
  if (1 != fread(hdr, sizeof(PCX_HEADER), 1, fp)) {
    return FALSE;
  }

  return hdr->id == 0x0A && hdr->encoding == 1 && hdr->version == 5 &&
         hdr->planebits == 8 && hdr->nplanes == 1;
}

/*
======================================================================
put_header()

Write a PCX version 5 file header.

INPUTS
   fp          file to write
   w           width of the image in pixels
   h           height of the image in pixels

RESULTS
   Fills the 128-byte PCX header with information appropriate to an 8-
   bit version 5 image.  Returns TRUE if the write succeeds, otherwise
   FALSE.
====================================================================== */

static int put_header(FILE *fp, int w, int h) {
  PCX_HEADER hdr;

  hdr.id = 0x0A;
  hdr.version = 5;
  hdr.encoding = 1;
  hdr.planebits = 8;
  hdr.x0 = 0;
  hdr.y0 = 0;
  hdr.x1 = w - 1;
  hdr.y1 = h - 1;
  hdr.hres = w;
  hdr.vres = h;
  hdr.reserved1 = 0;
  hdr.nplanes = 1;
  hdr.rowbytes = w + (w & 1);
  hdr.palkey = 1;
  hdr.hscreen = 0;
  hdr.vscreen = 0;

  memset(hdr.pal, 0, 48);
  memset(hdr.reserved2, 0, 54);

  return (1 == fwrite(&hdr, sizeof(PCX_HEADER), 1, fp));
}

/*
======================================================================
put_pal()

Write PCX version 5 palette.

INPUTS
   fp          file to write
   pal         DIB style palette

RESULTS
   Writes the palette and returns TRUE if successful, FALSE otherwise.

The file pointer must be positioned at the end of the bitmap data when
this function is called.
====================================================================== */

static int put_pal(FILE *fp, RGBQUAD *pal) {
  PCX_RGB3 *buf;
  int i, ok = FALSE;

  buf = calloc(256, sizeof(PCX_RGB3));
  if (!buf) {
    return FALSE;
  }
  for (i = 0; i < 256; i++) {
    buf[i].red = pal[i].rgbRed;
    buf[i].gre = pal[i].rgbGreen;
    buf[i].blu = pal[i].rgbBlue;
  }
  if (!putc(0x0C, fp)) {
    goto Finish;
  }
  if (1 != fwrite(buf, 768, 1, fp)) {
    goto Finish;
  }
  ok = TRUE;

Finish:
  free(buf);
  return ok;
}

/*
======================================================================
unpack()

Decompress PCX run-length encoded bytes.

INPUTS
   psrc        pointer to buffer containing compressed bytes
   dst         buffer to receive decompressed bytes
   size        the expected number of decompressed bytes

RESULTS
   Decompresses bytes from *psrc into dst.  Returns FALSE if decom-
   pression results in more than size bytes (implying a corrupt or
   non-PCX source), otherwise TRUE.
====================================================================== */

static int unpack(unsigned char **psrc, unsigned char *dst, int size) {
  unsigned char n, c, *src = *psrc;

  while (size > 0) {
    n = *src++;

    if ((n & 0xC0) == 0xC0) {
      n &= 0x3F;
      if (n == 0) {
        continue;
      }
      size -= n;
      if (size < 0) {
        return 0;
      }
      c = *src++;
      while (n--) {
        *dst++ = c;
      }
    } else {
      *dst++ = n;
      --size;
    }
  }

  *psrc = src;
  return 1;
}

/*
======================================================================
pack()

Perform PCX run-length encoding.

INPUTS
   src         buffer containing uncompressed bytes
   dst         buffer to receive compressed bytes
   size        the number of bytes to compress

RESULTS
   Compresses bytes into the destination buffer and returns the size
   of the destination in bytes.
====================================================================== */

#define DUMP 0
#define RUN 1
#define MINRUN 2
#define MAXRUN 63

static int pack(unsigned char *src, unsigned char *dst, int size) {
  unsigned char c, lastc, sbyte;
  int mode = DUMP, rstart = 0, putsize = 0, sp = 1, i;

  lastc = *src;
  size--;

  while (size > 0) {
    c = *(src + sp);
    sp++;
    size--;

    switch (mode) {
      case DUMP:
        if (c == lastc) {
          if ((sp - rstart) >= MINRUN) {
            if (rstart > 0) {
              for (i = 0; i < rstart; i++) {
                sbyte = *(src + i);
                if ((sbyte & 0xC0) == 0xC0) {
                  *dst++ = 0xC1;
                  putsize++;
                }
                *dst++ = sbyte;
              }
              putsize += rstart;
            }
            mode = RUN;
          } else if (rstart == 0) {
            mode = RUN;
          }
        } else {
          rstart = sp - 1;
        }
        break;

      case RUN:
        if ((c != lastc) || (sp - rstart > MAXRUN)) {
          *dst++ = (sp - (rstart + 1)) | 0xC0;
          *dst++ = lastc;
          putsize += 2;
          src += sp - 1;
          sp = 1;
          rstart = 0;
          mode = DUMP;
        }
    }
    lastc = c;
  }

  switch (mode) {
    case DUMP:
      for (i = 0; i < sp; i++) {
        sbyte = *(src + i);
        if ((sbyte & 0xC0) == 0xC0) {
          *dst++ = 0xC1;
          putsize++;
        }
        *dst++ = sbyte;
      }
      putsize += sp;
      break;

    case RUN:
      *dst++ = (sp - rstart) | 0xC0;
      *dst = lastc;
      putsize += 2;
  }
  return putsize;
}
