/* r-getsiz.p */

DEFINE VARIABLE bitmapinfo       AS MEMPTR.
DEFINE VARIABLE bitmapinfoheader AS MEMPTR.
DEFINE VARIABLE RGBcolors        AS MEMPTR.

SET-SIZE(bitmapinfo) = 4        /* Pointer to bitmapinfoheader */
                     + 4        /* Pointer to RGBcolors        */
                      .
SET-SIZE(bitmapinfoheader) = 4  /* biSize          */
                           + 4  /* biWidth         */
                           + 4  /* biHeight        */
                           + 2  /* biPlanes        */
                           + 2  /* biBitCount      */
                           + 4  /* biCompression   */
                           + 4  /* biSizeImage     */
                           + 4  /* biXpelsPerMeter */
                           + 4  /* biYPelsPerMeter */
                           + 4  /* biClrUsed       */
                           + 4  /* biClrImportant  */
                            .
SET-SIZE(RGBcolors) = 16 * 4.   /* Array for 16 RGB color values */

DISPLAY 
   GET-SIZE(bitmapinfo) 
      LABEL "Bitmap info structure size" COLON 30 SKIP
   GET-SIZE(bitmapinfoheader) 
      LABEL "Bitmap header structure size" COLON 30 SKIP
   GET-SIZE(RGBcolors)
      LABEL "Bitmap colors array size" COLON 30
WITH SIDE-LABELS.
