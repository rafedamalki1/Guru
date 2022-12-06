/* r-seek.p */
/* This procedure seeks to the end-of-file, collects the seek address,
   and writes a record. The record is subsequently retrieved using the
   SEEK statement on the stashed seek address.
*/
  DEFINE VAR savepos AS INT.
  DEFINE VAR c       AS CHAR FORMAT "x(20)".
  OUTPUT TO seek.out APPEND NO-ECHO.
  savepos = SEEK(OUTPUT).
  PUT UNFORMATTED "abcdefg" SKIP.
  OUTPUT CLOSE.
  INPUT FROM seek.out NO-ECHO.
  SEEK INPUT TO savepos.
  SET c.
  DISPLAY c.
  INPUT CLOSE.
