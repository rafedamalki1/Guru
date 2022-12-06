/* r-opfl.p

Sample program to read and display data from a file using CTOS Requests.

*/

DEF NEW SHARED VAR ercret  AS INTEGER.               /* error value    */
DEF NEW SHARED VAR file    AS CHARACTER.             /* file name      */
DEF NEW SHARED VAR passwd  AS CHARACTER INITIAL "".  /* file password  */
DEF NEW SHARED VAR data    AS CHARACTER.             /* Data read      */
DEF NEW SHARED VAR fh      AS INTEGER.               /* file handle    */
DEF NEW SHARED VAR byteret AS INTEGER.               /* num bytes read */
DEF NEW SHARED VAR resv    AS INTEGER INITIAL 0.     /* control reserved */
DEF NEW SHARED VAR mode    AS INTEGER INITIAL 28018. /* open mode read */

file = "[sys]<sys>.user".

/* open file */
CTOS OS-REQUEST 4 ercret 3 2 1 0 mode 0 %c[60] file %c[12] passwd %i fh.

IF ercret <> 0 THEN DO:
    MESSAGE "Open File Request Failed - Error: " ercret.
    BELL.  PAUSE.  LEAVE.
    END.

/* read from file */
CTOS OS-REQUEST 35 ercret 3 0 2 fh resv resv %c[512] data %i byteret.

DISPLAY   byteret LABEL "Number of Bytes Read"
  SKIP(1) data    FORMAT "x(80)" NO-LABEL
  WITH SIDE-LABELS NO-BOX.

IF ercret <> 0 THEN DO:
    MESSAGE "Read File Request Failed - Error: " ercret.
    BELL.  PAUSE.  LEAVE.
    END.

/* close file */
CTOS OS-REQUEST 10 ercret 1 0 0 fh.

IF ercret <> 0 THEN DO:
    MESSAGE "Close File Request Failed - Error: " ercret.
    BELL.  PAUSE.  LEAVE.
    END.
