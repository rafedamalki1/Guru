/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* This routine runs quoter on <infile>, producing <outfile>.*/

/* NOTE: quotes are not available as delimiters!!! (Some     */
/*  OS have problems with passing them on <hutegger 94/06>   */

DEFINE INPUT PARAMETER infile  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delim   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER colum   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER outfile AS CHARACTER NO-UNDO.
DEFINE VAR tmp AS CHAR.

IF delim = '"'
  THEN DO:
    MESSAGE "Quotes are not allowed as delimiter."
            VIEW-AS ALERT-BOX ERROR.
    RETURN /*ERROR*/.        
    END.

IF delim = ?          THEN delim = "".
ELSE IF OPSYS = "VMS" THEN delim = "/DELIMITER=~"" + delim + "~"".
ELSE                       delim = "-d ~"" + delim + "~" ".

IF colum = ?          THEN colum = "".
ELSE IF OPSYS = "VMS" THEN colum = "/COLUMN=~"" + colum + "~"".
ELSE                       colum = "-c ~"" + colum + "~" ".

IF CAN-DO("MSDOS,WIN32",OPSYS) THEN DO:
  /* There is a limit to the size of a DOS command issued from MS-WINDOWS.
     So, we generate a batch file to disk, execute it, then clean it up. */
  IF "{&WINDOW-SYSTEM}" begins "MS-WIN" THEN DO:
    /* generate a temp file for the batch command */
    RUN "adecomm/_tmpfile.p" (INPUT "qt", INPUT ".bat", OUTPUT tmp).
    OUTPUT TO VALUE(tmp) NO-ECHO.
    PUT UNFORMATTED
      "QUOTER "
      delim  " " 
      colum  " " 
      infile " " 
      ">" outfile.
    OUTPUT CLOSE.
    DOS SILENT VALUE(tmp).
    OS-DELETE VALUE(tmp).
  END.  /* MS-WINDOWS */
  
  ELSE  /* DOS TTY    */
    DOS SILENT quoter
      VALUE(delim)
      VALUE(colum)
      VALUE(infile)
      VALUE(">" + outfile).
END.  /* CAN-DO("MSDOS,WIN32",OPSYS) */

ELSE IF OPSYS = "OS2" THEN
  OS2 SILENT quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).

ELSE IF OPSYS = "UNIX" THEN
  UNIX SILENT quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).

ELSE IF OPSYS = "VMS" THEN
  VMS SILENT PROGRESS/TOOLS=QUOTER
    VALUE(delim)
    VALUE(colum)
    VALUE("/OUTPUT=" + outfile)
    VALUE(infile).

ELSE IF OPSYS = "BTOS" THEN DO:
  IF delim = ? AND colum = ? THEN
    BTOS SILENT OS-QUOTER VALUE(infile) VALUE(outfile).
  ELSE
    BTOS SILENT VALUE(SEARCH("Quoter.Run")) Quoter
      VALUE(delim)
      VALUE(colum)
      VALUE(infile)
      VALUE(">" + outfile).
END.

ELSE MESSAGE "osquoter.p: Unknown Operating System -" OPSYS.

RETURN.
