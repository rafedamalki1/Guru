/*
     Filename: XLADDABLOBFIL.P
      Created: 2004.02.17 10:58ELPAO     
     Modified: 
*/
{BLOB.I}
/* INPUT FROM "C:\Protemp9\blobcomp.d" convert target "iso8859-1" source "iso8859-1" NO-ECHO. */
/* REPEAT:                                                                                    */
/*    CREATE blobcomptemp.                                                                    */
/*    IMPORT blobcomptemp.                                                                    */
/*    DISP blobcomptemp.                                                                      */
/* END.                                                                                       */
/* INPUT CLOSE.                                                                               */
/* DELETE blobcomptemp.                                                                       */
/* FOR EACH blobcomptemp:                                                                     */
/*     DISP blobcomptemp.                                                                     */
/* END.                                                                                       */

   
INPUT FROM "D:\delad\server\pro9s\wrk\blob\blobinfo.d" convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE BLOBINFO.
   IMPORT BLOBINFO.   
END. 
INPUT CLOSE.
/* DELETE BLOBINFO. */

INPUT FROM "D:\delad\server\pro9s\wrk\blob\blobdata.d" convert target "iso8859-1" source "iso8859-1" NO-ECHO.
REPEAT:
   CREATE BLOBDATA.
   IMPORT BLOBDATA.   
END. 
INPUT CLOSE.
/* DELETE BLOBDATA. */
