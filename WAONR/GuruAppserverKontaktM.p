
/*------------------------------------------------------------------------
    File        : GuruAppserverKontaktM.p
    Purpose     : sÅ ATT MAN INTE TAPPAR KONTAKTEN VID INAKTIVITET

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Dec 05 15:20:31 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE kontakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE intProcessHandle AS INTEGER NO-UNDO.

PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
   DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.

RUN GetCurrentProcessId (OUTPUT intProcessHandle).
/*Anders Olsson Elpool i Umeå AB  20 sep 2017 15:07:18 
Om man vill ha loggning 

RUN Kontakt.
*/
 
PROCEDURE Kontakt:
   kontakt = TRUE.
   OUTPUT TO "kontakt.txt" APPEND.
   PUT UNFORMATTED  "multi " STRING(NOW) " " intProcessHandle SKIP.
   OUTPUT CLOSE.
END PROCEDURE.