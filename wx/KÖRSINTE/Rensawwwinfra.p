/*Rensawwwinfra.p */
/*Anders Olsson Elpool i Umeå AB  17 aug 2021 10:37:39 
 körs inte funkarv ej
*/
{AMERICANEUROPEAN.I}
DEFINE {&NEW} SHARED TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE filtemp NO-UNDO
   FIELD KOPDIR AS CHARACTER
   FIELD KOPFIL AS CHARACTER.
RUN backuppkoll_UI (INPUT "d:\wwwinfratek\").  /*Sökväg till de filer*/
RUN backuppkoll_UI (INPUT "d:\elpoolKOPIOR\").
RUN backuppkoll_UI (INPUT "d:\pc112 D\").
RUN backuppkoll_UI (INPUT "d:\pc120 D\").
RUN backuppkoll_UI (INPUT "d:\pc208 D\").

RUN ftpskicka_UI (INPUT TRUE, INPUT "d:\wwwinfratek\", INPUT "externa\wwwinfratek\").

RUN ftphamta_UI (INPUT FALSE,INPUT 1, INPUT "d:\filer.txt", INPUT "kontor\filer.txt").

EMPTY TEMP-TABLE filtemp NO-ERROR. 

INPUT FROM "d:\filer.txt" convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE filtemp.
   ASSIGN.
   IMPORT filtemp.
END.
INPUT CLOSE.
FOR EACH filtemp WHERE filtemp.KOPDIR MATCHES "*pc112*":
   RUN ftphamta_UI (INPUT FALSE,INPUT 2, INPUT "d:\pc112" + filtemp.KOPFIL, INPUT "kontord\pc112" + filtemp.KOPFIL).
END.
FOR EACH filtemp WHERE filtemp.KOPDIR MATCHES "*pc120*":
   RUN ftphamta_UI (INPUT FALSE,INPUT 2, INPUT "d:\pc120" + filtemp.KOPFIL, INPUT "kontord\pc120" + filtemp.KOPFIL).
END.
FOR EACH filtemp WHERE filtemp.KOPDIR MATCHES "*pc208*":
   RUN ftphamta_UI (INPUT FALSE,INPUT 2, INPUT "d:\pc208" + filtemp.KOPFIL, INPUT "kontord\pc208" + filtemp.KOPFIL).
END.
PROCEDURE ftphamta_UI :
   DEFINE INPUT PARAMETER skicka      AS LOGICAL NO-UNDO.    /*skicka = true hämta = false*/
   DEFINE INPUT  PARAMETER hur AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER prognamn AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER prognamn2 AS CHARACTER NO-UNDO.
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT skicka, INPUT hur,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se:22", OUTPUT TABLE felmeddftptemp).
  
  
   OUTPUT TO VALUE("d:\ftpresult.txt") APPEND.
   FOR EACH felmeddftptemp:
      PUT felmeddftptemp.FELMEDD SKIP.
      DELETE felmeddftptemp.
   END.    
   OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE ftpskicka_UI :
   DEFINE INPUT PARAMETER skicka      AS LOGICAL NO-UNDO.    /*skicka = true hämta = false*/
   DEFINE INPUT  PARAMETER searchdir AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   FIND FIRST filtemp WHERE filtemp.KOPDIR = prognamn NO-LOCK NO-ERROR.
   prognamn = searchdir + filtemp.KOPFIL.
   prognamn2 = prognamn2 + filtemp.KOPFIL.
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT skicka, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se:22", OUTPUT TABLE felmeddftptemp).
   OUTPUT TO VALUE(searchdir + "ftpresult.txt") APPEND.
   FOR EACH felmeddftptemp:
      PUT felmeddftptemp.FELMEDD.
      DELETE felmeddftptemp.
   END.    
   OUTPUT CLOSE.
END PROCEDURE.


{EUROPEANAMERICAN.I}
PROCEDURE backuppkoll_UI :
   DEFINE INPUT  PARAMETER  searchdir AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE kolldat AS DATE NO-UNDO.
   DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(70)" LABEL "File" NO-UNDO.
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE kolldatvar AS CHARACTER NO-UNDO.
   kolldat = ?.
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn MATCHES "*.MRIMG" THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         /*
         kolldatvar = STRING(FILE-INFO:FILE-MOD-DATE).
         */
         IF kolldat = ? THEN DO: 
            kolldat = FILE-INFO:FILE-MOD-DATE.
            FIND FIRST filtemp WHERE filtemp.KOPDIR = searchdir NO-LOCK NO-ERROR.
            IF NOT AVAILABLE filtemp THEN DO:
               CREATE filtemp.
            END.   
            ASSIGN
            filtemp.KOPDIR = searchdir
            filtemp.KOPFIL = filnamn.
         END.    
         IF kolldat < FILE-INFO:FILE-MOD-DATE THEN  DO:
            kolldat = FILE-INFO:FILE-MOD-DATE.
            FIND FIRST filtemp WHERE filtemp.KOPDIR = searchdir NO-LOCK NO-ERROR.
            IF NOT AVAILABLE filtemp THEN DO:
               CREATE filtemp.
            END.   
            ASSIGN
            filtemp.KOPDIR = searchdir
            filtemp.KOPFIL = filnamn.        
         END.                      
      END.
   END. 
   INPUT CLOSE.
   
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
      IF filnamn MATCHES "*.MRIMG" THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         IF FILE-INFO:FILE-MOD-DATE < TODAY - 28 THEN DO:
            IF kolldat > FILE-INFO:FILE-MOD-DATE THEN DO:
               OS-DELETE VALUE(searchdir + filnamn) NO-ERROR.               
            END.   
         END.   
      END.
   END.
   INPUT CLOSE.      
END PROCEDURE.      
