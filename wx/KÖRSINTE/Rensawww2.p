/*Rensawww2.p  körs ej längre */
/*Anders Olsson Elpool i Umeå AB  7 aug 2017 11:08:46 
flyttra alla filer på ftp till c:\GURUZIP på www2 dvs alla guru prog samt st databasen.

*/
/*Anders Olsson Elpool i Umeå AB  17 aug 2021 10:37:39 
 körs inte
 */
{AMERICANEUROPEAN.I}
DEFINE TEMP-TABLE filtemp NO-UNDO
   FIELD KOPDIR AS CHARACTER
   FIELD KOPFIL AS CHARACTER.
DEFINE TEMP-TABLE felmeddftptemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL AS INTEGER.
RUN FlyttaBkc (INPUT "C:\elpool\FTP\Upload\*.*", INPUT "C:\GURUZIP\").

RUN backuppkoll_UI (INPUT "C:\GURUZIP\").  /*Sökväg till de filer*/


PROCEDURE FlyttaBkc :
   DEFINE INPUT  PARAMETER sourcefilename AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER copyfilename AS CHARACTER NO-UNDO.
   DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
   guruwtidir = "xcopy " + sourcefilename + " " + copyfilename + " /c/d/f/s/e/y ".
   OS-COMMAND SILENT VALUE(guruwtidir). 
   sourcefilename = SUBSTRING(sourcefilename,1,INDEX(sourcefilename,"*") - 1).
   OS-DELETE VALUE(sourcefilename) RECURSIVE.
   OS-CREATE-DIR VALUE(sourcefilename).  
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
      IF filnamn MATCHES "GURU*.ZIP" THEN DO: 
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
      IF filnamn MATCHES "GURU*.ZIP" THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         IF FILE-INFO:FILE-MOD-DATE < TODAY - 32 THEN DO:
            IF kolldat > FILE-INFO:FILE-MOD-DATE THEN DO:
               OS-DELETE VALUE(searchdir + filnamn) NO-ERROR.               
            END.   
         END.   
      END.
   END.
   INPUT CLOSE.      
END PROCEDURE.      
