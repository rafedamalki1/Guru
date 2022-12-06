
/*DBBACKAI.P*/
DEFINE TEMP-TABLE felmeddftptemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE filtemp NO-UNDO
   FIELD KOPDIR AS CHARACTER
   FIELD KOPFIL AS CHARACTER
   FIELD MODDATE AS DATE
   INDEX MODDATE MODDATE. 
     
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE namevar AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER prognamnque AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dbfilename AS CHARACTER NO-UNDO.  /*dabasnamn*/
DEFINE INPUT PARAMETER dbbackupp AS CHARACTER NO-UNDO.   /*vart kopian ska = dbkopia\ */
DEFINE INPUT  PARAMETER wwwftp AS LOGICAL NO-UNDO.
DEFINE VARIABLE dbkopianamn AS CHARACTER NO-UNDO.
dbkopianamn = dbfilename.
IF WEEKDAY(TODAY) = 1 THEN dbkopianamn = dbkopianamn + "son".
ELSE IF WEEKDAY(TODAY) = 2 THEN dbkopianamn = dbkopianamn + "mon".
ELSE IF WEEKDAY(TODAY) = 3 THEN dbkopianamn = dbkopianamn + "tis".
ELSE IF WEEKDAY(TODAY) = 4 THEN dbkopianamn = dbkopianamn + "ons".
ELSE IF WEEKDAY(TODAY) = 5 THEN dbkopianamn = dbkopianamn + "tor".
ELSE IF WEEKDAY(TODAY) = 6 THEN dbkopianamn = dbkopianamn + "fre".
ELSE IF WEEKDAY(TODAY) = 7 THEN dbkopianamn = dbkopianamn + "lor".
IF DAY(TODAY) = 1 THEN DO:
   /*ger tex dbkopianamn = gran07*/
   dbkopianamn = dbfilename.
   dbkopianamn = dbkopianamn + STRING(MONTH(TODAY)).
END.
/*Anders Olsson Elpool i Umeå AB  11 jan 2018 17:02:22 
vi tar inga årsbackupper längre 

IF DAY(TODAY) = 31 AND MONTH(TODAY) = 12 THEN DO:
   /*ger tex dbkopianamn = gran2014*/
   dbkopianamn = dbfilename.
   dbkopianamn = dbkopianamn + STRING(YEAR(TODAY)).
END.
*/
progflytt = SUBSTRING(prognamnque,1,INDEX(prognamnque,"autotid") - 1).
namevar = dbfilename + " BK".
RUN ut_UI (INPUT namevar).

IF SEARCH("probkup.BAT") NE ? THEN DO:
   kommando = SEARCH("probkup.BAT").
   kommando = REPLACE(kommando,"probkup.BAT","probkup").
   RUN ut_UI (INPUT kommando).
END.    
ELSE kommando = "probkup".
/*
kommando = kommando + " online " + progflytt + "db\" + dbfilename + " " + progflytt + dbbackupp + dbfilename + ".bck". 
probkup.BAT  online  E:\delad\pro9s\db\VSAB  -aiarcdir  E:\delad\pro9s\db\ -com E:\delad\pro9s\db\VSABTT.BCK
"C:\DELAD\PRO11\DLC\bin\probkup.BAT online C:\delad\pro9s\db\VSAB C:\delad\pro9s\dbkopia\VSABmon.bck"
"E:\DELAD\PRO11\DLC\bin\probkup.BAT online E:\delad\pro9s\db\VSAB C:\delad\pro9s\dbkopia\VSABmon.bck -aiarcdir E:\delad\pro9s\dbkopia\ -com"

*/

kommando = kommando + " online " + progflytt + "db\" + dbfilename + " " + progflytt + dbbackupp + dbkopianamn + ".bck".
FIND FIRST _connect  WHERE _Connect-Type = "AIMD" NO-LOCK NO-ERROR.
/*AIMGT*/
IF AVAILABLE _connect THEN DO:      /* "c:\delad\pro10s\dbkopia\"*/
   kommando = kommando + " -aiarcdir " + progflytt + dbbackupp + " -com".
END.   
ELSE DO:
   kommando = kommando + " enableaiarchiver -aiarcdir " + progflytt + dbbackupp + " -com".
END.   
DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.  
RUN ut_UI (INPUT kommando).
 RUN os_UI.

RUN ut_UI (INPUT "extra backup"). 
/*Kopier gran.bck till granfre.bck

OS-COPY VALUE(progflytt + dbbackupp + dbfilename + ".bck") VALUE(progflytt + dbbackupp + dbkopianamn + ".bck").
*/
/*
OS-DELETE VALUE(progflytt + dbbackupp + dbfilename + ".bck") NO-ERROR.
*/
RUN ut_UI (INPUT dbfilename). 
/*om ftp till www.guruonweb.se*/
/*
IF wwwftp = TRUE THEN RUN ftpWWW_UI.
*/
/*Anders Olsson Elpool i Umeå AB  15 jan 2019 15:20:07 
ingen ftp 
*/
wwwftp = FALSE.
IF wwwftp = TRUE THEN RUN ftpNetWWW_UI.

/*tar bort gamla filer ??*/
RUN backuppkoll_UI (INPUT progflytt + dbbackupp).
PROCEDURE ftpNetWWW_UI :
   DEFINE VARIABLE FtpResult AS CHARACTER NO-UNDO.
   DEFINE VARIABLE FtpNet AS Helpers.FtpNet.
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   ftpNet = NEW Helpers.FtpNet().
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "ftp://www.guruonweb.se/fortum/" + dbfilename + ".bck".
   
   RUN ut_UI (INPUT "FTP av-" + prognamn). 
   FtpResult = ftpNet:Skicka(CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110),prognamn2,prognamn).
   RUN ut_UI (INPUT "FTP klar-" + FtpResult).
END PROCEDURE.  
PROCEDURE ftpWWW_UI :
  
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ziptill AS CHARACTER NO-UNDO.
   DEFINE VARIABLE zipNamn  AS CHARACTER NO-UNDO.
   /*
   prognamn = progflytt + dbbackupp + dbfilename + ".bck".
   prognamn2 = "FORTUM\" + dbfilename + ".bck".
  */
  /*granfre.bck skickas*/
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "FORTUM\" + dbkopianamn + ".bck".
   RUN ut_UI (INPUT "FTP av-" + prognamn). 
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111),INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   FIND FIRST felmeddftptemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddftptemp THEN DO:
     /* Allt ok*/
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad" THEN DO:
         /*TAR BORT  MÅNADS OCH ÅRS FIL*/
         RUN backdel_UI (INPUT prognamn).
         /*TAR BORT  GARAN.BCK*/
         RUN backfildel_UI (INPUT progflytt + dbbackupp + dbfilename + ".bck").
      END.   
   END.   
   /*svar från ftp*/               
   RUN ftpsvar_UI. 

END PROCEDURE.

PROCEDURE backdel_UI :
   DEFINE INPUT  PARAMETER backdel AS CHARACTER NO-UNDO.
   IF DAY(TODAY) = 1 THEN DO:
      OS-DELETE VALUE(backdel).
   END.
   ELSE IF DAY(TODAY) = 31 AND MONTH(TODAY) = 12 THEN DO:
      OS-DELETE VALUE(backdel).
   END.
   
END PROCEDURE.
PROCEDURE backfildel_UI :
   DEFINE INPUT  PARAMETER backdel AS CHARACTER NO-UNDO.
   OS-DELETE VALUE(backdel).
   
END PROCEDURE.

PROCEDURE ftpsvar_UI :
   FOR EACH felmeddftptemp:
      RUN ut_UI (INPUT felmeddftptemp.FELMEDD).
      DELETE felmeddftptemp.
   END.
END PROCEDURE.
PROCEDURE os_UI:
   OS-COMMAND SILENT VALUE(kommando) .
END PROCEDURE.
PROCEDURE osnos_UI:
   OS-COMMAND VALUE(kommando) .
END PROCEDURE.
PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER uttxt AS CHARACTER FORMAT "X(20)" NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED uttxt " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE backuppkoll_UI :
   /* searchdir = dbkopia*/
   DEFINE INPUT  PARAMETER  searchdir AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO
   VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 3. 
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE kolldatvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hjai1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hjai2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hjai3 AS CHARACTER NO-UNDO.
   RUN ut_UI (INPUT searchdir + " start backuppkoll").
   hjai1 = "*" + dbfilename + ".A1".
   hjai2 = "*" + dbfilename + ".A2".
   hjai3 = "*" + dbfilename + ".A3".
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      IF filnamn MATCHES hjai1 OR filnamn MATCHES hjai2 OR filnamn MATCHES hjai3 THEN DO: 
         FILE-INFO:FILE-NAME = searchdir + filnamn.
         FIND FIRST filtemp WHERE filtemp.KOPDIR = searchdir AND filtemp.KOPFIL = filnamn NO-LOCK NO-ERROR.
         IF NOT AVAILABLE filtemp THEN DO:
            CREATE filtemp.
         END.   
         ASSIGN
         filtemp.KOPDIR = searchdir
         filtemp.KOPFIL = filnamn
         filtemp.MODDATE = FILE-INFO:FILE-MOD-DATE.
                              
      END.
   END. 
   INPUT CLOSE.
   FIND LAST filtemp WHERE filtemp.KOPFIL  MATCHES hjai1 NO-LOCK NO-ERROR.
   IF AVAILABLE filtemp THEN DO:
      DELETE filtemp.
   END.
   FIND LAST filtemp WHERE filtemp.KOPFIL  MATCHES hjai2 NO-LOCK NO-ERROR.
   IF AVAILABLE filtemp THEN DO:
      DELETE filtemp.
   END.
   FIND LAST filtemp WHERE filtemp.KOPFIL  MATCHES hjai3 NO-LOCK NO-ERROR.
   IF AVAILABLE filtemp THEN DO:
      DELETE filtemp.
   END.   
   FOR EACH filtemp WHERE filtemp.KOPFIL  MATCHES hjai1 NO-LOCK:
      OS-DELETE  VALUE(filtemp.KOPDIR + filtemp.KOPFIL) NO-ERROR.
   END.
   FOR EACH filtemp WHERE filtemp.KOPFIL  MATCHES hjai2 NO-LOCK:
      OS-DELETE  VALUE(filtemp.KOPDIR + filtemp.KOPFIL) NO-ERROR.
   END.
   FOR EACH filtemp WHERE filtemp.KOPFIL  MATCHES hjai3 NO-LOCK:
      OS-DELETE  VALUE(filtemp.KOPDIR + filtemp.KOPFIL) NO-ERROR.
   END.        
   RUN ut_UI (INPUT searchdir + " slut backuppkoll").
END PROCEDURE.      

