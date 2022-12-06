/*AUTOELPSALJ.p*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE TEMP-TABLE felmeddftptemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE filtemp NO-UNDO
   FIELD KOPDIR AS CHARACTER
   FIELD KOPFIL AS CHARACTER
   FIELD MODDATE AS DATE
   INDEX MODDATE MODDATE. 
     
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbflytta AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbkopianamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbkopianamn2 AS CHARACTER NO-UNDO.
dbkopianamn = "st".
IF WEEKDAY(TODAY) = 1 THEN dbkopianamn = dbkopianamn + "son".
ELSE IF WEEKDAY(TODAY) = 2 THEN dbkopianamn = dbkopianamn + "mon".
ELSE IF WEEKDAY(TODAY) = 3 THEN dbkopianamn = dbkopianamn + "tis".
ELSE IF WEEKDAY(TODAY) = 4 THEN dbkopianamn = dbkopianamn + "ons".
ELSE IF WEEKDAY(TODAY) = 5 THEN dbkopianamn = dbkopianamn + "tor".
ELSE IF WEEKDAY(TODAY) = 6 THEN dbkopianamn = dbkopianamn + "fre".
ELSE IF WEEKDAY(TODAY) = 7 THEN dbkopianamn = dbkopianamn + "lor".
IF DAY(TODAY) = 1 THEN DO:
   dbkopianamn = "st".
   dbkopianamn = dbkopianamn + STRING(MONTH(TODAY)).
END.
  

dbkopianamn = dbkopianamn + ".bck".
dbkopianamn2 = dbkopianamn.
dbflytta = dbkopianamn.
dbkopianamn = "c:\delad\pro11s\dbkopia\" + dbkopianamn.
dbkopianamn2 = "D:\delad\pro11s\dbkopia\" + dbkopianamn2.
{VALDBDEF.I}
{VALDBELPSALJ.I}
prognamnque = "c:\delad\PRO11S\autotid.txt". 
prognamnque2 = "C:\delad\PRO11S\autotidkop.txt".
/* se rensaelpoolkopior.p
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.

IF DAY(TODAY) = 28 AND valdbtemp.WWWFTP = FALSE  THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
*/

OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   {AppSprinSet.I}
   dbfilename = valdbtemp.DBNAMN.
   
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:       
     /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
      RUN ALIASSATT.P.
      RUN MEDDBORT.P.
      RUN textut_UI (INPUT "MEDDBORT KLAR").       
   END.  
   {DBBACKAI.I}
   
   OS-RENAME VALUE(dbkopianamn) VALUE(dbkopianamn2).
   
   
   
   RUN ftpWWW2_UI. 
   GET NEXT vq NO-LOCK.
END.

PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:AppSpringSet[2] " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE ftpWWW2_UI :
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE FtpResult AS CHARACTER NO-UNDO.
   DEFINE VARIABLE FtpNet AS Helpers.FtpNet.
   DEFINE VARIABLE dbkopianamn AS CHARACTER NO-UNDO.
   
   IF WEEKDAY(TODAY) = 5 THEN.
   ELSE RETURN.
   
   /*Anders Olsson Elpool i Umeå AB  11 jan 2018 17:02:22 
vi tar inga årsbackupper längre 
   IF DAY(TODAY) = 31 AND MONTH(TODAY) = 12 THEN DO:
      /*ger tex dbkopianamn = gran2014*/
      dbkopianamn = dbfilename.
      dbkopianamn = dbkopianamn + STRING(YEAR(TODAY)).
   END.
   */
   ftpNet = NEW Helpers.FtpNet().
   prognamn = dbkopianamn2.
   prognamn2 = dbflytta.
   prognamn2 = "ftp://www2.guruonweb.se/Upload/" + prognamn2.
   FtpResult = ftpNet:Skicka(CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), CHR(115) + CHR(117) + CHR(112) + CHR(101) + CHR(114) + CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110),prognamn2,prognamn).
   
   RUN ut_UI (INPUT "FTP klar-" + FtpResult).
   
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


PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER uttxt AS CHARACTER FORMAT "X(20)" NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED uttxt " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

