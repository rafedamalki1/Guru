
/*DBBACK.P körs ej*/
DEFINE TEMP-TABLE felmeddftptemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL AS INTEGER.
   
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE namevar AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER prognamnque AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dbfilename AS CHARACTER NO-UNDO.  /*dabasnamn*/
DEFINE INPUT PARAMETER dbbackupp AS CHARACTER NO-UNDO.   /*vart kopian ska*/
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
   dbkopianamn = dbfilename.
   dbkopianamn = dbkopianamn + STRING(MONTH(TODAY)).
END.
/*Anders Olsson Elpool i Umeå AB  11 jan 2018 17:02:22 
vi tar inga årsbackupper längre 
IF DAY(TODAY) = 31 AND MONTH(TODAY) = 12 THEN DO:
   dbkopianamn = dbfilename.
   dbkopianamn = dbkopianamn + STRING(YEAR(TODAY)).
END.
*/
progflytt = SUBSTRING(prognamnque,1,INDEX(prognamnque,"autotid.txt") - 1).
namevar = dbfilename + " AI".
RUN ut_UI (INPUT namevar).
prognamnold = progflytt + "AI\" + dbfilename + ".A1".
prognamnold2 = progflytt + dbbackupp + dbfilename + ".A1".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
prognamnold = progflytt + "AI\" + dbfilename + ".A2".
prognamnold2 = progflytt + dbbackupp + dbfilename + ".A2".
OS-COPY VALUE(prognamnold) VALUE(prognamnold2).
namevar = dbfilename + " EMP".
RUN ut_UI (INPUT namevar).
IF SEARCH("rfutil.BAT") NE ? THEN
kommando = SEARCH("rfutil.BAT") + " " + progflytt + "db\" + dbfilename + " -C aimage extent empty".
ELSE kommando = "rfutil " + progflytt + "db\" + dbfilename + " -C aimage extent empty".
RUN ut_UI (INPUT kommando).
RUN os_UI.
/*
RUN osnos_UI.
*/
namevar = dbfilename + " BK".
RUN ut_UI (INPUT namevar).
IF SEARCH("probkup.BAT") NE ? THEN
kommando = SEARCH("probkup.BAT") + " online " + progflytt + "db\" + dbfilename + " " + progflytt + dbbackupp + dbfilename + ".bck -com".
ELSE kommando = "probkup online " + progflytt + "db\" + dbfilename + " " + progflytt + dbbackupp + dbfilename + ".bck -com".
RUN ut_UI (INPUT kommando).
RUN os_UI.
RUN ut_UI (INPUT "extra backup"). 
OS-COPY VALUE(progflytt + dbbackupp + dbfilename + ".bck") VALUE(progflytt + dbbackupp + dbkopianamn + ".bck").
RUN ut_UI (INPUT dbfilename). 
IF dbfilename = "FORS" THEN DO:
   RUN ftpFORS_UI.  
END.
IF dbfilename = "ELKB" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "SKOGSK" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "ORBI" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "HANA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "ATS" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "ETSA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "SWEO" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "HJEL" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "LIMO" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "JSBF" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "EKSK" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "SSEL" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "LAKL" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "PICA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "AFCO" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "OXEL" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "PPKO" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "KEWA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "DUTA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "elpc" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "NYLB" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "POLA" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "KNOR" THEN DO:
   RUN ftpWWW_UI.  
END.
IF dbfilename = "YSEN" THEN DO:
   RUN ftpWWW_UI.  
END.
PROCEDURE ftpWWW_UI :
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ziptill AS CHARACTER NO-UNDO.
   DEFINE VARIABLE zipNamn  AS CHARACTER NO-UNDO.
   prognamn = progflytt + dbbackupp + dbfilename + ".bck".
   prognamn2 = "FORTUM\" + dbfilename + ".bck".
  /*
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI.
   */
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "FORTUM\" + dbkopianamn + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   FIND FIRST felmeddftptemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddftptemp THEN DO:
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad" THEN DO:
         RUN backdel_UI (INPUT prognamn).
      END.   
   END.                     
   RUN ftpsvar_UI. 

END PROCEDURE.

PROCEDURE ftpFORS_UI :
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
  
   prognamn = progflytt + dbbackupp + dbfilename + ".bck".
   prognamn2 = "FORTUM\" + dbfilename + ".bck".
   /*
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI.
   */
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "FORTUM\" + dbkopianamn + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   FIND FIRST felmeddftptemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddftptemp THEN DO:
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad" THEN DO:
         RUN backdel_UI (INPUT prognamn).
      END.   
   END.   
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

/*
PROCEDURE ftpSKOGSK_UI :
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ziptill AS CHARACTER NO-UNDO.
   DEFINE VARIABLE zipNamn  AS CHARACTER NO-UNDO.
   prognamn = progflytt + dbbackupp + dbfilename + ".bck".
   prognamn2 = "FORTUM\" + dbfilename + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI.
   
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "FORTUM\" + dbkopianamn + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI. 

END PROCEDURE.

PROCEDURE ftpELKB_UI :
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ziptill AS CHARACTER NO-UNDO.
   DEFINE VARIABLE zipNamn  AS CHARACTER NO-UNDO.
   prognamn = progflytt + dbbackupp + dbfilename + ".bck".
   prognamn2 = "FORTUM\" + dbfilename + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI.
   
   prognamn = progflytt + dbbackupp + dbkopianamn + ".bck".
   prognamn2 = "FORTUM\" + dbkopianamn + ".bck".
   RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                     INPUT prognamn, INPUT prognamn2,
                     INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
   RUN ftpsvar_UI. 
   
   
   
   ziptill = "C:\elpool\FTP\".
   zipNamn = "hemisida.zip".
   RUN zippaALLT (INPUT ziptill, INPUT zipNamn, INPUT "C:\elpool\FTP\Plakat\").
   IF SEARCH(ziptill + zipNamn) NE ? THEN DO:
      prognamn = ziptill + zipNamn.
      prognamn2 = "FORTUM\" + zipNamn.
      RUN FTPFILE.P (INPUT CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111), INPUT CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110), INPUT TRUE, INPUT 2,
                        INPUT prognamn, INPUT prognamn2,
                        INPUT "www.guruonweb.se", OUTPUT TABLE felmeddftptemp).
      RUN ftpsvar_UI. 
   END.         
END PROCEDURE.

PROCEDURE zippaALLT :
   DEFINE INPUT  PARAMETER ziptill AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER zipNamn  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER zipfiler  AS CHARACTER NO-UNDO.        
   
   DEFINE VARIABLE zippfil AS CLASS Start.ZipElpool NO-UNDO.
   DEFINE VARIABLE namnvar AS CHARACTER NO-UNDO.
   
   namnvar = ziptill +  zipNamn.
   /*   RUN zippaALLT (INPUT ziptill, INPUT zipNamn, INPUT "C:\elpool\FTP\Plakat\").*/
   
   zippfil = NEW Start.ZipElpool().
   zippfil:MakeZip(zipfiler, namnvar).        
END PROCEDURE.
*/


