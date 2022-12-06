/*VNATAOIN.P AONUMMER FRÅN VNAT TILL GURU*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.




DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE sparfil AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE aklock AS CHARACTER NO-UNDO.
DEFINE VARIABLE bklock AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD AONR               AS CHARACTER     
   FIELD BENAMNING          AS CHARACTER
   FIELD OMRADE             AS CHARACTER
   INDEX AONR IS PRIMARY AONR.

DEFINE TEMP-TABLE tidin2   
   FIELD ENR                AS CHARACTER FORMAT "X(11)"
   FIELD BENAMNING          AS CHARACTER FORMAT "X(40)" 
   FIELD PRIS               AS DECIMAL FORMAT ">>>>>9.99"   
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   EMPTY TEMP-TABLE tidin2 NO-ERROR.
{AMERICANEUROPEAN.I}    
   aklock = STRING(TIME,"HH:MM:SS").
   bklock = SUBSTRING(aklock,1,2) + SUBSTRING(aklock,4,2) + SUBSTRING(aklock,7,2).   
   
   IF OPSYS = "UNIX" THEN DO:
      ASSIGN
      filnamn = "/guru/import/import.txt"
      sparfil = "/guru/import/vnat" + STRING(TODAY,"99999999") + bklock + ".txt"
      wtidvar = "/guru/import/import.q"
      dlcvar = "/usr/dlc9/bin/quoter". 
   END.  
   ELSE DO:
      RUN PROVAG.P.
      ASSIGN
      filnamn = "\\pc112\delad\elpool\elpnj\vnat\import.txt"
      sparfil = "\\pc112\delad\elpool\elpnj\vnat\" + STRING(TODAY,"99999999") + bklock + ".txt"
      dlcvar = dlcvar + "QUOTER.EXE"
      wtidvar = wtidvar + "import.q".
   END.              
   kommando = SEARCH(filnamn).
   IF kommando = ? THEN DO:          
      RETURN.       
   END.   
   OS-COPY VALUE(filnamn) VALUE(sparfil).
   
   /*OS-COMMAND SILENT VALUE(dlcvar)
 *    VALUE(filnamn) > VALUE(wtidvar).*/   
 
   INPUT FROM VALUE(filnamn) NO-ECHO.    
   
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER "ü" tidin   NO-ERROR.
      END.               
   END.   
   OS-DELETE VALUE(filnamn).
   OS-DELETE VALUE(wtidvar).
   
   RUN skapaao_UI.
  {EUROPEANAMERICAN.I} 
PROCEDURE skapaao_UI:
   IF OPSYS NE "UNIX" THEN DO:
      RUN skapaaowin_UI.
      RETURN.
   END.   
   musz = FALSE.   
   
   IF CONNECTED("VNAT") THEN DO:
      {VERALIAS.I}
      RUN VNATAO.P (INPUT TABLE tidin).  
      {DELALIAS.I}
      DISCONNECT VNAT NO-ERROR.
   END. 
   ELSE DO:
      OUTPUT TO "/guru/import/felimport.txt" 
      APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
      NO-ECHO.
      PUT UNFORMATTED "fel" + STRING(TODAY,"99999999") + bklock + ".txt" SKIP.
      OUTPUT CLOSE.
   END.             
   musz = FALSE.
END PROCEDURE.   

PROCEDURE skapaaowin_UI:
   RUN VNATAO.P (INPUT TABLE tidin).    
   musz = FALSE.
END PROCEDURE.
                
