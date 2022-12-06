/*UMEENRG.P INLÄSNING AV UMEÅENERGIS ARTIKLAR*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.




DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD ENR                AS CHARACTER FORMAT "X(11)" 
   FIELD B1                 AS CHARACTER FORMAT "X(40)" 
   FIELD B2                 AS CHARACTER FORMAT "X(40)"
   FIELD ENHET              AS CHARACTER   
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE tidin2   
   FIELD ENR                AS CHARACTER FORMAT "X(11)"
   FIELD BENAMNING          AS CHARACTER FORMAT "X(40)"    
   FIELD ENHET              AS CHARACTER FORMAT "X(3)"
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
{AMERICANEUROPEAN.I}   
   ASSIGN
   filnamn = "a:\guru.skv"
   leverant = "1".
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.    
   FOR EACH tidin2:
      DELETE tidin2.
   END.      
   RUN PROVAG.P.
   ASSIGN
   dlcvar = dlcvar + "QUOTER.EXE"
   wtidvar = wtidvar + "kabpris.q".           
   
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
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
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   /*FOR EACH tidin WHERE tidin.ENR = "":
 *       DELETE tidin.
 *    END.*/      
   FOR EACH tidin NO-LOCK:
      DO TRANSACTION:         
         CREATE tidin2.
/*          IF SUBSTRING(tidin.ENR,1,1) = "E" THEN DO:                           */
/*             tidin2.ENR = SUBSTRING(tidin.ENR,1,3) + SUBSTRING(tidin.ENR,5,7). */
/*          END.                                                                 */
/*          ELSE DO:                                                             */
/*             tidin2.ENR = tidin.ENR.                                           */
/*          END.                                                                 */
         ASSIGN
         tidin2.ENR = tidin.ENR
         tidin2.BENAMNING = TRIM(tidin.B1) + " " + TRIM(tidin.B2)
         tidin2.ENHET = tidin.ENHET.
/*          melvar = INDEX(STRING(tidin.PRIS),".",1). */
/*          IF melvar = 0 THEN                        */
/*          tidin2.PRIS = tidin.PRIS / 100.           */
/*          ELSE tidin2.PRIS = tidin.PRIS * 1000.     */
      END.   
   END.     
   RUN skapasats_UI.           
   OS-DELETE VALUE(wtidvar).
{EUROPEANAMERICAN.I}
PROCEDURE skapasats_UI:   
   FOR EACH tidin2 NO-LOCK:                                
      DO TRANSACTION:                                 
         FIND FIRST MTRL WHERE MTRL.ENR = tidin2.ENR AND
         MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.                     
         IF AVAILABLE MTRL THEN DO:
            ASSIGN
            MTRL.BENAMNING = tidin2.BENAMNING 
            MTRL.NPRIS = 0
            MTRL.BPRIS = 0.
            {MTRLCREATE.I} 
         END.
         ELSE DO:
            CREATE MTRL.
            ASSIGN
            MTRL.ENR = tidin2.ENR
            MTRL.BENAMNING = tidin2.BENAMNING 
            MTRL.NPRIS = 0
            MTRL.BPRIS = 0 
            MTRL.LEVKOD = leverant
            MTRL.KALKNR = 0
            MTRL.ENHET = tidin2.ENHET.
            {MTRLCREATE.I} 
         END.
      END.         
   END.
END PROCEDURE.   

                
