/*VSABKALK.P*/
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
DEFINE VARIABLE typvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE arbvar LIKE LOP1.ARBKOD NO-UNDO.
DEFINE VARIABLE lopvar LIKE LOP1.LOPNR NO-UNDO.
DEFINE VARIABLE kabmsk AS LOGICAL NO-UNDO.
DEFINE VARIABLE rorlig AS DECIMAL NO-UNDO.
DEFINE VARIABLE ebrmont AS DECIMAL NO-UNDO.
DEFINE VARIABLE ebrmask1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ebrmask2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mskpris AS INTEGER NO-UNDO.
DEFINE VARIABLE vitkatalog AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD LOPNR                  AS INTEGER 
   FIELD BENAMNING              AS CHARACTER 
   FIELD ENHET                  AS CHARACTER
   FIELD F1                     AS DECIMAL
   FIELD F2                     AS DECIMAL
   FIELD F3                     AS DECIMAL   
   FIELD MTRL                   AS INTEGER.


DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   {AMERICANEUROPEAN.I}
{muswait.i} 
   ASSIGN
   vitkatalog = FALSE
   typvar = 2
   arvar = 2004
   ebrmont = 501
   ebrmask1 = 470
   ebrmask2 = 440
   rorlig = 268.65. 
    
   FOR EACH tidin:
      DELETE tidin.
   END.   
   filnamn = "c:\skj.skv".
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.      
   RUN skapasats_UI.    
   FOR EACH tidin:
      DELETE tidin.
   END.
   filnamn = "c:\skj2.skv".
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.      
   RUN skapasats2_UI.
/*    OS-DELETE VALUE(wtidvar). */
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE skapasats_UI:   
   FOR EACH tidin NO-LOCK:                                
      DO TRANSACTION:                             
         FIND FIRST LOP2 WHERE LOP2.KATAR = arvar AND 
         LOP2.ARBKOD = "SKJ" AND 
         LOP2.LOPNR = tidin.LOPNR 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LOP2 THEN DO:
            CREATE LOP2.
            ASSIGN
            LOP2.ARBKOD = "SKJ"
            LOP2.LOPNR = tidin.LOPNR            
            LOP2.BENAMNING = tidin.BENAMNING
            LOP2.ENHET = tidin.ENHET
            LOP2.F1 = tidin.F1 / 100
            LOP2.F2 = tidin.F2 / 100  
            LOP2.F3 = tidin.F3 / 100
            LOP2.MATERIEL = tidin.MTRL                       
            LOP2.FAST = FALSE
            LOP2.KATAR = arvar.                        
         END.            
      END.                           
   END.   
END PROCEDURE.   
PROCEDURE skapasats2_UI:   
   FOR EACH tidin NO-LOCK:                                
      DO TRANSACTION:                             
         FIND FIRST LOP2 WHERE LOP2.KATAR = arvar AND 
         LOP2.ARBKOD = "SKJ" AND 
         LOP2.LOPNR = tidin.LOPNR 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE LOP2 THEN DO:            
            ASSIGN            
            SUBSTRING(LOP2.BENAMNING,60,1000) = " ".            
            SUBSTRING(LOP2.BENAMNING,60) = tidin.BENAMNING.            
         END.            
      END.                           
   END.   
END PROCEDURE.   

