/*INLÄSNING AV PRISFIL AHLSELL ELEKTRO.P*/       

DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
{PROVAG.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER  
   FIELD BENAMNING          AS CHARACTER    
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
{muswait.i}      
RUN in_UI.

PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   ASSIGN
   leverant = "14"  
   filnamn = "e:\delad\pro9\guru\noratrafo.skv".
   INPUT FROM VALUE(filnamn) CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.
   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.       
   RUN skapaenr_UI.              
END PROCEDURE.

PROCEDURE skapaenr_UI:   
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  tidinah.ENR AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:     
            ASSIGN
            mtrlbuff.BPRIS = tidinah.NPRIS
            mtrlbuff.NPRIS = tidinah.NPRIS.
            mtrlbuff.INDATETIME = NOW.
            mtrlbuff.INANVPROG = THIS-PROCEDURE:NAME   + " " + Guru.Konstanter:globanv.
         END.
         ELSE DO:                               
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = tidinah.ENR
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = tidinah.BENAMNING                       
            MTRL.NPRIS = tidinah.NPRIS
            MTRL.BPRIS = tidinah.NPRIS.
            {MTRLCREATE.I} 
         END.   
      END.            
   END.   
END PROCEDURE.   

                
