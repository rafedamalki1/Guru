    /*ELEFIN.P*/
    
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
{PROVAG.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidinelb
   FIELD ENR                AS CHARACTER FORMAT "X(11)"    
   FIELD BENAMNING          AS CHARACTER FORMAT "X(40)"
   FIELD ENHET              AS CHARACTER FORMAT "X(3)" 
   FIELD BPRIS              AS CHARACTER FORMAT "X(15)"   
   INDEX ENR IS PRIMARY ENR.
   
DEFINE TEMP-TABLE tidinelb2
   FIELD ENR                AS CHARACTER FORMAT "X(11)"    
   FIELD BENAMNING          AS CHARACTER FORMAT "X(40)"
   FIELD ENHET              AS CHARACTER FORMAT "X(3)" 
   FIELD BPRIS              AS DECIMAL FORMAT ">>>>>9.99"   
   INDEX ENR IS PRIMARY ENR.   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE INPUT PARAMETER filnamn AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{muswait.i} 
{AMERICANEUROPEAN.I}     
RUN in_UI.
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinelb NO-ERROR.
   EMPTY TEMP-TABLE tidinelb2 NO-ERROR.
   
   
   kommando = wtidvar + "elefpris.q".  
   ASSIGN
   kommando2 = dlcvar + "QUOTER.EXE".
   OS-COMMAND SILENT VALUE(kommando2)
   VALUE(filnamn) > VALUE(kommando).   
   INPUT FROM VALUE(kommando) NO-ECHO.    
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
   OUTPUT TO VALUE(kommando).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(kommando) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinelb.
         ASSIGN.
         IMPORT DELIMITER ";" tidinelb   NO-ERROR.
      END.               
   END.
   FOR EACH tidinelb WHERE tidinelb.ENR = "":
      DELETE tidinelb.
   END.    
   FOR EACH tidinelb NO-LOCK:
      DO TRANSACTION:
         ASSIGN
         satsvar = ""
         enrvar = STRING(tidinelb.BPRIS)
         langd = LENGTH(enrvar)
         pos1 = 1
         melvar = INDEX(enrvar,CHR(160),pos1).         
         IF melvar NE 0 THEN DO:
            DO WHILE melvar < langd:                     
               ASSIGN
               satsvar = satsvar + SUBSTRING(enrvar,pos1,melvar - 1).            
               ASSIGN
               pos1 = melvar + 1
               melvar2 = INDEX(enrvar,CHR(160),pos1).
               IF melvar2 = 0 THEN LEAVE.
               ELSE melvar = melvar2.
            END.             
            satsvar = satsvar + SUBSTRING(enrvar,pos1,langd - pos1 + 1).
            CREATE tidinelb2.
            ASSIGN
            tidinelb2.BPRIS = DECIMAL(satsvar)
            tidinelb2.BENAMNING = tidinelb.BENAMNING
            tidinelb2.ENR = tidinelb.ENR
            tidinelb2.ENHET = tidinelb.ENHET.                
         END.
         ELSE DO:
            CREATE tidinelb2.
            ASSIGN
            tidinelb2.BPRIS = tidinelb2.BPRIS
            tidinelb2.BENAMNING = tidinelb.BENAMNING
            tidinelb2.ENR = tidinelb.ENR
            tidinelb2.ENHET = tidinelb.ENHET.
         END.          
      END.   
   END.      
   RUN skapaenr_UI.           
   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE skapaenr_UI:   
   FOR EACH tidinelb2 NO-LOCK:                                   
      DO TRANSACTION: 
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  SUBSTRING(tidinelb2.ENR,1,11) AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:
            ASSIGN
            mtrlbuff.BPRIS = tidinelb2.BPRIS.
            IF mtrlbuff.NPRIS = 0 THEN mtrlbuff.NPRIS = tidinelb2.BPRIS.
         END.
         ELSE DO:                               
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = SUBSTRING(tidinelb2.ENR,1,11)
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = SUBSTRING(tidinelb2.BENAMNING,1,40)
            MTRL.ENHET = SUBSTRING(tidinelb2.ENHET,1,3)
            MTRL.BPRIS = tidinelb2.BPRIS
            MTRL.NPRIS = tidinelb2.BPRIS.                      
         END.   
      END.            
   END.   
END PROCEDURE.   

                
