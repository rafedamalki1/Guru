/*ONSPECES.P INLÄSNING AV PRISFIL AHLSELL*/
       
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

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   FIELD BENAMNING          AS CHARACTER
   FIELD BENAMNING2          AS CHARACTER
   FIELD EJ1                AS CHARACTER
   FIELD EJ2                AS CHARACTER
   FIELD ENHET              AS CHARACTER
   FIELD EJ3                AS CHARACTER
   FIELD EJ4                AS CHARACTER   
   FIELD EJ5                AS CHARACTER   
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
{muswait.i} 
FIND FIRST FORETAG NO-LOCK NO-ERROR.
ASSIGN
filnamn = "\\server04\d\elpool\elpnj\ESGraninge\Onninen\on200902\on081222hela.skv".
/*filnamn = "\\server04\d\elpool\elpnj\ESGraninge\EonAhlsell\Eon081020.skv".*/
/*filnamn = "\\server04\d\elpool\elpnj\ESGraninge\EonAhlsell\EonEs.skv".*/
/*filnamn = "\\server04\d\elpool\elpnj\ESGraninge\EonAhlsell\Prislista Elpool.skv" */
/*filnamn = "\\server04\d\elpool\elpnj\ESGraninge\AhlsellES.skv"*/ 

leverant = "1".
RUN in_UI.
{EUROPEANAMERICAN.I}
PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
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
   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE skapaenr_UI:   

   FOR EACH tidinah :                                   
      IF FORETAG.FORETAG = "sund" OR FORETAG.FORETAG = "SNAT" OR FORETAG.FORETAG = "ORBI" THEN .
      ELSE IF SUBSTRING(tidinah.ENR,1,1) = "E" THEN DO:
         tidinah.ENR = SUBSTRING(tidinah.ENR,2).          
      END.      
      DO TRANSACTION: 
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  tidinah.ENR AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:

            ASSIGN 
            mtrlbuff.BENAMNING = tidinah.BENAMNING + " " + tidinah.BENAMNING2
            mtrlbuff.BPRIS = tidinah.NPRIS / 100
            mtrlbuff.NPRIS = tidinah.NPRIS / 100
            mtrlbuff.ENHET = tidinah.ENHET.
            mtrlbuff.INDATETIME = NOW.
   mtrlbuff.INANVPROG = THIS-PROCEDURE:NAME   + " " + Guru.Konstanter:globanv.
         END.
         ELSE DO:                               
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = tidinah.ENR
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = tidinah.BENAMNING + " " + tidinah.BENAMNING2
            MTRL.BPRIS = tidinah.NPRIS / 100             
            MTRL.NPRIS = tidinah.NPRIS / 100
            MTRL.ENHET = tidinah.ENHET.
            {MTRLCREATE.I} 
         END.   
      END.            
   END.   

END PROCEDURE.   

                
