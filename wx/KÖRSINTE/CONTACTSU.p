/*CONTACTSU.p INLÄSNING AV PRISFIL CONTACTEN*/       

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

/*DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   FIELD BENAMNING          AS CHARACTER
   FIELD BENAMNING2          AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   FIELD ENHET              AS CHARACTER
   INDEX ENR IS PRIMARY ENR.*/

DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   FIELD BENAMNING          AS CHARACTER
   FIELD EJ1              AS CHARACTER
   FIELD EJ2              AS CHARACTER
   FIELD NPRIS              AS DECIMAL   
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{muswait.i} 
ASSIGN
   
   filnamn = "\\server04\d\elpool\elpnj\Sund\Contacten\Netto110k.txt" 
  /* filnamn = "\\server04\d\elpool\elpnj\tectel\materiel\prisertot.skv" */
/*filnamn = "\\server04\d\elpool\elpnj\Luleå\Priser20080331\nettol2.skv" */

leverant = "21".
FIND FIRST FORETAG NO-LOCK NO-ERROR.
{AMERICANEUROPEAN.I}
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
/*    OUTPUT TO C:\klklkl.txt. */
/*    FOR EACH tidinah:        */
/*       EXPORT tidinah.       */
/*    END.                     */
/*    OUTPUT CLOSE.            */
   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.      
/*    OUTPUT TO C:\klkl.txt. */
/*    FOR EACH tidinah:      */
/*       EXPORT tidinah.     */
/*    END.                   */
/*    OUTPUT CLOSE.          */
   RUN skapaenr_UI.           
   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE skapaenr_UI:   
/*    OUTPUT TO C:\kaka.txt. */
   /*OBS ! Luleå ska ha brutto = netto*/
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         IF FORETAG.FORETAG = "sund" OR FORETAG.FORETAG = "SNAT" OR FORETAG.FORETAG = "ORBI" OR FORETAG.FORETAG = "ELPA" THEN DO:
            IF SUBSTRING(tidinah.ENR,1,1) = "E" THEN.
            ELSE tidinah.ENR = "E" + tidinah.ENR.
         END.
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  tidinah.ENR AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:
/*             EXPORT tidinah. */
            ASSIGN 
            mtrlbuff.BENAMNING = tidinah.BENAMNING 
            /*mtrlbuff.BPRIS = tidinah.BPRIS / 100   */
            mtrlbuff.NPRIS = tidinah.NPRIS / 100
            mtrlbuff.BPRIS = tidinah.NPRIS / 100
            /*mtrlbuff.BPRIS = mtrlbuff.NPRIS*/
            mtrlbuff.ENHET = "st".                                     /*tidinah.ENHET.*/
            
/*             mtrlbuff.BPRIS = tidinah.BPRIS / 100. */
/*             mtrlbuff.NPRIS = tidinah.NPRIS / 100. */
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
            MTRL.NPRIS = tidinah.NPRIS / 100
            /*MTRL.BPRIS = MTRL.NPRIS*/
            MTRL.ENHET =  "st".                                        /*tidinah.ENHET.*/
            
/*             MTRL.BPRIS = tidinah.BPRIS / 100  */
/*             MTRL.NPRIS = tidinah.NPRIS / 100. */
            {MTRLCREATE.I}
         END.   
      END.            
   END.   
/*    OUTPUT CLOSE. */
END PROCEDURE.   

                
