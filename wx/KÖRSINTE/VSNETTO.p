/*VSNETTO.P Uppdatering nettopris Onninen VSAB MTRL.KUND*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
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
   FIELD EJ                 AS CHARACTER
   FIELD EJ1                 AS CHARACTER
   FIELD EJ2                 AS CHARACTER
   FIELD EJ3                 AS CHARACTER
   FIELD EJ4                 AS CHARACTER
   FIELD EJ5                 AS CHARACTER
   FIELD EJ6                 AS CHARACTER
   FIELD EJ7                 AS CHARACTER
   FIELD EJ8                 AS CHARACTER
   FIELD EJ9                 AS CHARACTER
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
   /* kör också ECnetto.xls i Uppdatera katalog Märkta excel. Ericsson cabels ligger i egen katalog*/
ASSIGN
filnamn = "\\server04\d\elpool\elpnj\VESAB\Onninen\Nettoprislista\VSNetto2009.skv".
/*filnamn = "\\server04\d\elpool\elpnj\VESAB\Onninen\Nettoprislista\Nettolista2009.skv".*/
/*filnamn = "\\server04\d\elpool\elpnj\VESAB\Onninen\Nettoprislista\Onninennetto.skv".*/

leverant = "16".
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
   RUN markenr_UI.           
   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE markenr_UI:   

   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         IF SUBSTRING(tidinah.ENR,1,1) = "E" THEN DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  SUBSTRING(tidinah.ENR,2) AND
            mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
            EXCLUSIVE-LOCK NO-ERROR.
            
         END.
         ELSE DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  tidinah.ENR AND
            mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
            EXCLUSIVE-LOCK NO-ERROR.
         END.
         IF AVAILABLE mtrlbuff THEN DO:
            ASSIGN mtrlbuff.KUND = TRUE.            
         END.
         
      END.            
   END.   

END PROCEDURE.   

                
