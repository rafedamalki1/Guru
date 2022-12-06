/*ELEF.p INLÄSNING AV PRISFIL ELEF*/ 
    
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.
/*

*/

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

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidineln
   FIELD ENR                AS CHARACTER 
   FIELD EJ1                AS CHARACTER   
   FIELD EJ2                AS CHARACTER   
   FIELD BENAMNING          AS CHARACTER         
   FIELD PRIS               AS DECIMAL
   FIELD ENHET              AS CHARACTER
   FIELD EJ3                AS CHARACTER
   FIELD EJ4                AS CHARACTER
   FIELD EJ5                AS CHARACTER
   FIELD EJ6                AS CHARACTER
   FIELD UTG                AS CHARACTER
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

DEFINE BUFFER mbuff FOR mtrl.
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER leverant AS CHARACTER NO-UNDO.
{muswait.i}      
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidineln NO-ERROR.
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   /*IF FORETAG.FORETAG = "ELPA" THEN DO:
      filnamn = "\\server04\d\elpool\elpnj\VESAB\ONNINEN\nyonninen.txt". 
   END.*/
   IF FORETAG.FORETAG = "ELPA" THEN DO:
      filnamn = "c:\nyonninen.txt".
      /*filnamn = "F:\elpool\elpnj\ats\onninen\Netto\nyonninen.txt".*/
      /*filnamn = "F:\elpool\elpnj\mtrlelpool\nyonninen.txt".*/
      /*filnamn = "f:\elpool\elpnj\VESAB\onninen\nyonninen.txt".*/
      /*filnamn = "f:\elpool\elpnj\Esgraninge\onninen\nyonninen.txt". */
   END.
   ELSE IF FORETAG.FORETAG = "sund" OR FORETAG.FORETAG = "SNAT" THEN DO:
      filnamn = "d:\delad\klient\pro10\wrk\nyonninen.txt". 
   END.
   ELSE IF FORETAG.FORETAG = "VSYD" OR FORETAG.FORETAG = "VORD" OR FORETAG.FORETAG = "VAST" OR 
      FORETAG.FORETAG = "VOST" THEN DO:
      filnamn = "e:\delad\pro9\guru\nyonninen.txt".
   END.
   ELSE IF FORETAG.FORETAG = "LULE" THEN DO:
      filnamn = "d:\elpool\delad\pro9\guru\nyonninen.txt". 
   END.
   ELSE IF FORETAG.FORETAG = "GRAN" THEN DO:
      filnamn = "d:\elpool\delad\pro9\wrk\nyonninen.txt". 
   END.
   ELSE IF FORETAG.FORETAG = "BORL" THEN DO:
      filnamn = "D:\GURU\PRO9\GURU\nyonninen.txt".
   END.
   ELSE IF FORETAG.FORETAG = "reji" OR FORETAG.FORETAG = "SWEO" OR FORETAG.FORETAG = "PICA" OR FORETAG.FORETAG = "OVIK" OR FORETAG.FORETAG = "GETB" OR FORETAG.FORETAG = "ELKB"
   OR FORETAG.FORETAG = "BODE"   THEN DO:
      filnamn = "c:\nyonninen.txt".
   END.
   ELSE DO:
      filnamn = SESSION:TEMP-DIRECTORY + "nyonninen.txt". 
   END.

    /*filnamn = "\\server04\d\elpool\elpnj\VESAB\ONNINEN\onninen070319.txt". */
   /*filnamn = "\\SERVER04\D\ELPOOL\ELPNJ\esgraninge\onninen070313.txt".*/
     /*filnamn = "\\SERVER04\D\ELPOOL\ELPNJ\borl\onninen070221.txt".*/
   kommando = filnamn.
   {AMERICANEUROPEAN.I}
   SESSION:NUMERIC-FORMAT = "european".
   /*SESSION:SET-NUMERIC-FORMAT(" ","."). */
   INPUT FROM VALUE(kommando) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidineln.
         ASSIGN.
         IMPORT DELIMITER ";" tidineln   .
      END.               
   END.

/*    SESSION:SET-NUMERIC-FORMAT(" ","."). */
  
   FOR EACH tidineln WHERE tidineln.ENR = "":
      DELETE tidineln.
   END.  
   IF FORETAG.FORETAG = "VAST"  THEN DO:
   
      /*ta bort enr som utgått Handfast 17/1 2008*/
      /*FOR EACH tidineln WHERE tidineln.UTG = "U":
         DELETE tidineln.
      END.  30/1 2008*/
      /*ta bort enr med pris = 0   Handfast 17/1 2008*/
      FOR EACH tidineln WHERE tidineln.PRIS = 0:
         DELETE tidineln.
      END.  
   END.

   
   RUN skapaenr_UI.           
   SESSION:NUMERIC-FORMAT = "AMERICAN".
   {EUROPEANAMERICAN.I}

PROCEDURE skapaenr_UI:      
   FOR EACH tidineln NO-LOCK:        
      
      DO TRANSACTION:          
         IF FORETAG.FORETAG = "Celpa" OR FORETAG.FORETAG = "sund" OR FORETAG.FORETAG = "SNAT" OR FORETAG.FORETAG = "ORBI" THEN .
         ELSE IF SUBSTRING(tidineln.ENR,1,1) = "E" THEN tidineln.ENR = SUBSTRING(tidineln.ENR,2).          

         FIND FIRST mtrl WHERE mtrl.levkod = leverant AND mtrl.enr = tidineln.enr AND mtrl.kalknr = 0
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE mtrl THEN DO:                  
            CREATE MTRL.
            /*IF FORETAG.FORETAG = "Celpa" OR FORETAG.FORETAG = "sund" OR FORETAG.FORETAG = "sund" THEN MTRL.ENR = tidineln.ENR.       
            ELSE IF SUBSTRING(tidineln.ENR,1,1) = "E" THEN DO:
               MTRL.ENR = SUBSTRING(tidineln.ENR,2).          
            END.
            ELSE DO:
               MTRL.ENR = tidineln.ENR.          
            END.*/
            ASSIGN            
            MTRL.ENR = tidineln.ENR
            MTRL.LEVKOD = leverant
            MTRL.KALKNR = 0
            MTRL.BENAMNING = TRIM(tidineln.BENAMNING)
            MTRL.ENHET = tidineln.ENHET          
            MTRL.npris = tidineln.pris
            MTRL.bpris = tidineln.pris.
            {MTRLCREATE.I} 
            IF FORETAG.FORETAG = "ELPA" OR  FORETAG.FORETAG = "REJI" OR FORETAG.FORETAG = "SWEO" OR FORETAG.FORETAG = "PICA" OR FORETAG.FORETAG = "GETB" OR FORETAG.FORETAG = "ELKB" THEN DO:
               /*SINGEL*/
               ASSIGN
               MTRL.npris = 0
               MTRL.bpris = 0.
            END.
            /*IF SUBSTRING(tidineln.BENAMNING2,1,1) NE "." THEN MTRL.BENAMNING = MTRL.BENAMNING + " " + tidineln.BENAMNING2.         */            
         END.      
         ELSE DO:
            ASSIGN
            MTRL.BENAMNING = TRIM(tidineln.BENAMNING)
            MTRL.ENHET = tidineln.ENHET          
            MTRL.npris = tidineln.pris
            MTRL.bpris = tidineln.pris. 
            {MTRLCREATE.I}   
            IF FORETAG.FORETAG = "ELPA"  OR  FORETAG.FORETAG = "REJI" OR FORETAG.FORETAG = "SWEO" OR FORETAG.FORETAG = "PICA" OR FORETAG.FORETAG = "GETB" OR FORETAG.FORETAG = "ELKB"  THEN DO:
               /*SINGEL*/
               ASSIGN
               MTRL.npris = 0
               MTRL.bpris = 0.
            END.
         END.         
      END.
      RELEASE MTRL NO-ERROR.      
   END.   
END PROCEDURE.   

                
