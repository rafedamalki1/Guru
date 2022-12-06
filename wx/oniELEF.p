/*ONIELEF.P INLÄSNING AV PRISFIL ONIELEF.P*/       
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
   FIELD BENAMNING          AS CHARACTER      
   FIELD BENAMNING2          AS CHARACTER      
   FIELD ENHET              AS CHARACTER
   FIELD PRIS               AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .


DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
  EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidineln NO-ERROR.
   leverant = "16".
   filnamn = "d:\delad\pro9\wrk\oni.txt". 
   /*filnamn = "\\SERVER04\D\ELPOOL\ELPNJ\SUND\nyanettopriser2005.skv".*/
   kommando = filnamn.
   SESSION:SET-NUMERIC-FORMAT(" ",","). 
   INPUT FROM VALUE(kommando) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   REPEAT:
      CREATE tidineln.
      ASSIGN.
      IMPORT DELIMITER ";" tidineln   .      
   END.

   SESSION:SET-NUMERIC-FORMAT(" ",".").
  
   FOR EACH tidineln WHERE tidineln.ENR = "":
      DELETE tidineln.
   END.  
   
   RUN skapaenr_UI.           
  
FOR EACH MTRL WHERE MTRL.levkod = leverant :
   DISP
   MTRL.ENR 
   MTRL.BENAMNING
   MTRL.ENHET
   MTRL.npris
   MTRL.bpris.
END.
{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:      
   FOR EACH tidineln NO-LOCK:        
      
      DO TRANSACTION:          
         FIND FIRST MTRL WHERE MTRL.levkod = leverant AND MTRL.enr = TRIM(tidineln.enr) AND MTRL.kalknr = 0
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE MTRL THEN DO:                  
            CREATE MTRL.
           
            /*IF SUBSTRING(tidineln.BENAMNING2,1,1) NE "." THEN MTRL.BENAMNING = MTRL.BENAMNING + " " + tidineln.BENAMNING2.         */            
         END.      
         ASSIGN
         MTRL.ENR = TRIM(tidineln.ENR)          
         MTRL.LEVKOD = leverant
         MTRL.KALKNR = 0
         MTRL.ENHET = TRIM(tidineln.ENHET)          
         MTRL.npris = tidineln.pris
         MTRL.bpris = tidineln.pris.
         {MTRLCREATE.I} 
         IF TRIM(tidineln.BENAMNING2) = "." THEN MTRL.BENAMNING = TRIM(tidineln.BENAMNING).
         ELSE IF TRIM(tidineln.BENAMNING2) = "" THEN MTRL.BENAMNING = TRIM(tidineln.BENAMNING).
         ELSE IF TRIM(tidineln.BENAMNING2) = TRIM(tidineln.BENAMNING) THEN MTRL.BENAMNING = TRIM(tidineln.BENAMNING).
         ELSE MTRL.BENAMNING = TRIM(tidineln.BENAMNING) + " " + TRIM(tidineln.BENAMNING2).
      END.
      
   END.   
END PROCEDURE.   

                
