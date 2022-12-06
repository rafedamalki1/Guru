
/*------------------------------------------------------------------------
    File        : UTBYTSTARTBER.I
    Purpose     : berskaptemp

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Sep 29 14:34:56 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE UtbytStart_UI :
   DEFINE INPUT  PARAMETER Revers AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ValdBerLev AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER BerValdNr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER BerValdaOmr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inkaofenrUB AS LOGICAL NO-UNDO.
   
   {UTBBEREDNINGUID.I}
   
   FOR EACH berskaptemp:
      berskaptemp.LISTAKUNDLEV = FALSE.
   END.   
   FOR EACH berskaptemp WHERE berskaptemp.LISTAKUNDLEV = FALSE:
      IF berskaptemp.ENR = "" THEN.
      ELSE IF berskaptemp.LEVKOD =  ValdBerLev AND listnr = 0 THEN DO:
         /*Anders Olsson Elpool i Umeå AB  16 nov 2021 16:30:39 
           byter ej mtrl om lev = vald lev och ingen kundlista byter bara om man har en lev till samma lev
         */
         
         RUN UTBYTESLISTA.P (INPUT "GbNb", INPUT listnr,INPUT berskaptemp.LEVKOD,INPUT berskaptemp.ENR,INPUT ValdBerLev, OUTPUT mrID). 
         IF mrID = ? THEN.
         ELSE DO:
            FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
            ASSIGN
            berskaptemp.ENR       = MTRL.ENR
            berskaptemp.BENAMNING = MTRL.BENAMNING
            berskaptemp.ENHET     = MTRL.ENHET
            berskaptemp.PRIS      = MTRL.NPRIS
            berskaptemp.LEVKOD    = MTRL.LEVKOD.
         END. 
      END. 
        
      ELSE IF inkaofenrUB = FALSE THEN DO:  
         mrID = ?.
         RUN UTBYTESLISTA.P (INPUT Revers, INPUT listnr,INPUT berskaptemp.LEVKOD,INPUT berskaptemp.ENR,INPUT ValdBerLev, OUTPUT mrID). 
         IF mrID = ? THEN.
         ELSE DO:
            FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
            ASSIGN
            berskaptemp.ENR       = MTRL.ENR
            berskaptemp.BENAMNING = MTRL.BENAMNING
            berskaptemp.ENHET     = MTRL.ENHET
            berskaptemp.PRIS      = MTRL.NPRIS
            berskaptemp.LEVKOD    = MTRL.LEVKOD.
         END. 
      END.
      berskaptemp.LISTAKUNDLEV = TRUE.
   END.    
  
END PROCEDURE.


