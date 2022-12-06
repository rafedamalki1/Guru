
/*------------------------------------------------------------------------
    File        : UTBYTSTARTUM.I
    Purpose     : upp_mtrlTT

    Syntax      :   KABELANDBER.P

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
   FOR EACH upp_mtrlTT:
      upp_mtrlTT.LISTAKUNDLEV = FALSE.
   END.   
   FOR EACH upp_mtrlTT WHERE upp_mtrlTT.LISTAKUNDLEV = FALSE:
      IF upp_mtrlTT.LEVKOD =  ValdBerLev AND listnr = 0 THEN DO:
         /*Anders Olsson Elpool i Umeå AB  16 nov 2021 16:30:39 
           byter ej mtrl om lev = vald lev och ingen kundlista byter bara om man har en lev till samma lev
         */
         
         RUN UTBYTESLISTA.P (INPUT "GbNb", INPUT listnr,INPUT upp_mtrlTT.LEVKOD,INPUT upp_mtrlTT.ENR,INPUT ValdBerLev, OUTPUT mrID). 
         IF mrID = ? THEN.
         ELSE DO:
            FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
            ASSIGN
            upp_mtrlTT.ENR       = MTRL.ENR
            upp_mtrlTT.BENAMNING = MTRL.BENAMNING
            upp_mtrlTT.ENHET     = MTRL.ENHET
            upp_mtrlTT.PRIS      = MTRL.NPRIS
            upp_mtrlTT.LEVKOD    = MTRL.LEVKOD.
         END. 
      END.
         
      ELSE IF inkaofenrUB = FALSE THEN  DO:  
         RUN UTBYTESLISTA.P (INPUT Revers, INPUT listnr,INPUT upp_mtrlTT.LEVKOD,INPUT upp_mtrlTT.ENR,INPUT ValdBerLev, OUTPUT mrID). 
         IF mrID = ? THEN.
         ELSE DO:
            FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
            ASSIGN
            upp_mtrlTT.ENR       = MTRL.ENR
            upp_mtrlTT.BENAMNING = MTRL.BENAMNING
            upp_mtrlTT.ENHET     = MTRL.ENHET
            upp_mtrlTT.PRIS      = MTRL.NPRIS
            upp_mtrlTT.LEVKOD    = MTRL.LEVKOD.
         END. 
      END.
      upp_mtrlTT.LISTAKUNDLEV = TRUE.
   END.    
  
END PROCEDURE.


