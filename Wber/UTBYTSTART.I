
/*------------------------------------------------------------------------
    File        : UTBYTSTART.I
    Purpose     : list_mtrl

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
   
   FOR EACH list_mtrl:
      list_mtrl.LISTAKUNDLEV = FALSE.
   END.   
   /*Anders Olsson Elpool i Umeå AB  9 nov 2022 17:43:59 
    list_mtrl.LISTAKUNDLEV = true på de poster som är kollade 
   */
   FOR EACH list_mtrl WHERE list_mtrl.LISTAKUNDLEV = FALSE:
      inkaofenrUB = FALSE.
      FIND FIRST INKMTRL WHERE INKMTRL.BERNR = INTEGER(BerValdNr) AND INKMTRL.OMRADE = BerValdaOmr AND INKMTRL.INKBESTID > 0  
      AND INKMTRL.LEVKOD = list_mtrl.LEVKOD AND INKMTRL.ENRORG = list_mtrl.ENR AND INKMTRL.NUM = list_mtrl.NUM AND INKMTRL.SKAPNUM = list_mtrl.SKAPNUM NO-LOCK NO-ERROR.               
      IF AVAILABLE INKMTRL THEN inkaofenrUB = TRUE.
      IF inkaofenrUB = FALSE THEN DO:        
         IF list_mtrl.LEVKOD =  ValdBerLev AND listnr = 0 THEN DO:
            /*Anders Olsson Elpool i Umeå AB  16 nov 2021 16:30:39 
              byter ej mtrl om lev = vald lev och ingen kundlista byter bara om man har en lev till samma lev
            */
            
            RUN UTBYTESLISTA.P (INPUT "GbNb", INPUT listnr,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT ValdBerLev, OUTPUT mrID). 
            IF mrID = ? THEN.
            ELSE DO:
               FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
               ASSIGN
               list_mtrl.ENR       = MTRL.ENR
               list_mtrl.BENAMNING = MTRL.BENAMNING
               list_mtrl.ENHET     = MTRL.ENHET
               list_mtrl.PRIS      = MTRL.NPRIS
               list_mtrl.LEVKOD    = MTRL.LEVKOD.
            END. 
         END.   
         ELSE DO:  
            RUN UTBYTESLISTA.P (INPUT Revers, INPUT listnr,INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR,INPUT ValdBerLev, OUTPUT mrID). 
            IF mrID = ? THEN.
            ELSE DO:
               FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
               ASSIGN
               list_mtrl.ENR       = MTRL.ENR
               list_mtrl.BENAMNING = MTRL.BENAMNING
               list_mtrl.ENHET     = MTRL.ENHET
               list_mtrl.PRIS      = MTRL.NPRIS
               list_mtrl.LEVKOD    = MTRL.LEVKOD.
            END. 
         END.   
      END.
      list_mtrl.LISTAKUNDLEV = TRUE.
   END.    
  
END PROCEDURE.

