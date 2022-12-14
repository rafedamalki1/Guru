/*FORKUND.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE exkundtemp NO-UNDO
   FIELD ADRESS   AS CHARACTER
   FIELD KONTAKT  AS CHARACTER
   FIELD KUNDNAMN AS CHARACTER
   FIELD KUNDNR   AS INTEGER
   FIELD ORT      AS CHARACTER
   FIELD PNR      AS CHARACTER
   FIELD TEL      AS CHARACTER
   INDEX KUNDNR KUNDNR.
DEFINE TEMP-TABLE extraexkundtemp NO-UNDO LIKE exkundtemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE kundtemp NO-UNDO
   FIELD KALKNR   AS INTEGER
   FIELD K1       AS INTEGER
   FIELD K2       AS INTEGER
   FIELD K3       AS INTEGER
   FIELD K4       AS INTEGER
   FIELD K5       AS INTEGER
   INDEX KALKNR KALKNR.
DEFINE TEMP-TABLE kalkftemp2
   FIELD KALKNR AS INTEGER FORMAT ">>>>>>9"  
   FIELD NUM AS INTEGER FORMAT ">>>9" 
   FIELD ARBKOD AS CHARACTER FORMAT "X(5)"   
   FIELD LOPNR AS INTEGER FORMAT ">>>"
   FIELD PROCENT AS INTEGER FORMAT ">>9"
   FIELD ANTAL AS DECIMAL FORMAT ">>>9.999" LABEL "Antal"
   FIELD ENHET AS CHARACTER FORMAT "x(3)"
   FIELD BESTID AS CHARACTER FORMAT "X(6)"
   FIELD BESTNAMN AS CHARACTER FORMAT "X(30)" LABEL "Beställare"
   FIELD ANM AS CHARACTER FORMAT "X(20)"
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)"
   FIELD K1 AS INTEGER FORMAT ">>9"
   FIELD K2 AS INTEGER FORMAT ">>9"
   FIELD K3 AS INTEGER FORMAT ">>9"
   FIELD K4 AS INTEGER FORMAT ">>9"
   FIELD K5 AS INTEGER FORMAT ">>9"
   INDEX NUM IS PRIMARY NUM ARBKOD LOPNR. 
    

