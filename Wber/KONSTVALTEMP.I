
/*------------------------------------------------------------------------
    File        : KONSTVALTEMP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 14 13:44:20 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE konstvaltemp NO-UNDO
   FIELD TRIMKVALKOD   AS CHARACTER  FORMAT "X(8)"              LABEL "Konstruktionsvalskod"    
   FIELD KVALKOD   AS CHARACTER  FORMAT "X(8)"              LABEL "Konstruktionsvalskod"    
   FIELD KTYPKOD   AS CHARACTER  FORMAT "X(8)"              LABEL "Konstruktionskod"      
   FIELD KONSKOD   AS INTEGER    FORMAT ">>>>9"  INITIAL 0  LABEL "Konstruktionsgruppkod" 
   FIELD KOPP      AS LOGICAL    FORMAT "Ja/Nej" INITIAL NO LABEL "Kopplad"               
   FIELD BENAMNING AS CHARACTER  FORMAT "X(40)"             LABEL "Benämning"             
   FIELD BB        AS CHARACTER  FORMAT "x(20)"             LABEL "Konstruktionsvals fält"
   FIELD ORDNING   AS INTEGER    FORMAT ">9"     INITIAL ?  LABEL "Ordning"  
   FIELD KVALROW   AS ROWID
   FIELD BILD AS CHARACTER
   INDEX KOPIA IS PRIMARY KONSKOD KTYPKOD KVALKOD BB KOPP
   INDEX ORD KONSKOD KTYPKOD KOPP BB ORDNING
   INDEX ORD2 KONSKOD BB ORDNING.
