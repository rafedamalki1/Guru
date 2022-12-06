/*DEPATEMP.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE depatemp NO-UNDO
   FIELD Dep-Nr     AS  INTEGER FORMAT ">>>" LABEL "Depå-nr"                            
   FIELD Benamning    AS  CHARACTER FORMAT "x(40)" LABEL "Benämning"                            
   FIELD Adress     AS  CHARACTER  FORMAT "x(30)"                           
   FIELD FakAdress     AS  CHARACTER FORMAT "x(30)" LABEL "Adress"                          
   FIELD FakOrt     AS  CHARACTER FORMAT "x(30)" LABEL "Ort"
   FIELD FakPnr AS  CHARACTER  FORMAT "XXX XX" LABEL "Postnr"                     
   FIELD FaxNr     AS  CHARACTER  FORMAT "x(15)"                         
   FIELD Firma    AS  CHARACTER FORMAT "x(40)" LABEL "Firmanamn" 
   FIELD LevAdress     AS  CHARACTER   FORMAT "x(30)"
   FIELD LevOrt     AS  CHARACTER  FORMAT "x(30)" LABEL "Ort"
   FIELD LevPnr     AS  CHARACTER FORMAT "XXX XX" LABEL "Postnr"
   FIELD Ort     AS  CHARACTER FORMAT "x(30)"
   FIELD Pnr     AS  CHARACTER FORMAT "XXX XX"
   FIELD Telefon     AS  CHARACTER FORMAT "x(15)"
   FIELD ANVANDARE     AS  CHARACTER FORMAT "x(12)"
   INDEX dep-nr IS PRIMARY dep-nr.
 DEFINE BUFFER depatempbuff FOR depatemp.  

