/*MTRLTAB.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrltemp NO-UNDO
   FIELD Enr     AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"                         
   FIELD Benamning     AS CHARACTER FORMAT "x(40)" LABEL "Benämning"                            
   FIELD Bestkvant     AS  DECIMAL FORMAT "->>>>9.99" INITIAL 0                          
   FIELD Enhet AS  CHARACTER    FORMAT "x(5)"                    
   FIELD BERKVANT    AS  DECIMAL FORMAT "->>>>9.99" INITIAL 0
   FIELD SUMMA     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0
   FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"
   FIELD LEVNAMN     AS  CHARACTER FORMAT "x(25)"
   FIELD BPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Bruttopris"
   FIELD NPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Nettopris"
   FIELD KALKNR    AS  INTEGER FORMAT ">>>>>>9" INITIAL 0
   FIELD BERLEV     AS  CHARACTER  FORMAT "x(4)"                           
   FIELD OFFERT     AS  LOGICAL FORMAT "Ja/Nej" INITIAL NO
   FIELD KPRIS     AS  DECIMAL   FORMAT ">>>>99.99" INITIAL 0
   FIELD KUND     AS  LOGICAL  FORMAT "Ja/Nej" INITIAL NO
   FIELD MTRLROW AS ROWID
   FIELD INDATETIME AS DATETIME
   INDEX BEN LEVKOD KALKNR BENAMNING
   INDEX Enr IS PRIMARY enr
   INDEX KALKNR KALKNR enr BENAMNING
   INDEX Lev LEVKOD KALKNR enr.
   
DEFINE TEMP-TABLE emtrltemp NO-UNDO LIKE mtrltemp.
DEFINE {&NEW} {&SHARED} TEMP-TABLE  kmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE  mkmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av för mtrlmeny.p nettopris*/
DEFINE TEMP-TABLE  ikmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris inköp*/
DEFINE TEMP-TABLE  eikmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia av bermtrl för att märka nettopris inköp*/
DEFINE TEMP-TABLE  ekmtrltemp NO-UNDO LIKE mtrltemp.  /*kopia kmtrltemp för kommentarer*/
