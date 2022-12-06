/*BESTSTATTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE beststattemp NO-UNDO
   FIELD BESTNR      AS INTEGER   FORMAT "->>>>>>9"            
   FIELD BERNR       AS INTEGER   FORMAT "->>>>>>9" INITIAL 0
   FIELD ANVANDARE   AS CHARACTER FORMAT "x(12)"               LABEL "ANVÄNDARE"  
   FIELD DATUM       AS DATE      FORMAT "99/99/99"  INITIAL ?          
   FIELD BESTALLD    AS CHARACTER FORMAT "X(18)"               LABEL "Beställd"   
   FIELD LEVKOD      AS CHARACTER FORMAT "x(4)"                LABEL "LEV-ID"     
   FIELD TID         AS DECIMAL   FORMAT 99.99                 LABEL "Klockan"    
   FIELD Dep-Nr      AS INTEGER   FORMAT ">>>"      INITIAL 0  LABEL "Depå-Nr"    
   FIELD OMRADE      AS CHARACTER FORMAT "x(6)"                LABEL "OMRÅDE"     
   FIELD BERDATUM    AS DATE      FORMAT "99/99/99" INITIAL ?          
   INDEX BERNR IS PRIMARY BERNR DATUM TID
   INDEX BERNR2 OMRADE BERNR DATUM TID
   INDEX BESTNR BESTNR DATUM TID
   INDEX BESTNR2 Dep-Nr BESTNR DATUM TID.

DEFINE {&NEW} {&SHARED} TEMP-TABLE ebeststattemp NO-UNDO LIKE beststattemp.

/*Komb. av BESTSTAT och LEVERANTOR*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE bestatlevtemp NO-UNDO
   FIELD BESTNR      AS INTEGER   FORMAT "->>>>>>9"            
   FIELD BERNR       AS INTEGER   FORMAT "->>>>>>9" INITIAL 0
   FIELD ANVANDARE   AS CHARACTER FORMAT "x(12)"               LABEL "ANVÄNDARE"  
   FIELD DATUM       AS DATE      FORMAT "99/99/99"  INITIAL ?          
   FIELD BESTALLD    AS CHARACTER FORMAT "X(18)"               LABEL "Beställd"   
   FIELD TID         AS DECIMAL   FORMAT 99.99                 LABEL "Klockan"    
   FIELD Dep-Nr      AS INTEGER   FORMAT ">>>"      INITIAL 0  LABEL "Depå-Nr"    
   FIELD OMRADE      AS CHARACTER FORMAT "x(6)"                LABEL "OMRÅDE"     
   FIELD BERDATUM    AS DATE      FORMAT "99/99/99" INITIAL ?          
   FIELD LEVKOD      AS CHARACTER FORMAT "x(4)"                LABEL "LEV-ID"     
   FIELD LEVNAMN    AS  CHARACTER FORMAT "x(25)"               LABEL "Namn"                            
   FIELD LEVADR     AS  CHARACTER FORMAT "x(20)"                             
   FIELD LEVORT     AS  CHARACTER FORMAT "x(15)"                             
   FIELD LEVTEL     AS  CHARACTER FORMAT "xxxx-xxxxxxx"                      
   FIELD LEVKONTAKT AS  CHARACTER FORMAT "x(25)"                             
   FIELD LEVPNR     AS  CHARACTER FORMAT "x(6)"                              
   FIELD BORTTAG    AS  LOGICAL   FORMAT "Ja/Nej" INITIAL "Nej"  
   INDEX BERNR IS PRIMARY BERNR DATUM TID
   INDEX BERNR2 OMRADE BERNR DATUM TID
   INDEX BESTNR BESTNR DATUM TID
   INDEX BESTNR2 Dep-Nr BESTNR DATUM TID
   INDEX LEV LEVKOD.
