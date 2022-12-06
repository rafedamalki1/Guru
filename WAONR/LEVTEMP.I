/*LEVTEMP.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE levtemp NO-UNDO
   FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"                              
   FIELD LEVNAMN    AS  CHARACTER FORMAT "x(25)" LABEL "Namn"                            
   FIELD LEVADR     AS  CHARACTER FORMAT "x(20)"                             
   FIELD LEVORT     AS  CHARACTER FORMAT "x(15)"                             
   FIELD LEVTEL     AS  CHARACTER FORMAT "xxxx-xxxxxxx"                      
   FIELD LEVKONTAKT AS  CHARACTER FORMAT "x(25)"                             
   FIELD LEVPNR     AS  CHARACTER FORMAT "x(6)"                              
   FIELD BORTTAG    AS  LOGICAL   FORMAT "Ja/Nej" INITIAL "Nej"
   FIELD ORDNING AS INTEGER INITIAL 999
   
   INDEX LEV IS PRIMARY ORDNING LEVKOD.
   /*
   
INDEX LEV IS PRIMARY LEVKOD.    
*/

DEFINE TEMP-TABLE extralevtemp NO-UNDO LIKE levtemp.
