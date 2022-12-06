/*BBENAMNTEMP.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE bbenamntemp NO-UNDO
   FIELD B1       AS  CHARACTER FORMAT "x(20)" LABEL "F1"               
   FIELD B2       AS  CHARACTER FORMAT "X(20)" LABEL "F2"               
   FIELD B3       AS  CHARACTER FORMAT "X(20)" LABEL "F3"               
   FIELD B4       AS  CHARACTER FORMAT "X(20)" LABEL "F4"               
   FIELD B5       AS  CHARACTER FORMAT "X(20)" LABEL "F5"               
   FIELD B6       AS  CHARACTER FORMAT "X(20)" LABEL "F6"               
   FIELD KONSKOD  AS  INTEGER   FORMAT ">>>>9" LABEL "Konstruktionskod" 
   FIELD ID1      AS  CHARACTER FORMAT "X(20)" LABEL "ID1"              
   FIELD ID2      AS  CHARACTER FORMAT "X(20)" LABEL "ID2"              
   INDEX BBEN B1
   INDEX KOD IS PRIMARY KONSKOD.
