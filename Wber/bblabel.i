/* bblabel.i */

DEFINE TEMP-TABLE bblabeltemp NO-UNDO  
   FIELD F1      AS CHARACTER FORMAT "x(20)" LABEL "F1"               
   FIELD F2      AS CHARACTER FORMAT "X(20)" LABEL "F2"               
   FIELD F3      AS CHARACTER FORMAT "X(20)" LABEL "F3"               
   FIELD F4      AS CHARACTER FORMAT "X(20)" LABEL "F4"               
   FIELD F5      AS CHARACTER FORMAT "X(20)" LABEL "F5"               
   FIELD F6      AS CHARACTER FORMAT "X(20)" LABEL "F6"               
   FIELD KONSKOD AS INTEGER   FORMAT ">>>>9" LABEL "Konstruktionskod" 
   FIELD ID1     AS CHARACTER
   FIELD ID2     AS CHARACTER FORMAT "X(20)" LABEL "ID"              
   FIELD FRI3    AS CHARACTER FORMAT "X(20)" LABEL "FRI"              
   INDEX BBEN           F1
   INDEX KOD IS PRIMARY KONSKOD. 

