/*kalktemp2.i*/

{kalktemp23.i}

DEFINE {&NEW} {&SHARED} TEMP-TABLE ekalk_temp NO-UNDO LIKE kalk_temp
   FIELD SID AS INTEGER.
DEFINE TEMP-TABLE bytkalk_temp NO-UNDO 
   FIELD ARBKOD      AS CHARACTER FORMAT "X(5)"
   FIELD LOPNR       AS INTEGER FORMAT ">>>"
   FIELD BENAMNING   AS CHARACTER FORMAT "X(40)"
   FIELD ENHET       AS CHARACTER FORMAT "X(3)"
   INDEX KOD ARBKOD LOPNR ASCENDING. 

DEFINE TEMP-TABLE esumkalk_temp NO-UNDO LIKE sumkalk_temp.
DEFINE TEMP-TABLE ebytkalk_temp NO-UNDO LIKE bytkalk_temp.   
   
      
   

