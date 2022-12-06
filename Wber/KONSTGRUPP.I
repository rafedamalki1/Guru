/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: KONSTGRUPP.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.07.03 08:38 ELPAO   
     Modified: 
*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE konstgrptemp NO-UNDO
   FIELD KONSKOD   AS INTEGER FORMAT ">>>>9" INITIAL 0 LABEL "Konstruktionsgruppkod"
   FIELD BENAMNING AS CHARACTER FORMAT "X(40)" LABEL "Konstruktionsgrupp"
   FIELD ORDNING   AS INTEGER FORMAT ">9" INITIAL ? LABEL "Ordning"
   FIELD UID AS INTEGER  FORMAT "->,>>>,>>9" INITIAL 0 
   FIELD UNAMN AS CHARACTER FORMAT "X(8)" LABEL "Kund" 
   FIELD KONSKODMASTER AS INTEGER FORMAT ">>>>9" LABEL "Konstruktionsgruppkod"
   FIELD KONSTGRPLAST AS LOGICAL  
   INDEX BEN BENAMNING
   INDEX KOD IS PRIMARY KONSKOD
   INDEX ORD ORDNING.
 