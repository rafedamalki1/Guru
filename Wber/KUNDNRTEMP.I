/*KUNDNRTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE kundnrtemp NO-UNDO
   FIELD KUNDNR AS INTEGER   FORMAT "->>>>>>9" INITIAL 0 
   FIELD Dep-Nr AS INTEGER   FORMAT ">>>"        INITIAL 0 LABEL "Dep?-Nr" 
   FIELD LEVKOD AS CHARACTER FORMAT "x(4)"                 LABEL "LEV-ID"
   INDEX DEPNR IS PRIMARY Dep-Nr LEVKOD.
