/*DHMT.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE arbtidtemp NO-UNDO
   FIELD ARBTIDKOD AS INTEGER 
   FIELD START AS DECIMAL
   FIELD FRUKOSTSTART   AS DECIMAL
   FIELD FRUKOSTSLUT    AS DECIMAL
   FIELD LUNCHSTART     AS DECIMAL
   FIELD LUNCHSLUT      AS DECIMAL
   FIELD KAFESTART      AS DECIMAL
   FIELD KAFESLUT       AS DECIMAL
   FIELD SLUT           AS DECIMAL
   FIELD TOTDAGTID      AS DECIMAL
   FIELD TYP            AS INTEGER
   INDEX ARBTIDKOD IS PRIMARY ARBTIDKOD
   INDEX TYP TYP.
DEFINE TEMP-TABLE  earbtidtemp LIKE arbtidtemp .