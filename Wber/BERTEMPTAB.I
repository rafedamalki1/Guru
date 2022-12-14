/*BERTEMPTAB.I*/  
DEFINE {&NEW} {&SHARED} TEMP-TABLE urberedningtemp NO-UNDO
   FIELD AKTIV          AS LOGICAL
   FIELD ANVANDARE      AS CHARACTER
   FIELD AONR           AS CHARACTER
   FIELD BENAMNING      AS CHARACTER
   FIELD BERAONR        AS CHARACTER
   FIELD BERNR          AS INTEGER
   FIELD DELNR          AS INTEGER
   FIELD KALKYL         AS LOGICAL
   FIELD KATAR          AS INTEGER
   FIELD OMRADE         AS CHARACTER
   INDEX BERNR IS PRIMARY BERNR
   INDEX OMRADE OMRADE BERNR AONR DELNR
   INDEX AONR AONR DELNR BERNR
   INDEX DELNR DELNR AONR
   INDEX AKTIV AKTIV BERNR.

DEFINE {&NEW} {&SHARED} TEMP-TABLE eurberedningtemp NO-UNDO LIKE urberedningtemp. 
DEFINE {&NEW} {&SHARED} TEMP-TABLE valberedningtemp NO-UNDO LIKE urberedningtemp.

