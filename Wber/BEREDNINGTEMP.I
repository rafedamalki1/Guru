/*BEREDNINGTEMP.I*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE beredningtemp NO-UNDO
   FIELD BERNR        AS INTEGER      FORMAT    "->>>>>>9"   INITIAL 0
   FIELD BERAONR      AS CHARACTER    FORMAT    "x(6)"
   FIELD OMRADE       AS CHARACTER    FORMAT    "x(6)"
   FIELD BENAMNING    AS CHARACTER    FORMAT    "x(30)"
   FIELD ANVANDARE    AS CHARACTER    FORMAT    "x(12)"
   FIELD AONR         AS CHARACTER    FORMAT    "X(6)"         INITIAL ?
   FIELD DELNR        AS INTEGER      FORMAT    999            INITIAL ?
   FIELD AKTIV        AS LOGICAL      FORMAT    "Ja/Nej"       INITIAL TRUE 
   FIELD KALKYL       AS LOGICAL      FORMAT    "Ja/Nej"       INITIAL FALSE
   FIELD KATAR        AS INTEGER      FORMAT    9999           INITIAL ?
   FIELD UID AS INTEGER
   FIELD TTRECID AS RECID
   INDEX BERNR IS PRIMARY OMRADE BERNR
   INDEX AONR AONR DELNR.
DEFINE TEMP-TABLE eberedningtemp NO-UNDO LIKE beredningtemp.
