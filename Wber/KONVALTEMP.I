/*KONVALTEMP.I DEFINITIONER TEMP-TABLE kon_val BERVAL*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE kon_val NO-UNDO                         
   
   FIELD BERAONR AS CHARACTER
   FIELD OMRADE AS CHARACTER 
   FIELD NUM         AS INTEGER FORMAT ">>>9"
   FIELD GRUPP       AS INTEGER
   FIELD F1          AS CHARACTER FORMAT "X(10)"
   FIELD F2          AS CHARACTER FORMAT "X(10)"
   FIELD F3          AS CHARACTER FORMAT "X(10)"
   FIELD F4          AS CHARACTER FORMAT "X(10)"
   FIELD F5          AS CHARACTER FORMAT "X(10)"
   FIELD F6          AS CHARACTER FORMAT "X(10)"
   FIELD ID          AS LOGICAL FORMAT "Ja/Nej" INITIAL NO
   FIELD UPPLAG      AS INTEGER FORMAT ">>" INITIAL ?
   FIELD KSKAP       AS LOGICAL INITIAL NO
   FIELD TYP         AS INTEGER FORMAT ">" INITIAL 0 
   FIELD SKAPNUM     AS INTEGER FORMAT ">>>9"
   FIELD ANMARK      AS CHARACTER FORMAT "X(8)"
   FIELD ID1         AS CHARACTER FORMAT "X(10)"
   FIELD ID2         AS CHARACTER FORMAT "X(10)"
   FIELD EXTRA       AS CHARACTER FORMAT "X(1)"
   FIELD EXTRA1      AS CHARACTER 
   FIELD EXTRA2      AS CHARACTER 
   FIELD EXTRA3      AS CHARACTER
   FIELD ORD         AS INTEGER
   FIELD ANDRAD      AS CHARACTER
   FIELD MTRLFINNS   AS CHARACTER
   FIELD KALKFINNS   AS CHARACTER
   FIELD ORT         AS CHARACTER
   FIELD TTRECID  AS RECID
   FIELD UTFORD AS LOGICAL INITIAL FALSE FORMAT "Ja/Nej"
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX NUM2 NUM SKAPNUM ASCENDING
   INDEX ORD ORD ASCENDING
   INDEX ANDRAD ANDRAD
   INDEX MTRLFINNS MTRLFINNS
   INDEX KALKFINNS KALKFINNS
   INDEX ID2 ID2.
DEFINE TEMP-TABLE sparakon_val NO-UNDO LIKE kon_val.
DEFINE TEMP-TABLE bortkon_val NO-UNDO LIKE kon_val.
DEFINE TEMP-TABLE ekon_val NO-UNDO LIKE kon_val.
DEFINE TEMP-TABLE ekon_val2 NO-UNDO LIKE kon_val.
DEFINE {&NEW} {&SHARED} TEMP-TABLE kskkon_val NO-UNDO  LIKE kon_val
   FIELD KABELADRESS AS CHARACTER.
DEFINE {&NEW} {&SHARED} TEMP-TABLE andrakon_val NO-UNDO  LIKE kon_val
    FIELD KABELADRESS AS CHARACTER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE kon_display NO-UNDO  LIKE kon_val.

DEFINE {&NEW} {&SHARED} TEMP-TABLE idkon_val NO-UNDO  LIKE kon_val.
/*
   FIELD NUM LIKE BERVAL.NUM
   FIELD GRUPP LIKE BERVAL.KONSKOD
   FIELD F1 LIKE BERVAL.KTYPKOD
   FIELD F2 LIKE BERVAL.F2
   FIELD F3 LIKE BERVAL.F3
   FIELD F4 LIKE BERVAL.F4
   FIELD F5 LIKE BERVAL.F5
   FIELD F6 LIKE BERVAL.F6
   FIELD ID LIKE BERVAL.ID        
   FIELD UPPLAG LIKE BERVAL.UPPLAG
   FIELD KSKAP LIKE BERVAL.KSKAP 
   FIELD TYP LIKE BERVAL.TYP   
   FIELD SKAPNUM LIKE BERVAL.SKAPNUM
   FIELD ANMARK LIKE BERVAL.ANMARK
   FIELD ID2 AS CHARACTER FORMAT "X(10)"
   FIELD EXTRA AS CHARACTER FORMAT "X(1)"
   FIELD ORD AS INTEGER
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX NUM2 NUM SKAPNUM ASCENDING
   INDEX ORD ORD ASCENDING. 
   */
   
