/*LONTILLAGG.I*/
DEFINE  TEMP-TABLE lontilltemp NO-UNDO
   FIELD AONROBL      AS LOGICAL
   FIELD ERSFINNS      AS LOGICAL
   FIELD ENHET        AS CHARACTER
   FIELD ERSATTNING   AS DECIMAL
   FIELD KOD          AS CHARACTER
   FIELD LONKODTEXT   AS CHARACTER
   FIELD LONTILLAGG   AS CHARACTER 
   FIELD LONTILLANTAL AS DECIMAL
   FIELD MULTIP       AS DECIMAL
   FIELD TYPKOD       AS CHARACTER
   FIELD VALBAR       AS LOGICAL
   FIELD VILART       AS CHARACTER
   FIELD MOMS       AS LOGICAL
   INDEX VILART IS PRIMARY KOD VILART
   INDEX LON KOD LONTILLAGG
   INDEX LONKODTEXT KOD LONKODTEXT
   INDEX ENHET KOD ENHET
   INDEX ERSFINNS ERSFINNS.
DEFINE TEMP-TABLE lonkorttemp NO-UNDO
   FIELD KORTLON     AS CHARACTER 
   FIELD LONTILLAGG  AS CHARACTER 
   INDEX LONTILLAGG IS PRIMARY LONTILLAGG KORTLON.
DEFINE TEMP-TABLE lontilltemp2 NO-UNDO LIKE lontilltemp .

