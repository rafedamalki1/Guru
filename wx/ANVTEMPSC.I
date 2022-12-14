/*ANVTEMPSC.I*/ 
DEFINE {&NEW} {&SHARED} TEMP-TABLE anvandartemp NO-UNDO
   FIELD ALLAONR AS LOGICAL
   FIELD ALLPERS AS LOGICAL 
   FIELD ANVANDARE AS CHARACTER FORMAT "X(12)"
   FIELD AV-LEVEL AS INTEGER
   FIELD AV-LOSEN AS CHARACTER CASE-SENSITIVE
   FIELD AV-NAMN AS CHARACTER FORMAT "X(40)"
   FIELD AVDELNINGNR AS INTEGER
   FIELD PERSONALKOD AS CHARACTER
   FIELD SIDL AS INTEGER
   FIELD SIDS AS INTEGER
   FIELD SKRIVARID AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD TTRECID AS RECID
   INDEX ANV IS PRIMARY ANVANDARE PERSONALKOD
   INDEX PERS PERSONALKOD AV-NAMN
   INDEX NAMN AV-NAMN PERSONALKOD.
