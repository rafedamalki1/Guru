/*ktotemp.I*/
DEFINE  {&NEW} {&SHARED} TEMP-TABLE ktotemp
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD JUDID AS CHARACTER
   FIELD VIJUDID AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD AVDELNINGNR AS INTEGER
   FIELD AKTIV AS LOGICAL
   FIELD RECTIDVIS AS RECID
   INDEX KONTO KONTO KONTONR.


