/* mtrlkomtemp.i*/
DEFINE TEMP-TABLE mtrlkomtemp NO-UNDO
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD KOMMENTAR AS CHARACTER
   FIELD LEVKOD AS CHARACTER
   FIELD OMREGTID AS DECIMAL
   INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE emtrlkomtemp NO-UNDO LIKE mtrlkomtemp.
DEFINE TEMP-TABLE ekmtrlkomtemp NO-UNDO LIKE mtrlkomtemp.
DEFINE TEMP-TABLE felkmex NO-UNDO LIKE mtrlkomtemp.
