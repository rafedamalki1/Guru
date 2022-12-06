/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: LOGGNING.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.08.20 16:31 ELPAO   
     Modified: 
*/


DEFINE {&NEW} {&SHARED} TEMP-TABLE templogpost  NO-UNDO   
   FIELD LOGNIV AS INTEGER
   FIELD TYP AS CHARACTER
   FIELD DATUM AS DATE
   FIELD TID AS INTEGER
   FIELD ANVANDARE AS CHARACTER
   FIELD DATORNAMN AS CHARACTER
   FIELD PRONAMN AS CHARACTER
   FIELD SOKBEGREPP AS CHARACTER
   FIELD LOGROW AS ROWID.

DEFINE {&NEW} {&SHARED} TEMP-TABLE templogniv  NO-UNDO   
   FIELD LOGNIV AS INTEGER.


DEFINE {&NEW} {&SHARED} TEMP-TABLE templogpost2 NO-UNDO LIKE templogpost.
DEFINE {&NEW} {&SHARED} TEMP-TABLE templogniv2 NO-UNDO LIKE templogniv.
