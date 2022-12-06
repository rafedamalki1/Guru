/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: FLBERTEMP.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.03.30 15:46 ELPAO   
     Modified: 2009.03.30 15:46 ELPAO    
     Modified: 
*/

DEFINE TEMP-TABLE flerbertemp NO-UNDO
   FIELD BERAONR  AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD BERNR AS INTEGER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BEREDARE AS CHARACTER
   INDEX OMR OMRADE BERAONR
   INDEX AONR AONR DELNR.
