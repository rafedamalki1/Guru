/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: DATTABBER.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.05.11 09:51 ELPAO   
     Modified: 2009.05.11 11:02 ELPAO    
     Modified: 
*/


DEFINE TEMP-TABLE dat_tab
   FIELD DATUM AS DATE
   FIELD DELNR AS INTEGER
   FIELD KLOCKAN AS DECIMAL
   INDEX DATUM IS PRIMARY DATUM KLOCKAN.
   
