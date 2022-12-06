/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: DEFPRINTER.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2004.09.02 11:23 ELPAO   
     Modified: 2004.09.02 12:51 ELPAO    
     Modified: 
*/
DEFINE OUTPUT PARAMETER printername AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
/*DEFINE SHARED VARIABLE hpApi AS HANDLE NO-UNDO.*/
DEF VAR cchRet      AS INTEGER NO-UNDO.
 
printername = FILL(" ",100).  /* = allocate memory, don't forget! */
RUN GetProfileStringA IN Guru.Konstanter:hpApi ("windows",
                                   "device",
                                   "-unknown-,",
                                   OUTPUT printername,
                                   LENGTH(printername),
                                   OUTPUT cchRet).
 
printername = ENTRY(1,printername).
 
IF printername="-unknown-" THEN musz = TRUE.


 
