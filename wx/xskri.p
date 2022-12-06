/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: XSKRI.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2004.09.02 11:23 ELPAO   
     Modified: 
*/

{windows.i}
DEF VAR printername AS CHAR    NO-UNDO.
DEF VAR cchRet      AS INTEGER NO-UNDO.
 
printername = FILL(" ",100).  /* = allocate memory, don't forget! */
RUN GetProfileString{&A} IN Guru.Konstanter:hpApi ("windows",
                                   "device",
                                   "-unknown-,",
                                   OUTPUT printername,
                                   LENGTH(printername),
                                   OUTPUT cchRet).
 
/* split name from driver and port. Note that the 
   default "-unknown-," must have at leat one comma or 
   the ENTRY function may raise an error  */
printername = ENTRY(1,printername).
 
/* use the result */
IF printername="-unknown-" THEN
   MESSAGE "something is wrong with your WIN.INI" 
           VIEW-AS ALERT-BOX.
ELSE
   MESSAGE "your default printer is: " printername
           VIEW-AS ALERT-BOX.

 
