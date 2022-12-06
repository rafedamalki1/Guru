/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XMBER.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.04.14 14:48 ELPAO   
     Modified: 
*/

FOR EACH mtrlber WHERE Ktypkod = "CDC" AND F1 = "" AND F2 = "" AND F3 = "" AND F4 = "" AND F5 = "" NO-LOCK:
   DISP mtrlber.
END.
