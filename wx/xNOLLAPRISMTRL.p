/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: G:\PRO9S\WX\XNOLLAPRISMTRL.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2010.04.15 10:16 ELPAO   
     Modified: 2010.04.15 11:41 ELPAO    
     Modified: 
*/

OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.LEVKOD = "2" AND MTRL.KALKNR = 0 NO-LOCK. 
GET FIRST mtrlq NO-LOCK.
DO WHILE AVAILABLE(MTRL):
   DO TRANSACTION:         
      FIND CURRENT MTRL EXCLUSIVE-LOCK.
      ASSIGN MTRL.NPRIS = 0 MTRL.BPRIS = 0.
   END.
   RELEASE MTRL.
   GET NEXT mtrlq NO-LOCK.
END.      
CLOSE QUERY mtrlq.
  