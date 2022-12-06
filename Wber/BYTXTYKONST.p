/* BYTXTYKONST.p BYT ARTIKEL X TILL ARTIKEL Y*/

{SMTRL.I} 
{BMTRL.I}

DEFINE TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE  TEMP-TABLE kon_temp
   FIELD KONSKOD AS INTEGER
   FIELD KTYPKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD ORDNING ASCENDING.

DEFINE INPUT PARAMETER TABLE FOR spec_mtrl.
DEFINE INPUT PARAMETER TABLE FOR byt_mtrl.
DEFINE INPUT PARAMETER TABLE FOR kon_temp.
DEFINE BUFFER mtrlberbuff FOR MTRLBER.
DEFINE BUFFER berstolpbuff FOR BERSTOLP.


FIND FIRST byt_mtrl NO-LOCK NO-ERROR.        
FOR EACH kon_temp:
   FOR EACH spec_mtrl :
      FOR EACH  MTRLBER WHERE MTRLBER.KTYPKOD = kon_temp.KTYPKOD AND  MTRLBER.ENR = spec_mtrl.ENR AND
      MTRLBER.LEVKOD = spec_mtrl.LEVKOD EXCLUSIVE-LOCK:
         FIND FIRST mtrlberbuff WHERE mtrlberbuff.ENR =  byt_mtrl.ENR AND mtrlberbuff.LEVKOD = byt_mtrl.LEVKOD AND 
         mtrlberbuff.KTYPKOD  = MTRLBER.KTYPKOD  AND mtrlberbuff.F1  = MTRLBER.F1 AND mtrlberbuff.F2  = MTRLBER.F2 AND 
         mtrlberbuff.F3  = MTRLBER.F3 AND mtrlberbuff.F4  = MTRLBER.F4 AND mtrlberbuff.F5  = MTRLBER.F5 AND 
         mtrlberbuff.LEVKOD   = MTRLBER.LEVKOD NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlberbuff THEN DO:
            DELETE MTRLBER.     
         END.
         ELSE DO:         
            ASSIGN
            MTRLBER.ENR = byt_mtrl.ENR
            MTRLBER.BENAMNING = byt_mtrl.BENAMNING
            MTRLBER.ENHET = byt_mtrl.ENHET
            MTRLBER.PRIS = byt_mtrl.NPRIS
            MTRLBER.SATS = byt_mtrl.SATS
            MTRLBER.LEVKOD = byt_mtrl.LEVKOD.
         END.   
      END.
      
   END.
END.         
         
