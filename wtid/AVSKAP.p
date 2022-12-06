/*AVSKAP.P*/
{AVDTEMP.I}
DEFINE OUTPUT PARAMETER TABLE FOR avdtemp .

OPEN QUERY oq FOR EACH AVDELNING WHERE AVDELNING.KOSTMASK = 0 NO-LOCK.
GET FIRST oq NO-LOCK.
DO WHILE AVAILABLE(AVDELNING):
   CREATE avdtemp.
   ASSIGN
   avdtemp.AVDELNINGNAMN = AVDELNING.AVDELNINGNAMN 
   avdtemp.AVDELNINGNR = AVDELNING.AVDELNINGNR.     
   GET NEXT oq NO-LOCK.
END.

   