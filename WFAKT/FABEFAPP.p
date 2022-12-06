/*FABEFAPP.P*/
&Scoped-define NEW NEW                          
{FAKTTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR sumtidtemp.

OPEN QUERY sq FOR EACH sumtidtemp NO-LOCK,
EACH OVERTEXTFAKT WHERE OVERTEXTFAKT.OTEXTID = sumtidtemp.OTEXTID NO-LOCK.
GET FIRST sq EXCLUSIVE-LOCK.
DO WHILE AVAILABLE(sumtidtemp):
   ASSIGN sumtidtemp.VIOBEFATTNING = OVERTEXTFAKT.OTEXT. 
   GET NEXT sq EXCLUSIVE-LOCK.
END.
OPEN QUERY sbq FOR EACH sumtidtemp NO-LOCK,
EACH BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK.
GET FIRST sbq EXCLUSIVE-LOCK.
DO WHILE AVAILABLE(sumtidtemp):
   ASSIGN 
   sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + BEFATTNINGSTAB.NAMN
   sumtidtemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN. 
   GET NEXT sbq EXCLUSIVE-LOCK.
END.
OPEN QUERY slq FOR EACH sumtidtemp NO-LOCK,
EACH LONTILL WHERE LONTILL.LONTILLAGG = sumtidtemp.LONTILLAGG AND
LONTILL.KOD = sumtidtemp.ANSF NO-LOCK.
GET FIRST slq EXCLUSIVE-LOCK.
DO WHILE AVAILABLE(LONTILL):
   ASSIGN sumtidtemp.VILART = LONTILL.VILART. 
   GET NEXT slq EXCLUSIVE-LOCK.
END.
OPEN QUERY stq FOR EACH sumtidtemp NO-LOCK,
EACH TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = sumtidtemp.TRAAVTAL AND
TRAKTATAB.TRAKTKOD = sumtidtemp.TRAKTKOD NO-LOCK.
GET FIRST stq EXCLUSIVE-LOCK.
DO WHILE AVAILABLE(TRAKTATAB):
   ASSIGN sumtidtemp.VITRAKT = TRAKTATAB.VILART. 
   GET NEXT stq EXCLUSIVE-LOCK.
END.
