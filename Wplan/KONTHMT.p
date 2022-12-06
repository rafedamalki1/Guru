/*KONTHMT.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTPLANTEMP.I}
DEFINE NEW SHARED TEMP-TABLE kontkod   
   FIELD KONTO LIKE KONTO.KONTO
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD BENAMNING LIKE KONTO.BENAMNING
   INDEX KNR IS PRIMARY KONTONR ASCENDING. 
DEFINE INPUT PARAMETER ktovar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kontkod.
EMPTY TEMP-TABLE kontkod NO-ERROR. 

OPEN QUERY kq FOR EACH KONTO WHERE KONTO.KONTO = ktovar NO-LOCK.
GET FIRST kq NO-LOCK.
DO WHILE AVAILABLE(KONTO):
   CREATE kontkod.
   ASSIGN
   kontkod.KONTO = ktovar
   kontkod.KONTONR = KONTO.KONTONR
   kontkod.BENAMNING = KONTO.BENAMNING.
   GET NEXT kq NO-LOCK. 
END.
CLOSE QUERY kq.