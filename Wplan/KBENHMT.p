/*KBENHMT.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTPLANTEMP.I}
DEFINE OUTPUT PARAMETER TABLE FOR kbenamntemp.
EMPTY TEMP-TABLE kbenamntemp NO-ERROR. 
OPEN QUERY kbq FOR EACH KBENAMNING USE-INDEX KBEN NO-LOCK.
GET FIRST kbq NO-LOCK.
DO WHILE AVAILABLE(KBENAMNING):
   CREATE kbenamntemp.
   BUFFER-COPY KBENAMNING TO kbenamntemp.
   GET NEXT kbq NO-LOCK.
END.
CLOSE QUERY kbq.