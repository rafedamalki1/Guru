/*
     Filename: XDYNSTART.P
      Created: 05.01.0021 13:00ELPAO     
     Modified: 
*/
DEFINE VARIABLE orgtab AS CHARACTER NO-UNDO.
DEFINE VARIABLE temptab AS CHARACTER NO-UNDO.
DEFINE VARIABLE extrfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE tth AS HANDLE NO-UNDO.
DEFINE VARIABLE dynladdh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE mtrltemp NO-UNDO LIKE MTRL
   FIELD ROWFALT AS ROWID.
extrfalt = "ROWFALT".
orgtab = "mtrl".
temptab = "mtrltemp".
tth = TEMP-TABLE mtrltemp:HANDLE.
/*
RUN XDY.P PERSISTENT SET dynladdh
            (INPUT TABLE-HANDLE tth).
            */
RUN XDY.P PERSISTENT SET dynladdh (INPUT TABLE-HANDLE tth).
            

RUN laddatemp_UI IN dynladdh (OUTPUT TABLE-HANDLE tth,INPUT orgtab,
                                       INPUT temptab,INPUT extrfalt).
MESSAGE "ok1" VIEW-AS ALERT-BOX.
RUN laddasort_UI IN dynladdh (OUTPUT TABLE-HANDLE tth APPEND,INPUT orgtab,
                                       INPUT temptab,INPUT extrfalt).

MESSAGE "ok2" VIEW-AS ALERT-BOX.
