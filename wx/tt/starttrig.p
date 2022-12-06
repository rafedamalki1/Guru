/* starttrig.P*/

DEFINE input parameter brw_UFAKT AS HANDLE NO-UNDO.

def SHARED TEMP-TABLE ttTest  NO-UNDO
   FIELD iValue AS INTEGER.

Def var Okvar as logical.

DEFINE VARIABLE hDefaultBuffer AS HANDLE NO-UNDO.
hDefaultBuffer = TEMP-TABLE ttTest:DEFAULT-BUFFER-HANDLE. 

ON 'VALUE-CHANGED':U OF  BRW_UFAKT PERSISTENT RUN vcbrwbere_UI IN THIS-PROCEDURE.


PROCEDURE vcbrwbere_UI :
   MESSAGE string(hDefaultBuffer:rowid) VIEW-AS ALERT-BOX. 
END PROCEDURE.



