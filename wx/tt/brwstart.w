
def SHARED TEMP-TABLE ttTest  NO-UNDO
   FIELD iValue AS INTEGER.

DEFINE VARIABLE hProc AS HANDLE      NO-UNDO.
DEFINE QUERY BRW_UFAKT FOR ttTest SCROLLING.

OPEN QUERY BRW_UFAKT FOR EACH ttTest.

DEFINE BROWSE BRW_UFAKT
QUERY BRW_UFAKT NO-LOCK DISPLAY
     
    ttTest.iValue WIDTH 13 
   WITH SIZE 63 BY 13.25. 
    
DEFINE FRAME FRAME-A
     BRW_UFAKT AT ROW 13.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 28.25.


RUN tt\starttrig.P PERSISTENT SET hProc (INPUT BRW_UFAKT:handle).



