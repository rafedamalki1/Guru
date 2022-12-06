def SHARED TEMP-TABLE ttTest  NO-UNDO
   FIELD iValue AS INTEGER.

DEFINE VARIABLE hProc AS HANDLE      NO-UNDO.
DEFINE QUERY qTest FOR ttTest SCROLLING.

OPEN QUERY qTest FOR EACH ttTest.

/*
DEFINE BROWSE BRW_UFAKT
    faktplantemp.FAKTNR 
    faktplantemp.NAMN .
*/

RUN starttrig.P PERSISTENT SET hProc (INPUT QUERY qTest:handle).



