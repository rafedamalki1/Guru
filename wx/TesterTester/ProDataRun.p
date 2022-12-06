DEFINE NEW SHARED VARIABLE appcon  AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE apphand AS HANDLE  NO-UNDO.

DEFINE            VARIABLE musz    AS LOGICAL NO-UNDO.



SESSION:DEBUG-ALERT = YES.
RUN modules/global/startclass.p.
RUN modules/global/startloginclass.p (INPUT 2, OUTPUT musz).
IF musz = TRUE THEN QUIT.
DEFINE VARIABLE r AS Tester.ProDataTest NO-UNDO.
r = NEW Tester.ProDataTest().
WAIT-FOR r:ShowDialog().
DELETE OBJECT r NO-ERROR.
 
