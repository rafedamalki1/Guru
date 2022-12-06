/*DELETEHANDPROC.I*/

DEFINE VARIABLE i AS INTEGER NO-UNDO INITIAL 1.
DO WHILE i LE 25:
   IF VALID-HANDLE(handproc[i]) THEN DELETE PROCEDURE handproc[i].
   handproc[i] = ?.
   i = i + 1.
END.

DELETE OBJECT tthandle NO-ERROR.
tthandle = ?.
