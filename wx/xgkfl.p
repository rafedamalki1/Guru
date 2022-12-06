/*xGKFL.p*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}

RUN GKALFL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
(INPUT 01/31/2003, INPUT "elpa").

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.

