/*G:\DELAD\PRO9\GURU\XGKTIDFEL.P*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}

RUN GKAFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
(INPUT "\\granguru\guru_ser\server\pro9s\KALESAMM.TXT",INPUT 05/31/2003).


IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.
