/*STARTFORAPP.I*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
/*{EGENBNS.I}*/
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
