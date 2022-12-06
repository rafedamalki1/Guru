/*STARTFORAPPC.I*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
/*{EGENBNS.I}*/
Guru.Konstanter:varforetypchar[48] = classerkollin.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
