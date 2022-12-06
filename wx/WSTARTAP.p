/*WSTARTAP.P SKA EJ GÅ ATT KOMPILERA I WEB*/

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.    /*DEN VAR SKA EJ VARA DELAD*/
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

{CONAPP.I}
DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO.
IF NOT Guru.Konstanter:appcon THEN RETURN.
nyaprog = TRUE.

/*
REPEAT:
   RUN WSTART.W (INPUT gforetag,INPUT "",INPUT-OUTPUT nyaprog).
   IF nyaprog = FALSE THEN LEAVE.
END.
*/
RUN WSTART.W (INPUT gforetag,INPUT "",INPUT-OUTPUT nyaprog).
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
apphand = ?.
appcon = FALSE.
