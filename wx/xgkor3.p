/*xGKor.p*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
DEFINE VARIABLE manval AS LOGICAL NO-UNDO.
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vknummer AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
     
      IF globforetag = "GKAL" THEN DO:
         RUN XVKOSATT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.

      END.
      

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.
