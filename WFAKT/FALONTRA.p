/*FALONTRA.P*/
&Scoped-define NEW NEW
{REGVAR.I}
{FAKTTEMP.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE sumpkod LIKE TIDREGITAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE sumanstf LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO. 
DEFINE INPUT PARAMETER faktrec AS RECID NO-UNDO.     
DEFINE INPUT PARAMETER FILL-IN-TOMDAT AS DATE NO-UNDO.
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kollvecko LIKE TIDREGITAB.VECKOKORD NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR sumtidtemp.
FIND FAKTPLAN WHERE RECID(FAKTPLAN) = faktrec NO-LOCK NO-ERROR.
{FALONTRASTART.I}
{FALONTRA.I}