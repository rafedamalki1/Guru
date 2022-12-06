/*AONRUTSID.I*/
DEFINE VARIABLE aoutvar AS CHARACTER NO-UNDO FORMAT "X(30)".
IF Guru.GlobalaVariabler:plusaonr = "" OR Guru.GlobalaVariabler:plusaonr = ? THEN aoutvar = "".
ELSE aoutvar = Guru.Konstanter:gaok + ": " + Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1]).

