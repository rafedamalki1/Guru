/*REGDAG.P*/

DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
IF WEEKDAY(regdatum) = 1 THEN regdagnamn = 's�n'.
ELSE IF WEEKDAY(regdatum) = 2 THEN regdagnamn = 'm�n'.
ELSE IF WEEKDAY(regdatum) = 3 THEN regdagnamn = 'tis'.
ELSE IF WEEKDAY(regdatum) = 4 THEN regdagnamn = 'ons'.
ELSE IF WEEKDAY(regdatum) = 5 THEN regdagnamn = 'tor'.
ELSE IF WEEKDAY(regdatum) = 6 THEN regdagnamn = 'fre'.
ELSE IF WEEKDAY(regdatum) = 7 THEN regdagnamn = 'l�r'.
Guru.GlobalaVariabler:regdagnamn = regdagnamn.