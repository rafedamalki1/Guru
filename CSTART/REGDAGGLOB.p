/*REGDAGGLOB.p*/

IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 1 THEN Guru.GlobalaVariabler:regdagnamn = 's�n'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 2 THEN Guru.GlobalaVariabler:regdagnamn = 'm�n'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 3 THEN Guru.GlobalaVariabler:regdagnamn = 'tis'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 4 THEN Guru.GlobalaVariabler:regdagnamn = 'ons'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 5 THEN Guru.GlobalaVariabler:regdagnamn = 'tor'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 6 THEN Guru.GlobalaVariabler:regdagnamn = 'fre'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 7 THEN Guru.GlobalaVariabler:regdagnamn = 'l�r'.
