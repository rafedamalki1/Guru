/*REGDAGGLOB.p*/

IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 1 THEN Guru.GlobalaVariabler:regdagnamn = 'sön'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 2 THEN Guru.GlobalaVariabler:regdagnamn = 'mån'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 3 THEN Guru.GlobalaVariabler:regdagnamn = 'tis'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 4 THEN Guru.GlobalaVariabler:regdagnamn = 'ons'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 5 THEN Guru.GlobalaVariabler:regdagnamn = 'tor'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 6 THEN Guru.GlobalaVariabler:regdagnamn = 'fre'.
ELSE IF WEEKDAY(Guru.GlobalaVariabler:regdatum) = 7 THEN Guru.GlobalaVariabler:regdagnamn = 'lör'.
