 /*BERTITLE.I*/
 
 IF Guru.GlobalaVariabler:plusaonr = ? THEN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " Beredning nr: " + STRING(valaonr) + " Benämning: " + valort.
 ELSE DO:
    
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " " + Guru.Konstanter:gaok + ": " + Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1]) + " Beredning nr: " + STRING(valaonr) + " Benämning: " + valort.
 END.
