/*DIA_M_SLUT.I*/
/*
{center.i}

*/


RUN FRONTSLUT.P (INPUT TABLE widgettemp).
FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
DEFINE VARIABLE status-musdia AS LOGICAL NO-UNDO.
Guru.GlobalaVariabler:windowsizeh = FRAME {&FRAME-NAME}:HANDLE. 
Guru.Konstanter:BtnOkAvbFlytt(). 
status-musdia = SESSION:SET-WAIT-STATE("").
IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END.
  
