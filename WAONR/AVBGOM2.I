/*AVBGOM2.I*/
/* 
avb-mus = SESSION:SET-WAIT-STATE("GENERAL").
*/
/*BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.*/

whandltemp.WF[1]:HIDDEN = TRUE NO-ERROR.
 DEFAULT-WINDOW:HIDDEN = TRUE NO-ERROR.
 
whandltemp.WF[1]:ALWAYS-ON-TOP = FALSE NO-ERROR.         

IF Guru.GlobalaVariabler:retvalkoll = FALSE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
END.

