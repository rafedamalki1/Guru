/*AVBFRAM2.I*/
whandltemp.WF[1]:HIDDEN = FALSE NO-ERROR.

whandltemp.WF[1]:MOVE-TO-TOP () NO-ERROR.

/*{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.*/
/*BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.*/

IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END.


/*
avb-mus = SESSION:SET-WAIT-STATE("").
*/
