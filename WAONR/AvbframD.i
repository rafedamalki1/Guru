/*AVBFRAMD.I*/

                                                                               
BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END.
/*
avb-mus = SESSION:SET-WAIT-STATE("").
*/
