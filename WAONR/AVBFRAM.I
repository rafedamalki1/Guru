/*AVBFRAM.I*/

{&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
/*{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.*/
/*BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.*/
{&WINDOW-NAME}:ALWAYS-ON-TOP = TRUE.

IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
END.
Guru.GlobalaVariabler:retvalkoll = {&WINDOW-NAME}:MOVE-TO-TOP().
Guru.GlobalaVariabler:retvalkoll = FALSE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.

APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
  

/*
avb-mus = SESSION:SET-WAIT-STATE("").
*/
