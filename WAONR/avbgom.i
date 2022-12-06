/*AVBGOM.I*/
/* 
avb-mus = SESSION:SET-WAIT-STATE("GENERAL").
*/
/*BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.*/

{&WINDOW-NAME}:HIDDEN = TRUE.
 DEFAULT-WINDOW:HIDDEN = TRUE.
 
{&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.         

IF Guru.GlobalaVariabler:retvalkoll = FALSE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
END.

