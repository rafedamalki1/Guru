/*AVBGOMD.I*/
/* 
avb-mus = SESSION:SET-WAIT-STATE("GENERAL").
*/

BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

IF Guru.GlobalaVariabler:retvalkoll = FALSE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN  RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
END.
  
