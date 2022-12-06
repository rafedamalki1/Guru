/*SLUTWIN.I*/
      /*
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.   
   */
   RUN SetDefaultCursor.
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
