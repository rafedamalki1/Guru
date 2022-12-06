   /*STARTWIN2.I*/
   
   /*{windows.i} */
   
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
{FRMFONTDEF.I}
