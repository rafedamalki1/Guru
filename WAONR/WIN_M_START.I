/*WIN_M_START.I*/
/*{windows.i}*/
SESSION:DATA-ENTRY-RETURN = FALSE.
IF Guru.GlobalaVariabler:retvalkoll = FALSE THEN DO:
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
END.
IF Guru.GlobalaVariabler:LastTimeDepa NE 0 THEN Guru.GlobalaVariabler:LastTimeDepa = TIME.
{FRMFONTDEF.I}
 
