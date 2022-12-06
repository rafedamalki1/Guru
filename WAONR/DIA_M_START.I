/*DIA_M_START.I*/
SESSION:DATA-ENTRY-RETURN = FALSE.
IF PROGRAM-NAME(1) = "SKRIVVAL.W" THEN. 
ELSE IF Guru.GlobalaVariabler:retvalkoll = FALSE THEN DO:  
   IF VALID-HANDLE(Guru.Konstanter:hpApi) THEN RUN SetAppstartingCursor IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = TRUE.
END.
{FRMFONTDEF.I}
IF Guru.GlobalaVariabler:LastTimeDepa NE 0 THEN Guru.GlobalaVariabler:LastTimeDepa = TIME.
ON F2 OF FRAME {&FRAME-NAME} ANYWHERE
DO:
   RETURN NO-APPLY.
END.
  
