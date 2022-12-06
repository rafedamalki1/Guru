/*BENHMTINKBE.I START AV SOKNING AV BENAMNING */
  IF FILL-IN-BENBYTBE:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BENBYTBE.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BENBYTBE:SCREEN-VALUE,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOKBE:SCREEN-VALUE = "1" THEN DO:
      ASSIGN
      aosok = FILL-IN-BENBYTBE:SCREEN-VALUE.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOKBE:SCREEN-VALUE = "2" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BENBYTBE:SCREEN-VALUE + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOKBE:SCREEN-VALUE = "3" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BENBYTBE:SCREEN-VALUE.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
