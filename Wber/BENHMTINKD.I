/*BENHMTINKD.I START AV SOKNING AV BENAMNING */
  IF FILL-IN-BENDIREKT:SCREEN-VALUE = "" THEN DO:
      MESSAGE "S?kbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BENDIREKT.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BENDIREKT:SCREEN-VALUE,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni beh?ver inte anv?nda tecknet * vid s?kningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOKDIREKT:SCREEN-VALUE = "1" THEN DO:
      ASSIGN
      aosok = FILL-IN-BENDIREKT:SCREEN-VALUE.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOKDIREKT:SCREEN-VALUE = "2" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BENDIREKT:SCREEN-VALUE + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOKDIREKT:SCREEN-VALUE = "3" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BENDIREKT:SCREEN-VALUE.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
