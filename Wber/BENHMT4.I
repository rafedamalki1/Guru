/*BENHMT4.I START AV SOKNING AV BENAMNING */
   
   IF FILL-IN-BEN:SCREEN-VALUE = "" THEN DO:
      MESSAGE "S?kbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN:SCREEN-VALUE,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni beh?ver inte anv?nda tecknet * vid s?kningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK:SCREEN-VALUE = "1" THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN:SCREEN-VALUE.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "2" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN:SCREEN-VALUE + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "3" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN:SCREEN-VALUE.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
