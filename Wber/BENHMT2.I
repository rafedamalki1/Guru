/*BENHMT2.I START AV SOKNING AV BENAMNING */
   ASSIGN
   RAD_SOK = INPUT FRAME {&FRAME-NAME} RAD_SOK
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
