   /*ENRHMT2.I*/
   ASSIGN
   RAD_SOK = INPUT FRAME {&FRAME-NAME} RAD_SOK
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-ENR,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-ENR.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
   
