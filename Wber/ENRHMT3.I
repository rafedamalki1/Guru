   /*ENRHMT3.I*/
   ASSIGN
   RAD_SOK = INPUT FRAME {&FRAME-NAME} RAD_SOK
   FILL-IN-KOD = INPUT  FILL-IN-KOD.
   IF FILL-IN-KOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-KOD.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD.
      begvar = FALSE.
   END.

   
