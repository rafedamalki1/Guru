   /*ENRHMT5.I*/
   IF FILL-IN-KOD:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD:SCREEN-VALUE,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK:SCREEN-VALUE = "1" THEN DO:
      ASSIGN
      posok = FILL-IN-KOD:SCREEN-VALUE.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "2" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD:SCREEN-VALUE + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "3" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD:SCREEN-VALUE.
      begvar = FALSE.
   END.

   
