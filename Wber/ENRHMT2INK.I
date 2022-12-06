   /*ENRHMT2INK.I*/
   IF FILL-IN-ENRBYT:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENRBYT.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-ENRBYT:SCREEN-VALUE,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK:SCREEN-VALUE = "1" THEN DO:
      ASSIGN
      posok = FILL-IN-ENRBYT:SCREEN-VALUE.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "2" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENRBYT:SCREEN-VALUE + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK:SCREEN-VALUE = "3" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENRBYT:SCREEN-VALUE.
      begvar = FALSE.
   END.
   {MTRLHANDLE.I}
   
