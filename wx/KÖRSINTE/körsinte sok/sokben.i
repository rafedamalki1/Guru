/*SOKBEN.I START AV SOKNING AV BENAMNING */

   ASSIGN
   sok = FALSE  
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar = 0 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN + "*"
      begvar = FALSE.
   END.
   ELSE DO:
      IF SUBSTRING(FILL-IN-BEN,1,1) = "*" THEN DO:
         stjarnvar = INDEX(FILL-IN-BEN,"*",2).
         IF stjarnvar = 0 THEN DO:
            ASSIGN
            aosok = FILL-IN-BEN
            begvar = FALSE.
         END.
         ELSE DO:
            MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.  
      ELSE DO:
         soklangd = LENGTH(FILL-IN-BEN).
         IF SUBSTRING(FILL-IN-BEN,soklangd,1) = "*" THEN DO:
            stjarnvar = INDEX(FILL-IN-BEN,"*",1).
            IF stjarnvar = soklangd THEN DO:
               ASSIGN
               aosok = SUBSTRING(FILL-IN-BEN,1,soklangd - 1)
               begvar = TRUE.
            END.
            ELSE DO:
               MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
         END.
         ELSE DO:
            ASSIGN
            aosok = "*" + FILL-IN-BEN + "*"
            begvar = FALSE.
         END.
      END.    
   END.          
