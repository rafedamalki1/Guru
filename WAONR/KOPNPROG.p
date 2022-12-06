/*KOPNPROG.P*/
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
MESSAGE "Vill du kopieras svara JA!" Skip
        "Vill du inte kopera utan bara titta p� vilka program som skall kopieras svara NEJ!"  
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE kval AS LOGICAL.
         CASE kval:            
            WHEN TRUE THEN DO:
               IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO: 
                  filename = SEARCH("NYPROG.BAT").
                  IF filename NE ? THEN DO:
                     RUN kopiera_UI.
                  END.
               END.
            END.
            WHEN FALSE THEN DO:
               IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO: 
                  filename = SEARCH("NYTITT.BAT").
                  IF filename NE ? THEN DO:
                     DOS VALUE(filename).      
                     MESSAGE "Vill starta kopieingen svara JA!" Skip
                     "Vill du inte g�ra n�gon kopiering svara NEJ!"  
                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE kval.
                     IF kval = TRUE THEN RUN kopiera_UI.
                  END.
               END.
            END.
            OTHERWISE RETURN.                               
         END CASE.
IF filename = ? THEN DO:
   MESSAGE "Det saknas program i din maskin f�r att utf�ra denna operation. Kontakta ansvarig!" 
   VIEW-AS ALERT-BOX.         
END.
PROCEDURE kopiera_UI:
   DOS VALUE(filename). 
   MESSAGE "Nu �r allt klart!" VIEW-AS ALERT-BOX.                         
END PROCEDURE.
