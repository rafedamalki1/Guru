   /*APPATFRAGA.I*/
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN Guru.Konstanter:appcon = Guru.Konstanter:appcon.
   ELSE DO:
      IF Guru.Konstanter:apphand NE ? THEN DO: 
         IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN DO:
            MESSAGE "Vill du köra app:at ?" VIEW-AS ALERT-BOX QUESTION BUTTONS 
            YES-NO UPDATE val1 AS LOGICAL.
            CASE val1:
               WHEN TRUE THEN DO:
                  Guru.Konstanter:appcon = TRUE.
               END.
               WHEN FALSE THEN DO:
                  Guru.Konstanter:appcon = FALSE.
               END.
            END.
         END.   
      END.      
   END.
