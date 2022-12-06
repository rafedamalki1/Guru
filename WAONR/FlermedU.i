/*FLERMEDU.I*/
MESSAGE "Vill du skicka meddelande till andra databaser ?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
CASE val1:
   WHEN TRUE THEN DO:
      musz = FALSE.          
   END.
   WHEN FALSE THEN DO:      
      RETURN.
   END.
END CASE. 

IF (Guru.Konstanter:globforetag = "GRAN"  OR     
    Guru.Konstanter:globforetag = "GADM"  OR 
    Guru.Konstanter:globforetag = "GKAL"  ) THEN DO: 
   
   FOR EACH valdbtemp WHERE valdbtemp.FORETAG = "GRAN": 
      IF {TAEJMEDDB.I} THEN musz = musz.
      ELSE IF valdbtemp.DBNAMN = "VUTBI" THEN musz = musz.
      ELSE IF Guru.Konstanter:globforetag NE valdbtemp.GFORETAG THEN DO:
         MESSAGE "Vill du skicka meddelande till " valdbtemp.VALDB " ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val21 AS LOGICAL.
         CASE val21:
             WHEN TRUE THEN RUN flermed2_UI (INPUT valdbtemp.GFORETAG,INPUT 1).              
         END CASE.                   
      END.
   END.    
      
END.

IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VNAT" THEN DO: 
   FOR EACH valdbtemp WHERE valdbtemp.FORETAG = "VSAB": 
      IF {TAEJMEDDB.I} THEN musz = musz.
      ELSE IF valdbtemp.DBNAMN = "VUTBI" THEN musz = musz.
      ELSE IF valdbtemp.GFORETAG NE "utbi" THEN DO:
         IF Guru.Konstanter:globforetag NE valdbtemp.GFORETAG THEN DO:
            MESSAGE "Vill du skicka meddelande till " valdbtemp.VALDB " ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val27 AS LOGICAL.
            CASE val27:
                WHEN TRUE THEN RUN flermed2_UI (INPUT valdbtemp.GFORETAG,INPUT 1).              
            END CASE.          
         END.
      END.
   END.       
END.

            
