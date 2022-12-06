/*FLERMED.I*/
DEFINE VARIABLE apphandgran AS HANDLE NO-UNDO.
DEFINE VARIABLE appcongran AS LOGICAL NO-UNDO.
DEFINE VARIABLE appforetag AS CHARACTER NO-UNDO.
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = Guru.Konstanter:globforetag NO-ERROR. 
IF NOT AVAILABLE valdbtemp THEN RETURN.
IF valdbtemp.FORETAG = "GRAN" THEN musz = musz.
ELSE IF valdbtemp.FORETAG = "VESAB" THEN musz = musz.
ELSE RETURN.
IF vart NE "FMED" THEN DO:
   RETURN.
END.   
appforetag = valdbtemp.FORETAG.
IF Guru.Konstanter:globforetag = "GSOL" THEN DO:   
   RETURN.
END.   
ELSE RETURN.


MESSAGE "Vill du hämta meddelande från andra databaser ?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
CASE val1:
   WHEN TRUE THEN DO:
      musz = FALSE.          
   END.
   WHEN FALSE THEN DO:
      musz = FALSE.          
      RETURN.
   END.
END CASE. 
{muswait.i}
FOR EACH valdbtemp WHERE valdbtemp.FORETAG = appforetag:
   IF {TAEJMEDDB.I} THEN musz = musz.
   ELSE IF valdbtemp.DBNAMN = "VUTBI" THEN musz = musz.
   ELSE DO:   
      IF Guru.Konstanter:globforetag NE valdbtemp.GFORETAG THEN DO:
         RUN flermed2_UI (INPUT valdbtemp.GFORETAG,INPUT 1).      
      END.
   END.
END.

/*
IF Guru.Konstanter:globforetag NE "GSOL" THEN DO:
   RUN flermed2_UI (INPUT "GSOL").   
END.
*/

            
