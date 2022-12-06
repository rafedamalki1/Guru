/*FAVRAAPP.P*/

&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flexdagtemp.

FOR EACH flexdagtemp:
   FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = flexdagtemp.PERSONALKOD AND 
   FLEXDAG.DATUM = flexdagtemp.DATUM USE-INDEX FLEX NO-LOCK NO-ERROR.        
   IF AVAILABLE FLEXDAG THEN DO:
      IF FLEXDAG.FELOK = TRUE THEN DO:      
        DELETE flexdagtemp.   
      END.
      ELSE DO:
         ASSIGN
         flexdagtemp.FELMED = FLEXDAG.FELMED.
      END.
   END.   
END.
