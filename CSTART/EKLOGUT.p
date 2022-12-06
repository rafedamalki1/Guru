/*EKLOGUT.P*/
DEFINE INPUT  PARAMETER ligga AS LOGICAL NO-UNDO.
&Scoped-define NEW   
{GLOBVAR2DEL1.I}


DEFINE VARIABLE raderpersida AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE radpos AS INTEGER NO-UNDO.
 
{AONRUTSID.I}
{TIDUTTTSHARED.I}
DEFINE FRAME FRAME-TID3
     tidut.UT AT ROW 1 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 141 BY 1
    WITH DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 7
         SCROLLABLE SIZE 142 BY 16.59.  
IF ligga = FALSE THEN DO:
   raderpersida = Guru.GlobalaVariabler:globsids.
   radpos       = 80.
   {UTSTAPDF.I}
END.
ELSE DO:
   raderpersida = Guru.GlobalaVariabler:globsidl.
   radpos       = 120.
   {UTLIGGPDF.I}
END.   
         
          
IF ligga = FALSE THEN DO: 
   {PRINTSTAENDE.I}
END.   
ELSE DO:
   {PRINTLIGGANDE.I}
END.   
PUT SKIP (2).
PUT PAGE-NUMBER AT radpos SKIP.              /*RAD 3*/
raknare = 0.
FOR EACH tidut:  
   raknare = raknare + 1.
   IF SUBSTRING(tidut.UT,1,6) = "BILAGA" THEN DO:
      RUN sidb_UI.      
   END.
   ELSE IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
      RUN sidb_UI.     
   END.
   ELSE DO:
      PUT tidut.UT AT 6 SKIP .      
      IF LINE-COUNTER > PAGE-SIZE THEN DO:
         PAGE.
         PUT SKIP (2).
         IF aoutvar = "" THEN DO:
            PUT PAGE-NUMBER AT radpos  SKIP. 
         END.
         ELSE DO:
            PUT aoutvar AT 6.
            PUT PAGE-NUMBER AT radpos  SKIP. 
         END.
         raknare = 0.
      END.      
   END.                     
END.  

OUTPUT CLOSE.
         
PROCEDURE sidb_UI :
   IF (raknare - 1) / (raderpersida - 3) - INTEGER((raknare - 1) / (raderpersida - 3)) = 0 THEN.            
   ELSE DO:  
      PAGE.
      PUT SKIP (2).
      IF aoutvar = "" THEN DO:
         PUT PAGE-NUMBER AT radpos  SKIP. 
      END.
      ELSE DO:
         PUT aoutvar AT 6.
         PUT PAGE-NUMBER AT radpos  SKIP. 
      END.
      raknare = 0.         
   END.         
   PUT tidut.UT AT 6 SKIP . 
END PROCEDURE.