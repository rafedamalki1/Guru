/*FLEXSTART.I
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: FLEXSTART.I
      Comment: ANVÄNDS INTE
   Parameters:
         Uses:
      Used by:
      Created: 2009.01.29 10:56 ELPAO   
     Modified: 2009.01.29 10:58 ELPAO    
     Modified: 2009.10.01 14:05 ELPAO    
     Modified: 
*/


isweb = TRUE.
RUN val_UI.
IF musz = FALSE THEN DO:
   IF isweb = TRUE THEN DO:
      Guru.Konstanter:appfel = TRUE.
      {WCONAPP.I}
      IF Guru.Konstanter:appcon = TRUE THEN DO:        
         nyaprog = TRUE.
        /*
         REPEAT:
            RUN WSTART.W (INPUT gforetag,INPUT "",INPUT-OUTPUT nyaprog).
            IF nyaprog = FALSE THEN LEAVE.
         END.
         */
         RUN WSTART.W (INPUT gforetag,INPUT "",INPUT-OUTPUT nyaprog).
      END.
      IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
      DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   END.
   ELSE DO:
      RUN FLEXAPP.P.    
   END.
   musz = FALSE.
END.
PROCEDURE medd_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   MESSAGE 
   "Anslutningen till " valdbtemp.VALDB " misslyckades!" SKIP
   ERROR-STATUS:NUM-MESSAGES 
   " fel uppkom vid anslutningen." SKIP 
   "Vill du se dem ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
   UPDATE view-errs AS LOGICAL.       
   IF view-errs THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(i)
      ERROR-STATUS:GET-MESSAGE(i)
      VIEW-AS ALERT-BOX.
   END.   
END PROCEDURE.
PROCEDURE con_UI :
   kommando = valdbtemp.DBCON. 
   CONNECT VALUE(kommando) NO-ERROR.                
END PROCEDURE.
PROCEDURE val_UI :
   {VALDBVAL.I}
   {musarrow.i}  
END PROCEDURE.
