/*VALDBVAL2.I*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   IF NOT AVAILABLE valdbtemp THEN musz = musz.
   ELSE DO:
      IF namndb() NE valdbtemp.DBNAMN THEN DO:
         IF namndb() NE ? THEN DISCONNECT VALUE(namndb()) NO-ERROR.         
      END.
      IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO :
         RUN con_UI.            
      END.
      varmess = "".
      IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
         DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            IF ERROR-STATUS:GET-NUMBER(i) = 6126 THEN DO:
               ASSIGN
               varerror = i
               varmess = "6126".         
            END.
         END.
         IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 4069 THEN musz = musz.
         ELSE IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 565 THEN musz = musz.
         ELSE RUN medd_UI.
      END.
      IF CONNECTED(valdbtemp.DBNAMN) THEN DO:
         {VERALIAS.I}
          musz = FALSE.
      END.
      ELSE musz = TRUE.      
   END.
   
      
      
