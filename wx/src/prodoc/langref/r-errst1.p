CONNECT "db-xyz" NO-ERROR.
RUN chk-connect NO-ERROR.
IF ERROR-STATUS:ERROR
THEN MESSAGE "Run statement failed.".

PROCEDURE chk-connect.
DEFINE VARIABLE connect-ok AS LOGICAL INITIAL TRUE NO-UNDO.

   IF ERROR-STATUS:ERROR
   THEN DO:
       MESSAGE "Connect failed.".
       connect-ok = FALSE NO-ERROR.
       IF ERROR-STATUS:ERROR
       THEN MESSAGE "Assignment failed.".
   END.
   
   IF connect-ok
   THEN RETURN "OK".
   ELSE RETURN "FAILED".

END PROCEDURE.
