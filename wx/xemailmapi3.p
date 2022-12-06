PROCEDURE MapiReturnCode  :
 
DEFINE INPUT PARAMETER ResultInt AS INTEGER. /* result from MAPISendMail */
DEFINE VARIABLE RESULT AS CHARACTER NO-UNDO.
 
IF ResultInt <> 0 THEN DO:  /* 0 = Success */ 
   CASE ResultInt:
     WHEN  1 THEN RESULT = "User Abort".
     WHEN  2 THEN RESULT = "Failure".
     WHEN  3 THEN RESULT = "Login Failure".
     WHEN  4 THEN RESULT = "Disk Full".
     WHEN  5 THEN RESULT = "Insufficient Memory".
     WHEN  6 THEN RESULT = "Blk Too Small".
     WHEN  8 THEN RESULT = "Too Many Sessions".
     WHEN  9 THEN RESULT = "Too Many Files".
     WHEN 10 THEN RESULT = "Too Many Recipients".
     WHEN 11 THEN RESULT = "Attachment Not Found".
     WHEN 12 THEN RESULT = "Attachment Open Failure".
     WHEN 13 THEN RESULT = "Attachment Write Failure".
     WHEN 14 THEN RESULT = "Unknown Recipient".
     WHEN 15 THEN RESULT = "Bad Recipient type".
     WHEN 16 THEN RESULT = "No Messages".
     WHEN 17 THEN RESULT = "Invalid Message".
     WHEN 18 THEN RESULT = "Bodytext Too Large".
     WHEN 19 THEN RESULT = "Invalid Session".
     WHEN 20 THEN RESULT = "Type Not Supported".
     WHEN 21 THEN RESULT = "Ambiguous Recipient".
     WHEN 22 THEN RESULT = "Message in use".
     WHEN 23 THEN RESULT = "Network failure".
     WHEN 24 THEN RESULT = "Invalid edit fields".
     WHEN 25 THEN RESULT = "Invalid recipients".
     WHEN 26 THEN RESULT = "Feature not supported"
     OTHERWISE RESULT    = "Unknown error".
   END CASE.
 
   DO ON ENDKEY UNDO, LEAVE:
      MESSAGE ResultInt RESULT
              VIEW-AS ALERT-BOX.
   END.
END.
 
