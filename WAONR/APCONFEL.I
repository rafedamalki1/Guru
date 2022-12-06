/*APCONFEL.I*/
DEFINE VARIABLE i AS INTEGER NO-UNDO.
MESSAGE 
"Du kan inte starta denna funktion! Kontakta system ansvarig."  SKIP
ERROR-STATUS:NUM-MESSAGES 
" fel uppkom vid anslutningen." SKIP 
"Vill du se dem ?" 
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
UPDATE view-errs AS LOGICAL .       
IF view-errs THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
   MESSAGE ERROR-STATUS:GET-NUMBER(i)
   ERROR-STATUS:GET-MESSAGE(i)
   VIEW-AS ALERT-BOX.
END.
