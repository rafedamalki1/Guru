/*WCONAPP.I*/
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE utbivar AS CHARACTER NO-UNDO.
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.


ASSIGN
gforetag = valdbtemp.GFORETAG
conappvar = valdbtemp.APPCON.

CREATE SERVER Guru.Konstanter:apphand.

IF conappvar = "" THEN DO:
   MESSAGE "Kontakta Elpool tel 090/184540 för du kan inte ansluta korrekt!"
   VIEW-AS ALERT-BOX.
END. 
/*obs case-sensitv -AppService appguru9*/
ELSE DO:
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
END.
IF NOT Guru.Konstanter:appcon THEN DO:
   MESSAGE 
   ERROR-STATUS:NUM-MESSAGES 
   " fel uppkom vid anslutningen." SKIP 
   "Det går ej att ansluta appserver och databasen i Guru." SKIP
   "Kontakta system ansvarig." SKIP
   "Kontakta Elpool tel 090/184540." SKIP
   SKIP
   "Vill du se felmeddelandena ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
   UPDATE view-errs AS LOGICAL .       
   IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
      ERROR-STATUS:GET-MESSAGE(ivar)
      VIEW-AS ALERT-BOX.
   END. 
   DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   Guru.Konstanter:apphand = ?.    
   RETURN NO-APPLY.
END.
ELSE DO:
   Guru.Konstanter:appcon = Guru.Konstanter:appcon.
   Guru.Konstanter:apphand = Guru.Konstanter:apphand.
   RUN FORVER.P ON Guru.Konstanter:apphand (INPUT gforetag,INPUT conappvar).
END.   
 
