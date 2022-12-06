


DEFINE VARIABLE appcon  AS LOGICAL NO-UNDO.
 DEFINE VARIABLE apphand AS handle  NO-UNDO.
 DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.

   DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
 
 

 
conappvar = "-AppService APPWWW2 -H pas.guruonweb.se -S 2671 -ssl".
 
RUN appcon_UI.

conappvar = "-AppService appsll -H pas.guruonweb.se -S 2671 -ssl".
 
RUN appcon_UI.


conappvar = "-URL https://pas.guruonweb.se:8445/apsv".
 
 RUN appcon_UI.
 
 conappvar = "-URL https://pas.guruonweb.se:8443/apsv".
 
 RUN appcon_UI.

 PROCEDURE appcon_UI:
 CREATE SERVER apphand.
   appcon = apphand:CONNECT(conappvar,"InUser","InPasswd","KRIN") NO-ERROR.
   
   
   
         MESSAGE conappvar 
         ERROR-STATUS:NUM-MESSAGES 
        
         VIEW-AS ALERT-BOX QUESTION 
         UPDATE view-errs AS LOGICAL .       
         IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
            ERROR-STATUS:GET-MESSAGE(ivar)
            VIEW-AS ALERT-BOX.
         END.     
     
END PROCEDURE.
