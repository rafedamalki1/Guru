/*  APPLICATIONLOAD.I*/
   /* curren or admin applikationen*/
   {HKEYADMPER.I}
   /*Anders Olsson Elpool i Umeå AB  27 apr 2017 09:58:56 
   Gör ingentig :) 
   */
   
   {HKEYCURRENTUSER.I}
   ASSIGN 
   companyname = "SOFTWARE\Elpool i Umeå AB\"
   appnamn = "GuruOnWeb11".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      appnamn = "GuruOnWeb10".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "EKGOnWeb11".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "GuruOnWeb116".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.            
         END.
      END.
   END.  
   IF ERROR-STATUS:NUM-MESSAGES = 0 THEN DO:
      UNLOAD companyname + appnamn + "\".  
      LOAD companyname BASE-KEY hkeyvar NO-ERROR.
   END.
   ELSE DO:
      MESSAGE "Något är fel på installationen"
      VIEW-AS ALERT-BOX.
      QUIT.
   END.  
        
