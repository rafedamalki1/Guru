
/*------------------------------------------------------------------------
    File        : RESTUSERS.p
    Purpose     : 

    Syntax      : ACCES UTANFÖR GURU gpl darwinplus för användre som finns i _user

    Description : 

    Author(s)   : 
    Created     : Fri Apr 29 10:25:56 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE felmeddtemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL     AS INTEGER.  
DEFINE VARIABLE usrtBuffer AS HANDLE    NO-UNDO.
DEFINE VARIABLE usrfBuffer AS HANDLE    NO-UNDO.
DEFINE VARIABLE anvBuffer AS HANDLE    NO-UNDO.
DEFINE VARIABLE secBuffer AS HANDLE    NO-UNDO.      
DEFINE VARIABLE guruAnv   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gurupassword  AS CHARACTER NO-UNDO.
DEFINE VARIABLE poolname AS CHARACTER NO-UNDO.
poolname = "RestDynTable" + STRING(TIME).
CREATE WIDGET-POOL poolname NO-ERROR.
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL poolname.
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
/*KOLLAR OM ANVÄNDARE FINNS I GURU FÖR ACCES UTANFÖR GURU gpl darwinplus*/
PROCEDURE restUserStart_UI :
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER okuser AS LOGICAL NO-UNDO.
   
   CREATE BUFFER usrtBuffer FOR TABLE "_User" IN WIDGET-POOL poolname. 
   CREATE BUFFER anvBuffer FOR TABLE "ANVANDARE" IN WIDGET-POOL poolname. 
   CREATE BUFFER usrfBuffer FOR TABLE "_User" IN WIDGET-POOL poolname. 
   CREATE BUFFER secBuffer FOR TABLE "_sec-granted-role" IN WIDGET-POOL poolname.
   IF VALID-HANDLE(anvBuffer) THEN  DO:
      anvBuffer:FIND-FIRST("WHERE PERSONALKOD = '" + pkod + "'", NO-LOCK) NO-ERROR.
      IF anvBuffer:AVAILABLE THEN  DO:
         guruAnv = anvBuffer:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
         gurupassword = anvBuffer:BUFFER-FIELD("AV-LOSEN"):BUFFER-VALUE.
         anvBuffer:BUFFER-RELEASE().
         okuser = TRUE.
      END.   
      ELSE DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "Ingen användare upplagd i Guru för Enhet/Sign".
         RUN RestUserEnd_UI.
         okuser = FALSE.
         RETURN.
      END. 
   END.
END PROCEDURE. 
PROCEDURE avslutarestUsr_UI:
   RUN RestUserEnd_UI.
   DELETE WIDGET-POOL poolname NO-ERROR.
END PROCEDURE.
PROCEDURE RestUserEnd_UI :
   secBuffer:BUFFER-RELEASE() NO-ERROR.
   usrtBuffer:BUFFER-RELEASE() NO-ERROR.
   usrfBuffer:BUFFER-RELEASE() NO-ERROR.
   
END PROCEDURE.
PROCEDURE restSkapaUser_UI:
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   IF pkod = "" OR pkod = ? THEN RETURN.
   DEFINE VARIABLE userNr    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE qString   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE okuser AS LOGICAL NO-UNDO.
   RUN restUserStart_UI (INPUT pkod, OUTPUT okuser).
   IF okuser = FALSE THEN RETURN.
   qString = "WHERE _Userid = '" + guruAnv + "'".
   usrtBuffer:FIND-FIRST(qString, NO-LOCK) NO-ERROR.
   IF(usrtBuffer:AVAILABLE) THEN DO:
      CREATE felmeddtemp.
      ASSIGN 
      felmeddtemp.FELMEDD = "GPL-Användare " + guruAnv + " fanns redan.".  
      RETURN.
   END.
   
   IF VALID-HANDLE(usrtBuffer) THEN  DO:
      qString = "FOR EACH " + usrtBuffer:TABLE + " WHERE _User._Domain-Name = 'RESTDomain' NO-LOCK BY _User_number DESCENDING ".
      RUN CreateCustomQuery(INPUT usrtBuffer,INPUT qString,OUTPUT hQuery).                                        
      hQuery:GET-FIRST(NO-LOCK).
      IF(usrtBuffer:AVAILABLE) THEN userNr = usrtBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE + 1.
      ELSE userNr = 1.
      DELETE OBJECT hQuery.
   END.
   
   DO TRANSACTION:
      CREATE BUFFER usrtBuffer FOR TABLE "_User" IN WIDGET-POOL poolname. 
      usrtBuffer:BUFFER-CREATE ().
      usrtBuffer:BUFFER-FIELD("_Userid"):BUFFER-VALUE = guruAnv.
      
      IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = gurupassword.
      ELSE usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = ENCODE(gurupassword).
      usrtBuffer:BUFFER-FIELD("_User-Name"):BUFFER-VALUE = pkod.
      usrtBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE = userNr.
      usrtBuffer:BUFFER-FIELD("_Domain-Name"):BUFFER-VALUE = "RESTDomain".
      usrtBuffer:BUFFER-FIELD("_Disabled"):BUFFER-VALUE = NO.
      secBuffer:BUFFER-CREATE ().
      secBuffer:BUFFER-FIELD("_grantee"):BUFFER-VALUE = guruAnv.
      secBuffer:BUFFER-FIELD("_role-name"):BUFFER-VALUE = "PSCUser".
      secBuffer:BUFFER-FIELD("_grantor"):BUFFER-VALUE = "".
      secBuffer:BUFFER-FIELD("_grant-rights"):BUFFER-VALUE = YES.
      secBuffer:BUFFER-FIELD("_granted-role-guid"):BUFFER-VALUE = SUBSTRING(BASE64-ENCODE(GENERATE-UUID), 1, 22).
      CREATE felmeddtemp.
      ASSIGN 
      felmeddtemp.FELMEDD = "GPL-Användare för " + guruAnv + " är skapad.".
   END.
   RUN RestUserEnd_UI.
   
END PROCEDURE.

PROCEDURE restTabortUser_UI:
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   IF pkod = "" OR pkod = ? THEN RETURN.
   DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE qString   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE okuser AS LOGICAL NO-UNDO.
   RUN restUserStart_UI (INPUT pkod, OUTPUT okuser).
   IF okuser = FALSE THEN RETURN.
   
   IF VALID-HANDLE(usrtBuffer) THEN DO TRANSACTION: 
      qString = "WHERE _Domain-Name = 'RESTDomain' AND _Userid = '" + guruAnv + "'".
      usrtBuffer:FIND-FIRST(qString,EXCLUSIVE-LOCK) NO-ERROR.
      IF usrtBuffer:AVAILABLE THEN DO:
         usrtBuffer:BUFFER-DELETE().
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "GPL-Användare för " + guruAnv + " togs bort.". 
      END.
      ELSE DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "GPL-Användaren som du försökte ta bort fanns inte.".
         RUN RestUserEnd_UI.
         RETURN.
      END.
      
   END.
   DO TRANSACTION:
      qString = "FOR EACH " + secBuffer:TABLE + " WHERE _grantee = '" + STRING(guruAnv) + "' NO-LOCK".
      RUN CreateCustomQuery(INPUT secBuffer,INPUT qString,OUTPUT hQuery). 
      hQuery:GET-FIRST(EXCLUSIVE-LOCK).
      DO WHILE hQuery:QUERY-OFF-END = FALSE:
         secBuffer:BUFFER-DELETE().
         hQuery:GET-NEXT(EXCLUSIVE-LOCK).
      END.
      DELETE OBJECT hQuery.
   END.
   RUN RestUserEnd_UI.
   
END PROCEDURE.

PROCEDURE restBytLosenord_UI:
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER newpassword AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   IF pkod = "" OR pkod = ? THEN RETURN.
   DEFINE VARIABLE qString    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE okuser AS LOGICAL NO-UNDO.
   RUN restUserStart_UI (INPUT pkod, OUTPUT okuser).
   IF okuser = FALSE THEN RETURN.
   
   IF VALID-HANDLE(usrfBuffer) THEN DO TRANSACTION: 
      qString = "WHERE _Userid = '" + guruAnv + "'".
      usrfBuffer:FIND-FIRST(qString,EXCLUSIVE-LOCK) NO-ERROR.
      IF usrfBuffer:AVAILABLE THEN DO:
         usrtBuffer:BUFFER-CREATE().
         usrtBuffer:BUFFER-FIELD("_User-Name"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_User-Name"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_Domain-Name"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_Domain-Name"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_Disabled"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_Disabled"):BUFFER-VALUE.
         IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = newpassword.
         ELSE usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = ENCODE(newpassword).
         usrfBuffer:BUFFER-DELETE().
         usrtBuffer:BUFFER-FIELD("_Userid"):BUFFER-VALUE = guruAnv.
      END.
      ELSE DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "GPL-Användaren som du ville ändra på fanns inte".
      END.
   END. 
   RUN RestUserEnd_UI.
END PROCEDURE.

PROCEDURE restBytUser_UI:
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER gamanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   IF pkod = "" OR pkod = ? THEN RETURN.
   DEFINE VARIABLE qString    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE okuser AS LOGICAL NO-UNDO.
   RUN restUserStart_UI (INPUT pkod, OUTPUT okuser).
   IF okuser = FALSE THEN RETURN.
   
   IF VALID-HANDLE(usrfBuffer) THEN  DO TRANSACTION: 
      qString = "WHERE _Userid = '" + gamanv + "'".
      usrfBuffer:FIND-FIRST(qString,EXCLUSIVE-LOCK) NO-ERROR.
      IF usrfBuffer:AVAILABLE THEN DO:
         usrtBuffer:BUFFER-CREATE ().
         usrtBuffer:BUFFER-FIELD("_Userid"):BUFFER-VALUE = anvBuffer:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE. 
         usrtBuffer:BUFFER-FIELD("_User-Name"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_User-Name"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_User_number"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_Domain-Name"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_Domain-Name"):BUFFER-VALUE.
         usrtBuffer:BUFFER-FIELD("_Disabled"):BUFFER-VALUE = usrfBuffer:BUFFER-FIELD("_Disabled"):BUFFER-VALUE.
         
         IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = anvBuffer:BUFFER-FIELD("AV-LOSEN"):BUFFER-VALUE.
         ELSE usrtBuffer:BUFFER-FIELD("_Password"):BUFFER-VALUE = ENCODE(anvBuffer:BUFFER-FIELD("AV-LOSEN"):BUFFER-VALUE).
         usrfBuffer:BUFFER-DELETE().
         qString = "FOR EACH " + secBuffer:TABLE + " WHERE _grantee = '" + STRING(gamanv) + "' NO-LOCK".
         RUN CreateCustomQuery(INPUT secBuffer,INPUT qString,OUTPUT hQuery). 
         hQuery:GET-FIRST(EXCLUSIVE-LOCK).
         DO WHILE hQuery:QUERY-OFF-END = FALSE:
            secBuffer:BUFFER-FIELD("_grantee"):BUFFER-VALUE = usrtBuffer:BUFFER-FIELD("_Userid"):BUFFER-VALUE.
            hQuery:GET-NEXT(EXCLUSIVE-LOCK).
         END.
         DELETE OBJECT hQuery.
      END.
      ELSE  DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "GPL-Användaren som du ville ändra på fanns inte".
      END.
   END. 
   RUN RestUserEnd_UI.
END PROCEDURE.
/*FINNS ANVÄNDAREN I _USER*/
PROCEDURE fannsRESTUser_UI:
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER fanns AS LOGICAL.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   IF pkod = "" OR pkod = ? THEN RETURN.
   DEFINE VARIABLE qString   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE okuser AS LOGICAL NO-UNDO.
   RUN restUserStart_UI (INPUT pkod, OUTPUT okuser).
   IF okuser = FALSE THEN RETURN. 
   
   IF VALID-HANDLE(usrtBuffer) THEN DO: 
      qString = "WHERE _Userid = '" + guruAnv + "'".
      usrtBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
      IF usrtBuffer:AVAILABLE THEN DO:    
         fanns = TRUE.
      END.
      ELSE DO:
         fanns = FALSE.
      END.
   END. 
   RUN RestUserEnd_UI.
   
END PROCEDURE.




   
