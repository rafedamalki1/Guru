/*SMTPPOST.P  körs ej ?*/        /*mailgran*/
DEFINE INPUT PARAMETER vSmtpServer AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE INPUT PARAMETER vTo         AS CHARACTER LABEL "To" NO-UNDO.
DEFINE INPUT PARAMETER vSubject    AS CHARACTER LABEL "Subject" NO-UNDO.
DEFINE INPUT PARAMETER vBody       AS CHARACTER LABEL "Body" NO-UNDO.
DEFINE INPUT PARAMETER Filpath  AS CHARACTER.      /* Name of file to attach */
DEFINE INPUT PARAMETER Filnamn  AS CHARACTER.      /* Name only  */
DEFINE INPUT PARAMETER vFrom       AS CHARACTER LABEL "From" NO-UNDO.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE OUTPUT PARAMETER Ereslut  AS LOGICAL.        /* Email status  */
DEFINE OUTPUT PARAMETER Emailtxt AS CHARACTER.      /* Status txt  */
DEFINE VARIABLE vbuffer AS MEMPTR NO-UNDO.
DEFINE VARIABLE hSocket AS HANDLE NO-UNDO.
DEFINE VARIABLE vstatus AS LOGICAL NO-UNDO.
DEFINE VARIABLE vState  AS INTEGER NO-UNDO.

/*
DEFINE VARIABLE vdebugmode  AS LOGICAL
      VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE BUTTON bSend LABEL "Send".
 */                                      
   /*
    Status:
    0 - No Connection to the server
    1 - Waiting for 220 connection to SMTP server
    2 - Waiting for 250 OK status to start sending email
    3 - Waiting for 250 OK status for sender
    4 - Waiting for 250 OK status for recipient
    5 - Waiting for 354 OK status to send data
    6 - Waiting for 250 OK status for message received
    7 - Quiting
    */  
RUN sendmail_UI.
RETURN.
 /* Internal Procedures */
PROCEDURE cleanup_UI:
   hSocket:DISCONNECT() NO-ERROR.
   DELETE OBJECT hSocket NO-ERROR.
END.
PROCEDURE sendmail_UI:
   CREATE SOCKET hSocket.
   hSocket:SET-READ-RESPONSE-PROCEDURE ("readHandler_UI",
   THIS-PROCEDURE).
   vstatus = hSocket:CONNECT("-S 25 -H " + vSmtpServer)NO-ERROR.
   IF NOT vstatus THEN DO:
      Emailtxt = "Server is unavailable".
      Ereslut = FALSE.
      RETURN.
   END.
   vstate = 1.
   REPEAT:
      IF vstate < 0 OR vstate = 7 THEN LEAVE.
      WAIT-FOR READ-RESPONSE OF hSocket.
   END.
   Ereslut = TRUE.
   RUN cleanup_UI.
END.
PROCEDURE newState_UI:
   DEFINE INPUT PARAMETER newState AS INTEGER.
   DEFINE INPUT PARAMETER pstring AS CHARACTER.   
   vState = newState.
   IF pstring = "" THEN RETURN.
   SET-SIZE(vbuffer) = LENGTH(pstring) + 1.
   PUT-STRING(vbuffer,1) = pstring.
   hSocket:WRITE(vbuffer, 1, LENGTH(pstring)).
   SET-SIZE(vbuffer) = 0.   
END.
PROCEDURE readHandler_UI:
   DEFINE VARIABLE vlength AS INTEGER NO-UNDO.
   DEFINE VARIABLE str AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v AS INTEGER NO-UNDO.
   vlength = hSocket:GET-BYTES-AVAILABLE().
   IF vlength > 0 THEN DO:
      SET-SIZE(vbuffer) = vlength + 1.
      hSocket:READ(vbuffer, 1, vlength, 1).
      str = GET-STRING(vbuffer,1).      
      SET-SIZE(vbuffer) = 0.
      v = INTEGER(ENTRY(1, str," ")).
      CASE vState:
         WHEN 1 THEN IF v = 220 THEN RUN newState_UI (2, "HELO how are you? ~n").
         ELSE vState = -1.
         WHEN 2 THEN IF v = 250 THEN RUN newState_UI (3, "MAIL From: " + vFrom + "~n").
         ELSE vState = -1.
         WHEN 3 THEN IF v = 250 THEN RUN newState_UI (4, "RCPT TO: " + vTo + "~n").
         ELSE vState = -1.
         WHEN 4 THEN IF v = 250 THEN RUN newState_UI (5, "DATA ~n").
         ELSE vState = -1.
         WHEN 5 THEN IF v = 354 THEN RUN newState_UI(6, "From: " + vFrom + " ~n" + "To: " + vTo + " ~n" + "Subject: " + vSubject + " ~n~n" + vBody + "~n" + ".~n").
         ELSE vState = -1.
         WHEN 6 THEN IF v = 250 THEN RUN newState_UI (7,"QUIT~n").
         ELSE vState = -1.
      END CASE.   
   END.
   IF vState = 7 THEN Emailtxt =  "Email has been accepted for delivery.".
   IF vState < 0 THEN Emailtxt = "Email has been aborted".
END.
