/*
     Filename: XSOCKSERV.P
      Created: 03.05.0030 10:51ELPAO     
     Modified: 
*/

DEFINE VARIABLE hServerSocket AS HANDLE.
DEFINE VARIABLE aOK AS LOGICAL.
DEFINE VARIABLE mBuffer AS MEMPTR.
DEFINE VARIABLE greeting AS CHAR.
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
      LABEL "Avsluta"
      SIZE 10 BY 1.5.

DEFINE FRAME DEFAULT-FRAME
      BTN_AVS AT ROW 18 COLUMN 38
      WITH 1 DOWN NO-BOX THREE-D
      AT COLUMN 1 ROW 1
      SIZE 50 BY 20.

MESSAGE "We are in the socket server".
CREATE SERVER-SOCKET hServerSocket.

SET-SIZE(mBuffer) = 64.
greeting = "SERVER - Hello!".
PUT-STRING(mBuffer,1) = greeting.

MESSAGE "Start event handling".
hServerSocket:SET-CONNECT-PROCEDURE("connProc").
aOK = hServerSocket:ENABLE-CONNECTIONS( "-S 3333").
MESSAGE "Enable connections:" aOK.
ENABLE BTN_AVS WITH FRAME DEFAULT-FRAME.
DISPLAY BTN_AVS WITH FRAME DEFAULT-FRAME.

IF NOT aOK THEN RETURN.
WAIT-FOR CHOOSE OF BTN_AVS.

MESSAGE "Freeing up resources".
hServerSocket:DISABLE-CONNECTIONS().
DELETE OBJECT hServerSocket.
MESSAGE "Finished".

PROCEDURE connProc.
   DEFINE INPUT PARAMETER hSocket AS HANDLE.

   MESSAGE "We are in CONNECT event procedure, connProc".
   hSocket:WRITE(mBuffer,1,LENGTH(greeting)).
   MESSAGE hSocket:BYTES-WRITTEN "bytes written".
END.
