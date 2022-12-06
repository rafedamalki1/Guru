/*
     Filename: XSOCKCLI.P
      Created: 03.05.0030 11:02ELPAO     
     Modified: 
*/

DEFINE VARIABLE hSocket AS HANDLE.
DEFINE VARIABLE aOK AS LOGICAL.
DEFINE VARIABLE mBuffer AS MEMPTR.
DEFINE VARIABLE cString AS CHAR.

MESSAGE "We are in the socket client".
CREATE SOCKET hSocket.

hSocket:CONNECT("-H localhost -S 3333").
IF hSocket:CONNECTED() THEN MESSAGE "Connected OK".
ELSE DO:
   MESSAGE "Could not connect".
   RETURN.
END.

MESSAGE "Start event handling".
hSocket:SET-READ-RESPONSE-PROCEDURE("readProc").

WAIT-FOR READ-RESPONSE OF hSocket.

MESSAGE "Freeing up resources".
hSocket:DISCONNECT().
DELETE OBJECT hSocket.
MESSAGE "Finished".

PROCEDURE readProc.
   DEFINE VARIABLE mBuffer AS MEMPTR.
   
   MESSAGE "We are in READ-RESPONSE event procedure, readProc".
   SET-SIZE(mBuffer) = 64.
   SELF:READ(mBuffer,1,SELF:GET-BYTES-AVAILABLE()).
   MESSAGE SELF:BYTES-READ "bytes read".
   cString = GET-STRING(mBuffer,1).
   DISPLAY cString FORMAT "x(64)".
END PROCEDURE.
