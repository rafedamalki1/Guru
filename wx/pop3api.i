
/*
 * Written by Scott Auge scott_auge@yahoo.com sauge@amduus.com
 * Copyright (c) 2001 Amduus Information Works, Inc.  www.amduus.com
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Amduus Information Works
 *      Inc. and its contributors.
 * 4. Neither the name of Amduus Information Works, Inc. nor the names of
 *    its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY AMDUUS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AMDUUS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


/*
 * Perform operations on POP3 mail servers
 *
 * Use this file as an include or a persistance procedure.
 */

DEF VAR RCSVersionpop3apii AS CHARACTER INIT "$Header: /appl/juror/src/RCS/pop3api.i,v 1.1 2003/01/01 00:08:36 sauge Exp $" NO-UNDO.

&GLOBAL-DEFINE NOERROR  0
&GLOBAL-DEFINE NOCONNECT 1
&GLOBAL-DEFINE TEMP "/tmp/"

DEF VAR mData             AS MEMPTR    NO-UNDO.
DEF VAR mData1            AS MEMPTR    NO-UNDO.

SET-SIZE (mData) = 12288.  /* 12K of space to hold a server reponse */
SET-SIZE (mData1) = 1.     /* Spins like heck, but works!           */

{memptr.i}
{MakeID3.i}

/****************************************************************/
/* Create the socket, connect to server.                        */
/****************************************************************/

PROCEDURE ConnectToServer:

  DEF INPUT PARAMETER MailServer AS CHARACTER NO-UNDO.
  DEF OUTPUT PARAMETER hHandle AS HANDLE NO-UNDO.
  DEF OUTPUT PARAMETER Result AS INTEGER NO-UNDO.

  DEF VAR hSocket AS HANDLE NO-UNDO.
  DEF VAR lConnectParameters AS CHARACTER NO-UNDO.

  CREATE SOCKET hSocket.
  hSocket:SET-SOCKET-OPTION ("TCP-NODELAY", "FALSE").
  hSocket:SET-SOCKET-OPTION ("SO-LINGER", "FALSE").

  ASSIGN lConnectParameters = '-H ' + MailServer + ' -S pop3'.

  hSocket:CONNECT(lConnectParameters) NO-ERROR.

  IF hSocket:CONNECTED() = FALSE THEN DO:
    ASSIGN Result = {&NOCONNECT}.
    RETURN.
  END.

  ASSIGN Result = {&NOERROR}
         hHandle = hSocket.

END. /* PROCEDURE ConnectToServer */

PROCEDURE DisconnectFromServer:

  DEF INPUT PARAMETER hSocket AS HANDLE NO-UNDO.

  hSocket:DISCONNECT().

END.  /* PROCEDURE DisconnectFromServer */

/****************************************************************/
/* Create the socket, connect to server.                        */
/****************************************************************/

PROCEDURE Login:

  DEF INPUT PARAMETER hSocket AS HANDLE NO-UNDO.
  DEF INPUT PARAMETER pUserID  AS CHARACTER NO-UNDO.
  DEF INPUT PARAMETER pPassword AS CHARACTER NO-UNDO.
  DEF OUTPUT PARAMETER pMessageText AS CHARACTER NO-UNDO.

  DEF VAR lPOP3Command AS CHARACTER NO-UNDO.

  ASSIGN lPOP3Command = "USER " + pUserID + "~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse (INPUT hSocket, OUTPUT pMessageText).

  ASSIGN lPOP3Command = "PASS " + pPassword + "~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse (INPUT hSocket, OUTPUT pMessageText).
  RUN CleanResponse (INPUT pMessageText, OUTPUT pMessageText).

END. /* PROCEDURE Login */

/****************************************************************/
/* List messages available on the server.                       */
/****************************************************************/

PROCEDURE ListMessages:

  DEF INPUT PARAMETER hSocket AS HANDLE NO-UNDO.
  DEF OUTPUT PARAMETER pMessageText AS CHARACTER NO-UNDO.

  DEF VAR lPOP3Command AS CHARACTER NO-UNDO.
  DEF VAR i AS INTEGER NO-UNDO.
  DEF VAR t AS CHARACTER NO-UNDO.

  ASSIGN lPOP3Command = "LIST~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse (INPUT hSocket, OUTPUT pMessageText).


  ASSIGN pMessageText = trim (pMessageText).

  /* Replace CR/LF with , for NUM-ENTRIES */

  ASSIGN pMessageText = REPLACE (pMessageText, CHR(13) + CHR(10), ",").

/*
  /*--- remove hard carriage returns with spaces ---*/

  DO WHILE (INDEX(pMessageText, CHR(13)) <> 0):
    ASSIGN SUBSTRING(pMessageText, INDEX(pMessageText, CHR(13))) = " ".
  END.
  
  /*--- remove hard carriage returns with spaces ---*/
  DO WHILE (INDEX(pMessageText, CHR(10)) <> 0):
    ASSIGN SUBSTRING(pMessageText, INDEX(pMessageText, CHR(10))) = ",".
  END.
*/

  /* Start at 2, because first entry will be a +OK */
  IF NUM-ENTRIES(pMessageText) > 2 THEN 
    DO i = 2 TO NUM-ENTRIES(pMessageText):
      ASSIGN t = t + ENTRY(1, ENTRY(i, pMessageText), " ") + ",".
    END.

  /* Remove off the final , as well the ., that comes from the end of the */
  /* LIST command.                                                        */

  ASSIGN pMessageText = SUBSTRING(t, 1, INDEX(t, ".") - 1).

  /* Grrrr, if the LAST entry is still a "" pull off another byte */

  IF NUM-ENTRIES(pMessageText) > 0 THEN 
    IF ENTRY(NUM-ENTRIES(pMessageText), pMessageText) = "" THEN
      ASSIGN pMessageText = SUBSTRING(pMessageText, 1, LENGTH(pMessageText) - 1).

END. /* PROCEDURE ListMessages */

/****************************************************************/
/* Retreive message from the server.                            */
/****************************************************************/

PROCEDURE RetrieveMessage:

  DEF INPUT PARAMETER hSocket as HANDLE NO-UNDO.
  DEF INPUT PARAMETER pMessageID AS CHARACTER NO-UNDO.
  DEF OUTPUT PARAMETER cFileName AS CHARACTER NO-UNDO.

  DEF VAR lPOP3Command AS CHARACTER NO-UNDO.

  ASSIGN lPOP3Command = "RETR " + pMessageID + "~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse1 (
    INPUT hSocket, 
    OUTPUT cFileName).

END. /* PROCEDURE RetrieveMessage */

/****************************************************************/
/* Delete a message from the server.                            */
/****************************************************************/

PROCEDURE DeleteMessage:

  DEF INPUT PARAMETER hSocket as HANDLE NO-UNDO.
  DEF INPUT PARAMETER pMessageID AS CHARACTER NO-UNDO.
  DEF OUTPUT PARAMETER lServerResponse AS CHARACTER NO-UNDO.

  DEF VAR lPOP3Command AS CHARACTER NO-UNDO.

  ASSIGN lPOP3Command = "DELE " + pMessageID + "~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse (INPUT hSocket, OUTPUT lServerResponse).
  RUN CleanResponse (INPUT lServerResponse, OUTPUT lServerResponse).

END. /* PROCEDURE DeleteMessage */

/****************************************************************/
/* Logout of the server.                                        */
/****************************************************************/

PROCEDURE Logout:

  DEF INPUT PARAMETER hSocket as HANDLE NO-UNDO.
  DEF OUTPUT PARAMETER lServerResponse AS CHARACTER NO-UNDO.

  DEF VAR lPOP3Command AS CHARACTER NO-UNDO.

  ASSIGN lPOP3Command = "QUIT~n".
  RUN lmemput (INPUT-OUTPUT mData, lPOP3Command).
  hSocket:WRITE(mData,1,LENGTH(lPOP3Command)) NO-ERROR.

  RUN ProcessServerResponse (INPUT hSocket, OUTPUT lServerResponse).
  RUN CleanResponse (INPUT lServerResponse, OUTPUT lServerResponse).

END.

/****************************************************************/
/* Get a response from the server.  This routine is big enough  */
/* for most reponses from the server without blowing the 32K    */
/* varible limit.                                               */
/****************************************************************/

PROCEDURE ProcessServerResponse:

  DEF INPUT PARAMETER hSocket AS HANDLE NO-UNDO.
  DEF OUTPUT PARAMETER lResponse AS CHARACTER NO-UNDO.

  DEF VAR i AS INTEGER INIT 0 NO-UNDO.

  /****************************************************************/
  /* Sometimes we may need to wait a little bit for the server to */
  /* respond so we loop a bit till we get something interesting.  */
  /* If we dont, we just bail out of here.                        */
  /****************************************************************/

  DO WHILE i < 6:
    IF hSocket:GET-BYTES-AVAILABLE() <> 0 THEN LEAVE.
    PAUSE 1 NO-MESSAGE.
    ASSIGN i = i + 1.
  END.

  IF hSocket:GET-BYTES-AVAILABLE() = 0 THEN RETURN.

  /****************************************************************/
  /* We discover we have something to read from the socket, lets  */
  /* see what we got!  Pop it into raw memory, then put it into   */
  /* a 4GL varible.                                               */
  /****************************************************************/

  RUN lmemset (INPUT-OUTPUT mData, " ").
  hSocket:READ (mData, 1, GET-SIZE(mData), READ-AVAILABLE).

  RUN lmemget (INPUT-OUTPUT mData, OUTPUT LResponse).

END. /* PROCEDURE ProcessServerResponse */

/****************************************************************/
/* This routine will put the server repsonse into a file on the */
/* disk system.  This is for messages from the server.  It is   */
/* possible some of these messages are to large for the 32K     */
/* limit on Progress varibles.  We send back a file name where  */
/* the text of the message can be located.                      */
/****************************************************************/

PROCEDURE ProcessServerResponse1:

  DEF INPUT PARAMETER hSocket AS HANDLE NO-UNDO.
  DEF OUTPUT PARAMETER cFileName AS CHARACTER NO-UNDO.

  DEF VAR lResponse AS CHARACTER NO-UNDO.
  DEF VAR i         AS INTEGER INIT 0 NO-UNDO.
  DEF VAR cEOF      AS CHARACTER NO-UNDO.
  DEF VAR EndOfFile AS LOGICAL NO-UNDO.
  DEF VAR cChkEOF   AS CHARACTER NO-UNDO.
  DEF VAR cDirectory AS CHARACTER NO-UNDO.

  /****************************************************************/
  /* Define the signal that the message has been totally received.*/
  /****************************************************************/

  ASSIGN cEOF =  CHR(13) + CHR(10)+ "." + CHR(13) + CHR(10).
  ASSIGN cDirectory = SESSION:TEMP-DIRECTORY.

  /****************************************************************/
  /* Sometimes we may need to wait a little bit for the server to */
  /* respond so we loop a bit till we get something interesting.  */
  /* If we dont, we just bail out of here.                        */
  /****************************************************************/

  DO WHILE i < 6:
    IF hSocket:GET-BYTES-AVAILABLE() <> 0 THEN LEAVE.
    PAUSE 1 NO-MESSAGE.
    ASSIGN i = i + 1.
  END.

  IF hSocket:GET-BYTES-AVAILABLE() = 0 THEN RETURN.

  /****************************************************************/
  /* Figure out a name to hold the response.  Open the file for   */
  /* output.                                                      */
  /****************************************************************/

  IF SUBSTRING(cDirectory,LENGTH(cDirectory)) <> "/" THEN 
    ASSIGN cDirectory = cDirectory + "/".

  ASSIGN cFileName = cDirectory + MakeID3(10).

  OUTPUT TO VALUE(cFileName).

  /****************************************************************/
  /* We discover we have something to read from the socket, lets  */
  /* see what we got!  Pop it into raw memory, then put it into   */
  /* a 4GL varible.                                               */
  /****************************************************************/

  ASSIGN EndOfFile = NO.
  DO WHILE NOT EndOfFile:

    RUN lmemset (INPUT-OUTPUT mData1, " ").
    hSocket:READ (mData1, 1, GET-SIZE(mData1), READ-AVAILABLE).

    ASSIGN lResponse = "".
    RUN lmemget (INPUT-OUTPUT mData1, OUTPUT LResponse).

    /* Keep the LAST four characters read available to see */
    /* if we are dealing with the EOF in the message.      */

    ASSIGN cChkEOF = cChkEOF + lResponse.

    IF LENGTH(cChkEOF) > 5 THEN 
      ASSIGN cChkEOF = substring(cChkEOF, length(cChkEOF) - 5, 6).

    IF INDEX (cChkEOF, cEOF) <> 0 THEN ASSIGN EndOfFile = YES.

    /* Clean up the response if we are at the end. */

    IF EndOfFile THEN ASSIGN lResponse = RIGHT-TRIM(lResponse). 
    PUT UNFORMATTED lResponse.

  END.

  OUTPUT CLOSE.

END. /* PROCEDURE ProcessServerResponse */


/****************************************************************/
/* Some responses from the server can be cleaned up.  The code  */
/* has a \n in it and that makes it harder to deal with.  This  */
/* code gives the programmer the option of cleaning up the re-  */
/* sponse from the server.                                      */
/****************************************************************/

PROCEDURE CleanResponse:

  DEF INPUT PARAMETER cOldResponse AS CHARACTER NO-UNDO.
  DEF OUTPUT PARAMETER cNewResponse AS CHARACTER NO-UNDO.

  ASSIGN cNewResponse = REPLACE(cOldResponse, CHR(13), "").
  ASSIGN cNewResponse = REPLACE(cOldResponse, CHR(10), "").
  ASSIGN cNewResponse = TRIM(cOldResponse).

END.
