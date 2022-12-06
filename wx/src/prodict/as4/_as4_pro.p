/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/* 
  10/12/93 J.Morin  Modified to run in V7 GUI.
  Modified 12/21/93 jmorin
           - Display error message at end if some def's not dumped. 
  Modified 94/03/07 hutegger  replaced "STOP" with KBLABEL("STOP")
                              in frame marker                 
                              
   Modified  95/06/01 dmcmann added shared frame so field names and index names
                                would display.  Bug 94-08-01-030
*/
/* __as4_pro.p - as/400 dump data definitions */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

/* LANGUAGE DEPENDENCIES START */ /*-------------------------------------*/
{ prodict/as4/as4_pro.f  new }

  
/* LANGUAGE DEPENDENCIES END */ /*----------------------------------------*/

DEFINE VARIABLE ddl     AS CHARACTER EXTENT 30 NO-UNDO.
DEFINE VARIABLE i       AS INTEGER             NO-UNDO.
DEFINE VARIABLE stopped AS LOGICAL             NO-UNDO  INIT TRUE.

DEFINE NEW SHARED VARIABLE fil-e   AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE fil-df  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE i-cnt   AS INTEGER   NO-UNDO.
DEFINE NEW SHARED STREAM ddl.
DEFINE NEW SHARED STREAM dumperr.

IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("WAIT").
  
ASSIGN fil-df = user_env[2]
            i = INDEX(user_env[2],".").
ASSIGN fil-e  = (IF i = 0 THEN user_env[2] + ".e"
                 ELSE SUBSTRING(user_env[2],1,i,"character") + "e").
       
OUTPUT STREAM ddl TO VALUE(user_env[2]) NO-ECHO NO-MAP.
SESSION:IMMEDIATE-DISPLAY = yes.

DO ON STOP UNDO, LEAVE:
  FOR EACH DICTDB._File WHERE DICTDB._File._Db-recid = drec_db
    AND (
     IF user_filename = "ALL" THEN
       NOT DICTDB._File._Hidden
     ELSE
     IF user_filename = "SOME" THEN
       CAN-DO(user_env[1],DICTDB._File._File-name)
     ELSE
       RECID(DICTDB._File) = drec_file)
    BREAK BY DICTDB._File._File-num:

    IF TERMINAL <> "" THEN DISPLAY DICTDB._File._File-name 
                                                 WITH FRAME marker.

    RUN "prodict/as4/_as4_ddl.p" (RECID(DICTDB._File)).
    DISPLAY "" @  DICTDB._Field._Field-name WITH FRAME marker.
    DISPLAY "" @ DICTDB._Index._Index-name WITH FRAME marker.
  END. /* for each _file */
  stopped = FALSE.  
END. /* do on stop */

OUTPUT STREAM ddl CLOSE.
IF i-cnt NE 0 THEN
  OUTPUT STREAM dumperr CLOSE.

SESSION:IMMEDIATE-DISPLAY = no.
IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("").

IF stopped THEN 
   MESSAGE "Dump terminated."
      	   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE DO:
  IF TERMINAL <> "" THEN DO:
    IF i-cnt NE 0 THEN DO:
      MESSAGE
       "Warning:  Some data definitions were not dumped because they are not valid"
       + " for an AS/400 database."
       SKIP(1) 
       "Refer to the following error file for details: "
       STRING(fil-e,"X(60)") 
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      HIDE MESSAGE.
    END.                 
    MESSAGE "Dump of definitions completed." 
	   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END. 	   
END.	   
IF TERMINAL <> "" THEN
  HIDE FRAME marker NO-PAUSE.	   
RETURN.
