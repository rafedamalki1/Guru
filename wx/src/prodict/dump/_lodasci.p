/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* loadasci */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED STREAM   loaderr.
DEFINE NEW SHARED VARIABLE recs   AS INTEGER INITIAL 0. /*UNDO*/
DEFINE NEW SHARED VARIABLE errs   AS INTEGER           NO-UNDO.
DEFINE NEW SHARED VARIABLE xpos   AS INTEGER INITIAL ? NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos   AS INTEGER INITIAL ? NO-UNDO.

DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
DEFINE VARIABLE reason   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfile  AS CHARACTER NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 7 NO-UNDO INITIAL [
  /*  1*/ "records in Delimited ASCII format loaded into",
  /*  2*/ "records in FIXED-LENGTH format loaded into",
  /*  3*/ "There was one error encountered during the translation.",
  /*  4*/ "It is listed in the error file",
  /*5,6*/ "There were", "errors encountered during the translation.",
  /*  7*/ "They are listed in the error file"
].

FORM HEADER
   " Importing.  Press " + 
   KBLABEL("STOP") + " to terminate import process. " format "x(60)" skip(0.5)
   WITH FRAME importing CENTERED ROW 5 USE-TEXT.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

RUN "adecomm/_tmpfile.p" (INPUT "", INPUT ".adm", OUTPUT tmpfile).
RUN "prodict/misc/osquoter.p"
  (user_env[4],user_env[3],user_env[7],tmpfile).

/* create and run import program */

/* user_env[1]=filename, 
   user_env[6]=fieldnames, 
   user_env[10]=disable trigger flag
*/
OUTPUT STREAM loaderr TO VALUE(user_env[1] + ".e") NO-ECHO.
INPUT FROM VALUE(tmpfile) NO-ECHO NO-MAP.
CREATE ALIAS "DICTDB2" FOR DATABASE VALUE(user_dbname) NO-ERROR.

run adecomm/_setcurs.p ("WAIT").
SESSION:IMMEDIATE-DISPLAY = yes.
VIEW FRAME importing.

DO ON STOP UNDO, LEAVE:
  RUN "prodict/misc/_runload.i" (INPUT user_env[10])
      VALUE(user_env[1]) 100 100 user_env[6] 0.
END.

HIDE FRAME importing NO-PAUSE.
SESSION:IMMEDIATE-DISPLAY = no.
INPUT CLOSE.
OUTPUT STREAM loaderr CLOSE.
run adecomm/_setcurs.p ("").

IF errs = 0 THEN DO:
  MESSAGE recs new_lang[IF user_env[3] = ? THEN 2 ELSE 1] user_env[1] 
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-DELETE VALUE(user_env[1] + ".e").  
END.
ELSE
  MESSAGE recs new_lang[IF user_env[3] = ? THEN 2 ELSE 1] user_env[1] SKIP
      	  new_lang[IF errs = 1 THEN 3 ELSE 5] 
      	  (IF errs = 1 THEN "" ELSE STRING(errs) + " " + new_lang[6]) SKIP
          "***" new_lang[IF errs = 1 THEN 4 ELSE 7] user_env[1] + ".e"
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

OS-DELETE VALUE(tmpfile).
RETURN.



