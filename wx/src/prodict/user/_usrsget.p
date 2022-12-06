/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* _usrsget.p - select working schema */

/*
user_env[1] = "dis" - this program disconnects a database
user_env[1] = "new" - this may be the first database connected
user_env[1] = "get" - user wants to select a database to work with
user_env[1] = "sys" - dictionary forcing user to pick a database
*/
/*

History:
    gfs         94/06/23    Fixed F4 prob on disconnect 94-06-02-005
    hutegger    94/05/05    "automatically get select-dialogbox after
                            connecting to an n. db (n > 1)" was switched
                            off; I switched it on again.      
    hutegger    94/05/05    I inserted an extention of user_path, in case
                            there are more then 1 db left after disconnect
                            1. commit transaction; 
                            2. call this routine again

*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

DEFINE VARIABLE answer AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE choice AS INTEGER   INITIAL ?     NO-UNDO.
DEFINE VARIABLE d_char AS CHARACTER EXTENT  4     NO-UNDO.
DEFINE VARIABLE d_log  AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE dbpick AS CHARACTER               NO-UNDO.
DEFINE VARIABLE i      AS INTEGER                 NO-UNDO.
DEFINE VARIABLE is_dis AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE j      AS INTEGER   INITIAL 0     NO-UNDO.
DEFINE VARIABLE l      AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE look   AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE old_db AS INTEGER   INITIAL ?     NO-UNDO.
DEFINE VARIABLE oldbs  AS INTEGER   INITIAL 0     NO-UNDO.
DEFINE VARIABLE olname AS CHARACTER               NO-UNDO.
DEFINE VARIABLE p_down AS INTEGER                 NO-UNDO.
DEFINE VARIABLE p_init AS INTEGER   INITIAL ?     NO-UNDO.
DEFINE VARIABLE p_line AS INTEGER                 NO-UNDO.
DEFINE VARIABLE redraw AS LOGICAL   INITIAL TRUE  NO-UNDO.
DEFINE VARIABLE rpos   AS INTEGER                 NO-UNDO.
DEFINE VARIABLE typed  AS CHARACTER INITIAL ""    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE fast_track AS LOGICAL. /* FT active? */

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 17 NO-UNDO INITIAL [
  /*  1*/ "Are you sure that you want to disconnect database",
  /*  2*/ "Nothing was disconnected.",
  /*  3*/ "Cannot disconnect a database if it is not connected.",
  /*  4*/ "has been disconnected.",
  /*  5*/ "There are only V5/V6/V7 databases connected.  Please use the",
  /*  6*/ "dictionary from V5/V6/V7 Progress to view/edit these databases.",
  /*  7*/ "ERROR! Database type inconsistency in _usrsget.p",
  /*8,9*/ "Database", "is not connected.  Would you like to connect it?",
  /* 10*/ "",  /* reserved */
  /* 11*/ "is the only database connected - it is already selected.",
  /* 12*/ "You have been automatically switched to database",
  /* 13*/ "The V8 Dictionary can't be used with a PROGRESS/V5, /V6 or /V7 database.",
  /* 14*/ "There are no databases connected to select!",
  /* 15*/ "This copy of PROGRESS does not support database type",
  /* 16*/ "There are no databases connected for you to disconnect.",
  /* 17*/ "You have to leave Fast Track before disconnecting a database"
].
FORM
  d_char[1] FORMAT "x(18)"  LABEL "Logical DBName"  ATTR-SPACE SPACE(0)
  d_char[2] FORMAT "x(20)"  LABEL "Physical DBName"
  d_char[3] FORMAT "x(12)"  LABEL "DB Type"         ATTR-SPACE SPACE(0)
  d_char[4] FORMAT "x(18)"  LABEL "Schema Holder"
  d_log     FORMAT "yes/no" LABEL "Con"             ATTR-SPACE
  WITH FRAME schema_stuff NO-ATTR-SPACE USE-TEXT
  ROW 4 COLUMN 1 SCROLL 1 p_down DOWN TITLE COLOR NORMAL
  " Select " + TRIM(STRING(is_dis,"Database to Disconnect/Working Database"))
	     + " ".

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

is_dis = user_env[1] = "dis".

IF is_dis AND NUM-DBS = 0 THEN DO:
  MESSAGE new_lang[16]. /* ain't nothin' to disconnect */
  user_path = "".
  RETURN.
END.

RUN "prodict/_dctsget.p".

IF is_dis THEN DO: /* removed unconnected items from list */
  DO i = 1 TO cache_db#:
    IF CONNECTED(cache_db_l[i]) THEN ASSIGN
      j             = j + 1
      cache_db_e[j] = cache_db_e[i]
      cache_db_l[j] = cache_db_l[i]
      cache_db_p[j] = cache_db_p[i]
      cache_db_s[j] = cache_db_s[i]
      cache_db_t[j] = cache_db_t[i].
  END.
  cache_db# = j.
END.

DO i = 1 TO cache_db#:
  IF cache_db_l[i] = user_dbname AND old_db = ? THEN old_db = i.
  IF  index(cache_db_t[i],"/V5") <> 0
   or index(cache_db_t[i],"/V6") <> 0
   or index(cache_db_t[i],"/V7") <> 0
   /*CAN-DO("PROGRESS/V5,PROGRESS/V6,PROGRESS/V7",cache_db_t[i])*/
   THEN oldbs = oldbs + 1.
END.

ASSIGN
  cache_dirty = TRUE
  p_down      = MINIMUM(cache_db#,SCREEN-LINES - 8)
  p_init      = MINIMUM(p_down,old_db)
  rpos        = (IF old_db = ? THEN 1 ELSE old_db).

IF cache_db# = oldbs AND oldbs > 0 THEN DO:
  IF not is_dis THEN
    MESSAGE new_lang[5] SKIP /* only non-v8 dbs connected */
      	    new_lang[6] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  look = TRUE.
END.
IF cache_db# = 0 OR cache_db# = oldbs THEN DO:
  ASSIGN
    user_path     = ""
    user_dbname   = ""
    user_dbtype   = ""
    user_filename = "".
  DISPLAY user_dbname user_filename WITH FRAME user_ftr.
  IF CAN-DO("get,new",user_env[1]) AND oldbs = 0 THEN
    MESSAGE new_lang[14]. /* no dbs connected */
  IF NOT (look AND is_dis) THEN RETURN.
END.

/* <hutegger> 94/05/05 user should be automatically prompted to select */
/* the current working db, if there are more then 1 db's connected     */
/*
IF old_db <> ? AND user_env[1] = "new" THEN
  choice = old_db.
ELSE
*/
IF cache_db# = 1 AND NOT look THEN DO:
  choice = 1.
  IF user_env[1] = "get" THEN 
    MESSAGE cache_db_l[1] new_lang[11]. /* only 1 db to choose */
  IF cache_db_l[1] <> olname AND
    (user_env[1] = "new"
    OR (user_env[1] = "sys" AND user_dbname <> cache_db_l[1])) THEN
    MESSAGE new_lang[12] '"' + cache_db_l[1] + '"'. /* auto-switch to db... */
END.

DO WHILE choice = ?:

  rpos = MINIMUM(cache_db#,MAXIMUM(rpos,1)).

  IF redraw THEN DO:
    ASSIGN
      p_line = MAXIMUM(1,FRAME-LINE(schema_stuff))
      j      = rpos - (IF p_init = ? THEN p_line ELSE p_init) + 1.
    UP p_line - 1 WITH FRAME schema_stuff.
    IF j < 1 THEN ASSIGN
      j      = 1
      p_line = 1
      rpos   = 1.
    DO i = j TO j + p_down - 1:
      IF i > cache_db# THEN
        CLEAR FRAME schema_stuff NO-PAUSE.
      ELSE
        DISPLAY
          cache_db_l[i] @ d_char[1]
          SUBSTRING(cache_db_p[i]
                   ,MAXIMUM(1,LENGTH(cache_db_p[i],"character") - 20)
                   ,21
                   ,"character"
                   )
            @ d_char[2]
          cache_db_e[i] @ d_char[3]
          cache_db_s[i] @ d_char[4]
          CONNECTED(cache_db_l[i]) @ d_log
          WITH FRAME schema_stuff.
      IF i < j + p_down - 1 THEN
        DOWN WITH FRAME schema_stuff.
    END.
    p_line = (IF p_init = ? THEN p_line /*1*/ ELSE p_init).
    UP p_down - p_line WITH FRAME schema_stuff.
    ASSIGN
      p_init = ?
      redraw = FALSE.
  END.

  DISPLAY
    cache_db_l[rpos] @ d_char[1]
    SUBSTRING(cache_db_p[rpos]
             ,MAXIMUM(1,LENGTH(cache_db_p[rpos],"character") - 20)
             ,21
             ,"character"
             )
      @ d_char[2]
    cache_db_e[rpos] @ d_char[3]
    cache_db_s[rpos] @ d_char[4]
    CONNECTED(cache_db_l[rpos]) @ d_log
    WITH FRAME schema_stuff.
  COLOR DISPLAY MESSAGES d_char[1 FOR 4] d_log WITH FRAME schema_stuff.
  READKEY.
  COLOR DISPLAY NORMAL d_char[1 FOR 4] d_log WITH FRAME schema_stuff.
  PAUSE 0.

  IF ( KEYFUNCTION(LASTKEY) = CHR(LASTKEY) 
   AND LASTKEY >= 32
     )
   OR ( KEYFUNCTION(LASTKEY) = "BACKSPACE"
   AND  LENGTH(typed,"character") > 0
      )
   THEN DO:
    typed = (IF KEYFUNCTION(LASTKEY) = "BACKSPACE"
        THEN SUBSTRING(typed,1,LENGTH(typed,"character") - 1,"character")
        ELSE typed + CHR(LASTKEY)).
    IF typed = "" OR cache_db_l[rpos] BEGINS typed THEN NEXT.
    DO p_line = rpos TO cache_db#:
      IF cache_db_l[p_line] BEGINS typed THEN LEAVE.
    END.
    IF p_line > cache_db# THEN DO:
      DO p_line = 1 TO rpos:
        IF cache_db_l[p_line] BEGINS typed THEN LEAVE.
      END.
      IF p_line > rpos THEN p_line = cache_db# + 1.
    END.
    IF p_line > cache_db# THEN DO:
      typed = CHR(LASTKEY).
      DO p_line = 1 TO cache_db#:
        IF cache_db_l[p_line] BEGINS typed THEN LEAVE.
      END.
    END.
    IF p_line <= cache_db# THEN ASSIGN
      rpos   = p_line
      redraw = TRUE.
    NEXT.
  END.

  typed = "".
  IF KEYFUNCTION(LASTKEY) = "CURSOR-DOWN" AND rpos < cache_db# THEN DO:
    rpos = rpos + 1.
    IF FRAME-LINE(schema_stuff) = FRAME-DOWN(schema_stuff) THEN
      SCROLL UP WITH FRAME schema_stuff.
    ELSE
      DOWN WITH FRAME schema_stuff.
  END.
  ELSE
  IF KEYFUNCTION(LASTKEY) = "CURSOR-UP" AND rpos > 1 THEN DO:
    rpos = rpos - 1.
    IF FRAME-LINE(schema_stuff) = 1 THEN
      SCROLL DOWN WITH FRAME schema_stuff.
    ELSE
      UP WITH FRAME schema_stuff.
  END.
  ELSE
  IF KEYFUNCTION(LASTKEY) = "PAGE-DOWN" THEN ASSIGN
    rpos   = rpos + p_down
    redraw = TRUE.
  ELSE
  IF KEYFUNCTION(LASTKEY) = "PAGE-UP" THEN ASSIGN
    rpos   = rpos - p_down
    redraw = TRUE.
  ELSE
  IF CAN-DO("HOME,MOVE",KEYFUNCTION(LASTKEY)) AND rpos > 1 THEN DO:
    ASSIGN
      rpos   = 1
      redraw = TRUE.
    UP FRAME-LINE(schema_stuff) - 1 WITH FRAME schema_stuff.
  END.
  ELSE
  IF CAN-DO("END,HOME,MOVE",KEYFUNCTION(LASTKEY)) THEN DO:
    ASSIGN
      rpos   = cache_db#
      redraw = TRUE.
    DOWN p_down - FRAME-LINE(schema_stuff) WITH FRAME schema_stuff.
  END.
  ELSE
  IF CAN-DO("GO,RETURN,END-ERROR",KEYFUNCTION(LASTKEY)) THEN DO:
    l = KEYFUNCTION(LASTKEY) = "END-ERROR".
    IF NOT l AND NOT is_dis
      AND (index(cache_db_t[rpos],"/V5") <> 0
      or   index(cache_db_t[rpos],"/V6") <> 0
      or   index(cache_db_t[rpos],"/V7") <> 0 )
      /*CAN-DO("PROGRESS/V5,PROGRESS/V6,PROGRESS/V7",cache_db_t[rpos])*/
      THEN DO:
      MESSAGE new_lang[13]. /* cannot use V5/V6/V7 db and V8 dict together */
      NEXT.
    END.
    ELSE
    /*-----
    IF NOT l AND NOT CAN-DO(GATEWAYS,cache_db_t[rpos]) THEN DO:
      MESSAGE new_lang[15] cache_db_e[rpos]. /* unsupported dbtype */
      NEXT.
    END.
    ------*/ 
    ASSIGN
      choice = (IF l THEN old_db ELSE rpos)
      choice = (IF choice = ? THEN 1 ELSE choice).
  END.

  p_line = 1.
END.

dbpick = cache_db_l[choice].
IF KEYFUNCTION(LASTKEY)  = "END-ERROR" THEN user_path = "".

IF is_dis THEN _discon: DO: /*-----------------------------*/ /* disconnect */
  IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN LEAVE _discon.
  answer = FALSE.
  RUN "prodict/user/_usrdbox.p" (INPUT-OUTPUT answer,?,?,
    new_lang[1] + ' "' + dbpick + '"?'). /* discon? */
  IF NOT answer OR answer = ? THEN DO:
    MESSAGE new_lang[2]. /* nothing disconnected */
    user_path = "".
    LEAVE _discon.
  END.
  /* If dictionary was started from Fast Track, return here.     */
  /* (Disconnecting within Fast Track doesn't make sense because */
  /* file-caching is done at startup-time when ft.p is run.      */
  IF fast_track THEN DO:
    BELL.
    MESSAGE new_lang[17].
    PAUSE.
    RETURN.
  END.
  DISCONNECT VALUE(dbpick).
  RUN "prodict/_dctsget.p". /* recache list */
  MESSAGE dbpick new_lang[4]. /* disconnected */
  IF  (        user_dbname  = dbpick
    OR SDBNAME(user_dbname) = dbpick )
    THEN DO:
    ASSIGN
      user_dbname   = ""
      user_dbtype   = ""
      user_filename = "".
    /* recount the residing db's:                                   */
    /* if disconnecting a shemaholder, then don't count NEITHER the */
    /* schemaholder NOR its schemas; else don't count only the      */
    /* disconnected db                                              */
    ASSIGN j = 0.
    DO i = 1 TO cache_db#:
      IF  ( cache_db_l[i] <> dbpick
        OR  cache_db_t[i] <> "PROGRESS" )
        AND cache_db_s[i] <> dbpick       
        THEN j = j + 1.
    END.

    if j > 1 then assign user_path = "*C,1=get,_usrsget" + user_path.
    DISPLAY user_dbname user_filename WITH FRAME user_ftr.
  END.
END. /*---------------------------------------------------------------------*/
ELSE _select: DO: /*-------------------------------------------*/ /* select */
  IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN LEAVE _select.
  ASSIGN
    user_dbname   = dbpick
    user_dbtype   = cache_db_t[choice]
    cache_dirty   = TRUE
    user_filename = ""
    drec_db       = ?
    drec_file     = ?.
  { prodict/user/usercon.i user_filename }

  CREATE ALIAS "DICTDB" FOR DATABASE VALUE(cache_db_s[choice]) NO-ERROR.
  IF LDBNAME("DICTDBG") <> ? THEN DELETE ALIAS "DICTDBG".
  IF user_dbtype = "PROGRESS" THEN
    dbpick = ?.
  ELSE IF CONNECTED(dbpick) THEN
    CREATE ALIAS "DICTDBG" FOR DATABASE VALUE(dbpick) NO-ERROR.
  RUN "prodict/_dctsset.p" (dbpick).

  IF NOT CONNECTED(user_dbname) AND CAN-DO(GATEWAYS,user_dbtype) THEN DO:
    answer = TRUE.
    RUN "prodict/user/_usrdbox.p" (INPUT-OUTPUT answer,?,?,
      new_lang[8] + ' "' + user_dbname + '" ' + new_lang[9]).
    IF answer THEN user_path = "*C,1=usr,_usrscon".
  END.
END. /*---------------------------------------------------------------------*/

HIDE FRAME schema_stuff NO-PAUSE.
RETURN.
