/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _loddata.p */ /**** Data Dictionary load contents module ****/

/*
  user_env[1] = comma-delim file list
  user_env[2] = load path (for >1 file) or load filename (for 1 file)
  user_env[3] = "MAP <name>" or "NO-MAP" OR ""
  user_env[4] = error percentage
  user_env[5] = comma separated list of "y" (yes) or "n" (no) which
                corresponds to file list in user_env[1], indicating for each,
                whether triggers should be disabled when the dump is done.
  user_env[10] = codepage (set in _usrload.p)

history
    laurief     98/06/09    removed any remaining code that defaults to
                            message alert-boxes by using user_env[6] as a
                            flag for screen or no-screen display, which is
                            now a toggle box in the data load dialog
    hutegger    95/06/26    removed message alert-boxes and output error
                            to <file>.e instead; and "!ERROR!" displayed 
                            in line on screen
    hutegger    95/05/08    PDBNAME brings "<dbname>.db" instead of
                            "<dbname>". So we work around it.
    gfs         94/07/25    use new lodtrail.i
    gfs         94/07/21    expanded "load" message for Windows
    hutegger    94/06/27    uses codepage of trailer if user_env[10] = ""
    gfs         94/06/24    removed lodtrail.i support, codepage now set
                            in _usrload.p and is available via user_env[10]
                            (bug 94-04-28-032)
    hutegger    94/02/24    code-page support
                            new error-message, new input-statments
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED STREAM   loaderr.
DEFINE NEW SHARED VARIABLE errs   AS INTEGER  NO-UNDO.
DEFINE NEW SHARED VARIABLE recs   AS INTEGER. /*UNDO*/
DEFINE NEW SHARED VARIABLE xpos   AS INTEGER  NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos   AS INTEGER  NO-UNDO.

DEFINE VARIABLE c         AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cerror    AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cerrors   AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE codepage  AS CHARACTER           NO-UNDO init "UNDEFINED".
DEFINE VARIABLE d-ldbname AS CHARACTER           NO-UNDO.
DEFINE VARIABLE d-was     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE dsname    AS CHARACTER           NO-UNDO.
DEFINE VARIABLE error%    AS INTEGER             NO-UNDO.
DEFINE VARIABLE fil-d     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE fil-e     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE i         AS INTEGER             NO-UNDO.
DEFINE VARIABLE infinity  AS LOGICAL             NO-UNDO.
DEFINE VARIABLE irecs     AS INTEGER             NO-UNDO.
DEFINE VARIABLE l         AS LOGICAL             NO-UNDO.
DEFINE VARIABLE load_size AS INTEGER             NO-UNDO.
DEFINE VARIABLE lpath     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE lvar      AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE lvar#     AS INTEGER             NO-UNDO.
DEFINE VARIABLE maptype   AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mdy       AS CHARACTER           NO-UNDO.
DEFINE VARIABLE msg       AS CHARACTER           NO-UNDO.
DEFINE VARIABLE numformat AS CHARACTER           NO-UNDO.
DEFINE VARIABLE terrors   AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE use_ds    AS LOGICAL             NO-UNDO.
DEFINE VARIABLE yy        AS INTEGER             NO-UNDO.
DEFINE VARIABLE stopped   AS LOGICAL   INIT TRUE NO-UNDO.

DEFINE STREAM dsfile.

/* LANGUAGE DEPENDENCIES START */ /*---------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 14 NO-UNDO INITIAL [
  /* 1*/ "Loading",
  /* 2*/ "Could not find load file",
  /* 3*/ "for table",
  /* 4*/ "Load of database contents completed.",
  /* 5*/ "Errors listed in .e files placed into same directory as .d files",
  /* 6*/ "ERROR! Trailer indicated",
  /* 7*/ "records, but",
  /* 8*/ "records were loaded.",
  /* 9*/ "ERROR! -d <mdy> or -yy <n> settings of dump were",
  /*10*/ "but current settings are",
  /*11*/ "May cause load errors.",
  /*12*/ "!ERROR!",
  /*13*/ "Take a look at .ds file placed into same directory as Your database.",
  /*14*/ "Expected # of records is unknown.Error % interpreted as absolute #."
].

DEFINE VARIABLE no-file AS CHARACTER FORMAT "x(32)".
DEFINE VARIABLE fil-out AS CHARACTER FORMAT "x(32)".

FORM
  DICTDB._File._File-name  COLUMN-LABEL "Table" FORMAT "x(18)" AT 2
    ATTR-SPACE SPACE(0)
  fil-d  FORMAT "x(27)"    COLUMN-LABEL "Load file"
  msg    FORMAT "x(8)"     COLUMN-LABEL "Records!Loaded"
  errs   FORMAT "ZZZZZZZ9" COLUMN-LABEL "Total  !Errors " 
    ATTR-SPACE SPACE(0)
  irecs  FORMAT "ZZZZZZZ9" COLUMN-LABEL "Expected!Records" SPACE(1)
  HEADER
    " Loading Data.   Press " +
    KBLABEL("STOP") + " to terminate the load process." format "x(66)" SKIP(1)
  WITH FRAME loaddata NO-ATTR-SPACE
  SCREEN-LINES - 7 DOWN ROW 1 CENTERED USE-TEXT
  &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN VIEW-AS DIALOG-BOX THREE-D SCROLLABLE WIDTH 84 TITLE "Load Table Contents"
  &ENDIF
  .


FORM
  DICTDB._File._File-name FORMAT "x(12)" LABEL "Table"
  fil-d                   FORMAT "x(14)" LABEL "Input File"
  recs                                   LABEL "# of Records Read"
  errs                                   LABEL "# of Errors"
  fil-e                   FORMAT "x(14)" LABEL "Error File"
  WITH FRAME dsfile DOWN NO-BOX NO-ATTR-SPACE USE-TEXT.


/* LANGUAGE DEPENDENCIES END */ /*-----------------------------------------*/

PAUSE 0.
VIEW FRAME loaddata.
PAUSE 5 BEFORE-HIDE.

SESSION:IMMEDIATE-DISPLAY = yes.
run adecomm/_setcurs.p ("WAIT").

RUN "prodict/_dctyear.p" (OUTPUT mdy,OUTPUT yy).
{ prodict/dictgate.i &action=query &dbtype=user_dbtype &dbrec=? &output=c }
ASSIGN
  load_size = INTEGER(ENTRY(4,c))
  infinity  = TRUE /* use this to mark initial entry into loop */
  use_ds    = INDEX(user_env[1],",") > 0
  lpath     = user_env[2] /* load path */
  error%    = INTEGER(user_env[4])
  dsname    = (IF user_dbtype = "PROGRESS"
                THEN PDBNAME(user_dbname)
                ELSE user_dbname
              )
  cerrors   = 0 - integer(NOT use_ds).

IF index(dsname,".db") > 0
 THEN SUBSTRING(dsname,INDEX(dsname,".db"),3,"RAW") = "".

IF use_ds THEN OUTPUT STREAM dsfile TO VALUE(dsname + ".ds") NO-ECHO.
stoploop:
DO ON STOP UNDO, LEAVE:
  DO WHILE ENTRY(1,user_env[1]) <> "":
    FIND DICTDB._File
      WHERE DICTDB._File._File-name = ENTRY(1,user_env[1])
        AND DICTDB._File._Db-recid = drec_db.
    user_env[1] = SUBSTRING(user_env[1]
                           ,LENGTH(ENTRY(1,user_env[1]),"character") + 2
                           ,-1
                           ,"character").
    IF infinity THEN .
    ELSE do:
       &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
       &THEN
         /* Nothing to do */
         down 1 with Frame loaddata.
       &ELSE
          IF FRAME-LINE(loaddata) = FRAME-DOWN(loaddata) THEN
             UP FRAME-LINE(loaddata) - 1 WITH FRAME loaddata.
          ELSE DOWN 1 WITH FRAME loaddata.
       &ENDIF
    END.
 
    
    ASSIGN
      infinity = FALSE
      recs     = 0
      irecs    = ?
      d-was    = ?
      maptype  = ""
      fil-d    = (IF NOT use_ds THEN lpath
                 ELSE lpath + (IF DICTDB._File._Dump-name = ?
                 OR SEARCH(lpath + DICTDB._File._Dump-name + ".d") = ?
                 THEN DICTDB._File._File-name ELSE DICTDB._File._Dump-name)
               + ".d").
    IF SEARCH(fil-d) <> ? THEN fil-d = SEARCH(fil-d).
  
    fil-e = SUBSTRING(fil-d,1,LENGTH(fil-d,"character") - 1,"character") + "e".
  
    DISPLAY DICTDB._File._File-name fil-d new_lang[1] @ msg
      WITH FRAME loaddata. /* loading */
    COLOR DISPLAY MESSAGES DICTDB._File._File-name fil-d msg errs irecs
      WITH FRAME loaddata.
      
    IF SEARCH(fil-d) <> ? THEN DO:    
      {prodict/dump/lodtrail.i
        &file    = "fil-d"
        &entries = "IF lvar[i] BEGINS ""records=""
                      THEN irecs     = INTEGER(SUBSTRING(lvar[i],9,-1,""character"")).
                    IF lvar[i] BEGINS ""ldbname=""    
                      THEN d-ldbname = SUBSTRING(lvar[i],9,-1,""character"").
                    IF lvar[i] BEGINS ""dateformat=""
                      THEN d-was     = SUBSTRING(lvar[i],12,-1,""character"").
                    IF lvar[i] BEGINS ""numformat=""
                      THEN numformat = SUBSTRING(lvar[i],11,-1,""character"").
                    IF lvar[i] BEGINS ""map=""       
                      THEN maptype   = SUBSTRING(lvar[i],5,-1,""character"").
                   "
        }
     END.
     ELSE DO:
      ASSIGN fil-out = (IF NOT use_ds THEN lpath
                       ELSE lpath + (IF DICTDB._File._Dump-name = ?
                       OR SEARCH(lpath + DICTDB._File._Dump-name + ".d") = ?
                       THEN DICTDB._File._File-name 
                       ELSE DICTDB._File._Dump-name)).

      ASSIGN no-file = (IF DICTDB._File._Dump-name <> ? 
                        THEN DICTDB._File._Dump-name + ".d" ELSE fil-d).

      DISPLAY new_lang[12] @ msg WITH FRAME loaddata. /* !ERROR! */
      OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
      PUT STREAM loaderr
        "The file " no-file "can not be found." SKIP.
      OUTPUT STREAM loaderr CLOSE.
      ASSIGN terrors = terrors + 1.
      next. /* skip that file */
      END.
      
    DISPLAY  irecs WITH FRAME loaddata. 
    
    /* check if number of records is unknown or known */
    IF irecs = ? AND (error% > 0 AND error% < 100)
     THEN DO:
      DISPLAY new_lang[14] @ msg WITH FRAME loaddata. /* !Warning! */
      OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
      PUT STREAM loaderr
              "Expected # of records is unknown."        SKIP
                    "Error percentage will be interpreted"     SKIP   
                    "as an absolute number of errors."         SKIP.
      OUTPUT STREAM loaderr CLOSE.
      ASSIGN terrors = terrors + 1.
      END.

    /*----- this is not yet intended ------------------------------------
    /* check for ldbname and eventually display warning **/
    IF LDBNAME("DICTDB") <> d-ldbname
     THEN DO:  /* ldbname differ */
      DISPLAY new_lang[???] @ msg WITH FRAME loaddata. /* !ERROR! */
      OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
      PUT STREAM loaderr
        "The current logical DB-name is different from the" SKIP
        "one of the .d-file's source-db."                   SKIP .
      OUTPUT STREAM loaderr CLOSE.
      ASSIGN terrors = terrors + 1.
      END.    /* ldbname differ */
    ------- this is not yet intended ----------------------------------*/
           
    /* check for nummeric format (-E set or not) and error out if wrong */
    IF    numformat <> ""
      AND index(string(1.5,"9.9"),numformat) = 0
      THEN DO:  /* sesion-format and .d-format don't match */
        DISPLAY new_lang[12] @ msg WITH FRAME loaddata. /* !ERROR! */
        OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO APPEND. 
        IF numformat = "."
          THEN DO:

/* removed this message-alert-box to allow uninteruppted processing TH 
 *          MESSAGE "The numeric formats of this PROGRESS-session " SKIP
 *                        "and the .d-file don't match!"                  SKIP
 *                      "Please exit PROGRESS and start a new session"  SKIP
 *                    "without the -E startup parameter."             skip
 *                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
 */
            PUT STREAM loaderr
                    "The numeric formats of this PROGRESS-session " SKIP
                    "and the .d-file don't match!"                  SKIP   
                    "Please exit PROGRESS and start a new session"  SKIP
                    "without the -E startup parameter.".
            END.       
          ELSE DO:
            PUT  STREAM loaderr
                    "The numeric formats of this PROGRESS-session " SKIP
                    "and the .d-file don't match!"                  SKIP   
                    "Please exit PROGRESS and start a new session"  SKIP
                    "with the -E startup parameter.".
/* removed this message-alert-box to allow uninteruppted processing TH 
 *          MESSAGE "The numeric formats of this PROGRESS-session " SKIP
 *                     "and the .d-file don't match!"                  SKIP   
 *                     "Please exit PROGRESS and start a new session"  SKIP
 *                    "with the -E startup parameter."                skip
 *                       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
 */
            END.       
        OUTPUT STREAM loaderr CLOSE.
        ASSIGN terrors = terrors + 1.
        NEXT.   /* skip this file */
        end.    /* sesion-format and .d-format don't match */
          
    IF cerror = ?
     THEN DO:  /* conversion needed but NOT possible */

       /* screen display */
     IF user_env[6] = "f" THEN 
     OUTPUT STREAM loaderr TO fil-e NO-ECHO.
       DO:
        COLOR DISPLAY NORMAL DICTDB._File._File-name fil-d msg errs irecs
          WITH FRAME loaddata.
        DISPLAY DICTDB._File._File-name fil-d new_lang[12] @ msg irecs
          WITH FRAME loaddata. /* ERROR! */
       END.
     OUTPUT CLOSE.
        /* error-file *  /
     END.
        OUTPUT TO VALUE(fil-e) NO-ECHO APPEND.  
        if codepage-convert("a",SESSION:CHARSET,codepage) = ? then .
        OUTPUT CLOSE.      display message on screen!! */
        /* multi-file - load protokoll */
        IF use_ds THEN DO:
          DISPLAY STREAM dsfile
            DICTDB._File._File-name fil-d recs errs 
            "Conversion-error" @ fil-e
            WITH FRAME dsfile.
          DOWN STREAM dsfile WITH FRAME dsfile.
          END.
        /* error-count */
        assign cerrors = cerrors + 1.
        /* skip this file */
        NEXT.

      END.     /* conversion needed but NOT possible */

     ELSE DO:  /* conversion not needed OR needed and possible */

      if cerror = "no-convert"
       then do:
        IF maptype BEGINS "MAP:" 
         THEN INPUT FROM VALUE(fil-d) NO-ECHO 
                    MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
                    NO-CONVERT.
         ELSE INPUT FROM VALUE(fil-d) NO-ECHO 
                    NO-MAP
                    NO-CONVERT.
        end.
       else do:
        IF maptype BEGINS "MAP:"
         THEN INPUT FROM VALUE(fil-d) NO-ECHO
                    MAP VALUE(SUBSTRING(maptype,5,-1,"character"))
                    CONVERT SOURCE codepage TARGET SESSION:CHARSET.
         ELSE INPUT FROM VALUE(fil-d) NO-ECHO 
                    NO-MAP
                    CONVERT SOURCE codepage TARGET SESSION:CHARSET.
        end.

      END.     /* conversion not needed OR needed and possible */
        
    ASSIGN
      c    = DICTDB._File._File-name
      &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
      &THEN
          .
      &ELSE
        xpos = FRAME-COL(loaddata) + 49
        ypos = FRAME-ROW(loaddata) + FRAME-LINE(loaddata) + 5.
      &ENDIF
    CREATE ALIAS "DICTDB2" FOR DATABASE VALUE(user_dbname) NO-ERROR.
  
    OUTPUT STREAM loaderr TO VALUE(fil-e) NO-ECHO.

    IF SEEK(INPUT) = ? THEN DO:
      PUT STREAM loaderr UNFORMATTED 
        new_lang[2] ' "' fil-d '" ' new_lang[3] ' "' c '".'.
      errs = 1.
    END.
    ELSE DO:
      RUN "prodict/misc/_runload.i" (INPUT ENTRY(1, user_env[5]))
                                    c 
                                    error%
                                    load_size 
                                    c 
                                    (IF irecs = ? THEN 100 else irecs).
      IF RETURN-VALUE = "stopped" THEN UNDO stoploop, LEAVE stoploop.

      user_env[5] = SUBSTRING(user_env[5]
                             ,LENGTH(ENTRY(1,user_env[5]),"character") + 2
                             ,-1
                             ,"character"
                             ).
  
      IF irecs = ? THEN _irecs: DO: /*----------- for reading damaged trailer */
        /* this code is used when a trailer might be present, but not in its
           proper relative offset from the start of the file.  this can happen
           if the file is edited or damaged, changing its length. */
        i = SEEK(INPUT).
        READKEY PAUSE 0. IF LASTKEY <> ASC("P") THEN LEAVE _irecs.
        READKEY PAUSE 0. IF LASTKEY <> ASC("S") THEN LEAVE _irecs.
        READKEY PAUSE 0. IF LASTKEY <> ASC("C") THEN LEAVE _irecs.
        SEEK INPUT TO i. /* just to get eol right */
        REPEAT WHILE irecs = ?:
          IMPORT c.
          IF c BEGINS "records=" THEN irecs = INTEGER(SUBSTRING(c,9,-1,"character")).
        END.
      END. /*---------------------------------- end of damaged trailer reader */
  
      INPUT CLOSE.
    END.
    IF irecs <> ? AND irecs <> recs THEN DO:
      /* ERROR! Trailer indicated <n> records, but only <n> records loaded. */
      PUT STREAM loaderr UNFORMATTED 
        ">> " new_lang[6] " " irecs " " new_lang[7] " " recs " " 
              new_lang[8] SKIP.
    END.
    IF d-was <> ? AND d-was <> mdy + STRING(- yy) THEN DO:
      ASSIGN errs = errs + 1.
      /* ERROR! -d <mdy> or -yy <n> settings of dump were <mdy>-<nnnn> */
      /* but current settings are <mdy>-<nnnn>. */
      PUT STREAM loaderr UNFORMATTED
        ">> " new_lang[9] " " d-was SKIP
        "** " new_lang[10] " " mdy STRING(- yy) ".  " new_lang[11] SKIP.
    END.
    OUTPUT STREAM loaderr CLOSE.
    IF errs = 0 THEN OS-DELETE VALUE(fil-e).
  
    COLOR DISPLAY NORMAL DICTDB._File._File-name fil-d msg errs irecs
      WITH FRAME loaddata.
    DISPLAY
      DICTDB._File._File-name fil-d STRING(recs,"ZZZZZZZ9") @ msg errs irecs
      WITH FRAME loaddata.
    IF use_ds THEN DO:
      DISPLAY STREAM dsfile
        DICTDB._File._File-name fil-d recs errs
        (IF errs = 0 THEN "-" ELSE fil-e) @ fil-e
        WITH FRAME dsfile.
      DOWN STREAM dsfile WITH FRAME dsfile.
    END.
    terrors = terrors + errs.
  
  END. /* for each _file */

  stopped = false. 
END. /* end stop */

IF use_ds THEN OUTPUT STREAM dsfile CLOSE.

&IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
&THEN
   /* Nothing to Do */
&ELSE 
DO WHILE FRAME-LINE(loaddata) < FRAME-DOWN(loaddata):
  DOWN 1 WITH FRAME loaddata.
  CLEAR FRAME loaddata NO-PAUSE.
END.
&ENDIF
run adecomm/_setcurs.p ("").

IF stopped THEN
   MESSAGE "Load terminated."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE DO:
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    /* Don't use alert box in TTY because it obscures status report
       and you can't move it out of the away.
    */
    HIDE MESSAGE NO-PAUSE.
    IF      cerrors > 0 THEN MESSAGE new_lang[4] skip new_lang[13]. 
    ELSE IF terrors > 0 THEN MESSAGE new_lang[4] skip new_lang[5]. 
    ELSE                     MESSAGE new_lang[4]. 
    pause.
  &ELSE
    IF      cerrors > 0 THEN MESSAGE new_lang[4] skip new_lang[13] VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
    ELSE IF terrors > 0 THEN MESSAGE new_lang[4] skip new_lang[5]  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
    ELSE                     MESSAGE new_lang[4]                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
  &ENDIF
END.
HIDE MESSAGE NO-PAUSE.
HIDE FRAME loaddata NO-PAUSE.

SESSION:IMMEDIATE-DISPLAY = no.
RETURN.
