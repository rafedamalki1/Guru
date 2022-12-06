/*t_lodsddl.p*/
DEFINE INPUT PARAMETER df-file-name AS CHARACTER NO-UNDO.

{ ladda\loaddefs.i NEW }
{ ladda\dictvar.i   }
{ ladda\uservar.i   }
 
DEFINE VAR save_ab       AS LOGICAL        NO-UNDO.
DEFINE VAR codepage AS CHARACTER      NO-UNDO FORMAT "X(20)".
DEFINE VAR lvar     AS CHAR EXTENT 10 NO-UNDO.
DEFINE VAR lvar#    AS INT            NO-UNDO.
DEFINE VAR i        AS INT            NO-UNDO.





DEFINE NEW SHARED TEMP-TABLE s_ttb_fake-cp
    FIELD   db-name     AS CHARACTER
    FIELD   db-recid    AS RECID.
    

DEFINE VARIABLE dbload-e      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE hdr           AS INTEGER INIT 0      NO-UNDO.

DEFINE VARIABLE scrap         AS CHARACTER           NO-UNDO.

DEFINE VARIABLE cerror        AS CHARACTER           NO-UNDO.

DEFINE VARIABLE j             AS INTEGER             NO-UNDO.
DEFINE VARIABLE w             AS INTEGER             NO-UNDO.
DEFINE VARIABLE k             AS INTEGER             NO-UNDO.
DEFINE VARIABLE iobj          AS CHARACTER           NO-UNDO. /* d,t,f,i */
DEFINE VARIABLE inot          AS LOGICAL             NO-UNDO.
DEFINE VARIABLE inum          AS INTEGER             NO-UNDO.
DEFINE VARIABLE l_fld-stlen   AS INTEGER             NO-UNDO. /* foreign dbs... */
DEFINE VARIABLE l_fld-stoff   AS INTEGER             NO-UNDO. /* foreign dbs... */


DEFINE VARIABLE stopped       AS LOGICAL             NO-UNDO INIT TRUE.
DEFINE VARIABLE sav_dbnam     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE sav_dbtyp     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE sav_err       AS CHARACTER           NO-UNDO.
DEFINE VARIABLE sav_drec      AS RECID               NO-UNDO.
DEFINE VARIABLE xerror        AS LOGICAL             NO-UNDO. /* any error during process? */
DEFINE VARIABLE xwarn         AS LOGICAL             NO-UNDO. /* any warnings during process? */
DEFINE VARIABLE do-commit     AS LOGICAL             NO-UNDO.

/* collate/translate information */
DEFINE VARIABLE ct_version    AS CHARACTER           NO-UNDO.                /* version # */
DEFINE VARIABLE ct_changed    AS LOGICAL             NO-UNDO INIT no.  /* tables modified? */
DEFINE VARIABLE ix            AS INTEGER             NO-UNDO. /* for parsing version # */
DEFINE VARIABLE len           AS INTEGER             NO-UNDO. /* ditto */
DEFINE VARIABLE raw_val       AS RAW                 NO-UNDO.
DEFINE VARIABLE orig_vers     AS RAW                 NO-UNDO.
DEFINE VARIABLE rules         AS LONGCHAR            NO-UNDO.

DEFINE variable minimum-index AS INTEGER initial 0.
DEFINE variable new-number    AS INTEGER initial 0.
DEFINE VARIABLE hBuffer       AS HANDLE              NO-UNDO.

/* messages for frames working2 and backout. */
DEFINE VARIABLE msg1          AS CHARACTER           NO-UNDO FORMAT "x(53)":u.
DEFINE VARIABLE msg2          AS CHARACTER           NO-UNDO FORMAT "x(56)":u.
DEFINE VARIABLE msg3          AS CHARACTER           NO-UNDO FORMAT "x(12)":u.
DEFINE VARIABLE msg4          AS CHARACTER           NO-UNDO FORMAT "x(16)":u.


FORM
  wdbs._Db-name    LABEL "Database" COLON 11 FORMAT "x(32)":u SKIP
  wfil._File-name  LABEL "Table"    COLON 11 FORMAT "x(32)":u SKIP
  wfld._Field-name LABEL "Field"    COLON 11 FORMAT "x(32)":u SKIP
  widx._Index-name LABEL "Index"    COLON 11 FORMAT "x(32)":u SKIP
  wseq._Seq-Name   LABEL "Sequence" COLON 11 FORMAT "x(32)":u SKIP
  HEADER 
    " Loading Definitions.  Press " +
    KBLABEL("STOP") + " to terminate load process." FORMAT "x(70)" 
  WITH FRAME working 
  ROW 4 CENTERED USE-TEXT SIDE-LABELS ATTR-SPACE &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN VIEW-AS DIALOG-BOX THREE-D TITLE "Load Data Definitions" &ENDIF.

COLOR DISPLAY MESSAGES
  wdbs._Db-name wfil._File-name wfld._Field-name
  widx._Index-name wseq._Seq-Name
  WITH FRAME working.









/*==========================Mainline Code==============================*/

/* Fix problems which resulted when .rcode is run on Japanese DOS/WIN, 640x480
   while the .r-code was compiled/created with US fonts/Progress.ini
   Increase the frame size a little bit. 
*/
       
ASSIGN
 /* cache_dirty = TRUE
  */
  user_dbname = user_env[8]. /* for backwards compatibility with _lodsddl.p */

DO FOR _Db:
   FIND FIRST _Db WHERE _Db._Db-name = ( IF user_dbtype = "PROGRESS" THEN ? ELSE user_dbname) NO-ERROR.
   ASSIGN
   drec_db      = RECID(_Db)
   drec_file    = ?
   sav_dbnam    = user_dbname
   sav_dbtyp    = user_dbtype
   sav_drec     = drec_db
   ierror       = 0
   ilin         = ?
   ipos         = 0
   gate_dbtype  = user_dbtype
   gate_proc    = ""
   do-commit    = (IF user_env[15] = "yes" THEN TRUE ELSE FALSE).
END.
  


ASSIGN dbload-e = LDBNAME("DICTDB") + ".e".

is-pre-101b-db = YES.

IF INTEGER(DBVERSION("DICTDB")) >= 10 THEN DO:
   /* use a dyn buffer since v9 db's don't have the feature tbl */
   CREATE BUFFER hBuffer FOR TABLE "DICTDB._Code-feature" NO-ERROR.
   IF VALID-HANDLE(hBuffer) THEN DO:
      hBuffer:FIND-FIRST('where _Codefeature_Name = "Large Keys"',NO-LOCK) NO-ERROR.
      IF hBuffer:AVAILABLE THEN is-pre-101b-db = NO.
      DELETE OBJECT hBuffer.
   END.
END.


ASSIGN codepage = IF user_env[10] = "" THEN "UNDEFINED"  ELSE user_env[10]. /* set in _usrload.p */
IF codepage <> "UNDEFINED" AND SESSION:CHARSET <> ? THEN  ASSIGN cerror = CODEPAGE-CONVERT("a",SESSION:CHARSET,codepage).
ELSE ASSIGN cerror = "no-convert".

IF cerror = ? THEN DO:  /* conversion needed but NOT possible */
   ASSIGN user_env[4] = "error". /* to signal error to _usrload */
END.     /* conversion needed but NOT possible */

ELSE DO FOR _Db, _file, _Field, _Index, _Index-field TRANSACTION:
   SESSION:IMMEDIATE-DISPLAY = yes.
   IF cerror = "no-convert" THEN INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP NO-CONVERT.
   ELSE INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP CONVERT SOURCE codepage TARGET SESSION:CHARSET.
   IF FALSE THEN FIND NEXT wdbs.
   IF FALSE THEN FIND NEXT wfil.
   IF FALSE THEN FIND NEXT wfld.
   IF FALSE THEN FIND NEXT widx.
   IF FALSE THEN FIND NEXT wseq.  
   FIND FIRST _Db WHERE RECID(_Db) = drec_db.  
   DO ON STOP UNDO, LEAVE:       
      load_loop:
      REPEAT ON ERROR UNDO,RETRY ON ENDKEY UNDO, LEAVE:      
         IF ilin[1] <> ? THEN DO: 
            ASSIGN
            inum    = inum + (IF CAN-DO("OF,ON":u,ilin[inum + 1]) THEN 2 ELSE 0)
            ilin[1] = ilin[inum + 1]
            ilin[2] = ilin[inum + 2]
            ilin[3] = ilin[inum + 3]
            ilin[4] = ilin[inum + 4]
            ilin[5] = (IF inum + 5 > 9 THEN ? ELSE ilin[inum + 5])
            ilin[6] = (IF inum + 6 > 9 THEN ? ELSE ilin[inum + 6])
            ilin[7] = (IF inum + 7 > 9 THEN ? ELSE ilin[inum + 7])
            ilin[8] = (IF inum + 8 > 9 THEN ? ELSE ilin[inum + 8])
            ilin[9] = ?. 
         END.
         DO WHILE ilin[1] BEGINS "#":u OR ilin[1] = "" OR ilin[1] = ?:
            ASSIGN
            ipos = ipos + 1
            ilin = ?.
            IMPORT ilin.
         END.
         inum = 0.
         ASSIGN stopped = true.
         IF CAN-DO("ADD,CREATE,NEW,UPDATE,MODIFY,ALTER,CHANGE,DELETE,DROP,REMOVE,RENAME":u,ilin[1]) THEN DO:
            IF AVAILABLE wdbs AND imod <> ? THEN DO: 
                       
               IF CAN-FIND(DICTDB._Db WHERE DICTDB._Db._Db-name = wdbs._Db-name) THEN.
               ELSE DO:
                  RUN "ladda\t_lod_dbs.p".
                  IF drec_db <> RECID(_Db) THEN FIND _Db WHERE RECID(_Db) = drec_db.
               END.   
            END.            
            IF AVAILABLE wfil AND imod <> ? THEN DO:
               
               IF CAN-FIND(_File WHERE _File._Db-recid = drec_db AND _File._File-name = wfil._File-name AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN") ) THEN.
               ELSE RUN "ladda\t_lod_fil.p". 
            END.            
                         
            IF AVAILABLE wfld AND imod <> ? THEN DO:    
                 
               IF CAN-FIND(_Field WHERE _Field._File-recid = drec_file AND _Field._Field-name = wfld._Field-name) THEN.                  
               ELSE RUN "ladda\t_lod_fld.p"(INPUT-OUTPUT minimum-index).              
            END.               
            IF AVAILABLE widx AND imod <> ? THEN DO:   
               FIND _File WHERE RECID(_File) = drec_file.
               IF CAN-FIND(FIRST _Index OF _File  WHERE _Index._Index-name = widx._Index-name) THEN.                      
               ELSE RUN "ladda\t_lod_idx.p"(INPUT-OUTPUT minimum-index).               
            END.
           
            IF AVAILABLE wseq AND imod <> ? THEN  DO:
               IF CAN-FIND(_Sequence WHERE _Sequence._Db-recid = drec_db  AND _Sequence._Seq-name = wseq._Seq-name) THEN.          
               ELSE RUN "ladda\t_lod_seq.p".
            END.
  
        
            FOR EACH wdbs: DELETE wdbs. END.
            FOR EACH wfil: DELETE wfil. END.
            FOR EACH wfit: DELETE wfit. END.
            FOR EACH wfld: DELETE wfld. END.
            FOR EACH wflt: DELETE wflt. END.
            FOR EACH widx: DELETE widx. END.
            FOR EACH wixf: DELETE wixf. END.
            FOR EACH wseq: DELETE wseq. END.
  
            ASSIGN
            icomponent = 0
            iprimary   = FALSE
            inoerror   = FALSE
            imod       = ?
            iobj       = ?
            inum       = 3.
    
            /* set the action mode */
            CASE ilin[1]:
               WHEN "ADD":u    OR WHEN "CREATE":u OR WHEN "NEW":u  THEN imod = "a":u.
               WHEN "UPDATE":u OR WHEN "MODIFY":u OR WHEN "ALTER":u OR WHEN "CHANGE":u THEN imod = "m":u.
               WHEN "DELETE":u OR WHEN "DROP":u OR WHEN "REMOVE":u THEN imod = "d":u.
               WHEN "RENAME":u                                     THEN imod = "r":u.
            END CASE.
       
            /* set the object type */
            CASE ilin[2]:
               WHEN "DATABASE":u OR WHEN "CONNECT":u THEN iobj = "d":u.
               WHEN "FILE":u     OR WHEN "TABLE":u   THEN iobj = "t":u.
               WHEN "FIELD":u    OR WHEN "COLUMN":u  THEN iobj = "f":u.
               WHEN "INDEX":u    OR WHEN "KEY":u     THEN iobj = "i":u.
               WHEN "SEQUENCE":u                     THEN iobj = "s":u.
            END CASE.
        
            IF iobj = ? THEN DO:
               /* may be ADD [UNIQUE] [PRIMARY] [INACTIVE] INDEX name */
               IF CAN-DO("INDEX,KEY":u,ilin[3]) THEN DO:
                  ASSIGN
                  iobj    = "i":u
                  ilin[3] = ilin[4]  /* set name into slot 3 */
                  ilin[4] = ilin[2]. /* move other opt[s] to end */
               END.  
               ELSE IF CAN-DO("INDEX,KEY",ilin[4]) THEN DO:
                  ASSIGN
                  iobj    = "i"
                  ilin[4] = ilin[3]  /* move other opt[s] to end */
                  ilin[3] = ilin[5]  /* set name into slot 3 */
                  ilin[5] = ilin[2]. /* move other opt[s] to end */
               END.   
               ELSE IF CAN-DO("INDEX,KEY",ilin[5]) THEN DO:
                  ASSIGN
                  iobj    = "i"
                  ilin[5] = ilin[3]  /* move other opt[s] to end */
                  ilin[3] = ilin[6]  /* set name into slot 3 */
                  ilin[6] = ilin[2]. /* move other opt[s] to end */
               END.   
            END.
  
            IF iobj = "d" THEN CREATE wdbs.
            IF iobj = "t" THEN CREATE wfil.
            IF iobj = "f" THEN CREATE wfld.
            IF iobj = "i" THEN CREATE widx.
            IF iobj = "s" THEN CREATE wseq.
            IF iobj = "d" THEN DO: /* start database action block */
               IF TERMINAL <> "" THEN DO: 
                  DISPLAY
                  (IF ilin[3] = "?" THEN user_dbname ELSE ilin[3]) @ wdbs._Db-name
                  "" @ wfil._File-name  "" @ wfld._Field-name
                  "" @ widx._Index-name "" @ wseq._Seq-Name
                  WITH FRAME working.
               END.   
               IF imod = "a" THEN wdbs._Db-name = ilin[3].
               ELSE DO:
                  IF ilin[3] = "?" THEN FIND FIRST _Db WHERE _Db._Db-name = ? NO-ERROR.
                  ELSE FIND FIRST _Db WHERE _Db._Db-name = ilin[3] NO-ERROR.            
                  IF AVAILABLE _Db THEN drec_db = RECID(_Db).
               END.          
            END. /* end database action block */
  
            IF iobj = "s" THEN DO: /* start sequence action block */
               IF TERMINAL <> "" THEN DO: 
                  DISPLAY 
                  user_dbname @ wdbs._Db-name
                  "" @ wfil._File-name  "" @ wfld._Field-name
                  "" @ widx._Index-name ilin[3] @ wseq._Seq-Name
                  WITH FRAME working.           
               END.  
               IF imod = "a" THEN DO:
                  IF KEYWORD(ilin[3]) <> ? THEN DO:
                     ASSIGN wseq._Seq-Name = ilin[3]
                     ierror = 45.             
                  END.
                  ELSE ASSIGN wseq._Seq-Name = ilin[3].
               END.          
               ELSE DO:
                  FIND FIRST _Sequence WHERE _Sequence._Seq-Name = ilin[3] NO-ERROR.
                  IF NOT AVAILABLE _Sequence THEN DO:    
                     ASSIGN wseq._Seq-name = ilin[3].
                     ierror = 3. /* "Try to modify unknown &2" */              
                  END.   
                  IF imod = "r" THEN DO:
                     IF KEYWORD(ilin[5]) <> ? THEN DO:
                        ASSIGN wseq._Seq-Name = ilin[3]
                        ierror = 45.               
                     END.
                  END.
                  { ladda\copy_seq.i &from=_Sequence &to=wseq }
               END.
            END. /* end sequence action block */
  
            /* position _file record */
            IF (iobj = "d" OR iobj = "s") OR (imod = "a" AND iobj = "t") THEN .
            ELSE DO:
               scrap = ?.
               IF iobj = "t"                   THEN scrap = ilin[3].
               ELSE IF CAN-DO("OF,ON",ilin[4]) THEN scrap = ilin[5].
               ELSE IF CAN-DO("OF,ON",ilin[5]) THEN scrap = ilin[6].
               ELSE IF CAN-DO("OF,ON",ilin[6]) THEN scrap = ilin[7].
               ELSE IF CAN-DO("OF,ON",ilin[7]) THEN scrap = ilin[8].
               IF scrap = ? THEN .
               ELSE IF AVAILABLE _Db THEN DO:            
                  FIND _File OF _Db WHERE _File._File-name = scrap AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN") NO-ERROR.            
               END.
               ELSE DO:
                  IF INTEGER(DBVERSION(user_dbname)) > 8 THEN
                  FIND FIRST _File WHERE _File._File-name = scrap AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN") NO-ERROR.
                  ELSE FIND FIRST _File WHERE _File._File-name = scrap NO-ERROR.
               END.         
               IF AVAILABLE _File AND NOT AVAILABLE _Db THEN DO:
                  FIND _Db OF _File.
                  drec_db = RECID(_Db).
               END.
               IF scrap <> ? AND AVAILABLE _File THEN drec_file = RECID(_File).              
            END.  
            IF iobj = "t" THEN DO: /* start file action block */
               IF TERMINAL <> "" THEN DO: 
                  DISPLAY user_dbname @ wdbs._Db-name
                       ilin[3] @ wfil._File-name "" @ wfld._Field-name 
                       "" @ widx._Index-name "" @ wseq._Seq-Name
                         WITH FRAME working.
               END.          
               IF imod = "a" THEN DO:
                  IF KEYWORD(ilin[3]) <> ? THEN DO:
                     ASSIGN wfil._File-name = ilin[3]                      
                     ierror = 45.                
                  END.
                  ELSE ASSIGN wfil._File-name = ilin[3].
               END.
               ELSE DO:
                  IF NOT AVAILABLE _File THEN DO:
                     ASSIGN wfil._File-name = ilin[3].
                     ierror = 3. /* "Try to modify unknown &2" */              
                  END.
                  IF imod = "r" THEN DO:
                     IF KEYWORD(ilin[5]) <> ? THEN DO:
                        ASSIGN wfil._File-name = ilin[3] 
                        ierror = 45.
                     END.
                  END.
                  { ladda\copy_fil.i &from=_File &to=wfil &all=true}
   
                  FOR EACH _File-trig OF _File:
                     CREATE wfit.
                     { ladda\copy_fit.i &from=_File-trig &to=wfit }
                  END.
               END.
            END. /* end file action block */
  
            IF iobj = "f" THEN DO: /* start field action block */
               IF NOT AVAILABLE _File AND drec_file <> ? THEN  FIND _File WHERE drec_file = RECID(_File) NO-ERROR.
               IF NOT AVAILABLE _File THEN DO:
                  ierror = 5. /* "Try to modify &2 without file" */            
               END.
               ELSE IF AVAILABLE _File AND _file._File-name <> ilin[5] THEN DO:
                  FIND _file WHERE _file._file-name = ilin[5] AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN") NO-ERROR.
               END.   
               IF NOT AVAILABLE _File THEN DO:
                  ierror = 5. /* "Try to modify &2 without file" */            
               END.
               IF TERMINAL <> "" THEN DO: 
                  DISPLAY _File._File-name @ wfil._File-name
                                    ilin[3] @ wfld._Field-name 
                                    "" @ widx._Index-name
                       WITH FRAME working.
               END. 
               IF imod = "a" THEN DO:
                  IF KEYWORD(ilin[3]) <> ? THEN DO:
                     ASSIGN wfld._Field-name = ilin[3] 
                           ierror = 45.
                   
                  END.
                  ELSE DO:
                     ASSIGN wfld._Field-name = ilin[3]    
                        wfld._Initial    = "". /* to be checkable in _lod_fld */
                  END.      
               END.
               ELSE DO:
                  FIND _Field OF _File WHERE _Field._Field-name = ilin[3] NO-ERROR.
                  IF NOT AVAILABLE _Field THEN DO:
                     ASSIGN wfld._Field-name = ilin[3].
                     ierror = 3. /* "Try to modify unknown &2" */              
                  END.
                  IF imod = "r" THEN DO:
                     IF KEYWORD(ilin[7]) <> ? THEN DO:
                        ASSIGN wfld._Field-name = ilin[3] 
                          ierror = 45.               
                     END.
                  END.
                  { ladda\copy_fld.i &from=_Field &to=wfld &all=true}
                  FOR EACH _Field-trig OF _Field:
                     CREATE wflt.
                    { ladda\copy_flt.i &from=_Field-trig &to=wflt }
                  END.
               END.
            END. /* end field action block */
  
            IF iobj = "i" THEN DO: /* start index action block */
               IF NOT AVAILABLE _File AND drec_file <> ? THEN  FIND _File WHERE drec_file = RECID(_File) NO-ERROR.
               IF NOT AVAILABLE _File THEN DO:
                  ierror = 5. /* "try to modify index w/o file" */              
               END.
               ELSE IF AVAILABLE _File THEN DO k = 4 TO 8:
                  IF ilin[k] = "ON" THEN DO:
                     IF _File._File-name <> ilin[k + 1] THEN DO:                  
                        FIND _File WHERE _File._file-name = ilin[k + 1] AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN") NO-ERROR.
                        ASSIGN k = 8.
                     END.
                  END.
               END.
               IF NOT AVAILABLE _File THEN DO:
                  ierror = 5.  /* "Try to modify &2 without file" */              
               END.         
               IF TERMINAL <> "" THEN DO:
                  DISPLAY _File._File-name @ wfil._File-name
                                    "" @ wfld._Field-name
                       ilin[3] @ widx._Index-name 
                  WITH FRAME working.
               END. 
               IF imod = "a" THEN DO:
                  IF KEYWORD(ilin[3]) <> ? THEN DO:
                     ASSIGN widx._Index-name = ilin[3]
                     ierror = 45.                
                  END.
                  ELSE DO:           
                     ASSIGN widx._Index-name = ilin[3]
                     widx._Unique     = FALSE. /* different from schema default of TRUE */
                  END.  
               END.
               ELSE DO:
                  FIND _Index OF _File WHERE _Index._Index-name = ilin[3] NO-ERROR.
                  IF NOT AVAILABLE _Index THEN DO:
                     ASSIGN widx._Index-name = ilin[3].
                     ierror = 3. /* "Try to modify unknown &2" */                
                  END.
                  IF imod = "r" THEN DO:
                     IF KEYWORD(ilin[5]) <> ? THEN DO:
                        ASSIGN widx._Index-name = ilin[3]
                            ierror = 45.                  
                     END.
                  END.
                  { ladda\copy_idx.i &from=_Index &to=widx }           
               END.
            END. /* end index action block */
         
         END. /* end of block handling action keyword */
      
    /* inot - is used for logical keywords that have no argument but where
              the keyword indicates the yes/no value of the field.
       inum - is the number of tokens (keyword plus arguments)  It is set to
              3 above when ilin[1] is the action word (e.g., ADD) and not
              one of the field values (e.g., DATA-TYPE Character)
       ikwd - is the keyword.
       iarg - is the field value.
    */
    
         IF inum <> 3 THEN DO:
            ASSIGN
             ikwd   = ilin[1]
             iarg   = ilin[2]
             inot   = FALSE
             inum   = 2
             inot   = (ikwd BEGINS "NOT-")
             ikwd   = SUBSTRING(ikwd, IF inot THEN 5 ELSE 1, -1, "CHARACTER")
             inum   = (IF ikwd = "NO-ERROR"
                    OR (iobj = "t" AND CAN-DO("FROZEN,HIDDEN",ikwd))
                    OR (iobj = "f" AND CAN-DO("MAND*,NULL,NULL-A*,CASE-SENS*",ikwd))
                    OR (iobj = "i" AND CAN-DO("UNIQUE,INACTIVE,PRIMARY,WORD",ikwd))
                    THEN 1 ELSE 2)
             iarg   = (IF inum = 2 THEN iarg ELSE (IF inot THEN "no" ELSE "yes")).
         END.    
    /* Load the value from the .df file into the appropriate field
       of the object we're working on.
    */
      
         IF inum = 3 THEN .
         ELSE IF ikwd = "NO-ERROR" THEN inoerror = TRUE.
         ELSE IF imod = "r" AND ilin[1] = "TO" THEN irename = ilin[2].
         ELSE IF iobj = "d" THEN DO: /*------------- DB-CASE ----------------------*/
            CASE ikwd:
               WHEN "DBNAME"  OR WHEN "ADDRESS"     THEN wdbs._Db-addr     = iarg.
               WHEN "PARAMS"  OR WHEN "COMM"        THEN wdbs._Db-comm     = iarg.
               WHEN "CONNECT" OR WHEN "DATABASE"    THEN wdbs._Db-name     = iarg.
               WHEN "TYPE"                          THEN wdbs._Db-type     = iarg.
               WHEN "DB-MISC11"                     THEN wdbs._Db-misc1[1] = INTEGER(iarg).
               WHEN "DB-MISC12"                     THEN wdbs._Db-misc1[2] = INTEGER(iarg).
               WHEN "DB-MISC13"                     THEN wdbs._Db-misc1[3] = INTEGER(iarg).
               WHEN "DB-MISC14"                     THEN wdbs._Db-misc1[4] = INTEGER(iarg).
               WHEN "DB-MISC15"                     THEN wdbs._Db-misc1[5] = INTEGER(iarg).
               WHEN "DB-MISC16"                     THEN wdbs._Db-misc1[6] = INTEGER(iarg).
               WHEN "DB-MISC17"                     THEN wdbs._Db-misc1[7] = INTEGER(iarg).
               WHEN "DB-MISC18"                     THEN wdbs._Db-misc1[8] = INTEGER(iarg).
               WHEN "DRIVER-NAME"                   THEN wdbs._Db-misc2[1] = iarg.
               WHEN "DRIVER-VERS"                   THEN wdbs._Db-misc2[2] = iarg.
               WHEN "ESCAPE-CHAR"                   THEN wdbs._Db-misc2[3] = iarg.
               WHEN "DRIVER-CHARS"                  THEN wdbs._Db-misc2[4] = iarg.
               WHEN "DBMS-VERSION"                  THEN wdbs._Db-misc2[5] = iarg.
               WHEN "DSRVR-VERSION"                 THEN wdbs._Db-misc2[6] = iarg.
               WHEN "PROGRESS-VERSION"              THEN wdbs._Db-misc2[7] = iarg.
               WHEN "DSRVR-MISC"                    THEN wdbs._Db-misc2[8] = iarg.
               WHEN "COLLATION-TRANSLATION-VERSION" THEN DO:
                  /* Store the first part of the version e.g., 2.0 in collate[5].
		               The whole version format is m.n-x
		            */
                  ASSIGN
                  ct_version = iarg
                  ix         = INDEX(ct_version, "-").
                  orig_vers = wdbs._Db-collate[5].
                  IF SUBSTR(ct_version, 1, ix - 1, "CHARACTER") >= "6.0" THEN DO:
                     user_env[4] = "y". /* stop the load - db will be corrupted. */
                     ierror = 27.
                  END.
                  ASSIGN
                  ix                             = INDEX(ct_version, ".")
                  wdbs._Db-collate[5]            = raw_val /* to replace ?, if there */
                  PUTBYTE(wdbs._Db-collate[5],1) = 
                  INTEGER(SUBSTR(ct_version, 1, ix - 1, "CHARACTER"))
                  ix                             = ix + 1
                  len                            = INDEX(ct_version, "-") - ix
                  PUTBYTE(wdbs._Db-collate[5],2) = 
                  INTEGER(SUBSTR(ct_version, ix, len, "CHARACTER")).
                  IF orig_vers <> wdbs._Db-collate[5] THEN ct_changed = yes.
               END.
               WHEN     "TRANSLATION-NAME" OR WHEN "CODEPAGE-NAME"  THEN wdbs._Db-xl-name = iarg.
               WHEN "COLLATION-NAME"               THEN  wdbs._Db-coll-name = iarg.
               WHEN "INTERNAL-EXTERNAL-TRAN-TABLE" THEN  RUN Load_Tran_Collate_Tbl ("_Db-xlate", 1).
               WHEN "EXTERNAL-INTERNAL-TRAN-TABLE" THEN  RUN Load_Tran_Collate_Tbl ("_Db-xlate",2).
               WHEN "CASE-INSENSITIVE-SORT"        THEN  RUN Load_Tran_Collate_Tbl ("_Db-collate",1).
               WHEN "CASE-SENSITIVE-SORT"          THEN  RUN Load_Tran_Collate_Tbl ("_Db-collate",2).
               WHEN "UPPERCASE-MAP"                THEN  RUN Load_Tran_Collate_Tbl ("_Db-collate",3).
               WHEN "LOWERCASE-MAP"                THEN     RUN Load_Tran_Collate_Tbl ("_Db-collate",4).
               WHEN "ICU-RULES"                THEN IF SUBSTRING(ct_version,1,2) <> "5." THEN ASSIGN ierror = 4.
               ELSE DO:
                  j = 0.
                  DO i = 29 TO 32:
                     j = j * 256 + GET-BYTE(wdbs._DB-collate[1],  i).
                  END.                                             
                  RUN Load_Icu_Rules ( INPUT-OUTPUT j ) .
                  IF RETURN-VALUE NE "" THEN ierror = 40.
                  ELSE DO:
   	              IF cerror = "no-convert" THEN INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP NO-CONVERT.
   	              ELSE INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP CONVERT SOURCE codepage TARGET SESSION:CHARSET.
                     REPEAT:
                        IMPORT UNFORMATTED ILIN[1].
                        IF TRIM(ILIN[1]) = "END-RULES" THEN LEAVE.
                     END.
                     ILIN[1] = ?.
                  END.
               END.
               OTHERWISE ASSIGN ierror = 4. /* "Unknown &2 keyword" */
            END CASE.
     
         END. /*---------------------------DB-CASE---------------------------------------*/
         ELSE IF iobj = "s" THEN DO: /*-------------s------------------------------*/
            CASE ikwd:
               WHEN "SEQUENCE"       THEN wseq._Seq-Name = iarg.
               WHEN "INITIAL"        THEN DO:
                  /* this is just to catch an integer overflow */
                  IF is-pre-101b-db THEN               ASSIGN wseq._Seq-Init = INT(IARG) NO-ERROR.
                  ELSE                  ASSIGN wseq._Seq-Init = INT64(IARG) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN ASSIGN ierror = 53.
                  ELSE  ierror = 0.
               END.
               WHEN "INCREMENT"      THEN DO: 
                  /* this is just to catch an integer overflow */
                  IF is-pre-101b-db THEN ASSIGN wseq._Seq-Incr = INT(IARG) NO-ERROR.
                  ELSE ASSIGN wseq._Seq-Incr = INT64(IARG) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN ASSIGN ierror = 53.
                  ELSE ierror = 0.
               END.
               WHEN "CYCLE-ON-LIMIT" THEN wseq._Cycle-Ok = (iarg = "yes").
               WHEN "MIN-VAL"        THEN DO: 
                  /* this is just to catch an integer overflow */
                  IF is-pre-101b-db THEN ASSIGN wseq._Seq-Min = INT(IARG) NO-ERROR.
                  ELSE ASSIGN wseq._Seq-Min = INT64(IARG) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN ASSIGN ierror = 53.
                  ELSE ierror = 0.
               END.
               WHEN "MAX-VAL"        THEN DO:
                  /* this is just to catch an integer overflow */
                  IF is-pre-101b-db THEN  ASSIGN wseq._Seq-Max = INT(IARG) NO-ERROR.
                  ELSE ASSIGN wseq._Seq-Max = INT64(IARG) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN ASSIGN ierror = 53.
                  ELSE ierror = 0.
               END.
               WHEN "FOREIGN-NAME"   THEN wseq._Seq-Misc[1] = iarg.
               WHEN "FOREIGN-OWNER"  THEN wseq._Seq-Misc[2] = iarg.
               /* keywords for seq-misc elements 3-8 */ 
               WHEN "SEQ-MISC3"      THEN wseq._Seq-Misc[3] = iarg.
               WHEN "SEQ-MISC4"      THEN wseq._Seq-Misc[4] = iarg.
               WHEN "SEQ-MISC5"      THEN wseq._Seq-Misc[5] = iarg.
               WHEN "SEQ-MISC6"      THEN wseq._Seq-Misc[6] = iarg.
               WHEN "SEQ-MISC7"      THEN wseq._Seq-Misc[7] = iarg.
               WHEN "SEQ-MISC8"      THEN wseq._Seq-Misc[8] = iarg.
               OTHERWISE ierror = 4. /* "Unknown &2 keyword" */
            END CASE.
         END. /*--------------------------------s-----------------------------------*/
         ELSE IF iobj = "t" THEN DO: /*----------t----------------------------------*/ 
            CASE ikwd:
               WHEN    "FILE" OR WHEN "TABLE"          THEN wfil._File-name    = iarg.
               WHEN    "AREA"           THEN DO:
                  ASSIGN iarg = ""
                  j    = 2.
                  _areatloop:
                  DO WHILE TRUE:
                     IF ilin[j] = ? OR ilin[j] = "" THEN DO:
                        ASSIGN iarg = SUBSTRING(iarg, 1, (LENGTH(iarg) - 1))
                        ilin = "".
                        LEAVE _areatloop.
                     END.
                     ELSE ASSIGN iarg = iarg + ilin[j] + " "
                     j = j + 1.
                  END.           
                  FIND _AREA WHERE _Area._Area-name = iarg NO-LOCK NO-ERROR.
                  IF AVAILABLE _Area THEN ASSIGN file-area-number   = _Area._Area-number.
                  ELSE ASSIGN ierror = 31.                
               END.           
               WHEN    "CAN-CREATE" OR WHEN "CAN-INSERT"     THEN wfil._Can-Create   = iarg.
               WHEN    "CAN-READ"   OR WHEN "CAN-SELECT"     THEN wfil._Can-Read     = iarg.
               WHEN    "CAN-WRITE"  OR WHEN "CAN-UPDATE"     THEN wfil._Can-Write    = iarg.
               WHEN    "CAN-DELETE" THEN wfil._Can-Delete   = iarg.
               WHEN    "CAN-DUMP"       THEN wfil._Can-Dump     = iarg.
               WHEN    "CAN-LOAD"       THEN wfil._Can-Load     = iarg.
               WHEN    "TYPE"           THEN wfil._Db-lang      = LOOKUP(iarg,"SQL").
               WHEN    "LABEL"          THEN wfil._File-Label   = iarg.
               WHEN    "LABEL-SA"       THEN wfil._File-Label-SA = iarg. 
               WHEN    "DESCRIPTION"    THEN wfil._Desc         = iarg.
               WHEN    "VALEXP"         THEN wfil._Valexp       = TRIM(iarg).
               WHEN    "VALMSG"         THEN wfil._Valmsg       = iarg.
               WHEN    "VALMSG-SA"      THEN wfil._Valmsg-SA    = iarg.
               WHEN    "FROZEN"         THEN wfil._Frozen       = (iarg = 'yes').
               WHEN    "HIDDEN"         THEN wfil._Hidden       = (iarg = 'yes').
               WHEN    "DUMP-NAME"      THEN DO:
                 /* check that there are no spaces in the dump name */
                  IF INDEX(iarg, " ") > 0 THEN  ASSIGN ierror = 54.
                  ELSE wfil._Dump-name    = iarg.
               END.
               WHEN    "FOREIGN-FLAGS"  THEN wfil._For-Flag     = INTEGER(iarg).
               WHEN    "FOREIGN-FORMAT" THEN wfil._For-Format   = iarg.
               WHEN    "FOREIGN-GLOBAL" THEN wfil._For-Cnt1     = INTEGER(iarg).
               WHEN    "FOREIGN-ID"     THEN wfil._For-Id       = INTEGER(iarg).
               WHEN    "FOREIGN-LEVEL"  THEN wfil._Fil-misc1[4] = INTEGER(iarg).
               WHEN    "FOREIGN-LOCAL"  THEN wfil._For-Cnt2     = INTEGER(iarg).
               WHEN    "FOREIGN-MARK"   THEN wfil._For-Info     = iarg.
               WHEN    "FOREIGN-NAME"   THEN wfil._For-Name     = iarg.
               WHEN    "FOREIGN-NUMBER" THEN wfil._For-number   = INTEGER(iarg).
               WHEN    "FOREIGN-OWNER"  THEN wfil._For-Owner    = iarg.
               WHEN    "FOREIGN-SIZE"   THEN ASSIGN wfil._For-Size     = INTEGER(iarg).
               WHEN    "PROGRESS-RECID" OR WHEN "FILE-MISC11"    THEN wfil._Fil-misc1[1] = INTEGER(iarg).
               WHEN    "FOREIGN-SPAN"   THEN wfil._Fil-misc1[2] = LOOKUP(iarg,"yes").
               WHEN    "FILE-MISC12"    THEN wfil._Fil-misc1[2] = INTEGER(iarg).
               WHEN    "INDEX-FREE-FLD"  OR WHEN "FILE-MISC13"    THEN wfil._Fil-misc1[3] = INTEGER(iarg).
               WHEN    "OVERLOAD-NR" OR WHEN "RECID-COL-NO" OR WHEN "FILE-MISC14"    THEN wfil._Fil-misc1[4] = INTEGER(iarg).
               WHEN    "FILE-MISC15"    THEN wfil._Fil-misc1[5] = INTEGER(iarg).
               WHEN    "FILE-MISC16"    THEN wfil._Fil-misc1[6] = INTEGER(iarg).
               WHEN    "FILE-MISC17"    THEN wfil._Fil-misc1[7] = INTEGER(iarg).
               WHEN    "FILE-MISC18"    THEN wfil._Fil-misc1[8] = INTEGER(iarg).
               WHEN    "FOREIGN-TYPE"   THEN wfil._For-Type     = iarg.
               WHEN    "QUALIFIER" OR WHEN "FILE-MISC21"    THEN wfil._Fil-misc2[1] = iarg. 
               WHEN    "HIDDEN-FLDS" OR WHEN "FILE-MISC22"    THEN wfil._Fil-misc2[2] = iarg.
               WHEN    "RECID-FLD-NAME" OR WHEN "FILE-MISC23"    THEN wfil._Fil-misc2[3] = iarg.
               WHEN    "FLD-NAMES-LIST" OR WHEN "FILE-MISC24"    THEN wfil._Fil-misc2[4] = iarg.
               WHEN    "FILE-MISC25"    THEN wfil._Fil-misc2[5] = iarg.
               WHEN    "FILE-MISC26"    THEN wfil._Fil-misc2[6] = iarg.
               WHEN    "FILE-MISC27"    THEN wfil._Fil-misc2[7] = iarg.
               WHEN    "DB-LINK-NAME" OR WHEN "FILE-MISC28"    THEN wfil._Fil-misc2[8] = iarg.
               WHEN    "FILE-TRIGGER" OR WHEN "TABLE-TRIGGER"  THEN DO:
                  FIND FIRST wfit WHERE wfit._Event = iarg NO-ERROR.
                  IF NOT AVAILABLE wfit THEN CREATE wfit.
                  wfit._Event = ilin[2].
                  CASE ilin[3]:
                     WHEN    "DELETE" OR WHEN "DROP" OR WHEN "REMOVE"      THEN wfit._Proc-Name = "!":u.
                     WHEN    "OVERRIDE"    THEN wfit._Override  = TRUE.
                     WHEN    "NO-OVERRIDE" THEN wfit._Override  = FALSE.
                  END CASE.
                  IF ilin[4] = "PROCEDURE":u THEN wfit._Proc-Name = ilin[5].
                  IF ilin[6] = "CRC" THEN wfit._Trig-CRC = (IF ilin[7] = ? THEN ? ELSE INTEGER(ilin[7])).
                  ilin = ?.
               END.
               OTHERWISE ierror = 4. /* "Unknown &2 keyword" */
            END CASE.  
         END. /*---------------------------t----------------------------------------*/
         ELSE IF iobj = "f" THEN DO: /*---------------f-----------------------------*/
            CASE ikwd:
               WHEN "AS" OR WHEN "TYPE" THEN DO: 
                  ASSIGN wfld._Data-type = (IF iarg = "boolean" THEN "logical"
                  ELSE IF iarg = "dbkey"   THEN "recid" 
                  ELSE iarg).     
               END.                                   
               WHEN    "FIELD"     OR WHEN "COLUMN"      THEN wfld._Field-name = iarg.
               WHEN    "DESC"      OR WHEN "DESCRIPTION" THEN wfld._Desc = iarg.
               WHEN    "INITIAL"   OR WHEN "DEFAULT"     THEN DO:
                 /* check for integer overflow */
                  IF LOOKUP(wfld._Data-Type,"INT,INTEGER") > 0 THEN DO:
                     /* if this is a pre-101b db, just make sure initial
	                     value for an integer is not too big. In theory,
	                     this could not happen since a field needs to be int64
	                     to overflow an integer, and we already prevent int64 from
	                     loading into a pre-10.1B db, but just in case the .df
	                     was manually changed.
	                  */
                     IF is-pre-101b-db THEN DO:
                         /* this is just to catch an integer overflow */
                        ASSIGN ierror = INT(iarg) NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN   ASSIGN ierror = 52.
                        ELSE ierror = 0.
                     END.
                  END.
   
                  wfld._Initial = iarg.
               END.
               WHEN    "CAN-READ"  OR WHEN "CAN-SELECT"  THEN wfld._Can-Read = iarg.
               WHEN    "CAN-WRITE" OR WHEN "CAN-UPDATE"  THEN wfld._Can-Write = iarg.
               WHEN    "NULL" OR WHEN "NULL-ALLOWED" THEN wfld._Mandatory = (iarg = "no").
               WHEN    "SQL-WIDTH" OR WHEN "MAX-WIDTH" OR WHEN "LOB-BYTES" THEN DO:
   	           wfld._Width = INTEGER(iarg).
                  IF LOOKUP(wfld._Data-Type,"CHARACTER,CHAR,DECIMAL,DEC,RAW") > 0 OR  wfld._Extent > 0 THEN.                
               END.
               WHEN "LOB-AREA" THEN DO:
                  /* Area names can have space*/
                  ASSIGN iarg = ""
                  j    = 2.
                  _areafloop:
                  DO WHILE TRUE:
                     IF ilin[j] = ? OR ilin[j] = "" THEN DO:
                        ASSIGN iarg = SUBSTRING(iarg, 1, (LENGTH(iarg) - 1))
                        ilin = "".
                        LEAVE _areafloop.
                     END.
                     ELSE DO: 
                        ASSIGN iarg = iarg + ilin[j] + " "
                        j = j + 1.
                     END.   
                  END.     
                  FIND _AREA WHERE _Area._Area-name = iarg NO-LOCK NO-ERROR.
                  IF AVAILABLE _Area THEN ASSIGN wfld._Fld-stlen   = _Area._Area-number.
                  ELSE ASSIGN ierror = 31. 
               END.
               WHEN    "CLOB-CODEPAGE"          THEN wfld._Charset = iarg.
               WHEN    "CLOB-COLLATION"         THEN wfld._Collation = iarg.
               WHEN    "CLOB-TYPE"              THEN wfld._Attributes1 = INTEGER(iarg).
               WHEN    "FORMAT"                 THEN wfld._Format = iarg.
               WHEN    "FORMAT-SA"              THEN wfld._Format-SA = iarg.
               WHEN    "LABEL"                  THEN wfld._Label = iarg.
               WHEN    "LABEL-SA"               THEN wfld._Label-SA = iarg.
               WHEN    "COLUMN-LABEL"           THEN wfld._Col-label = iarg.
               WHEN    "COLUMN-LABEL-SA"        THEN wfld._Col-label-SA = iarg.
               WHEN    "INITIAL-SA"             THEN wfld._Initial-SA = iarg.
               WHEN    "POSITION"               THEN wfld._Field-rpos = INTEGER(iarg).
               WHEN    "VALEXP"                 THEN wfld._Valexp = TRIM(iarg).
               WHEN    "VALMSG"                 THEN wfld._Valmsg = iarg.
               WHEN    "VALMSG-SA"              THEN wfld._Valmsg-SA = iarg.
               WHEN    "VIEW-AS"                THEN wfld._View-As = iarg.
               WHEN    "HELP"                   THEN wfld._Help = iarg.
               WHEN    "HELP-SA"                THEN wfld._Help-SA = iarg.
               WHEN    "EXTENT"                 THEN wfld._Extent = INTEGER(iarg).
               WHEN    "DECIMALS"               THEN DO:
                  wfld._Decimals      = INTEGER(iarg).
   
                 /*  20041202-001  allow .df to contain DECIMALS ? for any data type 
	                 which is the default value for non-decimal fields anyway
	              */
                 IF wfld._Decimals <> ? AND wfld._Data-type <> "decimal" AND wfld._Data-type <> "dec" THEN.
                    
               END.
               WHEN "LENGTH" OR WHEN "SCALE"           THEN           DO:
                  wfld._Decimals      = INTEGER(iarg).
               END.
               WHEN    "FOREIGN-BITS"           THEN wfld._Decimals      = INTEGER(iarg).
               WHEN    "ORDER"                  THEN wfld._Order         = INTEGER(iarg).
               WHEN    "MANDATORY"              THEN wfld._Mandatory     = (iarg = "yes").
               WHEN    "CASE-SENSITIVE"         THEN wfld._Fld-case      = (iarg = "yes").
               WHEN    "FOREIGN-ALLOCATED"      THEN wfld._For-Allocated = INTEGER(iarg).
               WHEN    "FOREIGN-CODE"           THEN wfld._For-Itype     = INTEGER(iarg).
               WHEN    "FOREIGN-ID"             THEN wfld._For-Id        = INTEGER(iarg).
               WHEN    "FOREIGN-MARK"           THEN . /* unused in V7 */
               WHEN    "FOREIGN-MAXIMUM"        THEN wfld._For-Maxsize   = INTEGER(iarg).
               WHEN    "FOREIGN-NAME"           THEN wfld._For-Name      = iarg.
               WHEN    "FOREIGN-POS"            THEN DO: 
                  ASSIGN   wfld._Fld-stoff     = INTEGER(iarg)
                  l_Fld-stoff         = wfld._Fld-stoff.
               END.  
               WHEN    "FOREIGN-RETRIEVE"       THEN wfld._For-retrieve  = (iarg = "yes").
               WHEN    "FOREIGN-SCALE"          THEN wfld._For-Scale     = INTEGER(iarg).
               WHEN    "FOREIGN-SEP"            THEN wfld._For-Separator = iarg.
               WHEN    "FOREIGN-SIZE"           THEN DO:
                  ASSIGN
                  wfld._Fld-stlen     = INTEGER(iarg)
                  l_Fld-stlen         = wfld._Fld-stlen.
               END.   
               WHEN    "FOREIGN-SPACING"        THEN wfld._For-Spacing   = INTEGER(iarg).
               WHEN    "FOREIGN-TYPE"           THEN DO:
                  IF wfld._Data-type = "character" AND wfld._Initial = ""  AND (iarg = "time" OR iarg = "timestamp") THEN wfld._Initial = ?.
                  wfld._For-Type = iarg.
               END.
               WHEN    "FOREIGN-XPOS"           THEN wfld._For-Xpos      = INTEGER(iarg).
               WHEN    "DSRVR-PRECISION" OR WHEN "FIELD-MISC11"           THEN wfld._Fld-misc1[1]  = INTEGER(iarg).
               WHEN    "DSRVR-SCALE"     OR WHEN "FIELD-MISC12"           THEN wfld._Fld-misc1[2]  = INTEGER(iarg).
               WHEN    "DSRVR-LENGTH"    OR WHEN "FIELD-MISC13"           THEN wfld._Fld-misc1[3]  = INTEGER(iarg).
               WHEN    "DSRVR-FLDMISC"   OR WHEN "FIELD-MISC14"           THEN wfld._Fld-misc1[4]  = INTEGER(iarg).
               WHEN    "DSRVR-SHADOW"    OR WHEN "FIELD-MISC15"           THEN wfld._Fld-misc1[5]  = INTEGER(iarg).
               WHEN    "FIELD-MISC16"           THEN wfld._Fld-misc1[6]  = INTEGER(iarg).
               WHEN    "FIELD-MISC17"           THEN wfld._Fld-misc1[7]  = INTEGER(iarg).
               WHEN    "FIELD-MISC18"           THEN wfld._Fld-misc1[8]  = INTEGER(iarg).
               WHEN    "FIELD-MISC21"  OR       WHEN    "LOB-SIZE"              THEN wfld._Fld-misc2[1]  = iarg.
               WHEN    "SHADOW-COL"    OR WHEN "FIELD-MISC22"           THEN wfld._Fld-misc2[2]  = iarg.
               WHEN    "QUOTED-NAME"   OR WHEN "FIELD-MISC23"           THEN wfld._Fld-misc2[3]  = iarg.
               WHEN    "MISC-PROPERTIES"  OR WHEN "FIELD-MISC24"           THEN wfld._Fld-misc2[4]  = iarg.
               WHEN    "SHADOW-NAME"      OR WHEN "FIELD-MISC25"           THEN wfld._Fld-misc2[5]  = iarg.
               WHEN    "FIELD-MISC26"           THEN wfld._Fld-misc2[6]  = iarg.
               WHEN    "FIELD-MISC27"           THEN wfld._Fld-misc2[7]  = iarg.
               WHEN    "FIELD-MISC28"           THEN wfld._Fld-misc2[8]  = iarg.
               WHEN    "FIELD-TRIGGER"          THEN DO:
                  FIND FIRST wflt WHERE wflt._Event = iarg NO-ERROR.
                  IF NOT AVAILABLE wflt THEN CREATE wflt.
                  wflt._Event = ilin[2].
                  CASE ilin[3]:
                     WHEN "DELETE" OR WHEN "DROP" OR WHEN "REMOVE" THEN  wflt._Proc-Name = "!":u.
                     WHEN "OVERRIDE"    THEN wflt._Override = TRUE.
                     WHEN "NO-OVERRIDE" THEN wflt._Override = FALSE.
                  end case.
                  IF ilin[4] = "PROCEDURE" THEN wflt._Proc-Name = ilin[5].
                  IF ilin[6] = "CRC" THEN wflt._Trig-CRC = (IF ilin[7] = ? THEN ? ELSE INTEGER(ilin[7])).
                  ilin = ?.
               END.
               OTHERWISE ierror = 4. /* "Unknown &2 keyword" */
            END CASE.
         END. /*----------------------------------f---------------------------------*/
         ELSE IF iobj = "i" THEN DO: /*-------------i-------------------------------*/  
            CASE ikwd:
               WHEN    "INDEX" OR WHEN "KEY" THEN widx._Index-Name = iarg.
               WHEN    "UNIQUE"              THEN widx._Unique     = (iarg = "yes").
               WHEN    "INACTIVE"            THEN widx._Active     = (iarg = "no").
               WHEN    "PRIMARY"             THEN iprimary         = TRUE.
               WHEN    "WORD"                THEN widx._Wordidx    = LOOKUP(iarg,"yes").
               WHEN    "INDEX-NUM"           THEN widx._Idx-num    = INTEGER(iarg).
               WHEN    "AREA"                THEN DO:
                  ASSIGN iarg = ""
                  j    = 2.
                  _areailoop:
                  DO WHILE TRUE:
                     IF ilin[j] = ? OR ilin[j] = "" THEN DO:
                        ASSIGN iarg = SUBSTRING(iarg, 1, (LENGTH(iarg) - 1))
                        ilin = "".
                        LEAVE _areailoop.
                     END.
                     ELSE DO:
                        ASSIGN iarg = iarg + ilin[j] + " "
                        j = j + 1.
                     END.   
                  END.           
                  FIND _AREA WHERE _Area._Area-name = iarg NO-LOCK NO-ERROR.
                  IF AVAILABLE _Area THEN  ASSIGN index-area-number   = _Area._Area-number.
                  ELSE  ASSIGN ierror = 31.                
               END.                    
               WHEN    "FOREIGN-LEVEL"       THEN widx._I-misc1[1] = INTEGER(iarg).
               WHEN    "FOREIGN-NAME"        THEN widx._For-name   = iarg.
               WHEN    "FOREIGN-TYPE"        THEN widx._For-type   = iarg.
               WHEN    "RECID-INDEX"         THEN widx._I-misc2[1] = iarg.
               WHEN    "DESC"  OR WHEN "DESCRIPTION"         THEN widx._Desc = iarg.
               WHEN    "INDEX-FIELD" OR WHEN "KEY-FIELD"           THEN DO:
                  IF ilin[2] = ? OR ilin[2] = "" THEN DO:
                     ASSIGN ilin = ?.               
                  END.
                  FIND _Field WHERE _Field._File-recid = drec_file AND _Field._Field-name = ilin[2] NO-ERROR.               
                  IF imod <> "a":u THEN ierror = 12.
                    /* "Cannot add index field to existing index" */
                  IF AVAILABLE _Field THEN DO:
                    /* "Cannot find field to index" */
                     CREATE wixf.
                     ASSIGN
                     icomponent        = icomponent + 1
                     wixf._Index-Seq   = icomponent
                     wixf._Field-recid = RECID(_Field)
                     wixf._Ascending   = TRUE.
                     IF ilin[3] BEGINS "DESC":u OR ilin[4] BEGINS "DESC":u
                     OR ilin[5] BEGINS "DESC":u THEN wixf._Ascending  = FALSE.
                     IF ilin[3] BEGINS "ABBR":u OR ilin[4] BEGINS "ABBR":u
                     OR ilin[5] BEGINS "ABBR":u THEN wixf._Abbreviate = TRUE.
                     IF ilin[3] BEGINS "UNSO":u OR ilin[4] BEGINS "UNSO":u
                     OR ilin[5] BEGINS "UNSO":u THEN wixf._Unsorted   = TRUE.
                  END.   
                  ilin = ?.
               END.
               OTHERWISE ierror = 4. /* "Unknown &2 keyword" */
            END CASE.  
         END. /*------------------------------i-------------------------------------*/
       
         IF ierror > 0 AND ierror <> 50 THEN  ASSIGN stopped = false.
      
      END.  /* end repeat load_loop*/
   END.  /* end stop */
   
   IF stopped THEN .       
   ELSE DO:  /* all but last definition-set executed */
      IF do-commit OR (NOT (ierror > 0 AND user_env[4] BEGINS "y":u)) THEN DO:
         finish: 
         DO:
            ASSIGN stopped = TRUE.
            
            /* Copy any remaining buffer values to the database */
            IF AVAILABLE wdbs AND imod <> ? THEN RUN "ladda\t_lod_dbs.p".                     
            IF AVAILABLE wfil AND imod <> ? THEN RUN "ladda\t_lod_fil.p".         
            IF AVAILABLE wfld AND imod <> ? THEN RUN "ladda\t_lod_fld.p"(INPUT-OUTPUT minimum-index).
            IF AVAILABLE widx AND imod <> ? THEN RUN "ladda\t_lod_idx.p"(INPUT-OUTPUT minimum-index).  
            IF AVAILABLE wseq AND imod <> ? THEN RUN "ladda\t_lod_seq.p".       
            RUN "ladda\t_lodfini.p".
            ASSIGN stopped = false.
         END.   /* finish: */
      END.
   END.     /* all but last definition-set executed */  
   INPUT CLOSE.
   HIDE MESSAGE NO-PAUSE.

   IF TERMINAL <> "" THEN DO:  /* TERMINAL <> "" */
      HIDE FRAME working NO-PAUSE.
      IF do-commit AND xerror THEN DO:      
         ASSIGN do-commit = FALSE.
         MESSAGE "There have been errors encountered in the loading of this df and " SKIP
              "you have selected to commit the transaction anyway. " SKIP(1)
              "Are you sure you want to commit with missing information? " SKIP (1)
         VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE do-commit.
     
         IF NOT do-commit THEN MESSAGE msg2 VIEW-AS ALERT-BOX ERROR.
         ELSE ASSIGN xerror = FALSE.
      END.
      ELSE IF NOT (xerror OR stopped OR xwarn) THEN DO:
         IF CURRENT-WINDOW:MESSAGE-AREA = yes THEN MESSAGE msg1.
         ELSE DO:
            DISPLAY msg1 WITH FRAME working2.
            pause 10.
         END.
      END.
      ELSE IF CURRENT-WINDOW:MESSAGE-AREA = yes THEN DO:
         IF user_env[19] = "" THEN DO:
            IF xerror OR stopped THEN MESSAGE msg2 VIEW-AS ALERT-BOX ERROR.
            IF user_env[6] = "f" OR user_env[6] = "b" THEN MESSAGE msg3 dbload-e msg4 VIEW-AS ALERT-BOX INFORMATION.
         END.
         ELSE DO:
            IF xerror OR STOPPED THEN MESSAGE msg2.
            MESSAGE msg3 dbload-e msg4.
            PAUSE.
         END.
      END.
   END.     /* TERMINAL <> "" */  
   
   SESSION:IMMEDIATE-DISPLAY = no.

END.     /* conversion not needed OR needed and possible */


IF (xerror OR stopped) THEN ASSIGN user_path = "9=h,4=error,_usrload":u.

/* ASSIGN ? to all the _db-records, that have interims-value of
 * SESSION:CHARSET ASSIGNed to _db-xl-name, because they had no
 * codepage-name entry in the .df-file
 */
RETURN.

/*--------------------------------------------------------------------*/


