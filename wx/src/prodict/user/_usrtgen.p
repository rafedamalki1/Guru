/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _usrtgen.p */

/* This program generates CREATE TABLE statements from _File records
stored in the database.  These CREATE TABLE statements can then be
run on another database to define those tables. */

/*
  user_env[2] = output file
  user_env[3] = '' or separate output file for 'create index'
  user_env[4] = allow PROGRESS format statements, also xlat "-" -> "_"
  user_env[5] = statement terminator
  user_env[6] = unique index names
  user_env[7] = support DEFAULT <init value>
  user_env[8] = xlat "-" -> "_"
  user_env[9] = userid
  user_env[11..18] = strings of how to convert PROGRESS datatypes to SQL:
                character -> char, character, vchar, varchar, long char
                date      -> date, datetime
                logical   -> logical
                integer   -> integer, smallint, number
                decimal   -> decimal, number, real, float, double precision
                decimal*  -> decimal, integer, number
                recid     -> recid, rowid
                char**    -> long, character, long varchar
              (decimal* is decimal with _Decimals=0)
              (char** is character longer than 240)
 user_env[19] = character to use for unique name creation
 user_env[21] = Add shadow column for case-insensitive key fields
 user_env[22] = dbtype ("ORACLE", "SYBASE", ...)
 user_env[23] = Min width for character fields.
 user_env[24] = Min width for numeric fields.
 user_env[25] = create sequences (oracle only).
 
History:
    Mario B.    98/12/04    Gray l_cmptbl when Progress is SQL flavor
    hutegger    95/05/03    turned ue8 on for PROGRESS ("-" -> "_")
    hutegger    95/02/07    changed UI and created list of switches for
                            different SQL-flavors and extented the
                            switches set for wrktgen dpending on the
                            SQL-flavor
    hutegger    94/05/06    added parameters user_env[20 ... 25] for
                            _wrktgen.p (based on info from Marceau)
                            
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE allusers  AS LOGICAL   INITIAL TRUE  NO-UNDO.
DEFINE VARIABLE answer    AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE canned    AS LOGICAL   INITIAL TRUE  NO-UNDO.

DEFINE VARIABLE fn        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fot       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE foi       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE ft        AS CHARACTER INITIAL "pro" NO-UNDO.
DEFINE VARIABLE l_cmptbl  AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE l_dbtyp   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_i       AS INTEGER                 NO-UNDO.
DEFINE VARIABLE l_ue-4    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-5    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-6    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-7    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-8    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-11   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-12   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-13   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-14   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-15   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-16   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-17   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-18   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-19   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-20   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-21   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-22   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-23   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-24   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-25   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-28   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE l_ue-29   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE usrnm     AS CHARACTER               NO-UNDO.
DEFINE VARIABLE uidtag    AS CHARACTER   	     NO-UNDO.
DEFINE VARIABLE alltables AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE optlab    AS CHARACTER INITIAL "Output Option:"
			             FORMAT "X(15)"  NO-UNDO.
DEFINE VARIABLE usrlab AS CHARACTER INITIAL "Which Users:"
			             FORMAT "X(15)"  NO-UNDO.
{prodict/misc/filesbtn.i &NAME="btn_File_t"}
{prodict/misc/filesbtn.i &NAME="btn_File_i" &NOACC=yes}

/* LANGUAGE DEPENDENCIES START */ /*---------------------------------------*/

DEFINE VARIABLE new_lang AS CHARACTER EXTENT 7 NO-UNDO INITIAL [
  /* 1*/ "Could not find a user in the connected database with this userid.",
  /* 2*/ "A file named",
  /* 3*/ "already exists.",
  /* 4*/ "Overwrite it?",
  /* 5*/ "All Tables",
  /* 6*/ "You do not have permission to use this option.",
  /* 7*/ "(Enter a Userid or ~"ALL~")"
].


&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
&IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN  
   &GLOBAL-DEFINE LINEUP 45
   &GLOBAL-DEFINE FILLCH 48
&ELSE
   &GLOBAL-DEFINE LINEUP 48
   &GLOBAL-DEFINE FILLCH 50
&ENDIF

FORM 
  SKIP({&TFM_WID})
  "Output File for CREATE TABLE:"  VIEW-AS TEXT       	               AT 2
  SKIP({&VM_WID})

  fot {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 AT 2
  btn_File_t 
  SKIP ({&VM_WIDG})

  "Output File for CREATE INDEX:" VIEW-AS TEXT 	      	               AT 2
  SKIP({&VM_WID})

  foi {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 AT 2
  btn_File_i 
  SKIP ({&VM_WIDG})

  "SQL-Flavor:" VIEW-AS TEXT                                           AT 2
  SKIP({&VM_WID})

  ft AT 2 VIEW-AS RADIO-SET horizontal
	  RADIO-BUTTONS "PROGRESS",      "pro",
	                "ORACLE",        "ora",
	                "SYBASE-10",     "s10",
	                "MS SQL Server", "mss"
  SKIP({&VM_WIDG}) 

  l_cmptbl AT 2 label "Generate PROGRESS-Compatible Schema"
          VIEW-AS toggle-box
          HELP "Create PROGRESS_RECID Column/Index, Shadow-Columns...?"

  SKIP({&VM_WID})

  {prodict/user/userbtns.i}
  WITH FRAME createtable
  NO-LABELS CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " Dump CREATE TABLE for ~"" + fn + "~" ".

FORM 
  SKIP({&TFM_WID})
  "Output File for CREATE TABLE:" VIEW-AS TEXT                         AT 2 
  SKIP({&VM_WID})

  fot {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 AT 2
  btn_File_t 
  SKIP ({&VM_WIDG})

  "Output File for CREATE INDEX:" VIEW-AS TEXT                         AT 2 
  SKIP({&VM_WID})

  foi {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 AT 2
  btn_File_i 
  SKIP ({&VM_WIDG})
   
  usrlab VIEW-AS TEXT                                                  AT 2
  SKIP({&VM_WID})
  allusers AT 2 VIEW-AS RADIO-SET HORIZONTAL 
	RADIO-BUTTONS "All Users",yes,"Single User",no SPACE(0)
  usrnm {&STDPH_FILL} 
  SKIP({&VM_WIDG}) 

  "SQL-Flavor:" VIEW-AS TEXT AT 2 
  SKIP({&VM_WID})
  ft AT 2 VIEW-AS RADIO-SET HORIZONTAL
	  RADIO-BUTTONS "PROGRESS",      "pro",
	                "ORACLE",        "ora",
	                "SYBASE-10",     "s10",
	                "MS SQL Server", "mss"
  SKIP({&VM_WIDG}) 

  l_cmptbl AT 2 label "Generate PROGRESS-Compatible Schema"
          VIEW-AS toggle-box
          HELP "Create PROGRESS_RECID Column/Index, Shadow-Columns...?"

  SKIP({&VM_WID})

  {prodict/user/userbtns.i}

  WITH FRAME createalltables
  NO-LABELS CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " Dump CREATE TABLE for All Tables".

&ELSE
   &GLOBAL-DEFINE LINEUP 30
   &GLOBAL-DEFINE LINEUP2 37
   &GLOBAL-DEFINE FILLCH 34

FORM 
  SKIP({&TFM_WID})
  fot {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 
	LABEL "Output File for CREATE TABLE" COLON {&LINEUP}
  btn_File_t SKIP ({&VM_WIDG})
  foi {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 
	LABEL "Output File for CREATE INDEX" COLON {&LINEUP}
  btn_File_i SKIP ({&VM_WIDG})

  "SQL-Flavor:" VIEW-AS TEXT AT 2 
  SKIP({&VM_WID})
  ft NO-LABEL AT 5 VIEW-AS RADIO-SET HORIZONTAL 
	  RADIO-BUTTONS "PROGRESS",      "pro",
	                "ORACLE",        "ora",
	                "SYBASE-10",     "s10",
	                "MS SQL Server", "mss"
  SKIP({&VM_WIDG}) 

  l_cmptbl label "Generate PROGRESS-Compatible Schema" COLON {&LINEUP2}
          HELP "Create PROGRESS_RECID Column/Index, Shadow-Columns...?"

  SKIP({&VM_WIDG})

  "This program generates a SQL DDL program containing CREATE TABLE statements" 
     	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "equivalent to those originally used to define the table.  It does NOT" 
      	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "generate any GRANT or REVOKE statements to set permissions on the tables." 
      	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "It also generates CREATE INDEX statements in a separate file (these will"
				     VIEW-AS TEXT   AT    2    SKIP
  "be omitted if no file is specified for them above)."
				     VIEW-AS TEXT   AT 2
  {prodict/user/userbtns.i}

  WITH FRAME createtable
  SIDE-LABELS ATTR-SPACE ROW 1 CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " Dump CREATE TABLE for ~"" + fn + "~" ".

FORM 
  SKIP({&TFM_WID})
  fot {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 
	LABEL "Output File for CREATE TABLE" COLON {&LINEUP}
  btn_File_t SKIP ({&VM_WIDG})
  foi {&STDPH_FILL} FORMAT "x(80)" VIEW-AS FILL-IN SIZE {&FILLCH} BY 1 
	LABEL "Output File for CREATE INDEX" COLON {&LINEUP}
  btn_File_i SKIP ({&VM_WIDG})
  usrnm  LABEL "Which Users"                COLON {&LINEUP}
  uidtag FORMAT "x(25)" NO-LABEL 	     AT 53    
  "SQL-Flavor:" VIEW-AS TEXT AT 2 
  SKIP({&VM_WID})
  ft NO-LABEL AT 5 VIEW-AS RADIO-SET HORIZONTAL 
	  RADIO-BUTTONS "PROGRESS",      "pro",
	                "ORACLE",        "ora",
	                "SYBASE-10",     "s10",
	                "MS SQL Server", "mss"
  SKIP({&VM_WIDG})
  l_cmptbl label "Generate PROGRESS-Compatible Schema" COLON {&LINEUP2}
          HELP "Create PROGRESS_RECID Column/Index, Shadow-Columns...?"
  SKIP({&VM_WIDG})

  "This program generates a SQL DDL program containing CREATE TABLE statements" 
     	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "equivalent to those originally used to define the table.  It does NOT" 
      	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "generate any GRANT or REVOKE statements to set permissions on the tables." 
      	       	     	      	    VIEW-AS TEXT   AT    2     SKIP
  "It also generates CREATE INDEX statements in a separate file (these will"
				     VIEW-AS TEXT   AT    2    SKIP
  "be omitted if no file is specified for them above)."
				     VIEW-AS TEXT   AT 2
  {prodict/user/userbtns.i}

  WITH FRAME createalltables
  SIDE-LABELS ATTR-SPACE ROW 1 CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " Dump CREATE TABLE for All Tables".
&ENDIF

/* LANGUAGE DEPENDENCIES END */ /*-----------------------------------------*/


/*===============================Triggers=================================*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
/*----- HELP -----*/
on HELP of frame createtable
   or CHOOSE OF btn_Help in frame createtable
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
      	       	     	     INPUT {&Dump_Create_Table_Dlg_Box},
      	       	     	     INPUT ?).
on HELP of frame createalltables
   or CHOOSE OF btn_Help in frame createalltables
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
      	       	     	     INPUT {&Dump_Create_Table_All_Dlg_Box},
      	       	     	     INPUT ?).
&ENDIF


ON GO OF FRAME createtable
DO:
  DEFINE VAR f1  AS CHAR NO-UNDO.
  DEFINE VAR f2  AS CHAR NO-UNDO.
  DEFINE VAR uid AS CHAR NO-UNDO.

  f1 = fot:SCREEN-VALUE IN FRAME createtable.
  f2 = foi:SCREEN-VALUE IN FRAME createtable.

  IF SEARCH(f1) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[2] f1 new_lang[3] SKIP new_lang[4]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO fot IN FRAME createtable.
      RETURN NO-APPLY.
    END.
  END.
  IF SEARCH(f2) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[2] f2 new_lang[3] SKIP new_lang[4]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO foi IN FRAME createtable.
      RETURN NO-APPLY.
    END.
  END.
END.
ON GO OF FRAME createalltables
DO:
  DEFINE VAR f1  AS CHAR NO-UNDO.
  DEFINE VAR f2  AS CHAR NO-UNDO.
  DEFINE VAR uid AS CHAR NO-UNDO.

  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
    IF allusers:SCREEN-VALUE = "yes" THEN
	  uid = "ALL".
    ELSE DO:
	  uid = usrnm:SCREEN-VALUE.
	  usrnm = uid.
    END.
  &ELSE
    uid  = usrnm:SCREEN-VALUE.
  &ENDIF

  IF uid <> "ALL" AND NOT CAN-FIND(_User WHERE _User._Userid = uid) THEN DO:
    MESSAGE new_lang[1] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "ENTRY" TO usrnm IN FRAME createalltables.
    RETURN NO-APPLY.
  END.


  f1 = fot:SCREEN-VALUE IN FRAME createalltables.
  f2 = foi:SCREEN-VALUE IN FRAME createalltables.

  IF SEARCH(f1) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[2] f1 new_lang[3] SKIP new_lang[4]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO fot IN FRAME createalltables.
      RETURN NO-APPLY.
    END.
  END.
  IF SEARCH(f2) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[2] f2 new_lang[3] SKIP new_lang[4]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO foi IN FRAME createalltables.
      RETURN NO-APPLY.
    END.
  END.
END.
ON CHOOSE OF btn_File_t in frame createtable DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT fot:handle in frame createtable /*Fillin*/,
        INPUT "Find Output File"  /*Title*/,
        INPUT ""                 /*Filter*/,
        INPUT no                /*Must exist*/).
END.

ON LEAVE OF fot in frame createtable
   fot:screen-value in frame createtable = 
        TRIM(fot:screen-value in frame createtable).

ON CHOOSE OF btn_File_i in frame createtable DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT foi:handle in frame createtable /*Fillin*/,
        INPUT "Find Output File"  /*Title*/,
        INPUT ""                 /*Filter*/,
        INPUT no                /*Must exist*/).
END.

ON LEAVE OF foi in frame createtable
   foi:screen-value in frame createtable = 
        TRIM(foi:screen-value in frame createtable).

ON VALUE-CHANGED OF ft IN FRAME createtable DO:
   CASE SELF:SCREEN-VALUE:
      WHEN "pro" THEN
      ASSIGN
         l_cmptbl:SENSITIVE = FALSE
	 l_cmptbl:SCREEN-VALUE = "no".
      OTHERWISE
         DO:
            l_cmptbl:SENSITIVE = TRUE.
	 END.
   END CASE.
END.

ON ENTRY OF FRAME createtable DO:
   /* Just assigning l_cmptbl:SENSITIVE = FALSE doesn't work because the *
    * widget is not realized until the UPDATE statements are executed.   *
    * Once that happens, this is the only way to force the assignment.   */
   APPLY "VALUE-CHANGED" TO ft IN FRAME createtable.
END.

ON WINDOW-CLOSE OF FRAME createtable
   APPLY "END-ERROR" TO FRAME createtable.

ON CHOOSE OF btn_File_t in frame createalltables DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT fot:handle in frame createalltables /*Fillin*/,
        INPUT "Find Output File"  /*Title*/,
        INPUT ""                 /*Filter*/,
        INPUT no                /*Must exist*/).
END.

ON LEAVE OF fot in frame createalltables
   fot:screen-value in frame createalltables = 
        TRIM(fot:screen-value in frame createalltables).

ON CHOOSE OF btn_File_i in frame createalltables DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT foi:handle in frame createalltables /*Fillin*/,
        INPUT "Find Output File"  /*Title*/,
        INPUT ""                 /*Filter*/,
        INPUT no                /*Must exist*/).
END.

ON LEAVE OF foi in frame createalltables
   foi:screen-value in frame createalltables = 
        TRIM(foi:screen-value in frame createalltables).

ON VALUE-CHANGED OF ft IN FRAME createalltables DO:
   CASE SELF:SCREEN-VALUE:
      WHEN "pro" THEN
      ASSIGN
         l_cmptbl:SENSITIVE = FALSE
	 l_cmptbl:SCREEN-VALUE = "no".
      OTHERWISE
         DO:
            l_cmptbl:SENSITIVE = TRUE.
	 END.
   END CASE.
END.

ON ENTRY OF FRAME createalltables DO:
   /* Just assigning l_cmptbl:SENSITIVE = FALSE doesn't work because the *
    * widget is not realized until the UPDATE statements are executed.   *
    * Once that happens, this is the only way to force the assignment.   */
   APPLY "VALUE-CHANGED" TO ft IN FRAME createalltables.
END.

ON WINDOW-CLOSE OF FRAME createalltables
   APPLY "END-ERROR" TO FRAME createalltables.

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
ON VALUE-CHANGED OF allusers IN FRAME createalltables DO:
   DEFINE VARIABLE dummyl AS LOGICAL NO-UNDO.

   IF allusers:SCREEN-VALUE = "yes" THEN DO:
	usrnm:SENSITIVE = FALSE.
	usrnm:SCREEN-VALUE = "".
   END.
   ELSE DO:
        usrnm = "".
	usrnm:SENSITIVE = TRUE.
	dummyl = usrnm:MOVE-AFTER-TAB-ITEM(allusers:HANDLE).
	APPLY "ENTRY" TO usrnm.
   END.
END.
&ENDIF

/*========================= Initializations ============================*/

FIND DICTDB._File "_File".
answer = CAN-DO(_Can-read,USERID("DICTDB")).
FIND DICTDB._File "_Field".
answer = answer AND CAN-DO(_Can-read,USERID("DICTDB")).
FIND DICTDB._File "_Index".
answer = answer AND CAN-DO(_Can-read,USERID("DICTDB")).
IF NOT answer THEN DO:
  MESSAGE new_lang[6] VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  user_path = "".
  RETURN.
END.

RELEASE DICTDB._File.
assign alltables = (IF user_env[1] = "ALL" THEN yes ELSE no).

IF NOT alltables THEN
  FIND DICTDB._File 
    WHERE DICTDB._File._Db-recid = drec_db 
    AND DICTDB._File._File-name = user_env[1].


ASSIGN
  answer = TRUE
  fot = (IF alltables THEN user_dbname ELSE DICTDB._File._File-name)
      + ".p"
  foi = (IF alltables THEN user_dbname ELSE DICTDB._File._File-name)
      + ".i"
  fn  = (IF alltables THEN new_lang[5] ELSE DICTDB._File._File-name)
  usrnm  = (IF alltables THEN "ALL" ELSE ENTRY(1,_File._Can-read))
  ft  = ( if lookup(user_env[22],"pro,ora,mss,s10") = 0
            then "pro"
            else user_env[22]
        )
  .

/* Adjust the graphical rectangle and the ok and cancel buttons */
IF alltables THEN DO:
  {adecomm/okrun.i  
    &FRAME  = "FRAME createalltables" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
    {&HLP_BTN}
  }
END.
ELSE DO:
  {adecomm/okrun.i  
    &FRAME  = "FRAME createtable" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
    {&HLP_BTN}
  }
END.
PAUSE 0.

/* here is the matrix of parameters per db-type */
assign
  l_dbtyp = "pro,ora,s10,mss"
  l_ue-4  = "y,n,n,n"             /* allow PROGRESS format statements */
  l_ue-5  = ".,;,go,go"                       /* statement terminator */
  l_ue-6  = "n,y,y,y"                          /* Unique index names? */
  l_ue-7  = "y,n,n,n"                         /* default <init value> */
  l_ue-8  = "y,y,y,y"                              /* xlat "-" -> "_" */
  l_ue-11 = "character,varchar2,varchar,varchar"         /* character */
  l_ue-12 = "date,date,datetime,datetime"                /*           */
  l_ue-13 = "logical,number,tinyint,tinyint"             /* logical   */
  l_ue-14 = "integer,number,integer,integer"             /* integer   */
  l_ue-15 = "decimal,number,decimal(18,5),decimal(18,5)" /* decimal   */
  l_ue-16 = "decimal,number,decimal,decimal"   /* decimal(_decimal=0) */
  l_ue-17 = "recid,number,integer,integer"               /* recid     */
  l_ue-18 = "character,long,text,text"               /* char > x(240) */
  l_ue-19 = "logical,number,tinyint,tinyint"     /* logical(idx-comp) */
  l_ue-20 = ",#,#,#"          /* char for arrays; e.g. mnth-sales##12 */
  l_ue-21 = "n,y,y,y"            /* no shadowing for case-sensitivity */
  l_ue-22 = "PROGRESS,ORACLE,SYBASE-10,MS SQL Server"              /* */
  l_ue-23 = "8,30,30,30"        /* minimum width for character-fields */
  l_ue-24 = "7,15,15,15"          /* minimum width for numeric-fields */
  l_ue-25 = "n,y,y,y"                          /* sequences supported */
  l_ue-28 = "32,30,30,30"                     /* max-length(idx-name) */
  l_ue-29 = "32,26,24,24"                      /* max-length(id-name) */
  .
/*========================== Mainline code =============================*/

IF alltables THEN 
 DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN  
    uidtag = new_lang[7].
    DISPLAY uidtag WITH FRAME createalltables.
    usrnm = "ALL".
  
    UPDATE
      fot btn_File_t
      foi btn_File_i
      usrnm
      ft
      l_cmptbl
      btn_OK 
      btn_Cancel
      {&HLP_BTN_NAME}
      WITH FRAME createalltables.
  &ELSE
    DISPLAY usrlab WITH FRAME createalltables.
    UPDATE
      fot btn_File_t
      foi btn_File_i
      allusers
      ft
      l_cmptbl
      btn_OK 
      btn_Cancel
      {&HLP_BTN_NAME}
      WITH FRAME createalltables.
    IF allusers THEN usrnm = "ALL".
  &ENDIF

  /* if cr tab and cr idx both going to same file, wipe out idx fil name */
  IF foi = fot AND (OPSYS <> "UNIX" OR ENCODE(foi) = ENCODE(fot)) THEN foi = "".

  ASSIGN
    l_i          = lookup(ft,l_dbtyp)
    user_env[ 2] = fot  /* output file */
    user_env[ 3] = foi  /* '' or separate output file for 'create index' */
    user_env[ 4] = entry(l_i,l_ue-4)
    user_env[ 5] = entry(l_i,l_ue-5)
    user_env[ 6] = entry(l_i,l_ue-6)
    user_env[ 7] = entry(l_i,l_ue-7)
    user_env[ 8] = entry(l_i,l_ue-8)
    user_env[ 9] = usrnm             /* userid */
    user_env[11] = entry(l_i,l_ue-11)
    user_env[12] = entry(l_i,l_ue-12)
    user_env[13] = entry(l_i,l_ue-13)
    user_env[14] = entry(l_i,l_ue-14)
    user_env[15] = entry(l_i,l_ue-15)
    user_env[16] = entry(l_i,l_ue-16)
    user_env[17] = entry(l_i,l_ue-17)
    user_env[18] = entry(l_i,l_ue-18)
    user_env[19] = entry(l_i,l_ue-19)
    user_env[20] = entry(l_i,l_ue-20)
    user_env[21] = entry(l_i,l_ue-21)
    user_env[22] = entry(l_i,l_ue-22)
    user_env[23] = entry(l_i,l_ue-23)
    user_env[24] = entry(l_i,l_ue-24)
    user_env[25] = entry(l_i,l_ue-25)
    user_env[27] = string(l_cmptbl,"y/n")
    user_env[28] = entry(l_i,l_ue-28)
    user_env[29] = entry(l_i,l_ue-29)
    user_env[31] = "-- **"           /*comment-character*/
    .

  RUN "prodict/misc/_wrktgen.p".
  canned = FALSE.
END.

ELSE /*Single table*/
 DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN  
   UPDATE
    fot btn_File_t
    foi btn_File_i
    ft
    l_cmptbl
    btn_OK 
    btn_Cancel
    {&HLP_BTN_NAME}
    WITH FRAME createtable.
  &ELSE
/*   DISPLAY optlab with frame createtable.*/
   UPDATE
    fot btn_File_t
    foi btn_File_i
    ft
    l_cmptbl
    btn_OK 
    btn_Cancel
    {&HLP_BTN_NAME}
    WITH FRAME createtable.
  &ENDIF

  /* if cr tab and cr idx both going to same file, wipe out idx fil name */
  IF foi = fot AND (OPSYS <> "UNIX" OR ENCODE(foi) = ENCODE(fot)) THEN foi = "".

  ASSIGN
    l_i          = lookup(ft,l_dbtyp)
    user_env[ 2] = fot  /* output file */
    user_env[ 3] = foi  /* '' or separate output file for 'create index' */
    user_env[ 4] = entry(l_i,l_ue-4)
    user_env[ 5] = entry(l_i,l_ue-5)
    user_env[ 6] = entry(l_i,l_ue-6)
    user_env[ 7] = entry(l_i,l_ue-7)
    user_env[ 8] = entry(l_i,l_ue-8)
    user_env[ 9] = "ALL"      	     /* userid */
    user_env[11] = entry(l_i,l_ue-11)
    user_env[12] = entry(l_i,l_ue-12)
    user_env[13] = entry(l_i,l_ue-13)
    user_env[14] = entry(l_i,l_ue-14)
    user_env[15] = entry(l_i,l_ue-15)
    user_env[16] = entry(l_i,l_ue-16)
    user_env[17] = entry(l_i,l_ue-17)
    user_env[18] = entry(l_i,l_ue-18)
    user_env[19] = entry(l_i,l_ue-19)
    user_env[20] = entry(l_i,l_ue-20)
    user_env[21] = entry(l_i,l_ue-21)
    user_env[22] = entry(l_i,l_ue-22)
    user_env[23] = entry(l_i,l_ue-23)
    user_env[24] = entry(l_i,l_ue-24)
    user_env[25] = entry(l_i,l_ue-25)
    user_env[27] = string(l_cmptbl,"y/n")
    user_env[28] = entry(l_i,l_ue-28)
    user_env[29] = entry(l_i,l_ue-29)
    user_env[31] = "-- **"           /*comment-character*/
    .
  
  RUN "prodict/misc/_wrktgen.p".
  canned = FALSE.
END.

HIDE FRAME createtable NO-PAUSE.
IF canned THEN
  user_path = "".

RETURN.
