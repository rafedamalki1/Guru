/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _as4schg.p - change _Db record information for as400 dataserver 
   Created from _usrschg.p which is used by other dataservers.
   D. McMann 03/02/95  
   
   History:  06/19/96 Added support for logical name D. McMann   
             09/12/96 Added assign of lname if left blank D. McMann
   
*/

/*
in:  user_env[1] = "add" or "upd"
     user_env[3] = dbtype to add or "" for any AS400    
     user_env[35] = error on connect will equal redo.
 
out: user_env[2] = new _Db._Db-name
     no other environment variables changed
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

DEFINE VARIABLE amode    AS LOGICAL              NO-UNDO. /*true=add,false=modify*/
DEFINE VARIABLE c        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE codepage AS CHARACTER            NO-UNDO format "x(40)".
DEFINE VARIABLE dblst    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE f_comm   AS CHARACTER            NO-UNDO.
DEFINE VARIABLE fmode    AS LOGICAL              NO-UNDO. /* part of gateway */
DEFINE VARIABLE i        AS INTEGER  INITIAL 0   NO-UNDO.
DEFINE VARIABLE j        AS INTEGER              NO-UNDO.
DEFINE VARIABLE okay     AS LOGICAL  INIT FALSE  NO-UNDO.
DEFINE VARIABLE ronly    AS LOGICAL              NO-UNDO. /* read only */
DEFINE VARIABLE x-l      AS LOGICAL              NO-UNDO. /* allow set ldb name */
DEFINE VARIABLE canned   AS LOGICAL  INIT TRUE   NO-UNDO.
DEFINE VARIABLE dname    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE ttl      AS CHARACTER            NO-UNDO.
DEFINE VARIABLE lname    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE pname    AS CHARACTER            NO-UNDO.

/* LANGUAGE DEP  END.ENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 3 NO-UNDO INITIAL [
  /* 1*/ "There is currently a database using this name as a dbname or alias",
  /* 2*/ "Internal Dictionary Error: inconsistent dbtype encountered.",
  /* 3*/ "Database Library Name may not be left blank or unknown."
].


FORM
  SKIP ({&TFM_WID})
  pname  FORMAT "x(10)" COLON 24 LABEL "Dictionary Library Name" 
      {&STDPH_FILL}  SKIP ({&VM_WID})
  lname        FORMAT "x(10)" COLON 24 LABEL "Logical Database Name"
      {&STDPH_FILL}  SKIP ({&VM_WID})
  _Db._Db-type  FORMAT "x(10)" COLON 24 LABEL "Database Type" 
      {&STDPH_FILL}  SKIP ({&VM_WID})
  codepage            COLON 24 LABEL "Code-Page" 
      {&STDPH_FILL}  SKIP  ({&TFM_WID})
 
  "Connection Parameters:" AT 2 VIEW-AS TEXT SKIP({&VM_WID})
  f_comm                   AT 2 NO-LABEL {&STDPH_EDITOR}
      VIEW-AS EDITOR 
      &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      	 SIZE 65 BY 4 BUFFER-LINES 4
      &ELSE 
      	 SIZE 65 BY 3 SCROLLBAR-VERTICAL
      &ENDIF
  SKIP({&VM_WIDG})

  {prodict/user/userbtns.i}
  WITH FRAME userschg ROW 1 CENTERED SIDE-LABELS
      DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
      VIEW-AS DIALOG-BOX     TITLE ttl. 
      

/*================================Triggers=================================*/

/*----- LEAVE of DATABASE LIBRARY NAME -----*/
ON LEAVE OF pname IN FRAME userschg DO:

    Define variable btn_ok   as logical initial true.
  
    /* If logical name was edited and name is in use: */
    dname = TRIM(INPUT FRAME userschg pname).
    IF pname ENTERED 
      THEN DO:  /* pname ENTERED */
        IF LDBNAME(dname) <> ?
          THEN DO:
            MESSAGE new_lang[1] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
        END.
     END.    /* pname ENTERED */
END.

ON LEAVE OF lname IN FRAME userschg DO:

    Define variable btn_ok   as logical initial true.
    
    ASSIGN dname = TRIM(INPUT FRAME userschg lname).
    /* If logical name was edited and name is in use: */
    IF lname ENTERED THEN DO: 
       IF dname = "" or dname = ? THEN 
          ASSIGN lname = INPUT FRAME userschg pname
                 lname:SCREEN-VALUE IN FRAME userschg = lname.
       ASSIGN dname = TRIM(INPUT FRAME userschg lname).          
       IF LDBNAME(dname) <> ? THEN DO:
         MESSAGE new_lang[1] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN NO-APPLY.
       END.    /* lname ENTERED */    
       ELSE IF user_env[1] = "chg" THEN DO:    
         IF NOT CONNECTED(_Db._Db-name) THEN DO:
           message "Changing the logical database name causes"  skip
                    "the Data Administration tool to close."     skip
                    "You can then continue working with the "    skip
                    "tool by restarting it."                     skip(1)
                    "Do you want to make the change?"
                    view-as alert-box QUESTION buttons yes-no 
                    update btn_ok.
            if btn_ok 
              THEN ASSIGN user_path = "*E"
                          okay      = FALSE.
           END.               
           ELSE DO:
              message "Changing the logical database name causes"  skip
                      "the DB2/400 database to be disconnected and" SKIP
                      "reconnected using the new logical name." SKIP (1)                      
                      "Do you want to make the change?"
              view-as alert-box QUESTION buttons yes-no 
              update btn_ok.
              IF btn_ok 
                THEN ASSIGN user_path = "*C,_as4crcn"
                            okay      = TRUE.                          
              ELSE 
                ASSIGN okay = false
                       lname:SCREEN-VALUE IN FRAME userschg = _db._Db-name
                       lname = _Db._Db-name.    
           END.            
       END.    
    END.
    /* lname blank */ 

    ELSE IF dname = "" or dname = ? THEN 
        ASSIGN lname = INPUT FRAME userschg pname
               lname:SCREEN-VALUE IN FRAME userschg = lname.
                  
END.                  

/*----- LEAVE of code-page -----*/
{prodict/gate/gat_cpvl.i
  &frame    = "userschg"
  &variable = "codepage"
  &adbtype  = "user_env[3]" 
  }  /* checks if codepage contains convertable code-page */

/*----- GO or OK -----*/
ON GO OF FRAME userschg DO:
    dname = TRIM(INPUT FRAME userschg pname).
    IF dname = "" OR dname = ? THEN DO:
        MESSAGE new_lang[3] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY" TO pname IN FRAME userschg.
        RETURN NO-APPLY.
     END.
     IF INPUT FRAME userschg lname = "" or INPUT FRAME userschg lname = ? THEN
        ASSIGN lname = INPUT FRAME userschg pname
               lname:SCREEN-VALUE IN FRAME userschg = lname.
END.

/*-----WINDOW-CLOSE-----*/
ON WINDOW-CLOSE OF FRAME userschg
    APPLY "END-ERROR" TO FRAME userschg.


/*----- HELP -----*/
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
ON HELP OF FRAME userschg OR CHOOSE of btn_Help IN FRAME userschg DO:
    RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
                              INPUT {&Create_DataServer_Schema_Dlg_Box},
      	       	     	      INPUT ?).
    END.
&ENDIF

/*============================Mainline Code===============================*/

 IF  user_env[1] = "add"  OR user_env[1] = "redo" THEN 
                    ttl =  "Create DB2/400 DataServer Schema".
         ELSE  
                   ttl =   "Edit DB2/400 Connection Information".

ASSIGN
  amode    = (user_env[1] = "add")
  fmode    = (user_env[3] <> "")
  dblst    = (IF fmode THEN user_env[3] ELSE SUBSTRING(GATEWAYS,10)).
             /* 10 = LENGTH("PROGRESS") + 2 */

IF NOT amode
  THEN DO:
    FIND _Db WHERE RECID(_Db) = drec_db NO-ERROR.
    IF NOT _Db._Db-slave THEN i = 1. /* no _Db rec */
    IF fmode AND dblst <> user_dbtype THEN i = 2. /* inconsistent dbtype */
    END.

if user_env[1] = "add"
  then do:
    { prodict/dictgate.i &action=query &dbtype=dblst &dbrec=? &output=codepage }
    assign codepage = ENTRY(11,codepage).
 END.
 else assign
    codepage = _DB._Db-xl-name.

IF i > 0 THEN DO:
  MESSAGE new_lang[i] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
  END.

ASSIGN
  f_comm = (IF AVAILABLE _Db THEN _Db._Db-comm ELSE "") .
  
  IF amode or user_env[1] = "redo" THEN
      ASSIGN x-l   = yes.
  ELSE x-l = no.
  
{adecomm/okrun.i  
  &FRAME  = "FRAME userschg" 
  &BOX    = "rect_Btns"
  &OK     = "btn_OK" 
  {&CAN_BTN}
  {&HLP_BTN}
}
  
DO TRANSACTION:
  IF AVAILABLE _Db AND (_Db._Db-addr = ? OR _Db._Db-addr = "") THEN 
    ASSIGN _Db._Db-addr = _Db._Db-name.
END.
  
ASSIGN f_comm:RETURN-INSERT in frame userschg = yes
       pname = (IF AVAILABLE _Db THEN _Db._Db-addr ELSE "")
       lname = (IF AVAILABLE _Db THEN _Db._Db-name ELSE "").
       

DISPLAY
  pname
  lname
  _Db._Db-type WHEN AVAILABLE _Db
  codepage
  f_comm
  WITH FRAME userschg.
IF INDEX(dblst,",") = 0 THEN DISPLAY dblst @ _Db._Db-type WITH FRAME userschg.

 _trx: 
 DO TRANSACTION WITH FRAME userschg:
     
    DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
      PROMPT-FOR
        pname WHEN x-l  
        lname 
        codepage     WHEN amode  or x-l
        f_comm
        btn_OK
        btn_Cancel
        {&HLP_BTN_NAME}.
      canned = false.
      END.

    IF canned THEN UNDO _trx,LEAVE _trx.

    IF amode THEN DO: /* create a new _Db for a schema for a Non-PROGRESS db 
                               _Db-misc1[08] is assigned to 7 because client needs to
                               know that is a version 7 schema holder for the AS400 
                               database.  */
        CREATE _Db.
        ASSIGN
            _Db._Db-slave = TRUE
            _Db._Db-type  = CAPS(INPUT _Db._Db-type)             
            user_dbtype   = _Db._Db-type.
        IF NOT fmode THEN 
          &IF "{&WINDOW-SYSTEM}" = "TTY"
            &THEN
          	 user_path = "1=sys,_usrsget".
            &ELSE
          	 user_path = "1=sys,_guisget".
            &ENDIF
        { prodict/dictgate.i &action=query &dbtype=_Db._Db-type &dbrec=? &output=c }

        IF INDEX(ENTRY(1,c),"a") > 0 THEN DO:
            { prodict/dictgate.i &action=add
              &dbtype=_Db._Db-type &dbrec=RECID(_Db) &output=c }
        END.
     END.   /* create a new _Db for a schema for a Non-PROGRESS db */
    
    IF okay THEN 
        DISCONNECT VALUE(_DB._Db-name).
           
    ASSIGN
      _Db._Db-name    = CAPS(INPUT lname)
      user_env[2]     = _Db._Db-name
      _Db._Db-addr    = CAPS(INPUT pname)
      _Db._Db-comm    = TRIM(INPUT f_comm)
      /* Remove any line feeds (which we get on WINDOWS) */
      _Db._Db-comm    = REPLACE(_Db._Db-comm, CHR(13), "")
      user_dbname     = _Db._Db-name
      .             
          
    { prodict/gate/gat_cp1a.i &incpname = "codepage" }
    /* This is assigned here so the client knows that this is a version 7 database
          being created.  It will be assigned again in prodict/as4/_as4crcn since the
          load program overwrites.  */
          
     IF x-l  THEN ASSIGN _db._Db-misc1[8] = 7.
     IF okay THEN DO:
        ASSIGN user_dbname = _Db._Db-name
               user_env[1] = "connect".
     END.                      
 END.   /* _trx: do transaction */

IF canned  AND user_env[1] = "redo" THEN ASSIGN user_path = "_as4_del,_usrsget".   
ELSE IF canned THEN ASSIGN user_path = "".

 RELEASE _Db.   /* I'm not sure why we need this? (los) */

HIDE FRAME userschg NO-PAUSE.      
RETURN.


