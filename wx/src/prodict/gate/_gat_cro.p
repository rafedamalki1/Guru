/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

file: prodict/gate/_gat_cro.p

Description:
    creates PROGRESS object according to the foreign info contained
    in the temp-tables, ev. deleting the object first, if it already
    exists, while retaining the PROGRESS-only info of the object to
    use if when re-creating it
    
    <DS>_get.p gets a list of all pullable objects from the foreign DB
    <DS>_pul.p pulled over the definition from the foreign side
    gat_cmp.p  compared the existing definitions with the pulled info
    gat_cro.p  replaces the existing definitions with the pulled info
               or creates the new object if it didn't already exist

    Create <DS> Schema: <DS>_get.p <DS>_pul.p gat_cro.p
    Update <DS> Schema: <DS>_get.p <DS>_pul.p gat_cro.p
    Verify <DS> Schema: <DS>_get.p <DS>_pul.p gat_cmp.p gat_cro.p

Input:
    gate-work   contains names of all objects to be created/updated
    s_ttb_tbl   Table information from foreign schema
    s_ttb_fld   Field information from foreign schema
    s_ttb_idx   Index information from foreign schema
    s_ttb_idf   Index-Field information from foreign schema

Output:
    gate-work   contains names of all objects to be created/updated
                gate-edit contains report of verify
                gate-flag = YES, if differences found
                gate-flag = NO,  if object doesn't exist anymore
                    on foreign side, or definitions don't differ

Unchanged:
    s_ttb_tbl   Table information from foreign schema
    s_ttb_fld   Field information from foreign schema
    s_ttb_idx   Index information from foreign schema
    s_ttb_idf   Index-Field information from foreign schema

History:
    hutegger    03/95   creation (derived from ds_vrf.p)

--------------------------------------------------------------------*/


/*    &DS_DEBUG   DEBUG to protocol the creation    */
/*                ""    to turn off protocol        */
&SCOPED-DEFINE xxDS_DEBUG                   DEBUG

&SCOPED-DEFINE DATASERVER                 YES
&SCOPED-DEFINE FOREIGN_SCHEMA_TEMP_TABLES INCLUDE
{ prodict/dictvar.i }
{ prodict/user/uservar.i }
&UNDEFINE DATASERVER
&UNDEFINE FOREIGN_SCHEMA_TEMP_TABLES

define variable batch_mode	 as logical.
define variable edbtyp           as character no-undo. /* db-type external format */
define variable has_id_ix        as logical   no-undo.
define variable i                as integer   no-undo.
define variable indn             as integer   no-undo.
define variable l_char-types     as character no-undo.
define variable l_chda-types     as character no-undo.
define variable l_date-types     as character no-undo.
define variable l_dcml-types     as character no-undo.
define variable l_deil-types     as character no-undo.
define variable l_dein-types     as character no-undo.
define variable l_ds_recid       as integer   no-undo.
define variable l_extnt-char     as character no-undo initial "##".
define variable l_intg-types     as character no-undo.
define variable l_logi-types     as character no-undo.
define variable l_order-max      as integer   no-undo.
define variable l_pro_recid      as integer   no-undo.
define variable msg              as character no-undo   EXTENT 6.
define variable odbtyp           as character no-undo. /* ODBC db-types */
define variable oldf  	         as logical   no-undo. /* old file definition */
define variable scrap            as logical   no-undo.

/*------------------------------------------------------------------*/
/* These variables/workfile are so we can save the Progress-only
   portion of the DataServer definitions when rebuilding. */
   
define variable idx_Desc         as character no-undo. /*_Desc   */
define variable tab_Can-Crea     as character no-undo. /*_Can-Create */
define variable tab_Can-Dele     as character no-undo. /*_Can-Delete */
define variable tab_Can-Read     as character no-undo. /*_Can-Read */
define variable tab_Can-Writ     as character no-undo. /*_Can-Write */
define variable tab_Desc         as character no-undo. /*_Desc        */
define variable tab_Dump-nam     as character no-undo. /*_Dump-name */
define variable tab_Hidden       as logical   no-undo. /*_Hidden */
define variable tab_PIForNum     as integer   no-undo. /*_Prime-Index */
define variable tab_PrimeIdx     as character no-undo. /*_Prime-Index */
define variable tab_recidNew     as character no-undo. /*_Recid-Index */
define variable tab_recidOld     as character no-undo. /*_Recid-Index */
define variable tab_RIfstoff     as integer   no-undo. /*_Recid-Index */
define variable tab_RIidx#       as integer   no-undo. /*_Recid-Index */
define variable tab_RILevel      as integer   no-undo. /*_Recid-Index */
define variable tab_RImsc23      as character no-undo. /*_Recid-Index */
define variable tab_Valexp       as character no-undo. /*_Valexp */
define variable tab_Valmsg       as character no-undo. /*_Valmsg */

define TEMP-TABLE w_field no-undo
            field ds_name             as character case-sensitive
            field ds_type             as character
            field pro_Can-Read        as character
            field pro_Can-Writ        as character
            field pro_Col-lbl         as character
            field pro_Decimals        as integer
            field pro_Desc            as character
            field pro_Fld-case        as logical
            field pro_Format          as character
            field pro_Help            as character
            field pro_Initial         as character
            field pro_Label           as character
            field pro_Mandatory       as logical
            field pro_Name            as character
            field pro_Order           as integer
            field pro_type            as character
            field pro_Valexp          as character
            field pro_Valmsg          as character
            index upi        IS UNIQUE PRIMARY ds_name ds_type
            index order      is unique         pro_order.

define temp-table w_index NO-UNDO
            field     pro_Active       like DICTDB._index._Active        
            field     pro_Desc         like DICTDB._index._Desc        
     /*     field     pro_File-recid   like DICTDB._index._File-recid  */
            field     pro_For-Name     like DICTDB._index._For-Name    
     /*     field     pro_For-Type     like DICTDB._index._For-Type    */
     /*     field     pro_I-misc1      like DICTDB._index._I-misc1     */
     /*     field     pro_I-misc2      like DICTDB._index._I-misc2     */
     /*     field     pro_I-res1       like DICTDB._index._I-res1      */
     /*     field     pro_I-res2       like DICTDB._index._I-res2      */
            field     pro_idx-num      like DICTDB._index._idx-num     
            field     pro_Index-Name   like DICTDB._index._Index-Name  
     /*     field     pro_num-comp     like DICTDB._index._num-comp    */
            field     pro_Unique       like DICTDB._index._Unique      
            field     pro_Wordidx      like DICTDB._index._Wordidx     
            INDEX upi        IS UNIQUE PRIMARY pro_idx-num
            INDEX for-name                     pro_For-Name.

define temp-table w_index-field NO-UNDO
        field     pro_Abbreviate   like DICTDB._index-field._Abbreviate  
        field     pro_Ascending    like DICTDB._index-field._Ascending   
        field     pro_For-name     like DICTDB._field._For-name 
        field     pro_For-type     like DICTDB._field._For-type 
     /* field     pro_If-misc1     like DICTDB._index-field._If-misc1  */
     /* field     pro_If-misc2     like DICTDB._index-field._If-misc2  */
     /* field     pro_If-res1      like DICTDB._index-field._If-res1   */
     /* field     pro_If-res2      like DICTDB._index-field._If-res2   */
        field     pro_idx-num      like DICTDB._index._idx-num 
        field     pro_Index-Seq    like DICTDB._index-field._Index-Seq   
        field     pro_Unsorted     like DICTDB._index-field._Unsorted 
        INDEX upi        IS UNIQUE PRIMARY pro_idx-num
                                           pro_For-name
                                           pro_For-type.   

define temp-table y_Tmp-File-Trig
        field     y_Event          like DICTDB._File-Trig._Event
        field     y_Proc-name      like DICTDB._File-Trig._Proc-Name
        field     y_Override       like DICTDB._File-Trig._Override
        field     y_Trig-Crc       like DICTDB._File-Trig._Trig-Crc.
        
define temp-table y_Tmp-Field-Trig
        field     y_Event          like DICTDB._Field-Trig._Event
        field     y_Proc-name      like DICTDB._Field-Trig._Proc-Name
        field     y_Override       like DICTDB._Field-Trig._Override
        field     y_Trig-Crc       like DICTDB._Field-Trig._Trig-Crc
        field     y_Field-Name     like DICTDB._Field._field-Name.
        
/*
define buffer   l_Index          FOR  DICTDB._Index.
define buffer   l_Index-Field    FOR  DICTDB._Index-Field.
*/
define buffer   l_ttb_idx        FOR  s_ttb_idx.


/* LANGUAGE DEPENDENCIES START */ /*--------------------------------*/

define variable err-msg     as character format "x(40)" extent 19
      initial [
/*  1 */ "WARNING: Column &1 is hidden; it cannot be an index component",
/*  2 */ "WARNING: No index for the RECID &1 field",
/*  3 */ "WARNING: No possible RECID-Index found for table &1 (Owner: &2)",
/*  4 */ "WARNING: The Driver sends wrong data about indexes, they cant be build automatically",
/*  5 */ "WARNING: The previously selected index for ROWID functionality for table",
/*  6 */ "WARNING: &1 might be not the optimal index for ROWID functionality.",
/*  7 */ "Use Dictionary to check correctness.",
/*  8 */ "ERROR: TempTable-record for &1 not found.",
/*  9 */ "&1 ",  /* un-used - except debugging messages /**/ */
/* 10 */ "WARNING: The table &1 is frozen. It did not get updated.",
/* 11 */ "DETACHED FILE-TRIGGER   :",
/* 12 */ "DETACHED FIELD-TRIGGER  :",
/* 13 */ "REASSIGNED FILE-TRIGGER :",
/* 14 */ "REASSIGNED FIELD-TRIGGER:",
/* 15 */ "doesn't exist anymore! Trigger cannot be reassigned!",
/* 16 */ "Please check errors, warnings and messages in the file ""ds_upd.e""!",
/* 17 */ "WARNING: Index name changed from &1 to &2!",
/* 18 */ "WARNING: Index &1 changed from UNIQUE to NON-UNIQUE - change not reflected!",
/* 19 */ "Wait, or press any key to continue ..."
              ].     

FORM
                                                    SKIP(1)
  msg[1]   FORMAT "x(29)" LABEL "Object" colon 8 
    "->"
    msg[2] FORMAT "x(25)" LABEL "Object"            SKIP(1)
/** /
  msg[3]   FORMAT "x(29)" LABEL "Column" colon 8 
    "->"
    msg[4] FORMAT "x(25)" LABEL "Field"             SKIP
  msg[5]   FORMAT "x(29)" LABEL "Key"    colon 8 
    "->"
    msg[6] FORMAT "x(25)" LABEL "Index"             SKIP (1)
/ **/
  WITH FRAME ds_make ATTR-SPACE OVERLAY SIDE-LABELS ROW 4 CENTERED
  TITLE " Transferring " + edbtyp + " Definition " USE-TEXT.

/* LANGUAGE DEPENDENCIES END */ /*----------------------------------*/
      
/*---------------------  Internal Procedures  ----------------------*/

PROCEDURE delete-file.

/* in case there is no field in the first table */
  if s_1st-error = false
   then do:        
    find first DICTDB._File-Trig  of DICTDB._File no-lock no-error.
    find first DICTDB._Field-Trig of DICTDB._File no-lock no-error.
    if  available DICTDB._File-Trig
     or available DICTDB._Field-Trig
     then do:  /* delete ev. old error.files & set "errors = occured" */
      assign s_1st-error = true.
      output stream s_stm_errors to ds_upd.e.
      output stream s_stm_errors close.
      end.
    end.
                 
  for each DICTDB._File-Trig of DICTDB._File exclusive-lock:

    output stream s_stm_errors to ds_upd.e append.
    put stream s_stm_errors 
      err-msg[11]              format "x(26)"
      DICTDB._File._File-Name  format "x(64)"
      DICTDB._File-Trig._Event format "x(15)" 
      DICTDB._File-Trig._Proc-Name skip.
    output stream s_stm_errors close.

    create y_Tmp-File-Trig.
    assign
      y_Tmp-File-Trig.y_Event     = DICTDB._File-Trig._Event
      y_Tmp-File-Trig.y_Proc-name = DICTDB._File-Trig._Proc-Name
      y_Tmp-File-Trig.y_Override  = DICTDB._File-Trig._Override
      y_Tmp-File-Trig.y_Trig-Crc  = DICTDB._File-Trig._Trig-Crc.
    delete DICTDB._File-Trig.

    end.

  delete DICTDB._File.

  end PROCEDURE.  /* delete-file */

/*------------------------------------------------------------------*/        
      
PROCEDURE delete-field.

/* on delete of DICTDB._Field do:*/

  if s_1st-error = false
   then do:        
    find first DICTDB._File-Trig  of DICTDB._File no-lock no-error.
    find first DICTDB._Field-Trig of DICTDB._File no-lock no-error.
    if  available DICTDB._File-Trig
     or available DICTDB._Field-Trig
     then do:  /* delete ev. old error.files & set "errors = occured" */
      assign s_1st-error = true.
      output stream s_stm_errors to ds_upd.e.
      output stream s_stm_errors close.
      end.
    end.
                 
  for each DICTDB._Field-Trig of DICTDB._Field exclusive-lock:

    output stream s_stm_errors to ds_upd.e append.
    put stream s_stm_errors 
      err-msg[12]               format "x(26)" 
      DICTDB._File._File-Name 
      DICTDB._Field._Field-Name 
      DICTDB._Field-Trig._Event format "x(15)" 
      DICTDB._Field-Trig._Proc-Name skip.
    output stream s_stm_errors close.

    create y_Tmp-Field-Trig.
    assign
      y_Tmp-Field-Trig.y_Event      = DICTDB._Field-Trig._Event
      y_Tmp-Field-Trig.y_Proc-name  = DICTDB._Field-Trig._Proc-Name
      y_Tmp-Field-Trig.y_Override   = DICTDB._Field-Trig._Override
      y_Tmp-Field-Trig.y_Trig-Crc   = DICTDB._Field-Trig._Trig-Crc
      y_Tmp-Field-Trig.y_Field-name = DICTDB._Field._Field-Name.

    delete DICTDB._Field-Trig.

    end.

  delete DICTDB._Field.

  end PROCEDURE.  /* delete-field */

/*------------------------------------------------------------------*/        
      
PROCEDURE create-file.

  define input parameter p_table-name as character.
  
  create DICTDB._File.
  
  for each y_Tmp-File-Trig:     

    output stream s_stm_errors to ds_upd.e append.
    put stream s_stm_errors 
      err-msg[13]           format "x(26)" 
      p_table-name          format "x(64)"
      y_Tmp-File-Trig.y_Event format "x(15)" 
      y_Tmp-File-Trig.y_Proc-Name skip.
    output stream s_stm_errors close.

    create DICTDB._File-Trig.
    assign 
      DICTDB._File-Trig._File-Recid  = RECID(_File)
      DICTDB._File-Trig._Event       = y_Tmp-File-Trig.y_Event
      DICTDB._File-Trig._Proc-Name   = y_Tmp-File-Trig.y_Proc-name 
      DICTDB._File-Trig._Override    = y_Tmp-File-Trig.y_Override  
      DICTDB._File-Trig._Trig-Crc    = ?
                                    /* y_Tmp-File-Trig.y_Trig-Crc */ 
      .
    delete y_Tmp-File-Trig.

    end.        /* create all file-triggers for this file */

  end PROCEDURE.  /* create-file */

      
/*------------------------------------------------------------------*/        
      
Procedure Field-Triggers:

  if CAN-FIND(first y_Tmp-Field-Trig)
   then do:     /* there are field-triggers */
    
    for each y_Tmp-Field-Trig:
      
      find first DICTDB._Field of DICTDB._File
        where DICTDB._Field._Field-Name = y_Tmp-Field-Trig.y_Field-Name
        no-lock no-error.
        
      if not available DICTDB._Field    /* in case a field got dropped or */
       then do:                         /* its name got changed */

        output stream s_stm_errors to ds_upd.e append.
        put stream s_stm_errors 
          err-msg[15]                    format "x(26)" 
          DICTDB._File._File-Name 
          y_Tmp-Field-Trig.y_Field-Name  
          y_Tmp-Field-Trig.y_Event       format "x(15)" 
          y_Tmp-Field-Trig.y_Proc-Name skip.
        output stream s_stm_errors close.

        end.
       else do:  /* found field to reconnect trigger */ 

        output stream s_stm_errors to ds_upd.e append.
        put stream s_stm_errors 
          err-msg[4]                    format "x(26)" 
          DICTDB._File._File-Name 
          y_Tmp-Field-Trig.y_Field-Name 
          y_Tmp-Field-Trig.y_Event      format "x(15)" 
          y_Tmp-Field-Trig.y_Proc-Name skip.
        output stream s_stm_errors close.

        create DICTDB._Field-Trig.
        assign 
          DICTDB._Field-Trig._Field-Recid = RECID(_Field)
          DICTDB._Field-Trig._File-Recid  = RECID(_File)
          DICTDB._Field-Trig._Event       = y_Tmp-Field-Trig.y_Event
          DICTDB._Field-Trig._Proc-Name   = y_Tmp-Field-Trig.y_Proc-name 
          DICTDB._Field-Trig._Override    = y_Tmp-Field-Trig.y_Override  
          DICTDB._Field-Trig._Trig-Crc    = ?
                                         /* y_Tmp-Field-Trig.y_Trig-Crc */
          . 

        end.    /* found field to reconnect trigger */

      delete y_Tmp-Field-Trig.
            
      end.      /* create all field-triggers for this file */
      
    end.        /* there are field-triggers */
  
  end PROCEDURE.  /* Field-Triggers */

/*------------------------------------------------------------------*/        

procedure error_handling:

  define INPUT PARAMETER error-nr         as INTEGER.
  define INPUT PARAMETER param1           as CHARACTER.
  define INPUT PARAMETER param2           as CHARACTER.

  if param1 = ? then assign param1 = "".
  if param2 = ? then assign param2 = "".
    
  if s_1st-error = false
   then do:
    assign s_1st-error = true.
    output stream s_stm_errors to ds_upd.e.
    output stream s_stm_errors close.
    end.
  output stream s_stm_errors to ds_upd.e append.
  PUT stream s_stm_errors unformatted
            SUBSTITUTE(err-msg[error-nr],param1,param2)  skip.
  output stream s_stm_errors close.
 
  end PROCEDURE.  /* error_handling */

/*---------------------------  TRIGGERS  ---------------------------*/

/*------------------------------------------------------------------*/
/*---------------------------  MAIN-CODE  --------------------------*/
/*------------------------------------------------------------------*/

assign
  batch_mode = SESSION:BATCH-MODE
  cache_dirty = TRUE
  edbtyp     = {adecomm/ds_type.i
                 &direction = "itoe"
                 &from-type = "user_dbtype"
                 }
  odbtyp      = {adecomm/ds_type.i
                    &direction = "ODBC"
                    &from-type = "odbtyp"
                }
  .
  

IF NOT batch_mode then assign SESSION:IMMEDIATE-DISPLAY = yes.

RUN adecomm/_setcurs.p ("WAIT").

/**/&IF "{&DS_DEBUG}" = "DEBUG"
/**/ &THEN
/**/ message "_gat_cro.p: creating dbug-output" view-as alert-box.
/**/ run error_handling(9, "*****----- gat_cro.p!!! -----*****" ,"").
/**/
/**/  if s_1st-error = false
/**/   then do:
/**/    assign s_1st-error = true.
/**/    output stream s_stm_errors to ds_upd.e.
/**/    output stream s_stm_errors close.
/**/    end.
/**/  output stream s_stm_errors to ds_upd.e append.
/**/  for each gate-work no-lock: 
/**/    display stream s_stm_errors
/**/      gate-work.gate-name gate-work.gate-flag
/**/      RECID(gate-work) gate-work.ttb-recid
/**/      with width 140. 
/**/    end.
/**/  for each s_ttb_seq: 
/**/    display stream s_stm_errors
/**/      s_ttb_seq.pro_name format "x(20)"
/**/      s_ttb_seq.ds_name  format "x(20)"
/**/      s_ttb_seq.ds_spcl  format "x(20)"
/**/      RECID(s_ttb_seq)
/**/      with width 140. 
/**/    end.
/**/  for each s_ttb_tbl: 
/**/    display stream s_stm_errors
/**/      s_ttb_tbl.pro_name  s_ttb_tbl.ds_name
/**/      s_ttb_tbl.pro_recid s_ttb_tbl.ds_type
/**/      RECID(s_ttb_tbl) 
/**/      with width 140. 
/**/    end.
/**/  for each s_ttb_fld: 
/**/    display stream s_stm_errors
/**/      s_ttb_fld.pro_name s_ttb_fld.ds_name
/**/      s_ttb_fld.pro_type s_ttb_fld.ds_type
/**/      s_ttb_fld.pro_frmt s_ttb_fld.ds_shdn
/**/      s_ttb_fld.ttb_tbl
/**/      with width 140. 
/**/    end.
/**/  for each s_ttb_idx: display stream s_stm_errors s_ttb_idx with width 140. end.
/**/  for each s_ttb_idf: display stream s_stm_errors s_ttb_idf with width 140. end.
/**/  output stream s_stm_errors close.
/**/
/**/  &ENDIF

if can-do(odbtyp,user_dbtype)
 then assign
    l_char-types = "LONGVARBINARY,LONGVARCHAR,CHAR,VARCHAR,BINARY,VARBINARY,TIME"
    l_chda-types = "TIMESTAMP"
    l_date-types = "DATE"
    l_dcml-types = ""
    l_dein-types = "DECIMAL,NUMERIC,DOUBLE,FLOAT,REAL,BIGINT"
    l_deil-types = "INTEGER,SMALLINT,TINYINT"
    l_intg-types = ""
    l_logi-types = "BIT".
else if user_dbtype = "ORACLE"
   then assign
    l_char-types = "CHAR,VARCHAR,VARCHAR2,ROWID,LONG,RAW,LONGRAW"
    l_chda-types = "DATE"
    l_date-types = ""
    l_dcml-types = "FLOAT"
    l_dein-types = ""
    l_deil-types = "NUMBER"
    l_intg-types = "TIME"
    l_logi-types = "LOGICAL".
 else assign
    l_char-types = "CHAR,BINARY,IMAGE,SYSNAME,TEXT,TIMESTAMP,VARCHAR,VARBINARY"
    l_chda-types = ""
    l_date-types = "DATETIME,DATETIMEN,DATETIME4"
    l_dcml-types = "MONEY,MONEYN,MONEY4,REAL,FLOAT,FLOATN"
    l_dein-types = ""
    l_deil-types = ""
    l_intg-types = "INT,INTN,SMALLINT,TIME,TIME4,TINYINT"
    l_logi-types = "BIT".


/*------------------------------------------------------------------*/  
/*---------------------------- MAIN-LOOP ---------------------------*/
/*------------------------------------------------------------------*/  

/*---------------------------- Sequences ---------------------------*/  

for each gate-work
  where gate-work.gate-slct = TRUE:
  
  if not
   (    gate-work.gate-type = "SEQUENCE"
   or ( gate-work.gate-type = "SYNONYM"
   and  entry(3,gate-work.gate-edit,":") = "SEQUENCE"
   )  )
   then next.  /* neither sequence nor synonym for a sequence */

  find first s_ttb_seq
    where RECID(s_ttb_seq) = gate-work.ttb-recid
    no-error.
  if not available s_ttb_seq
   then do:
    if SESSION:BATCH-MODE and logfile_open
     then put unformatted
       "SEQUENCE"               at 10
       gate-work.gate-name      at 25
       "NO s_ttb_seq FOUND !!!" at 60 skip.
    run error_handling(8, gate-work.gate-name ,"").
    next.
    end.
  
  if TERMINAL <> "" and NOT batch_mode
   then DISPLAY 
      s_ttb_seq.ds_name  @ msg[1]
      s_ttb_seq.pro_name @ msg[2]
      WITH FRAME ds_make.
/* 
 * changed because of performance resaons
 *  
 *     s_ttb_seq.ds_name  @ msg[1] ""       @ msg[4]
 *     s_ttb_seq.pro_name @ msg[2] ""       @ msg[5]
 *     ""                 @ msg[3] ""       @ msg[6]
 *     WITH FRAME ds_make.
 */

  if SESSION:BATCH-MODE and logfile_open
   then put unformatted  "SEQUENCE" at 10 s_ttb_seq.ds_name at 25.

  if s_ttb_seq.pro_recid = ?
   then do:  /* s_ttb_seq.pro_recid = ? */

    if user_dbtype = "ORACLE"
     then find first DICTDB._Sequence
      where DICTDB._Sequence._Db-Recid    = drec_db
      and   DICTDB._Sequence._Seq-Misc[1] = s_ttb_seq.ds_name
      and   DICTDB._Sequence._Seq-Misc[2] = s_ttb_seq.ds_user
      and   DICTDB._Sequence._Seq-misc[8] = s_ttb_seq.ds_spcl
      no-error.
    else if can-do(odbtyp,user_dbtype)
     then find first DICTDB._Sequence
      where DICTDB._Sequence._Db-Recid    = drec_db
      and   DICTDB._Sequence._Seq-Misc[1] = s_ttb_seq.ds_name
      and   DICTDB._Sequence._Seq-Misc[2] = s_ttb_seq.ds_user
      no-error.

    if not available DICTDB._Sequence
     then find first DICTDB._Sequence
      where DICTDB._Sequence._Seq-name     = s_ttb_seq.pro_name
      and   DICTDB._Sequence._Db-recid     = drec_db
      and   DICTDB._Sequence._Seq-misc[2]  = "%TEMPORARY%"
      no-error.

    end.     /* s_ttb_seq.pro_recid = ? */

   else find first DICTDB._Sequence
    where RECID(DICTDB._Sequence) = s_ttb_seq.pro_recid
    no-error.

  if not available DICTDB._Sequence
   then do:  
    if SESSION:BATCH-MODE and logfile_open
     then put unformatted  "NEW" at 60 skip.

    create DICTDB._Sequence.
    assign
      DICTDB._Sequence._Db-Recid    = drec_db
      DICTDB._Sequence._Seq-Name    = s_ttb_seq.pro_name.
    end.
   else if SESSION:BATCH-MODE and logfile_open
     then put unformatted skip.
  
  assign
    DICTDB._Sequence._Seq-Incr    = s_ttb_seq.ds_incr
    DICTDB._Sequence._Seq-Init    = s_ttb_seq.ds_min
    DICTDB._Sequence._Seq-Max     = s_ttb_seq.ds_max
    DICTDB._Sequence._Seq-Min     = s_ttb_seq.ds_min
    DICTDB._Sequence._Cycle-ok    = s_ttb_seq.ds_cycle
    DICTDB._Sequence._Seq-Misc[1] = s_ttb_seq.ds_name
    DICTDB._Sequence._Seq-Misc[2] = s_ttb_seq.ds_user
    DICTDB._Sequence._Seq-misc[3] = ( if can-do(odbtyp,user_dbtype)
                                       then s_ttb_seq.ds_spcl
                                       else DICTDB._Sequence._Seq-misc[3]
                                    )
    DICTDB._Sequence._Seq-misc[8] = ( if user_dbtype = "ORACLE"
                                       then s_ttb_seq.ds_spcl
                                       else DICTDB._Sequence._Seq-misc[8]
                                    ).
  
  end.
  
/*------------------------------ Tables ----------------------------*/  

for each gate-work
  where gate-work.gate-slct = TRUE:

  if  gate-work.gate-type = "SEQUENCE" 
   or gate-work.gate-type = "PROGRESS" then next.
  
  for each s_ttb_tbl
    where recid(gate-work) = s_ttb_tbl.gate-work:
  
    /* just in case some garbage left over */
    for each w_field:       delete w_field.         end.
    for each w_index:       delete w_index.         end.
    for each w_index-field: delete w_index-field.   end.

    if TERMINAL <> "" and NOT batch_mode
     then DISPLAY 
        s_ttb_tbl.ds_name  @ msg[1]
        s_ttb_tbl.pro_name @ msg[2]
        WITH FRAME ds_make.
/* 
 * changed because of performance resaons
 *  
 *     s_ttb_tbl.ds_name  @ msg[1] ""       @ msg[4]
 *     s_ttb_tbl.pro_name @ msg[2] ""       @ msg[5]
 *     ""                 @ msg[3] ""       @ msg[6]
 *     WITH FRAME ds_make.
 */

    if SESSION:BATCH-MODE and logfile_open
     then put unformatted
       s_ttb_tbl.ds_type at 10
       s_ttb_tbl.ds_name at 25.

    if s_ttb_tbl.pro_recid <> ?
     then find first DICTDB._File
        where RECID(DICTDB._File)        = s_ttb_tbl.pro_recid
        no-error.
    else if user_dbtype = "ORACLE"
     and s_ttb_tbl.ds_msc21 <> ?
     and s_ttb_tbl.ds_msc21 <> ""
     then find first DICTDB._File
        where DICTDB._File._Db-Recid     = drec_db
        and   DICTDB._File._For-name     = s_ttb_tbl.ds_name
        and   DICTDB._File._For-owner    = s_ttb_tbl.ds_user
        and   DICTDB._File._Fil-misc2[8] = s_ttb_tbl.ds_spcl
        and   DICTDB._File._Fil-misc2[1] = s_ttb_tbl.ds_msc21
        no-error.
    else if user_dbtype = "ORACLE"
     then find first DICTDB._File
        where DICTDB._File._Db-Recid     = drec_db
        and   DICTDB._File._For-name     = s_ttb_tbl.ds_name
        and   DICTDB._File._For-owner    = s_ttb_tbl.ds_user
        and   DICTDB._File._Fil-misc2[8] = s_ttb_tbl.ds_spcl
        no-error.
    else if can-do(odbtyp,user_dbtype)
     then find first DICTDB._File
        where DICTDB._File._Db-Recid     = drec_db
        and   DICTDB._File._For-name     = s_ttb_tbl.ds_name
        and   DICTDB._File._For-owner    = s_ttb_tbl.ds_user
        and   DICTDB._File._Fil-misc2[1] = s_ttb_tbl.ds_spcl
        no-error.
     else find first DICTDB._File
        where DICTDB._File._Db-Recid     = drec_db
        and   DICTDB._File._For-name     = s_ttb_tbl.ds_name
        and   DICTDB._File._For-owner    = s_ttb_tbl.ds_user
        no-error.
    assign
      oldf         = available DICTDB._File
      tab_recidNew = ""
      tab_recidOld = "".

/*
  if NOT oldf
   then do:
    find first DICTDB._File
      where DICTDB._File._Db-Recid     = drec_db
      and   DICTDB._File._File-name    = s_ttb_tbl.pro_name
      and   DICTDB._File._For-name     = ?
      no-error.
    assign newf = available DICTDB._File.
    end.
*/
    
  
    assign tab_PrimeIdx = ?.


/*---------------------- SAVE CURRENT VALUES -----------------------*/

    if oldf
     then do:  /* retain all file, index and field-information */

      if DICTDB._File._Frozen = TRUE
       then do:
        run error_handling(10, DICTDB._File._File-name, "").
        next.
        end.


      for each DICTDB._Index of DICTDB._File:     /*---- Indexes -----*/
      
        for each DICTDB._Index-field of DICTDB._Index:
          find first DICTDB._Field of DICTDB._Index-field.
          create w_index-field.
          assign
            w_index-field.pro_Abbreviate = DICTDB._index-field._Abbreviate
            w_index-field.pro_Ascending  = DICTDB._index-field._Ascending
            w_index-field.pro_For-name   = DICTDB._field._For-name
            w_index-field.pro_For-type   = DICTDB._field._For-type
            w_index-field.pro_idx-num    = DICTDB._index._idx-num   
            w_index-field.pro_Index-Seq  = DICTDB._index-field._Index-Seq
            w_index-field.pro_Unsorted   = DICTDB._index-field._Unsorted.
          delete DICTDB._Index-field.
          end.  /* for each DICTDB._Index-Field */
          
        if DICTDB._File._Prime-Index = RECID(DICTDB._Index) 
         then assign
          tab_PrimeIdx = DICTDB._Index._Index-name
          tab_PIForNum = DICTDB._index._idx-num.
        create w_index.
        assign
          w_index.pro_active     = DICTDB._index._active  
          w_index.pro_Desc       = DICTDB._index._Desc  
          w_index.pro_For-Name   = DICTDB._index._For-Name  
          w_index.pro_idx-num    = DICTDB._index._idx-num  
          w_index.pro_Index-Name = DICTDB._index._Index-Name  
          w_index.pro_WordIdx    = DICTDB._index._WordIdx  
          w_index.pro_Unique     = DICTDB._index._Unique
          tab_recidOld           = ( if DICTDB._index._I-misc2[1] begins "r"
                                      then DICTDB._index._For-Name
                                    else tab_recidOld
                                 ). 
        delete DICTDB._Index.
        
        end.  /* for each DICTDB._Index */
   
      for each DICTDB._Field OF DICTDB._File:     /*----- fields -----*/

          CREATE w_field.
          assign
            w_field.ds_Name      = DICTDB._Field._For-name
            w_field.ds_Type      = DICTDB._Field._For-type
            w_field.pro_Can-Read = DICTDB._Field._Can-Read
            w_field.pro_Can-Writ = DICTDB._Field._Can-Write
            w_field.pro_Col-lbl  = DICTDB._Field._Col-label
            w_field.pro_Decimals = DICTDB._Field._Decimals
            w_field.pro_Desc     = DICTDB._Field._Desc
            w_field.pro_type     = DICTDB._Field._Data-type
            w_field.pro_fld-case = DICTDB._Field._Fld-case
            w_field.pro_Format   = DICTDB._Field._Format
            w_field.pro_Help     = DICTDB._Field._Help
            w_field.pro_Initial  = DICTDB._Field._Initial
            w_field.pro_Label    = DICTDB._Field._Label
            w_field.pro_Name     = DICTDB._Field._Field-name
            w_field.pro_Order    = DICTDB._Field._Order
            w_field.pro_Valexp   = DICTDB._Field._Valexp
            w_field.pro_Valmsg   = DICTDB._Field._Valmsg.

        RUN delete-field.
       
        end.   /* for each DICTDB._Field OF DICTDB._File */
      
      assign
/*      tab_name     = DICTDB._File._File-name  */
        tab_Can-Crea = DICTDB._File._Can-Create
        tab_Can-Dele = DICTDB._File._Can-Delete
        tab_Can-Read = DICTDB._File._Can-Read
        tab_Can-Writ = DICTDB._File._Can-Write
        tab_Desc     = DICTDB._File._Desc
        tab_Dump-nam = DICTDB._File._Dump-name
        tab_Hidden   = DICTDB._File._Hidden
        tab_Valexp   = DICTDB._File._Valexp
        tab_Valmsg   = DICTDB._File._Valmsg.
      RUN delete-file.

      end.     /* retain all file, index and field-information */

/*--------------------------- CREATION -----------------------------*/

    RUN create-file (INPUT s_ttb_tbl.pro_name).
/*                                                      ORA    others
 *                                                     pro/ds  pro/ds
 * s_ttb_tbl.ds_recid > 0 -> progress_recid             # / ?   # / ?
 *                    = 0 -> use nativ rowid            ? /-#   # / ?
 *                    < 0 -> normal column for recid    ? /-#   # / ?
 */
    assign
      l_pro_recid                = ( if     user_dbtype         = "ORACLE"
                                        and s_ttb_tbl.ds_recid <= 0
                                        then ? /* no progress_recid */
                                        else s_ttb_tbl.ds_recid
                                   )
      l_ds_recid                 = ( if l_pro_recid = ?
                                        then s_ttb_tbl.ds_recid * -1
                                        else ?
                                   )
      DICTDB._File._File-Name    = s_ttb_tbl.pro_name
      DICTDB._File._Desc         = s_ttb_tbl.pro_desc
      DICTDB._File._Db-recid     = drec_db
      DICTDB._File._Fil-misc1[1] = l_pro_recid /* might change */
      DICTDB._File._Fil-misc1[2] = s_ttb_tbl.ds_rowid /* might change */
      DICTDB._File._Fil-misc1[3] = s_ttb_tbl.ds_msc13
      DICTDB._File._Fil-misc1[4] = l_ds_recid
      DICTDB._File._Fil-misc2[1] = ( if can-do(odbtyp,user_dbtype)
                                      then s_ttb_tbl.ds_spcl /* qualifier */
                                      else s_ttb_tbl.ds_msc21
                                   )
      DICTDB._File._Fil-misc2[2] = s_ttb_tbl.ds_msc22
      DICTDB._File._Fil-misc2[3] = s_ttb_tbl.ds_msc23 /* might change */
      DICTDB._File._Fil-misc2[4] = s_ttb_tbl.ds_msc24
      DICTDB._File._Fil-misc2[8] = ( if can-do("ORACLE",user_dbtype)
                                      then s_ttb_tbl.ds_spcl /* db-link */
                                      else DICTDB._File._Fil-misc2[8]
                                   )
      DICTDB._File._For-type     = s_ttb_tbl.ds_type
      DICTDB._File._For-owner    = s_ttb_tbl.ds_user
      DICTDB._File._For-name     = s_ttb_tbl.ds_name.
  /*  DICTDB._File._Dump-name    = ! assigned at end of procedure ! */
  /*  DICTDB._File._Prime-Index  = ! assigned later !               */


    if oldf
     then assign
      DICTDB._File._Can-Create   = tab_Can-Crea
      DICTDB._File._Can-Read     = tab_Can-Read
      DICTDB._File._Can-Write    = tab_Can-Writ
      DICTDB._File._Can-Delete   = tab_Can-Dele
      DICTDB._File._Desc         = ( if   tab_Desc <> ""
                                      and tab_Desc <> ?
                                      then tab_Desc
                                      else DICTDB._File._Desc
                                   )
      DICTDB._File._Valexp       = tab_Valexp
      DICTDB._File._Valmsg       = tab_Valmsg
      DICTDB._File._Hidden       = tab_Hidden
      DICTDB._File._Dump-name    = tab_Dump-nam.


/*---------------------------- FIELDS ------------------------------*/
    
    find last w_field
    where w_field.pro_order > 0
    no-error.
    if available w_field
     then assign l_order-max = w_field.pro_order + 10.
     else assign l_order-max = 0.
     
    for each s_ttb_fld
      where s_ttb_fld.ttb_tbl = RECID(s_ttb_tbl):

      /* if the main-part of date-field is of type character we don't
       * need the time-part, so we skip it...
       */
      if   lookup(s_ttb_fld.ds_type,l_chda-types) <> 0
       and s_ttb_fld.pro_type  =  "integer"
       and can-find( w_field
               where w_field.ds_name  = s_ttb_fld.ds_name
               and   w_field.pro_type = "character"  )
       then next.
      
      /* we need to find the w_field by name PLUS type because of
       * date-time fields....
       */
      if lookup(s_ttb_fld.ds_type,l_chda-types) <> 0
       then find first w_field
        where w_field.ds_name = s_ttb_fld.ds_name
        and   lookup(w_field.ds_type,"character,date") <> 0
        no-error.
       else find first w_field
        where w_field.ds_name = s_ttb_fld.ds_name
        and   w_field.ds_type = s_ttb_fld.ds_type
        no-error.
      if not available w_field
       then find first w_field
        where w_field.ds_name = s_ttb_fld.ds_name
        no-error.

      if   available w_field
       and w_field.pro_type    =  "date"
       and w_field.ds_type begins "date"
       and s_ttb_fld.pro_type  =  "integer"
       then release w_field.
      
/* turned off because of performance resaons
 *
 *   if TERMINAL <> "" and NOT batch_mode
 *    then DISPLAY
 *        s_ttb_fld.ds_name             @ msg[3] 
 *        ( if available w_field
 *           then w_field.pro_name 
 *           else s_ttb_fld.pro_name
 *        )                             @ msg[4]  
 *        with frame ds_make.
 */
       
      create DICTDB._Field.
      assign
        DICTDB._Field._Desc         = ( if available w_field
                                          and w_field.pro_desc <> ""
                                          and w_field.pro_desc <> ?
                                          then w_field.pro_desc
                                          else s_ttb_fld.pro_desc
                                      )
        DICTDB._Field._Extent       = s_ttb_fld.pro_extnt
        DICTDB._Field._Field-Name   = ( if available w_field
                                          then w_field.pro_name
                                          else s_ttb_fld.pro_name
                                      )
        DICTDB._Field._File-recid   = RECID(DICTDB._File)
        DICTDB._Field._Fld-Case     = s_ttb_fld.pro_case
        DICTDB._Field._Fld-misc1[1] = s_ttb_fld.ds_prec
        DICTDB._Field._Fld-misc1[2] = s_ttb_fld.ds_scale
        DICTDB._Field._Fld-misc1[3] = s_ttb_fld.ds_lngth
        DICTDB._Field._Fld-misc1[4] = s_ttb_fld.ds_radix
        DICTDB._Field._Fld-misc1[5] = s_ttb_fld.ds_shd#
        DICTDB._Field._Fld-misc2[2] = ( if user_dbtype = "ORACLE"
                                        then s_ttb_fld.ds_shdn
                                        else DICTDB._Field._Fld-misc2[2]
                                      )
        DICTDB._Field._Fld-misc2[3] = s_ttb_fld.ds_msc23
        DICTDB._Field._Fld-misc2[4] = s_ttb_fld.ds_msc24
        DICTDB._Field._Fld-misc2[5] = ( if can-do(odbtyp,user_dbtype)
                                        then s_ttb_fld.ds_shdn
                                        else DICTDB._Field._Fld-misc2[5]
                                      )
        DICTDB._Field._Fld-stoff    = s_ttb_fld.ds_stoff
        DICTDB._Field._Fld-stdtype  = s_ttb_fld.ds_stdtype
        DICTDB._Field._For-Itype    = s_ttb_fld.ds_Itype
        DICTDB._Field._For-Name     = s_ttb_fld.ds_name
        DICTDB._Field._For-type     = s_ttb_fld.ds_type
        DICTDB._Field._Initial      = s_ttb_fld.pro_init
        DICTDB._Field._Mandatory    = s_ttb_fld.pro_mand
        DICTDB._Field._Decimals     = s_ttb_fld.pro_dcml
        DICTDB._Field._Order        = ( if available w_field
                                         then w_field.pro_order
                                         else l_order-max 
                                            + s_ttb_fld.pro_order
                                      )
        DICTDB._Field._Data-type    = s_ttb_fld.pro_type
        DICTDB._Field._Initial      = s_ttb_fld.pro_init
        DICTDB._Field._Format       = ( if s_ttb_fld.pro_frmt = ""
                                          then DICTDB._Field._Format
                                          else s_ttb_fld.pro_frmt
                                      )
        s_ttb_fld.fld_recid         = RECID(DICTDB._Field).

      if available w_field
       then do:
        if (
         (   can-do(l_char-types + "," + l_chda-types               ,s_ttb_fld.ds_type) 
         AND can-do("character"              ,w_field.pro_type) )
         OR
         (   can-do(l_chda-types                                    ,s_ttb_fld.ds_type) 
         AND can-do("character,date"         ,w_field.pro_type) )
         OR
         (   can-do(l_chda-types + "," + l_date-types               ,s_ttb_fld.ds_type) 
         AND can-do("date"                   ,w_field.pro_type) )
         OR
         (   can-do(l_dcml-types + "," + l_deil-types + "," + l_dein-types,s_ttb_fld.ds_type) 
         AND can-do("decimal"                ,w_field.pro_type) )
         OR
         (   can-do(l_deil-types                                    ,s_ttb_fld.ds_type) 
         AND can-do("decimal,integer,logical",w_field.pro_type) )
         OR
         (   can-do(l_deil-types + "," + l_dein-types               ,s_ttb_fld.ds_type) 
         AND can-do("decimal,integer"        ,w_field.pro_type) )
         OR
         (   can-do(l_deil-types + "," + l_dein-types + "," + l_intg-types,s_ttb_fld.ds_type) 
         AND can-do("integer"                ,w_field.pro_type) )
         OR
         (   can-do(l_deil-types + "," + l_logi-types               ,s_ttb_fld.ds_type) 
         AND can-do("logical"                ,w_field.pro_type) )
            )
         then assign
           DICTDB._Field._Data-type = w_field.pro_type
           DICTDB._Field._Decimals  = w_field.pro_decimals
           DICTDB._Field._Initial   = w_field.pro_initial
           DICTDB._Field._Format    = w_field.pro_Format.

        assign
          DICTDB._Field._Can-Read  = w_field.pro_Can-Read
          DICTDB._Field._Can-Write = w_field.pro_Can-Writ
          DICTDB._Field._Col-label = w_field.pro_Col-lbl
          DICTDB._Field._Fld-case  = DICTDB._Field._Fld-case
                                  or ( DICTDB._Field._Data-type = "character" 
                                         AND w_field.pro_Fld-case )
                  /* could change lateron!
                   * In case there's a shadow-column for this field
                   * _Fld-case will get set to FALSE in _odb_mak.p
                   */
          DICTDB._Field._Help      = w_field.pro_Help
          DICTDB._Field._Label     = w_field.pro_Label
          DICTDB._Field._Mandatory = ( DICTDB._Field._Mandatory
                                         OR w_field.pro_Mandatory )
          DICTDB._Field._Valexp    = w_field.pro_Valexp
          DICTDB._Field._Valmsg    = w_field.pro_Valmsg.
      
        end.  /* available w_field */

      end.  /* for each s_ttb_fld */

/* since we keep adding fields to the end of the file, the order-numbers
 * will increase pretty high, so we reorder the new added fields in a
 * way that garanties to have just 10 as spacing
 */
 
    find last DICTDB._Field of DICTDB._File
      where DICTDB._Field._Order >= l_order-max
      no-lock no-error.
    if available DICTDB._Field
     and l_order-max > 0
     then do:  /* renumber new fields */

      /* calculate the number to shift new fields back */
      assign
        i    = DICTDB._Field._Order + 10 - l_order-max
        indn = 10.
      for each DICTDB._Field of DICTDB._File
        where DICTDB._Field._Order >= l_order-max:
        assign indn = indn + 10.
        end.
      assign i = max(i, indn).

      if indn > 20
       then do:  /* more than one new fields */

        /* move new-fields back, to generate enough free space */
        for each DICTDB._Field of DICTDB._File
          where DICTDB._Field._Order >= l_order-max
          and   DICTDB._Field._Order < i + l_order-max:
          assign
            DICTDB._Field._Order = i + DICTDB._Field._Order.
          end.

        /* move new-fields forward, giving them their new, real number */
        assign indn = 0.
        for each DICTDB._Field of DICTDB._File
          where DICTDB._Field._Order > i + l_order-max:
          assign
            DICTDB._Field._Order = l_order-max + indn
            indn                 = indn + 10.
          end.

        end.     /* more than one new fields */

       else do:  /* only one new fields */
        find first DICTDB._Field of DICTDB._File
          where DICTDB._Field._Order >= l_order-max.
        assign
          DICTDB._Field._Order = l_order-max + 10.
        end.     /* only one new fields */
        
      end.     /* renumber new fields */


/*----------------------------- INDEXES ------------------------------*/

    for each s_ttb_idx
      where s_ttb_idx.ttb_tbl = RECID(s_ttb_tbl)
      by    s_ttb_idx.ds_name:
    
/* turned off because of performance resaons
 *
 *  if TERMINAL <> "" and NOT batch_mode
 *   then DISPLAY
 *     s_ttb_idx.ds_name  @ msg[5] 
 *     s_ttb_id.pro_name @ msg[6]
 *     with frame ds_make.
 */

      if user_dbtype = "SYB"
       then do:
        find first w_index
          where w_index.pro_for-name = s_ttb_idx.ds_name
                                     + STRING(s_ttb_idx.pro_Idx#)
          no-error.
        if not available w_index
         then find first w_index
          where w_index.pro_for-name = s_ttb_idx.ds_name
                                     + STRING(w_index.pro_idx-num)
          no-error.
        end.
       else find first w_index
        where w_index.pro_for-name = s_ttb_idx.ds_name
        no-error.

    /* We have two index-lists with each unique names within itself.
     * However, these two list are connected by foreign-name and not by 
     * PROGRESS-name!
     * When creating the indexes we go thru all s_ttb-idx but give the 
     * index the name of the w_index (we want to retain as much as
     * possible).
     * There are two possible problem-cases:
     * a) we don't find a corresponding w_index record, however
     *    there is another one with this progress-name
     * b) we do find the corresponding w_index record, but another
     *    s_ttb_idx uses the w_index's progress name
     * in both cases we have to switch the names
     */
      if not available w_index
       then do:  /* check if s_ttb_idx.pro_name is used in other w_index */
     
        assign i = 1.
        find first w_index
          where w_index.pro_index-name = s_ttb_idx.pro_name
          no-error.
        repeat while available w_index:  /* potential problem */

        /* find corresponding l_ttb_idx */
          if user_dbtype = "SYB"
           then do:
            find first l_ttb_idx
              where l_ttb_idx.ds_name 
                  + STRING(s_ttb_idx.pro_Idx#) = w_index.pro_for-name
              no-error.
            if not available l_ttb_idx
             then find first l_ttb_idx
              where l_ttb_idx.ds_name 
                  + STRING(w_index.pro_idx-num) = w_index.pro_for-name
              no-error.
            end.
           else find first l_ttb_idx
              where l_ttb_idx.ds_name  = w_index.pro_for-name
              no-error.

        /* ev. switch progress-names */
          if available l_ttb_idx
           then do:  /* switch names */
            assign
              user_env[1]        = l_ttb_idx.pro_name
              /* w_index.pro_index-name := s_ttb_idx.pro_name */
              s_ttb_idx.pro_name = ? 
              l_ttb_idx.pro_name = w_index.pro_index-name
              s_ttb_idx.pro_name = user_env[1].
          /* another potential names-collision? */
            find first w_index
              where w_index.pro_index-name = s_ttb_idx.pro_name
              no-error.
            end.     /* switch names */

          else if w_index.pro_For-name = ?
           then do:  /* user-defined index exists -> change name */
            assign
             s_ttb_idx.pro_name = ( if i > 9
                                     then substring
                                      (s_ttb_idx.pro_name
                                      ,1
                                      ,length(s_ttb_idx.pro_name
                                             ,"character"
                                             ) - 3
                                      ,"character"
                                      ) + string(0 - i,"-99")
                                    else if i > 1
                                     then substring
                                      (s_ttb_idx.pro_name
                                      ,1
                                      ,length(s_ttb_idx.pro_name
                                             ,"character"
                                             ) - 2
                                      ,"character"
                                      ) + string(0 - i,"-9")
                                     else s_ttb_idx.pro_name
                                        + string(0 - i,"-9")
                                    )
             i = i + 1.
          /* another potential names-collision? */
            find first w_index
              where w_index.pro_index-name = s_ttb_idx.pro_name
              no-error.
            end.     /* user-defined index exists -> change name */

           else release w_index.

          end.     /* repeat while available w_index:  potential problem */
        
        end.     /* check if s_ttb_idx.pro_name is used in other w_index */

       else do:  /* check if w_index' name gets used by another s_ttb_idx */

        find first l_ttb_idx
          where l_ttb_idx.ttb_tbl  = RECID(s_ttb_tbl)
          and   l_ttb_idx.pro_name = w_index.pro_index-name
          and   RECID(l_ttb_idx)  <> RECID(s_ttb_idx)
          no-error.

        if available l_ttb_idx
         then assign 
           user_env[1]        = s_ttb_idx.pro_name
           /* w_index.pro_index-name := l_ttb_idx.pro_name */
           l_ttb_idx.pro_name = ? 
           s_ttb_idx.pro_name = w_index.pro_index-name
           l_ttb_idx.pro_name = user_env[1].

        end.     /* check if w_index' name gets used by another s_ttb_idx */

      create DICTDB._Index.
      assign
        DICTDB._Index._Index-Name = ( if available w_index
                                       then w_index.pro_index-name
                                       else s_ttb_idx.pro_name
                                    )
        DICTDB._Index._File-recid = RECID(DICTDB._File)
        DICTDB._Index._Unique     = ( s_ttb_idx.pro_uniq
                                       or  ( AVAILABLE w_index 
                                       and   w_index.pro_unique )
                                    )
        DICTDB._Index._For-name   = s_ttb_idx.ds_name 
                                  + ( if user_dbtype = "SYB"
                                        then string(s_ttb_idx.pro_Idx#)
                                        else ""
                                    )
        DICTDB._Index._Idx-num    = s_ttb_idx.pro_idx#
        DICTDB._Index._Active     = s_ttb_idx.pro_actv
        DICTDB._Index._Desc       = ( if available w_index
                                       then w_index.pro_Desc
                                       else ""
                                    )
        DICTDB._Index._I-misc2[1] = s_ttb_idx.ds_msc21.

      if s_ttb_idx.ds_msc21 begins "r"
       then assign
         tab_recidNew = s_ttb_idx.ds_name.

      if available w_index
       then do:  /* available w_index */
        
        /* give warning if name changed or uniqueness weirdness */
        if DICTDB._Index._Index-name <> w_index.pro_index-name
         then run error_handling
                ( INPUT 17,
                  INPUT DICTDB._Index._Index-name,
                  INPUT w_index.pro_index-name
                  ).
        if w_index.pro_unique       /* index used to be unique but is */
         and not s_ttb_idx.pro_uniq /* in foreign schema not anymore  */
         then do:
          run error_handling
                ( INPUT 18,
                  INPUT DICTDB._Index._Index-name,
                  INPUT string(w_index.pro_unique)
                  ).
          run error_handling(7, "", "").
          end.
        
        /* save index specifications */
        if w_index.pro_for-name = tab_recidOld
         then assign
           tab_RILevel  = s_ttb_idx.hlp_level
           tab_RIfstoff = s_ttb_idx.hlp_fstoff
           tab_RIidx#   = s_ttb_idx.pro_idx#
           tab_RImsc23  = s_ttb_idx.hlp_msc23.
        
        /* delete this w_index record */
        for each w_index-field
          where  w_index-field.pro_idx-num = w_index.pro_idx-num:
          DELETE w_index-field.
          end.
        DELETE w_index.  
        end.     /* available w_index */
 
/*-------------------------- INDEX-FIELDS --------------------------*/

      for each s_ttb_idf
        where s_ttb_idf.ttb_idx = RECID(s_ttb_idx):
      
        find first s_ttb_fld
          where RECID(s_ttb_fld) = s_ttb_idf.ttb_fld.
        find first DICTDB._Field
          where RECID(DICTDB._Field) = s_ttb_fld.fld_recid.

        CREATE DICTDB._Index-field.
        assign
          DICTDB._Index-Field._Ascending   = s_ttb_idf.pro_asc
          DICTDB._Index-Field._Abbreviate  = s_ttb_idf.pro_abbr
          DICTDB._Index-Field._Field-recid = RECID(DICTDB._Field)
          DICTDB._Index-Field._Index-recid = RECID(DICTDB._Index)
          DICTDB._Index-Field._Index-Seq   = s_ttb_idf.pro_order.

        end.   /* for each s_ttb_idf */

      end.   /* for each s_ttb_idx */
  

/*--------------------------- RECID-INDEX ----------------------------*/

    if  ( DICTDB._File._Fil-misc1[1] <= 0
     or   DICTDB._File._Fil-misc1[1]  = ? )
     and NOT can-do("PROCEDURE,BUFFER",DICTDB._File._For-Type)
     then do:  /* no progress_recid & RECID-Index needed
                *     -> check indexes for ROWID usability
                */
    
    /* if there was a previously selected index, we reselect it. However
     * the foreign schema might have changed, so we give a warning, if
     * there is another index, which might fit better (i.e. has a lower
     * value in s_ttb_idx.level)
     */
    
      if tab_recidOld <> ""
       then do:  /* there was a previously selected index */

        if tab_recidOld <> tab_recidNew
         then do:  /* deselect new index, re-select old one */ 

          find first DICTDB._Index of DICTDB._File
            where DICTDB._Index._I-misc2[1] begins "r"
            no-error.
          if available DICTDB._Index
           then assign
            DICTDB._Index._I-misc2[1] = substring
                                        (DICTDB._Index._I-misc2[1]
                                        ,2
                                        ,-1
                                        ,"character"
                                        ).

          find first DICTDB._Index of DICTDB._File
            where DICTDB._Index._For-name = tab_recidOld
            no-error.
          if available DICTDB._Index
           then assign
            DICTDB._Index._I-misc2[1]  = "r" + DICTDB._Index._I-misc2[1]
            DICTDB._File._Fil-misc1[1] = tab_RIfstoff
            DICTDB._File._Fil-misc1[2] = tab_RIidx#
            DICTDB._File._Fil-misc2[3] = tab_RImsc23
            tab_recidNew               = tab_recidOld.

          end.     /* deselect new index, re-select old one */ 

        end.     /* there was a previously selected index */

      if  tab_recidNew <> ""
       then do:  /* found index to be selected */
        find first s_ttb_idx
          where s_ttb_idx.ttb_tbl   = RECID(s_ttb_tbl)
          and   s_ttb_idx.hlp_level < tab_RILevel
          and   s_ttb_idx.ds_name  <> tab_recidNew
          no-error.
        if available s_ttb_idx
         then do:  /* ev. message if prev. selected idx <> optimal */
          RUN error_handling(5, "", "").
          RUN error_handling(6, s_ttb_tbl.pro_name, "").
          end.     /* ev. message if prev. selected idx <> optimal */
        end.     /* found index to be selected */
      
       else do:  /* found no index to be selected */
        RUN error_handling
          ( INPUT 3,
            INPUT DICTDB._File._File-name,
            INPUT DICTDB._File._For-Owner
          ).
        end.    /* found no index to be selected */
          
      end.     /* no progress_recid  & RECID-Index needed
                *     -> check indexes for ROWID usability
                */


  /* message if no recid-index found */
    if has_id_ix /* PROGRESS-RECID-field -> reset recid-info of all indexes */
     then for each DICTDB._Index of DICTDB._File:
        assign DICTDB._Index._I-misc2[1] = ?.
        end.         /* PROGRESS-RECID-field -> reset recid-info of all indexes */
      
    if oldf and tab_PrimeIdx <> ?
     then do:
  /* now attempt to set primary index to same index it used to use */
      find first DICTDB._Index OF DICTDB._File
        where DICTDB._Index._Index-name = tab_PrimeIdx no-error.
      if available DICTDB._Index then
        DICTDB._File._Prime-Index = RECID(DICTDB._Index).
      end.


    run Field-Triggers. /* reassign field-triggers */


  /* delete all left-over, old, "foreign" indexes */
    for each w_index
      where w_index.pro_for-name <> ""
      AND   w_index.pro_for-name <> ?:
      for each w_index-field
        where  w_index-field.pro_idx-num = w_index.pro_idx-num:
        DELETE w_index-field.
        end.
      DELETE w_index.  
      end.


/*----------------------- USER-DEFINED-INDEX -------------------------*/

  /* find last used index-number */
    find last DICTDB._Index
      where DICTDB._Index._Idx-num > 0
      no-error.
    assign indn = ( if available DICTDB._Index
                      then DICTDB._Index._idx-num + 1
                      else 1
                  ).
                
  /* try to recreate all old, "USER-defined" indexes */
    for each w_index:

      assign scrap = true.
      for each w_index-field
        where  w_index-field.pro_idx-num = w_index.pro_idx-num
        while scrap = true:
        find first DICTDB._field of DICTDB._File
          where DICTDB._field._for-name = w_index-field.pro_for-name
          and   DICTDB._field._for-type = w_index-field.pro_for-type
          no-lock no-error.
        if not available DICTDB._field then assign scrap = false.
        end.
      
      if NOT scrap  
       then for each w_index-field  /* index not valid anymore */
        where  w_index-field.pro_idx-num = w_index.pro_idx-num: 
        delete w_index-field.
        end.

       else do:  /* retain index */

        assign user_env[1] = w_index.pro_index-name.
      RUN "prodict/gate/_gat_xlt.p"
            (FALSE,RECID(DICTDB._File),INPUT-OUTPUT user_env[1]).

        if user_env[1] <> w_index.pro_index-name
         then do:

          if s_1st-error = false
           then do:
            assign s_1st-error = true.
            output stream s_stm_errors to ds_upd.e.
            output stream s_stm_errors close.
            end.

          output stream s_stm_errors to ds_upd.e append.
          put stream s_stm_errors unformatted
            "Index name changed from " 
            w_index.pro_index-name " to " user_env[1]   skip.
          output stream s_stm_errors close.

          end.                            
         
        create DICTDB._Index.
        assign
          DICTDB._Index._Index-Name = user_env[1]
          DICTDB._Index._File-recid = RECID(DICTDB._File)
          DICTDB._Index._Unique     = w_index.pro_unique
          DICTDB._Index._For-name   = ""
          DICTDB._Index._Idx-num    = indn
          DICTDB._Index._Active     = w_index.pro_active
          DICTDB._Index._WordIdx    = w_index.pro_WordIdx
          DICTDB._Index._Desc       = w_index.pro_Desc
          indn                      = indn + 1
          i                         = 1.

        if tab_PIForNum = w_index.pro_idx-num
          then assign DICTDB._File._Prime-Index = RECID(DICTDB._Index).
     
        for each w_index-field  
          where  w_index-field.pro_idx-num = w_index.pro_idx-num: 
        
          find first DICTDB._field of DICTDB._File
            where DICTDB._field._for-name = w_index-field.pro_for-name
            and   DICTDB._field._for-type = w_index-field.pro_for-type
            no-lock no-error.
          create DICTDB._Index-field.
          assign
            DICTDB._Index-Field._Index-recid = RECID(DICTDB._Index)
            DICTDB._Index-Field._Index-Seq   = i
            DICTDB._Index-Field._Field-recid = RECID(DICTDB._Field)
            DICTDB._Index-Field._Ascending   = w_index-field.pro_Ascending
            DICTDB._Index-Field._Abbreviate  = w_index-field.pro_Abbreviate
            DICTDB._Index-Field._Unsorted    = w_index-field.pro_Unsorted
            i                            = i + 1.
          delete w_index-field.

          end.     /* for each w_index-field */
       
        end.     /* retain index */
       
      delete w_index.  
    
      end.  /* for each w_index */

    end.  /* for each s_ttb_tbl of gate-work */
    
  end. /* for each gate-work */

if NOT batch_mode
 then SESSION:IMMEDIATE-DISPLAY = no.

RUN adecomm/_setcurs.p ("").

find first DICTDB._File
  where DICTDB._File._Dump-name      =     ?
  and   NOT DICTDB._File._File-name BEGINS "_"
  no-error.
if available DICTDB._File then RUN "prodict/dump/_lodname.p".

if NOT batch_mode
 then HIDE FRAME ds_make NO-PAUSE.

if s_1st-error = true
 then do:   /* there are warnings or messages */
       
        &IF "{&WINDOW-SYSTEM}" = "TTY" 
         &THEN 
          message err-msg[16]. 
         &ELSE
          message err-msg[16] view-as alert-box warning buttons ok.
         &ENDIF         
  
  end.      /* there are warnings or messages */

/* to make sure the next update starts a new ds_upd.e-file, reset flag */
assign s_1st-error = false.

RETURN.

/*------------------------------------------------------------------*/        
