/*************************************************************/
/* copyright (c) 1984-1995 by Progress Software corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

file: prodict/gate/gat_pul.i

description:
    pulls schemainfo of all objects contained in gate-work into
    the temp-tables

    <DS>_get.p gets a list of all pullable objects from the foreign DB
    <DS>_pul.p pulled over the definition from the foreign side
    gat_cmp.p  compared the existing definitions with the pulled info
    gat_cro.p  replaces the existing definitions with the pulled info
               or creates the new object if it didn't already exist

    create <DS> Schema: <DS>_get.p <DS>_pul.p gat_cro.p
    Update <DS> Schema: <DS>_get.p <DS>_pul.p gat_cro.p
    Verify <DS> Schema: <DS>_get.p <DS>_pul.p gat_cmp.p gat_cro.p

Text-Parameters:
    &buffer
    &col-fields     additional fields for the fields-phrase for
                    ds_columns
    &colid          ds-field containing the Column-Id
    &comment        ds-field containing the comment-text
    &dbtyp          {"ora"|"syb"}
    &db-type        {"oracle"|"sybase"}
    &ds_recid       phrase for the ds_recid field (db-field, constant,
                    variable ...)
    &for-idx-name   name of the name-field (for example:
                    sybase_objects.name + STRING(indn))
    &for-idx-nam2   ev. 2 possible name of the name-field (for example:
                    sybase_objects.name + STRING(w_index.pro_idx-num))
    &for-obj-name   name of the name-field (sybase_objects.name, ...)
    &idx-fields     additional fields for the fields-phrase for
                    ds_indexes
    &idx-tbl-break  break by phrase for ds_objects of ds_indexes
    &idx-uniq-cond  condition for uniqueness of an index 
    &idx-where      condition for ds_indexes 
    &idxid          ds-field containing the Index-Id
    &init           phrase for initial field
    &length         ds-field containing the length
    &mand           condition for mandatory-field
    &msc23          "{&msc23}"
    &name           ds-field containing the name
    &objid          ds-field containing the Object-Id
    &precision      ds-field containing the Precision
    &radix          ds-field containing the Radix
    &scale          ds-field containing the Scale
    &type           ds-field containing the type
    &typvar         type-name if NOT buffer
    &typvar-b       type-name if buffer
    &usrid          ds-field containing the user-id 
    &usrid-t        ds_objects-field containing the user-id
    
Input:
    &DS_DEBUG   DEBUG to protocol the creation
                ""    to turn off protocol
    gate-work   shared temp-table that contains all the objects to pull

Output:
    s_ttb_tbl   table-information of all objects
    s_ttb_fld   field-information of all objects
    s_ttb_idx   index-information of all objects
    s_ttb_idf   index-field-information of all objects
    s_ttb_seq   sequence-information of all objects

Included in:
    ora/_ora_pul.p
    syb/syb_getp.i

History:
    hutegger    95/03   created out of ora67mak.i and syb_makp.i

*/

/*
relevant header-comments from syb_mak.p:
----------------------------------------

History
    hutegger    95/01/26    changed schmea-triggers to internal procs
    hutegger    94/11/02    creation
*/
/*h-*/

/*    &DS_DEBUG   DEBUG to protocol the creation    */
/*                ""    to turn off protocol        */
&SCOPED-DEFINE xxDS_DEBUG                   DEBUG

&SCOPED-DEFINE DATASERVER                 YES
&SCOPED-DEFINE FOREIGN_SCHEMA_TEMP_TABLES INCLUDE
{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/{&dbtyp}/{&dbtyp}_ctl.i 7 }
&UNDEFINE DATASERVER
&UNDEFINE FOREIGN_SCHEMA_TEMP_TABLES

&IF "{&db-type}" = "sybase"
 &THEN 
&GLOBAL-DEFINE shdw-prefix U##
  define variable l_keycp       as integer   no-undo. 
  define variable l_keyid       as integer   no-undo. 
  define variable typevar       as character no-undo.
&ELSEIF "{&db-type}" = "oracle"
 &THEN 
&GLOBAL-DEFINE shdw-prefix U##
  define variable typevar       as integer   no-undo.
 &ELSE  /* ODBC */ 
&GLOBAL-DEFINE shdw-prefix _S#_
 &ENDIF
define variable array_name      as character no-undo. 
define variable batch-mode      as logical   no-undo initial false. 
define variable dtyp            as integer   no-undo.
define variable fnam            as character no-undo.
define variable i               as integer   no-undo.
define variable indn            as integer   no-undo.
define variable l_char-types    as character no-undo.
define variable l_chrw-types    as character no-undo.
define variable l_date-types    as character no-undo.
define variable l_dcml          as integer   no-undo.
define variable l_dcml-types    as character no-undo.
define variable l_dt            as character no-undo.
define variable l_fld-descr     as character no-undo.
define variable l_fld-msc24     as character no-undo init ?.
define variable l_fld-pos       as integer   no-undo.
define variable l_floa-types    as character no-undo.
define variable l_frmt          as character no-undo.
define variable l_i#dl-types    as character no-undo.
define variable l_i##d-types    as character no-undo.
define variable l_i##l-types    as character no-undo.
define variable l_i###-types    as character no-undo.
define variable l_init          as character no-undo. 
define variable l_link          as character no-undo.
define variable l_logi-types    as character no-undo.
define variable l_prec          as integer   no-undo.
define variable l_scale         as integer   no-undo.
define variable l_time-types    as character no-undo.
define variable l_tmst-types    as character no-undo.
define variable m1              as integer   no-undo.
define variable m2              as integer   no-undo. 
define variable msg             as character no-undo   EXTENT 8.
define variable namevar         as character no-undo {&case-sensitive}.
define variable namevar-s       as character no-undo. /* synonym */
define variable ntyp            as character no-undo.
define variable onum            as integer   no-undo.
define variable pnam            as character no-undo.
define variable progvar         as character no-undo.
/*define variable shadow_col    as character no-undo.*/
define variable spclvar         as character no-undo.
define variable typevar-s       as character no-undo. /* synonym */
define variable unique-prime    as logical   no-undo. /* upi already found */
define variable uservar         as character no-undo.

/*------------------------------------------------------------------*/

/* LANGUAGE DEPENDENCIES START */ /*--------------------------------*/

form
                                                        skip(1)
  msg[1]        format "x(25)" label "Table"    colon 11 
        "->"
        msg[2]  format "x(25)" label "Table"            skip
  msg[3]        format "x(25)" label "Column"   colon 11 
        "->"
        msg[4]  format "x(25)" label "Field"            skip
  msg[5]        format "x(25)" label "Key"      colon 11 
        "->"
        msg[6]  format "x(25)" label "Index"            skip
  msg[7]        format "x(25)" label "Sequence" colon 11 
        "->"
        msg[8]  format "x(25)" label "Sequence"         skip
  skip(1)
 with row 4 centered
  overlay side-labels attr-space
&IF "{&db-type}" = "sybase"
 &THEN 
  title " Loading SYBASE Definitions " use-text
 &ELSE 
  title " Loading ORACLE Definitions " + l_link use-text
 &ENDIF
  frame ds_make.
  
/* LANGUAGE DEPENDENCIES END */ /*----------------------------------*/


/*------------------------------------------------------------------*/
procedure error_handling:

define INPUT PARAMETER error-nr         as INTEGER.
define INPUT PARAMETER param1           as cHARACTER.
define INPUT PARAMETER param2           as cHARACTER.

define       variable  err-msg as character extent 5 initial [
/*  1 */ "WARNING: Field not found for Index-Field (Object#: &1 Field#: &2).",
/*  2 */ "ERROR: Datatype &1 is not supported (Object#: &2).",
/*  3 */ "skipping this field...",
/*  4 */ "WARNING: Index &1 has too many components; Accepting only first 16.",
/*  5 */ " &1 &2 " /* intentionally left blank for div. error-messages */
    ].
    
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
  
    end.  /* error_handling */


/*------------------------------------------------------------------*/
/*---------------------------  MAIN-CODE  --------------------------*/
/*------------------------------------------------------------------*/

/*------------------------ INITIALIZATIONS -------------------------*/


RUN adecomm/_setcurs.p ("WAIT").

/** / message "gat_pul.i DS_DEBUG {&DS_DEBUG} " view-as alert-box. / **/

/**/ &IF "{&DS_DEBUG} " = "DEBUG"
/**/ &THEN
/**/  message "gat_pul.i" view-as alert-box.
/**/  run error_handling(5, "*****----- BEGIN gat_pul.i!!! -----*****" ,"").
/**/
/**/  output stream s_stm_errors to gate-work.d.
/**/  for each gate-work no-lock: 
/**/    display stream s_stm_errors gate-work with width 255. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/    output to s_ttb_link.d3.
/**/    for each s_ttb_link:
/**/      display
/**/        slctd srchd level
/**/        "*" + master + "*" + s_ttb_link.name + "*" format "x(20)"
/**/        with side-labels frame a.
/**/      for each gate-work
/**/        where gate-work.gate-qual = ( if s_ttb_link.master + s_ttb_link.name = ""
/**/                            then ?
/**/                            else s_ttb_link.master + s_ttb_link.name
/**/                                    ):
/**/        display
/**/          gate-slct label "slct"
/**/          gate-flag label "flg"
/**/          gate-name format "x(20)"
/**/          gate-prog format "x(20)"
/**/          gate-qual format "x(20)"
/**/          gate-type format "x(9)"
/**/          gate-user format "x(9)"
/**/          with down frame b width 100.
/**/        end.  /* for each gate-work */
/**/      end.  /* for each s_ttb_link */
/**/    output close.
/**/ &ENDIF
/**/


&IF "{&db-type}" = "oracle"
 &THEN 
  find first s_ttb_link
    where s_ttb_link.slctd  = TRUE /*actually redundant, but what the heck...*/
    and   s_ttb_link.level  = s_level
    and   s_ttb_link.master = s_master
    and   s_ttb_link.name   = s_lnkname
    no-error.
  if not available s_ttb_link
   then do:  /* should actually never happen */
    assign user_path = "*C,_ora_lkx". /* clean-up */
    leave.
    end.     /* should actually never happen */

 &ENDIF
  
assign
  cache_dirty      = TRUE
  l_dt             = ?
  batch-mode       = SESSION:BATCH-MODE
&IF "{&db-type}" = "sybase"
 &THEN
  l_char-types     = "CHAR,BINARY,IMAGE,SYSNAME,TEXT,TIMESTAMP,VARCHAR,VARBINARY"
  l_chrw-types     = " "
  l_date-types     = "DATETIME,DATETIMEN,DATETIME4"
  l_dcml-types     = "MONEY,MONEYN,MONEY4,REAL,FLOAT,FLOATN"
  l_floa-types     = ""
  l_i#dl-types     = ""
  l_i##d-types     = ""
  l_i##l-types     = ""
  l_i###-types     = "INT,INTN,SMALLINT,TIME,TIME4,TINYINT"
  l_logi-types     = "BIT"
/*l_time-types     = "" */
  l_tmst-types     = ""
  user_env         = "". /* yes this is destructive, but we need the -l space */
&ELSEIF "{&db-type}" = "oracle"
 &THEN 
  l_char-types     = "CHAR,VARCHAR,VARCHAR2,ROWID"
  l_chrw-types     = "LONG,RAW,LONGRAW,UNDEFINED"
  l_date-types     = "DATE"
  l_dcml-types     = "NUMBER"
  l_floa-types     = "FLOAT"
  l_i#dl-types     = ""
  l_i##d-types     = ""
  l_i##l-types     = ""
  l_i###-types     = "TIME"
  l_logi-types     = "LOGICAL"
/*l_time-types     = ""*/
  l_tmst-types     = ""
  l_link           = user_env[25]
  user_env         = "" /* yes this is destructive, but we need the -l space */
  user_env[25]     = l_link
  l_link           = s_ttb_link.master + s_ttb_link.name
  s_ttb_link.srchd = TRUE.
 &ENDIF
  

if NOT batch-mode 
 then do:
  assign SESSION:IMMEDIATE-DISPLAY = yes.
  view frame ds_make.
  end.

RUN prodict/{&dbtyp}/_{&dbtyp}_typ.p
  ( INPUT-OUTPUT i,
    INPUT-OUTPUT i,
    INPUT-OUTPUT l_dt,
    INPUT-OUTPUT l_dt,
    OUTPUT       l_dt
    ). /* fills user_env[11..17] with datatype-info */



/*---------------------------- MAIN-LOOP ---------------------------*/

for each gate-work
  where gate-work.gate-slct = TRUE:
  
  /* Skip pseudo-entry, which is needed to signal, if user wants to 
   * compare not just <DS> -> PROGRESS, but also the other direction
   */
  if gate-work.gate-type = "PROGRESS" then next.

&if "{&db-type}" = "oracle"
 &then
  /* skip ORACLE-entries for remote-db if current db is the local one
   * and vice versa
   */
  assign
    spclvar = ( if gate-work.gate-type = "SYNONYM"
                  and num-entries(gate-work.gate-edit,":") = 4
                  then entry(4,gate-work.gate-edit,":")
                else if gate-work.gate-qual <> ?
                  then gate-work.gate-qual
                  else  ""
              ).
  if l_link <> spclvar
   then next.
 &endif

  assign
    namevar   = ( if gate-work.gate-type = "SYNONYM"
                   then entry(2,gate-work.gate-edit,":")
                   else gate-work.gate-name
                )
    namevar   = TRIM( if gate-work.gate-type = "BUFFER"
                   then {&buffer} + gate-work.gate-type
                      + "_"       + namevar
                   else             namevar
                )
    namevar-s = ( if gate-work.gate-type = "SYNONYM"
                   then gate-work.gate-name
                   else namevar
                )                   /* name of the synonym */
    typevar-s = ( if gate-work.gate-type = "SYNONYM"
                   then entry(3,gate-work.gate-edit,":")
                   else gate-work.gate-type
                )
    typevar   = ( if typevar-s = "BUFFER"
                   then {&typvar-b}
                        /*       "VIEW"                             |
                         *       LOOKUP("VIEW" ,oobjects) - 1       */
                   else {&typvar}
                        /* ENTRY(LOOKUP(typevar-s,pobjects),sobjects) |
                         *       LOOKUP(typevar-s,oobjects) - 1       */
                )
    uservar   = ( if gate-work.gate-type = "SYNONYM"
                   then entry(1,gate-work.gate-edit,":")
                   else gate-work.gate-user
                )
    progvar   = gate-work.gate-prog
    spclvar   = gate-work.gate-qual
    .
    
  if SESSION:BATCH-MODE and logfile_open
   then put unformatted
     gate-work.gate-type at 10
     gate-work.gate-name at 25 skip.

  find first ds_users
    where ds_users.name = uservar
    no-lock no-error.


  case typevar-s:

/*-------------------------- SEQUENCES -----------------------------*/

    when    "SEQUENCE" then do:

      if TERMINAL <> "" and NOT batch-mode
       then DISPLAY 
          ""      @ msg[1]   ""       @ msg[5]
          ""      @ msg[2]   ""       @ msg[6]
          ""      @ msg[3]   namevar  @ msg[7]
          ""      @ msg[4]   progvar  @ msg[8]
          with frame ds_make.

       create s_ttb_seq.

&if "{&db-type}" = "oracle"
 &then
        for each ds_objects
          where ds_objects.name        = namevar
          and   ds_objects.{&usrid-t}  = ds_users.{&usrid}
          no-lock:
            if ds_objects.type = typevar then leave.

        end.

        if not available ds_objects
         then next.
        find first ds_sequences
          where ds_sequences.obj#        = ds_objects.obj#
          no-lock no-error.

        assign
          s_ttb_seq.ds_incr  =   ds_sequences.increment$
          s_ttb_seq.ds_max   = ( if ds_sequences.maxvalue > 2147483647
                                    then ?
                                    else ds_sequences.maxvalue
                               )
          s_ttb_seq.ds_min   =   ds_sequences.minvalue
          s_ttb_seq.ds_cycle = ( ds_sequences.increment$ = 1 )
          .
&endif

      if gate-work.gate-type = "SYNONYM"
       then assign
        gate-work.ttb-recid = RECID(s_ttb_seq)
        s_ttb_seq.ds_name   = namevar-s
        s_ttb_seq.ds_user   = ""
        s_ttb_seq.ds_spcl   = spclvar
        s_ttb_seq.gate-work = RECID(gate-work)
        s_ttb_seq.pro_name  = progvar.
       else assign
        gate-work.ttb-recid = RECID(s_ttb_seq)
        s_ttb_seq.ds_name   = namevar /* used to be progvar, but:
                                       * progvar could be <> namevar
                                       * in case of name-collisions */
        s_ttb_seq.ds_user   = uservar
        s_ttb_seq.ds_spcl   = spclvar
        s_ttb_seq.gate-work = RECID(gate-work)
        s_ttb_seq.pro_name  = progvar.

      NEXT.
      end.

/*--------------------------- PROCEDURES ---------------------------*/

    when    "PROCEDURE"
    or when "FUNCTION"
    or when "PACKAGE" then do:
      if user_dbtype = "ORACLE"
       then do:
        RUN prodict/ora/_ora_prc.p
          (INPUT  typevar,
           INPUT  namevar,
           INPUT  namevar-s,
           INPUT  uservar,
           INPUT  progvar,
           INPUT  spclvar,
           INPUT  RECID(gate-work),
           OUTPUT gate-work.ttb-recid
          ).
        /* synonym    : namevar   = name of base-object
         *              namevar-s = name of synonym
         * non-synonym: namevar   = namevar-s = name of object
         */
        if   gate-work.gate-type = "SYNONYM"
         then do:  /* adjust synonym-object(s) */

          if gate-work.ttb-recid = ?
           then do:  /* package */
            for each s_ttb_tbl
              where s_ttb_tbl.ds_msc21 = namevar:
              assign
                s_ttb_tbl.ds_msc21 = namevar-s
                s_ttb_tbl.ds_user  = "".
              end.     /* for each s_ttb_tbl */
            end.     /* package */

           else do:  /* procedure or function */
            find first s_ttb_tbl
              where recid(s_ttb_tbl) = gate-work.ttb-recid
              no-error.
            if available s_ttb_tbl
             then assign
              s_ttb_tbl.ds_name = namevar-s
              s_ttb_tbl.ds_user = "".
            end.     /* procedure of function */

          end.     /* adjust synonym-object */

        NEXT.
        end.
      else if user_dbtype <> "SYBASE"
       then NEXT.
      end.

    end case.
    

/*----------------------------- TABLES -----------------------------*/

  /* remaining typevars values: "TABLE", "VIEW", "BUFFER" */

  if TERMINAL <> "" and NOT batch-mode
   then DISPLAY
      namevar @ msg[1]  "" @ msg[5]
      progvar @ msg[2]  "" @ msg[6]
      ""      @ msg[3]  "" @ msg[7]
      ""      @ msg[4]  "" @ msg[8]
      with frame ds_make.

  for each ds_objects
    where ds_objects.name        = namevar
  /*  and   ds_objects.type        = typevar */
    and   ds_objects.{&usrid-t}  = ds_users.{&usrid}
    no-lock:
    
    if ds_objects.type = typevar then leave.
  end.  

  if not available ds_objects
   then next.

  assign
    onum = ds_objects.{&objid}.

  find first ds_comments
    where ds_comments.{&objid} = onum
    and   ds_comments.{&colid} = {&colid-t-cmnt}
    no-lock no-error.

  create s_ttb_tbl.

  assign
    gate-work.ttb-recid = RECID(s_ttb_tbl)
    s_ttb_tbl.ds_name   = namevar
    s_ttb_tbl.ds_recid  = 0
    s_ttb_tbl.ds_spcl   = spclvar
    s_ttb_tbl.ds_type   = typevar-s
    s_ttb_tbl.ds_user   = uservar
    s_ttb_tbl.gate-work = RECID(gate-work)
    s_ttb_tbl.pro_desc  = ( if available ds_comments
                             then ds_comments.{&comment}
                             else s_ttb_tbl.pro_desc
                          )                               
    s_ttb_tbl.pro_name  = progvar.
    
  /*s_ttb_tbl._Dump-name fields all assigned at end of procedure*/
  /*s_ttb_tbl._Prime-Index*/ /* this field is assigned later */


/*---------------------------- FIELDS ------------------------------*/

  assign
 /* shadow_col = "" */
    array_name = "".

  for each ds_columns
    fields ({&objid} {&colid} name {&type} {&col-fields})
    where ds_columns.{&objid} = onum
    no-lock
    by    ds_columns.{&colid}:

    if ds_columns.name begins "PROGRESS_RECID"
     then do:
      assign s_ttb_tbl.ds_recid = {&ds_recid}.
      NEXT.
      end.

    if   length(array_name,"character")  >   0
     and ds_columns.name BEGINS array_name
     then NEXT.

    if ds_columns.name BEGINS "U##"
     then do:
/*      assign shadow_col = string(ds_columns.{&colid}). */
      NEXT.
      end.

  &IF "{&db-type}" = "sybase"
   &THEN
    if ds_columns.name <> "timestamp"
     then do:
   &ENDIF
    
      { prodict/{&dbtyp}/{&dbtyp}_typ.i }
      assign m1 = 0.

  
  &IF "{&db-type}" = "oracle"
   &THEN
      find first ds_comments
        where ds_comments.{&objid} = onum
        and   ds_comments.{&colid} = ds_columns.{&colid}
        no-lock no-error.
    
      assign l_fld-descr = ( if available ds_comments 
                                then ds_comments.{&comment}
                                else ""
                           ).
      if ds_columns.name MATCHES "*##1"
       then do:  /* collect array elements & determine extent */

        assign
          m1          = 1
          i           = length(ds_columns.name,"character") - 1
          array_name  = substring(ds_columns.name, 1, i,"character").

        for each ds_columns-2
          fields(name)
          where ds_columns-2.{&objid} = onum
          no-lock
          by    ds_columns-2.{&colid}:

  	  if NOT ds_columns-2.name BEGINS array_name
  	   then NEXT.  /* can't do in where */
	               /* since array_name may have an '_' in it. */

	  assign
	    m2 = INTEGER(substr(ds_columns-2.name,i + 1,-1,"character"))
	    m1 = ( if m2 > m1
	            then m2
	            else m1
	         ).

          end. /* for each ds_columns-2 */

        end.     /* collect array elements & determine extent */
  &ENDIF

      assign
        l_fld-pos = ds_columns.{&colid}.

      { prodict/gate/gat_pulf.i 
        &extent       = "m1"
        &init         = "{&init}"
        &length       = "{&Length}"
        &mand         = "{&mand}"
        &msc23        = "{&msc23}"
        &name         = "{&name}"
        &order-offset = "0"
        &precision    = "{&Precision}"
        &radix        = "{&Radix}"
        &scale        = "{&Scale}"
        }

      if gate-work.gate-type = "BUFFER"
       then assign
          s_ttb_fld.ds_itype = ds_columns.{&type}.

/*
      if length(shadow_col,"character") > 0
       then assign
        s_ttb_fld.ds_shdn  = shadow_col
        s_ttb_fld.ds_shd#  = integer(shadow_col)
        s_ttb_fld.pro_case = FALSE
        shadow_col         = "".
*/
      find first ds_columns-2
        where ds_columns-2.{&objid} = onum
        and   ds_columns-2.name  = "{&shdw-prefix}" + ds_columns.name
        no-lock no-error.
      if available ds_columns-2
       then assign
        s_ttb_fld.ds_shd#  = ds_columns-2.{&colid}
        s_ttb_fld.ds_shdn  = string(s_ttb_fld.ds_shd#)
        s_ttb_fld.pro_case = FALSE
        .

  &IF "{&db-type}" = "sybase"
   &THEN 

      end.   /* each ds_columns */
    end.   /* each ds_columns */

  for each ds_columns 
    where  ds_columns.id = onum
    and   (ds_columns.type = 61
    or     ds_columns.type = 111
    or     ds_columns.type = 58)
    no-lock:
    assign
      l_fld-pos = ds_columns.{&colid}
      l_dt      = ( if    ds_columns.type = 58 
                     or ( ds_columns.type = 111
                     and  ds_columns.syb_length = 4 )
                     then "TIME4" 
                     else "TIME"
                   ).
    { prodict/gate/gat_pulf.i 
        &extent       = "m1"
        &init         = "{&init}"
        &length       = "{&Length}"
        &mand         = "{&mand}"
        &msc23        = "{&msc23}"
        &name         = "{&name}"
        &order-offset = "5"
        &precision    = "{&Precision}"
        &radix        = "{&Radix}"
        &scale        = "{&Scale}"
        }

  &ELSEIF "{&db-type}" = "oracle"
   &THEN 

    if l_dt = "DATE"
     then do:  /* Add time fields */
      assign l_dt = "TIME".
      { prodict/gate/gat_pulf.i 
        &extent       = "m1"
        &init         = "{&init}"
        &length       = "{&Length}"
        &mand         = "{&mand}"
        &msc23        = "{&msc23}"
        &name         = "{&name}"
        &order-offset = "5"
        &precision    = "{&Precision}"
        &radix        = "{&Radix}"
        &scale        = "{&Scale}"
        }
      end.     /* Add time fields */
   &ENDIF
   
    end.   /* each ds_columns */


/*---------------------------- INDEXES -----------------------------*/

  assign 
    indn         = 1
    unique-prime = no.

  for each ds_indexes
      fields({&idx-fields})
      where ds_indexes.{&idxid} = onum
      {&idx-where} no-lock,
    each ds_objects-2
      fields({&objid} name)
      where ds_objects-2.{&objid} = ds_indexes.{&objid}
      no-lock
      {&idx-tbl-break}:
      
    /* skip PROGRESS_RECID index */
    if ds_objects-2.name MATCHES "*##progress_recid"
     then NEXT.

    {prodict/gate/gat_puli.i
      &for-idx-name = "{&for-idx-name}"
      &for-idx-nam2 = "{&for-idx-nam2}"
      &for-obj-name = "{&for-obj-name}"
      &frame        = "ds_make"
      &idx-uniq-cond = "{&idx-uniq-cond}"
      }                                  /* try to recreate index */


/*-------------------------- INDEX-FIELDS --------------------------*/

  &IF "{&db-type}" = "sybase"
   &THEN 
    repeat l_keycp = 1 to 8:

      if      l_keycp = 1 THEN l_keyid = ds_indexes.key1.
      else if l_keycp = 2 THEN l_keyid = ds_indexes.key2.
      else if l_keycp = 3 THEN l_keyid = ds_indexes.key3.
      else if l_keycp = 4 THEN l_keyid = ds_indexes.key4.
      else if l_keycp = 5 THEN l_keyid = ds_indexes.key5.
      else if l_keycp = 6 THEN l_keyid = ds_indexes.key6.
      else if l_keycp = 7 THEN l_keyid = ds_indexes.key7.
      else if l_keycp = 8 THEN l_keyid = ds_indexes.key8.

      if l_keyid <> ? 
       then do: /* index-component */
        find first s_ttb_fld
          where s_ttb_fld.ttb_tbl  = RECID(s_ttb_tbl)
          and   s_ttb_fld.ds_stoff = l_keyid
          no-error.

        if not available s_ttb_fld
         then do:  /* was PROGRESS_RECID or shadow column */
                   /* Try to find 'real' column           */
          find first s_ttb_fld
            where s_ttb_fld.ttb_tbl = RECID(s_ttb_tbl)
            and   s_ttb_fld.ds_shd# = l_keyid
            no-error.
          if not available s_ttb_fld then next.
          end.     /* was PROGRESS_RECID or shadow column */
                   /* Try to find 'real' column           */
                   
        create s_ttb_idf.
        assign
          s_ttb_idf.pro_abbr  = FALSE
          s_ttb_idf.pro_asc   = TRUE
          s_ttb_idf.pro_order = l_keycp
          s_ttb_idf.ttb_fld   = RECID(s_ttb_fld)
          s_ttb_idf.ttb_idx   = RECID(s_ttb_idx).

        end.     /* index-component */
        
      end.     /* repeat l_keycp = 1 to 8 */
    
  &ELSEIF "{&db-type}" = "oracle"
   &THEN 

    assign i = 0.   /* i := number of date-time fields */
    for each ds_idx-cols
      fields({&objid} {&colid} pos#)
      where ds_idx-cols.{&objid} = ds_indexes.{&objid}
      no-lock
      by ds_idx-cols.pos# /*{&colid}*//**/:

      if i + ds_idx-cols.pos# > 16
       then do:  /* too many index-fields */
        run error_handling(4, 
                           string(s_ttb_idx.ds_name), 
                           string(ds_idx-cols.{&colid})
                          ).
        next.
        end.     /* too many index-fields */
        
      if s_ttb_tbl.ds_recid = ds_idx-cols.{&colid}
       then next.  /* progress_recid */
       
      find first s_ttb_fld
        where s_ttb_fld.ttb_tbl    =  RECID(s_ttb_tbl)
        and   s_ttb_fld.ds_stoff   =  ds_idx-cols.{&colid}
        and   s_ttb_fld.ds_stdtype <> 7 * 4096 /* not of type TIME */
        no-error.
      if not available s_ttb_fld
       then find first s_ttb_fld
        where s_ttb_fld.ttb_tbl    =  RECID(s_ttb_tbl)
        and   s_ttb_fld.ds_stoff   =  ds_idx-cols.{&colid}
        no-error.
      if not available s_ttb_fld
       then find first s_ttb_fld /* shadow-col */
        where s_ttb_fld.ttb_tbl    =  RECID(s_ttb_tbl)
        and   s_ttb_fld.ds_shd#    =  ds_idx-cols.{&colid}
        no-error.
      if not available s_ttb_fld
       then do:
        run error_handling(1, 
                           string(s_ttb_idx.ds_name), 
                           string(ds_idx-cols.{&colid})
                          ).
        next.
        end.

      create s_ttb_idf.
      assign
        s_ttb_idf.pro_abbr  = FALSE
        s_ttb_idf.pro_asc   = TRUE
        s_ttb_idf.pro_order = ds_idx-cols.pos# + i
        s_ttb_idf.ttb_fld   = RECID(s_ttb_fld)
        s_ttb_idf.ttb_idx   = RECID(s_ttb_idx).
      
      if s_ttb_fld.ds_type = "date"
       and NOT user_env[25] matches "*PROTOXXX*"
        /* marceau: DO NOT ADD TIME TO index 
         * hutegger: if it's run by a protoxxx tool; otherwise
         * do add time-fields to index (to prevent uniqueness problems)
         */
       then do:  /* add time-field to that index too */
       
        find first s_ttb_fld
          where s_ttb_fld.ttb_tbl    =  RECID(s_ttb_tbl)
          and   s_ttb_fld.ds_stoff   =  ds_idx-cols.{&colid}
          and   s_ttb_fld.ds_stdtype = 7 * 4096 /* type TIME */
          no-error.
        if available s_ttb_fld
         then do:
          create s_ttb_idf.
          assign
            i                   = i + 1
            s_ttb_idf.pro_abbr  = FALSE
            s_ttb_idf.pro_asc   = TRUE
            s_ttb_idf.pro_order = ds_idx-cols.pos# + i
            s_ttb_idf.ttb_fld   = RECID(s_ttb_fld)
            s_ttb_idf.ttb_idx   = RECID(s_ttb_idx).
          end.
          
        end.     /* add time-field to that index too */
        
      end.   /* for each ds_idx-cols */
    
  &ENDIF
  
    end.   /* for each ds_indexes */
  

&IF "{&db-type}" = "sybase"
 &THEN 

/*--------------- MANDATORY OF UNIQUE-INDEX COMPONENTS ---------------*/

/* check if all fields, part of unique-index, are mandatory and
 * eventually set mandatory to true
 */
  for each s_ttb_idx
    where s_ttb_idx.ttb_tbl  = RECID(s_ttb_tbl)
    and   s_ttb_idx.pro_uniq = TRUE:
    for each s_ttb_idf
      where s_ttb_idf.ttb_idx = RECID(s_ttb_idx):
      find first s_ttb_fld
        where /* s_ttb_fld.ttb_tbl = RECID(s_ttb_tbl)
        and   */ RECID(s_ttb_fld)  = s_ttb_idf.ttb_fld
        no-error.
      if available s_ttb_fld
       and s_ttb_fld.pro_mand = FALSE
       then assign s_ttb_fld.pro_mand = TRUE.
      end.     /* for each s_ttb_idf */
    end.     /* for each s_ttb_idx */

&ELSEIF "{&db-type}" = "oracle"
 &THEN 

/*--------------------------- RECID-INDEX ----------------------------*/

/* We are looking for a mandatory integer-field with an unique index.
 * if the field isn't mandatory and/or its precision, scale is out of
 * range and/or the index isn't unique, then the field/index is only
 * user-selectable (= Level 2). That means, he ahs to make sure, that
 * his application provides the adaquate checks for what ever the schema
 * isn't restrictive enough.
 * Otherwise the field/index is automatically selectable (= Level 1).
 */
 
  if  s_ttb_tbl.ds_recid <= 0
   or s_ttb_tbl.ds_recid  = ?
   then do:  /* no progress_recid -> check indexes for ROWID usability */
    
    /* check all indexes and calculate their usability-level */
    for each s_ttb_idx
      where s_ttb_idx.ttb_tbl = RECID(s_ttb_tbl)
   /* and   s_ttb_idx.ds_name <> ""*/ :
    
      find first s_ttb_idf 
        where s_ttb_idf.ttb_idx = RECID(s_ttb_idx)
        no-error.
      if not available s_ttb_idf then NEXT.

      find first s_ttb_fld 
        where RECID(s_ttb_fld) = s_ttb_idf.ttb_fld.
      find next s_ttb_idf 
        where s_ttb_idf.ttb_idx = RECID(s_ttb_idx)
        no-error.
      if not available s_ttb_fld            /* no components at all   */
       or available s_ttb_idf               /* more than 1 components */
       or s_ttb_fld.ds_type <> "NUMBER" then NEXT. /* wrong data-type */
       
      /* index has for-name & only one numeric field -> user-selectable */
      assign
        s_ttb_idx.ds_msc21   = "u"
        s_ttb_idx.hlp_fstoff = s_ttb_fld.ds_stoff.

      if  s_ttb_idx.pro_uniq = FALSE
       or s_ttb_fld.pro_mand = FALSE then NEXT.
      
      find first ds_columns
        where ds_columns.{&objid} = onum
        and   ds_columns.name     = s_ttb_fld.ds_name
        no-lock no-error.
      if   NOT AVAILABLE ds_columns
       or  {&precision} >= 10
       or  {&scale}     >   0
       then NEXT.  /* not automatically selectable */
      
      /* index is automatically selectable */
      assign s_ttb_idx.ds_msc21 = "a".

      end. /* for each s_ttb_idx */

    /* select first "a" index */
    find first s_ttb_idx
      where s_ttb_idx.ttb_tbl = RECID(s_ttb_tbl)
      and s_ttb_idx.ds_msc21 = "a"
      no-error.
    if available s_ttb_idx
     then assign
        s_ttb_idx.ds_msc21 = "ra" 
        s_ttb_tbl.ds_recid = s_ttb_idx.hlp_fstoff * -1
        s_ttb_tbl.ds_rowid = s_ttb_idx.pro_idx#.
     else assign
        s_ttb_tbl.ds_recid = ?
        s_ttb_tbl.ds_rowid = ?.
     
    end.     /* no progress_recid -> check indexes for ROWID usability */

  /* PROGRESS-RECID-field -> reset recid-info of all indexes */
   else for each s_ttb_idx
      where s_ttb_idx.ttb_tbl = RECID(s_ttb_tbl):
      assign s_ttb_idx.ds_msc21 = ?.
      end.

 &ENDIF


/* as last step we have to replace the synonyms' names and users */ 
  if gate-work.gate-type = "SYNONYM"
   then assign
     s_ttb_tbl.ds_name = namevar-s
     s_ttb_tbl.ds_user = "".


  end. /* for each gate-work */


&IF "{&db-type}" = "oracle"
 &THEN 

/*--------------------- PREPARE FOR NEXT LINK  ---------------------*/

/* we are done with this link, lets do the next one (srchd = false)
 * if there aren't anymore, we continue to the next step, wich is pulling the
 * object-names into the gate-work temp-table
 */

find first s_ttb_link
  where s_ttb_link.slctd  = TRUE
  and   s_ttb_link.srchd  = FALSE
  no-lock no-error.
if not available s_ttb_link
 then      /* done with pulling objects             */
           /*   -> ev. compare and/or transfer them */
  assign user_path = "*C,"
                   + ( if user_env[25] = "compare"
                         then "_gat_cmp,"
                         else ""
                     )
                   + "_gat_cro,_ora_lkx". /* clean-up */

 else do:  /* pull objects from next selected link */

  assign
    s_level   = s_ttb_link.level
    s_master  = s_ttb_link.master
    s_lnkname = s_ttb_link.name.

  if connected(s_ldbname)
   then disconnect value(s_ldbname) /*no-error*/.

  do transaction:
    find first DICTDB._Db
      where DICTDB._Db._Db-name = s_ldbname
      and   DICTDB._Db._Db-type = "ORACLE".
    assign DICTDB._Db._Db-misc2[8] = s_master + s_ttb_link.name.
    end.     /* transaction */

  assign  user_path = "*C,_ora_lkc,_ora_pul".

  end.     /* pull objects from next selected link */
&ENDIF

  
/**/&IF "{&DS_DEBUG}" = "DEBUG"
/**/ &THEN
/**/
/**/ message "gat_pul.i:  &ds_debug: {&DS_DEBUG}" view-as alert-box.
/**/
/**/ run error_handling(5, "*****----- END gat_pul.i!!! -----*****" ,"").
/**/
/**/  output stream s_stm_errors to gate-work.d.
/**/  for each gate-work no-lock: 
/**/    display stream s_stm_errors gate-work with stream-io width 255.
/**/    display  stream s_stm_errors gate-work.gate-edit format "x(30)".
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_tbl.d.
/**/  for each s_ttb_tbl no-lock: 
/**/    display stream s_stm_errors  
/**/      s_ttb_tbl.ds_msc13      format "zzzz9-"
/**/      s_ttb_tbl.ds_msc21      format "x(15)"
/**/      s_ttb_tbl.ds_msc22      format "x(15)"
/**/      s_ttb_tbl.ds_msc23      format "x(15)"
/**/      s_ttb_tbl.ds_msc24      format "x(15)"
/**/      s_ttb_tbl.ds_name       format "x(15)" skip
/**/      s_ttb_tbl.ds_recid      format "zzzz9-"
/**/      s_ttb_tbl.ds_rowid      format "zzzz9-"
/**/      s_ttb_tbl.ds_spcl       format "x(15)"
/**/      s_ttb_tbl.ds_type       format "x(15)"
/**/      s_ttb_tbl.ds_user       format "x(15)"
/**/      s_ttb_tbl.gate-work     format "zzzz9-"
/**/      s_ttb_tbl.pro_desc      format "x(15)" skip
/**/      s_ttb_tbl.pro_name      format "x(15)"
/**/      s_ttb_tbl.pro_prime-idx format "x(15)"
/**/      s_ttb_tbl.pro_recid     format "zzzz9-"
/**/      with stream-io width 255. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_fld.d.
/**/  for each s_ttb_fld no-lock: 
/**/    display stream s_stm_errors 
/**/     RECID(s_ttb_fld) format "zzzzzz9" label "RECID"
/**/     s_ttb_fld.pro_name s_ttb_fld.ttb_tbl  s_ttb_fld.pro_type
/**/     s_ttb_fld.ds_prec  format "zzz9-" label "m11"
/**/     s_ttb_fld.ds_scale format "zzz9-" label "m12"
/**/     s_ttb_fld.ds_lngth format "zzz9-" label "m13"
/**/     s_ttb_fld.ds_radix format "zzz9-" label "m14"
/**/     s_ttb_fld.ds_msc23 s_ttb_fld.ds_msc24
/**/     s_ttb_fld.ds_shdn  s_ttb_fld.ds_shd# format "zzz9-" label "sh#"
/**/     s_ttb_fld.ds_name  s_ttb_fld.ds_stoff s_ttb_fld.ds_type
/**/     s_ttb_fld.ds_stdtype format "zzzz9-" label "std"
/**/     with width 255. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_fl1.d.
/**/  for each s_ttb_fld no-lock: 
/**/    display stream s_stm_errors 
/**/     RECID(s_ttb_fld) format "zzzzzz9" label "RECID"
/**/     s_ttb_fld.pro_name  s_ttb_fld.ttb_tbl   s_ttb_fld.pro_case
/**/     s_ttb_fld.pro_dcml  s_ttb_fld.pro_desc  s_ttb_fld.pro_extnt
/**/     s_ttb_fld.pro_frmt  s_ttb_fld.pro_init  s_ttb_fld.pro_mand
/**/     s_ttb_fld.pro_order s_ttb_fld.pro_type
/**/     with width 255. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_idx.d.
/**/  for each s_ttb_idx no-lock: 
/**/    display stream s_stm_errors 
/**/     RECID(s_ttb_idx)     format "zzzzzz9" label "RECID"
/**/     s_ttb_idx.pro_name s_ttb_idx.ttb_tbl 
/**/     s_ttb_idx.pro_prim s_ttb_idx.pro_actv 
/**/     s_ttb_idx.pro_uniq    
/**/     s_ttb_idx.pro_idx#   format "zzz9-"    label "ix#"
/**/     s_ttb_idx.ds_name    s_ttb_idx.ds_msc21
/**/     s_ttb_idx.hlp_dtype  format "zzzz9-"  label "dty#"
/**/     s_ttb_idx.hlp_fld#   format "zzz9-"    label "fld#"
/**/     s_ttb_idx.hlp_fstoff format "zzz9-"    label "fst#"
/**/     s_ttb_idx.hlp_level  format "9"       label "L"
/**/     s_ttb_idx.hlp_mand   s_ttb_idx.hlp_msc23  
/**/     with width 255. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_idf.d.
/**/  for each s_ttb_idf no-lock: 
/**/    display stream s_stm_errors s_ttb_idf. 
/**/    end.
/**/  output stream s_stm_errors close.
/**/  output stream s_stm_errors to s_ttb_seq.d.
/**/  for each s_ttb_seq no-lock: 
/**/    display stream s_stm_errors s_ttb_seq except s_ttb_seq.ds_max. 
/**/    display stream s_stm_errors s_ttb_seq.ds_max format "-z,zzz,zzz,zz9". 
/**/    end.
/**/  output stream s_stm_errors close.
/**/
/**/ message "gat_pul.i: end of debug-output" skip "wait"
/**/    view-as alert-box.
/** / assign
/**/   user_path = ""
/**/   s_1st-error = FALSE.
/ **/
/**/  &ENDIF

if NOT batch-mode
 then SESSION:IMMEDIATE-DISPLAY = no.

RUN adecomm/_setcurs.p ("").

if NOT batch-mode
 then HIDE FRAME ds_make NO-PAUSE.

RETURN.

/*------------------------------------------------------------------*/
