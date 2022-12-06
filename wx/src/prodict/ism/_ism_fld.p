/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* ism_fld - field editor for C-ISAM and NetISAM files */

/*
dfields is NOT AVAILABLE to create, otherwise contains record to
UPDATE.

When you come into this routine, the field name is already set on the
form.

_File._For-name      = Foreign file physical file name.
_Field._Fld-stoff    = Storage position in record.
_Field._Fld-stlen    = Absolute storage length (bytes, or bits for "Bits").
_Field._Fld-stdtype  = Storage datatype.
_Field._For-type     = Foreign field datatype.
_Field._For-spacing  = spacing for array structures

See _ism_typ.p for complete datatype summary
*/

DEFINE INPUT  PARAMETER ronly   AS CHARACTER             NO-UNDO.
DEFINE INPUT  PARAMETER junk2   AS RECID                 NO-UNDO.
DEFINE OUTPUT PARAMETER changed AS LOGICAL INITIAL FALSE NO-UNDO.

DEFINE SHARED BUFFER dfields FOR _Field.

DEFINE VARIABLE answer      AS LOGICAL               NO-UNDO.
DEFINE VARIABLE c           AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ism_size    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ism_stdtype AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ism_type    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ism_desc    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ism_type_ix AS INTEGER               NO-UNDO.
DEFINE VARIABLE copied      AS LOGICAL               NO-UNDO.
DEFINE VARIABLE i           AS INTEGER               NO-UNDO.
DEFINE VARIABLE inindex     AS LOGICAL               NO-UNDO.
DEFINE VARIABLE inother     AS LOGICAL               NO-UNDO.
DEFINE VARIABLE inview      AS LOGICAL               NO-UNDO.
DEFINE VARIABLE j           AS INTEGER               NO-UNDO.
DEFINE VARIABLE neworder    AS INTEGER               NO-UNDO.
DEFINE VARIABLE newpos      AS INTEGER               NO-UNDO.
DEFINE VARIABLE foreign     AS LOGICAL               NO-UNDO.
DEFINE VARIABLE pro_format  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE pro_type    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE ok_types    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE y_fld-stlen AS INTEGER               NO-UNDO.
DEFINE VARIABLE pi          AS INTEGER               NO-UNDO.
DEFINE VARIABLE pj          AS INTEGER               NO-UNDO.
DEFINE VARIABLE px          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE py          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE pz          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE new_stlen   AS INTEGER               NO-UNDO.

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userpik.i NEW }

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
  DEFINE VARIABLE new_lang AS CHARACTER EXTENT 13 NO-UNDO INITIAL [
  /* 1*/ "This field is used in a View or Index - cannot delete.",
  /* 2*/ "This field is used in a View - cannot rename.",
  /* 3*/ "Are you sure that you want to delete the field named",
  /* 4*/ "The field name you have supplied is not a valid PROGRESS identifier",
  /* 5*/ ?, /* see below */
  /* 6*/ "You must enter a field name here",
  /* 7*/ "Attempt to add with same name as existing field -switching to MODIFY",
  /* 8*/ "There is a field in these tables", /* make sure 4..6 fit in the */
  /* 9*/ "with the same name, pick one to",  /* frame format 'x(31)'      */
  /*10*/ ?, /*see below*/                    /* frame name 'frm_top'      */
  /*11*/ "create new field",
  /*12*/ "This is not an equivalent PROGRESS datatype for the ISAM datatype",
  /*13*/ ? /* see below */
].
ASSIGN
  new_lang[ 5] = "Bad " + user_dbtype
               + " datatype or datatype/length combination."
  new_lang[10] = "copy, or press [" + KBLABEL("END-ERROR") + "]."
  new_lang[13] = "This PROGRESS-datatype can't be used with this" 
               + user_dbtype + " datatype.".

FORMAT
  dfields._Field-name   LABEL "  Field-Name" FORMAT "x(32)"
        VALIDATE(KEYWORD(dfields._Field-name) = ?,
                 "This name conflicts with a PROGRESS reserved keyword.") SPACE
  dfields._For-type     LABEL    " ISAM-Type" FORMAT "x(12)"
        HELP "Help is available on the ISAM datatypes."  SKIP


  dfields._Format       LABEL "      Format" FORMAT "x(30)"
  "PROGRESS Type: (" SPACE(0) dfields._Data-type FORMAT "x(9)"
          NO-LABELS NO-ATTR-SPACE SPACE(0) ")" SKIP

  dfields._Label        LABEL "       Label" FORMAT "x(30)" SPACE(1)
  dfields._Fld-stlen    LABEL   "    Length" FORMAT ">>>>9"
        VALIDATE(dfields._Fld-stlen > 0
                 OR CAN-DO("int,long,nat*,iint,ilong,byte,*date*",
                 INPUT FRAME ism_fld dfields._For-type),
                 "Field length must be greater than 0.")
        HELP "Measured in bytes" 
  dfields._Fld-stoff    LABEL     "Position" FORMAT ">>>>9"
        VALIDATE(dfields._Fld-stoff <> ? AND
             dfields._Fld-stoff + INPUT fRAME ism_fld dfields._Fld-stlen <= _File._For-size,
      	    "Position must be supplied. Position plus Length must fit within record size.")
       	               	               	               	              SKIP
  dfields._Col-label    LABEL "Column-label" FORMAT "x(30)" SPACE(1)
  dfields._Extent       LABEL   "    Extent" FORMAT ">>>>"
  dfields._For-spacing  LABEL    "  Spacing" FORMAT ">>>>"
    VALIDATE(dfields._For-spacing >= dfields._Fld-stlen,
      "Spacing cannot be less than field size")
    HELP "Number of bytes between occurences of this field in an array" SKIP

  dfields._Initial      LABEL "     Initial" FORMAT "x(30)"
          VALIDATE(NOT(dfields._Initial = "today" AND
           CAN-DO("*date*", INPUT FRAME ism_fld dfields._Data-type) ) ,
                "Initial value of TODAY is not supported")
  dfields._Decimals     LABEL    " Decimals" FORMAT ">>>>9"
        VALIDATE(dfields._Decimals <> ?,"You must enter a value here")
  dfields._Order        LABEL     "   Order" FORMAT ">>>>9" SKIP

  dfields._Valexp       LABEL "Valexp"       VIEW-AS EDITOR
       	       	     	      	       	     INNER-CHARS 63 INNER-LINES 4
      	       	     	      	       	     BUFFER-LINES 4 SKIP
  dfields._Valmsg       LABEL "Valmsg"       FORMAT "x(63)" SKIP
  dfields._Help         LABEL "  Help"       FORMAT "x(63)" SKIP
  dfields._Desc         LABEL "  Desc"       FORMAT "x(70)" SKIP

  HEADER ""
  WITH FRAME ism_fld NO-BOX ATTR-SPACE OVERLAY SIDE-LABELS
  ROW (SCREEN-LINES - 19) COLUMNS 1.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

FORM
  new_lang[8] FORMAT "x(37)" SKIP
  new_lang[9] FORMAT "x(37)" SKIP
  new_lang[10] FORMAT "x(37)" SKIP
  WITH FRAME frm_top OVERLAY NO-ATTR-SPACE NO-LABELS
  ROW pik_row - 5 COLUMN pik_column.

/*--------------------------Triggers-----------------------------*/

/*---- LEAVE of progress Data-type field ----*/
ON LEAVE OF dfields._Data-type IN FRAME ism_fld DO:

  define variable l_dt-new      as character.
  define variable l_dt-old      as character.
  define variable l_format      as character.
  define variable l_init        as character.
  define variable l_length      as integer.
  
  if SELF:SCREEN-VALUE <> dfields._Data-type
   then do:  /* user changed data-type */
   
    /* Get the list of Progress types that map to this gateway type */
    ASSIGN
       ok_types = "get-list"
       c = ?
       i = ?
       px = dfields._For-type:SCREEN-VALUE IN FRAME ism_fld.
    RUN "prodict/ism/_ism_typ.p"
      (INPUT-OUTPUT i,INPUT-OUTPUT i,
       INPUT-OUTPUT ok_types, INPUT-OUTPUT px, 
       OUTPUT c).
    
    /* check if data-type is valid for this ism-type */
    if not can-do(ok_types, SELF:SCREEN-VALUE)
     then do:
      bell.
      message new_lang[13].
      return NO-APPLY.
      end.     /* wrong data-type */
    
    /* check if format needs to be changed */
    assign
      l_dt-new = SELF:SCREEN-VALUE
      l_dt-old = dfields._Data-type.
    if  ( l_dt-new = "character" and l_dt-old <> "character" )
     or ( l_dt-new = "date"      and l_dt-old <> "date"      )
     or ( l_dt-new = "logical"   and l_dt-old <> "logical"   )
     or ( can-do("integer,decimal",l_dt-new) 
                  and not can-do("integer,decimal",l_dt-old) )
     then do:  /* change format, length and ev. initial */
      assign
        ism_type_ix = LOOKUP(INPUT frame ism_fld dfields._For-type, ism_type)
        l_format    = ENTRY(ism_type_ix, pro_format)
        l_length    = integer(dfields._Fld-stlen:SCREEN-VALUE IN FRAME ism_fld)
        l_init      = INPUT frame ism_fld dfields._Initial.
      
      case l_format:
        when    "c" then assign l_format = "x(" + string(l_length) + ")".
        when    "d" then assign l_format = ( if l_length = 8
                                                then "99/99/99"
                                                else "99/99/9999"
                                           ).
        when    "i" 
        or when "#" then assign l_format = fill("9",l_length).
        when    "l" then assign l_format = "yes/no".
        end case.
        
      display
        l_format @ dfields._Format
        WITH FRAME ism_fld.
    
      if   l_init   <> ""
       and l_init   <> ?
       and l_dt-new <> "character"
       then do:  /* change initial-value */
        assign l_init = "".
        display
          l_init @ dfields._Initial
          WITH FRAME rms_fld.
        end.     /* change initial-value */

      end.     /* change format, length and ev. initial */

    end.     /* user changed data-type */
     
END.

/*---- LEAVE of foreign type field ----*/
ON LEAVE OF dfields._For-type IN FRAME ism_fld DO:
  IF NOT CAN-DO(ism_type, SELF:SCREEN-VALUE) THEN DO:
    BELL.
    MESSAGE new_lang[5].
    RETURN NO-APPLY.
  END.


  /* Display the default progress type for the foreign type entered.  
     If user typed a different pro type, don't overwrite it.
  */
  ism_type_ix  = LOOKUP(INPUT frame ism_fld dfields._For-type, ism_type).
  IF dfields._Data-type:SCREEN-VALUE IN FRAME ism_fld = "" THEN 
    DISPLAY ENTRY(ism_type_ix, pro_type) @ dfields._Data-type
      WITH FRAME ism_fld.

  /* Show the default length.  Don't override a length with 0 and if
     field's in an index, length can't change so leave it.
  */
  new_stlen = INTEGER(ENTRY(ism_type_ix, ism_size)).
  IF NOT inindex AND new_stlen > 0 THEN
    DISPLAY new_stlen @ dfields._Fld-stlen 
      WITH FRAME ism_fld.

  /* Get the list of Progress types that map to this gateway type */
  ASSIGN
     ok_types = "get-list"
     c = ?
     i = ?
     px = dfields._For-type:SCREEN-VALUE IN FRAME ism_fld.
  RUN "prodict/ism/_ism_typ.p"
    (INPUT-OUTPUT i,INPUT-OUTPUT i,
     INPUT-OUTPUT ok_types, INPUT-OUTPUT px, 
     OUTPUT c).

  /* pro type may or may not be strictly dictated by foreign type */
  dfields._Data-type:SENSITIVE IN FRAME ism_fld = (NUM-ENTRIES(ok_types) > 1).
END.

/*---- GET or HELP of foreign type field---- */
ON GET,HELP OF dfields._For-type IN FRAME ism_fld DO:

  RUN "prodict/user/_usrpick.p".
  IF pik_first <> ? THEN DO:
    /* Show the gateway data type they chose */
    DISPLAY
      SUBSTRING(pik_first,1,INDEX(pik_first,":") - 1,"character")
                                                     @ dfields._For-type
      WITH FRAME ism_fld.

    /* Show the corresponding progress type */
    ism_type_ix = LOOKUP(INPUT frame ism_fld dfields._For-type, ism_type).
    DISPLAY ENTRY(ism_type_ix, pro_type) @ dfields._Data-type
      WITH FRAME ism_fld.

    /* Show the default length.  Don't override a length with 0 and if
       field's in an index, length can't change so leave it.
    */
    new_stlen = INTEGER(ENTRY(ism_type_ix, ism_size)).
    IF NOT inindex AND new_stlen > 0 THEN
      DISPLAY new_stlen @ dfields._Fld-stlen 
        WITH FRAME ism_fld.
  END.
END.

/*----- Any ASCII key in foreign type field -----*/
ON ANY-PRINTABLE OF dfields._For-type IN FRAME ism_fld DO:
  /* blank out the pro type if they change the gate type */
  IF dfields._Data-type:SCREEN-VALUE IN FRAME ism_fld <> "" THEN 
    DISPLAY "" @ dfields._Data-type WITH FRAME ism_fld.
END.


/*-----------------------Mainline code---------------------------*/

ASSIGN
  c = ?
  i = ?.
RUN "prodict/ism/_ism_typ.p"
  (INPUT-OUTPUT i,INPUT-OUTPUT i,INPUT-OUTPUT c,INPUT-OUTPUT c,OUTPUT c).
ASSIGN
  ism_desc    = user_env[11]  user_env[11] = ""
  ism_type    = user_env[12]  user_env[12] = ""
  ism_size    = user_env[13]  user_env[13] = ""
  ism_stdtype = user_env[14]  user_env[14] = ""
  pro_type    = user_env[15]  user_env[15] = ""
  /* gate-family */           user_env[16] = ""
  pro_format  = user_env[17]  user_env[17] = "".

IF NOT AVAILABLE dfields THEN
DO:
  CLEAR FRAME ism_fld NO-PAUSE.
  DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
    PROMPT-FOR _Field-name WITH FRAME ism_fld.
    IF INPUT FRAME ism_fld _Field-name = "" THEN
    DO:
      MESSAGE new_lang[6]. /* nothing entered! */
      UNDO,RETRY.
    END.
  END.
  IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN
  DO:
    HIDE FRAME ism_fld NO-PAUSE.
    RETURN.
  END.
  FIND FIRST dfields WHERE dfields._File-recid = drec_file
    AND dfields._field-name = INPUT FRAME ism_fld _Field-name NO-ERROR.
  IF AVAILABLE dfields THEN
  MESSAGE new_lang[7].
END.

copied = FALSE.
IF AVAILABLE dfields THEN DO:
  ASSIGN
    inindex    = CAN-FIND(FIRST _Index-field OF dfields)
    inview     = CAN-FIND(FIRST _View-ref
                 WHERE _View-ref._Ref-Table = user_filename
                   AND _View-ref._Base-Col = dfields._Field-name)
    neworder   = dfields._Order
    newpos     = dfields._Fld-stoff.
END.
ELSE DO:
  FIND LAST _Field USE-INDEX _Field-Position
    WHERE _Field._File-recid = drec_file NO-ERROR.
  ASSIGN
    inindex    = FALSE
    inview     = FALSE
    inother    = CAN-FIND(FIRST _Field WHERE _Field._Field-name =
                 INPUT FRAME ism_fld dfields._Field-name)
    neworder   = (IF AVAILABLE _Field
                 THEN _Field._Order + 10 ELSE 10)
    newpos     = (IF AVAILABLE _Field THEN
                 _Field._Fld-stoff
                   + (IF _Field._Fld-stlen < _Field._For-spacing
                     THEN _Field._For-spacing ELSE _Field._Fld-stlen)
                   * (IF _Field._Extent > 1 THEN _Field._Extent ELSE 1)
                 ELSE ?)
    pik_column = 40
    pik_row    = SCREEN-LINES - 10
    pik_hide   = TRUE
    pik_init   = ""
    pik_title  = ""
    pik_list   = ""
    pik_wide   = FALSE
    pik_multi  = FALSE
    pik_number = FALSE
    pik_count  = 1
    pik_list[1] = "<<" + new_lang[11] + ">>". /* <<create new field>> */

  IF inother THEN
    FOR EACH _Field
      WHERE _Field._Field-name = INPUT FRAME ism_fld dfields._Field-name,
    EACH _File OF _Field
      WHERE _File._Db-recid = drec_db AND RECID(_File) <> drec_file
    BY _File._File-name:
    ASSIGN
      pik_count = pik_count + 1
      pik_list[pik_count] = _File._File-name.
  END.
  /* Since we can't tell if a field is in another (non-progress)
     schema, we have to wait until we attempt the join above to eliminate
     those candidates.  hence, the following test: 
  */
  IF pik_count <= 1 THEN inother = FALSE.

  IF inother THEN _in-other: DO:
    PAUSE 0.
    DISPLAY new_lang[8 FOR 3] WITH FRAME frm_top.
    RUN "prodict/user/_usrpick.p".
    HIDE FRAME frm_top NO-PAUSE.
    IF pik_return = 0 OR pik_first BEGINS "<<" THEN LEAVE _in-other.
    FIND _File WHERE _File._Db-recid = drec_db AND _File._File-name = pik_first.
    FIND _Field OF _File WHERE _Field._Field-name =
      INPUT FRAME ism_fld dfields._Field-name.
    ASSIGN
      copied     = TRUE.
    DISPLAY
      _Field._Field-name  @ dfields._Field-name /*match case*/
      _Field._For-type    @ dfields._For-type
      _Field._Format      @ dfields._Format
      _Field._Label       @ dfields._Label
      _Field._Col-label   @ dfields._Col-label
      _Field._Initial     @ dfields._Initial
      _Field._Extent      @ dfields._Extent
      _Field._For-spacing @ dfields._For-spacing
      _Field._Decimals WHEN _Field._Data-type = "DECIMAL"
                          @ dfields._Decimals
      neworder            @ dfields._Order
      newpos              @ dfields._Fld-stoff
      _Field._Fld-stlen   @ dfields._Fld-stlen
      _Field._Valmsg      @ dfields._Valmsg
      _Field._Help        @ dfields._Help
      _Field._Desc        @ dfields._Desc
      WITH FRAME ism_fld.

    /* Can't seem to do @ on a view-as editor widget so: */
      ASSIGN dfields._Valexp:screen-value in frame ism_fld = _Field._Valexp.
  END.


  CREATE dfields.
  ASSIGN
    dfields._File-recid = drec_file
    dfields._Fld-case   = TRUE.

END.

ASSIGN
  pik_column = 40
  pik_down   = 0
  pik_hide   = TRUE
  pik_init   = ""
  pik_list   = ""
  pik_multi  = FALSE
  pik_number = FALSE
  pik_row    = 3
  pik_skip   = FALSE
  pik_title  = ""
  pik_wide   = FALSE.

j = 1.
DO i = 1 TO NUM-ENTRIES(ism_type) - 1:
  /* There are some multiple entries in the table with the same foreign
     type but different progress types.  They are always contiguous.
     Only show the first one.
  */
  IF i = 1 OR ENTRY(i, ism_type) <> ENTRY(i - 1, ism_type) THEN DO:
    pik_list[j] = STRING(ENTRY(i,ism_type) + ":","x(11)") + ENTRY(i,ism_desc).
    j = j + 1.
  END.
END.
pik_count = j - 1.

ASSIGN
  ism_desc = "" /* save RAM */
  c        = (IF NEW dfields THEN ? ELSE dfields._Data-type).

IF NOT NEW dfields THEN
  DISPLAY
    dfields._Field-name /*match case*/
    dfields._For-type
    dfields._Data-type
    dfields._Format
    dfields._Label
    dfields._Col-label
    dfields._Initial
    dfields._Extent
    dfields._For-spacing
    dfields._Decimals WHEN dfields._Data-type = "DECIMAL"
    neworder @ dfields._Order
    newpos   @ dfields._Fld-stoff
    dfields._Fld-stlen
    dfields._Valexp
    dfields._Valmsg
    dfields._Help
    dfields._Desc
    WITH FRAME ism_fld.

IF ronly = "r/o" THEN DO:
  { prodict/user/userpaus.i }
  HIDE FRAME ism_fld NO-PAUSE.
  RETURN.
END.

DO ON ERROR UNDO, RETRY ON ENDKEY UNDO, LEAVE:
  
  ASSIGN 
    /* to be used for length-spacing-automatism */
    y_fld-stlen = dfields._fld-stlen. 
  FIND _File WHERE RECID(_File) = drec_file.
  NEXT-PROMPT dfields._For-type WITH FRAME ism_fld.

  /* See triggers above */ 
  SET
    dfields._Field-name
    dfields._For-type
    dfields._Data-type  
        /* make sure progress type maps to the given foreign type */
        VALIDATE(CAN-DO(ok_types, dfields._Data-type), new_lang[12]) 
    dfields._Fld-stlen   WHEN NOT inindex
    dfields._Fld-stoff   WHEN NOT inindex
    dfields._Extent      WHEN NOT inindex
    WITH FRAME ism_fld.

  ASSIGN
    dfields._Fld-stlen = INPUT FRAME ism_fld dfields._Fld-stlen
    dfields._For-type  = INPUT FRAME ism_fld dfields._For-type
    pi                 = ?
    pj                 = dfields._Fld-stlen
    px                 = INPUT FRAME ism_fld dfields._Data-type
    py                 = dfields._For-type
    pz                 = ?
    newpos             = dfields._Fld-stoff.

  /* Get the data type code (stdtype) and the format */
  RUN "prodict/ism/_ism_typ.p" (
    INPUT-OUTPUT pi,INPUT-OUTPUT pj,
    INPUT-OUTPUT px,INPUT-OUTPUT py,OUTPUT pz).
 
  IF dfields._Data-type <> px THEN
    ASSIGN
      dfields._format = pz. /* if data type changed, reset format */

  /* the user might have entered a different length, we don't want to 
   * overwrite that! <hutegger> 94/09/29
   *IF dfields._Fld-stlen <> pj THEN dfields._Fld-stlen = pj.
   */
  IF dfields._Fld-stdtype <> pi THEN dfields._Fld-stdtype = pi.
  /* prevented user from changing format. */
  /* IF dfields._Format <> pz THEN dfields._Format = pz. */
  IF dfields._Extent > 1 AND
    (dfields._For-spacing = ? OR dfields._For-spacing < dfields._Fld-stlen)
    THEN ASSIGN dfields._For-spacing = dfields._Fld-stlen.

  IF dfields._Extent > 1 AND
    (dfields._For-spacing = y_fld-stlen
    AND y_fld-stlen > dfields._Fld-stlen)
    THEN DO:
      ASSIGN answer = true.
      MESSAGE
        "Before you edited Length, it was equal to Spacing." skip
        "Would you like to adjust Spacing too?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE answer.
      IF answer = ?  THEN UNDO, RETRY.
      ELSE IF answer THEN ASSIGN dfields._For-spacing = dfields._Fld-stlen.
      END.
  
  CASE dfields._data-type :
    WHEN    "DECIMAL"
    OR WHEN "INTEGER"
     THEN IF NOT CAN-DO("INTEGER,DECIMAL",c)
       THEN ASSIGN dfields._initial = "0".
       
    WHEN "CHARACTER"
     THEN IF NOT c = "CHARACTER" 
       THEN ASSIGN dfields._initial = "".
       
    WHEN "DATE"
     THEN IF NOT c = "DATE" 
       THEN ASSIGN dfields._initial = ?.
       
    WHEN "LOGICAL"
     THEN IF NOT c = "LOGICAL" 
       THEN ASSIGN dfields._initial = "NO".
       
    END CASE.
    
  DISPLAY
    dfields._Fld-stlen
    dfields._Format
    dfields._For-spacing
    dfields._initial
    WITH FRAME ism_fld.

  IF INPUT FRAME ism_fld dfields._Data-type = ? THEN UNDO,RETRY.

  IF dfields._Field-name ENTERED AND NOT NEW dfields AND inview THEN DO:
    MESSAGE new_lang[2]. /* sorry, used in view */
    UNDO,RETRY.
  END.
  /* I think SET takes care of this - remove it? */
  RUN "prodict/_dctname.p" (dfields._Field-name,OUTPUT answer).
  IF NOT answer THEN DO:
    MESSAGE new_lang[4]. /* bad name */
    UNDO,RETRY.
  END.

  IF dfields._Data-type = "character" THEN
    dfields._Decimals = dfields._Fld-stlen.
END.

IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN DO:
  HIDE FRAME ism_fld NO-PAUSE.
  IF NEW dfields THEN
    DELETE dfields.
  ELSE
    UNDO,RETURN.
  RETURN.
END.

IF NEW dfields AND NOT copied THEN
  DISPLAY
    dfields._Format
    dfields._Label
    dfields._Col-label
    dfields._Initial
    dfields._Extent
    dfields._For-spacing
    dfields._Decimals WHEN dfields._Data-type = "DECIMAL"
    neworder @ dfields._Order
    newpos   @ dfields._Fld-stoff
    dfields._Fld-stlen
    dfields._Valexp
    dfields._Valmsg
    dfields._Help
    dfields._Desc
    WITH FRAME ism_fld.

DO ON ERROR UNDO, RETRY ON ENDKEY UNDO, LEAVE:
  FIND _File WHERE RECID(_File) = drec_file.
  SET
    dfields._Format
    dfields._Label
    dfields._Col-label
    dfields._Initial
    dfields._For-spacing WHEN dfields._Extent > 0
    dfields._Decimals    WHEN dfields._Data-type = "DECIMAL"
    dfields._Order
    dfields._Valexp
    dfields._Valmsg
    dfields._Help
    dfields._Desc
    WITH FRAME ism_fld.

  ASSIGN
    dfields._Valexp = (IF TRIM(dfields._Valexp) = "" 
      	       	     	 THEN ? 
      	       	     	 ELSE TRIM(dfields._Valexp)).

  changed = TRUE.
END.

HIDE FRAME ism_fld NO-PAUSE.

IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN DO:
  IF NEW dfields THEN
    DELETE dfields.
  ELSE
    UNDO,RETURN.
END.

RETURN.
