/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ism/_ism_uid.p

Description:

    UserInterface for maintaining UserDefined DataTypes

    1. create ttb_uddt from _db-misc2[8]
    2. allow user to edit datatype-specs
    3. ev. adjust to new default-formats
    4. create _db-misc2[8] from ttb_uddt

    NOTE: update of 
            for-type
            id-number
            pro-type
            is allowed only if that datatype is not yet used
    NOTE: deelte only allowed if that datatype is not yet used
    NOTE: format changes make it into those fields of that datatype, that
          have the old default-format
          
Input-Parameters:  
    none
    
Output-Parameters: 
    none
    
Used/Modified Shared Objects:
    drec_db         used to identify the right _Db-record

    
History:
    gfs         12/06/94    disallow duplicate datatypes
    hutegger    94/09/01    creation
    
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

/*
define input-output parameter dbnum    as integer.

define variable   xyz        as   character no-undo extent 3 format "x(60)".
define variable   xyz        like DICTDB.zyx.xyz.

define buffer     xyz        for zyx.
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

define variable   canned     as logical.
define variable   i          as integer.
define variable   l_result   as logical.
define variable   l_stri     as character.
define variable   l_text1    as character.
define variable   l_text2    as character.
define variable   l_text3    as character.

define temp-table ttb_uddt
            field descr      as   character format "x(20)"
            field for-type   as   character format "x(10)"
            field stnd-lngth as   integer   format "zzz9"
            field id-number  as   integer   format "zzz9"
            field pro-type   as   character format "xx"
            field family     as   integer   format "z9"
            field dflt-frmt  as   character format "x(30)"
            field old-frmt   as   character format "x(30)"
            field used       as   logical   format "*/ "
            index uit        is unique         for-type
            index upi        is unique primary id-number.

define query      qry_uddt   for   ttb_uddt.
define browse     brw_uddt   query qry_uddt
          display descr      /*at  1*/ label "Description"   
                  for-type   /*at 21*/ label "ISAM-Type"    
                  stnd-lngth /*at 32*/ label "Ln"            
                  id-number  /*at 35*/ label "Num"           
                  pro-type   /*at 40*/ label "PT"            
            /*    family     /*at 43*/ label "Fm"   */         
                  used       /*at 46*/ label "U"
                  dflt-frmt  /*at 48*/ label "Default-Format"
&IF "{&WINDOW-SYSTEM}" <> "TTY"
 &THEN
          with size 78 by 14 /* font 0 */.
 &ELSE
          with size 70 by 10 /* font 0 */.
 &ENDIF
          
Define button     btn_add       label "&Add"               SIZE 19 BY 1.
Define button     btn_delete    label "&Delete selected"   SIZE 19 BY 1.
Define button     btn_update    label "&Update selected"   SIZE 19 BY 1.

&IF "{&WINDOW-SYSTEM}" <> "TTY"
 &THEN

  form
    " "                                                skip({&VM_WIDG})
    l_text1       at 2 format "x(72)"     view-as text skip
    l_text2       at 2 format "x(72)"     view-as text skip({&VM_WIDG})
    btn_add       at 2                                 space(7)
    btn_update                                         space(7)
    btn_delete                                         skip({&VM_WIDG})
    brw_uddt      at 2   
    {prodict/user/userbtns.i}
   with frame frm_uddt
    view-as dialog-box 
    row 2 centered no-labels scrollable
    title "  User-defined Data Types  "
    default-button btn_OK cancel-button btn_Cancel.

  form
    " "                                                     skip({&VM_WIDG})
    ttb_uddt.descr       colon 24 label "Description"       
      validate(ttb_uddt.descr <> "" AND ttb_uddt.descr <> ?,
      "You must specify a Description.")                    skip({&VM_WIDG})
    ttb_uddt.for-type    colon 24 label "ISAM-Type" 
      validate(ttb_uddt.for-type <> "" AND ttb_uddt.for-type <> ?,
      "You must specify an ISAM-Type.")                     skip({&VM_WIDG})
    ttb_uddt.stnd-lngth  colon 24 label "Default Length"    
      HELP "Leave the Default Length 0 if no default is desired" skip({&VM_WIDG})
    ttb_uddt.id-number   colon 24 label "ID-Number"         skip({&VM_WIDG})
    ttb_uddt.pro-type    colon 24 label "PROGRESS-Data Type" 
      validate(CAN-DO("c,da,de,i,l",ttb_uddt.pro-type),
      "Valid entries are c,da,de,i,l")                 
      HELP "(c)haracter,(da)te,(de)ecimal,(i)nteger,(l)ogical" skip({&VM_WIDG})
    ttb_uddt.dflt-frmt   colon 24 label "Default-Format"    skip({&VM_WIDG})
    ttb_uddt.used        colon 24 label "Already Used" 
                                        format "yes/no"     skip({&VM_WIDG})
    {prodict/user/userbtns.i}
   with frame frm_prop
    view-as dialog-box
    row 5 centered side-labels
    title "  User-defined Data Types  "
    default-button btn_OK cancel-button btn_Cancel.

 &ELSE      /* TTY-version */

  form
    l_text1       at 2 format "x(70)"     view-as text skip
    l_text2       at 2 format "x(70)"     view-as text skip
    l_text3       at 2 format "x(70)"     view-as text skip({&VM_WIDG})
    brw_uddt      at 2   
    {prodict/user/userbtns.i}
   with frame frm_uddt
    view-as dialog-box 
    row 2 centered no-labels scrollable
    title "  User-defined Data Types  "
    default-button btn_OK cancel-button btn_Cancel.

  form
    " "                                                          skip
    ttb_uddt.descr       colon 22 label "Description"            
      validate(ttb_uddt.descr <> "" AND ttb_uddt.descr <> ?,
      "You must specify a Description.")                         skip
    ttb_uddt.for-type    colon 22 label "ISAM-Type"         
      validate(ttb_uddt.for-type <> "" AND ttb_uddt.for-type <> ?,
      "You must specify an ISAM-Type.")                          skip
    ttb_uddt.stnd-lngth  colon 22 label "Default Length"      
      HELP "Leave the Default Length 0 if no default is desired" skip
    ttb_uddt.id-number   colon 22 label "ID-Number"              skip
    ttb_uddt.pro-type    colon 22 label "PROGRESS-Type" 
      validate(CAN-DO("c,da,de,i,l",ttb_uddt.pro-type),
      "Valid entries are c,da,de,i,l")                     
      HELP "(c)haracter,(da)te,(de)ecimal,(i)nteger,(l)ogical"   skip
    ttb_uddt.dflt-frmt   colon 22 label "Default-Format"         skip
    ttb_uddt.used        colon 22 label "Already Used" 
                                        format "yes/no"          skip
    {prodict/user/userbtns.i}
   with frame frm_prop
    view-as dialog-box
    row 5 centered side-labels
    title "  User-defined Data Types  "
    default-button btn_OK cancel-button btn_Cancel.
 &ENDIF

/*---------------------------  TRIGGERS  ---------------------------*/

&IF "{&WINDOW-SYSTEM}" <> "TTY"
 &THEN
  on  HELP of frame frm_uddt 
   or choose of btn_Help in frame frm_uddt
    RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
                              INPUT 0{&User_Def_Datatype_Dlg_Box},
                              INPUT ?).
  on  HELP of frame frm_prop 
   or choose of btn_Help in frame frm_uddt
    RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
                              INPUT 0{&User_Def_Datatype_Dlg_Box_Prop},
                              INPUT ?).
 &ENDIF

ON LEAVE OF ttb_uddt.for-type IN FRAME frm_prop DO:
    DEFINE BUFFER tbuf FOR ttb_uddt.
    FIND tbuf WHERE tbuf.for-type = self:SCREEN-VALUE
                and recid(tbuf) ne recid(ttb_uddt) NO-ERROR.
    IF AVAILABLE tbuf THEN DO:
        MESSAGE "You have already defined a Data Type of <" + 
            self:SCREEN-VALUE + ">" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
END. 
   
/*----- CLEAR or choose of DELETE buttton -----*/
&IF "{&WINDOW-SYSTEM}" = "TTY"
 &THEN on CLEAR  of brw_uddt   in frame frm_uddt do: 
 &ELSE on choose of btn_delete in frame frm_uddt do:
 &ENDIF
  
  define variable l_answer   as logical.

  if brw_uddt:num-selected-rows in frame frm_uddt = 0
   then do:  /* nothing selected to delete */
    message "There is no Data Type selected to be deleted."
      view-as alert-box.
    return NO-APPLY.
    end.     /* nothing selected to delete */

  if ttb_uddt.used = TRUE
   then do:  /* can't delete used data-types */
    message "There are fields defined with this Data Type. Delete not allowed."
      view-as alert-box.
    return NO-APPLY.
    end.     /* can't delete used data-types */

  message "Are you sure you want to delete the Data Type" ttb_uddt.descr "?"
    view-as alert-box question buttons yes-no update l_answer.

  if l_answer
   then do:
    delete ttb_uddt.
    close query qry_uddt.
    open query qry_uddt for each ttb_uddt.
    end.

  apply "entry" to brw_uddt in frame frm_uddt.
      
  end.   /* on CLEAR of frame frm_uddt */

/*----- INSERT or choose of ADD button -----*/
&IF "{&WINDOW-SYSTEM}" = "TTY"
/* &THEN on INSERT of brw_uddt in frame frm_uddt do: */
 &THEN on PUT    of brw_uddt in frame frm_uddt do:
 &ELSE on choose of btn_add  in frame frm_uddt do:
 &ENDIF 
  
  define variable l_id-number as integer.
  define variable l_positn    as recid.
  
  close query qry_uddt.

  assign
    l_positn = ( if available ttb_uddt 
                    then recid(ttb_uddt)
                    else ?
               ).
               
/**/ disable brw_uddt with frame frm_uddt.
  do transaction
      on ERROR  undo, leave
      on ENDKEY undo, leave
      on STOP   undo, leave:
  
    find last ttb_uddt use-index upi no-error.
    if available ttb_uddt then assign l_id-number = ttb_uddt.id-number + 1.
                          else assign l_id-number = 0.
                    
    create ttb_uddt.
    assign
      l_positn             = recid(ttb_uddt)
      ttb_uddt.id-number   = l_id-number
      ttb_uddt.family      = 1
      ttb_uddt.used        = false
      frame frm_prop:title = " ADD User-defined Data Type ".
  
    display
      ttb_uddt.used
      with frame frm_prop.

    update 
      ttb_uddt.descr
      ttb_uddt.for-type
      ttb_uddt.stnd-lngth
      ttb_uddt.id-number
      ttb_uddt.pro-type
      ttb_uddt.dflt-frmt
      btn_OK
      btn_cancel
      {&HLP_BTN_NAME}
      with frame frm_prop.
    hide frame frm_prop.

   
    end.     /* transaction */
/**/ enable brw_uddt with frame frm_uddt.
    
    open query qry_uddt for each ttb_uddt.
    if l_positn <> ?
     then reposition qry_uddt to recid(l_positn).
    if available ttb_uddt
     then assign l_result = brw_uddt:select-focused-row().
    apply "entry" to brw_uddt in frame frm_uddt.
    
  end.   /* on INSERT of frame frm_uddt */

/*----- RECALL or choose of UPDATE button -----*/
&IF "{&WINDOW-SYSTEM}" = "TTY"
 &THEN on RECALL of brw_uddt   in frame frm_uddt do:
 &ELSE on choose of btn_update in frame frm_uddt do:
 &ENDIF
 
  define variable l_positn   as recid.
  
  if brw_uddt:num-selected-rows in frame frm_uddt = 0
   then do:  /* nothing selected to update */
    message "There is no Data Type selected to be updated."
      view-as alert-box.
    return NO-APPLY.
    end.     /* nothing selected to update */
 
  assign
    l_positn = ( if available ttb_uddt 
                    then recid(ttb_uddt)
                    else ?
               )
    frame frm_prop:title = " UPDATE User-defined Data Type ".
               
 
/* */ disable brw_uddt with frame frm_uddt. /**/
  do transaction
      on ERROR  undo, leave
      on ENDKEY undo, leave
      on STOP   undo, leave:
    if ttb_uddt.used = TRUE
     then do:  /* datatype already used */
      display
        ttb_uddt.for-type
        ttb_uddt.id-number
        ttb_uddt.pro-type
        ttb_uddt.used
        with frame frm_prop.
      update 
        ttb_uddt.descr
        ttb_uddt.stnd-lngth
        ttb_uddt.dflt-frmt
        btn_OK
        btn_cancel
        {&HLP_BTN_NAME}
        with frame frm_prop.
      end.     /* datatype already used */
     else do:  /* datatype not yet used */
      display
        ttb_uddt.used
        with frame frm_prop.
      update 
        ttb_uddt.descr
        ttb_uddt.for-type
        ttb_uddt.stnd-lngth
        ttb_uddt.id-number
        ttb_uddt.pro-type
        ttb_uddt.dflt-frmt
        btn_OK
        btn_cancel
        {&HLP_BTN_NAME}
        with frame frm_prop.
      end.     /* datatype not yet used */
    hide frame frm_prop.

    end.     /* transaction */
/**/ enable brw_uddt with frame frm_uddt. /**/
    
    close query qry_uddt.
    open query qry_uddt for each ttb_uddt.
    reposition qry_uddt to recid(l_positn).
    assign l_result = brw_uddt:select-focused-row().
    apply "entry" to brw_uddt in frame frm_uddt.
    
  end.   /* on RECALL of frame frm_uddt */

/*----- GO or choose of OK button -----*/
on GO of frame frm_uddt do: /* or OK because of auto-go */

  define variable i          as integer.
  define variable l_answer   as logical.
  
  assign i = 0.
  find first ttb_uddt
    where ttb_uddt.id-number = i
    no-error.
  repeat while available ttb_uddt:
    assign i = i + 1.
    find first ttb_uddt
      where ttb_uddt.id-number = i
      no-error.
    end.
  find first ttb_uddt
    where ttb_uddt.id-number > i
    no-error.

  if available ttb_uddt
   then do:  /* there's a hole in the sequence */
    message
      "There's a hole in the id-number-sequence."      skip
      "All Data Types with numbers higher than" i - 1
                                   " will be ignored." skip
      "Is this is your intention?"
      view-as alert-box error buttons yes-no update l_answer.
    if not l_answer then RETURN no-apply.
    end.     /* there's a hole in the sequence */
    
  end.   /* on GO of frame frm_uddt */

/*----- WINDOW-CLOSE of dialog -----*/
on window-close of frame frm_uddt
   apply "END-ERROR" to frame frm_uddt.

on window-close of frame frm_prop
   apply "END-ERROR" to frame frm_prop.

/*
on CANCEL of frame frm_prop
  apply "END-ERROR" to frame frm_prop.
*/

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

/* Run time layout for button areas. */
{adecomm/okrun.i  
   &FRAME  = "frame frm_uddt" 
   &BOX    = "rect_Btns"
   &OK     = "btn_OK" 
   {&CAN_BTN}
   {&HLP_BTN}
}

{adecomm/okrun.i  
   &FRAME  = "frame frm_prop" 
   &BOX    = "rect_Btns"
   &OK     = "btn_OK" 
   {&CAN_BTN}
   {&HLP_BTN}
}


/********** create ttb_uddt from _db-misc2[8] **********/

find first DICTDB._Db where RECID(DICTDB._Db) = drec_db no-lock no-error.
if not available DICTDB._Db then leave.

repeat i = 1 to num-entries(_Db._Db-misc2[8],"~\"):
  create ttb_uddt.
  assign
    l_stri              =         entry(i,_Db._Db-misc2[8],"~\")
    ttb_uddt.descr      =         entry(1,l_stri,"|")
    ttb_uddt.for-type   =         entry(2,l_stri,"|")
    ttb_uddt.stnd-lngth = integer(entry(3,l_stri,"|"))
    ttb_uddt.id-number  = integer(entry(4,l_stri,"|")) - 100
    ttb_uddt.pro-type   =         entry(5,l_stri,"|")
    ttb_uddt.family     = integer(entry(6,l_stri,"|"))
    ttb_uddt.dflt-frmt  =         entry(7,l_stri,"|")
    ttb_uddt.old-frmt   =         entry(7,l_stri,"|").
  end.
  
for each ttb_uddt:  /* determine if that datatype is already used */
  for each _Field
    where _Field._for-type = ttb_uddt.for-type
    no-lock 
    while ttb_uddt.used = false:
    find _file of _Field.
    if _file._db-recid = drec_db then assign ttb_uddt.used = true.
    end.
  end.  /* for each ttb_uddt */


/********** allow user to edit datatype-specs **********/

open query qry_uddt for each ttb_uddt.

assign
  canned  = true
  l_text1 = "Below is the list of ISAM-Data Types which are defined."
&IF "{&WINDOW-SYSTEM}" <> "TTY"
 &THEN
  l_text2 = "You may add a new type, or select one from the list to update or delete."
 &ELSE
  l_text2 = "You may add a new type(F6), or select one from the list"
  l_text3 = "to update(F7) or delete(F8)."
 &ENDIF
  brw_uddt:max-data-guess = 100.

display
  l_text1
  l_text2
&IF "{&WINDOW-SYSTEM}" = "TTY"
 &THEN
  l_text3
 &ENDIF
  with frame frm_uddt.
  
&IF "{&WINDOW-SYSTEM}" <> "TTY"
 &THEN
  enable 
    btn_add
    btn_update
    btn_delete
    brw_uddt
    btn_OK
    btn_Cancel
    {&HLP_BTN_NAME}
    with frame frm_uddt.
 &ELSE
  enable 
    brw_uddt
    btn_OK
    btn_Cancel
    {&HLP_BTN_NAME}
    with frame frm_uddt.
 &ENDIF
  
find first ttb_uddt no-error.
if available ttb_uddt
 then assign l_result = brw_uddt:select-focused-row().
apply "entry" to brw_uddt in frame frm_uddt.

do on endkey undo, leave:
  wait-for go of frame frm_uddt.
  assign canned  = false.
  end.
  
hide frame frm_uddt.

if canned then leave.


/********** ev. adjust to new default-formats **********/

for each ttb_uddt
  where ttb_uddt.used      =  TRUE
  and   ttb_uddt.dflt-frmt <> ttb_uddt.old-frmt:  /* format changes */
  for each _Field
    where _Field._for-type = ttb_uddt.for-type
    no-lock:
    find _file of _Field.
    if   _file._db-recid = drec_db
     and _field._Format  = ttb_uddt.old-frmt
     then assign _Field._Format = ttb_uddt.dflt-frmt.
    end.
  end.  /* for each ttb_uddt */


/********** create _db-misc2[8] from ttb_uddt **********/

assign l_stri = "".
for each ttb_uddt:
  assign 
    l_stri = l_stri                            + "~\"
           +        ttb_uddt.descr             + "|"
           +        ttb_uddt.for-type          + "|"
           + string(ttb_uddt.stnd-lngth)       + "|"
           + string(ttb_uddt.id-number + 100)  + "|"
           +        ttb_uddt.pro-type          + "|"
           + string(ttb_uddt.family)           + "|"
           +        ttb_uddt.dflt-frmt        .
  end.

find first _Db where RECID(_Db) = drec_db exclusive-lock.
assign _Db._Db-misc2[8] = substring(l_stri,2).

/*------------------------------------------------------------------*/
