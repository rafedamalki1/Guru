/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userpik.i NEW }

DEFINE VARIABLE idxfld AS CHARACTER NO-UNDO.

DEFINE VARIABLE c       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chr_def AS CHARACTER NO-UNDO.
DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ix      AS INTEGER   NO-UNDO.
DEFINE VARIABLE idxname AS CHARACTER NO-UNDO.
DEFINE VARIABLE j       AS INTEGER   NO-UNDO.
DEFINE VARIABLE l       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE pos     AS INTEGER   NO-UNDO.
DEFINE VARIABLE dbkey   AS RECID     NO-UNDO.
DEFINE VARIABLE idxnum  AS INTEGER   NO-UNDO.
DEFINE VARIABLE rec     AS CHARACTER NO-UNDO.
DEFINE BUFFER idxbuf FOR _Index.

DEFINE VARIABLE new_lang AS CHARACTER EXTENT 14 NO-UNDO INITIAL [
  /* 1*/ "In many ISAM-type files, multi-",
  /* 2*/ "component indexes are implemented as",
  /* 3*/ "a single component index that spans",
  /* 4*/ "multiple, contiguous fields.  In",
  /* 5*/ "order to take better advantage of",
  /* 6*/ "the index via Progress queries you",
  /* 7*/ "can define a new index based on all",
  /* 8*/ "the individual field components or",
  /* 9*/ "on just the leading field",
  /*10*/ "component(s).",

  /*11*/ "Each of the following sets of fields",
  /*12*/ "can be used to define an alternate",
  /*13*/ "index for the index chosen.",
  /*14*/ "Please choose which set to use."
].

FORM
  new_lang[1]  format "x(37)" SKIP
  new_lang[2]  format "x(37)" SKIP
  new_lang[3]  format "x(37)" SKIP
  new_lang[4]  format "x(37)" SKIP
  new_lang[5]  format "x(37)" SKIP
  new_lang[6]  format "x(37)" SKIP
  new_lang[7]  format "x(37)" SKIP
  new_lang[8]  format "x(37)" SKIP
  new_lang[9]  format "x(37)" SKIP
  new_lang[10] format "x(37)" SKIP
  WITH FRAME step-one ROW 4 COLUMN 1 NO-ATTR-SPACE NO-LABELS.

FORM
  new_lang[11] format "x(40)" SKIP
  new_lang[12] format "x(40)" SKIP
  new_lang[13] format "x(40)" SKIP
  new_lang[14] format "x(40)" SKIP
  WITH FRAME step-two ROW 3 COLUMN 1 NO-ATTR-SPACE NO-LABELS.

FORM
  SKIP({&TFM_WID})
  idxname AT 2 FORMAT "x(32)" LABEL "Enter name of new index"
  {prodict/user/userbtns.i}
  WITH FRAME idx-ask CENTERED ATTR-SPACE SIDE-LABELS
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE "Index Name".

/*------------------------------------------------------------------*/


/* Find all possible lists of fields that start at the same offset position 
   in the record as the first index field and overlaps all or the beginning
   of the index fields.
   e.g., If a field overlaps only the starting area of the index field
   this is a candidate for a new index.  Another good match is two fields
   that are contiguous and coincide with the area of a single index field.  
   The FOR EACH loop is used to find all fields that starts at the correct
   offset.  Each recursive call to gate_sub finds the next contiguous
   field that overlaps the area of the index field(s).
*/ 
PROCEDURE gate_sub.
  DEFINE INPUT PARAMETER rec AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER fld AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pos AS INTEGER   NO-UNDO.
  DEFINE VARIABLE wrk AS CHARACTER NO-UNDO.

  DEFINE BUFFER bField FOR _Field.

  pos = INDEX(rec,STRING(INDEX(rec,"a") > 0,"a/d")).

  FOR EACH bField
    WHERE bField._File-recid = drec_file
      AND bField._Fld-stoff = pos - 1 AND bField._Extent = 0:

    IF   INDEX(SUBSTRING(rec,pos,bField._Fld-stlen),"a") > 0
      OR INDEX(SUBSTRING(rec,pos,bField._Fld-stlen),"d") > 0 THEN DO:
      ASSIGN
        wrk = rec 
        OVERLAY(wrk,pos,bField._Fld-stlen) = FILL("o",bField._Fld-stlen)
        lst = (IF fld = "" THEN "" ELSE fld + ";") + bField._Field-name.

      IF lst <> idxfld THEN /* Do not include current components */
        ASSIGN
          pik_count           = pik_count + 1
          pik_list[pik_count] = lst.

      IF INDEX(wrk,"a") + INDEX(wrk,"d") > 0 THEN RUN gate_sub (wrk,lst).
    END.
  END.

END PROCEDURE.

/*------------------------------------------------------------------*/

{ prodict/dictgate.i &action=query &dbtype=user_dbtype &dbrec=? &output=rec }
/* grab a copy of the data-type definition tables */
ASSIGN
  rec = ENTRY(9,rec)
  c   = ?
  i   = ?.
RUN VALUE("prodict/" + rec + "/_" + rec + "_typ.p")
  (INPUT-OUTPUT i,INPUT-OUTPUT i,INPUT-OUTPUT c,INPUT-OUTPUT c,OUTPUT c).
/* the first entry will always be a standard, fixed-length character string */
ASSIGN
  chr_def      = ENTRY(1,user_env[12]) + "," + ENTRY(1,user_env[14])
                 /* gateway-data-type     ,    _fld-stdtype */
  user_env[11] = ""  /* save ram */
  user_env[12] = ""
  user_env[13] = ""
  user_env[14] = ""
  user_env[15] = ""
  user_env[16] = ""
  user_env[17] = "".

FIND _File WHERE RECID(_File) = drec_file.

FOR EACH _Index OF _File:
  ASSIGN
    pik_count           = pik_count + 1
    pik_list[pik_count] = _Index._Index-name.
END.
IF pik_count = 0 THEN DO:
  MESSAGE "Sorry, but there are no indexes to redefine" view-as alert-box.
  RETURN.
END.

ASSIGN
  pik_row    = 4
  pik_column = 41
  pik_multi  = FALSE
  pik_number = FALSE
  pik_wide   = FALSE
  pik_down   = MINIMUM(pik_count,11)
  pik_title  = "Select Indexes".

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   DISPLAY new_lang[1 for 10] WITH FRAME step-one.
   RUN "prodict/user/_usrpick.p".
   HIDE FRAME step-one NO-PAUSE.
&ELSE
   pik_text = "".
   DO ix = 1 TO 10:
     pik_text = pik_text + new_lang[ix] + "~n".
   END.
   pik_help = {&Redef_Index_IdxSel_Dlg_Box}.
   RUN "prodict/gui/_guipick.p".
&ENDIF

IF pik_return = 0 THEN DO:
  user_path = "".
  RETURN.
END.

FIND _Index OF _File WHERE _Index._Index-name = pik_first.
ASSIGN
  idxnum = _Index._Idx-num
  dbkey  = RECID(_Index)
  rec    = "".

/* rec will be set to a pattern of "a"'s or "d"'s showing where the
   ascending and descending field positions are within the record
   for the fields of this index.  e.g, If there are two index fields the
   first ascending, the other descending at pos 2, length 2 and at 
   pos 4, length 4, you'll get: 
   --aadddd
   (The fields will always be contiguous since this is all the backend
    supports.)
*/
FOR EACH _Index-field OF _Index,
  EACH _Field OF _Index-field
  BY _Field._Fld-stoff + _Field._Fld-stlen - 1 DESCENDING:
  ASSIGN
    rec = (IF rec = "" THEN FILL("-",_Fld-stoff + _Fld-stlen) ELSE rec)
    OVERLAY(rec,_Fld-stoff + 1,_Fld-stlen)
        = FILL(STRING(_Index-field._Ascending,"a/d"),_Fld-stlen)
    idxfld = _Field._Field-name + (IF idxfld = "" THEN "" ELSE ",") + idxfld.
END.

ASSIGN
  pik_count   = 1
  pik_list[1] = "<<Delete index: " + _Index._index-name + ">>"
  pik_wide    = TRUE
  pik_column  = 1
  pik_row     = 9
  pik_title  = "Select Index-Fields".
RUN gate_sub (rec,"").
IF pik_count = 1 THEN DO:
  MESSAGE "There is only one possible definition for this index"
       view-as alert-box.
  user_path = "".
  RETURN.
END.


&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  DISPLAY new_lang[11] new_lang[12] new_lang[13] new_lang[14]
      	  WITH FRAME step-two.
  RUN "prodict/user/_usrpick.p".
  HIDE FRAME step-two NO-PAUSE.
&ELSE
   pik_text = "".
   DO ix = 11 TO 14:
     pik_text = pik_text + new_lang[ix] + "~n".
   END.
   pik_help = {&Redef_Index_IdxFldSel_Dlg_Box}.
   RUN "prodict/gui/_guipick.p".
&ENDIF

IF pik_return > 0 THEN DO TRANSACTION ON ENDKEY UNDO,LEAVE:

  IF pik_first BEGINS "<<" THEN DO: /*----- delete index -----*/
    /* Check if this is the last index with this index-number */
    FOR EACH _Index OF _File
      WHERE _Index._Idx-num = idxnum i = 1 TO 2:
    END.
    IF i = 1 THEN DO:
      /* Don't delete the last index with number */
      MESSAGE "You are not allowed to delete the last index number" idxnum
           view-as alert-box.
      user_path = "".
      RETURN.
    END.
    /* Reassign prime-index */
    IF dbkey = _File._Prime-Index THEN DO:
      FIND FIRST _Index OF _File
        WHERE _Index._Idx-num = idxnum
          AND RECID(_Index) <> _File._Prime-Index.
      _File._Prime-Index = RECID(_Index).
    END.
    FIND _Index WHERE RECID(_Index) = dbkey.
    FOR EACH _Index-field OF _Index:
      DELETE _Index-field.
    END.
    DELETE _Index.
  END.
  ELSE DO ON ERROR UNDO, LEAVE  ON ENDKEY UNDO, LEAVE: /*--- add new index ---*/

    /*-----WINDOW-CLOSE-----*/
    ON WINDOW-CLOSE OF FRAME idx-ask
       APPLY "END-ERROR" TO FRAME idx-ask.

    {adecomm/okrun.i  
      &FRAME  = "FRAME idx-ask" 
      &BOX    = "rect_Btns"
      &OK     = "btn_OK" 
      {&CAN_BTN}
      {&HLP_BTN}
    }
    UPDATE idxname     
      	   btn_OK 
      	   btn_Cancel
           {&HLP_BTN_NAME}
           WITH FRAME idx-ask.
    HIDE FRAME idx-ask.

    FIND idxbuf WHERE RECID(idxbuf) = dbkey.

    CREATE _Index.
    ASSIGN
      _Index._Unique     = idxbuf._Unique
    /*_Index._For-name   = idxbuf._For-name*/
      _Index._File-recid = drec_file
      _Index._Index-Name = idxname.
    IF user_dbtype <> "PROGRESS" THEN /* for testing only! */
      _Index._Idx-num = idxnum.

    DO i = 1 TO NUM-ENTRIES(pik_first,";"):
      FIND _Field OF _File
        WHERE _Field._Field-name = ENTRY(i,pik_first,";").

      CREATE _Index-field.
      ASSIGN
        _Index-field._Abbreviate  = FALSE
        _Index-field._Ascending   = SUBSTRING(rec,_Fld-stoff + 1,1) = "a"
        _Index-field._Field-recid = RECID(_Field)
        _Index-field._Index-Seq   = i
        _Index-field._Index-recid = RECID(_Index).
        OVERLAY(rec,_Fld-stoff + 1,_Fld-stlen) = FILL("-",_Fld-stlen).
    END.

    /* If not complete overlap, define dummy-field to make complete overlap */

    pos = INDEX(rec,STRING(INDEX(rec,"a") > 0,"a/d")).
    IF pos > 0 THEN DO:
      FIND LAST _Field OF _File USE-INDEX _Field-pos.
      j = _Field._Order + 10.
      CREATE _Field.
      CREATE _Index-field.
      ASSIGN
        _Field._File-recid        = drec_file
        _Field._Data-type         = "character"
        _Field._Order             = j
        _Field._Fld-stoff         = pos - 1
        _Field._Fld-stlen         = LENGTH(rec) - pos + 1
        _Field._Field-name        = idxname
                                  + "-" + STRING(_Field._Fld-stoff)
                                  + "-" + STRING(_Field._Fld-stlen)
        _Field._For-type          = ENTRY(1,chr_def)
        _Field._Fld-stdtype       = INTEGER(ENTRY(2,chr_def))
        _Field._Fld-case          = TRUE
        _Index-field._Abbreviate  = FALSE
        _Index-field._Ascending   = SUBSTRING(rec,_Fld-stoff + 1,1) = "a"
        _Index-field._Field-recid = RECID(_Field)
        _Index-field._Index-Seq   = i
        _Index-field._Index-recid = RECID(_Index).
    END.
  END.	 /* End Add Index (DO ON ERROR ...) */
END.

HIDE FRAME idx-ask.
RETURN.

/*------------------------------------------------------------------*/
