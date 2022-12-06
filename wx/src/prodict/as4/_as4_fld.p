/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* __as4_fld.p - field editor for AS/400 files */

/*
dfields is NOT AVAILABLE to create, otherwise contains record to
UPDATE.

When you come into this routine, the field name is already set on the
form.

_File._For-name      = Foreign file physical file name.
_Field._Decimals     = Number of Progress decimal places.
_Field._Fld-stoff    = Storage position in record.
_Field._Fld-stlen    = Absolute storage length in bytes.
_Field._Fld-stdtype  = Storage datatype.
_Field._For-type     = Foreign field datatype.
_Field._For-xpos     = position for case-insensitive version of field

See __as4_typ.p for complete datatype summary
*/

DEFINE INPUT  PARAMETER ronly   AS CHARACTER             NO-UNDO.
DEFINE INPUT  PARAMETER junk2   AS RECID                 NO-UNDO.
DEFINE OUTPUT PARAMETER changed AS LOGICAL INITIAL FALSE NO-UNDO.

DEFINE SHARED BUFFER dfields FOR DICTDB._Field.

DEFINE VARIABLE answer      AS LOGICAL               NO-UNDO.
DEFINE VARIABLE inindex     AS LOGICAL               NO-UNDO.
DEFINE VARIABLE inview      AS LOGICAL               NO-UNDO.
DEFINE VARIABLE i           AS INTEGER               NO-UNDO.
DEFINE VARIABLE j           AS INTEGER               NO-UNDO.

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }

  /* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
  DEFINE VARIABLE new_lang AS CHARACTER EXTENT 4 NO-UNDO INITIAL [
  /* 1*/ "", /* reserved */
  /* 2*/ "This field is used in a View - cannot rename.",
  /* 3*/ "Cannot create AS/400 fields.  Must create on AS/400 side and",
  /* 4*/ "use ~"Load AS/400 .df Definitions File~" to bring definition over."
  ].

FORMAT
  dfields._Field-name   LABEL "  Field-Name" FORMAT "x(32)"
    VALIDATE(KEYWORD(dfields._Field-name) = ?,
      "This name conflicts with a PROGRESS reserved keyword.") SPACE
  dfields._For-type LABEL    " AS4-Type" FORMAT "x(8)"
  "(" SPACE(0) dfields._Data-type FORMAT "x(9)"
    NO-LABELS NO-ATTR-SPACE SPACE(0) ")" SKIP

  dfields._Format       LABEL "      Format" FORMAT "x(30)"
  dfields._Fld-stoff    LABEL     "Position" FORMAT ">>>>9"
  dfields._Fld-stlen    LABEL     "  Length" FORMAT ">>>>9" SKIP

  dfields._Label        LABEL "       Label" FORMAT "x(30)"
  dfields._Extent       LABEL     "  Extent" FORMAT ">>>>"
  dfields._For-xpos     LABEL    " CIns-Pos" FORMAT ">>>>" SKIP

  dfields._Col-label    LABEL "Column-label" FORMAT "x(30)" 
  dfields._Decimals     LABEL     "Decimals" FORMAT ">>>9"
    VALIDATE(dfields._Decimals <> ?,"You must enter a value here")
  dfields._Fld-case     LABEL    "Case-sens" FORMAT "yes/no" SKIP

  dfields._Initial      LABEL "     Initial" FORMAT "x(30)"
  dfields._Order        LABEL     "   Order" FORMAT ">>>9" SKIP

  dfields._Valexp       LABEL "Valexp"       VIEW-AS EDITOR
       	       	     	      	       	     INNER-CHARS 63 INNER-LINES 4
      	       	     	      	       	     BUFFER-LINES 4 SKIP

  dfields._Valmsg       LABEL "Valmsg"       FORMAT "x(63)" SKIP
  dfields._Help         LABEL "  Help"       FORMAT "x(63)" SKIP
  dfields._Desc         LABEL "  Desc"       FORMAT "x(70)" SKIP
  HEADER ""
  WITH FRAME as4_fld NO-BOX ATTR-SPACE OVERLAY SIDE-LABELS
    ROW (SCREEN-LINES - 19) COLUMNS 1.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

IF NOT AVAILABLE dfields THEN
  FIND FIRST dfields
    WHERE dfields._File-recid = drec_file
    USING FRAME as4_fld _Field-name NO-ERROR.

IF NOT AVAILABLE dfields THEN DO:
  MESSAGE new_lang[3]. /* cannot create */
  MESSAGE new_lang[4].
  { prodict/user/userpaus.i }
  HIDE MESSAGE NO-PAUSE.
  RETURN.
END.

ASSIGN
  inindex = CAN-FIND(FIRST DICTDB._Index-field OF dfields)
  inview  = CAN-FIND(FIRST DICTDB._View-ref
            WHERE DICTDB._View-ref._Ref-Table = user_filename
            AND DICTDB._View-ref._Base-Col = dfields._Field-name).

DISPLAY
  dfields._Field-name
  dfields._For-type
  dfields._Data-type
  dfields._Format
  dfields._Fld-stoff
  dfields._Fld-stlen
  dfields._Label
  dfields._Extent
  dfields._For-xpos
  dfields._Col-label
  dfields._Decimals WHEN dfields._Data-type = "DECIMAL"
  dfields._Fld-case
  dfields._Initial
  dfields._Order
  dfields._Valexp
  dfields._Valmsg
  dfields._Help
  dfields._Desc
  WITH FRAME as4_fld.

IF ronly = "r/o" THEN DO:
  { prodict/user/userpaus.i }
  HIDE FRAME as4_fld NO-PAUSE.
  RETURN.
END.

NEXT-PROMPT dfields._Format WITH FRAME as4_fld.

DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  SET
    dfields._Field-name
    dfields._Format
    dfields._Label
    dfields._Col-label
    dfields._Decimals WHEN dfields._Data-type = "DECIMAL"
    dfields._Initial
    dfields._Order
    dfields._Valexp
    dfields._Valmsg
    dfields._Help
    dfields._Desc
    WITH FRAME as4_fld.

  IF dfields._Field-name ENTERED AND NOT NEW dfields AND inview THEN DO:
    MESSAGE new_lang[2]. /* sorry, used in view */
    UNDO,RETRY.
  END.

  IF dfields._Data-type = "character" THEN
    dfields._Decimals = dfields._Fld-stlen.

  ASSIGN
    dfields._Valexp = (IF TRIM(dfields._Valexp) = "" 
      	       	     	 THEN ? 
      	       	     	 ELSE TRIM(dfields._Valexp)).
  changed = TRUE.
END.

HIDE FRAME as4_fld NO-PAUSE.
RETURN.
