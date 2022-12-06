/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _gat_of2.p - Shows RMS,CISAM,NetISAM,CTOSISAM record layout & fld overlaps */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.  /* not used here */
DEFINE SHARED STREAM rpt.

DEFINE VARIABLE i             AS INTEGER           NO-UNDO.
DEFINE VARIABLE curr-end      AS INTEGER           NO-UNDO.
DEFINE VARIABLE first-overlap AS LOGICAL           NO-UNDO.
DEFINE VARIABLE last-end      AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE scan-end      AS INTEGER           NO-UNDO.

DEFINE WORKFILE dfields NO-UNDO
  FIELD name LIKE _Field._Field-name
  FIELD rtyp AS CHARACTER
  FIELD ptyp AS CHARACTER
  FIELD len  AS INTEGER
  FIELD pos  AS INTEGER.
DEFINE BUFFER curr-fld FOR dfields.
DEFINE BUFFER scan-fld FOR dfields.

FORM
  curr-fld.name FORMAT "x(25)" LABEL "Field Name"
  curr-fld.rtyp FORMAT "x(8)"  LABEL "DS Type"
  curr-fld.ptyp FORMAT "x(9)"  LABEL "Pro Type"
  curr-fld.pos  FORMAT ">>>>9" LABEL "Pos"
  curr-fld.len  FORMAT ">>>>9" LABEL "Len"
  scan-fld.name FORMAT "x(19)" LABEL "Overlaps"
  WITH FRAME fld-frm NO-ATTR-SPACE 
  DOWN NO-BOX USE-TEXT STREAM-IO
  ROW 3 COLUMNS 1 WIDTH 80.


/*========================================================================*/

FOR EACH _Field WHERE _Field._File-recid = drec_file:
  CREATE dfields.
  /* all lengths and positions are stored as bits then converted for disp */
  ASSIGN
    dfields.name = _Field._Field-name
                 + (IF _Field._Extent > 0 THEN "[1]" ELSE "")
    dfields.rtyp = _Field._For-type
    dfields.ptyp = _Field._Data-type
    dfields.len  = _Field._Fld-stlen * 8
    dfields.pos  = _Field._Fld-stoff * 8.
  IF dfields.rtyp = "BITS" THEN
    ASSIGN
      dfields.pos = dfields.pos + _Field._Decimals
      dfields.len = dfields.len / 8.
  DO i = 2 TO _Field._Extent:
    CREATE dfields.
    ASSIGN
      dfields.name = _Field._Field-name + "[" + STRING(i) + "]"
      dfields.rtyp = _Field._For-type
      dfields.ptyp = _Field._Data-type
      dfields.len  = _Field._Fld-stlen * 8
      dfields.pos  = (_Field._Fld-stoff + (i - 1)
                   * (IF _Field._For-spacing = ?
                     THEN _Field._Fld-stlen
                     ELSE _Field._For-spacing)) * 8.
/*                   * (IF _Field._Fld-misc1[3] = ?
 *                     THEN _Field._Fld-stlen
 *                     ELSE _Field._Fld-misc1[3])) * 8.
 */
     IF dfields.rtyp = "BITS" THEN
      ASSIGN
        dfields.pos = dfields.pos + _Field._Decimals
        dfields.len = dfields.len / 8.
  END.
END.

FOR EACH curr-fld BY curr-fld.pos ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
  IF curr-end <> ? AND curr-end < curr-fld.pos - 1 THEN DO:
    DISPLAY STREAM rpt
      "...gap..." @ curr-fld.name
      (curr-end + 1) / 8 @ curr-fld.pos
      (curr-fld.pos - 1 - curr-end) / 8 @ curr-fld.len
      WITH FRAME fld-frm.
    DOWN STREAM rpt 1 WITH FRAME fld-frm.
  END.
  ASSIGN            /* do not change when prev fld ended short (overlap) */
    curr-end      = if curr-fld.pos + curr-fld.len - 1 gt curr-end then
                       curr-fld.pos + curr-fld.len - 1 else curr-end
    first-overlap = TRUE.
  DISPLAY STREAM rpt curr-fld.name curr-fld.rtyp curr-fld.ptyp
    /* no matter what bit the field starts on we only interested in the byte */
    TRUNCATE(curr-fld.pos / 8.0,0)       @ curr-fld.pos
    /*
    the following calculation is for support of BIT field length
    LENGTH in bits + field offset ( this is 0 for non bit fields)
      + 7 (so that any stray bits will cause us to round up to next byte)
    */
    TRUNCATE((curr-fld.len + (curr-fld.pos MOD 8) + 7) / 8.0,0) @ curr-fld.len
    WITH FRAME fld-frm.
  FOR EACH scan-fld WHERE RECID(scan-fld) <> RECID(curr-fld) BY scan-fld.pos:
    scan-end = scan-fld.pos + scan-fld.len - 1.
    IF (scan-fld.pos le curr-end AND scan-end ge curr-fld.pos) THEN DO:
/*                above line does the same thing (tomn 8/25):

       <---------------- scan pos ---------------|
                              |====== curr ======|
                              |--------------- scan end ---------------->
                              
    IF   (curr-fld.pos >= scan-fld.pos AND curr-fld.pos <= scan-end)
      OR (curr-end     >= scan-fld.pos AND curr-end     <= scan-end)
      OR (curr-fld.pos <  scan-fld.pos AND curr-end     >  scan-end) THEN DO:
*/
      IF first-overlap THEN
        first-overlap = FALSE.
      ELSE
        DOWN STREAM rpt 1 WITH FRAME fld-frm.
      DISPLAY STREAM rpt scan-fld.name WITH FRAME fld-frm.
    END.
  END.
  DOWN STREAM rpt 1 WITH FRAME fld-frm.
END.

RETURN.
