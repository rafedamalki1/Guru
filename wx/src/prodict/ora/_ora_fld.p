/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* ora_fld - field editor for Oracle files */

/*
dfields is NOT AVAILABLE to create, otherwise contains record to
UPDATE (you ain't spoz'ta CREATE with ORACLE files).

When you come into this routine, the field name is already set on the
form.

  _File._For-Name     = Foreign file name.
  _File._For-Owner    = Foreign file owner.
  _File._For-Type     = Foreign file type (TABLE, VIEW, SYNONYM, CLUSTER).
  _Field._Fld-stoff   = Field relative position in record.
  _Field._Fld-stdtype = Foreign field datatype.
  _Field._For-Name    = Foreign field name.
  _Field._For-retrieve = ? or 0 if field should be retrieved from Oracle,
                        1 if not (large objects ...)
  _Field._For-Type    = Foreign field datatype name.
  _Index._For-name    = Foreign index name.

  -----Oracle-----  ---Progress---
  Data Type  dtype  Data Type dtyp
  --------- ------  --------- ----
  Char       4,096  character    1
  Varchar    4,096  character    1
  Varchar2   4,096  character    1
  Float      8,192  decimal      5
  Number     8,192  decimal      5
  Number     8,192  integer      4
  Number     8,192  logical      3
  Date      12,288  date         2
  Long      16,384  character    1
  Raw       20,480  character    1
  Long Raw  24,576  character    1
  Time      28,672  integer      4
*/

DEFINE INPUT  PARAMETER ronly       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER junk2       AS RECID       NO-UNDO.
DEFINE OUTPUT PARAMETER changed     AS LOGICAL     NO-UNDO INITIAL FALSE.

DEFINE SHARED  BUFFER   dfields     FOR _Field.
DEFINE         VARIABLE answer      AS LOGICAL     NO-UNDO.
DEFINE         VARIABLE chg-extent  AS LOGICAL     NO-UNDO.
DEFINE         VARIABLE i           AS INTEGER     NO-UNDO.
DEFINE         VARIABLE j           AS INTEGER     NO-UNDO.
DEFINE         VARIABLE inindex     AS LOGICAL     NO-UNDO.
DEFINE         VARIABLE inview      AS LOGICAL     NO-UNDO.
DEFINE         VARIABLE retriev     AS LOGICAL     NO-UNDO.
DEFINE         VARIABLE c           AS CHARACTER   NO-UNDO.
DEFINE         VARIABLE pro_typ     AS CHARACTER   NO-UNDO.
DEFINE         VARIABLE gat_typ     AS CHARACTER   NO-UNDO.


{ prodict/dictvar.i }
{ prodict/user/uservar.i }

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 7 NO-UNDO INITIAL [
  /* 1*/ "This field is used in a View or Index - cannot delete.",
  /* 2*/ "This field is used in a View - cannot rename.",
  /* 3*/ "Are you sure that you want to delete the field named",
  /* 4*/ "Cannot create ORACLE fields.  Must create on ORACLE side and",
  /* 5*/ "use ~"Update ORACLE definition~" to bring definition over.",
  /* 6*/ "", /* reserved */
  /* 7*/ "This is not an equivalent PROGRESS datatype for the ORACLE datatype"
].

FORM
  dfields._Field-name   LABEL "  Field-Name" FORMAT "x(32)"
    VALIDATE(KEYWORD(dfields._Field-name) = ?,
      "This name conflicts with a PROGRESS reserved keyword.") SPACE
  dfields._Data-type    LABEL    "Data-Type" FORMAT "x(9)"  SKIP

  dfields._For-Name LABEL " Oracle-Name" FORMAT "x(30)" SPACE(4)
  dfields._For-Type LABEL     "Ora-Type" FORMAT "x(12)" SKIP

  dfields._Format       LABEL "      Format" FORMAT "x(30)" SPACE(4)
  dfields._Fld-stoff    LABEL     "Column #" FORMAT ">>>9"
  dfields._Fld-case	LABEL " Case-sens"   FORMAT "y/n"   SKIP

  dfields._Label        LABEL "       Label" FORMAT "x(30)" SPACE(4)
  dfields._Decimals     LABEL     "   Scale" FORMAT "->>>>9" "(Decimals)" SKIP

  dfields._Col-label    LABEL "Column-label" FORMAT "x(30)" SPACE(4)
  dfields._Order        LABEL     "   Order" FORMAT ">>>>9"
  /* dfields._For-retrieve  */
  retriev              LABEL     "Retrieve ?" FORMAT "y/n" SKIP

  dfields._Initial      LABEL "     Initial" FORMAT "x(30)" SPACE(4)
  dfields._Mandatory    LABEL     "Not Null" FORMAT "yes/no"
  dfields._Extent	LABEL	  "  Extent" FORMAT ">>>>9" SKIP

  dfields._Valexp       LABEL "Valexp"       VIEW-AS EDITOR
       	       	     	      	       	     INNER-CHARS 63 INNER-LINES 4
      	       	     	      	       	     BUFFER-LINES 4 SKIP

  dfields._Valmsg       LABEL "Valmsg"       FORMAT "x(63)" SKIP
  dfields._Help         LABEL "  Help"       FORMAT "x(63)" SKIP
  dfields._Desc         LABEL "  Desc"       FORMAT "x(70)" SKIP
  HEADER ""
  WITH FRAME ora_fld NO-BOX ATTR-SPACE OVERLAY SIDE-LABELS
  ROW (SCREEN-LINES - 19) COLUMN 1.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

IF NOT AVAILABLE dfields THEN
  FIND FIRST dfields WHERE dfields._File-recid = drec_file
    USING FRAME ora_fld _Field-name NO-ERROR.

IF NOT AVAILABLE dfields THEN DO:
  MESSAGE new_lang[4] SKIP /* cannot create */
      	  new_lang[5]
      	  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  HIDE MESSAGE NO-PAUSE.
  RETURN.
END.

/* PROCEDURES and FUNCTIONS and BUFFERS can have arrays as parameters
 * we have to allow the user to change the extent, because ORACLE does
 * not tell us the right number.
 */
FIND _File OF dfields NO-LOCK NO-ERROR.
 
ASSIGN
  chg-extent = available _File 
           AND CAN-DO("PROCEDURE,FUNCTION,BUFFER",_File._For-type)
           AND dfields._Extent > 0
  inindex    = CAN-FIND(FIRST _Index-field OF dfields)
  inview     = CAN-FIND(FIRST _View-ref
                          WHERE _View-ref._Ref-Table = user_filename
                          AND   _View-ref._Base-Col = dfields._Field-name).

RELEASE _File.

/* Get the list of Progress types that can be used for the 
   gateway type for this field. */
ASSIGN
   pro_typ = "get-list"
   gat_typ = dfields._For-type
   c = ?
   i = ?.
RUN "prodict/ora/_ora_typ.p"
  (INPUT-OUTPUT i,INPUT-OUTPUT i,
   INPUT-OUTPUT pro_typ,INPUT-OUTPUT gat_typ,
   OUTPUT c).

/*retriev = dfields._For-retrieve = 1.*/
DISPLAY
  dfields._Field-name
  dfields._Data-type
  dfields._For-Name
  dfields._For-Type
  retriev
  dfields._Format
  dfields._Label
  dfields._Col-label
  dfields._Initial
  dfields._Fld-stoff
  dfields._Fld-case WHEN dfields._Data-type = "CHARACTER"
  dfields._Decimals WHEN dfields._Data-type = "DECIMAL" 
  dfields._Order
  dfields._Mandatory
  dfields._Extent
  dfields._Valexp
  dfields._Valmsg
  dfields._Help
  dfields._Desc
  WITH FRAME ora_fld.

IF ronly = "r/o" THEN DO:
  { prodict/user/userpaus.i }
  HIDE FRAME ora_fld NO-PAUSE.
  RETURN.
END.

NEXT-PROMPT dfields._Format WITH FRAME ora_fld.

DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  SET
    dfields._Field-name
    dfields._Data-type VALIDATE(
      CAN-DO(pro_typ,dfields._Data-type), new_lang[7]) /* not equiv dtype */
  /*dfields._For-Name*/
  /*dfields._For-Type*/
    dfields._Format
    dfields._Label
    dfields._Col-label
    dfields._Initial
  /*dfields._Fld-stoff*/
    dfields._Decimals WHEN dfields._Data-type = "DECIMAL"
    dfields._Order
    dfields._Fld-case WHEN INPUT dfields._Data-type = "character"
                        AND (NEW dfields OR NOT inindex)
    /* Suppress update of retrieve until implemented in Progress */
  /* retriev
    dfields._For-retrieve = IF retriev THEN ? ELSE 1 */
    dfields._Mandatory
    dfields._Extent  WHEN chg-extent
    dfields._Valexp
    dfields._Valmsg
    dfields._Help
    dfields._Desc
    WITH FRAME ora_fld.

  IF dfields._Field-name ENTERED AND NOT NEW dfields AND inview THEN DO:
    MESSAGE new_lang[2]. /* sorry, used in view */
    UNDO,RETRY.
  END.

  ASSIGN
    dfields._Valexp = (IF TRIM(dfields._Valexp) = "" 
      	       	     	 THEN ? 
      	       	     	 ELSE TRIM(dfields._Valexp)).
  changed = TRUE.
END.

HIDE FRAME ora_fld NO-PAUSE.
RETURN.
