/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/dump/loaddefs.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE scrap AS LOGICAL NO-UNDO.
DEFINE BUFFER another_index FOR _Index.

FIND _File WHERE RECID(_File) = drec_file.
FIND FIRST widx.
IF imod <> "a" AND imod <> "d" THEN
  FIND _Index OF _File
    WHERE _Index._Index-name = widx._Index-name. /* proven to exist */

IF imod = "a" THEN DO: /*----------------------------------------------*/

  IF CAN-FIND(FIRST _Index OF _File 
      	      WHERE _Index._Index-name = widx._Index-name) THEN DO:
    ierror = 7. /* "&2 already exists with name &3" */
    RETURN.
  END.

  CREATE _Index.
  ASSIGN
    _Index._File-recid = drec_file
    _Index._I-misc1[1] = widx._I-misc1[1]
    _Index._Index-name = widx._Index-name
    _Index._Unique     = widx._Unique.
  IF widx._Desc       <> ? THEN _Index._Desc       = widx._Desc.
  IF widx._Wordidx    <> ? THEN _Index._Wordidx    = widx._Wordidx.
  IF widx._Idx-num    <> ? 
   AND user_dbtype    <> "PROGRESS" 
                           THEN _Index._Idx-num    = widx._Idx-num.
  IF widx._For-name   <> ? THEN _Index._For-name   = widx._For-name.
  IF widx._I-misc2[1] <> ? THEN _Index._I-misc2[1] = widx._I-misc2[1].
  IF NOT widx._Active      THEN _Index._Active     = FALSE.
  IF iprimary              THEN _File._Prime-index = RECID(_Index).

  FOR EACH wixf:
    CREATE _Index-field.
    ASSIGN
      _Index-field._Index-recid = RECID(_Index)
      _Index-field._Field-recid = wixf._Field-recid
      _Index-field._Index-Seq   = wixf._Index-Seq
      _Index-field._Abbreviate  = wixf._Abbreviate
      _Index-field._Ascending   = wixf._Ascending
      _Index-field._Unsorted    = FALSE.
    /*_Index-field._Unsorted    = wixf._Unsorted*/
    /*   The above line will be turned on when */
    /*   we implement a gateway that needs it. */
  END.

  /* only delete default index if there is a non-word index also */
  IF _File._dft-pk AND _Index._Wordidx <> 1 THEN DO:
    FIND _Index WHERE RECID(_Index) = _File._Prime-Index NO-ERROR.
    IF AVAILABLE _Index THEN DO:
      ASSIGN
        _File._Prime-Index = RECID(_Index) /* this block will delete the */
        _File._dft-pk      = FALSE.     /* default index if there is one */
      DELETE _Index.
    END.
  END.

END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "m" THEN DO: /*---------------------------------------------------*/
  IF NOT widx._Active AND _Index._Active THEN _Index._Active = FALSE.
  IF iprimary AND _Index._Wordidx <> 1
    AND _File._Prime-index <> RECID(_Index) THEN
    _File._Prime-index = RECID(_Index).

  IF widx._Desc     <> _Index._Desc     THEN _Index._Desc     = widx._Desc.
  IF widx._For-name <> _Index._For-name THEN _Index._For-name = widx._For-name.
  IF widx._For-type <> _Index._For-type THEN _Index._For-type = widx._For-type.
  IF widx._I-misc1[1] <> _Index._I-misc1[1] THEN _Index._I-misc1[1] = widx._I-misc1[1].
  IF widx._I-misc2[1] <> _Index._I-misc2[1] THEN _Index._I-misc2[1] = widx._I-misc2[1].
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "r" THEN DO: /*---------------------------------------------------*/
  IF CAN-FIND(FIRST _Index OF _File WHERE _Index._Index-name = irename) THEN
    ierror = 7. /* "&2 already exists with name &3" */
  IF ierror > 0 THEN RETURN.
  IF irename <> "default" THEN _Index._Index-name = irename.
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "d" THEN DO: /*---------------------------------------------------*/
  FIND _Index OF _File WHERE _Index._Index-name = widx._Index-name NO-ERROR.

  /* if a field is deleted, then its index is deleted.  therefore, this
  index might be missing.  so don't complain if we don't find it. */
  IF NOT AVAILABLE _Index AND NOT CAN-DO(kindexcache,widx._Index-name) THEN
    ierror = 9. /* "Index already deleted" */
  IF ierror > 0 THEN RETURN.

  IF AVAILABLE _Index THEN DO:
    FIND _File OF _Index.
    IF RECID(_Index) = _File._Prime-Index THEN DO:
      /* move primary to another index */
      FIND FIRST another_index OF _File
        WHERE RECID(another_index) <> RECID(_Index)
        AND another_index._Wordidx <> 1 NO-ERROR.
      IF NOT AVAILABLE another_index THEN ierror = 22.
        /* "Cannot remove last non-word index" */
      IF ierror > 0 THEN RETURN.
      _File._Prime-Index = RECID(another_index).
    END.
    FOR EACH _Index-field OF _Index:
      DELETE _Index-field.
    END.
    DELETE _Index.
  END.

END. /*---------------------------------------------------------------------*/

RETURN.
