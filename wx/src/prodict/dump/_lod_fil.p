/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _lod_fil.p  -  moves temptable-info to new or existing _File */

{ prodict/dictvar.i }
{ prodict/dump/loaddefs.i }

DEFINE VARIABLE scrap    AS CHARACTER NO-UNDO.
DEFINE VARIABLE old_name AS CHARACTER NO-UNDO CASE-SENS.
DEFINE VARIABLE new_name AS CHARACTER NO-UNDO CASE-SENS.

/* defines for dumpname.i */
DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.

/*---------------------------  MAIN-CODE  --------------------------*/

FIND FIRST wfil.
IF imod <> "a" THEN DO:
  FIND _File
    WHERE _File._db-recid  = drec_db
    AND   _File._File-name = wfil._File-name. /* proven to exist */
  IF _File._Frozen THEN ierror = 16. /* "Cannot &1 frozen file &3" */
END.
IF ierror > 0 THEN RETURN.

IF imod = "a" THEN DO: /*---------------------------------------------------*/
  IF CAN-FIND(_File WHERE _File._Db-recid = drec_db
    AND _File._File-name = wfil._File-name) THEN
    ierror = 7. /* "&2 already exists with name &3" */
  IF CAN-FIND(FIRST _View WHERE _View._View-name = wfil._File-name) THEN
    ierror = 19. /* VIEW exists with name &3" */
  IF ierror > 0 THEN RETURN.

/* tmp-change: check for duplicate dump-names <hutegger> 94/05 */
  IF wfil._Dump-name <> ? THEN
    DO:
    ASSIGN nam = wfil._Dump-name.
    {prodict/dump/dumpname.i}
    ASSIGN wfil._Dump-name = nam.
    END.

  ierror = 23. /* default error to general table attr error */
  CREATE _File.
  _File._Db-recid = drec_db.
  { prodict/dump/copy_fil.i &from=wfil &to=_File &all=false}
  IF wfil._Dump-name <> ? THEN _File._Dump-name = wfil._Dump-name.

  IF wfil._Db-lang = 1 THEN
    dblangcache = dblangcache + "," + STRING(RECID(_File)).
  IF wfil._Frozen THEN
    frozencache = frozencache + "," + STRING(RECID(_File)).

  ierror = 0.  /* if we get here, there were no errors */

END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "m" THEN DO: /*---------------------------------------------------*/
  ierror = 23. /* default error to general table attr error */
  IF _File._Db-lang = 0 THEN DO:
    IF _File._Can-create <> wfil._Can-create THEN
      _File._Can-create = wfil._Can-create.
    IF _File._Can-delete <> wfil._Can-delete THEN
      _File._Can-delete = wfil._Can-delete.
    IF _File._Can-write <> wfil._Can-write THEN
      _File._Can-write = wfil._Can-write.
    IF _File._Can-read <> wfil._Can-read THEN
      _File._Can-read = wfil._Can-read.
    IF _File._Can-dump <> wfil._Can-dump THEN
      _File._Can-dump = wfil._Can-dump.
    IF _File._Can-load <> wfil._Can-load THEN
      _File._Can-load = wfil._Can-load.
  END.

  IF _File._Desc       	<> wfil._Desc   THEN _File._Desc   = wfil._Desc.
  IF _File._File-label 	<> wfil._File-label THEN 
    _File._File-label = wfil._File-label.
  IF _File._File-label-SA <> wfil._File-label-SA THEN 
    _File._File-label-SA = wfil._File-label-SA.
  IF _File._Valexp   	<> wfil._Valexp THEN _File._Valexp = wfil._Valexp.
  IF _File._Valmsg   	<> wfil._Valmsg THEN _File._Valmsg = wfil._Valmsg.
  IF _File._Valmsg-SA  	<> wfil._Valmsg-SA THEN 
    _File._Valmsg-SA = wfil._Valmsg-SA.
  IF _File._Hidden   	<> wfil._Hidden THEN _File._Hidden = wfil._Hidden.
  IF _File._Dump-name 	<> wfil._Dump-name AND wfil._Dump-name <> ? THEN
    _File._Dump-name = wfil._Dump-name.

  IF _File._For-Format <> wfil._For-Format THEN
    _File._For-Format = wfil._For-Format.
  IF _File._For-Owner <> wfil._For-Owner THEN
    _File._For-Owner = wfil._For-Owner.
  IF _File._For-number <> wfil._For-number THEN
    _File._For-number = wfil._For-number.
  IF _File._For-Id   <> wfil._For-Id   THEN _File._For-Id   = wfil._For-Id.
  IF _File._For-Cnt1 <> wfil._For-Cnt1 THEN _File._For-Cnt1 = wfil._For-Cnt1.
  IF _File._For-Cnt2 <> wfil._For-Cnt2 THEN _File._For-Cnt2 = wfil._For-Cnt2.
  IF _File._For-Flag <> wfil._For-Flag THEN _File._For-Flag = wfil._For-Flag.
  IF _File._For-Info <> wfil._For-Info THEN _File._For-Info = wfil._For-Info.
  IF _File._For-Name <> wfil._For-Name THEN _File._For-Name = wfil._For-Name.
  IF _File._For-Size <> wfil._For-Size THEN _File._For-Size = wfil._For-Size.
  IF _File._For-Type <> wfil._For-Type THEN _File._For-Type = wfil._For-Type.

  IF _File._Fil-misc2[1] <> wfil._Fil-misc2[1] THEN
    _File._Fil-misc2[1] = wfil._Fil-misc2[1].

  IF _File._Fil-misc2[2] <> wfil._Fil-misc2[2] THEN
    _File._Fil-misc2[2] = wfil._Fil-misc2[2].

  IF _File._Fil-misc2[3] <> wfil._Fil-misc2[3] THEN
    _File._Fil-misc2[3] = wfil._Fil-misc2[3].

  IF _File._Fil-misc2[4] <> wfil._Fil-misc2[4] THEN
    _File._Fil-misc2[4] = wfil._Fil-misc2[4].

  IF _File._Fil-misc2[5] <> wfil._Fil-misc2[5] THEN
    _File._Fil-misc2[5] = wfil._Fil-misc2[5].

  IF _File._Fil-misc2[6] <> wfil._Fil-misc2[6] THEN
    _File._Fil-misc2[6] = wfil._Fil-misc2[6].

  IF _File._Fil-misc2[7] <> wfil._Fil-misc2[7] THEN
    _File._Fil-misc2[7] = wfil._Fil-misc2[7].

  IF _File._Fil-misc2[8] <> wfil._Fil-misc2[8] THEN
    _File._Fil-misc2[8] = wfil._Fil-misc2[8].

  IF _File._Fil-misc1[1] <> wfil._Fil-misc1[1] THEN
    _File._Fil-misc1[1] = wfil._Fil-misc1[1].

  IF _File._Fil-misc1[2] <> wfil._Fil-misc1[2] THEN
    _File._Fil-misc1[2] = wfil._Fil-misc1[2].

  IF _File._Fil-misc1[3] <> wfil._Fil-misc1[3] THEN
    _File._Fil-misc1[3] = wfil._Fil-misc1[3].

  IF _File._Fil-misc1[4] <> wfil._Fil-misc1[4] THEN
    _File._Fil-misc1[4] = wfil._Fil-misc1[4].

  IF _File._Fil-misc1[5] <> wfil._Fil-misc1[5] THEN
    _File._Fil-misc1[5] = wfil._Fil-misc1[5].

  IF _File._Fil-misc1[6] <> wfil._Fil-misc1[6] THEN
    _File._Fil-misc1[6] = wfil._Fil-misc1[6].

  IF _File._Fil-misc1[7] <> wfil._Fil-misc1[7] THEN
    _File._Fil-misc1[7] = wfil._Fil-misc1[7].

  IF _File._Fil-misc1[8] <> wfil._Fil-misc1[8] THEN
    _File._Fil-misc1[8] = wfil._Fil-misc1[8].

  IF wfil._Frozen THEN
    frozencache = frozencache + "," + STRING(RECID(_File)).
  
  ierror = 0.  /* if we get here, there were no errors */

END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "r" THEN DO: /*---------------------------------------------------*/
  IF CAN-FIND(FIRST _View-ref WHERE _Ref-Table = wfil._File-name) THEN
    ierror = 20. /* "Cannot &1 &2 referenced in SQL view" */
  IF _File._Db-lang = 1 THEN
    ierror = 18. /* "Cannot rename SQL table" */
  IF CAN-FIND(FIRST _File WHERE _File._Db-recid = drec_db
    AND _File._File-name = irename) THEN
    ierror = 7. /* "&2 already exists with name &3" */
  IF ierror > 0 THEN RETURN.
  _File._File-name = irename.
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "d" THEN DO: /*---------------------------------------------------*/
  IF _File._Db-lang = 1 THEN
    ierror = 17. /* "Use SQL DROP TABLE to remove &3" */
  IF CAN-FIND(FIRST _View-ref WHERE _Ref-Table = _File._File-name) THEN
    ierror = 20. /* "Cannot &1 &2 referenced in SQL view" */
  IF ierror > 0 THEN RETURN.
  { prodict/dump/loadkill.i }
END. /*---------------------------------------------------------------------*/

/* update triggers */
IF imod = "a" OR imod = "m" THEN DO:
  scrap = "".
  trig_loop:
  FOR EACH wfit:
    IF wfit._Proc-name = "!" THEN DO:
      DELETE wfit. /* triggers are deleted via .df when proc-name set to "!" */
      NEXT trig_loop.
    END.
    scrap = scrap + (IF scrap = "" THEN "" ELSE ",") + wfit._Event.
    FIND _File-trig OF _File WHERE _File-trig._Event = wfit._Event NO-ERROR.
    IF AVAILABLE _File-trig THEN DO:
      old_name = _File-trig._Proc-name.
      new_name = wfit._Proc-name.
      IF  _File-trig._Event     = wfit._Event
      AND _File-trig._Override  = wfit._Override
      AND old_name              = new_name
      AND _File-trig._Trig-CRC  = wfit._Trig-CRC THEN NEXT trig_loop.
    END.

    /* Progress doesn't let you modify a trigger record, so delete and
       recreate. */
    IF AVAILABLE _File-trig THEN DELETE _File-trig.
    CREATE _File-trig.
    ASSIGN
      _File-trig._File-recid = RECID(_File)
      _File-trig._Event      = wfit._Event
      _File-trig._Override   = wfit._Override
      _File-trig._Proc-Name  = wfit._Proc-Name
      _File-trig._Trig-CRC   = wfit._Trig-CRC.
  END.
  FOR EACH _File-trig OF _File WHERE NOT CAN-DO(scrap,_File-trig._Event):
    DELETE _File-trig.
  END.
END.

ASSIGN
  drec_file   = RECID(_File)
  kindexcache = "". /* and reset index delete cache on db or file change */

RETURN.
