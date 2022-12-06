
DEFINE INPUT PARAMETER p_edbtype AS CHARACTER NO-UNDO.

DEFINE VARIABLE sseqn            AS CHARACTER FORMAT "x(60)" NO-UNDO.
DEFINE VARIABLE sfiln            AS CHARACTER NO-UNDO.
DEFINE VARIABLE sfldn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE asfldn           AS CHARACTER NO-UNDO.
DEFINE VARIABLE m21              AS CHARACTER NO-UNDO.
DEFINE VARIABLE sidxn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE ppi              AS CHARACTER NO-UNDO.
DEFINE VARIABLE spi              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cext             AS INTEGER   NO-UNDO.
DEFINE VARIABLE ri               AS INTEGER   NO-UNDO.
DEFINE VARIABLE msg              AS CHARACTER   EXTENT 4 NO-UNDO.
DEFINE VARIABLE max_idx_len      AS INTEGER NO-UNDO.
DEFINE VARIABLE max_id_length    AS INTEGER NO-UNDO.
DEFINE VARIABLE idbtype          AS CHARACTER NO-UNDO.

DEFINE VARIABLE l_files          AS CHARACTER NO-UNDO.
DEFINE VARIABLE l_seqs           AS CHARACTER NO-UNDO.
DEFINE VARIABLE l_views          AS CHARACTER NO-UNDO.

DEFINE BUFFER   a_DICTDB         FOR DICTDB._Field.

{ prodict/dictvar.i }
{ prodict/odb/odbvar.i } /* temp */
{ prodict/user/uservar.i }

/* DICTDB is the newly created schema holder.
 * DICTDB2 is the original progress database.
 */

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
FORM
  SKIP(1)
  msg[1] FORMAT "x(25)" LABEL "  File" SKIP
  msg[2] FORMAT "x(25)" LABEL " Field" SKIP
  msg[3] FORMAT "x(25)" LABEL " Index" SKIP
  msg[4] FORMAT "x(25)" LABEL " Componet" SKIP
  SKIP(1)
  WITH FRAME odb_fix ATTR-SPACE OVERLAY SIDE-LABELS ROW 4 CENTERED
  TITLE " Updating " + p_edbtype + " Schema Holder".
/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

IF NOT SESSION:BATCH-MODE  
 then assign SESSION:IMMEDIATE-DISPLAY = yes.

if  user_env[25] = "**all**"
 or user_env[25] = ""
 then assign
  l_files = "**all**"
  l_seqs  = "**all**"
  l_views = "**all**".
else if  num-entries(user_env[25],";") < 2
 then assign
  l_files = entry(1,user_env[25],";")
  l_seqs  = "**all**"
  l_views = "**all**".
else if  num-entries(user_env[25],";") < 3
 then assign
  l_files = entry(1,user_env[25],";")
  l_seqs  = entry(2,user_env[25],";")
  l_views = "**all**".
 else assign
  l_files = entry(1,user_env[25],";")
  l_seqs  = entry(2,user_env[25],";")
  l_views = entry(3,user_env[25],";").

IF  s_file-sel = "" 
 OR s_file-sel = ?
 THEN ASSIGN s_file-sel = "*". /* used for _file-selection */

idbtype = { adecomm/ds_type.i
                &direction = "ETOI"
                &from-type = "p_edbtype"
                }.

IF user_env[28] <> ? THEN
  max_idx_len = INTEGER(user_env[28]).
ELSE
  max_idx_len = 0.  /* Since a max length of 0 is silly, this implies no max */

IF user_env[29] <> ? THEN
  max_id_length = INTEGER(user_env[29]).
ELSE
  max_id_length = 0.

FOR EACH DICTDB2._View
  where ( l_views = "**all**"
       or lookup(DICTDB2._View._View-Name,l_views) <> 0
        ):
  IF NOT CAN-FIND (DICTDB._View WHERE DICTDB._View._View-Name = 
    DICTDB2._View._View-Name) THEN DO:
    CREATE DICTDB._View.
  END.
  ASSIGN
     DICTDB._View._View-Name    = DICTDB2._View._View-Name
     DICTDB._View._Auth-Id      = DICTDB2._View._Auth-Id
     DICTDB._View._Base-Tables  = DICTDB2._View._Base-Tables
     DICTDB._View._Where-Cls    = DICTDB2._View._Where-Cls
     DICTDB._View._Group-By     = DICTDB2._View._Group-By
     DICTDB._View._View-Def     = DICTDB2._View._View-Def
     DICTDB._View._Can-Read     = DICTDB2._View._Can-Read
     DICTDB._View._Can-Write    = DICTDB2._View._Can-Write
     DICTDB._View._Can-Create   = DICTDB2._View._Can-Create
     DICTDB._View._Can-Delete   = DICTDB2._View._Can-Delete
     DICTDB._View._Desc         = DICTDB2._View._Desc
     DICTDB._View._Updatable    = DICTDB2._View._Updatable.

  FOR EACH DICTDB2._View-Col of DICTDB2._View:
    IF NOT CAN-FIND (DICTDB._View-Col WHERE DICTDB._View-Col._View-Name =
      DICTDB2._View-Col._View-Name) THEN DO:
      CREATE DICTDB._View-Col.
    END.
    ASSIGN
      DICTDB._View-Col._View-Name  = DICTDB2._View-Col._View-Name
      DICTDB._View-Col._Auth-Id    = DICTDB2._View-Col._Auth-Id
      DICTDB._View-Col._Col-Name   = DICTDB2._View-Col._Col-Name
      DICTDB._View-Col._Base-Col   = DICTDB2._View-Col._Base-Col
      DICTDB._View-Col._Can-Write  = DICTDB2._View-Col._Can-Write
      DICTDB._View-Col._Can-Create = DICTDB2._View-Col._Can-Create
      DICTDB._View-Col._Vcol-Order = DICTDB2._View-Col._Vcol-Order.
  END. /* DICTDB2._View-Col */

  FOR EACH DICTDB2._View-Ref of DICTDB2._View:
    IF NOT CAN-FIND (DICTDB._View-Ref WHERE DICTDB._View-Ref._View-Name =
      DICTDB2._View-Ref._View-Name) THEN DO:
      CREATE DICTDB._View-Ref.
    END.
    ASSIGN
      DICTDB._View-Ref._View-Name = DICTDB2._View-Ref._View-Name
      DICTDB._View-Ref._Auth-Id   = DICTDB2._View-Ref._Auth-Id
      DICTDB._View-Ref._Ref-Table = DICTDB2._View-Ref._Ref-Table
      DICTDB._View-Ref._Base-Col  = DICTDB2._View-Ref._Base-Col.
  END. /* DICTDB2._View-Ref */

END. /* each DICTDB2._View */


FOR EACH DICTDB2._Sequence
  where ( l_seqs = "**all**"
       or lookup(DICTDB2._Sequence._Seq-Name,l_seqs) <> 0
        ):
  sseqn = _Seq-Name.

  sseqn = sseqn + "," + p_edbtype + "," + string (max_id_length).
  RUN prodict/misc/_resxlat.p (INPUT-OUTPUT sseqn).

  FIND DICTDB._Db WHERE RECID(DICTDB._Db) = drec_db.

  FIND DICTDB._Sequence OF DICTDB._Db 
	WHERE DICTDB._Sequence._Seq-name = sseqn NO-ERROR.

  IF NOT AVAILABLE DICTDB._Seq THEN DO:
    CREATE DICTDB._Seq.
  END.

  ASSIGN
    DICTDB._Sequence._Seq-Name = DICTDB2._Sequence._Seq-Name
    DICTDB._Sequence._Seq-Init = DICTDB2._Sequence._Seq-Init
    DICTDB._Sequence._Seq-Incr = DICTDB2._Sequence._Seq-Incr
    DICTDB._Sequence._Seq-Min  = DICTDB2._Sequence._Seq-Min
    DICTDB._Sequence._Seq-Max  = DICTDB2._Sequence._Seq-Max
    DICTDB._Sequence._Cycle-Ok = DICTDB2._Sequence._Cycle-OK.

END. /* each DICTDB2._Sequence */


/* To avoid double dump-names we set them to ? for now, they will be
 * set to dictdb2._File._Dump-name lateron
 */
FOR EACH DICTDB._File 
      WHERE DICTDB._File._DB-recid = drec_db
      and   ( l_files = "**all**"
           or lookup(DICTDB._File._File-name,l_files) <> 0
            ):
  ASSIGN DICTDB._File._Dump-name = ?.
END. /* each DICTDB2._File */


FOR EACH DICTDB2._File 
  WHERE DICTDB2._File._File-name MATCHES s_file-sel
  or    ( l_files = "**all**"
       or lookup(DICTDB2._File._File-name,l_files) <> 0
        ):
  

  IF DICTDB2._File._File-name BEGINS "_" THEN LEAVE.

  sfiln = DICTDB2._File._File-name.
  sfiln = sfiln + "," + p_edbtype + "," + string (max_id_length).
  RUN prodict/misc/_resxlat.p (INPUT-OUTPUT sfiln).

  FIND DICTDB._File 
      WHERE DICTDB._File._File-name = sfiln and 
            DICTDB._File._DB-recid = drec_db NO-ERROR.

  IF NOT AVAILABLE DICTDB._File THEN NEXT.

  FOR EACH DICTDB._Field OF DICTDB._File WHERE	/* Do first to avoid _order collisions */
    DICTDB._Field._For-Type = "TIME":
   
    IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
      DISPLAY  DICTDB._File._File-name @ msg[1]
	DICTDB._Field._Field-name @ msg[2]
	"" @ msg[3] "" @ msg[4]
	WITH FRAME odb_fix.
    DELETE DICTDB._Field.

  END. /* each DICTDB._Field */


  FOR EACH DICTDB2._File-Trig OF DICTDB2._File:
    IF NOT CAN-FIND (DICTDB._File-Trig OF DICTDB._File WHERE
      DICTDB._File-Trig._Event = DICTDB2._File-Trig._Event) THEN DO:
      CREATE DICTDB._File-Trig.
    END.
    ASSIGN
      DICTDB._File-Trig._File-Recid = RECID (DICTDB._File)
      DICTDB._File-Trig._Event      = DICTDB2._File-Trig._Event
      DICTDB._File-Trig._Proc-Name  = DICTDB2._File-Trig._Proc-Name
      DICTDB._File-Trig._Override   = DICTDB2._File-Trig._Override.
  END. /* each DICTDB2._File-Trig OF DICTDB2._File */


  FOR EACH DICTDB2._Field OF DICTDB2._File:
    sfldn = DICTDB2._Field._Field-name.

    /* Avoid collisions with unrolled extents */

    ri = R-INDEX (sfldn, "#").    
    IF ri > 2 AND ri < LENGTH (sfldn) THEN DO:
      IF INDEX ("0123456789", SUBSTR (sfldn, ri + 1, 1)) > 0 THEN DO:
	asfldn = SUBSTR (sfldn, 1, ri - 1).
	IF CAN-FIND (DICTDB._Field OF DICTDB._File WHERE
	  DICTDB._Field._Field-name = asfldn) THEN DO:
	  OVERLAY (sfldn, ri, 1) = "_".
	END.
      END.
    END.

    sfldn = sfldn + "," + p_edbtype + "," + string (max_id_length).
    RUN prodict/misc/_resxlat.p (INPUT-OUTPUT sfldn).

    IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
      DISPLAY  DICTDB2._File._File-name @ msg[1]
        DICTDB2._Field._Field-name @ msg[2]
        "" @ msg[3] "" @ msg[4]
        WITH FRAME odb_fix.

    IF DICTDB2._Field._Extent > 0 AND NOT CAN-FIND (DICTDB._Field OF DICTDB._File
      WHERE DICTDB._Field._Field-name = sfldn) THEN DO:
      asfldn = sfldn + "#".
      FIND a_DICTDB OF DICTDB._File WHERE a_DICTDB._Field-name = asfldn NO-ERROR.
      IF NOT AVAILABLE a_DICTDB THEN NEXT.
      m21 = a_DICTDB._For-Name.
      IF R-INDEX (m21, "#") > 3 THEN
	m21 = SUBSTR (m21, 1, R-INDEX(m21, "#") - 2).

      CREATE DICTDB._Field.
      ASSIGN
	DICTDB._Field._File-recid      = RECID(DICTDB._File)
	DICTDB._Field._For-Name        = m21
	DICTDB._Field._Fld-stdtype     = a_DICTDB._Fld-stdtype
	DICTDB._Field._Fld-stoff       = a_DICTDB._Fld-stoff
	DICTDB._Field._Fld-misc2[8]    = a_DICTDB._Fld-misc2[8]

	DICTDB._Field._Field-name      = DICTDB2._Field._Field-name
	DICTDB._Field._Fld-case        = DICTDB2._Field._Fld-case
	DICTDB._Field._Data-type       = DICTDB2._Field._Data-type
	DICTDB._Field._Format          = DICTDB2._Field._Format
	DICTDB._Field._Initial         = DICTDB2._Field._Initial
	DICTDB._Field._Mandatory       = DICTDB2._Field._Mandatory
	DICTDB._Field._Decimals        = DICTDB2._Field._Decimals
	DICTDB._Field._Order           = DICTDB2._Field._Order
	DICTDB._Field._Desc            = DICTDB2._Field._Desc
	DICTDB._Field._Can-Read        = DICTDB2._Field._Can-Read
	DICTDB._Field._Can-Write       = DICTDB2._Field._Can-Write
	DICTDB._Field._Label           = DICTDB2._Field._Label
	DICTDB._Field._Col-label       = DICTDB2._Field._Col-label
	DICTDB._Field._Valexp          = DICTDB2._Field._Valexp
	DICTDB._Field._Valmsg          = DICTDB2._Field._Valmsg
	DICTDB._Field._Help            = DICTDB2._Field._Help
        DICTDB._Field._Col-label-sa    = DICTDB2._Field._Col-label-sa
        DICTDB._Field._Format-sa       = DICTDB2._Field._Format-sa
        DICTDB._Field._Help-sa         = DICTDB2._Field._Help-sa
        DICTDB._Field._Initial-sa      = DICTDB2._Field._Initial-sa
        DICTDB._Field._Label-sa        = DICTDB2._Field._Label-sa
        DICTDB._Field._Valmsg-sa       = DICTDB2._Field._Valmsg-sa

	DICTDB._Field._For-Id          = DICTDB2._Field._For-Id
	DICTDB._Field._For-Primary     = DICTDB2._Field._For-Primary
	DICTDB._Field._For-Spacing     = DICTDB2._Field._For-Spacing
	DICTDB._Field._For-Scale       = DICTDB2._Field._For-Spacing
	DICTDB._Field._For-Type        = DICTDB2._Field._For-Type
	DICTDB._Field._For-Itype       = DICTDB2._Field._For-Itype
	DICTDB._Field._For-Xpos        = DICTDB2._Field._For-Xpos
	DICTDB._Field._For-Retrieve    = DICTDB2._Field._For-Retrieve
	DICTDB._Field._For-Separator   = DICTDB2._Field._For-Separator
	DICTDB._Field._For-Maxsize     = DICTDB2._Field._For-Maxsize
	DICTDB._Field._For-Allocated   = DICTDB2._Field._For-Allocated
	DICTDB._Field._View-As         = DICTDB2._Field._View-As

	DICTDB._Field._Extent          = DICTDB2._Field._Extent.

        IF DICTDB._Field._Data-type = "RECID" THEN
          DICTDB._Field._Data-type = "INTEGER".

	DO cext = 1 TO DICTDB._Field._Extent:
	    asfldn = sfldn + "#" + STRING (cext).

	    FIND a_DICTDB OF DICTDB._File WHERE a_DICTDB._Field-name = asfldn NO-ERROR.

	    IF AVAILABLE a_DICTDB THEN
	      DELETE a_DICTDB.

	END. /* DICTDB._Field._Extent > 1 DO cext = 1 TO ... */

    END. /* DICTDB2._Field._Extent > 0 */
    ELSE DO: /* !  DICTDB2._Field._Extent > 0 */

      FIND DICTDB._Field OF DICTDB._File
	WHERE DICTDB._Field._Field-name = sfldn NO-ERROR.
      IF NOT AVAILABLE DICTDB._Field THEN NEXT.
    END. /* !  DICTDB2._Field._Extent > 0 */

    ASSIGN
      DICTDB._Field._Field-name = DICTDB2._Field._Field-name
      DICTDB._Field._Fld-case   = DICTDB2._Field._Fld-case
      DICTDB._Field._Data-type  = DICTDB2._Field._Data-type
      DICTDB._Field._Format         = DICTDB2._Field._Format
      DICTDB._Field._Initial        = DICTDB2._Field._Initial
      DICTDB._Field._Mandatory      = DICTDB2._Field._Mandatory
      DICTDB._Field._Decimals       = DICTDB2._Field._Decimals
      DICTDB._Field._Order          = DICTDB2._Field._Order
      DICTDB._Field._Desc           = DICTDB2._Field._Desc
      DICTDB._Field._Can-Read       = DICTDB2._Field._Can-Read
      DICTDB._Field._Can-Write      = DICTDB2._Field._Can-Write
      DICTDB._Field._Label          = DICTDB2._Field._Label
      DICTDB._Field._Col-label      = DICTDB2._Field._Col-label
      DICTDB._Field._Valexp         = DICTDB2._Field._Valexp
      DICTDB._Field._Valmsg         = DICTDB2._Field._Valmsg
      DICTDB._Field._Col-label-sa   = DICTDB2._Field._Col-label-sa
      DICTDB._Field._Format-sa      = DICTDB2._Field._Format-sa
      DICTDB._Field._Help-sa        = DICTDB2._Field._Help-sa
      DICTDB._Field._Initial-sa     = DICTDB2._Field._Initial-sa
      DICTDB._Field._Label-sa       = DICTDB2._Field._Label-sa
      DICTDB._Field._Valmsg-sa      = DICTDB2._Field._Valmsg-sa

      DICTDB._Field._For-Id         = DICTDB2._Field._For-Id
      DICTDB._Field._For-Primary    = DICTDB2._Field._For-Primary
      DICTDB._Field._For-Spacing    = DICTDB2._Field._For-Spacing
      DICTDB._Field._For-Scale      = DICTDB2._Field._For-Scale
      DICTDB._Field._For-Itype      = DICTDB2._Field._For-Itype
      DICTDB._Field._For-Xpos       = DICTDB2._Field._For-Xpos
      DICTDB._Field._For-Retrieve   = DICTDB2._Field._For-Retrieve
      DICTDB._Field._For-Separator  = DICTDB2._Field._For-Separator
      DICTDB._Field._For-Maxsize    = DICTDB2._Field._For-Maxsize
      DICTDB._Field._For-Allocated  = DICTDB2._Field._For-Allocated
      DICTDB._Field._View-As = DICTDB2._Field._View-As

      DICTDB._Field._Help = DICTDB2._Field._Help.

      IF DICTDB._Field._Data-type = "RECID" THEN
        DICTDB._Field._Data-type = "INTEGER".

    FOR EACH DICTDB2._Field-Trig OF DICTDB2._Field:
      IF NOT CAN-FIND (DICTDB._Field-Trig OF DICTDB._Field WHERE
	DICTDB._Field-Trig._Event = DICTDB2._Field-Trig._Event) THEN DO:
	CREATE DICTDB._Field-Trig.
      END.
      ASSIGN
	DICTDB._Field-Trig._Field-Recid = RECID (DICTDB._Field)
	DICTDB._Field-Trig._File-Recid  = RECID (DICTDB._File)
	DICTDB._Field-Trig._Event       = DICTDB2._Field-Trig._Event
	DICTDB._Field-Trig._Proc-Name   = DICTDB2._Field-Trig._Proc-Name
	DICTDB._Field-Trig._Override    = DICTDB2._Field-Trig._Override.
    END. /* each DICTDB2._Field-Trig OF DICTDB2._Field */

  END. /* each DICTDB2._Field OF DICTDB2._File */

  FOR EACH DICTDB2._Index OF DICTDB2._File:
    sidxn = DICTDB2._Index._Index-Name.


    IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
      DISPLAY  DICTDB2._File._File-name @ msg[1] "" @ msg[2]
         DICTDB2._Index._Index-Name @ msg[3] "" @ msg[4]
         WITH FRAME odb_fix.

    sidxn = sidxn + "," + p_edbtype + "," + string (max_idx_len).
    RUN prodict/misc/_resxlat.p (INPUT-OUTPUT sidxn).

    FIND DICTDB._Index OF DICTDB._File
      WHERE DICTDB._Index._Index-Name = sidxn NO-ERROR.

      IF DICTDB2._Index._Index-name = "default" THEN
	sidxn = "default_".
      ELSE
        sidxn = DICTDB2._Index._Index-name.

    IF NOT AVAILABLE DICTDB._Index THEN DO:
      CREATE DICTDB._Index.
      ASSIGN
        DICTDB._Index._File-recid   = RECID (DICTDB._File)

        DICTDB._Index._Wordidx      = DICTDB2._Index._Wordidx
        DICTDB._Index._Desc         = DICTDB2._Index._Desc
        DICTDB._Index._For-Name     = DICTDB2._Index._For-Name
        DICTDB._Index._For-type     = DICTDB2._Index._For-type

        DICTDB._Index._Index-name   = sidxn
        DICTDB._Index._Active       = DICTDB2._Index._Active
        DICTDB._Index._idx-num      = DICTDB2._Index._idx-num
        DICTDB._Index._Unique       = DICTDB2._Index._Unique.
    END. /* NOT AVAILABLE DICTDB._Index */
    ELSE DO: /* AVAILABLE DICTDB._Index */
      ASSIGN
        DICTDB._Index._Desc         = DICTDB2._Index._Desc
	DICTDB._Index._Index-name      = sidxn.
    END. /* AVAILABLE DICTDB._Index */

    FOR EACH DICTDB2._Index-field OF DICTDB2._Index:

      FIND DICTDB2._Field WHERE RECID (DICTDB2._Field) = 
	DICTDB2._Index-field._Field-recid NO-ERROR.
      IF NOT AVAILABLE DICTDB2._Field THEN DO:
	NEXT.
      END.

      IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
	DISPLAY  DICTDB2._File._File-name @ msg[1] "" @ msg[2]
	  DICTDB2._Index._Index-Name @ msg[3]
          DICTDB2._Field._Field-name @ msg[4]
	  WITH FRAME odb_fix.

      sfldn = DICTDB2._Field._Field-name.

      FIND DICTDB._Field WHERE
	DICTDB._Field._File-recid = RECID (DICTDB._File) AND
	DICTDB._Field._Field-name = sfldn NO-ERROR.
      IF NOT AVAILABLE DICTDB._Field THEN DO:
	NEXT.
      END.

      FIND DICTDB._Index-field OF DICTDB._Index
	WHERE DICTDB._Index-field._Index-seq = DICTDB2._Index-field._Index-seq
	NO-ERROR.
      IF NOT AVAILABLE DICTDB._Index-field THEN DO:
	  CREATE DICTDB._Index-field.
	  ASSIGN
	    DICTDB._Index-field._Index-recid = RECID (DICTDB._Index)
	    DICTDB._Index-field._Field-recid = RECID (DICTDB._Field)
	    DICTDB._Index-field._Index-seq   = DICTDB2._Index-field._Index-seq
	    DICTDB._Index-field._Ascending   = DICTDB2._Index-field._Ascending
	    DICTDB._Index-field._Abbreviate  = DICTDB2._Index-field._Abbreviate
	    DICTDB._Index-field._Unsorted    = DICTDB2._Index-field._Unsorted.

      END. /* NOT avaiable DICTDB._Index-field */
      ELSE DO: /* avaiable DICTDB._Index-field */
	ASSIGN
          DICTDB._Index-field._Ascending   = DICTDB2._Index-field._Ascending
          DICTDB._Index-field._Abbreviate  = DICTDB2._Index-field._Abbreviate.
      END. /* avaiable DICTDB._Index-field */

    END. /* each DICTDB2._Index-field */

  END. /* each DICTDB2._Index OF DICTDB2._File */

	/* Remove any extra indexes from the schema holder */

  FOR EACH DICTDB._Index OF DICTDB._File:
    sidxn = DICTDB._Index._Index-Name.
    IF sidxn = "default_" THEN NEXT.

    IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
      DISPLAY  DICTDB._File._File-name @ msg[1] "" @ msg[2]
         DICTDB._Index._Index-Name @ msg[3] "" @ msg[4]
        WITH FRAME odb_fix.

    FIND DICTDB2._Index OF DICTDB2._File
      WHERE  DICTDB2._Index._Index-Name = DICTDB._Index._Index-Name NO-ERROR.
    IF AVAILABLE DICTDB2._Index THEN
      NEXT.

    FOR EACH DICTDB._Index-field OF DICTDB._Index:
      DELETE DICTDB._Index-field.
    END. /* DICTDB._Index-field OF DICTDB._Index */

    DELETE DICTDB._Index.

  END. /* DICTDB._Index OF DICTDB._File */

	/* Set the primary index */

  FIND DICTDB._Index WHERE RECID(DICTDB._Index) = DICTDB._File._Prime-Index
    NO-ERROR.
  IF AVAILABLE DICTDB._Index THEN
    spi = DICTDB._Index._Index-Name.
  ELSE
    spi = ?.

  FIND DICTDB2._Index WHERE RECID(DICTDB2._Index) = DICTDB2._File._Prime-Index
    NO-ERROR.
  IF AVAILABLE DICTDB2._Index THEN
    ppi = DICTDB2._Index._Index-Name.
  ELSE
    ppi = ?.

  IF spi <> ppi AND ppi <> ? THEN DO:
    FIND DICTDB._Index OF DICTDB._File WHERE
      DICTDB._Index._Index-Name = ppi NO-ERROR.
    IF AVAILABLE DICTDB._Index THEN DO:
      DICTDB._File._Prime-Index= RECID(DICTDB._Index).
    END. /* AVAILABLE DICTDB._Index */
  END. /* spi <> ppi AND ppi <> ? */

  IF TERMINAL <> "" and NOT SESSION:BATCH-MODE THEN
    DISPLAY DICTDB2._File._File-name @ msg[1] "" @ msg[2] "" @ msg[3] 
      "" @ msg[4]
      WITH FRAME odb_fix.
  /* Do this last since it may set _File._Frozen. */

  ASSIGN
    DICTDB._File._File-name     = DICTDB2._File._File-name
    DICTDB._File._Can-Create    = DICTDB2._File._Can-Create
    DICTDB._File._Can-Read      = DICTDB2._File._Can-Read
    DICTDB._File._Can-Write     = DICTDB2._File._Can-Write
    DICTDB._File._Can-Delete    = DICTDB2._File._Can-Delete
    DICTDB._File._Desc          = DICTDB2._File._Desc
    DICTDB._File._Valexp        = DICTDB2._File._Valexp
    DICTDB._File._Valmsg        = DICTDB2._File._Valmsg

    DICTDB._File._For-Size      = DICTDB2._File._For-Size
    DICTDB._File._For-Flag      = DICTDB2._File._For-Flag
    DICTDB._File._For-Cnt1      = DICTDB2._File._For-Cnt1
    DICTDB._File._For-Cnt2      = DICTDB2._File._For-Cnt2
    DICTDB._File._For-Format    = DICTDB2._File._For-Format
    DICTDB._File._For-Info      = DICTDB2._File._For-Info
    DICTDB._File._For-Id        = DICTDB2._File._For-ID
    DICTDB._File._For-Number    = DICTDB2._File._For-Number
    DICTDB._File._File-Label    = DICTDB2._File._File-Label
    DICTDB._File._File-Label-sa = DICTDB2._File._File-Label-sa
    DICTDB._File._Valmsg-sa     = DICTDB2._File._Valmsg-sa

    DICTDB._File._Dump-name     = DICTDB2._File._Dump-name.

  ASSIGN 
    DICTDB._File._Frozen        = DICTDB2._File._Frozen.

END. /* each DICTDB2._File */

IF NOT SESSION:BATCH-MODE THEN 
    HIDE FRAME odb_fix NO-PAUSE.

IF NOT SESSION:BATCH-MODE  
 then assign SESSION:IMMEDIATE-DISPLAY = no.

RETURN.


