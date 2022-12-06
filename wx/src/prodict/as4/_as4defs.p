/* _as4defs.p - Dump Data Definitions for AS/400 V7 Client
		Dumps file and field triggers only in "UPDATE" format */

DEFINE INPUT  PARAMETER pi_method AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pi_recid  AS RECID     NO-UNDO.

DEFINE VARIABLE v_ispro AS LOGICAL NO-UNDO.
DEFINE VARIABLE byte1   AS INTEGER NO-UNDO.
DEFINE VARIABLE byte2   AS INTEGER NO-UNDO.
DEFINE VARIABLE vers    AS CHAR    NO-UNDO.
DEFINE VARIABLE xtrig   AS LOGICAL NO-UNDO.

DEFINE SHARED STREAM ddl.

/***************************************************************************
IF pi_method BEGINS "d" OR pi_method BEGINS "c" THEN DO: /* collate tables */
  FIND _Db WHERE RECID(_Db) = pi_recid NO-LOCK.
  PUT STREAM ddl UNFORMATTED "UPDATE DATABASE """ _Db._Db-name """" SKIP.
  /* _Db-collate[5] is used to store the version #.  e.g., 2.0 would be
     stored as 2 bytes, 0x0200.  We only started storing the # starting
     at 2.0, so if it's not there, the version is 1.0.
  */
  ASSIGN
    byte1 = GETBYTE(_Db._Db-collate[5], 1) 
    byte2 = GETBYTE(_Db._Db-collate[5], 2).
  IF byte1 <> ? AND byte1 > 0 THEN 
    vers = STRING(byte1) + "." + STRING(byte2) + "-16".
  ELSE
    vers = "1.0-16".
  PUT STREAM ddl UNFORMATTED "  COLLATION-TRANSLATION-VERSION " vers SKIP.
  PUT STREAM ddl CONTROL "  TRANSLATION-NAME ".
  EXPORT STREAM ddl _Db._Db-xl-name.
  PUT STREAM ddl CONTROL "  COLLATION-NAME ".
  EXPORT STREAM ddl _Db._Db-coll-name.
  PUT STREAM ddl CONTROL "  INTERNAL-EXTERNAL-TRAN-TABLE ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-xlate[1]).
  PUT STREAM ddl CONTROL "  EXTERNAL-INTERNAL-TRAN-TABLE ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-xlate[2]).
  PUT STREAM ddl CONTROL "  CASE-INSENSITIVE-SORT ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-collate[1]).
  PUT STREAM ddl CONTROL "  CASE-SENSITIVE-SORT ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-collate[2]).
  PUT STREAM ddl CONTROL "  UPPERCASE-MAP ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-collate[3]).
  PUT STREAM ddl CONTROL "  LOWERCASE-MAP ".
    PUT STREAM ddl UNFORMATTED SKIP(1).
    RUN prodict/dump/_dmp_raw.p (_Db._Db-collate[4]).
  PUT STREAM ddl UNFORMATTED SKIP(1).
END.

ELSE
****************************************************************************/

IF pi_method BEGINS "t" THEN DO: /*----------------------*/ /* table_record */
  
  FIND _File WHERE RECID(_File) = pi_recid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE _File THEN DO:
    FIND _Field WHERE RECID(_Field) = pi_recid NO-LOCK NO-ERROR.
    IF AVAILABLE _Field THEN FIND _File OF _Field.
  END.
  IF NOT AVAILABLE _File THEN DO:
    FIND _Index WHERE RECID(_Index) = pi_recid NO-LOCK NO-ERROR.
    IF AVAILABLE _Index THEN FIND _File OF _Index.
  END.

  FIND _Db OF _File NO-LOCK.
  IF RECID(_File) = pi_recid THEN DO:

  /* only dump update table line if table has file or field trigger */
    IF NOT CAN-FIND (FIRST _File-Trig OF _File) THEN DO:
      ASSIGN xtrig = FALSE.
      FOR EACH _Field OF _File NO-LOCK:
	IF CAN-FIND (FIRST _Field-Trig OF _Field) THEN
	  ASSIGN xtrig = TRUE.
      END.
      IF NOT xtrig THEN RETURN.
    END.    

    PUT STREAM ddl UNFORMATTED "UPDATE TABLE """ _File._File-name """" SKIP.
    FOR EACH _File-trig OF _File NO-LOCK BY _Event:
      PUT STREAM ddl UNFORMATTED
	"  TABLE-TRIGGER """ _File-Trig._Event """ "
	(IF _File-Trig._Override THEN 'OVERRIDE' ELSE 'NO-OVERRIDE') " "
	"PROCEDURE """ _File-Trig._Proc-Name """ "
	"CRC """ _File-Trig._Trig-CRC """ " SKIP.
    END. /* for each file-trig */
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END. /* if recid(file) */

  FOR EACH _Field OF _File NO-LOCK BY _Field-rpos:
    IF RECID(_File) <> pi_recid AND RECID(_Field) <> pi_recid THEN NEXT.
    IF NOT CAN-FIND (FIRST _Field-Trig OF _Field) THEN NEXT.
    PUT STREAM ddl UNFORMATTED
      "UPDATE FIELD """ _Field._Field-name """ "
      "OF """ _File._File-name """ "
      SKIP.
    FOR EACH _Field-trig OF _Field NO-LOCK BY _Event:
      PUT STREAM ddl UNFORMATTED
	"  FIELD-TRIGGER """ _Field-Trig._Event """ "
	(IF _Field-Trig._Override THEN 'OVERRIDE' ELSE 'NO-OVERRIDE') " "
	"PROCEDURE """ _Field-Trig._Proc-Name """ "
	"CRC """ _Field-Trig._Trig-CRC """ " SKIP.
    END.
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END. /* each field of file */

END.  /* pi_method begins t */

RETURN.
