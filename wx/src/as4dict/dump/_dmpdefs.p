/*-----------------------------------------------------------------*/
/* as4dict/dump/_dmpdefs.p - Dump Data Definitions for the As/400  */
/*                           Taken from prodict/dump/_dmpdefs      */
/* history:                                                        */
/*   Nhorn   5/01/95   Initial creation from prodict/dump/_dmpdefs */
/*-----------------------------------------------------------------*/       

DEFINE INPUT  PARAMETER pi_method AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pi_filenum AS INTEGER NO-UNDO.

{as4dict/dictvar.i shared }

DEFINE VARIABLE v_ispro AS LOGICAL NO-UNDO.
DEFINE VARIABLE byte1   AS INTEGER NO-UNDO.
DEFINE VARIABLE byte2   AS INTEGER NO-UNDO.
DEFINE VARIABLE vers    AS CHAR    NO-UNDO.

DEFINE SHARED STREAM ddl.
DEFINE SHARED FRAME working.   

/* User_Env[29] - will be equal to "incr" when coming from the incremental
   dump program.  In this case, we don't want to use frame "working",
   because the incremental program has its own frame.  We suppress the
   display commands when User_Env[29] is equal to "Incr".               */
   
/* put in dict var when merging */
/* DEFINE SHARED VAR dump_format as CHAR NO-UNDO. */

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/

{ as4dict/dump/as4dmpdf.f &FRAME = "working" }

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

IF pi_method BEGINS "d" and dump_format <> "AS400" THEN DO:
  FOR EACH as4dict.p__Db:
    PUT STREAM ddl UNFORMATTED
      "UPDATE DATABASE """ ? """ " SKIP.
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END.
END.

ELSE

IF pi_method BEGINS "s" THEN DO: /*-------------------------*/ /* sequences */
   FOR EACH AS4DICT.P__Seq BY P__SEQ._Seq-Name:
     IF TERMINAL <> "" THEN
        DISPLAY as4dict.p__seq._Seq-Name WITH FRAME working.
     PUT STREAM ddl UNFORMATTED "ADD SEQUENCE """ as4dict.P__Seq._Seq-Name """" SKIP.
     PUT STREAM ddl UNFORMATTED "  INITIAL " as4dict.P__Seq._Seq-Init SKIP.
     PUT STREAM ddl UNFORMATTED "  INCREMENT " as4dict.P__Seq._Seq-Incr SKIP.
     PUT STREAM ddl CONTROL "  CYCLE-ON-LIMIT ".
     EXPORT STREAM ddl as4dict.P__Seq._Cycle-Ok.
     IF as4dict.P__Seq._Seq-Min <> ? THEN
       PUT STREAM ddl UNFORMATTED "  MIN-VAL " as4dict.P__Seq._Seq-Min SKIP.
     IF as4dict.P__Seq._Seq-Max <> ? AND as4dict.p__Seq._Seq-Max <> 0 THEN
       PUT STREAM ddl UNFORMATTED "  MAX-VAL " as4dict.P__Seq._Seq-Max SKIP.
     PUT STREAM ddl UNFORMATTED SKIP(1).
   END.
    IF TERMINAL <> "" THEN
      DISPLAY "" @ as4dict.p__seq._Seq-Name WITH FRAME working. 
END.    
ELSE
IF pi_method BEGINS "t" THEN DO: /*----------------------*/ /* table_record */
  FIND as4dict.p__File WHERE as4dict.p__file._File-Number = pi_filenum NO-LOCK NO-ERROR.
  IF NOT AVAILABLE as4dict.p__File THEN DO:
    FIND as4dict.p__Field WHERE as4dict.p__Field._Fld-Number = pi_filenum NO-LOCK NO-ERROR.
    IF AVAILABLE as4dict.p__Field THEN FIND as4dict.p__file 
          where as4dict.p__field._File-Number = as4dict.p__file._File-Number.
  END.
  IF NOT AVAILABLE P__File THEN DO:
    FIND as4dict.p__index WHERE as4dict.P__INDEX._Idx-Num = pi_filenum NO-LOCK NO-ERROR.
    IF AVAILABLE as4dict.P__Index THEN FIND as4dict.P__File 
       WHERE as4dict.P__File._File-Number = as4dict.P__Index._File-Number.
  END.
  FIND FIRST as4dict.p__Db NO-LOCK.
  IF as4dict.P__File._File-Number = pi_filenum THEN DO:
/*   IF as4dict.p__File._For-Flag > 0 AND dump_format = "AS400" THEN NEXT. */
    PUT STREAM ddl UNFORMATTED "ADD TABLE """ as4dict.p__File._File-name """".
    IF dump_format = "AS400" THEN 
      PUT STREAM ddl UNFORMATTED "  TYPE AS400 " SKIP.
    ELSE
      PUT STREAM ddl UNFORMATTED SKIP.
    IF as4dict.p__File._Can-Create <> '*' AND as4dict.p__file._Can-Create <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-CREATE ".
      EXPORT STREAM ddl as4dict.p__File._Can-Create.
    END.
    IF as4dict.p__File._Can-Delete <> '*' AND as4dict.p__file._Can-Delete <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-DELETE ".
      EXPORT STREAM ddl as4dict.p__File._Can-Delete.
    END.
    IF as4dict.p__File._Can-Read <> '*' AND as4dict.p__file._Can-Read <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-READ ".
      EXPORT STREAM ddl as4dict.p__File._Can-Read.
    END.
    IF as4dict.p__File._Can-Write <> '*' AND as4dict.p__file._Can-Write <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-WRITE ".
      EXPORT STREAM ddl as4dict.p__File._Can-Write.
    END.
    IF as4dict.p__File._Can-Dump <> '*' AND as4dict.p__file._Can-Dump <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-DUMP ".
      EXPORT STREAM ddl as4dict.p__File._Can-Dump.
    END.
    IF as4dict.p__File._Can-Load <> '*' AND as4dict.p__file._Can-Load <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-LOAD ".
      EXPORT STREAM ddl as4dict.p__File._Can-Load.
    END.
    IF as4dict.p__File._File-Label <> ? AND as4dict.p__File._File-Label <> '' THEN DO:
      PUT STREAM ddl CONTROL "  LABEL ".
      EXPORT STREAM ddl as4dict.p__File._File-Label.
    END.
    IF as4dict.p__File._File-Label-SA <> ? AND as4dict.p__File._File-Label-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  LABEL-SA ".
      EXPORT STREAM ddl as4dict.p__File._File-Label-SA.
    END.
    IF as4dict.p__File._Desc <> ? AND as4dict.p__File._Desc <> '' THEN DO:
      PUT STREAM ddl CONTROL "  DESCRIPTION ".
      EXPORT STREAM ddl as4dict.p__File._Desc.
    END.
    IF as4dict.p__File._Valexp <> ? AND as4dict.p__File._Valexp <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALEXP ".
      EXPORT STREAM ddl as4dict.p__File._Valexp.
    END.
    IF as4dict.p__File._Valmsg <> ? AND as4dict.p__File._Valmsg <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALMSG ".
      EXPORT STREAM ddl as4dict.p__File._Valmsg.
    END.
    IF as4dict.p__File._Valmsg-SA <> ? AND as4dict.p__File._Valmsg-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALMSG-SA ".
      EXPORT STREAM ddl as4dict.p__File._Valmsg-SA.
    END.
    IF as4dict.p__File._Frozen = "Y" THEN
      PUT STREAM ddl UNFORMATTED "  FROZEN" SKIP.
    IF as4dict.p__File._Hidden = "Y" THEN
      PUT STREAM ddl UNFORMATTED "  HIDDEN" SKIP.
    IF as4dict.p__File._Dump-name <> ? AND as4dict.p__file._dump-name <> '' THEN DO:
      PUT STREAM ddl CONTROL "  DUMP-NAME ".
      EXPORT STREAM ddl as4dict.p__File._Dump-name.
    END.
    IF dump_format = "AS400" THEN DO:
      IF as4dict.p__file._As4-File <> "" THEN
         PUT STREAM ddl UNFORMATTED "  AS400-FILE " as4dict.p__File._As4-File SKIP.         
      /* Don't dump these switches for virtual files, let the load generate the
         default settings */
      IF as4dict.p__file._Fil-Misc2[4] <> "" AND NOT as4dict.p__file._For-Flag < 2 THEN
         PUT STREAM ddl UNFORMATTED "  AS400-FLAGS " as4dict.p__File._Fil-misc2[4] SKIP.     
      IF as4dict.p__File._For-Format <> ? and as4dict.p__file._For-Format <> "" THEN DO:
         PUT STREAM ddl CONTROL "  FORMAT-NAME ".
         EXPORT STREAM ddl as4dict.p__File._For-Format.
      END.
    END.  /* AS400 FORMAT */
/* ------  Take this out for AS400 to Progress dump    -------------
    IF as4dict.p__File._For-Flag <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-FLAGS " as4dict.p__File._For-Flag SKIP.
   END.
    IF _File._For-Cnt1 <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-GLOBAL " _File._For-Cnt1 SKIP.
    IF _File._For-Id <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-ID " _File._For-Id SKIP.
    IF _File._For-Cnt2 <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-LOCAL " _File._For-Cnt2 SKIP.
    IF _File._For-Info <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-MARK ".
      EXPORT STREAM ddl _File._For-Info.
    END.
    IF _File._For-Name <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-NAME ".
      EXPORT STREAM ddl _File._For-Name.
    END.
    IF _File._For-Number <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-NUMBER " _File._For-Number SKIP.
    IF _File._For-Owner <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-OWNER ".
      EXPORT STREAM ddl _File._For-Owner.
    END.
    IF _File._For-Size <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-SIZE " _File._For-Size SKIP.
    IF _File._For-Type <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-TYPE ".
      EXPORT STREAM ddl _File._For-Type.
    END.
    IF _File._Fil-misc1[1] <> ? THEN DO:
      IF CAN-DO("ORACLE,SYBASE," + odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl UNFORMATTED "  PROGRESS-RECID " _File._Fil-misc1[1] SKIP.
        ELSE PUT STREAM ddl UNFORMATTED "  FILE-MISC11 "    _File._Fil-misc1[1] SKIP.
    END.
    IF _File._Fil-misc1[2] <> ? THEN DO:
      IF CAN-DO("RMS",_Db._Db-type)
        THEN DO:
          PUT STREAM ddl CONTROL "  FOREIGN-SPAN ".
          EXPORT STREAM ddl (IF _File._Fil-misc1[2] = 1 THEN 'yes' ELSE 'no').
        END.
        ELSE PUT STREAM ddl UNFORMATTED "  FILE-MISC12 "    _File._Fil-misc1[2] SKIP.
    END.
    IF _File._Fil-misc1[3] <> ? THEN DO:
      IF CAN-DO(odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl UNFORMATTED "  INDEX-FREE-FLD " _File._Fil-misc1[3] SKIP.
        ELSE PUT STREAM ddl UNFORMATTED "  FILE-MISC13 "    _File._Fil-misc1[3] SKIP.
    END.
    IF (_File._Fil-misc1[4] <> ?) THEN DO:
      IF CAN-DO("ORACLE",_Db._Db-type)
        THEN PUT STREAM ddl UNFORMATTED "  RECID-COL-NO " _File._Fil-misc1[4] SKIP.
        ELSE PUT STREAM ddl UNFORMATTED "  FILE-MISC14 "  _File._Fil-misc1[4] SKIP.
    END.
    IF (_File._Fil-misc1[5] <> ?) THEN
      PUT STREAM ddl UNFORMATTED "  FILE-MISC15 " _File._Fil-misc1[5] SKIP.
    IF (_File._Fil-misc1[6] <> ?) THEN
      PUT STREAM ddl UNFORMATTED "  FILE-MISC16 " _File._Fil-misc1[6] SKIP.
    IF (_File._Fil-misc1[7] <> ?) THEN
      PUT STREAM ddl UNFORMATTED "  FILE-MISC17 " _File._Fil-misc1[7] SKIP.
    IF (_File._Fil-misc1[8] <> ?) THEN
      PUT STREAM ddl UNFORMATTED "  FILE-MISC18 " _File._Fil-misc1[8] SKIP.
    IF _File._Fil-misc2[1] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  QUALIFIER ".
      EXPORT STREAM ddl  _File._Fil-misc2[1].
    END.
    IF _File._Fil-misc2[2] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  HIDDEN-FLDS ".
      EXPORT STREAM ddl  _File._Fil-misc2[2].
    END.
    IF _File._Fil-misc2[3] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  RECID-FLD-NAME ".
      EXPORT STREAM ddl  _File._Fil-misc2[3].
    END.
    IF _File._Fil-misc2[4] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FLD-NAMES-LIST ".
      EXPORT STREAM ddl  _File._Fil-misc2[4].
    END.
    IF _File._Fil-misc2[5] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FILE-MISC25 ".
      EXPORT STREAM ddl  _File._Fil-misc2[5].
    END.
    IF _File._Fil-misc2[6] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FILE-MISC26 ".
      EXPORT STREAM ddl  _File._Fil-misc2[6].
    END.
    IF _File._Fil-misc2[7] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FILE-MISC27 ".
      EXPORT STREAM ddl  _File._Fil-misc2[7].
    END.
    IF _File._Fil-misc2[8] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  DB-LINK-NAME ".
      EXPORT STREAM ddl  _File._Fil-misc2[8].
    END.                                                            */
    FOR EACH as4dict.p__trgfl where p__trgfl._File-Number = p__file._File-number
        NO-LOCK BY _Event:
      PUT STREAM ddl UNFORMATTED
        "  TABLE-TRIGGER """ as4dict.p__trgfl._Event """ "
        (IF as4dict.p__Trgfl._Override = "Y" THEN 'OVERRIDE' ELSE 'NO-OVERRIDE') " "
        "PROCEDURE """ p__Trgfl._Proc-Name """ "
        "CRC """ p__Trgfl._Trig-CRC """ " SKIP.
    END.
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END.
  FOR EACH as4dict.p__Field WHERE as4dict.p__field._File-Number = as4dict.p__File._File-Number
        NO-LOCK BY _Field-rpos:
    IF as4dict.p__file._File-Number <> pi_filenum AND 
        as4dict.p__Field._Fld-Number <> pi_filenum THEN NEXT.

    /*  Skip AS/400 only fields (Fld-misc2[5] = A                      */
    IF as4dict.p__field._Fld-Misc2[5] = "A" THEN NEXT.
    IF TERMINAL <> "" AND NOT User_Env[29] BEGINS "Incr" THEN
         DISPLAY as4dict.p__field._Field-name with frame working.
    PUT STREAM ddl UNFORMATTED
      "ADD FIELD """ as4dict.p__Field._Field-name """ "
      "OF """ as4dict.p__File._File-name """ "
      "AS " as4dict.p__Field._Data-type " " SKIP.
    IF as4dict.p__Field._Desc <> ? AND as4dict.p__Field._Desc <> '' THEN DO:
      PUT STREAM ddl CONTROL "  DESCRIPTION ".
      EXPORT STREAM ddl as4dict.p__Field._Desc.
    END.
    PUT STREAM ddl CONTROL "  FORMAT ".
    EXPORT STREAM ddl as4dict.p__Field._Format.
    IF as4dict.p__Field._Format-SA <> ? AND as4dict.p__Field._Format-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  FORMAT-SA ".
      EXPORT STREAM ddl as4dict.p__Field._Format-SA.
    END.
    PUT STREAM ddl CONTROL "  INITIAL ".
    EXPORT STREAM ddl as4dict.p__Field._Initial.
    IF as4dict.p__Field._Initial-SA <> ? AND as4dict.p__Field._Initial-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  INITIAL-SA ".
      EXPORT STREAM ddl as4dict.p__Field._Initial-SA.
    END.
    IF as4dict.p__Field._Label <> ? AND as4dict.p__field._Label <> '' THEN DO:
      PUT STREAM ddl CONTROL "  LABEL ".
      EXPORT STREAM ddl as4dict.p__Field._Label.
    END.
    IF as4dict.p__Field._Label-SA <> ? AND as4dict.p__Field._Label-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  LABEL-SA ".
      EXPORT STREAM ddl as4dict.p__Field._Label-SA.
    END.
    IF as4dict.p__Field._View-As <> ? AND as4dict.p__field._View-As <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VIEW-AS ".
      EXPORT STREAM ddl as4dict.p__Field._View-As.
    END.
    IF as4dict.p__Field._Col-label <> ? AND as4dict.p__Field._Col-label <> '' THEN DO:
      PUT STREAM ddl CONTROL "  COLUMN-LABEL ".
      EXPORT STREAM ddl as4dict.p__Field._Col-label.
    END.
    IF as4dict.p__field._Col-label-SA <> ? AND as4dict.p__field._Col-label-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  COLUMN-LABEL-SA ".
      EXPORT STREAM ddl as4dict.p__field._Col-label-SA.
    END.
    IF as4dict.p__field._Can-Read <> '*' AND as4dict.p__field._Can-Read <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-READ ".
      EXPORT STREAM ddl as4dict.p__field._Can-Read.
    END.
    IF as4dict.p__field._Can-Write <> '*' AND as4dict.p__field._Can-Write <> '' THEN DO:
      PUT STREAM ddl CONTROL "  CAN-WRITE ".
      EXPORT STREAM ddl as4dict.p__field._Can-Write.
    END.
    IF as4dict.p__field._Valexp <> ? AND as4dict.p__field._Valexp <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALEXP ".
      EXPORT STREAM ddl as4dict.p__field._Valexp.
    END.
    IF as4dict.p__field._Valmsg <> ? AND as4dict.p__field._Valmsg <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALMSG ".
      EXPORT STREAM ddl as4dict.p__field._Valmsg.
    END.
    IF as4dict.p__field._Valmsg-SA <> ? AND as4dict.p__field._Valmsg-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  VALMSG-SA ".
      EXPORT STREAM ddl as4dict.p__field._Valmsg-SA.
    END.
    IF as4dict.p__field._Help <> ? AND as4dict.p__field._Help <> '' THEN DO:
      PUT STREAM ddl CONTROL "  HELP ".
      EXPORT STREAM ddl as4dict.p__field._Help.
    END.
    IF as4dict.p__field._Help-SA <> ? AND as4dict.p__field._Help-SA <> '' THEN DO:
      PUT STREAM ddl CONTROL "  HELP-SA ".
      EXPORT STREAM ddl as4dict.p__field._Help-SA.
    END.
    IF as4dict.p__field._Extent > 0 THEN
      PUT STREAM ddl UNFORMATTED "  EXTENT " as4dict.p__field._Extent SKIP.
    IF as4dict.p__field._Decimals <> ? AND as4dict.p__field._Decimals <> 0
      AND as4dict.p__field._Data-Type BEGINS "dec" THEN
      PUT STREAM ddl UNFORMATTED "  DECIMALS " as4dict.p__field._Decimals SKIP.
    IF as4dict.p__field._Decimals <> ? AND as4dict.p__field._Decimals <> 0 
      AND as4dict.p__field._Data-Type BEGINS "char" THEN
      PUT STREAM ddl UNFORMATTED "  LENGTH " as4dict.p__field._Decimals SKIP.
    PUT STREAM ddl UNFORMATTED "  ORDER " as4dict.p__field._Order SKIP.
    IF as4dict.p__field._Mandatory = "Y" THEN
      PUT STREAM ddl UNFORMATTED "  MANDATORY" SKIP.
    IF as4dict.p__field._Fld-case = "Y" AND as4dict.p__field._Data-Type BEGINS "char" THEN
      PUT STREAM ddl UNFORMATTED "  CASE-SENSITIVE" SKIP.

/*------   Information dumped for AS400 only  -------*/
    IF dump_format = "AS400" THEN DO:
       IF as4dict.p__Field._Fld-misc2[5] <> ? AND as4dict.p__field._fld-misc2[5] <> '' THEN  
         PUT STREAM ddl UNFORMATTED "  FLD-USAGE-TYPE " as4dict.p__Field._Fld-misc2[5] SKIP.
       IF as4dict.p__Field._Fld-misc2[6] <> ? AND as4dict.p__field._fld-misc2[6] <> "" THEN
         PUT STREAM ddl UNFORMATTED "  DDS-TYPE " as4dict.p__field._fld-misc2[6] SKIP.
       IF as4dict.p__field._Fld-stdtype <> 0 THEN
         PUT STREAM ddl UNFORMATTED "  FLD-STDTYPE " as4dict.p__field._fld-stdtype SKIP.
       IF as4dict.p__field._Fld-stlen <> ? AND as4dict.p__field._fld-stlen <> 0 THEN
         PUT STREAM ddl UNFORMATTED "  FLD-STLEN " as4dict.p__field._Fld-stlen SKIP.
       IF as4dict.p__field._For-Allocated <> ? AND as4dict.p__field._For-Allocated <> 0 THEN
         PUT STREAM ddl UNFORMATTED "  FOREIGN-ALLOCATED " as4dict.p__field._For-Allocated SKIP.
       IF as4dict.p__field._For-Maxsize <> ? AND as4dict.p__field._For-Maxsize <> 0 THEN
         PUT STREAM ddl UNFORMATTED "  FOREIGN-MAXIMUM " as4dict.p__field._For-Maxsize SKIP.
       IF as4dict.p__field._For-Name <> ? AND as4dict.p__field._For-Name <> "" THEN 
         PUT STREAM ddl UNFORMATTED "  FOREIGN-NAME " as4dict.p__field._For-Name SKIP.
       IF as4dict.p__field._For-Type <> ? AND as4dict.p__field._For-Type <> "" THEN 
         PUT STREAM ddl UNFORMATTED "  AS400-TYPE " as4dict.p__field._For-Type SKIP.
        PUT STREAM ddl UNFORMATTED (if as4dict.p__field._Fld-Misc2[2] = "Y" THEN
             "   NULL-CAPABLE " else "") SKIP.
    END. /* AS400 format dump */
/* ---------------------------------------------------------------------
    IF as4dict.p__field._Fld-stoff <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-POS " as4dict.p__field._Fld-stoff SKIP.
    IF as4dict.p__field._Fld-stdtype = 38 AND _Db._Db-type = 'RMS' THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-BITS " as4dict.p__field._Decimals SKIP.
    IF as4dict.p__field._For-Itype <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-CODE " as4dict.p__field._For-Itype SKIP.
    IF as4dict.p__field._For-Id <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-ID " as4dict.p__field._For-Id SKIP.
    END.
    IF as4dict.p__field._For-Retrieve <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-RETRIEVE ".
      EXPORT STREAM ddl as4dict.p__field._For-Retrieve.
    END.
    IF as4dict.p__field._For-Scale <> ? AND as4dict.p__field._For-Scale <> 0 THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-SCALE " as4dict.p__field._For-Scale SKIP.
    IF as4dict.p__field._For-Spacing <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-SPACING " as4dict.p__field._For-Spacing SKIP.
   IF as4dict.p__field._For-xpos <> ? THEN
      PUT STREAM ddl UNFORMATTED "  FOREIGN-XPOS " as4dict.p__field._For-xpos SKIP.
    IF as4dict.p__field._For-Separator <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-SEP ".
      EXPORT STREAM ddl as4dict.p__field._For-Separator.
    END.
  IF as4dict.p__field._Fld-misc1[1] <> ? THEN DO:
      IF CAN-DO("ORACLE," + odbtyp,_Db._Db-type) 
        THEN PUT STREAM ddl CONTROL "  DSRVR-PRECISION ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC11 ".
      EXPORT STREAM ddl _Field._Fld-misc1[1].
    END.
    IF _Field._Fld-misc1[2] <> ? THEN DO:
      IF CAN-DO(odbtyp,_Db._Db-type) 
        THEN PUT STREAM ddl CONTROL "  DSRVR-SCALE ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC12 ".
      EXPORT STREAM ddl _Field._Fld-misc1[2].
    END.
    IF _Field._Fld-misc1[3] <> ? THEN DO:
      IF CAN-DO(odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  DSRVR-LENGTH ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC13 ".
      EXPORT STREAM ddl _Field._Fld-misc1[3].
    END.
    IF _Field._Fld-misc1[4] <> ? THEN DO:
      IF CAN-DO(odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  DSRVR-FLDMISC ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC14 ".
      EXPORT STREAM ddl _Field._Fld-misc1[4].
    END.
    IF _Field._Fld-misc1[5] <> ? THEN DO:
      IF CAN-DO(odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  DSRVR-SHADOW ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC15 ".
      EXPORT STREAM ddl _Field._Fld-misc1[5].
    END.
    IF _Field._Fld-misc1[6] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FIELD-MISC16 ".
      EXPORT STREAM ddl _Field._Fld-misc1[6].
    END.
    IF _Field._Fld-misc1[7] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FIELD-MISC17 ".
      EXPORT STREAM ddl _Field._Fld-misc1[7].
    END.
    IF _Field._Fld-misc1[8] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FIELD-MISC18 ".
      EXPORT STREAM ddl _Field._Fld-misc1[8].
    END.
    IF _Field._Fld-misc2[1] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FIELD-MISC21 ".
      EXPORT STREAM ddl _Field._Fld-misc2[1].
    END.
    IF _Field._Fld-misc2[2] <> ? THEN DO:
      IF CAN-DO("ORACLE,SYBASE," + odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  SHADOW-COL ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC22 ".
      EXPORT STREAM ddl _Field._Fld-misc2[2].
    END.
    IF _Field._Fld-misc2[3] <> ? THEN DO:
      IF CAN-DO("ORACLE,SYBASE," + odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  QUOTED-NAME ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC23 ".
      EXPORT STREAM ddl _Field._Fld-misc2[3].
    END.
    IF _Field._Fld-misc2[4] <> ? THEN DO:
      IF CAN-DO("ORACLE,SYBASE," + odbtyp,_Db._Db-type)
        THEN PUT STREAM ddl CONTROL "  MISC-PROPERTIES ".
        ELSE PUT STREAM ddl CONTROL "  FIELD-MISC24 ".
      EXPORT STREAM ddl _Field._Fld-misc2[4].
    END.
    IF _Field._Fld-misc2[7] <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FIELD-MISC27 ".
      EXPORT STREAM ddl _Field._Fld-misc2[7].
    END.
    ------------------------------------------------------*/
    FOR EACH as4dict.p__trgfd WHERE as4dict.p__trgfd._Fld-Number = as4dict.p__Field._Fld-Number
         AND as4dict.p__trgfd._File-Number = as4dict.p__Field._File-Number
         NO-LOCK BY as4dict.p__Trgfd._Event:
      PUT STREAM ddl UNFORMATTED
        "  FIELD-TRIGGER """ as4dict.p__Trgfd._Event """ "
        (IF as4dict.p__Trgfd._Override = "Y" THEN 'OVERRIDE' ELSE 'NO-OVERRIDE') " "
        "PROCEDURE """ as4dict.p__Trgfd._Proc-Name """ "
        "CRC """ as4dict.p__Trgfd._Trig-CRC """ " SKIP.
    END.
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END.

  IF TERMINAL <> "" AND NOT User_Env[29] BEGINS "incr" THEN
     DISPLAY "" @ as4dict.p__field._Field-name with frame working.

  FOR EACH as4dict.p__Index WHERE as4dict.p__index._File-Number = as4dict.p__file._File-Number
    AND (as4dict.p__File._dft-pk <> "Y" OR 
       as4dict.p__File._Prime-Index <> as4dict.p__Index._Idx-Num) NO-LOCK
    BY STRING(as4dict.p__File._Prime-Index = as4dict.p__Index._Idx-Num, "1/2") +
        as4dict.p__Index._Index-name:
    IF as4dict.p__File._File-Number <> pi_filenum AND 
         as4dict.p__Index._Idx-Num <> pi_filenum THEN NEXT.

    IF TERMINAL <> "" AND NOT User_Env[29] BEGINS "incr" THEN 
        DISPLAY as4dict.p__Index._Index-name with frame working.
    PUT STREAM ddl UNFORMATTED
      "ADD INDEX """ as4dict.p__Index._Index-Name """ "
      "ON """ as4dict.p__File._File-name """ " SKIP.
    IF as4dict.p__Index._Unique = "Y" THEN
      PUT STREAM ddl UNFORMATTED "  UNIQUE" SKIP.
    IF as4dict.p__Index._Active <> "Y" THEN
      PUT STREAM ddl UNFORMATTED "  INACTIVE" SKIP.
    IF as4dict.p__File._Prime-index = as4dict.p__index._Idx-Num THEN
      PUT STREAM ddl UNFORMATTED "  PRIMARY" SKIP.
    IF as4dict.p__Index._Wordidx = 1 THEN
      PUT STREAM ddl UNFORMATTED "  WORD" SKIP.
    IF as4dict.p__Index._Desc <> ? AND as4dict.p__File._Desc <> '' THEN DO:
      PUT STREAM ddl CONTROL "  DESCRIPTION ".
      EXPORT STREAM ddl as4dict.p__Index._Desc.
    END.
 /*  Dump AS400  */
    IF dump_format = "AS400" THEN DO:
       IF as4dict.p__Index._For-Type <> ? AND 
          as4dict.p__Index._For-Type <> "" THEN
            PUT STREAM ddl UNFORMATTED "  FORMAT-NAME " as4dict.p__Index._For-Type SKIP.
        IF as4dict.p__Index._As4-File <> ? AND 
          as4dict.p__Index._As4-File <> "" THEN
            PUT STREAM ddl UNFORMATTED "  AS400-FILE " as4dict.p__Index._As4-File SKIP.            
       IF as4dict.p__Index._I-misc2[4] <> ? AND 
          as4dict.p__index._I-misc2[4] <> "" THEN 
            PUT STREAM ddl UNFORMATTED "  AS400-FLAGS " as4dict.p__Index._I-misc2[4] SKIP.
    END.  /* End AS400 Format */  
  
 /*--------------------------------------------------------------------    
    IF _Index._Idx-num <> ? AND _Db._Db-type <> 'PROGRESS' THEN
      PUT STREAM ddl UNFORMATTED "  INDEX-NUM " _Index._Idx-num SKIP.
    IF _Index._For-Name <> ? THEN DO:
      PUT STREAM ddl CONTROL "  FOREIGN-NAME ".
      EXPORT STREAM ddl _Index._For-Name.
    END.
--------------------------------------------------------------------*/
    FOR EACH as4dict.p__Idxfd WHERE as4dict.p__idxfd._Idx-Num = as4dict.p__Index._Idx-Num
         AND as4dict.p__Idxfd._file-number = as4dict.p__Index._file-number
            NO-LOCK
       BY as4dict.p__Idxfd._Index-seq:    
        IF as4dict.p__Idxfd._If-misc2[8] = "Y" THEN NEXT.
        FIND as4dict.p__Field 
           WHERE as4dict.p__field._Fld-Number = as4dict.p__idxfd._Fld-Number 
           AND as4dict.p__field._file-Number = as4dict.p__idxfd._file-number NO-LOCK.

        PUT STREAM ddl UNFORMATTED
        "  INDEX-FIELD """ as4dict.p__Field._Field-Name """ "
        (IF as4dict.p__Idxfd._Ascending = "Y" THEN "ASCENDING " ELSE "")
        (IF as4dict.p__Idxfd._Ascending <> "Y" THEN "DESCENDING " ELSE "")
        (IF as4dict.p__Idxfd._Abbreviate = "Y" THEN "ABBREVIATED " ELSE "")
        (IF as4dict.p__Idxfd._Unsorted = "Y" THEN "UNSORTED " ELSE "") SKIP.
     
      END.  /* FOR EACH Idx FD  */
    PUT STREAM ddl UNFORMATTED SKIP(1).
  END.  /* FOR EACH INDEX */

  IF TERMINAL <> "" AND NOT User_Env[29] BEGINS "incr" THEN
     DISPLAY "" @ as4dict.p__Index._Index-name with frame working.
END.  /* FOR EACH TABLE */

RETURN.
