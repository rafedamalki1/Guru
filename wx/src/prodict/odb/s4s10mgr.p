/*   SRCSH is the source schema holder database for the SYBASE4 schema  */
/*   TRGSH is the target schema holder database for the SYB10 schema  */

DEFINE INPUT PARAMETER src-ldname AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER trg-ldname AS CHARACTER NO-UNDO.
DEFINE VARIABLE msg AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE STREAM strm.
DEFINE VARIABLE maxordsrc AS INTEGER NO-UNDO.
DEFINE VARIABLE maxordtrg AS INTEGER NO-UNDO.
DEFINE VARIABLE maxord    AS INTEGER NO-UNDO.
DEFINE VARIABLE minordtrg AS INTEGER NO-UNDO.
DEFINE BUFFER trgfilwrk FOR TRGSH._File. 

FORM
  SKIP (1)
  msg[1] FORMAT "X(25)" LABEL "     File" SKIP
  msg[2] FORMAT "X(25)" LABEL "    Field" SKIP
  msg[3] FORMAT "X(25)" LABEL "    Index" SKIP
  msg[4] FORMAT "X(25)" LABEL "Component" SKIP
  SKIP (1)
  WITH FRAME fmigr ATTR-SPACE OVERLAY SIDE-LABELS ROW 4 CENTERED 
  TITLE " Updating SYBASE10 Schema Holder".


OUTPUT STREAM strm TO s4s10mgr.log.
FIND SRCSH._Db WHERE SRCSH._Db._Db-name = src-ldname.

FIND TRGSH._Db WHERE TRGSH._Db._Db-name = trg-ldname.

PUT STREAM strm UNFORMATTED 
    "SYBASE to SYB10 Schema Migration     " TODAY SPACE (2) 
                     STRING(TIME, "HH:MM:SS") SKIP
    "   Source schema holder: " PDBNAME ("SRCSH") SKIP
    "   Source logical db   : " src-ldname        SKIP
    "   Target schema holder: " PDBNAME ("TRGSH") SKIP
    "   Target logical db   : " trg-ldname        SKIP
    SKIP (2).

IF TRGSH._Db._Db-xl-name <> SRCSH._Db._Db-xl-name THEN
    PUT STREAM strm UNFORMATTED 
    "Code page of source does not match code page of target " SKIP
    "     source code page = " SRCSH._Db._Db-xl-name 
    " target code page = " TRGSH._Db._Db-xl-name SKIP(2). 

FOR EACH SRCSH._File OF SRCSH._Db:

    IF SRCSH._File._File-name BEGINS "_" THEN LEAVE.
    IF SRCSH._File._Hidden = TRUE THEN NEXT.

    FIND TRGSH._File OF TRGSH._Db WHERE  
            TRGSH._File._For-Owner = SRCSH._File._For-Owner AND 
            TRGSH._File._For-Name  = SRCSH._File._For-Name
                          NO-ERROR.

    PUT STREAM strm UNFORMATTED "Processing file: Name= " 
            SRCSH._File._File-Name " For-owner= " SRCSH._File._For-Owner
           " For-name= " SRCSH._File._For-Name SKIP.

    IF NOT AVAILABLE TRGSH._File THEN DO:
        PUT STREAM strm UNFORMATTED "Target file not found: " 
               SKIP "     File= "
               SRCSH._File._File-Name " For-owner= " SRCSH._File._For-Owner
               " For-name= " SRCSH._File._For-Name SKIP.
        NEXT.
    END.

    /* Delete existing triggers on target */

    FOR EACH TRGSH._File-Trig OF TRGSH._File:
        DELETE TRGSH._File-Trig.
    END.

    /*   File Triggers   */
    FOR EACH SRCSH._File-Trig OF SRCSH._File:

        IF NOT CAN-FIND (TRGSH._File-Trig OF TRGSH._File WHERE
                TRGSH._File-Trig._Event = SRCSH._File-Trig._Event) THEN DO:
            CREATE TRGSH._File-Trig.
        END.
        IF TERMINAL <> "" AND NOT SESSION:BATCH-MODE THEN
            DISPLAY SRCSH._File._File-name @ msg[1]
                    "" @ msg[2]
                    "" @ msg[3]
                    SRCSH._File-Trig._Event @ msg[4]
                    WITH FRAME fmigr.
        ASSIGN
          TRGSH._File-Trig._File-Recid = RECID (TRGSH._File)
          TRGSH._File-Trig._Event      = SRCSH._File-Trig._Event
          TRGSH._File-Trig._Proc-Name  = SRCSH._File-Trig._Proc-Name
          TRGSH._File-Trig._Override   = SRCSH._File-Trig._Override.
    END. /* EACH SRCSH._File-Trig OF SRCSH._File */

    /*  Ensure that the ranges for order are not overlapping  */ 
    maxordtrg = 0.
    minordtrg = 0.
    FOR EACH TRGSH._Field OF TRGSH._File:
        maxordtrg = MAXIMUM (maxordtrg, TRGSH._Field._Order ).
        minordtrg = MINIMUM (minordtrg, TRGSH._Field._Order ).
    END.

    maxordsrc = 0.
    FOR EACH SRCSH._Field OF SRCSH._File:
        maxordsrc = MAXIMUM (maxordsrc, SRCSH._Field._Order ).
    END.

    IF minordtrg <= maxordsrc THEN DO:
        maxord = MAXIMUM (maxordtrg, maxordsrc).
        FOR EACH TRGSH._Field OF TRGSH._File:
            TRGSH._Field._Order = TRGSH._Field._Order + maxord.
        END.
    END.

    FOR EACH SRCSH._Field OF SRCSH._File:

        IF SRCSH._Field._For-type = "TIME" OR
           SRCSH._Field._For-type = "TIME4" THEN DO:
            PUT STREAM strm UNFORMATTED 
    "Time part of datetime field not represented in Sybase10 schema holder"
                   SKIP  "     File= "
                   SRCSH._File._File-Name " Field= " 
                   SRCSH._Field._Field-name " For-name= " 
                   SRCSH._Field._For-name SKIP.
           NEXT.
        END.

        FIND TRGSH._Field OF TRGSH._File WHERE
              TRGSH._Field._For-name = SRCSH._Field._For-name NO-ERROR.
        IF NOT AVAILABLE TRGSH._field THEN DO:
            PUT STREAM strm UNFORMATTED "Target field not found: " 
                   SKIP  "     File= "
                   SRCSH._File._File-Name " Field= " 
                   SRCSH._Field._Field-name " For-name= " 
                   SRCSH._Field._For-name SKIP.
            NEXT.
        END.
        
        IF TERMINAL <> "" AND NOT SESSION:BATCH-MODE THEN
            DISPLAY SRCSH._File._File-name @ msg[1]
                    SRCSH._Field._Field-name @ msg[2]
                    "" @ msg[3]
                    "" @ msg[4]
                    WITH FRAME fmigr.
        ASSIGN
            TRGSH._Field._Can-Read      = SRCSH._Field._Can-Read
            TRGSH._Field._Can-Write     = SRCSH._Field._Can-Write
            TRGSH._Field._Col-label     = SRCSH._Field._Col-label
            TRGSH._Field._Data-type     = SRCSH._Field._Data-type
            TRGSH._Field._Decimals      = SRCSH._Field._Decimals
            TRGSH._Field._Desc          = SRCSH._Field._Desc
            TRGSH._Field._Fld-case      = SRCSH._Field._Fld-case
            TRGSH._Field._Field-name    = SRCSH._Field._Field-name
            TRGSH._Field._Format        = SRCSH._Field._Format
            TRGSH._Field._Help          = SRCSH._Field._Help 
            TRGSH._Field._Initial       = SRCSH._Field._Initial
            TRGSH._Field._Mandatory     = SRCSH._Field._Mandatory
            TRGSH._Field._Label         = SRCSH._Field._Label
            TRGSH._Field._Order         = SRCSH._Field._Order
            TRGSH._Field._Valexp        = SRCSH._Field._Valexp
            TRGSH._Field._Valmsg        = SRCSH._Field._Valmsg
            TRGSH._Field._View-As       = SRCSH._Field._View-As

            TRGSH._Field._For-Id        = SRCSH._Field._For-Id
            TRGSH._Field._For-Primary   = SRCSH._Field._For-Primary
            TRGSH._Field._For-Spacing   = SRCSH._Field._For-Spacing 
            TRGSH._Field._For-Scale     = SRCSH._Field._For-Scale
            TRGSH._Field._For-Itype     = SRCSH._Field._For-Itype
            TRGSH._Field._For-Xpos      = SRCSH._Field._For-Xpos
            TRGSH._Field._For-Retrieve  = SRCSH._Field._For-Retrieve
            TRGSH._Field._For-Separator = SRCSH._Field._For-Separator
            TRGSH._Field._For-Maxsize   = SRCSH._Field._For-Maxsize
            TRGSH._Field._For-Allocated = SRCSH._Field._For-Allocated.

        /*  Delete existing triggers on fields  */
        FOR EACH TRGSH._Field-Trig OF TRGSH._Field:
            DELETE  TRGSH._Field-Trig.
        END.

        /*   Field Triggers   */
        FOR EACH SRCSH._Field-Trig OF SRCSH._Field:

            IF NOT CAN-FIND (TRGSH._Field-Trig OF TRGSH._Field WHERE
                  TRGSH._Field-Trig._Event = SRCSH._Field-Trig._Event) THEN DO:
                CREATE TRGSH._Field-Trig.
            END.

            IF TERMINAL <> "" AND NOT SESSION:BATCH-MODE THEN
                DISPLAY SRCSH._File._File-name @ msg[1]
                        SRCSH._Field._Field-name @ msg[2]
                        "" @ msg[3]
                        SRCSH._Field-Trig._Event @ msg[4]
                        WITH FRAME fmigr.
            ASSIGN
              TRGSH._Field-Trig._Field-Recid = RECID (TRGSH._Field)
              TRGSH._Field-Trig._File-Recid  = RECID (TRGSH._File)
              TRGSH._Field-Trig._Event       = SRCSH._Field-Trig._Event
              TRGSH._Field-Trig._Proc-Name   = SRCSH._Field-Trig._Proc-Name
              TRGSH._Field-Trig._Override    = SRCSH._Field-Trig._Override.
        END. /* FOR EACH SRCSH._Field-Trig OF SRCSH._Field */
    END. /* EACH SRCSH._Field OF SRCSH._File */

    FOR EACH SRCSH._Index OF SRCSH._File:
        IF TERMINAL <> "" AND NOT SESSION:BATCH-MODE THEN
            DISPLAY SRCSH._File._File-name @ msg[1]
                    "" @ msg[2]
                    SRCSH._Index._Index-Name @ msg[3]
                    "" @ msg[4]
                    WITH FRAME fmigr.
    
    END. /*  EACH SRCSH._Index OF SRCSH._File */

    IF TERMINAL <> "" AND NOT SESSION:BATCH-MODE THEN
        DISPLAY SRCSH._File._File-name @ msg[1]
                "" @ msg[2]
                "" @ msg[3]
                "" @ msg[4]
                WITH FRAME fmigr.
    ASSIGN
        TRGSH._File._File-Name  = SRCSH._File._File-Name
        TRGSH._File._Can-Create = SRCSH._File._Can-Creat
        TRGSH._File._Can-Read   = SRCSH._File._Can-Read
        TRGSH._File._Can-Write  = SRCSH._File._Can-Write
        TRGSH._File._Can-Delete = SRCSH._File._Can-Delete
        TRGSH._File._Desc       = SRCSH._File._Desc
        TRGSH._File._Valexp     = SRCSH._File._Valexp
        TRGSH._File._Valmsg     = SRCSH._File._Valmsg
        TRGSH._File._Frozen     = SRCSH._File._Frozen

        TRGSH._File._For-Size   = SRCSH._File._For-Size
        TRGSH._File._For-Flag   = SRCSH._File._For-Flag
        TRGSH._File._For-Cnt1   = SRCSH._File._For-Cnt1
        TRGSH._File._For-Cnt2   = SRCSH._File._For-Cnt2
        TRGSH._File._For-Format = SRCSH._File._For-Format
        TRGSH._File._For-Info   = SRCSH._File._For-Info
        TRGSH._File._For-Id     = SRCSH._File._For-Id
        TRGSH._File._For-Number = SRCSH._File._For-Number
        TRGSH._File._File-Label = SRCSH._File._File-Label
        TRGSH._File._Hidden     = SRCSH._File._Hidden.

    /*  Assign dump name.  Dump names have to be unique, so need to check 
               first if the one we want to assign is already used.
        If it is used, then set it to ? in it's _file record, assign the 
               name to the desired record, and generate a new dump name for 
               the first record. 
    */

    IF TRGSH._File._Dump-name <> SRCSH._File._Dump-name THEN DO:

        FIND FIRST trgfilwrk WHERE trgfilwrk._Dump-name = 
                       SRCSH._File._Dump-name NO-ERROR.
        IF AVAILABLE trgfilwrk THEN DO:
            trgfilwrk._Dump-name = ?.
            RELEASE trgfilwrk.

            TRGSH._File._Dump-name = SRCSH._File._Dump-name.

            FIND FIRST trgfilwrk WHERE trgfilwrk._Dump-name = ?
               AND NOT trgfilwrk._File-name BEGINS "_" NO-ERROR.
            IF AVAILABLE trgfilwrk THEN RUN "prodict/dump/_lodname.p".
        END.
        ELSE
            TRGSH._File._Dump-name = SRCSH._File._Dump-name.
    END.
    
    PUT STREAM strm UNFORMATTED " " SKIP.

END. /* EACH SRCSH._File OF SRCSH._Db */

OUTPUT STREAM strm CLOSE.

RETURN.







