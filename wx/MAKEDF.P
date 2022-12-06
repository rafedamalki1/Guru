/* makedf.p                                                      */
/*                                                               */
/* This program will read the convmap.dat file and search for a  */
/* specified collation table.  Once it is found, this program    */
/* will create an _tran.df file suitable to load into a database */
/* with the dictionary function:                                 */
/*    Admin->                                                    */
/*       Load Data and Definitions ->                            */
/*          Data Definitions (.df file)...                       */
/* USAGE:                                                        */
/*    RUN prolang/makedf.p                                       */
/*                                                               */
/* It will then prompt for                                       */
/*  - the name of the convmap.dat file which will be searched    */
/*  - the name of the output file to be generated                */
/*  - the codepage-name which you want your database to contain  */
/*  - the collation-name which you want your database to contain */
/*                                                               */
/* After running this program, you should get an empty database  */
/* and load the .df file created by this program into it.        */
/*                                                               */
/* NOTE: this program is very sensitive to the exact format of   */
/* the convmap.dat file.  This program will not work properly if */
/* the convmap.dat file entrys do not have exactly the same      */
/* as those provided in the standard convmap.dat provided by     */
/* Progress Software Corporation                                 */

DEFINE STREAM outstrm.
DEFINE STREAM instrm.

DEFINE VAR i             AS INT                 NO-UNDO.
DEFINE VAR inline        AS CHAR FORMAT "x(80)" NO-UNDO.
DEFINE VAR codepage-name AS CHAR FORMAT "x(19)" NO-UNDO.
DEFINE VAR collate-name  AS CHAR FORMAT "x(19)" NO-UNDO.
DEFINE VAR entry-found   AS LOGICAL             NO-UNDO.
DEFINE VAR keyval        AS CHAR                NO-UNDO.
DEFINE VAR dataval       AS CHAR                NO-UNDO.
DEFINE VAR coll-vers     AS CHAR FORMAT "x(16)" NO-UNDO.

DEFINE VAR infile        AS CHAR FORMAT "x(70)" NO-UNDO INIT "convmap.dat".
DEFINE VAR outfile       AS CHAR FORMAT "x(70)" NO-UNDO INIT "_tran.df".

/* select a good default for infile */
infile = OS-GETENV("DLC") + "/prolang/convmap/convmap.dat".


/* prompt the user for the info needed to select the correct collation table */
REPEAT ON ERROR UNDO, RETRY:
    UPDATE "convmap.dat filename :"          infile        SKIP
           "Output .df filename  :"          outfile       SKIP
           "Desired database codepage name:" codepage-name SKIP
           "Desired collation name:"         collate-name
        WITH FRAME f1 CENTERED ROW 3 NO-LABELS.

    INPUT STREAM instrm CLOSE.
    INPUT STREAM instrm FROM VALUE(infile).
    OUTPUT STREAM outstrm CLOSE.
    OUTPUT STREAM outstrm TO VALUE(outfile).

    STATUS DEFAULT "Searching for entry...".

    /* search the input file for 
                COLLATION
                CODEPAGE-NAME  <desired name>
                COLLATION-NAME <desired name>
    */
    entry-found = no.
search-collation:
    REPEAT:
        IMPORT STREAM instrm UNFORMATTED inline.
        IF inline = "COLLATION"
        THEN REPEAT:
            /* a COLLATION entry was found,  check if it is the right one */
            IMPORT STREAM instrm keyval dataval.
            IF    (keyval = "CODEPAGE-NAME" and dataval <> codepage-name)
               OR (keyval = "COLLATION-NAME" and dataval <> collate-name)
            THEN NEXT search-collation.  /* wrong entry, skip it */
            IF keyval = "COLLATION-TRANSLATION-VERSION"
            THEN coll-vers = dataval.
            IF keyval = "CASE-INSENSITIVE-SORT"
            THEN DO:
                entry-found = yes.
                LEAVE search-collation.
            END.
        END.
    END.
    IF NOT entry-found
    THEN DO:
        Message "The desired entry was not found in" infile.
        UNDO, RETRY.
    END.

    STATUS DEFAULT "".

    PUT STREAM outstrm "UPDATE DATABASE ~"?~""                    SKIP.
    PUT STREAM outstrm "COLLATION-TRANSLATION-VERSION " coll-vers SKIP.
    PUT STREAM outstrm "CODEPAGE-NAME  "   codepage-name          SKIP.
    PUT STREAM outstrm "COLLATION-NAME "   collate-name           SKIP.
    PUT STREAM outstrm "INTERNAL-EXTERNAL-TRAN-TABLE"             SKIP.
    PUT STREAM outstrm "?"                                        SKIP.
    PUT STREAM outstrm "EXTERNAL-INTERNAL-TRAN-TABLE"             SKIP.
    PUT STREAM outstrm "?"                                        SKIP.

    /* Copy the CASE-INSENSITIVE-SORT table */
    PUT STREAM outstrm "CASE-INSENSITIVE-SORT"                SKIP.
    DO i = 1 to 16:
        IMPORT STREAM instrm UNFORMATTED inline.
        PUT STREAM outstrm inline FORMAT "x(80)" SKIP.
    END.

    /* skip forwards to the CASE-SENSITIVE-SORT table and copy it */
    DO WHILE inline <> "CASE-SENSITIVE-SORT":
        IMPORT STREAM instrm UNFORMATTED inline.
    END.
    PUT STREAM outstrm "CASE-SENSITIVE-SORT"                  SKIP.
    DO i = 1 to 16:
        IMPORT STREAM instrm UNFORMATTED inline.
        PUT STREAM outstrm inline FORMAT "x(80)" SKIP.
    END.

    PUT STREAM outstrm "UPPERCASE-MAP"                            SKIP.
    PUT STREAM outstrm "?"                                        SKIP.
    PUT STREAM outstrm "LOWERCASE-MAP"                            SKIP.
    PUT STREAM outstrm "?"                                        SKIP.
    PUT STREAM outstrm "."                                        SKIP.
    PUT STREAM outstrm "PSC"                                      SKIP.
    PUT STREAM outstrm "codepage=". 
    PUT STREAM outstrm codepage-name                              SKIP.
    PUT STREAM outstrm "."                                        SKIP.

    OUTPUT STREAM outstrm CLOSE.
    INPUT  STREAM instrm  CLOSE.
END.
