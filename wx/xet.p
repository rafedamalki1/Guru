



/* Sample code against Sports2000 */
DEFINE VARIABLE cBaseQuery      AS CHARACTER NO-UNDO INITIAL "FOR EACH extradatatemp NO-LOCK".
DEFINE VARIABLE hBrowse         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSalesRepBuffer AS HANDLE    NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER    NO-UNDO.
DEFINE VARIABLE hTest AS HANDLE     NO-UNDO.

FORM WITH FRAME X WIDTH 100 TITLE "BROWSE" 20 DOWN.

CREATE BUFFER hSalesRepBuffer FOR TABLE "extradatatemp".

CREATE QUERY hQuery.

hQuery:SET-BUFFERS(hSalesRepBuffer).
hQuery:QUERY-PREPARE(cBaseQuery).
hQuery:QUERY-OPEN.

CREATE BROWSE hBrowse
    ASSIGN X         = 5
           Y         = 10
           WIDTH     = 80
           DOWN      = 10
           QUERY     = hQuery
           FRAME     = FRAME X:HANDLE
           READ-ONLY = FALSE
           SENSITIVE = TRUE
           FONT      = 2.
DEFINE TEMP-TABLE extradatatemp NO-UNDO
   FIELD SOKCHAR  AS CHARACTER EXTENT 10.
CREATE extradatatemp.
   ASSIGN
   extradatatemp.SOKCHAR[1] = "WWW"
   extradatatemp.SOKCHAR[2] = "DDD".

FIND FIRST extradatatemp.
/*
dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   IF descvarcol = FALSE THEN DO:
      IF colindex = ? THEN DO:
         kommandosortquery = "FOR EACH " + dynbuffh:TABLE
         */
DO iLoop = 1 TO EXTENT(SOKCHAR) :

                hTest = hBrowse:ADD-LIKE-COLUMN(
                 SUBSTITUTE("extradatatemp.SOKCHAR[&1]", STRING(iloop))).
                MESSAGE hTest:NAME SKIP hTest:LABEL  EXTENT(SOKCHAR)
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
                                                                    
VIEW FRAME X.

WAIT-FOR CLOSE OF THIS-PROCEDURE.

DELETE OBJECT hBrowse.
DELETE OBJECT hQuery.
DELETE OBJECT hSalesRepBuffer.



