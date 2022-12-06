/*xdynexport.p

With the help of the peg and one (still) undocumented feature Tony was the first one to figure out a way 
to create a 100% Data Administration compatible 100% Dynamic Export program in Progress v9.1.

The basic tricks are the use of the following:

- EXPORT hField:BUFFER-VALUE which exports a field of any data on a line at the time in Progress export format 
- Use the SEEK function to remove the unwanted NEW LINE character at the end. 
- Use the still undocumented DISABLE-DUMP-TRIGGERS() function 

Since Tony worked on the original v6 Data Administration tools you can be pretty sure that the result is compatible.
*/

DEFINE VARIABLE cTables   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDatabase AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTable    AS INTEGER   NO-UNDO.

/* scan all databases for tables to dump */
DO iDatabase = NUM-DBS TO 1 BY -1:
   IF DBTYPE(iDatabase) <> "PROGRESS":u THEN NEXT.
   RUN Schema (LDBNAME(iDatabase),OUTPUT cTables).
   DO iTable = NUM-ENTRIES(cTables,"~n":u) TO 1 BY -1:
     RUN Dump (
       ENTRY(1,ENTRY(iTable,cTables,"~n":u)),
       ENTRY(2,ENTRY(iTable,cTables,"~n":u))
     ).
   END.
END.

QUIT.


/* scan all tables in this database */
PROCEDURE Schema:
   DEFINE INPUT  PARAMETER ip_cDatabase AS CHARACTER            NO-UNDO.
   DEFINE OUTPUT PARAMETER op_cTables   AS CHARACTER INITIAL "" NO-UNDO.

   DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

   DEFINE VARIABLE hName   AS HANDLE NO-UNDO.
   DEFINE VARIABLE hDump   AS HANDLE NO-UNDO.

   CREATE QUERY hQuery.
   CREATE BUFFER hBuffer FOR TABLE ip_cDatabase + '._File':u.

   hQuery:SET-BUFFERS(hBuffer).
   hQuery:QUERY-PREPARE(SUBSTITUTE('FOR EACH &1._File WHERE &1._File._File-number > 0 
                                    AND &1._File._File-number < 32768 NO-LOCK':u,ip_cDatabase)).
   hQuery:QUERY-OPEN().
   hQuery:GET-FIRST().

   DO WHILE hBuffer:AVAILABLE:
     hName = hBuffer:BUFFER-FIELD('_File-name':u).
     hDump = hBuffer:BUFFER-FIELD('_Dump-name':u).
     op_cTables = (IF op_cTables = "" THEN "" ELSE op_cTables + "~n":u)
                + ip_cDatabase + '.':u
                + hName:BUFFER-VALUE + ',':u
                + hDump:BUFFER-VALUE + '.d':u.
     hQuery:GET-NEXT().
   END.

   hQuery:QUERY-CLOSE().
   DELETE OBJECT hQuery.
   DELETE OBJECT hBuffer.
END PROCEDURE.


/* dump the passed table */
PROCEDURE Dump:
   DEFINE INPUT PARAMETER ip_cTable AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip_cFile  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE hField  AS HANDLE NO-UNDO.

   DEFINE VARIABLE lFirst   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE iField   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iNumber  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iExtent  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iBack    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iTrailer AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cTimeStamp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nRecords   AS DECIMAL DECIMALS 0 INITIAL 0 NO-UNDO.

   CREATE QUERY hQuery.
   CREATE BUFFER hBuffer FOR TABLE ip_cTable.

/*    hBuffer:DISABLE-DUMP-TRIGGERS(). /*optional*/ */
   hQuery:SET-BUFFERS(hBuffer).
   hQuery:QUERY-PREPARE('FOR EACH ':u + ip_cTable + ' NO-LOCK':u).
   hQuery:QUERY-OPEN().
   hQuery:GET-FIRST().
   ASSIGN
       iNumber    = hBuffer:NUM-FIELDS
       cTimeStamp = STRING(YEAR( TODAY),"9999":u) + "/":u
                  + STRING(MONTH(TODAY),"99":u  ) + "/":u
                  + STRING(DAY(  TODAY),"99":u  ) + "-":u
                  + STRING(TIME,"HH:MM:SS":u).

   OUTPUT TO VALUE(ip_cFile) NO-ECHO NO-MAP.
   EXPORT ?.
   iBack = SEEK(OUTPUT) - 1.
   SEEK OUTPUT TO 0.
   DO WHILE hBuffer:AVAILABLE:
      ASSIGN
        nRecords = nRecords + 1.
        lFirst   = TRUE.
      DO iField = 1 TO iNumber:
        hField = hBuffer:BUFFER-FIELD(iField).
        IF hField:DATA-TYPE = "recid":u THEN NEXT.
        IF lFirst THEN
          lFirst = FALSE.
        ELSE DO:
          SEEK OUTPUT TO SEEK(OUTPUT) - iBack.
          PUT CONTROL ' ':u.
        END.
        IF hField:EXTENT > 1 THEN DO iExtent = 1 TO hField:EXTENT:
          IF iExtent > 1 THEN DO:
            SEEK OUTPUT TO SEEK(OUTPUT) - iBack.
            PUT CONTROL ' ':u.
          END.
          EXPORT hField:BUFFER-VALUE(iExtent).
        END.
        ELSE
          EXPORT hField:BUFFER-VALUE.
      END.
      hQuery:GET-NEXT().
   END.

   PUT UNFORMATTED ".":u SKIP.
   iTrailer = SEEK(OUTPUT).
   PUT UNFORMATTED
       "PSC":u SKIP
       "filename=":u hBuffer:TABLE SKIP
       "records=":u  STRING(nRecords,"9999999999999":u) SKIP
       "ldbname=":u  hBuffer:DBNAME SKIP
       "timestamp=":u cTimeStamp SKIP
       "numformat=":u ASC(SESSION:NUMERIC-SEPARATOR) ",":u ASC(SESSION:NUMERIC-DECIMAL-POINT) SKIP
       "dateformat=":u SESSION:DATE-FORMAT "-":u SESSION:YEAR-OFFSET SKIP
       "map=NO-MAP":u SKIP
       "cpstream=":u SESSION:CPSTREAM SKIP
       ".":u SKIP
       STRING(iTrailer,"9999999999":u) SKIP.

   OUTPUT CLOSE.
   hQuery:QUERY-CLOSE().
   DELETE OBJECT hQuery.
   DELETE OBJECT hBuffer.
END PROCEDURE.

