DEFINE INPUT  PARAMETER pcSources    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcSourceKeys AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcFields     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcKeyValue   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet.

DEFINE VARIABLE iEntry      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCounter      AS INTEGER    NO-UNDO.
DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.



CREATE DATASET schaktProtDataSet.

CREATE TEMP-TABLE hdschakttempvisa.
hdschakttempvisa:CREATE-LIKE(hdschakttemp).

schaktProtDataSet:ADD-BUFFER(hdschakttempvisa:DEFAULT-BUFFER-HANDLE).

FOR EACH hdschakttemp:
  iCounter = iCounter + 1.
  CREATE TEMP-TABLE hTable.
  hTable:CREATE-LIKE(hdschakttemp).
  hTable:TEMP-TABLE-PREPARE("hdschakttempvisa" + STRING(iCounter)).
  schaktProtDataSet:ADD-BUFFER(hdschakprottempvisa:DEFAULT-BUFFER-HANDLE).
END.

schaktProtDataSet:ADD-BUFFER(hdschakprottempvisa:DEFAULT-BUFFER-HANDLE).

CREATE TEMP-TABLE hdprotkopbertempvisa.
hdprotkopbertempvisa:CREATE-LIKE(hdprotkopbertemp).

schaktProtDataSet:ADD-BUFFER(hdprotkopbertempvisa:DEFAULT-BUFFER-HANDLE).

CREATE TEMP-TABLE hdschakprothandtempvisa.
hdschakprothandtempvisa:CREATE-LIKE(hdschakprothandtemp).

schaktProtDataSet:ADD-BUFFER(hdschakprothandtempvisa:DEFAULT-BUFFER-HANDLE).

CREATE TEMP-TABLE hdkalktempvisa.
hdkalktempvisa:CREATE-LIKE(hdkalktemp).

schaktProtDataSet:ADD-BUFFER(hdkalktempvisa:DEFAULT-BUFFER-HANDLE).

CREATE TEMP-TABLE schkordstartsluttempvisa.
schkordstartsluttempvisa:CREATE-LIKE(schkordstartsluttemp).

schaktProtDataSet:ADD-BUFFER(schkordstartsluttempvisa:DEFAULT-BUFFER-HANDLE).

phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1), 
                       phDataSet:GET-BUFFER-HANDLE(2),
                       pcFields).

DO iEntry = 1 TO NUM-ENTRIES(pcSources):
  CREATE DATA-SOURCE hDataSource.
  CREATE BUFFER hBuffer FOR TABLE ENTRY(iEntry, pcSources).
  hDataSource:ADD-SOURCE-BUFFER(hBuffer, ENTRY(iEntry,pcSourceKeys)).
  phDataSet:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).

  IF iEntry = 1 THEN DO:
    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ENTRY(1, pcSources) +
                         " WHERE " + ENTRY(1, pcSourceKeys) + pcKeyValue).
    hDataSource:QUERY = hQuery.
  END.
END.
    
phDataSet:FILL().
DELETE OBJECT hQuery.

DO iEntry = 1 TO phDataSet:NUM-BUFFERS:
  hBuffer = phDataSet:GET-BUFFER-HANDLE(iEntry).
  MESSAGE "Data-Source: "
    hBuffer:DATA-SOURCE:GET-SOURCE-BUFFER:NAME SKIP
    "WHERE-STRING:" hBuffer:DATA-SOURCE:FILL-WHERE-STRING
    VIEW-AS ALERT-BOX.
  DELETE OBJECT hBuffer:DATA-SOURCE.
END.