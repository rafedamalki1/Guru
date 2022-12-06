/* clientChanges.p -- client side of generic commitChanges support */
DEFINE INPUT PARAMETER phDataSet AS HANDLE.
DEFINE INPUT PARAMETER phSupportProc AS HANDLE.
DEFINE OUTPUT PARAMETER pcStatus AS CHARACTER.

DEFINE VARIABLE hDSChanges AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE  NO-UNDO.
DEFINE VARIABLE iBuffer    AS INTEGER NO-UNDO.

CREATE DATASET hDSChanges.
hDSChanges:CREATE-LIKE(phDataSet).
hDSChanges:GET-CHANGES(phDataSet).
RUN saveChanges IN phSupportProc
  (INPUT DATASET-HANDLE hDSChanges BY-REFERENCE).
  /*anders*
  (INPUT-OUTPUT DATASET-HANDLE hDSChanges BY-REFERENCE).
  */ 
/* Check the ERROR status that might have been returned. */
IF hDSChanges:ERROR THEN
DO iBuffer = 1 TO phDataSet:NUM-BUFFERS:
  CREATE QUERY hQuery.
  hBuffer = hDSChanges:GET-BUFFER-HANDLE(iBuffer).
  hQuery:ADD-BUFFER(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  DO WHILE NOT hQuery:QUERY-OFF-END:
    IF hBuffer:ERROR THEN
      pcStatus = pcStatus + hBuffer:ERROR-STRING + CHR(10).
    hQuery:GET-NEXT().
  END.

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
END.

hDSChanges:MERGE-CHANGES(phDataSet).
DELETE OBJECT hDSChanges.
phDataSet:GET-BUFFER-HANDLE(1):SYNCHRONIZE().
