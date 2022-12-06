/* CustomerDynamicDataSet.p
 */
 

PROCEDURE DefAndLoadDs_UI :
   DEFINE INPUT PARAMETER antaltab AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER pcBuffers    AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE INPUT PARAMETER pcRelFields  AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE INPUT PARAMETER pcRelTabels  AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE INPUT PARAMETER pcSources    AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE INPUT PARAMETER pcSourceKeys AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE INPUT PARAMETER pcKeyValue   AS CHARACTER NO-UNDO EXTENT 20.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet.
   DEFINE VARIABLE addsrc AS LOGICAL NO-UNDO.
   DEFINE VARIABLE reltab1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE reltab2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE iEntry      AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.
   
   CREATE DATASET phDataSet.
   
   DO iEntry = 1 TO antaltab:
      phDataSet:ADD-BUFFER(WIDGET-HANDLE(pcBuffers[iEntry])).
   END.
   
   DO iEntry = 1 TO antaltab - 1:
      IF pcRelFields[iEntry] = "" THEN.
      ELSE DO:
         IF pcRelTabels[iEntry] = "" THEN DO:
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1),phDataSet:GET-BUFFER-HANDLE(iEntry + 1), pcRelFields[iEntry]).
         END.
         ELSE DO:   
            reltab1 = INTEGER(ENTRY(1,pcRelTabels[iEntry])).
            reltab2 = INTEGER(ENTRY(2,pcRelTabels[iEntry])).
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(reltab1),phDataSet:GET-BUFFER-HANDLE(reltab2), pcRelFields[iEntry]).
         END.
      END.      
   END.

  
   DO iEntry = 1 TO antaltab:
      CREATE DATA-SOURCE hDataSource.
      CREATE BUFFER hBuffer FOR TABLE pcSources[iEntry].
      addsrc = hDataSource:ADD-SOURCE-BUFFER(hBuffer,pcSourceKeys[iEntry]).
      phDataSet:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).
      IF iEntry = 1 THEN DO:
         CREATE QUERY hQuery.
         hQuery:ADD-BUFFER(hBuffer).
         hQuery:QUERY-PREPARE("FOR EACH " + pcSources[1] + " WHERE " + pcKeyValue[1]).
         hDataSource:QUERY = hQuery.
      END. /* DO IF iEntry = 1 */
   END. /* DO iEntry = 1 TO NUM-ENTRIES */
   phDataSet:FILL().
   
   DELETE OBJECT hQuery.
   
   DO iEntry = 1 TO phDataSet:NUM-BUFFERS:
      hBuffer = phDataSet:GET-BUFFER-HANDLE(iEntry).
      DELETE OBJECT hBuffer:DATA-SOURCE.
   END.
   
END PROCEDURE.


