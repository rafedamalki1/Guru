/* DynamicDataSet.p -- creates a dynamic DataSet and Data-Sources, fills it for 
   a key value passed in, and returns it. 
 RUN DefAndLoadDs_UI IN dyndamicDSh (INPUT STRING(anvegenTTh) + "," + STRING(kalknumanvegensubTEMPH), 
 eller
 RUN DefAndLoadDs_UI IN dyndamicDSh (INPUT STRING(BUFFER kalknumanvegenTT:HANDLE) + "," + STRING(BUFFER kalknumanvegensubtt:HANDLE), 
*/


DEFINE VARIABLE phDataSet AS HANDLE NO-UNDO.
DEFINE VAR DynWp AS CHARACTER NO-UNDO.
DynWp = "DYNDS" + STRING(TIME).
CREATE WIDGET-POOL STRING(DynWp) NO-ERROR.
CREATE DATASET phDataSet IN WIDGET-POOL STRING(DynWp).
{DataSetVariable.I}
DEFINE VARIABLE ChDataSet AS HANDLE NO-UNDO.
/*Anders Olsson Elpool i Umeå AB  8 nov 2017 11:21:55 
Laddar och skapar ds 
*/
PROCEDURE DefAndLoadDs_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR DatasetDeftt.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet.
   
   DEFINE VARIABLE addsrc AS LOGICAL NO-UNDO.
   DEFINE VARIABLE reltab1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE reltab2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE iEntry      AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuerykommando AS CHARACTER NO-UNDO.
  
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.

   CREATE DATASET phDataSet IN WIDGET-POOL STRING(DynWp).
   
   phDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFill", THIS-PROCEDURE).
   DO iEntry = 1 TO antaltab:
      phDataSet:ADD-BUFFER(WIDGET-HANDLE(DatasetDeftt.pcBuffers[iEntry])).
   END.
   
   DO iEntry = 1 TO DatasetDeftt.antaltab - 1:
      IF DatasetDeftt.pcRelFields[iEntry] = "" THEN.
      ELSE DO:
         IF DatasetDeftt.pcRelTables[iEntry] = "" THEN DO:
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1),phDataSet:GET-BUFFER-HANDLE(iEntry + 1), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
         ELSE DO:   
            /*om inte tabelen är rellaterad till den första utan någon annan*/
            reltab1 = INTEGER(ENTRY(1,DatasetDeftt.pcRelTables[iEntry])).
            reltab2 = INTEGER(ENTRY(2,DatasetDeftt.pcRelTables[iEntry])).
            /*
            MESSAGE DatasetDeftt.pcRelTables[iEntry] phDataSet:GET-BUFFER-HANDLE(reltab1):TABLE  phDataSet:GET-BUFFER-HANDLE(reltab2):TABLE
            DatasetDeftt.pcRelFields[iEntry]  iEntry
            VIEW-AS ALERT-BOX.
            */
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(reltab1),phDataSet:GET-BUFFER-HANDLE(reltab2), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
      END.      
   END.
   DO iEntry = 1 TO DatasetDeftt.antaltab:
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL STRING(DynWp).
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iEntry] IN WIDGET-POOL STRING(DynWp).
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iEntry]).
     
      phDataSet:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).
      
      IF iEntry = 1 THEN DO:
         CREATE QUERY hQuery IN WIDGET-POOL STRING(DynWp).
         hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
         IF DatasetDeftt.pcKeyValue[1] = ? THEN. 
         ELSE DO: 
            hQuery:QUERY-PREPARE("FOR EACH " + DatasetDeftt.pcSources[1] + " WHERE " + DatasetDeftt.pcKeyValue[1]) .
            hDataSource:QUERY = hQuery NO-ERROR.
         END.   
      END.
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  10 mar 2017 14:02:51 
         denna ska bara används då det inte ska finnas realationer mellan tabellerna
         DatasetDeftt.pcRelTables[1] osv samt DatasetDeftt.pcRelFields[1] ska inte vara blanka.
         se EXIMSTORDStest.P och EXIMSTORDS.P
        */
        /*
         IF DatasetDeftt.pcRelTables[iEntry] = "" THEN.
         */
         
         IF DatasetDeftt.pcKeyValue[iEntry] = "" THEN.
         ELSE IF DatasetDeftt.pcKeyValue[iEntry] = ? THEN.
         ELSE IF DatasetDeftt.pcSources[iEntry] NE "" THEN DO:
            CREATE QUERY hQuery IN WIDGET-POOL STRING(DynWp).
            hQuery:ADD-BUFFER(hBuffer).
            hQuery:QUERY-PREPARE("FOR EACH " + DatasetDeftt.pcSources[iEntry] + " WHERE " + DatasetDeftt.pcKeyValue[iEntry]) NO-ERROR.
            hDataSource:QUERY = hQuery NO-ERROR.
         END.
         
      END.   
       /* DO IF iEntry = 1 */
   END. /* DO iEntry = 1 TO NUM-ENTRIES */
   phDataSet:FILL().
   hQuery:QUERY-CLOSE().
   DELETE OBJECT hQuery NO-ERROR.
   hQuery = ?.
   
   DO iEntry = 1 TO phDataSet:NUM-BUFFERS:
      hBuffer = phDataSet:GET-BUFFER-HANDLE(iEntry) NO-ERROR.
      DELETE OBJECT hBuffer:DATA-SOURCE NO-ERROR.
   END.
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
   
END PROCEDURE.
/*om man måste reload*/
PROCEDURE DefAndLoadDsRe_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR DatasetDeftt.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSetOUT.
   DEFINE VARIABLE iEntry      AS INTEGER NO-UNDO.  
   DEFINE VARIABLE reltab1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE reltab2 AS INTEGER NO-UNDO.
  
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   /*
   phDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFill", THIS-PROCEDURE).
   */
   DO iEntry = 1 TO antaltab:
      phDataSet:ADD-BUFFER(WIDGET-HANDLE(DatasetDeftt.pcBuffers[iEntry])).
   END.
   
   DO iEntry = 1 TO DatasetDeftt.antaltab - 1:
      IF DatasetDeftt.pcRelFields[iEntry] = "" THEN.
      ELSE DO:
         IF DatasetDeftt.pcRelTables[iEntry] = "" THEN DO:
            /*
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1),phDataSet:GET-BUFFER-HANDLE(iEntry + 1), DatasetDeftt.pcRelFields[iEntry]).
            */
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1),phDataSet:GET-BUFFER-HANDLE(iEntry + 1), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
         ELSE DO:   
            /*om inte tabelen är rellaterad till den första utan någon annan*/
            reltab1 = INTEGER(ENTRY(1,DatasetDeftt.pcRelTables[iEntry])).
            reltab2 = INTEGER(ENTRY(2,DatasetDeftt.pcRelTables[iEntry])).
            /*
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(reltab1),phDataSet:GET-BUFFER-HANDLE(reltab2), DatasetDeftt.pcRelFields[iEntry]).
            */
            
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(reltab1),phDataSet:GET-BUFFER-HANDLE(reltab2), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
      END.      
   END.
   
   RUN load_UI (INPUT-OUTPUT DATASET-HANDLE phDataSet).
   phDataSetOUT = phDataSet.
   
END PROCEDURE.
/*spara ett tomt ds*/
PROCEDURE JustDefAndDs_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR DatasetDeftt.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet.
   
   DEFINE VARIABLE addsrc AS LOGICAL NO-UNDO.
   DEFINE VARIABLE reltab1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE reltab2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE iEntry      AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuerykommando AS CHARACTER NO-UNDO.
 
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.

   CREATE DATASET phDataSet IN WIDGET-POOL STRING(DynWp).
   
   
   DO iEntry = 1 TO antaltab:
      phDataSet:ADD-BUFFER(WIDGET-HANDLE(DatasetDeftt.pcBuffers[iEntry])).
   END.
   
   DO iEntry = 1 TO DatasetDeftt.antaltab - 1:
      IF DatasetDeftt.pcRelFields[iEntry] = "" THEN.
      ELSE DO:
         IF DatasetDeftt.pcRelTables[iEntry] = "" THEN DO:
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1),phDataSet:GET-BUFFER-HANDLE(iEntry + 1), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
         ELSE DO:   
            /*om inte tabelen är rellaterad till den första utan någon annan*/
            reltab1 = INTEGER(ENTRY(1,DatasetDeftt.pcRelTables[iEntry])).
            reltab2 = INTEGER(ENTRY(2,DatasetDeftt.pcRelTables[iEntry])).
            /*
            MESSAGE DatasetDeftt.pcRelTables[iEntry] phDataSet:GET-BUFFER-HANDLE(reltab1):TABLE  phDataSet:GET-BUFFER-HANDLE(reltab2):TABLE
            DatasetDeftt.pcRelFields[iEntry]  iEntry
            VIEW-AS ALERT-BOX.
            */
            phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(reltab1),phDataSet:GET-BUFFER-HANDLE(reltab2), DatasetDeftt.pcRelFields[iEntry],DatasetDeftt.repMode,DatasetDeftt.nestMode).
         END.
      END.      
   END.
  
   DO iEntry = 1 TO DatasetDeftt.antaltab:
      
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL STRING(DynWp).
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iEntry] IN WIDGET-POOL STRING(DynWp).
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iEntry]).
      phDataSet:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).
      
       /* DO IF iEntry = 1 */
   END. /* DO iEntry = 1 TO NUM-ENTRIES */
   
   DO iEntry = 1 TO phDataSet:NUM-BUFFERS:
      
      hBuffer = phDataSet:GET-BUFFER-HANDLE(iEntry) NO-ERROR.
      DELETE OBJECT hBuffer:DATA-SOURCE NO-ERROR.
   END.
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
   
END PROCEDURE.



PROCEDURE load_UI :
   DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSetoUT.
   DEFINE VARIABLE addsrc AS LOGICAL NO-UNDO.
   DEFINE VARIABLE iEntry      AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hQuerykommando AS CHARACTER NO-UNDO.

   phDataSetoUT:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFill", THIS-PROCEDURE).
   DO iEntry = 1 TO DatasetDeftt.antaltab:
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL STRING(DynWp).
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iEntry] IN WIDGET-POOL STRING(DynWp).
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iEntry]).
      phDataSetoUT:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).
     
      IF iEntry = 1 THEN DO:
         CREATE QUERY hQuery IN WIDGET-POOL STRING(DynWp).
         hQuery:ADD-BUFFER(hBuffer).
         hQuery:QUERY-PREPARE("FOR EACH " + DatasetDeftt.pcSources[1] + " WHERE " + DatasetDeftt.pcKeyValue[1]).
         hDataSource:QUERY = hQuery.
      END.
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  10 mar 2017 14:02:51 
         denna ska bara används då det inte ska finnas realationer mellan tabellerna
         DatasetDeftt.pcRelTables[1] osv samt DatasetDeftt.pcRelFields[1] ska inte vara blanka.
         se EXIMSTORDStest.P och EXIMSTORDS.P
        */
        /*
         IF DatasetDeftt.pcRelTables[iEntry] = "" THEN.
         */
         
         IF DatasetDeftt.pcKeyValue[iEntry] = "" THEN.
         ELSE IF DatasetDeftt.pcKeyValue[iEntry] = ? THEN.
         ELSE IF DatasetDeftt.pcSources[iEntry] NE "" THEN DO:
            CREATE QUERY hQuery IN WIDGET-POOL STRING(DynWp).
            hQuery:ADD-BUFFER(hBuffer).
            hQuery:QUERY-PREPARE("FOR EACH " + DatasetDeftt.pcSources[iEntry] + " WHERE " + DatasetDeftt.pcKeyValue[iEntry]).
            hDataSource:QUERY = hQuery.
         END.
         
      END.   
      /* DO IF iEntry = 1 */
   END. /* DO iEntry = 1 TO NUM-ENTRIES */
   phDataSetoUT:FILL().
   hQuery:QUERY-CLOSE().
   DELETE OBJECT hQuery NO-ERROR.
   hQuery = ?.
   
   DO iEntry = 1 TO phDataSetoUT:NUM-BUFFERS:
      hBuffer = phDataSetoUT:GET-BUFFER-HANDLE(iEntry).
      DELETE OBJECT hBuffer:DATA-SOURCE NO-ERROR.
   END.
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
END PROCEDURE.
PROCEDURE postDataSetFill :
   DEFINE INPUT PARAMETER DATASET-HANDLE phDataSetin.
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynfalth AS HANDLE NO-UNDO.
   DO iBuff = 1 TO phDataSetin:NUM-BUFFERS:
      dynbuffh = phDataSetin:GET-BUFFER-HANDLE(iBuff).
      CREATE QUERY dynqueh IN WIDGET-POOL STRING(DynWp).
      dynqueh:SET-BUFFERS(dynbuffh).
      kommandosortquery = "FOR EACH " + dynbuffh:TABLE + ".".  
     
      IF DatasetDeftt.pcKeyValue[iBuff] NE "" THEN DO:
         kommandosortquery =  "FOR EACH " + dynbuffh:TABLE + " WHERE " + DatasetDeftt.pcKeyValue[iBuff] + ".".
         kommandosortquery = REPLACE(kommandosortquery,DatasetDeftt.pcSources[iBuff] + ".",dynbuffh:TABLE + "."). 
      END.   
     
      dynok = dynqueh:QUERY-PREPARE(kommandosortquery) NO-ERROR.
      dynok = dynqueh:QUERY-OPEN() NO-ERROR.
      REPEAT:
         dynqueh:GET-NEXT(NO-LOCK) NO-ERROR.
         IF dynqueh:QUERY-OFF-END THEN LEAVE.            
         dynfalth = dynbuffh:BUFFER-FIELD("TTRECID") NO-ERROR.
         IF dynfalth = ? THEN.
         ELSE DO:
            IF dynbuffh:AVAILABLE THEN dynfalth:BUFFER-VALUE = dynbuffh:RECID.
            ELSE LEAVE.
         END.  
      END.
     
      IF DatasetDeftt.pcKeyValue[iBuff] NE "" THEN DO:
         dynfalth = dynbuffh:BUFFER-FIELD("TTRECID") NO-ERROR.
         IF dynfalth = ? THEN.
         ELSE DO:
            kommandosortquery =  "FOR EACH " + dynbuffh:TABLE + " WHERE " + "TTRECID = ? "  + ".".
            dynok = dynqueh:QUERY-PREPARE(kommandosortquery) NO-ERROR.
            dynok = dynqueh:QUERY-OPEN() NO-ERROR.
            dynqueh:GET-FIRST (NO-LOCK) NO-ERROR.
            DO WHILE dynqueh:QUERY-OFF-END = FALSE:
               dynbuffh:BUFFER-DELETE() NO-ERROR. 
               dynqueh:GET-NEXT(NO-LOCK) NO-ERROR.
            END.
         END.   
      END.
             
   END.
   DELETE OBJECT dynfalth NO-ERROR.
   dynfalth = ?.
   DELETE OBJECT dynbuffh NO-ERROR.
   dynbuffh = ?.
   dynqueh:QUERY-CLOSE().
   DELETE OBJECT dynqueh NO-ERROR.
   dynqueh = ?.
END PROCEDURE.
PROCEDURE GetDataSource_UI :
   DEFINE INPUT  PARAMETER DATASET-HANDLE ChDataSetin.
   DEFINE INPUT  PARAMETER buffnum AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER SourName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE GetSourh AS HANDLE NO-UNDO.
   GetSourh = ChDataSetin:GET-BUFFER-HANDLE(buffnum).
   SourName = GetSourh:DATA-SOURCE:GET-SOURCE-BUFFER:NAME.
   DELETE OBJECT GetSourh:DATA-SOURCE.
   GetSourh = ?.
  
END PROCEDURE.
PROCEDURE SaveDs_UI :
   DEFINE INPUT  PARAMETER DATASET-HANDLE ChDataSetin.
   DEFINE VARIABLE hTopBuff    AS HANDLE  NO-UNDO.
   DEFINE VARIABLE iBuff       AS INTEGER NO-UNDO.
   ChDataSet = ChDataSetin.
   DO iBuff = 1 TO ChDataSet:NUM-TOP-BUFFERS:
      hTopBuff = ChDataSet:GET-TOP-BUFFER(iBuff).
      IF hTopBuff:PARENT-RELATION NE ? THEN NEXT. 
      RUN traverseBuffers (hTopBuff).
   END.  
   DELETE OBJECT ChDataSet NO-ERROR.
   ChDataSet = ?.
END PROCEDURE.
PROCEDURE traverseBuffers:
  DEFINE INPUT PARAMETER phBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE iChildRel AS INTEGER NO-UNDO.
  RUN saveBuffer(phBuffer).
  DO iChildRel = 1 TO phBuffer:NUM-CHILD-RELATIONS:
    RUN traverseBuffers (phBuffer:GET-CHILD-RELATION(iChildRel):CHILD-BUFFER).
  END. /* END DO iChildRel */
END PROCEDURE. /* traverseBuffers */

PROCEDURE saveBuffer:
   DEFINE INPUT PARAMETER phBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE hBeforeBuff AS HANDLE    NO-UNDO. /*before buffer*/
   DEFINE VARIABLE hBeforeQry  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE cLogicProc  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iErrors AS INTEGER NO-UNDO.
      
   hBeforeBuff = phBuffer:BEFORE-BUFFER.
   IF VALID-HANDLE(hBeforeBuff) THEN DO:
      CREATE QUERY hBeforeQry IN WIDGET-POOL STRING(DynWp).
      hBeforeQry:ADD-BUFFER(hBeforeBuff).
      hBeforeQry:QUERY-PREPARE("FOR EACH " + hBeforeBuff:NAME).
      hBeforeQry:QUERY-OPEN().
      hBeforeQry:GET-FIRST().
      
      DO WHILE NOT hBeforeQry:QUERY-OFF-END:
         DO TRANSACTION :
            cLogicProc = phBuffer:TABLE-HANDLE:NAME +
            IF hBeforeBuff:ROW-STATE = ROW-DELETED THEN "Delete"
            ELSE IF hBeforeBuff:ROW-STATE = ROW-CREATED THEN "Create"
            ELSE "Modify".
        /*
           MESSAGE cLogicProc
           VIEW-AS ALERT-BOX.
         */
            /*   ORGINAL*/
            IF hBeforeBuff:AFTER-ROWID = ? THEN DO:
               phBuffer:FIND-BY-ROWID(hBeforeBuff:ROWID, EXCLUSIVE-LOCK) NO-ERROR.
               hBeforeBuff:SAVE-ROW-CHANGES() .
            END.   
            ELSE DO:
               phBuffer:FIND-BY-ROWID(hBeforeBuff:AFTER-ROWID, EXCLUSIVE-LOCK) NO-ERROR.
               hBeforeBuff:SAVE-ROW-CHANGES() .           
            END.
            IF NOT ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES = 0 THEN. 
            ELSE DO: 
               IF hBeforeBuff:ERROR THEN DO:
                  ASSIGN 
                  ChDataSet:ERROR       = TRUE
                  hBeforeBuff:REJECTED = TRUE.
               END.
               
               /*
               RUN lock_UI (input hBeforeBuff).
               MESSAGE "FEL"
               VIEW-AS ALERT-BOX.
               */
            END.  
             
            hBeforeBuff:ACCEPT-ROW-CHANGES () .
            IF hBeforeBuff:ERROR THEN DO:
               ASSIGN 
               ChDataSet:ERROR       = TRUE
               hBeforeBuff:REJECTED = TRUE.
               
            END.
            
            
         END.      
         hBeforeQry:GET-NEXT().
      END. /* DO WHILE NOT QUERY-OFF-END */
      hBeforeQry:QUERY-CLOSE().
      DELETE OBJECT hBeforeQry.
   END. /* DO IF VALID-HANDLE */
   DELETE OBJECT hBeforeBuff NO-ERROR.
   hBeforeBuff = ?.
   
   DELETE OBJECT hBeforeQry NO-ERROR.
   hBeforeQry = ?.
END PROCEDURE. /* saveChanges */


PROCEDURE lock_UI :
   DEFINE INPUT  PARAMETER hBeforeBuff AS HANDLE NO-UNDO.
   DEFINE VARIABLE hDBBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE hDBField AS HANDLE NO-UNDO.
   DEFINE VARIABLE hTTField AS HANDLE NO-UNDO.

   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   
   iNumFields = hBeforeBuff:NUM-FIELDS.
   
       
   OUTPUT TO Sparlog.txt APPEND.  
   DO i = 1 TO iNumfields:
       hDBField = hBeforeBuff:BUFFER-FIELD (i).
       PUT UNFORMATTED hBeforeBuff:TABLE " " hDBField:NAME " " hDBField:BUFFER-VALUE SKIP.
   END.
   OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE ReloadDs_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR DatasetDeftt.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSetoUT.
  /*
   phDataSetoUT:EMPTY-DATASET() NO-ERROR.  
   phDataSet:EMPTY-DATASET() NO-ERROR.
   */
   
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   RUN load_UI (INPUT-OUTPUT DATASET-HANDLE phDataSet).
   phDataSetoUT = phDataSet.
   
END PROCEDURE.

/*
PROCEDURE AttDataSource_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR DatasetDeftt.
   DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet.
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   DO iBuff = 1 TO DatasetDeftt.antaltab:
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL STRING(DynWp).
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iBuff] IN WIDGET-POOL STRING(DynWp).
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iBuff]).
      phDataSet:GET-BUFFER-HANDLE(iBuff):ATTACH-DATA-SOURCE(hDataSource).
   END. 
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
END PROCEDURE.
*/
PROCEDURE RelDatset_UI :
   DELETE WIDGET-POOL STRING(DynWp) NO-ERROR.
END PROCEDURE.