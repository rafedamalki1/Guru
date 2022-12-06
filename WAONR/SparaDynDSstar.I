
/* SparaDynDSstar.I*/

{DataSetVariable.I}
DEFINE VARIABLE dyndamicDSh AS HANDLE NO-UNDO .

RUN DynamicDataSet.p PERSISTENT SET dyndamicDSh. 

PROCEDURE GetDatasetDeftt_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DatasetDeftt THEN DO:
      CREATE DatasetDeftt.
      DatasetDeftt.dataDsName = indsname.
   END.   
END PROCEDURE.
PROCEDURE SparaDynDSstart_UI :
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER DATASET-HANDLE chDS BIND.
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
   DEFINE VAR DynWp AS CHARACTER NO-UNDO.
   DynWp = "DynTableDS" + STRING(TIME).
   CREATE WIDGET-POOL STRING(DynWp).
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   DO iBuff = 1 TO DatasetDeftt.antaltab:
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL STRING(DynWp).
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iBuff] IN WIDGET-POOL STRING(DynWp).
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iBuff]).
      chDS:GET-BUFFER-HANDLE(iBuff):ATTACH-DATA-SOURCE(hDataSource).
   END.
   RUN SaveDs_UI IN dyndamicDSh  (INPUT DATASET-HANDLE chDS BIND).
   DO iBuff = 1 TO chDS:NUM-BUFFERS:
      chDS:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
   END.
   DELETE WIDGET-POOL STRING(DynWp) NO-ERROR.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT chDS NO-ERROR.
   chDS = ?.
END PROCEDURE.