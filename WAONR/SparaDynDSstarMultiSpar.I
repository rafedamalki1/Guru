
/* SparaDynDSstarMultiSpar.I*/

/*HÄMTA SAMMA DS FLERA GÅNGER MEN INTE MED ATT SPARA KALKBERAPPDS.p FÖR ATT VISA FLERA BEREDNINGAR IHOP*/
/*Anders Olsson Elpool i Umeå AB  20 maj 2019 13:02:44 
FÖR ATT KUNNA DYNAMSIKT UPPDATERA FLERA ds MEN KUNNA LADDA OM DESSA UTAN SPARNING I MELLAN
1. HÄMTA DET DU VILL
2. SPARA DET DU VILL OM DU VILL
3. HÄMTA PÅ NYTT FRÅN KÄLLAN

 
*/
DEFINE VARIABLE dyndamicDSMh AS HANDLE NO-UNDO EXTENT 10.

   
{DataSetVariable.I}

 


PROCEDURE GetDatasetDeftt_UI :
   DEFINE INPUT  PARAMETER dsnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER indsname AS CHARACTER NO-UNDO.
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DatasetDeftt THEN DO:
      CREATE DatasetDeftt.
      DatasetDeftt.dataDsName = indsname.
      DatasetDeftt.NrDS = dsnr.
      RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[DatasetDeftt.NrDS].   
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
   RUN SaveDs_UI IN dyndamicDSMh[DatasetDeftt.NrDS]  (INPUT DATASET-HANDLE chDS BIND).
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
PROCEDURE AvslutaDS_UI :
   DEFINE VARIABLE dsi AS INTEGER NO-UNDO.
   DO dsi = 1 TO 10: 
      IF VALID-HANDLE(dyndamicDSMh[dsi]) THEN DO:
         RUN RelDatset_UI IN dyndamicDSMh[dsi].
         DELETE PROCEDURE dyndamicDSMh[dsi] NO-ERROR.
         dyndamicDSMh[dsi] = ?.
      END.   
   END.  
END PROCEDURE.
