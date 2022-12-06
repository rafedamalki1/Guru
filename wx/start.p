DEFINE var bernrDS AS CHARACTER.
DEFINE var  berOmrDS AS CHARACTER.  
DEFINE VAR DynWp AS CHARACTER NO-UNDO.
DEFINE VARIABLE berExtraBhtt AS HANDLE NO-UNDO.
DEFINE VARIABLE berExtraBhbuff AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE DatasetDeftt NO-UNDO
   FIELD dataDsName AS CHARACTER
   FIELD antaltab     AS INTEGER 
   FIELD pcBuffers    AS CHARACTER  EXTENT 30
   FIELD pcRelFields  AS CHARACTER  EXTENT 30
   FIELD pcRelTables  AS CHARACTER  EXTENT 30
   FIELD pcSources    AS CHARACTER  EXTENT 30
   FIELD pcSourceKeys AS CHARACTER  EXTENT 30
   FIELD pcKeyValue   AS CHARACTER  EXTENT 30
   FIELD pcSourceKeysWhere AS CHARACTER  EXTENT 30
   FIELD repMode  AS LOGICAL
   FIELD nestMode AS LOGICAL.
DEFINE VARIABLE dyndamicDSh AS HANDLE NO-UNDO .

RUN c:\temp\TDynamicDataSet.p PERSISTENT SET dyndamicDSh. 

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
   CREATE WIDGET-POOL "DynTableDS".
   FIND FIRST DatasetDeftt WHERE DatasetDeftt.dataDsName = indsname NO-LOCK NO-ERROR.
   DO iBuff = 1 TO DatasetDeftt.antaltab:
      CREATE DATA-SOURCE hDataSource IN WIDGET-POOL "DynTableDS".
      CREATE BUFFER hBuffer FOR TABLE DatasetDeftt.pcSources[iBuff] IN WIDGET-POOL "DynTableDS".
      hDataSource:ADD-SOURCE-BUFFER(hBuffer,DatasetDeftt.pcSourceKeys[iBuff]).
      IF DatasetDeftt.pcSourceKeysWhere[iBuff] NE "" THEN DO:
         hDataSource:SAVE-WHERE-STRING(1) = DatasetDeftt.pcSourceKeysWhere[iBuff].
      END. 
      chDS:GET-BUFFER-HANDLE(iBuff):ATTACH-DATA-SOURCE(hDataSource).
   END.
   RUN SaveDs_UI IN dyndamicDSh  (INPUT DATASET-HANDLE chDS BIND).
   DO iBuff = 1 TO chDS:NUM-BUFFERS:
      chDS:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
   END.
   DELETE WIDGET-POOL "DynTableDS" NO-ERROR.
   DELETE OBJECT hDataSource NO-ERROR.
   hDataSource = ?.
   DELETE OBJECT hBuffer NO-ERROR.
   hBuffer = ?.
   DELETE OBJECT chDS NO-ERROR.
   chDS = ?.
END PROCEDURE.
   
DynWp = STRING(DynWp) + STRING(TIME).
CREATE WIDGET-POOL STRING(DynWp) NO-ERROR.
bernrDS = "1450".
berOmrDS = "0911".
/*
DO TRANSACTION:
    CREATE EXTRADATA.
    ASSIGN 
    EXTRADATA.PROGRAM = "BERBILD"
    EXTRADATA.HUVUDINT = INTEGER(bernrDS)
    EXTRADATA.HUVUDCH = berOmrDS.
    EXTRADATA.SOKCHAR[2] = "start value".
END.
    
RELEASE EXTRADATA.
*/
DEFINE var BerExBDS as handle.


RUN BerDsExtraCreate_UI.
run Extra_BildDS_UI.

/*
DEFINE TEMP-TABLE ExtraDataTT  NO-UNDO LIKE EXTRADATA
BEFORE-TABLE ExtraDataTTbef  
    FIELD TTRECID AS RECID.
DEFINE DATASET ExtraDataDS FOR ExtraDataTT.
{SparaProDatasSet.i ExtraDataDS}

DEFINE QUERY ExtraDataQuery FOR EXTRADATA.
DEFINE DATA-SOURCE ExtraDataSrc FOR QUERY ExtraDataQuery EXTRADATA KEYS (PROGRAM,HUVUDINT,HUVUDCH).   /*keys unika nycklar*/

DEFINE VARIABLE hExtraDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hExtraDataSet = DATASET ExtraDataDS:HANDLE.      /*koppla handel till dataset*/

hExtraDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillExtraDataDS", THIS-PROCEDURE). 
RUN ExtraDataDS_UI (INPUT "BERBILD",INPUT bernrDS, INPUT berOmrDS).
PROCEDURE ExtraDataDS_UI :
   DEFINE INPUT PARAMETER pnamn AS CHARACTER.
   DEFINE INPUT PARAMETER intnum AS INTEGER.
   DEFINE INPUT  PARAMETER huvch AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   
   DATASET ExtraDataDS:EMPTY-DATASET().
   queryprep = "FOR EACH EXTRADATA WHERE PROGRAM = " + QUOTER(pnamn) + " AND HUVUDINT = " + STRING(intnum) + " AND  HUVUDCH = " +  QUOTER(huvch) + "  NO-LOCK".
   QUERY ExtraDataQuery:QUERY-PREPARE(queryprep).
   RUN attachExtraDataDS.
   DATASET ExtraDataDS:FILL().
   detachDataSetExtraDataDS(hExtraDataSet).
   TEMP-TABLE ExtraDataTT:TRACKING-CHANGES = TRUE.
   FIND FIRST ExtraDataTT WHERE NO-LOCK NO-ERROR.
   MESSAGE ExtraDataTT.SOKCHAR[2]
   VIEW-AS ALERT-BOX.
   ExtraDataTT.SOKCHAR[2] = "new valueds".
   TEMP-TABLE ExtraDataTT:TRACKING-CHANGES = FALSE.
    DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
    CREATE DATASET hDSChanges.
    hDSChanges:CREATE-LIKE (DATASET ExtraDataDS:HANDLE).
    hDSChanges:GET-CHANGES (DATASET ExtraDataDS:HANDLE).
    RUN SparaProDataSetExtraDataDS(INPUT DATASET-HANDLE hDSChanges).
    hDSChanges:MERGE-CHANGES(DATASET ExtraDataDS:HANDLE).
   
END PROCEDURE.
PROCEDURE attachExtraDataDS: /*kopplar ihop temptabell med skarptababell.      */
   hExtraDataSet:GET-BUFFER-HANDLE("ExtraDataTT"):ATTACH-DATA-SOURCE(DATA-SOURCE ExtraDataSrc:HANDLE).
END PROCEDURE.
*/
 
 
PROCEDURE BerDsExtraCreate_UI :
   CREATE TEMP-TABLE berExtraBhtt IN WIDGET-POOL STRING(DynWp).
   berExtraBhtt:CREATE-LIKE("EXTRADATA").
   berExtraBhtt:ADD-NEW-FIELD("TTRECID","RECID").
   berExtraBhtt:TEMP-TABLE-PREPARE("ex_bildtt").
   berExtraBhbuff = berExtraBhtt:DEFAULT-BUFFER-HANDLE.
END PROCEDURE.   
PROCEDURE Extra_BildDS_UI :
   
   RUN GetDatasetDeftt_UI ("BerExBDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(berExtraBhbuff).
   DatasetDeftt.pcBuffers[2] = "".
   DatasetDeftt.pcRelFields[1] = "".
   DatasetDeftt.pcSources[1] = "EXTRADATA". 
   DatasetDeftt.pcSources[2] = "".
   DatasetDeftt.pcSourceKeys[1] = "PROGRAM,HUVUDINT,HUVUDCH,SOKINT,SOKCHAR".
   DatasetDeftt.pcSourceKeys[2] = "".
   DatasetDeftt.pcSourceKeysWhere[1] = " WHERE EXTRADATA.PROGRAM = QUOTER(hBeforeBuff:BUFFER-FIELD('PROGRAM'):BUFFER-VALUE) AND EXTRADATA.HUVUDIN = STRING(hBeforeBuff:BUFFER-FIELD('HUVUDINT'):BUFFER-VALUE) AND
   EXTRADATA.HUVUDCH =  QUOTER(hBeforeBuff:BUFFER-FIELD('HUVUDCH'):BUFFER-VALUE) AND 
   EXTRADATA.SOKINT = STRING(hBeforeBuff:BUFFER-FIELD('SOKINT'):BUFFER-VALUE) AND 
   EXTRADATA.SOKCHAR =  QUOTER(hBeforeBuff:BUFFER-FIELD('SOKCHAR'):BUFFER-VALUE)".
   
   
   /*
   DatasetDeftt.pcSourceKeysWhere[1] = " WHERE EXTRADATA.PROGRAM = QUOTER(BIex_bildtt.PROGRAM) AND EXTRADATA.HUVUDIN = STRING(BIex_bildtt.HUVUDIN) AND
   EXTRADATA.HUVUDCH =  QUOTER(BIex_bildtt.HUVUDCH) AND 
   EXTRADATA.SOKINT = STRING(BIex_bildtt.SOKINT) AND 
   EXTRADATA.SOKCHAR =  QUOTER(BIex_bildtt.SOKCHAR)".
   */
   DatasetDeftt.pcKeyValue[1] = "PROGRAM = " + QUOTER('BERBILD') + " AND HUVUDINT = " + bernrDS + " AND  HUVUDCH = " +  QUOTER(berOmrDS).
   RUN DefAndLoadDs_UI IN dyndamicDSh
   (INPUT DatasetDeftt.dataDsName, INPUT TABLE DatasetDeftt,  OUTPUT DATASET-HANDLE BerExBDS BIND).
   berExtraBhbuff:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
   MESSAGE  berExtraBhbuff:BUFFER-FIELD("SOKCHAR"):BUFFER-VALUE(2)
        VIEW-AS ALERT-BOX.
        DEFINE VARIABLE sn AS CHARACTER NO-UNDO.
   /*
    RUN GetDataSource_UI IN dyndamicDSh (INPUT 1,output sn).
    */
    berExtraBhtt:TRACKING-CHANGES = TRUE.
    berExtraBhbuff:BUFFER-FIELD("SOKCHAR"):BUFFER-VALUE(2) = "wnew valuech".
    berExtraBhtt:TRACKING-CHANGES = FALSE.
    RUN SparaBerDs_ui.
END PROCEDURE.

PROCEDURE SparaBerDs_UI :
   DEFINE VARIABLE chVDS AS HANDLE NO-UNDO.
    
   CREATE DATASET chVDS.
   chVDS:CREATE-LIKE(BerExBDS).
   chVDS:GET-CHANGES(BerExBDS).
   /*
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      chVDS:WRITE-XML("FILE", SPARAXML).
     */
   RUN SparaDynDSstart_UI (INPUT "BerExBDS", INPUT DATASET-HANDLE chVDS).
   chVDS:MERGE-CHANGES(BerExBDS).
END PROCEDURE.
