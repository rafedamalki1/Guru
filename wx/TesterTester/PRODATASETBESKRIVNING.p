PRODATASETBESKRIVNING
DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.
/*KLIENT*/   
{KALKYLKAT.I}    /*TEMP TABLE  obs BEFORE-TABLE kalknumttbef*/
{prodatakalk.i}    /*DEF AV DATASETET*/
   /* prodatakalk.i
   DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt
   DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
   DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM).  
   */
RUN ProDataApp.p PERSISTENT SET AppServerHandle.
RUN LaddaKalkyl IN AppServerHandle (OUTPUT DATASET KalkylDS, INPUT 100284).
TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.   /*k�nner av f�r�ndringar vi f�r inte tracking.i att funka eller PDSTracker.cls */
TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE.
run upp.
RUN upp.
RUN upp.
PROCEDURE upp:
   
   find first kalknumtt.
   update kalknumtt.markning.
   run sparaKalkyl.
   FOR EACH kalknum WHERE kalknum.kalknr = 100284 NO-LOCK:   
      display kalknum.arbkod kalknum.LOPNR kalknum.NUM kalknum.MARKNING .
   end.

END PROCEDURE .
    
PROCEDURE SparaKalkyl:
   DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
   TEMP-TABLE kalknumtt:TRACKING-CHANGES = FALSE.  /*under lagring f�r inga f�r�ndrigar g�ras skall s�ttas p� igen efter*/ 
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = FALSE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = FALSE.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).
      hDSChanges:WRITE-XML("FILE", "C:\CTest.xml").   /*SPARA FIL F�R TEST EJ KVAR I SKARPA*/   
      RUN SparaProDataSetkalkylds IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).  /*skickar ner f�r�ndringar*/
      hDSChanges:MERGE-CHANGES(DATASET KalkylDS:HANDLE).  /*ser till att alla f�r�ndringar flyttas in i dataset som sparde f�r�ndringar*/
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.   /*k�nner av f�r�ndringar vi f�r inte tracking.i att funka eller PDSTracker.cls */
   TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE.
 END PROCEDURE .

 /*SERVER*/

 {KALKYLKAT.I}    /*TEMP TABLE  obs BEFORE-TABLE kalknumttbef*/
{prodatakalk.i}    /*DEF AV DATASETET*/
   /* prodatakalk.i
   DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt
   DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
   DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM).  
   */


{SparaProDatasSet.i KalkylDS}    /* allt som beh�vs f�r att spara obs! datasetets namn ska st� efter SparaProDatasSet.i
                                    har man flera dataset s� l�gger man denna rad f�r varje dataset.*/
 /*def av datasetets query*/
DEFINE QUERY KalkylQuery FOR KALKHUV .
DEFINE DATA-SOURCE KalkhuvSrc FOR QUERY KalkylQuery KALKHUV KEYS (KALKNR,OMRADE).   /*keys unika nycklar*/
DEFINE DATA-SOURCE NumSrc FOR KALKNUM KEYS (KALKNR,OMRADE,NUM).
DEFINE DATA-SOURCE NumsubSrc FOR KALKNUMSUB KEYS (KALKNR,OMRADE,NUM,NUMSUBID).

DEFINE VARIABLE hDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/

hDataSet = DATASET KalkylDS:HANDLE.      /*koppla handel till dataset*/

hDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylDS", THIS-PROCEDURE). 
 /*postDataSetFill finns i SparaProDatasSet.i f�r namnet postDataSetFillKalkylDS vid kompilering
proceduren k�rs efter datasetet har blivit fyllt med datat i proceduren laddakalkyl nedan.*/
           
PROCEDURE attachKalkylDS: /*kopplar ihop temptabell med skarptababell.      */
   hDataSet:GET-BUFFER-HANDLE("kalkhuvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkhuvSrc:HANDLE).
   hDataSet:GET-BUFFER-HANDLE("kalknumtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumSrc:HANDLE).
   hDataSet:GET-BUFFER-HANDLE("kalknumsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumsubSrc:HANDLE). 
   
END PROCEDURE.



PROCEDURE LaddaKalkyl:  
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylDS.
   DEFINE INPUT PARAMETER KalkNr AS INTEGER.
   DATASET KalkylDS:EMPTY-DATASET().
   QUERY KalkylQuery:QUERY-PREPARE("FOR EACH KALKHUV NO-LOCK WHERE KALKHUV.KALKNR = " + STRING(KalkNr)).
   RUN attachKalkylDS.
   DATASET KalkylDS:FILL().
   detachDataSetKalkylDS(hDataSet).
   
END PROCEDURE.


