
/*------------------------------------------------------------------------
    File        : KALKBORT.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Wed Aug 29 08:34:24 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
{KALKYLKAT.I}
   
{KALKYLPRODATABORT.I}
{SparaProDatasSet.i KalkylBortDS}
 /*def av datasetets query*/
DEFINE QUERY KalkylQuery FOR KALKHUV.


DEFINE DATA-SOURCE KalkhuvSrc FOR QUERY KalkylQuery KALKHUV KEYS (KALKNR,OMRADE).   /*keys unika nycklar*/
DEFINE DATA-SOURCE NumSrc FOR KALKNUM KEYS (KALKNR,OMRADE,NUM).
DEFINE DATA-SOURCE NumSubSrc FOR KALKNUMSUB KEYS (KALKNR,OMRADE,NUM,NUMSUBID).
DEFINE DATA-SOURCE AonrSrc FOR KALKAONR KEYS (KALKNR,OMRADE).
DEFINE DATA-SOURCE FaktSrc FOR KALKFAKTORER KEYS (KALKNR,OMRADE,KPID).
DEFINE DATA-SOURCE EgnaSrc FOR KALKEGNAPRISER KEYS (KALKNR,OMRADE,KPID).
DEFINE DATA-SOURCE MtrlSrc FOR KALKMTRL KEYS (KALKNR,OMRADE,MID).

DEFINE VARIABLE hKalkylDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylDataSet = DATASET KalkylBortDS:HANDLE.      /*koppla handel till dataset*/
hKalkylDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylBortDS", THIS-PROCEDURE). 
PROCEDURE attachKalkylBortDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkhuvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkhuvSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalknumtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalknumsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumsubSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkaonrTT"):ATTACH-DATA-SOURCE(DATA-SOURCE AonrSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkfaktorertt"):ATTACH-DATA-SOURCE(DATA-SOURCE FaktSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkegnaprisertt"):ATTACH-DATA-SOURCE(DATA-SOURCE EgnaSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalktmtrlTT"):ATTACH-DATA-SOURCE(DATA-SOURCE MtrlSrc:HANDLE).
   
END PROCEDURE.
PROCEDURE LaddaKalkylBort:
   DEFINE INPUT PARAMETER KalkNr AS INTEGER.
   DEFINE INPUT PARAMETER KalkOmr AS CHARACTER.  
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylBortDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET KalkylBortDS:EMPTY-DATASET().
   queryprep = "FOR EACH KALKHUV WHERE KALKHUV.KALKNR = " + STRING(KalkNr) + 
   " AND KALKHUV.OMRADE = " + "'" + KalkOmr + "'" + "  NO-LOCK". 
   QUERY KalkylQuery:QUERY-PREPARE(queryprep).
   RUN attachKalkylBortDS.
   DATASET KalkylBortDS:FILL().
   detachDataSetKalkylbortDS(hKalkylDataSet).
   
END PROCEDURE.