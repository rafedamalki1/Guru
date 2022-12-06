
/*------------------------------------------------------------------------
    File        : customdsapp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Wed Aug 29 08:34:24 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE ctt NO-UNDO LIKE customer
   BEFORE-TABLE cbef
   FIELD TTRECID AS RECID.
  
DEFINE TEMP-TABLE ott NO-UNDO LIKE order
   BEFORE-TABLE obef
   FIELD TTRECID AS RECID.
   
DEFINE TEMP-TABLE oltt NO-UNDO LIKE orderline
   BEFORE-TABLE olbef
   FIELD TTRECID AS RECID.


DEFINE DATASET custDS FOR ctt,ott,oltt
DATA-RELATION oDR FOR ctt, ott RELATION-FIELDS (ctt.custnum,ott.custnum)
DATA-RELATION olDR FOR ott, oltt RELATION-FIELDS (ott.ordernum,oltt.ordernum).
{SparaProDatasSet.i custDS}
 /*def av datasetets query*/
DEFINE QUERY cQuery FOR customer.


DEFINE DATA-SOURCE cSrc FOR QUERY cQuery customer KEYS (custnum).   /*keys unika nycklar*/
DEFINE DATA-SOURCE oSrc FOR order KEYS (custnum,ordernum).
DEFINE DATA-SOURCE olSrc FOR orderline KEYS (ordernum,linenum).

DEFINE VARIABLE hDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hDataSet = DATASET custDS:HANDLE.      /*koppla handel till dataset*/
hDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillcustDS", THIS-PROCEDURE). 
PROCEDURE attachcustDS: /*kopplar ihop temptabell med skarptababell.      */
   hDataSet:GET-BUFFER-HANDLE("ctt"):ATTACH-DATA-SOURCE(DATA-SOURCE cSrc:HANDLE).
   hDataSet:GET-BUFFER-HANDLE("ott"):ATTACH-DATA-SOURCE(DATA-SOURCE oSrc:HANDLE).
   hDataSet:GET-BUFFER-HANDLE("oltt"):ATTACH-DATA-SOURCE(DATA-SOURCE olSrc:HANDLE).
   
END PROCEDURE.
PROCEDURE LaddaCUST:
   DEFINE INPUT PARAMETER cnumvar AS INTEGER.   
   DEFINE OUTPUT PARAMETER DATASET FOR custDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET custDS:EMPTY-DATASET().
   queryprep = "FOR EACH customer WHERE customer.custnum = " + STRING(cnumvar) + "  NO-LOCK". 
   QUERY cQuery:QUERY-PREPARE(queryprep).
   RUN attachcustDS.
   DATASET custDS:FILL().
   detachDataSetcustDS(hDataSet).
   
END PROCEDURE.