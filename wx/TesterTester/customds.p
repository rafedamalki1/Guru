
/*------------------------------------------------------------------------
    File        : customds.p
    Purpose     : 

    Syntax      :

    Description : RUN C:\DELAD\pro9\guru\2Guru\Tester\customds.p.

    Author(s)   : elpao
    Created     : Wed Aug 29 09:17:29 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.
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
DEFINE VARIABLE hcustDS AS HANDLE NO-UNDO.   
   RUN customdsapp.p PERSISTENT SET AppServerHandle.
   RUN LaddaCUST IN AppServerHandle ( INPUT 1,OUTPUT DATASET custDS).
   
   TEMP-TABLE ctt:TRACKING-CHANGES = TRUE.
   TEMP-TABLE ott:TRACKING-CHANGES = TRUE.
   TEMP-TABLE oltt:TRACKING-CHANGES = TRUE.
   
   FOR EACH ctt WHERE,
EACH ott WHERE ott.custnum = ctt.custnum,
EACH oltt WHERE oltt.ordernum = ott.ordernum:
   DISPLAY ctt.custnum ott.ordernum oltt.linenum.
END.
FOR EACH ctt WHERE NO-LOCK:
      DELETE ctt.
   END.
   RUN SparaKalkyl.
   PROCEDURE SparaKalkyl:
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      TEMP-TABLE ctt:TRACKING-CHANGES = FALSE.
      TEMP-TABLE ott:TRACKING-CHANGES = FALSE.
      TEMP-TABLE oltt:TRACKING-CHANGES = FALSE.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET custDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET custDS:HANDLE).
      hDSChanges:WRITE-XML("FILE", "C:\Ccust.xml").
      RUN SparaProDataSetcustDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET custDS:HANDLE).
   END PROCEDURE .