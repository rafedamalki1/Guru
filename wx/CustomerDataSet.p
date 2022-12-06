
/*------------------------------------------------------------------------
    File        :CustomerDataSet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Dec 15 10:16:16 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE dyndamicDSh AS HANDLE NO-UNDO.
DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE CustomerDS AS HANDLE NO-UNDO.
DEFINE VARIABLE Customertth AS HANDLE NO-UNDO.
DEFINE VARIABLE Customerbuffh AS HANDLE NO-UNDO.
CREATE TEMP-TABLE Customertth.
Customertth:CREATE-LIKE("Customer").
Customertth:TEMP-TABLE-PREPARE("Customertt").
Customerbuffh = Customertth:DEFAULT-BUFFER-HANDLE.



RUN CustomerDynamicDataSet.p PERSISTENT SET dyndamicDSh.
DEFINE VARIABLE counttab     AS INTEGER NO-UNDO.
DEFINE VARIABLE pcBuffers    AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE pcRelFields  AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE pcRelTables   AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE pcSources    AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE pcSourceKeys AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE pcKeyValue   AS CHARACTER NO-UNDO EXTENT 20.



PROCEDURE loadCustomerDS_UI :
   DEFINE INPUT  PARAMETER incustnum AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE CustomerDS bind.
   counttab = 1.
   pcBuffers[1] = STRING(Customerbuffh). 
   pcSources[1] = "Customer".
   pcSourceKeys[1] = "CustNum".
   pcKeyValue[1] = "Customer.CustNum = " + STRING(incustnum).

    RUN DefAndLoadDs_UI IN dyndamicDSh 
     (INPUT counttab,
      INPUT pcBuffers,
      INPUT pcRelFields,
      INPUT pcRelTables,
      INPUT pcSources,
      INPUT pcSourceKeys,
      INPUT pcKeyValue,
      OUTPUT DATASET-HANDLE CustomerDS BIND).
      
      Customertth:TRACKING-CHANGES = TRUE.
     
END PROCEDURE.

PROCEDURE SaveCustomerDS_UI :
   DEFINE INPUT PARAMETER DATASET-HANDLE OrgDynDS.
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
   Customertth:TRACKING-CHANGES = FALSE.
   
   CREATE DATASET chDS.
   chDS:CREATE-LIKE(OrgDynDS).
   
   CREATE DATA-SOURCE hDataSource.
   CREATE BUFFER hBuffer FOR TABLE pcSources[1].
   hDataSource:ADD-SOURCE-BUFFER(hBuffer,pcSourceKeys[1]).
   chDS:GET-BUFFER-HANDLE(1):ATTACH-DATA-SOURCE(hDataSource).
  
   
   chDS:GET-CHANGES(OrgDynDS).
   
   Customertth:TRACKING-CHANGES = TRUE.
   
   RUN SPARADATSET.p (INPUT chDS).
   DO iBuff = 1 TO chDS:NUM-BUFFERS:
      chDS:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
   END.
   
END PROCEDURE.

PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh.
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.

PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE() NO-ERROR.
END PROCEDURE.

PROCEDURE dstest_UI :
   DEFINE VARIABLE dsTEMPH AS HANDLE NO-UNDO.
   DEFINE VARIABLE dsTTh AS HANDLE NO-UNDO.
   CREATE TEMP-TABLE dsTEMPH.
   dsTEMPH:ADD-NEW-FIELD("enr","character").
   dsTEMPH:ADD-NEW-FIELD("antal","character").
   dsTEMPH:TEMP-TABLE-PREPARE("ekalknumanvegentt").
   dsTTh = dsTEMPH:DEFAULT-BUFFER-HANDLE.
   DEFINE VARIABLE DSChanges AS HANDLE NO-UNDO. 
   CREATE DATASET DSChanges.
   DSChanges:ADD-BUFFER(dsTTh).
   dsTEMPH:TRACKING-CHANGES = TRUE.
   dsTTh:BUFFER-CREATE().
   dsTTh:BUFFER-FIELD("enr"):BUFFER-VALUE = "33".
   DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
   dsTEMPH:TRACKING-CHANGES = FALSE.
   DEFINE VARIABLE hBeforeBuff AS HANDLE NO-UNDO.
   
    hBeforeBuff = dsTTh:BEFORE-BUFFER.
   MESSAGE hBeforeBuff:NAME 
   VIEW-AS ALERT-BOX.
   CREATE DATASET hDSChanges.
   hDSChanges:CREATE-LIKE (DSChanges).
   hDSChanges:GET-CHANGES (DSChanges).
   
   
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML). 
  
   
  
END PROCEDURE.
