
/*------------------------------------------------------------------------
    File        : Customerdynstart.P
    Purpose     : 

   
  ----------------------------------------------------------------------*/

DEFINE VARIABLE Customerdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE Customerbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE CustomerDS AS HANDLE NO-UNDO. 
DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE Customertth AS HANDLE NO-UNDO.
RUN CustomerDataSet.p PERSISTENT SET Customerdynh.

RUN loadCustomerDS_UI IN Customerdynh(INPUT 1, OUTPUT DATASET-HANDLE CustomerDS BIND).
Customerbuffh = CustomerDS:GET-BUFFER-HANDLE(1).


queryvar = "FOR EACH " + Customerbuffh:TABLE + " NO-LOCK".
RUN CreateCustomQuery(INPUT Customerbuffh,INPUT queryvar,OUTPUT qh).
qH:GET-FIRST(NO-LOCK).
DO WHILE qH:QUERY-OFF-END = FALSE:
   DO TRANSACTION:
      MESSAGE Customerbuffh:BUFFER-FIELD("Name"):BUFFER-VALUE 
      VIEW-AS ALERT-BOX.
      
      Customerbuffh:BUFFER-FIELD("Name"):BUFFER-VALUE = Customerbuffh:BUFFER-FIELD("Name"):BUFFER-VALUE + "A". 
   END.
   qH:GET-NEXT(NO-LOCK).
END.
   
RUN CloseCustomQuery(INPUT qH).


RUN ChDsSave_UI.

PROCEDURE ChDsSave_UI :
   /*
   DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
   CREATE DATASET hDSChanges.
   hDSChanges:CREATE-LIKE (CustomerDS).
   hDSChanges:GET-CHANGES (CustomerDS).
   
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML). 
   hDSChanges:MERGE-CHANGES(CustomerDS).
   */
   RUN SaveCustomerDS_UI IN Customerdynh(DATASET-HANDLE CustomerDS).
  
   
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

