
/*------------------------------------------------------------------------
    File        : JmfBeredningInkop.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 10 09:23:49 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/
{FLBERTEMP.I}
{JMFBERINKTT.I}
DEFINE INPUT  PARAMETER beraonrvar AS CHARACTER NO-UNDO.
IF Guru.Konstanter:appcon THEN DO:
   RUN JMFBERINK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT beraonrvar,OUTPUT TABLE flerbertemp,OUTPUT TABLE berinkmtrtt,OUTPUT TABLE summberinkmtrtt).
END.
ELSE DO:
   RUN JMFBERINK.P (INPUT beraonrvar,OUTPUT TABLE flerbertemp,OUTPUT TABLE berinkmtrtt,OUTPUT TABLE summberinkmtrtt).
END.
RUN visa_UI.   
PROCEDURE visa_UI :
   DEFINE VARIABLE jmfberink   AS Modules.Beredning.JmfBerInkVisning NO-UNDO.  
   DEFINE VARIABLE jmfBerInkDataSet    AS HANDLE     NO-UNDO.
   DEFINE VARIABLE antalDSTabeller    AS INTEGER     NO-UNDO.
  
 
   RUN dyndsJmBerInkvis_UI (OUTPUT jmfBerInkDataSet, OUTPUT antalDSTabeller).

   jmfberink = NEW Modules.Beredning.JmfBerInkVisning().
 
   jmfberink:ConnectDataset(INPUT DATASET-HANDLE jmfBerInkDataSet BIND, INPUT antalDSTabeller).
  
   jmfberink:InitiateJmBerInkVis().
   
   WAIT-FOR jmfberink:ShowDialog().
   
   DELETE OBJECT jmfberink NO-ERROR.
   RETURN.

END PROCEDURE.   

PROCEDURE dyndsJmBerInkvis_UI :
   DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
   DEFINE VARIABLE bhTable      AS HANDLE     NO-UNDO.
   
   DEFINE OUTPUT PARAMETER jmfBerInkDataSet AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER iCounter      AS INTEGER    NO-UNDO.
  
   CREATE DATASET jmfBerInkDataSet.
   
   
   
   CREATE TEMP-TABLE hTable.
   iCounter = iCounter + 1.
   hTable:ADD-NEW-FIELD("NAMN","CHARACTER").
   hTable:ADD-NEW-FIELD("TOP","INTEGER").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("topvisa").
   
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   
   FIND FIRST flerbertemp WHERE USE-INDEX AONR NO-LOCK NO-ERROR.
   bhTable:BUFFER-CREATE.
   bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 1.
   bhTable:BUFFER-FIELD("NAMN"):BUFFER-VALUE = flerbertemp.BENAMNING.
   bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID. 
   
   
   bhTable:BUFFER-CREATE.
   bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 2.
   bhTable:BUFFER-FIELD("NAMN"):BUFFER-VALUE = "Summering".
   bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.
   
   bhTable:BUFFER-CREATE.
   hTable:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE.
   bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 3.
   bhTable:BUFFER-FIELD("NAMN"):BUFFER-VALUE = "Per " + Guru.Konstanter:gaol.
   bhTable:FIND-FIRST("WHERE TOP = 0 ",NO-LOCK) NO-ERROR.
   IF bhTable:AVAILABLE THEN DO:
      bhTable:BUFFER-DELETE. 
   END.
         
   jmfBerInkDataSet:ADD-BUFFER(bhTable).
   /*TOP*/
   
   CREATE TEMP-TABLE hTable.
   hTable:CREATE-LIKE("flerbertemp").
   iCounter = iCounter + 1.
   hTable:ADD-NEW-FIELD("TOP","INTEGER").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("flerber").
   
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH flerbertemp WHERE NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 1.
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.  
      bhTable:BUFFER-COPY(BUFFER flerbertemp:HANDLE).
   END.
   
   jmfBerInkDataSet:ADD-BUFFER(bhTable).
   jmfBerInkDataSet:ADD-RELATION(jmfBerInkDataSet:GET-BUFFER-HANDLE(1), jmfBerInkDataSet:GET-BUFFER-HANDLE(iCounter), "TOP,TOP").
   CREATE TEMP-TABLE hTable.
   iCounter = iCounter + 1.
   hTable:CREATE-LIKE("summberinkmtrtt").
   hTable:ADD-NEW-FIELD("TOP","INTEGER").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("summberink").
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH summberinkmtrtt WHERE NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 2.
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.  
      bhTable:BUFFER-COPY(BUFFER summberinkmtrtt:HANDLE).
   END.
   jmfBerInkDataSet:ADD-BUFFER(bhTable).
   jmfBerInkDataSet:ADD-RELATION(jmfBerInkDataSet:GET-BUFFER-HANDLE(1), jmfBerInkDataSet:GET-BUFFER-HANDLE(iCounter), "TOP,TOP").
   
   CREATE TEMP-TABLE hTable.
   iCounter = iCounter + 1.
   hTable:CREATE-LIKE("flerbertemp").
   hTable:ADD-NEW-FIELD("TOP","INTEGER").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("perflerber").
   
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH flerbertemp WHERE NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 3.
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.  
      bhTable:BUFFER-COPY(BUFFER flerbertemp:HANDLE).
   END.
   
   jmfBerInkDataSet:ADD-BUFFER(bhTable).
   jmfBerInkDataSet:ADD-RELATION(jmfBerInkDataSet:GET-BUFFER-HANDLE(1), jmfBerInkDataSet:GET-BUFFER-HANDLE(iCounter), "TOP,TOP").
   
   CREATE TEMP-TABLE hTable.
   iCounter = iCounter + 1.
   hTable:CREATE-LIKE("berinkmtrtt").
   hTable:ADD-NEW-FIELD("TOP","INTEGER").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("berinkmtr").
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH berinkmtrtt WHERE NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-FIELD("TOP"):BUFFER-VALUE = 3.
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.  
      bhTable:BUFFER-COPY(BUFFER berinkmtrtt:HANDLE).
   END. 
   jmfBerInkDataSet:ADD-BUFFER(bhTable).
   jmfBerInkDataSet:ADD-RELATION(jmfBerInkDataSet:GET-BUFFER-HANDLE(iCounter - 1), jmfBerInkDataSet:GET-BUFFER-HANDLE(iCounter), "DELNR,DELNR").
   DELETE OBJECT hTable NO-ERROR.
   hTable = ?.
   DELETE OBJECT bhTable NO-ERROR.
   bhTable = ?.
  
END.