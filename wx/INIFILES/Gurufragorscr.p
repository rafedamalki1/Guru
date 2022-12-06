
/*------------------------------------------------------------------------
    File        : Gurufragorscr.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 10 09:23:49 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

{EXTRADATA.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR extradatatemp.
{GURUFRAG.I}
DEFINE VARIABLE ordvar AS INTEGER NO-UNDO.



RUN visa_UI.   
PROCEDURE visa_UI :
   DEFINE VARIABLE fragorguru   AS Modules.Global.GuruFragorMaps NO-UNDO.  
   DEFINE VARIABLE fragorguruDataSet    AS HANDLE     NO-UNDO.
   DEFINE VARIABLE antalDSTabeller    AS INTEGER     NO-UNDO.
   
   FOR EACH extradatatemp WHERE extradatatemp.SOKCHAR[10] = "S2" OR extradatatemp.SOKCHAR[10] = "S3":
      ordvar = ordvar + 1.
      CREATE funktionsfragor.
      ASSIGN
      funktionsfragor.ID = extradatatemp.HUVUDINT
      funktionsfragor.ORDNING = ordvar
      funktionsfragor.STEG = extradatatemp.SOKCHAR[10]
      funktionsfragor.FRAGA =  extradatatemp.SOKCHAR[1] + " " + extradatatemp.SOKCHAR[2].
      IF extradatatemp.SOKCHAR[3] NE "" THEN DO:
         CREATE funktionstexter.
         ASSIGN
         funktionstexter.ID = funktionsfragor.ID
         funktionstexter.ORDNING = funktionsfragor.ORDNING
         funktionstexter.UNDERTEXT1 = extradatatemp.SOKCHAR[3] + " " + extradatatemp.SOKCHAR[4].
      END.   
   END.   
 
   RUN dyndsJmBerInkvis_UI (OUTPUT fragorguruDataSet, OUTPUT antalDSTabeller).

   fragorguru = NEW Modules.Global.GuruFragorMaps().
 
   fragorguru:ConnectDataset(INPUT DATASET-HANDLE fragorguruDataSet BIND, INPUT antalDSTabeller).
  
   fragorguru:InitiateGuruMaps().
   
   WAIT-FOR fragorguru:ShowDialog().
   
   DELETE OBJECT fragorguru NO-ERROR.
   RETURN.

END PROCEDURE.   

PROCEDURE dyndsJmBerInkvis_UI :
   DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
   DEFINE VARIABLE bhTable      AS HANDLE     NO-UNDO.
   
   DEFINE OUTPUT PARAMETER fragorguruDataSet AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER iCounter      AS INTEGER    NO-UNDO.
  
   CREATE DATASET fragorguruDataSet.
   
   CREATE TEMP-TABLE hTable.
   iCounter = iCounter + 1.
   hTable:CREATE-LIKE("funktionsfragor").
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("funktionsfragor").
   
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   
   FOR EACH funktionsfragor NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-COPY(BUFFER funktionsfragor:HANDLE).
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID. 
   END.
          
   fragorguruDataSet:ADD-BUFFER(bhTable).
   /*TOP*/
   
   CREATE TEMP-TABLE hTable.
   hTable:CREATE-LIKE("funktionstexter").
   iCounter = iCounter + 1.
   hTable:ADD-NEW-FIELD("TTRECID","RECID").
   hTable:ADD-NEW-FIELD("TYPSNITT","INTEGER").
   hTable:TEMP-TABLE-PREPARE("funktionstexter").
   
   bhTable = hTable:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH funktionstexter NO-LOCK:
      bhTable:BUFFER-CREATE.
      bhTable:BUFFER-COPY(BUFFER funktionstexter:HANDLE).
      bhTable:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = bhTable:RECID.  
      
   END.
   fragorguruDataSet:ADD-BUFFER(bhTable).
   fragorguruDataSet:ADD-RELATION(fragorguruDataSet:GET-BUFFER-HANDLE(1), fragorguruDataSet:GET-BUFFER-HANDLE(iCounter), "ID,ID").
   DELETE OBJECT hTable NO-ERROR.
   hTable = ?.
   DELETE OBJECT bhTable NO-ERROR.
   bhTable = ?.
  
END.