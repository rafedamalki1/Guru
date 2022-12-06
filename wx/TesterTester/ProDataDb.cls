 
 /*------------------------------------------------------------------------
    File        : ProDataDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed May 30 14:05:33 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Tester.ProDataDb:
   
   {KALKYLKAT.i}
{prodatakalk.i}   
   DEFINE PUBLIC PROPERTY KalkylNr AS INTEGER NO-UNDO
      PUBLIC GET.
      PROTECTED SET.
      
   DEFINE PUBLIC PROPERTY Omrade AS CHARACTER NO-UNDO
      PUBLIC GET.
      PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY Laddad AS LOGICAL NO-UNDO
      PUBLIC GET.
      PROTECTED SET.
   
   DEFINE PUBLIC VARIABLE AppServerHandle AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE NumTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE NumSubTT AS HANDLE NO-UNDO.
   



   CONSTRUCTOR PUBLIC ProDataDb():
      THIS-OBJECT:Connect().
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC ProDataDb():
      
   END DESTRUCTOR.
   
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ProDataApp.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN ProDataApp.p PERSISTENT SET AppServerHandle.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID LaddaKalkyl(KalkNr AS INTEGER):
      THIS-OBJECT:Rensa().
      RUN LaddaKalkyl IN AppServerHandle (OUTPUT DATASET KalkylDS, INPUT KalkNr).
      THIS-OBJECT:HuvudTT = TEMP-TABLE kalkhuvtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:NumTT = TEMP-TABLE kalknumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:NumSubTT = TEMP-TABLE kalknumsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      THIS-OBJECT:KalkylNr = KalkNr.
      THIS-OBJECT:HuvudTT:FIND-FIRST().
      THIS-OBJECT:Omrade = STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
      THIS-OBJECT:Laddad = TRUE.
      
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = TRUE.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = TRUE.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID SparaKalkyl():
      IF THIS-OBJECT:Laddad NE TRUE THEN RETURN.
      
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).

      
      
      
      RUN SparaKalkyl IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      
   END METHOD.

   METHOD PUBLIC VOID Rensa():
      DATASET KalkylDS:EMPTY-DATASET().
      THIS-OBJECT:KalkylNr = 0.
      THIS-OBJECT:Omrade = "".
      THIS-OBJECT:Laddad = FALSE.
      
   END METHOD.

END CLASS.