 
 /*------------------------------------------------------------------------
    File        : Sprakdb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue May 15 14:48:07 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Modules.Register.Sprakdb:
   /* Includes */
   {SPRAKTEMP.i}
   {dsSprak.i}
   /* Konstruktor & Destruktor */
    CONSTRUCTOR PUBLIC Sprakdb(INPUT r AS Guru.Root):
       THIS-OBJECT:Root = r.
       THIS-OBJECT:Connect().
       
       
     
    END CONSTRUCTOR.
    
    
   /* Variablar */
   DEFINE PUBLIC VARIABLE AppServerHandle AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
   DEFINE PUBLIC VARIABLE SprakTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE SprakStrangTT AS HANDLE NO-UNDO.
   /*
   DEFINE PUBLIC VARIABLE Tracker AS Helpers.PDSTracker NO-UNDO.
   */
   /* Metoder*/
   METHOD PRIVATE VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN SPRAKAPP.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN SPRAKAPP.p PERSISTENT SET AppServerHandle.
      END. 
   END.
  
   METHOD PUBLIC VOID FetchEverything():
      RUN FetchEverything IN AppServerHandle(OUTPUT DATASET SprakDS ).
      TEMP-TABLE spraktemp:TRACKING-CHANGES = TRUE.
      TEMP-TABLE sprakstrangtemp:TRACKING-CHANGES = TRUE. 
   END METHOD.
   
   {tracking.i SprakDS}
   
   METHOD PUBLIC VOID SaveAllChanges():
     
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET SprakDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET SprakDS:HANDLE).
      TEMP-TABLE spraktemp:TRACKING-CHANGES = FALSE .
      TEMP-TABLE sprakstrangtemp:TRACKING-CHANGES = FALSE.
      RUN SparaProDataSetSprakDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES (DATASET SprakDS:HANDLE).
      TEMP-TABLE spraktemp:TRACKING-CHANGES = TRUE.
      TEMP-TABLE sprakstrangtemp:TRACKING-CHANGES = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID GuruvarSet():
      EMPTY TEMP-TABLE esprakstrangtemp NO-ERROR. 
      CREATE esprakstrangtemp.
      BUFFER-COPY sprakstrangtemp TO esprakstrangtemp.
      RUN GuruvarSet IN AppServerHandle(INPUT TABLE esprakstrangtemp).
      EMPTY TEMP-TABLE esprakstrangtemp NO-ERROR. 
     
   END METHOD.
   
  

   METHOD PUBLIC VOID FetchTableHandles():
      THIS-OBJECT:SprakTT = TEMP-TABLE spraktemp:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:SprakStrangTT = TEMP-TABLE sprakstrangtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   END METHOD.
   
   METHOD PUBLIC VOID DeleteSprak(INPUT idvar AS INTEGER ):
      /*TAR BORT HELA SPRÅKET*/
      FOR EACH sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = idvar:
         DELETE sprakstrangtemp.
      END.
      SaveAllChanges().
   END METHOD.
   
   METHOD PUBLIC VOID DeleteStrang():
      /*tarbort en sträng*/
      
      SaveAllChanges().
   END METHOD.
   
   METHOD PUBLIC ROWID NewSprak(benamning AS CHARACTER, createconst AS LOGICAL):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      DEFINE VARIABLE idd AS INTEGER NO-UNDO.
      RUN NewSprak IN AppServerHandle(benamning, createconst, OUTPUT idd).
      THIS-OBJECT:FetchEverything().
      FIND FIRST spraktemp  WHERE spraktemp.ID = idd NO-LOCK NO-ERROR.
      returner = ROWID(spraktemp). 
      RETURN returner.
      /*
      RUN NewSprak IN AppServerHandle(benamning, createconst, OUTPUT TABLE espraktemp, OUTPUT TABLE esprakstrangtemp).
      
      FIND FIRST espraktemp WHERE NO-LOCK NO-ERROR.
      CREATE spraktemp.
      BUFFER-COPY espraktemp TO spraktemp.
      ASSIGN 
      spraktemp.TTROWID = ROWID(spraktemp)
      spraktemp.TTRECID = RECID(spraktemp).
      EMPTY TEMP-TABLE espraktemp NO-ERROR.
      FOR EACH esprakstrangtemp WHERE NO-LOCK:
         CREATE sprakstrangtemp.
         BUFFER-COPY esprakstrangtemp TO sprakstrangtemp.
         ASSIGN 
         sprakstrangtemp.TTROWID   = ROWID(sprakstrangtemp)
         sprakstrangtemp.TTRECID   = RECID(sprakstrangtemp). 
      END.
      EMPTY TEMP-TABLE esprakstrangtemp NO-ERROR. 
      SaveAllChanges().
       
      returner = ROWID(spraktemp). 
      RETURN returner. 
      */
   END METHOD.

   METHOD PUBLIC ROWID NewString(sprakid AS INTEGER, benamning AS CHARACTER, sokchar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      RUN NewString IN AppServerHandle(INPUT sprakid, INPUT benamning, INPUT sokchar, OUTPUT TABLE esprakstrangtemp).
      FIND FIRST esprakstrangtemp WHERE NO-LOCK NO-ERROR.
      CREATE sprakstrangtemp.
      BUFFER-COPY esprakstrangtemp TO sprakstrangtemp.
      ASSIGN 
      sprakstrangtemp.TTROWID   = ROWID(sprakstrangtemp)
      sprakstrangtemp.TTRECID   = RECID(sprakstrangtemp). 
      EMPTY TEMP-TABLE esprakstrangtemp NO-ERROR.
      SaveAllChanges().
      returner = ROWID(sprakstrangtemp). 
      RETURN returner.       
   END METHOD.

   METHOD PUBLIC VOID UpdateStringValue():
      SaveAllChanges().
   END METHOD.

END CLASS.