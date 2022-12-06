 
 /*------------------------------------------------------------------------
    File        : DarwinPlusUserDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Apr 21 10:01:29 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

CLASS Modules.Storning.DarwinPlusUserDb: 
   DEFINE VARIABLE AppDarwinh AS HANDLE NO-UNDO.
   DEFINE VARIABLE AppDarwinhand AS HANDLE NO-UNDO.
   DEFINE VARIABLE appconDarwinh AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControlShell                 AS Modules.Storning.DarwinPlusUserShell NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControlDarwinPlusUserCon                AS Modules.Storning.DarwinPlusUserCon NO-UNDO.
   DEFINE PUBLIC VARIABLE AnvDpbuffextrah AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh2 AS HANDLE NO-UNDO.
   {VALDBTEMP.I} 
   
   DEFINE TEMP-TABLE felmeddtemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL     AS INTEGER.  
   &Scoped-define PUBLIC PUBLIC
   {DARWINPUSUSER.I}  
   DEFINE VARIABLE DarwinPlusUsersDS AS HANDLE NO-UNDO.
   
  
   
   CONSTRUCTOR PUBLIC DarwinPlusUserDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      
      
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID Connect():
      DEFINE VARIABLE AppSpringFel AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE AppDplusSpringSet AS CHARACTER NO-UNDO {AppServerInfoExtent.i} .
      DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.   
      {VALDBSEKG.I}
      {VALDBDARPLU.I}
      {VALDBELPADPLUS.I}
      AppDplusSpringSet = Guru.Konstanter:AppSpringSet.
     
      IF Guru.Konstanter:appcon THEN DO:
         FIND FIRST valdbtemp  WHERE valdbtemp.GFORETAG = "DARPLU" NO-LOCK NO-ERROR.
         AppDplusSpringSet[1]   = valdbtemp.GFORETAG.
         AppDplusSpringSet[12]   = valdbtemp.DBNAMN.    
         CREATE SERVER AppDarwinhand. 
         appconDarwinh = AppDarwinhand:CONNECT(valdbtemp.APPCON,{APPCON1.i},{APPCON2.i},valdbtemp.GFORETAG) NO-ERROR. 
         RUN AppDarwinPlusDbCon.P ON  AppDarwinhand TRANSACTION DISTINCT (INPUT AppDplusSpringSet ,INPUT 0, OUTPUT AppSpringFel).    
         RUN DarwinPlusUserDS.P PERSISTENT SET AppDarwinh ON AppDarwinhand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         MESSAGE  "EJ Guru.Konstanter:appcon" STRING(TIME, "HH:MM:SS")
         VIEW-AS ALERT-BOX.
        
         FIND FIRST valdbtemp  WHERE valdbtemp.GFORETAG = "ELPADARPLU" NO-LOCK NO-ERROR.
         AppDplusSpringSet[1]   = valdbtemp.GFORETAG.
         AppDplusSpringSet[12]   = valdbtemp.DBNAMN. 
         RUN AppDarwinPlusDbCon.P (INPUT AppDplusSpringSet,INPUT 0, OUTPUT AppSpringFel).
         RUN DarwinPlusUserDS.P PERSISTENT SET AppDarwinh.
         
      END.
      THIS-OBJECT:LasInData().
   END METHOD.

   
  
   METHOD PUBLIC VOID LasInData():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
     
      RUN LaddaUsers_UI IN AppDarwinh (OUTPUT DATASET-HANDLE DarwinPlusUsersDS BIND).
      IF VALID-HANDLE(DarwinPlusUsersDS ) THEN DO:
         AnvDpbuffh = DarwinPlusUsersDS:GET-BUFFER-HANDLE(1).
         AnvForDPbuffh = DarwinPlusUsersDS:GET-BUFFER-HANDLE(2).
         ForetagDPbuffh = DarwinPlusUsersDS:GET-BUFFER-HANDLE(3).
         OrganisationDPbuffh = DarwinPlusUsersDS:GET-BUFFER-HANDLE(4).
         
      END.
     
      CREATE BUFFER AnvDpbuffextrah FOR TABLE AnvDpbuffh.
            
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:OrganisationDPbuffh,"FOR EACH " + OrganisationDPbuffh:TABLE).      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvDpbuffh,"FOR EACH " + AnvDpbuffh:TABLE + " WHERE ORGID = " + STRING(THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE)).      
         qH2:GET-FIRST().
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            THIS-OBJECT:AnvDpbuffh:BUFFER-FIELD("NAMN"):BUFFER-VALUE = THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("NAMN"):BUFFER-VALUE.
            qH2:GET-NEXT().
         END.
         
         qH:GET-NEXT().
      END. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvForDPbuffh,"FOR EACH " + AnvForDPbuffh:TABLE).      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ForetagDPbuffh,"FOR EACH " + ForetagDPbuffh:TABLE + " WHERE IdentifikationsnrForetag = " + STRING(THIS-OBJECT:AnvForDPbuffh:BUFFER-FIELD("IdentifikationsnrForetag"):BUFFER-VALUE)).      
         qH2:GET-FIRST().
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            THIS-OBJECT:AnvForDPbuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE = THIS-OBJECT:ForetagDPbuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE.
            qH2:GET-NEXT().
         END.
         qH:GET-NEXT().
      END.
      THIS-OBJECT:DarwinPlusUsersTracking(TRUE).
      THIS-OBJECT:OrganisationDPbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:OrganisationDPbuffh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:OrganisationDPbuffh:BUFFER-CREATE().
         THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE = 1.
         THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("NAMN"):BUFFER-VALUE = "Allmän organisation".
         THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:OrganisationDPbuffh:RECID.
         
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
   END METHOD.
   METHOD PUBLIC VOID DarwinPlusUsersTracking(INPUT onoff AS LOGICAL):
      
      IF NOT VALID-HANDLE(AnvDptth) THEN AnvDptth = AnvDpbuffh:TABLE-HANDLE.
      IF NOT VALID-HANDLE(AnvForDPtth) THEN AnvForDPtth = AnvForDPbuffh:TABLE-HANDLE.
      IF NOT VALID-HANDLE(ForetagDPtth) THEN ForetagDPtth = ForetagDPbuffh:TABLE-HANDLE.
      IF NOT VALID-HANDLE(OrganisationDPtth) THEN OrganisationDPtth = OrganisationDPbuffh:TABLE-HANDLE.
      
      AnvDptth:TRACKING-CHANGES = onoff.
      AnvForDPtth:TRACKING-CHANGES = onoff.
      ForetagDPtth:TRACKING-CHANGES = onoff.
      OrganisationDPtth:TRACKING-CHANGES = onoff.
   END METHOD.
   METHOD PUBLIC VOID SparaDarwinUser():
      DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
       DEFINE VARIABLE afrid AS ROWID NO-UNDO.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvDpbuffh,"FOR EACH " + AnvDpbuffh:TABLE + " WHERE ANVANDARNAMN = '' or ANVANDARNAMN = 'Byt User!' or ANVANDARNAMN = 'elpao'").      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AnvDpbuffh:BUFFER-DELETE().
         qH:GET-NEXT().
      END.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvDpbuffh,"FOR EACH " + AnvDpbuffh:TABLE + " WHERE LOSEN = 'Byt Lösen!'").      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AnvDpbuffh:BUFFER-DELETE().
         qH:GET-NEXT().
      END.
      THIS-OBJECT:DarwinPlusUsersTracking(FALSE).
      CREATE DATASET chDS.
      chDS:CREATE-LIKE(DarwinPlusUsersDS).
      chDS:GET-CHANGES(DarwinPlusUsersDS).
      
      RUN SparaDynDSstart_UI IN AppDarwinh (INPUT "DarwinPlusUsersDS", INPUT DATASET-HANDLE chDS).
      chDS:MERGE-CHANGES(DarwinPlusUsersDS).
      
      /*måste hitta alla som jag hade tidigare.
      */
     
      THIS-OBJECT:ControlDarwinPlusUserCon:GridAnvFore:GuruReopen().

      IF THIS-OBJECT:AnvForDPbuffh:AVAILABLE THEN DO:
         afrid = AnvForDPbuffh:ROWID.
      END.
      ELSE afrid = ?.
      THIS-OBJECT:ControlDarwinPlusUserCon:GridAnv:GuruReopen().
      
      THIS-OBJECT:ControlDarwinPlusUserCon:GridAnvFore:GuruRadvaljare(afrid).
      THIS-OBJECT:ControlDarwinPlusUserCon:GridForetag:GuruReopen().
      THIS-OBJECT:ControlDarwinPlusUserCon:GridOrg:GuruReopen().  
   
      
      THIS-OBJECT:DarwinPlusUsersTracking(TRUE).
   END METHOD.
   METHOD PUBLIC VOID LosenDarwinUser(INPUT  usvar  AS CHARACTER,INPUT  losvar  AS CHARACTER):
      RUN restSkapaUserBytLosenord_UI IN AppDarwinh (INPUT usvar,INPUT losvar, OUTPUT TABLE felmeddtemp).
      
   END METHOD.
   METHOD PUBLIC VOID RemoveAnv(usvar  AS CHARACTER):
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvForDPbuffh,"FOR EACH " + AnvForDPbuffh:TABLE + " WHERE ANVANDARNAMN = " + QUOTER(usvar)).      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AnvForDPbuffh:BUFFER-DELETE().
         qH:GET-NEXT().
      END.
     
   END METHOD.
   METHOD PUBLIC VOID BortDarwinUser(INPUT  usvar  AS CHARACTER):
      RUN tabortDarwinUser_UI IN AppDarwinh (INPUT usvar,OUTPUT TABLE felmeddtemp ).
   END METHOD.
   METHOD PUBLIC ROWID SkapaOrg():
      DEFINE VARIABLE nrorg AS INTEGER NO-UNDO.
      MESSAGE "NYORG"
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:OrganisationDPbuffh:FIND-LAST("WHERE  " +  " USE-INDEX ORGID " ,NO-LOCK) NO-ERROR.
      nrorg = THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE + 1 .
      THIS-OBJECT:OrganisationDPbuffh:BUFFER-CREATE().
      THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE = nrorg.
      THIS-OBJECT:OrganisationDPbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:OrganisationDPbuffh:RECID.
      RETURN THIS-OBJECT:OrganisationDPbuffh:ROWID.
      
      
   END METHOD.
   METHOD PUBLIC VOID SkapaAnvForetag():
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AnvForDPbuffh,"FOR EACH " + AnvForDPbuffh:TABLE + " WHERE ANVANDARNAMN = ''").      
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AnvForDPbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = THIS-OBJECT:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE.
         qH:GET-NEXT().
      END.
     
   END METHOD.
   
   METHOD PUBLIC ROWID SkapaAnv():
        
      THIS-OBJECT:AnvDpbuffh:BUFFER-CREATE().
      THIS-OBJECT:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "Byt User!".
      THIS-OBJECT:AnvDpbuffh:BUFFER-FIELD("LOSEN"):BUFFER-VALUE = "Byt Lösen!".
      THIS-OBJECT:AnvDpbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:AnvDpbuffh:RECID.
      RETURN THIS-OBJECT:AnvDpbuffh:ROWID.
      
   END METHOD.
   METHOD PUBLIC VOID SkapaDarwinUser(INPUT  usvar  AS CHARACTER,INPUT  losvar  AS CHARACTER):
   /*   
       qString = "WHERE ANVANDARNAMN = '" + darwinAnv + "'".
   anvBuffer:FIND-FIRST(qString, NO-LOCK) NO-ERROR.
      
      RUN skapaDarwinUser_UI IN AppDarwinh (INPUT usvar,INPUT losvar, OUTPUT TABLE felmeddtemp).
     */ 
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      THIS-OBJECT:ControlDarwinPlusUserCon = THIS-OBJECT:ControlShell:ControlDarwinPlusUserCon.
   END METHOD.
  
   
   DESTRUCTOR DarwinPlusUserDb():
      
      IF VALID-HANDLE(THIS-OBJECT:AppDarwinh) THEN DO:
         RUN avslutarestUsr_UI IN AppDarwinh.
         DELETE PROCEDURE THIS-OBJECT:AppDarwinh NO-ERROR.
      END.
      IF CONNECTED ("DARPLU") THEN DISCONNECT "DARPLU" NO-ERROR.
      IF appconDarwinh THEN appconDarwinh = AppDarwinhand:DISCONNECT().
      DELETE OBJECT AppDarwinhand NO-ERROR.
      AppDarwinhand = ?.   
      AppDarwinh = ?.
   END DESTRUCTOR.
   
END CLASS.
