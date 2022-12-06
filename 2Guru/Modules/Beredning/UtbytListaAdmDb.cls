 
 /*------------------------------------------------------------------------
    File        : UtbytListaAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Aug 24 15:19:14 CEST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Modules.Beredning.UtbytListaAdmDb: 
   /*
  DEFINE TEMP-TABLE utbimportTT
   FIELD UID AS INTEGER
   FIELD ORGENR AS CHARACTER 
   FIELD ORGLEVKOD AS CHARACTER
   FIELD OLEVNAMN AS CHARACTER
   FIELD OBENAMNING AS CHARACTER
   
   FIELD BYTTILLENR AS CHARACTER
   FIELD BYTTILLLEVKOD AS CHARACTER   
   FIELD BLEVNAMN AS CHARACTER   
   FIELD BBENAMNING AS CHARACTER
   INDEX UID UID ORGENR ORGLEVKOD. 
   */
   
   
   DEFINE TEMP-TABLE SokutbTT
      FIELD ORGENR AS CHARACTER 
      FIELD BYTTILLENR AS CHARACTER
      FIELD RADNR AS INTEGER
      FIELD ROWID AS ROWID  
      INDEX ORADNR RADNR ORGENR  
      INDEX BRADNR RADNR BYTTILLENR.
  DEFINE VARIABLE Benrsok AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE Oenrsok AS CHARACTER NO-UNDO. 
  DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO   
     PUBLIC GET. PUBLIC SET.
  DEFINE VARIABLE UtbytlistorDS AS HANDLE NO-UNDO.
  DEFINE VARIABLE UtbytDS AS HANDLE NO-UNDO.
  DEFINE VARIABLE UtbytNamnTTh AS HANDLE NO-UNDO.
 
  DEFINE PUBLIC VARIABLE UtbytNamnbuffh AS HANDLE NO-UNDO.
  DEFINE VARIABLE UtbyttabTTh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE Utbyttabbuffh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE EnrtillTTh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE EnrfranTTh AS HANDLE NO-UNDO.
  
  DEFINE PUBLIC VARIABLE Enrtillbuffh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE Enrfranbuffh AS HANDLE NO-UNDO.
  
  DEFINE PUBLIC VARIABLE utbimportTTh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE utbimportbuffh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE tiduth         AS HANDLE NO-UNDO. 
  
  /*DEFINE VARIABLE utbimportTTh AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.*/
  
  DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.Beredning.UtbytListaAdm NO-UNDO.
  DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
  DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
 {TIDUTTT.I}
 
   
  CONSTRUCTOR PUBLIC UtbytListaAdmDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Connect().
  END CONSTRUCTOR.
  METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN UTBYTESLISTADS.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN UTBYTESLISTADS.P PERSISTENT SET AppServerHandle.
      END. 
      RUN StartUtbyte_UI IN AppServerHandle.      
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      CREATE TEMP-TABLE EnrfranTTh.
      EnrfranTTh:CREATE-LIKE(Guru.GlobalaVariabler:MaterielTTh:TABLE-HANDLE).
      EnrfranTTh:ADD-NEW-FIELD("TTRECID","RECID").   
      EnrfranTTh:TEMP-TABLE-PREPARE("EnrfranTT").
      Enrfranbuffh = EnrfranTTh:DEFAULT-BUFFER-HANDLE.
      
      CREATE TEMP-TABLE EnrtillTTh.
      EnrtillTTh:CREATE-LIKE(Guru.GlobalaVariabler:MaterielTTh:TABLE-HANDLE).
      EnrtillTTh:ADD-NEW-FIELD("TTRECID","RECID").   
      EnrtillTTh:TEMP-TABLE-PREPARE("EnrtillTT").
      Enrtillbuffh = EnrtillTTh:DEFAULT-BUFFER-HANDLE.
      
      Guru.Konstanter:LeverantorTTh:BUFFER-CREATE().
      Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = "ALLA".
      Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE = "Alla Leverantörer".
      Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = "0".
      
      THIS-OBJECT:UtbyListor().
      THIS-OBJECT:UtbyMtrl(0).
      tiduth = TEMP-TABLE tidut:HANDLE:DEFAULT-BUFFER-HANDLE.
   END METHOD.
   METHOD PUBLIC VOID UtbyListor():
      IF VALID-HANDLE(UtbytNamnbuffh) THEN UtbytNamnbuffh:EMPTY-TEMP-TABLE().
      RUN HamtalistaUtb_UI IN AppServerHandle (OUTPUT DATASET-HANDLE UtbytlistorDS APPEND BIND). 
      IF VALID-HANDLE(UtbytlistorDS) THEN DO:
         UtbytNamnbuffh = UtbytlistorDS:GET-BUFFER-HANDLE(1).         
         THIS-OBJECT:UListaTracking(TRUE).
      END.
   END METHOD.
   METHOD PUBLIC VOID UtbyMtrl(INPUT utbytid AS INTEGER):
      IF VALID-HANDLE(Utbyttabbuffh) THEN DO:
         Utbyttabbuffh:EMPTY-TEMP-TABLE().
      END.   
      RUN HamtaMtrlUtb_UI IN AppServerHandle (INPUT utbytid,OUTPUT DATASET-HANDLE UtbytDS APPEND BIND). 
      Oenrsok = "".
      Benrsok = "".
      EMPTY TEMP-TABLE SokutbTT NO-ERROR. 
      IF VALID-HANDLE(UtbytDS) THEN DO:
         Utbyttabbuffh = UtbytDS:GET-BUFFER-HANDLE(1).         
         
      END.
      IF NOT VALID-HANDLE(UtbyttabTTh) THEN UtbyttabTTh = Utbyttabbuffh:TABLE-HANDLE.
      RUN LaddMtrlev_UI IN AppServerHandle (INPUT-OUTPUT TABLE-HANDLE UtbyttabTTh).
      THIS-OBJECT:UtbMtrlracking(TRUE).
      
      
      THIS-OBJECT:Utbyttabbuffh:FIND-FIRST("WHERE BYTTILLLEVKOD = '' ",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:Utbyttabbuffh:AVAILABLE THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:Utbyttabbuffh:TABLE + " WHERE BYTTILLLEVKOD = '' ".
         qH = THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Utbyttabbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE = THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE.
            qH:GET-NEXT(). 
         END.  
         THIS-OBJECT:UtbMtrlSpara().
         THIS-OBJECT:UtbMtrlracking(FALSE).
         IF VALID-HANDLE(Utbyttabbuffh) THEN Utbyttabbuffh:EMPTY-TEMP-TABLE().
         RUN HamtaMtrlUtb_UI IN AppServerHandle (INPUT utbytid,OUTPUT DATASET-HANDLE UtbytDS APPEND BIND). 
      END.
      
      CREATE TEMP-TABLE utbimportTTh.
      utbimportTTh:CREATE-LIKE(Utbyttabbuffh:TABLE-HANDLE).
      utbimportTTh:TEMP-TABLE-PREPARE("UtbytImp").
      utbimportbuffh = utbimportTTh:DEFAULT-BUFFER-HANDLE.
        
      
   END METHOD.
   METHOD PUBLIC VOID UListaTracking(INPUT onoff AS LOGICAL):
      IF NOT VALID-HANDLE(UtbytNamnTTh) THEN UtbytNamnTTh = UtbytNamnbuffh:TABLE-HANDLE.            
      UtbytNamnTTh:TRACKING-CHANGES = onoff.            
   END METHOD.
   METHOD PUBLIC VOID UtbMtrlracking(INPUT onoff AS LOGICAL):
      IF NOT VALID-HANDLE(UtbyttabTTh) THEN UtbyttabTTh = Utbyttabbuffh:TABLE-HANDLE.            
      UtbyttabTTh:TRACKING-CHANGES = onoff.            
   END METHOD.
   METHOD PUBLIC VOID NyLista():
      
   END METHOD.
   METHOD PUBLIC VOID AndLista():
      
   END METHOD.
   METHOD PUBLIC VOID BoLista(INPUT uidvar AS INTEGER):
      THIS-OBJECT:UtbytNamnbuffh:FIND-FIRST("WHERE UID  = " + STRING(uidvar),NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:UtbytNamnbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:UtbytNamnbuffh:BUFFER-DELETE().
         queryvar =  "FOR EACH " + THIS-OBJECT:Utbyttabbuffh:TABLE + " WHERE UID = " + STRING(uidvar).
         qH = THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Utbyttabbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:Utbyttabbuffh:BUFFER-DELETE().
            qH:GET-NEXT(). 
         END.  
      END.
      THIS-OBJECT:UListorspara().
      THIS-OBJECT:UtbMtrlSpara().      
   END METHOD.
   METHOD PUBLIC VOID NyMtrlKoll (INPUT uidvar AS INTEGER, INPUT efran AS CHARACTER , INPUT levfran AS CHARACTER ,INPUT etill AS CHARACTER ,INPUT levtill AS CHARACTER, OUTPUT kolldubb AS LOGICAL ):
      DEFINE VARIABLE kollstr AS CHARACTER NO-UNDO.
      
      kollstr = "WHERE  UID =   " + STRING(uidvar)  + " AND ORGLEVKOD  = " + QUOTER(levfran)   + " AND ORGENR  = " + QUOTER(efran)
       /*+ " AND BYTTILLENR  = " + QUOTER(etill)*/   + " AND BYTTILLLEVKOD  = " + QUOTER(levtill) .
       /*  Kolla bara om orgenr orglev finns till byttilllevkod. Det får inte finnas en annan översättning till sammam leverantör.
       kollstr = "WHERE  UID =   " + STRING(uidvar)  + " AND ORGLEVKOD  = " + QUOTER(levfran)   + " AND ORGENR  = " + QUOTER(efran)
       + " AND BYTTILLENR  = " + QUOTER(etill)   + " AND BYTTILLLEVKOD  = " + QUOTER(levtill) .*/       
      /*MESSAGE "KO"  kollstr
      VIEW-AS ALERT-BOX.*/
      THIS-OBJECT:Utbyttabbuffh:FIND-FIRST( kollstr  ,NO-LOCK) NO-ERROR.      
       IF THIS-OBJECT:Utbyttabbuffh:AVAILABLE THEN DO:
          /*MESSAGE "T" THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE  THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE
          THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE
          VIEW-AS ALERT-BOX.*/                              
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:ControlShell:Root:LanguageManager:GetStringAsMessage(238)).         
         kolldubb = FALSE.         
       END.
       ELSE kolldubb = TRUE.
       
   END METHOD.
   METHOD PUBLIC ROWID NyMtrl(INPUT uidvar AS INTEGER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      DEFINE VARIABLE kolldubb AS LOGICAL NO-UNDO.
      
      THIS-OBJECT:NyMtrlKoll(INPUT uidvar , INPUT THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE, INPUT THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, 
      INPUT THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE, INPUT THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, OUTPUT kolldubb).   
      IF kolldubb = TRUE THEN DO:         
         THIS-OBJECT:Utbyttabbuffh:BUFFER-CREATE().
         returner = Utbyttabbuffh:ROWID.
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:Utbyttabbuffh:RECID.
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE =  uidvar. 
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE =  THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE. 
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE =  THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE. 
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("OBENAMNING"):BUFFER-VALUE = THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE. 
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("OPRIS"):BUFFER-VALUE = THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE.
          
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE = THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE. 
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE = THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.  
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BBENAMNING"):BUFFER-VALUE = THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BPRIS"):BUFFER-VALUE = THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE.
         /*
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("OLEVNAMN"):BUFFER-VALUE = STRING(THIS-OBJECT:ControlShell:comboFranLev:SelectedRow:GetCellVALUE("LEVNAMN")).
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BLEVNAMN"):BUFFER-VALUE = STRING(THIS-OBJECT:ControlShell:comboTillLev:SelectedRow:GetCellVALUE("LEVNAMN")).
         */
         
         Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(THIS-OBJECT:Enrfranbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("OLEVNAMN"):BUFFER-VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE.
         
         Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(THIS-OBJECT:Enrtillbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         THIS-OBJECT:Utbyttabbuffh:BUFFER-FIELD("BLEVNAMN"):BUFFER-VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE.
       
         THIS-OBJECT:UtbMtrlSpara().
         
      END.
      ELSE DO:
         returner = Utbyttabbuffh:ROWID.
      END.   
      RETURN returner.
   END METHOD.

   METHOD PUBLIC VOID BortMtrl():
      THIS-OBJECT:UtbMtrlSpara().
   END METHOD.
   
   METHOD PUBLIC VOID ImpUtbMtrlSpara():
      THIS-OBJECT:UtbMtrlSpara().
      IF NOT VALID-HANDLE(UtbyttabTTh) THEN UtbyttabTTh = Utbyttabbuffh:TABLE-HANDLE.
      RUN LaddMtrlev_UI IN AppServerHandle (INPUT-OUTPUT TABLE-HANDLE UtbyttabTTh).    
   END METHOD.
   
   
   METHOD PUBLIC VOID UtbMtrlSpara():
      DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
      THIS-OBJECT:UtbMtrlracking(FALSE).
      
       
      CREATE DATASET chDS.
      chDS:CREATE-LIKE(UtbytDS).
      chDS:GET-CHANGES(UtbytDS).
/*
       SPARAXML = "C:\CTest.xml". 
       UtbytlistorDS:WRITE-XML("FILE", SPARAXML).
*/
      RUN SparaDynDSstart_UI IN AppServerHandle (INPUT "UtbytDS", INPUT DATASET-HANDLE chDS).
      chDS:MERGE-CHANGES(UtbytDS).
      THIS-OBJECT:UtbMtrlracking(TRUE).
   END METHOD.
   
   
   METHOD PUBLIC VOID UListorspara():
      DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
      THIS-OBJECT:UListaTracking(FALSE).
      CREATE DATASET chDS.
      chDS:CREATE-LIKE(UtbytlistorDS).
      chDS:GET-CHANGES(UtbytlistorDS).
/*       SPARAXML = "C:\CTest.xml". 
       UtbytlistorDS:WRITE-XML("FILE", SPARAXML).
*/
      RUN SparaDynDSstart_UI IN AppServerHandle (INPUT "UtbytlistorDS", INPUT DATASET-HANDLE chDS).
      chDS:MERGE-CHANGES(UtbytlistorDS).
      THIS-OBJECT:UListaTracking(TRUE).
   END METHOD.
   
   
   METHOD PUBLIC STATIC VOID handleClickStatic( ):
      MESSAGE " Static fine working NuDB"
      VIEW-AS ALERT-BOX.
      
      RETURN.
   END METHOD.
   /*körs ej*/
   METHOD PUBLIC VOID SokEnrUtb( enrS AS CHARACTER,FranTill AS LOGICAL):
      IF FranTill = FALSE THEN DO:
         IF Oenrsok = enrS THEN DO:
            FIND NEXT SokutbTT WHERE SokutbTT.ORGENR BEGINS enrS NO-LOCK NO-ERROR.
            IF AVAILABLE SokutbTT THEN THIS-OBJECT:ControlShell:GridUtb:GuruRadvaljare(SokutbTT.ROWID).
            ELSE DO:
               FIND FIRST SokutbTT WHERE SokutbTT.ORGENR BEGINS enrS NO-LOCK NO-ERROR.
               IF AVAILABLE SokutbTT THEN THIS-OBJECT:ControlShell:GridUtb:GuruRadvaljare(SokutbTT.ROWID).
            END.    
         END. 
         ELSE DO:
            Oenrsok = "".
            EMPTY TEMP-TABLE SokutbTT NO-ERROR. 
         END.
               
      END.
      ELSE DO:
         IF Benrsok = enrS THEN DO:
            FIND NEXT SokutbTT WHERE SokutbTT.BYTTILLENR BEGINS enrS NO-LOCK NO-ERROR.
            IF AVAILABLE SokutbTT THEN THIS-OBJECT:ControlShell:GridUtb:GuruRadvaljare(SokutbTT.ROWID).
            ELSE DO:
               FIND FIRST SokutbTT WHERE SokutbTT.BYTTILLENR BEGINS enrS NO-LOCK NO-ERROR.
               IF AVAILABLE SokutbTT THEN THIS-OBJECT:ControlShell:GridUtb:GuruRadvaljare(SokutbTT.ROWID).
            END.    
         END. 
         ELSE DO:
            Benrsok = "".
            EMPTY TEMP-TABLE SokutbTT NO-ERROR. 
         END.
         
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID SokFranMtrl(enrS AS CHARACTER,benS AS CHARACTER,kundvar AS LOGICAL, levid AS CHARACTER):
      IF THIS-OBJECT:ControlShell:EfBfEnter = "Bf" THEN enrS = "".
      IF THIS-OBJECT:ControlShell:EfBfEnter = "Ef" THEN benS = "".
      IF LENGTH(TRIM(enrS + benS)) < 2 THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:ControlShell:Root:LanguageManager:GetStringAsMessage(207)).
         RETURN.
      END.
      THIS-OBJECT:ControlShell:GridEnrFran:GuruTom().
      IF levid = "Alla" THEN levid = "".
      
      THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:FetchMateriel(levid, enrS, benS, ?).
      IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:ControlShell:GridEnrFran:GuruOrderby("BY kund DESCENDING by enr").
      THIS-OBJECT:Enrfranbuffh:EMPTY-TEMP-TABLE().
      
      queryvar =  "FOR EACH " + Guru.GlobalaVariabler:MaterielTTh:TABLE.
      qH = THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:MaterielTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Enrfranbuffh:BUFFER-CREATE().
         THIS-OBJECT:Enrfranbuffh:BUFFER-COPY(Guru.GlobalaVariabler:MaterielTTh). 
         qH:GET-NEXT(). 
      END.  
      
      
      THIS-OBJECT:ControlShell:GridEnrFran:GuruReopen().
      THIS-OBJECT:ControlShell:GridEnrFran:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID SokTillMtrl(enrS AS CHARACTER,benS AS CHARACTER,kundvar AS LOGICAL, levid AS CHARACTER):
      IF THIS-OBJECT:ControlShell:EtBtEnter = "Bt" THEN enrS = "".
      IF THIS-OBJECT:ControlShell:EtBtEnter = "Et" THEN benS = "".
      IF LENGTH(TRIM(enrS + benS)) < 2 THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:ControlShell:Root:LanguageManager:GetStringAsMessage(207)).
         RETURN.
      END.
      THIS-OBJECT:ControlShell:GridEnrTill:GuruTom().
      IF levid = "Alla" THEN levid = "".
      
      THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:FetchMateriel(levid, enrS, benS, ?).
      IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:ControlShell:GridEnrTill:GuruOrderby("BY kund DESCENDING by enr").
      THIS-OBJECT:EnrTillbuffh:EMPTY-TEMP-TABLE().
      
      queryvar =  "FOR EACH " + Guru.GlobalaVariabler:MaterielTTh:TABLE.
      qH = THIS-OBJECT:ControlShell:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:MaterielTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:EnrTillbuffh:BUFFER-CREATE().
         THIS-OBJECT:EnrTillbuffh:BUFFER-COPY(Guru.GlobalaVariabler:MaterielTTh). 
         qH:GET-NEXT(). 
      END.  
      
      
      THIS-OBJECT:ControlShell:GridEnrTill:GuruReopen().
      THIS-OBJECT:ControlShell:GridEnrTill:GuruFirstrow().
   END METHOD.
   
   /*METHOD PUBLIC VOID ExcelImporters(filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE extrakol AS LOGICAL NO-UNDO.
      utbimportTTh = TEMP-TABLE utbimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      

      THIS-OBJECT:utbimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:ReadExcelStart(INPUT filnamn).
      extrakol = FALSE.
      DEBUGGER:SET-BREAK().            
      DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:KalkVisaControl:VisaExcel:iRadslut:
         invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
         IF invar = ? THEN.
         ELSE IF invar NE "" THEN DO:
            
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
               THIS-OBJECT:utbimportTTh:BUFFER-CREATE().
               THIS-OBJECT:utbimportTTh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.   
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:                                      
               THIS-OBJECT:utbimportTTh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
            END.
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
               THIS-OBJECT:utbimportTTh:BUFFER-FIELD("BYTTILLLEVKOD "):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D" THEN DO:                     
               THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.
         END.
               
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().                        
         IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A". 
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         END.                        
      END.  
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:FelMeddCheck().
   END METHOD.*/
   METHOD PUBLIC VOID TidutHmt(vad AS CHARACTER):
      CREATE tidut.
      tidut.UT = vad.  
      tidut.TTRECID = RECID(tidut).            
   END METHOD.
   
   DESTRUCTOR UtbytListaAdmDb():
      Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = 'ALLA'" ,NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO:
         Guru.Konstanter:LeverantorTTh:BUFFER-DELETE().  
      END.
            
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN AvsUtb_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
      END.   
   END DESTRUCTOR.
END CLASS.