 
 /*------------------------------------------------------------------------
    File        : StorningDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Apr 21 10:01:29 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

CLASS Modules.Storning.StorningDb: 
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControlShell                 AS Modules.Storning.StorningShell NO-UNDO. 
   DEFINE PUBLIC PROPERTY Storningstart AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.
   DEFINE PUBLIC VARIABLE Storfelmedtth                AS HANDLE NO-UNDO. 
   
   {StorFelMedtt.i}
   &Scoped-define PUBLIC PUBLIC
   {StorImpvar.I} 
   {StorImpDSFilinfofix.i}  
   DEFINE VARIABLE StorningDS AS HANDLE NO-UNDO.
   
   /*
   &Scoped-define PUBLIC PUBLIC
   {StorningDYNTABLEH.I}
   {StorningTT.I}
   {StorningTTH.i}
   */
   
   CONSTRUCTOR PUBLIC StorningDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().  
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN StorDSImp.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN StorDSImp.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      END.

   END METHOD.

   METHOD PUBLIC LOGICAL StorningStart():
     
      DEFINE VARIABLE ejstart AS LOGICAL NO-UNDO.
      DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
      Storningstart = TRUE.
      RETURN   Storningstart. 
   END METHOD.
   
   METHOD PUBLIC VOID laddaStorning():

   END METHOD.
   METHOD PUBLIC VOID LasInData():
      DEFINE VARIABLE StorningImportDSh AS HANDLE NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      FOR EACH Storfelmedtt WHERE Storfelmedtt.ORDNING = 0 OR Storfelmedtt.TABORT = TRUE NO-LOCK:
         IF Storfelmedtt.DATATABELL = "ForetagData" THEN DO:
            FOR EACH ForetagData WHERE ForetagData.IdentifikationsnrForetag = Storfelmedtt.IdentifikationsnrForetag NO-LOCK:
               FOR EACH DistriktData WHERE DistriktData.IdentifikationsnrForetag = ForetagData.IdentifikationsnrForetag NO-LOCK:
                  DELETE DistriktData.
               END.
               FOR EACH KundData WHERE KundData.IdentifikationsnrForetag = ForetagData.IdentifikationsnrForetag NO-LOCK:
                  DELETE KundData.
               END.
               FOR EACH LedningData WHERE LedningData.IdentifikationsnrForetag = ForetagData.IdentifikationsnrForetag NO-LOCK:
                  DELETE LedningData.
               END.
               FOR EACH StorningData WHERE StorningData.IdentifikationsnrForetag = ForetagData.IdentifikationsnrForetag NO-LOCK:
                  DELETE StorningData.
               END.
               DELETE ForetagData.
            END.
         END. 
         IF Storfelmedtt.DATATABELL = "DistriktData" THEN DO:
            FOR EACH DistriktData WHERE DistriktData.IdentifikationsnrForetag = Storfelmedtt.IdentifikationsnrForetag NO-LOCK:
               FOR EACH KundData WHERE KundData.IdentifikationsnrForetag = DistriktData.IdentifikationsnrForetag NO-LOCK:
                  DELETE KundData.
               END.
               FOR EACH LedningData WHERE LedningData.IdentifikationsnrForetag = DistriktData.IdentifikationsnrForetag NO-LOCK:
                  DELETE LedningData.
               END.
               FOR EACH StorningData WHERE StorningData.IdentifikationsnrForetag = DistriktData.IdentifikationsnrForetag NO-LOCK:
                  DELETE StorningData.
               END.
               DELETE DistriktData.
            END.
         END. 
         IF Storfelmedtt.DATATABELL = "KundData" THEN DO:
            FOR EACH KundData WHERE KundData.IdentifikationsnrForetag = Storfelmedtt.IdentifikationsnrForetag AND KundData.DistriktsId = Storfelmedtt.DistriktsId NO-LOCK:
               DELETE KundData.
            END.
         END.
         IF Storfelmedtt.DATATABELL = "LedningData" THEN DO:
            FOR EACH LedningData WHERE LedningData.IdentifikationsnrForetag = Storfelmedtt.IdentifikationsnrForetag AND LedningData.DistriktsId = Storfelmedtt.DistriktsId NO-LOCK:
               DELETE LedningData.
            END.
         END.     
         IF Storfelmedtt.DATATABELL = "StorningData" THEN DO:
            FOR EACH StorningData WHERE StorningData.IdentifikationsnrForetag = Storfelmedtt.IdentifikationsnrForetag AND StorningData.DistriktsId = Storfelmedtt.DistriktsId NO-LOCK:
               DELETE StorningData.
            END.
         END.    
      END.
      FIND FIRST Storfelmedtt WHERE Storfelmedtt.LAGGTILL = FALSE AND Storfelmedtt.SKRIVOVER = FALSE AND Storfelmedtt.TABORT = FALSE NO-LOCK NO-ERROR.
      IF AVAILABLE Storfelmedtt THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(219),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         IF THIS-OBJECT:ControlShell:ControlStorFel NE ? THEN DO:
            THIS-OBJECT:ControlShell:ControlStorFel:GridFelMedd:GuruReopen().
            THIS-OBJECT:ControlShell:ControlStorFel:GridFelMedd:GuruRepositionto(ROWID(Storfelmedtt)).
         END.
         RETURN.
      END.   
      FIND FIRST ForetagData WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE ForetagData THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
        
         RUN Import_UI IN AppServerHandle (INPUT DATASET StorningImportDs,INPUT TABLE Storfelmedtt).
         DATASET StorningImportDs:EMPTY-DATASET().
         EMPTY TEMP-TABLE Storfelmedtt NO-ERROR. 
         StorningImportDSh = DATASET StorningImportDS:HANDLE.
         THIS-OBJECT:ControlShell:ControlStorImp:GridStorImp:GridData(DATASET-HANDLE StorningImportDSh).
         IF THIS-OBJECT:ControlShell:ControlStorFel NE ? THEN DO:
            THIS-OBJECT:ControlShell:ControlStorFel:GridFelMedd:GuruReopen().
            
         END.
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
      END.   
      ELSE rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(218),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).  
   END METHOD.
   METHOD PUBLIC VOID KontrollData():
      DEFINE VARIABLE StorningImportDSh AS HANDLE NO-UNDO.
      RUN ImportKoll_UI IN AppServerHandle (INPUT-OUTPUT DATASET StorningImportDs,OUTPUT TABLE Storfelmedtt).
      StorningImportDSh = DATASET StorningImportDS:HANDLE.
      THIS-OBJECT:ControlShell:ControlStorImp:GridStorImp:GridData(DATASET-HANDLE StorningImportDSh).
      THIS-OBJECT:ControlShell:FelMeddCheck().
   END METHOD.
   METHOD PUBLIC LOGICAL ImporteraFil(path AS CHARACTER):
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE StorningImportDSh AS HANDLE NO-UNDO.
      DATASET StorningImportDs:EMPTY-DATASET().
      IF CAPS(SUBSTRING(path, LENGTH(path) - 3)) = ".XML" THEN DO:
         DATASET StorningImportDs:READ-XML("FILE", path, "EMPTY", ?, FALSE).
      END.
      ELSE IF CAPS(SUBSTRING(path, LENGTH(path) - 4)) = ".xlsx" THEN DO:
      
        THIS-OBJECT:InExcelFil(path).
       
      END.
      ELSE IF CAPS(SUBSTRING(path, LENGTH(path) - 4)) = "." THEN DO:
         DATASET StorningImportDs:READ-JSON("FILE", path, "EMPTY").
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(216),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
     
      StorningImportDSh = DATASET StorningImportDS:HANDLE.
      THIS-OBJECT:ControlShell:ControlStorImp:SkapaExportTTREC().
      THIS-OBJECT:ControlShell:ControlStorImp:GridStorImp:Text = "Importerad fil: " + path.
      THIS-OBJECT:ControlShell:ControlStorImp:GridStorImp:GridData(DATASET-HANDLE StorningImportDSh).
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      
      Storfelmedtth = TEMP-TABLE Storfelmedtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      ForetagDBbuffh = TEMP-TABLE ForetagData:HANDLE:DEFAULT-BUFFER-HANDLE.
      DistriktDBbuffh = TEMP-TABLE DistriktData:HANDLE:DEFAULT-BUFFER-HANDLE.
      KundDataDBbuffh = TEMP-TABLE KundData:HANDLE:DEFAULT-BUFFER-HANDLE.
      LedninggsDataDBbuffh = TEMP-TABLE LedningData:HANDLE:DEFAULT-BUFFER-HANDLE.
      StorningsDataDBbuffh = TEMP-TABLE StorningData:HANDLE:DEFAULT-BUFFER-HANDLE.
      StorFelMtrlDBbuffh = TEMP-TABLE StorFelMtrlData:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      
   END METHOD.
   
   METHOD PUBLIC VOID InExcelFil(path AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarA AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarB AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarC AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarD AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarE AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invarF AS CHARACTER NO-UNDO.
      DEFINE VARIABLE InnvarTot AS INTEGER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      
            
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      
     
      Guru.Konstanter:AmericanSet().
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      /*
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      */
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT path). 
      
      THIS-OBJECT:VisaExcel:SetBladvar(1).  
      THIS-OBJECT:VisaExcel:iRad = 2.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
            IF invar = ? OR invar = "" THEN DO:
               THIS-OBJECT:VisaExcel:iRad = THIS-OBJECT:VisaExcel:iRadslut + 1.
            END.
            ELSE IF THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut THEN DO:
               IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
                  CREATE ForetagData.
                  ForetagData.IdentifikationsnrForetag = INTEGER(invar).
               END.
            END.
         END.           
         IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
            ForetagData.Foretag = invar.
         END.         
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "C" THEN THIS-OBJECT:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().          
      END.  
      FOR EACH ForetagData WHERE ForetagData.IdentifikationsnrForetag = 0 OR ForetagData.IdentifikationsnrForetag = ? NO-LOCK:
         DELETE ForetagData.
      END.
      THIS-OBJECT:VisaExcel:SetBladvar(2).  
      THIS-OBJECT:VisaExcel:iRad = 2.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
            IF invar = ? OR invar = "" THEN DO:
               THIS-OBJECT:VisaExcel:iRad = THIS-OBJECT:VisaExcel:iRadslut + 1.
            END.
            ELSE  IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               CREATE DistriktData.
               DistriktData.IdentifikationsnrForetag = INTEGER(invar).
            END.
         END.       
         IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
            DistriktData.DistriktsId = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
            DistriktData.Artal = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
            DistriktData.DistriktsNamn = invar.
         END.         
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "E" THEN THIS-OBJECT:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.
      FOR EACH DistriktData WHERE DistriktData.IdentifikationsnrForetag = 0 OR DistriktData.IdentifikationsnrForetag = ? NO-LOCK:
         DELETE DistriktData.
      END.
      THIS-OBJECT:VisaExcel:SetBladvar(3).  
      THIS-OBJECT:VisaExcel:iRad = 2.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
             
            IF invar = ? OR invar = "" THEN DO:
               THIS-OBJECT:VisaExcel:iRad = THIS-OBJECT:VisaExcel:iRadslut + 1.
            END.
            ELSE IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               invarA = invar.
               FIND FIRST kunddatabuff WHERE kunddatabuff.IdentifikationsnrForetag = INTEGER(invarA) AND 
               kunddatabuff.DistriktsId = invarB AND
               kunddatabuff.Artal = INTEGER(invarC) AND
               kunddatabuff.SpanNivI = INTEGER(invarD) 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE kunddatabuff THEN DO:
                  CREATE KundData.
                  KundData.IdentifikationsnrForetag = INTEGER(invar).
               END.
               ELSE DO:
                  FIND FIRST KundData WHERE KundData.IdentifikationsnrForetag = INTEGER(invarA) AND 
                  KundData.DistriktsId = invarB AND
                  KundData.Artal = INTEGER(invarC) AND
                  KundData.SpanNivI = INTEGER(invarD) 
                  NO-LOCK NO-ERROR.
                  InnvarTot = kunddatabuff.AntalKunderTotalt.
                  RELEASE kunddatabuff.  
               END.      
            END.
         END.       
         IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
            invarB = invar.
            KundData.DistriktsId = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
            invarC = invar.
            KundData.Artal = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
            invarD = invar.
            KundData.SpanNivID = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
            KundData.AntalKunderTotalt = INTEGER(invar) + InnvarTot.
            ASSIGN
            InnvarTot = 0
            invarA = ""
            invarB = ""
            invarC = ""
            invarD = "".
         END.
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
            THIS-OBJECT:VisaExcel:cColname = "A".
            
         END.                    
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.
      FOR EACH KundData WHERE KundData.IdentifikationsnrForetag = 0 OR KundData.IdentifikationsnrForetag = ? NO-LOCK:
         DELETE KundData.
      END.
      THIS-OBJECT:VisaExcel:SetBladvar(4).  
      THIS-OBJECT:VisaExcel:iRad = 2.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
     
      
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
            IF invar = ? OR invar = "" THEN DO:
               THIS-OBJECT:VisaExcel:iRad = THIS-OBJECT:VisaExcel:iRadslut + 1.
            END.
            ELSE IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               invarA = invar.
               FIND FIRST leddatabuff WHERE leddatabuff.IdentifikationsnrForetag = INTEGER(invarA) AND 
               leddatabuff.DistriktsId = invarB AND
               leddatabuff.Artal = INTEGER(invarC) AND
               leddatabuff.SpanNivI = INTEGER(invarD) AND 
               leddatabuff.Text1 = invarE AND
               leddatabuff.Text2 = invarF
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE leddatabuff THEN DO:
                  CREATE LedningData.
                  LedningData.IdentifikationsnrForetag = INTEGER(invar).
               END.
               ELSE DO:
                  FIND FIRST LedningData WHERE LedningData.IdentifikationsnrForetag = INTEGER(invarA) AND 
                  LedningData.DistriktsId = invarB AND
                  LedningData.Artal = INTEGER(invarC) AND
                  LedningData.SpanNivI = INTEGER(invarD) 
                  NO-LOCK NO-ERROR.
                  InnvarTot = leddatabuff.AntalTotalt.
                  RELEASE leddatabuff.  
               END.
            END.         
         END.    
         IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
            invarB = invar.
            LedningData.DistriktsId = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
            invarC = invar.
            LedningData.Artal = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
            invarD = invar.
            LedningData.SpanNivID = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
            invarE = invar.
            LedningData.Text1 = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
            invarF = invar.
            LedningData.Text2 = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "G" THEN DO:
            LedningData.AntalTotalt = INTEGER(invar) + InnvarTot.
            ASSIGN
            InnvarTot = 0
            invarA = ""
            invarB = ""
            invarC = ""
            invarD = ""
            invarE = ""
            invarF = "".
            
            
         END.
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "H" THEN THIS-OBJECT:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.
       FOR EACH LedningData WHERE LedningData.IdentifikationsnrForetag = 0 OR LedningData.IdentifikationsnrForetag = ? NO-LOCK:
         DELETE LedningData.
      END.
      THIS-OBJECT:VisaExcel:SetBladvar(5).  
      THIS-OBJECT:VisaExcel:iRad = 2.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
     
      
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
            IF invar = ? OR invar = "" THEN DO:
               THIS-OBJECT:VisaExcel:iRad = THIS-OBJECT:VisaExcel:iRadslut + 1.
            END.
            ELSE IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               CREATE StorningData.
               StorningData.IdentifikationsnrForetag = INTEGER(invar).
            END.          
         END.    
         IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:
            StorningData.DistriktsId = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
            StorningData.StortypId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
            StorningData.StorningsDatum = DATE(INTEGER(SUBSTRING(invar,6,2)),INTEGER(SUBSTRING(invar,9,2)),INTEGER(SUBSTRING(invar,1,4))).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
             IF invar = "" THEN invar = "0".
            StorningData.StorningKlockan = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "F" THEN DO:
            StorningData.SlutDatum = DATE(INTEGER(SUBSTRING(invar,6,2)),INTEGER(SUBSTRING(invar,9,2)),INTEGER(SUBSTRING(invar,1,4))).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "G" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.SlutKlockan = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "H" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.KundAvbrottsTid = DECIMAL(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "I" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.AntalHSP = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "J" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.AntalLSP = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "K" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.BortKW = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "L" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.FranspanId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "M" THEN DO:
             IF invar = "" THEN invar = "0".
            StorningData.FelspanId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "N" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.BrytoId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "O" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.BrytorgLit = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "P" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.SektionerId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "Q" THEN DO:
            StorningData.SektionLit = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "R" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.UtlosId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "S" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.RelinId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "T" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.FelYId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "U" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.StDriftId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "V" THEN DO:
            StorningData.StDriftLit = invar.
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "W" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.AdelId = INTEGER(invar).
         END.
         IF THIS-OBJECT:VisaExcel:cColname = "X" THEN DO:
            IF invar = "" THEN invar = "0".
            StorningData.FelOId = INTEGER(invar).
         END.
         
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "Y" THEN THIS-OBJECT:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:VisaExcel:cColname = "A" THEN THIS-OBJECT:VisaExcel:Rowdown().
      END.   
         
 /*     
      THIS-OBJECT:VisaExcel:ReleaseExcel(TRUE).
 */
      THIS-OBJECT:VisaExcel:SlutReadExcel().
   
      Guru.Konstanter:EuropeanAmericanReset().
      
     /*
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      */ 
   END METHOD.
   
   DESTRUCTOR StorningDb():
      /*EMPTY TEMP-TABLE Storninghuvudtth  NO-ERROR.
      EMPTY TEMP-TABLE Storningaktivitettth  NO-ERROR.*/
     
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN avslutaStorning_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      END.
   
      AppServerHandle = ?.
   END DESTRUCTOR.
   
END CLASS.
