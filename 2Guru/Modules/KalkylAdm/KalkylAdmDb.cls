 
 /*------------------------------------------------------------------------
    File        : KalkylAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.KalkylAdm.KalkylAdmDb: 
   
   
   {KALKYLKAT.I}
   {KALKYLADMEXPIMPDATA.i}
   &Scoped-define PUBLIC PUBLIC
   {KALKYLKATH.i}
   {KALKYLKATPRODATA.i}
   {EKGEBREGURU.I}
   {EKGKAT.I}   
   {EKGFREK12.I}
   
   DEFINE TEMP-TABLE tidin
   FIELD ARBKOD                 AS CHARACTER
   FIELD LOPNR                  AS CHARACTER    
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER.

  
   DEFINE VARIABLE initDir                 AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO   
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY SkapApph AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY KalkylKodTot AS DECIMAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
   
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO.
   DEFINE PRIVATE VARIABLE Ebr-Data_P1P2Frekvensh       AS HANDLE NO-UNDO.  
   DEFINE PRIVATE VARIABLE Ebr-Data_P2P3Frekvensh       AS HANDLE NO-UNDO.
      
   CONSTRUCTOR PUBLIC KalkylAdmDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Ebr-Data_P1P2Frekvensh = TEMP-TABLE Ebr-Data_P1P2Frekvens:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:Ebr-Data_P2P3Frekvensh = TEMP-TABLE Ebr-Data_P2P3Frekvens:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:Root = r.
      initDir = SESSION:TEMP-DIRECTORY.
      THIS-OBJECT:Connect().
   END CONSTRUCTOR.
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      END. 
      IF Guru.Konstanter:appcon THEN DO:
         RUN SKAPKATEKGAPP.P PERSISTENT SET SkapApph ON Guru.Konstanter:apphand TRANSACTION DISTINCT .
      END.
      ELSE DO:
         RUN SKAPKATEKGAPP.P PERSISTENT SET SkapApph.
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID InitializeTTs():
      {KALKYLKATTTH.i}      
      {EKGFREK12D.I}      
   END METHOD.
   /*hämtar kataloger + priser ej koder*/
   METHOD PUBLIC LOGICAL FetchKatalog(INPUT katnr AS INTEGER):
      RUN LaddaKataloger IN AppServerHandle (INPUT katnr,OUTPUT DATASET KalkylKatDS, OUTPUT TABLE markfiltertt).
      
      THIS-OBJECT:KatalogTracking(TRUE).
      THIS-OBJECT:KalkylAdmDef().
      FOR EACH kalkylkatalogsubttbuf  NO-LOCK:
         THIS-OBJECT:DelKatSum().
         FIND FIRST kalkylkatalogttbuf WHERE kalkylkatalogttbuf.KLOGID = kalkylkatalogsubttbuf.KLOGID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylkatalogttbuf THEN DO:
            THIS-OBJECT:IngarKat().                       
         END.   
      END.
      FOR EACH kalkanvtt:
         Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + kalkanvtt.ANVANDARE + "'")  NO-ERROR.
         IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
            ASSIGN
            kalkanvtt.AV-LEVEL = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-LEVEL"):BUFFER-VALUE
            kalkanvtt.AV-NAMN = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.                                 
         END.                 
      END.
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID KalkylAdmDef():
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYLADM",Guru.GlobalaVariabler:GuruDefaultAnv,0,"").
   END METHOD.   
   /*hämtar bara arbetskoder */
   METHOD PUBLIC LOGICAL FetchKoder(INPUT KatSubIdvar AS INTEGER):
      RUN LaddaKatalogerKoder IN AppServerHandle (INPUT KatSubIdvar,OUTPUT DATASET KalkylKoderDS).
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = KatSubIdvar NO-LOCK NO-ERROR.
      
      
      THIS-OBJECT:KoderTracking(TRUE).
      RETURN TRUE.
   END METHOD.
   
   /*hämtar löpnr + subbar*/
    METHOD PUBLIC LOGICAL FetchLopar(INPUT KatSubIdvar AS INTEGER,arkodvar AS CHARACTER):
      RUN LaddaKatalogerLopar IN AppServerHandle (INPUT KatSubIdvar,INPUT arkodvar,OUTPUT DATASET KalkylLoparDS).
      THIS-OBJECT:LopnrTracking(TRUE).
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID FetchFrekvens(INPUT klsubid AS INTEGER, INPUT arbkodvar AS CHARACTE, INPUT lopnrvar AS INTEGER):
      RUN LaddaFrekvensHmt IN AppServerHandle (INPUT klsubid,INPUT arbkodvar, INPUT lopnrvar,OUTPUT DATASET KalkylFrekDS).
      THIS-OBJECT:FrekTracking(TRUE).             
   END METHOD.
   METHOD PUBLIC VOID MallarHmt(INPUT mallarvar AS INTEGER):
      RUN LaddaMallar IN AppServerHandle (INPUT mallarvar ,OUTPUT DATASET KalkylMallarDS).
      THIS-OBJECT:MallarTracking(TRUE).       
   END METHOD.
   
   /*nya NyMall*/
   METHOD PUBLIC VOID NyMall(OUTPUT nymallnr AS INTEGER):
      FIND LAST KalkmallHuvudtt USE-INDEX MALLNR NO-LOCK NO-ERROR.
      IF AVAILABLE KalkmallHuvudtt THEN nymallnr = KalkmallHuvudtt.MALLNR + 1.
      ELSE nymallnr = 1.
      CREATE KalkmallHuvudtt.
      ASSIGN 
      KalkmallHuvudtt.MALLNR = nymallnr
      KalkmallHuvudtt.TYP = 2
      KalkmallHuvudtt.BENAMNING = "< Ange Ny Benämning >"
      KalkmallHuvudtt.TTRECID = RECID(KalkmallHuvudtt).
      THIS-OBJECT:MallarSpara().                             
   END METHOD.
   
   METHOD PUBLIC VOID BortMallar(INPUT bmallnr AS INTEGER):
      FOR EACH KalkmallKodertt WHERE KalkmallKodertt.MALLNR = bmallnr NO-LOCK:
         DELETE KalkmallKodertt.
      END.
      THIS-OBJECT:MallarSpara().
   END METHOD.
   
   METHOD PUBLIC VOID CopyMallar(INPUT gmallnr AS INTEGER, OUTPUT nymallnr AS INTEGER):
      THIS-OBJECT:NyMall(OUTPUT nymallnr).
      FIND FIRST KalkmallHuvudttbuf WHERE KalkmallHuvudttbuf.MALLNR = gmallnr NO-LOCK NO-ERROR.  
      FIND FIRST KalkmallHuvudtt WHERE KalkmallHuvudtt.MALLNR = nymallnr NO-LOCK NO-ERROR.  
      ASSIGN 
      KalkmallHuvudtt.BENAMNING = "< Kopia > " + KalkmallHuvudttbuf.BENAMNING
      KalkmallHuvudtt.TYP = KalkmallHuvudttbuf.TYP.
      FOR EACH KalkmallKoderttbuf WHERE KalkmallKoderttbuf.MALLNR = gmallnr NO-LOCK:
         CREATE KalkmallKodertt.
         BUFFER-COPY KalkmallKoderttbuf TO KalkmallKodertt.
         ASSIGN 
         KalkmallKodertt.MALLNR = KalkmallHuvudtt.MALLNR 
         KalkmallKodertt.TTRECID = RECID(KalkmallKodertt).
      END.
      THIS-OBJECT:MallarSpara().
   END METHOD.
   METHOD PUBLIC VOID CreateKoppKatRows(INPUT katnr AS INTEGER):
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGID = 0 NO-ERROR.
      IF AVAILABLE kalkylkatalogsubtt THEN DO:
         ASSIGN kalkylkatalogsubtt.KLOGID = katnr.
         /*Anders Olsson Elpool i Umeå AB  14 okt 2014 11:53:43 
         Finns det en sen tidigare okopplad måste den bort. 
         */
         FIND FIRST kalkylkatalogsubttbuf WHERE kalkylkatalogsubttbuf.KLOGID = ? AND kalkylkatalogsubttbuf.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylkatalogsubttbuf THEN DO:
            DELETE kalkylkatalogsubttbuf.
         END.
         FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID  =  kalkylkatalogsubtt.KLOGID AND kalkylkatalogtt.HKLOGSUBID = 0 NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylkatalogtt THEN DO:
            kalkylkatalogtt.HKLOGSUBID = kalkylkatalogsubtt.KLOGSUBID.
         END.            
      END.            
      THIS-OBJECT:KatalogSpara().
   END METHOD.
   METHOD PUBLIC LOGICAL KollaKoppKatRows(INPUT katnr AS INTEGER,INPUT KatSubIdvar AS INTEGER):
      DEFINE VARIABLE orgkatsubid AS INTEGER NO-UNDO.
      DEFINE VARIABLE uppdatok AS LOGICAL NO-UNDO.
      
      FIND FIRST kalkylarbkodertt WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylarbkodertt THEN orgkatsubid = kalkylarbkodertt.KLOGSUBID.
      RUN LaddaKatalogerKoder IN AppServerHandle (INPUT KatSubIdvar,OUTPUT DATASET KalkylKoderDS).
      THIS-OBJECT:KoderTracking(FALSE).
      FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGID = katnr:
         RUN LaddaKatalogerKoder IN AppServerHandle (INPUT kalkylkatalogsubtt.KLOGSUBID,OUTPUT DATASET KalkylKoderDS APPEND).
      END.
      uppdatok = TRUE.
      kolla:
      FOR EACH kalkylarbkodertt WHERE kalkylarbkodertt.KLOGSUBID = KatSubIdvar NO-LOCK:
         FIND FIRST kalkylarbkoderttbuf WHERE kalkylarbkoderttbuf.ARBKOD = kalkylarbkodertt.ARBKOD AND kalkylarbkoderttbuf.KLOGSUBID NE kalkylarbkodertt.KLOGSUBID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylarbkoderttbuf THEN DO:
            uppdatok = FALSE.
            LEAVE kolla.
         END.   
      END.
      IF orgkatsubid NE 0 THEN DO: 
         RUN LaddaKatalogerKoder IN AppServerHandle (INPUT orgkatsubid,OUTPUT DATASET KalkylKoderDS).
         THIS-OBJECT:KoderTracking(TRUE).
      END.   
      RETURN uppdatok. 
   END METHOD.
   
   METHOD PUBLIC VOID KontrollKat(INPUT ksubidvar AS INTEGER):
      EMPTY TEMP-TABLE ekalkylkatalogsubtt NO-ERROR. 
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = ksubidvar NO-LOCK NO-ERROR.
      CREATE ekalkylkatalogsubtt.
      BUFFER-COPY kalkylkatalogsubtt TO ekalkylkatalogsubtt.
   END METHOD.
   METHOD PUBLIC VOID RemoveKopKat():
      FIND FIRST ekalkylkatalogsubtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = ekalkylkatalogsubtt.KLOGSUBID NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylkatalogsubtt THEN.
      ELSE DO:
         CREATE kalkylkatalogsubtt.
         BUFFER-COPY ekalkylkatalogsubtt TO kalkylkatalogsubtt.
         ASSIGN 
         kalkylkatalogsubtt.KLOGID = ?
         kalkylkatalogsubtt.TTRECID = RECID(kalkylkatalogsubtt).
      END.        
      EMPTY TEMP-TABLE ekalkylkatalogsubtt NO-ERROR.
      THIS-OBJECT:KatalogSpara().
   END METHOD.
   METHOD PUBLIC VOID CreateAnvRows():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkylAdmSekControl:GridKatSek:ActiveRow:Cells["KLOGID"]:TEXT).
      FOR EACH kalkanvtt WHERE kalkanvtt.KLOGID = 0:
         ASSIGN 
         kalkanvtt.KLOGID = hittnum.         
      END.  
      FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = hittnum NO-LOCK NO-ERROR. 
      kalkylkatalogtt.SEKRETESS = TRUE.
      THIS-OBJECT:KatalogSpara().
   END METHOD.
   METHOD PUBLIC VOID CreateKoderMallarRows():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkylAdmMallControl:GridMallar:ActiveRow:Cells["MALLNR"]:TEXT).
      FOR EACH KalkmallKodertt WHERE KalkmallKodertt.MALLNR = 0:
         ASSIGN 
         KalkmallKodertt.MALLNR = hittnum.         
      END.   
      THIS-OBJECT:MallarSpara().
   END METHOD.
   METHOD PUBLIC VOID RemoveAnvSelected():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkylAdmSekControl:GridKatSek:ActiveRow:Cells["KLOGID"]:TEXT).
      FIND FIRST kalkanvtt WHERE kalkanvtt.KLOGID = hittnum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkanvtt THEN DO:
         FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = hittnum NO-LOCK NO-ERROR. 
         kalkylkatalogtt.SEKRETESS = FALSE.
      END.
      THIS-OBJECT:KatalogSpara().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID AddAnvSelected():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkylAdmSekControl:GridKatSek:ActiveRow:Cells["KLOGID"]:TEXT).
      FIND FIRST kalkanvtt WHERE kalkanvtt.KLOGID = hittnum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkanvtt THEN DO:
         FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = hittnum NO-LOCK NO-ERROR. 
         kalkylkatalogtt.SEKRETESS = TRUE.
      END.
      THIS-OBJECT:KatalogSpara().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID RemoveKodMallarSelected():
      THIS-OBJECT:MallarSpara().
      RETURN.
   END METHOD.
   
   
   METHOD PUBLIC VOID RemoveKatdelar():
      FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK:
         DELETE kalkylprisertt.
      END.
      RUN BortKatdelar_UI IN AppServerHandle (INPUT kalkylkatalogsubtt.KLOGSUBID).
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID BortkatOk(INPUT katnr AS INTEGER,INPUT subkatnr AS INTEGER,OUTPUT okbort AS LOGICAL):
      RUN BortkatOk_UI IN AppServerHandle (INPUT katnr,INPUT subkatnr, OUTPUT okbort).
   END METHOD.
   METHOD PUBLIC VOID BortkatkollOk(INPUT katnr AS INTEGER,INPUT subkatnr AS INTEGER,OUTPUT okbort AS LOGICAL):
                                               /*0 om bort med subkat annars klogid*/
      RUN BortkatkollOk_UI IN AppServerHandle (INPUT katnr,INPUT subkatnr, OUTPUT okbort).
   END METHOD.
   
   METHOD PUBLIC VOID BortKatalog(INPUT katnr AS INTEGER):
      
      FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = katnr NO-LOCK NO-ERROR.
      FOR EACH ingkalkylkatalogtt WHERE ingkalkylkatalogtt.KLOGID = kalkylkatalogtt.KLOGID NO-LOCK:
         DELETE ingkalkylkatalogtt.
      END.
      FOR EACH kalkanvtt WHERE kalkanvtt.KLOGID = kalkylkatalogtt.KLOGID NO-LOCK:
         DELETE kalkanvtt.
      END.
      
      FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGID = kalkylkatalogtt.KLOGID:
        
         FOR EACH kalkyldelkatalogtt WHERE kalkyldelkatalogtt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK:
            DELETE kalkyldelkatalogtt.
         END.
         THIS-OBJECT:RemoveKatdelar().
         THIS-OBJECT:Deletefrek(kalkylkatalogsubtt.KLOGSUBID). 
         DELETE kalkylkatalogsubtt.
      END. 
   END METHOD.
   
   METHOD PUBLIC VOID BortSubKatalog(INPUT subkatnr AS INTEGER):
      FIND FIRST kalkyldelkatalogtt WHERE kalkyldelkatalogtt.KLOGSUBID = subkatnr NO-LOCK NO-ERROR.
      FOR EACH ingkalkylkatalogtt WHERE ingkalkylkatalogtt.KLOGSUBID = kalkyldelkatalogtt.KLOGSUBID NO-LOCK:
         DELETE ingkalkylkatalogtt.
      END.
      FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = kalkyldelkatalogtt.KLOGSUBID:
         THIS-OBJECT:RemoveKatdelar().
         DELETE kalkylkatalogsubtt. 
      END. 
      DELETE kalkyldelkatalogtt.
      THIS-OBJECT:Deletefrek(subkatnr).     
   END METHOD.
   
   METHOD PUBLIC VOID NyKatalog(OUTPUT nykatnr AS INTEGER):
      RUN sistakatid_UI IN AppServerHandle (OUTPUT nykatnr).
      /*KATALOGEN*/
      CREATE kalkylkatalogttbuf.
      ASSIGN
      kalkylkatalogttbuf.BENAMNING = "< Ny katalog >" 
      kalkylkatalogttbuf.INDATUM = TODAY
      kalkylkatalogttbuf.AVSLUTAD = FALSE
      kalkylkatalogttbuf.KLOGID  = nykatnr
      kalkylkatalogttbuf.VISARTAL = YEAR(TODAY)
      kalkylkatalogttbuf.TTRECID = RECID(kalkylkatalogttbuf). 
      THIS-OBJECT:KatalogSpara().   
   END METHOD.
   METHOD PUBLIC VOID CopyKatalog(INPUT katnr AS INTEGER, OUTPUT nykatnr AS INTEGER):
      DEFINE VARIABLE nysubkatnr AS INTEGER NO-UNDO.
      FOR EACH kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = katnr NO-LOCK:
         RUN sistakatid_UI IN AppServerHandle (OUTPUT nykatnr).
         /*KATALOGEN*/
         CREATE kalkylkatalogttbuf.
         BUFFER-COPY kalkylkatalogtt TO kalkylkatalogttbuf.
         ASSIGN
         kalkylkatalogttbuf.BENAMNING = "< Ny katalog >" + kalkylkatalogttbuf.BENAMNING
         kalkylkatalogttbuf.INDATUM = TODAY
         kalkylkatalogttbuf.AVSLUTAD = FALSE
         kalkylkatalogttbuf.KLOGID  = nykatnr
         kalkylkatalogttbuf.TTRECID = RECID(kalkylkatalogttbuf). 
         FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGID = kalkylkatalogtt.KLOGID NO-LOCK BY kalkylkatalogsubtt.KLOGID BY kalkylkatalogsubtt.KLOGSUBID:
            RUN sistakatsubid_UI IN AppServerHandle (OUTPUT nysubkatnr).
            /*DELKATALOGEN*/
            CREATE kalkylkatalogsubttbuf.
            BUFFER-COPY kalkylkatalogsubtt TO kalkylkatalogsubttbuf.
            ASSIGN
            kalkylkatalogsubttbuf.BENAMNING = "< Ny delkatalog >" + kalkylkatalogsubttbuf.BENAMNING
            kalkylkatalogsubttbuf.INDATUM = TODAY
            kalkylkatalogsubttbuf.AVSLUTAD = FALSE
            kalkylkatalogsubttbuf.KLOGID  = kalkylkatalogttbuf.KLOGID
            kalkylkatalogsubttbuf.KLOGSUBID  = nysubkatnr
            kalkylkatalogsubttbuf.UPPDATBAR = TRUE
            kalkylkatalogsubttbuf.TTRECID = RECID(kalkylkatalogsubttbuf).
            IF kalkylkatalogtt.HKLOGSUBID = kalkylkatalogsubtt.KLOGSUBID THEN kalkylkatalogttbuf.HKLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID.
            THIS-OBJECT:DelKatSum().
            THIS-OBJECT:DelKatPriser().
            THIS-OBJECT:IngarKat().
            THIS-OBJECT:DelKatfrek().
            RUN KopieraSubKatalogerKoder IN AppServerHandle (INPUT kalkylkatalogsubtt.KLOGSUBID, INPUT kalkylkatalogsubttbuf.KLOGSUBID).
            FOR EACH kalkanvtt WHERE kalkanvtt.KLOGID = kalkylkatalogtt.KLOGID:
               THIS-OBJECT:CopyAnvSek().            
            END.
            THIS-OBJECT:KatalogSpara().
         END.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID NyPris(INPUT KatSubIdvar AS INTEGER, OUTPUT nykpid AS INTEGER):
      DEFINE VARIABLE maskinvar AS CHARACTER NO-UNDO.
      RUN sistaprisid_UI IN AppServerHandle (INPUT KatSubIdvar,OUTPUT nykpid, OUTPUT maskinvar).
      CREATE kalkylprisertt.
      ASSIGN  
      kalkylprisertt.KPID = nykpid
      kalkylprisertt.KVID = 3
      kalkylprisertt.KLOGSUBID  = KatSubIdvar
      kalkylprisertt.BENAMNING = "< Ny pris >"
      kalkylprisertt.SOKBENAMNING = maskinvar
      kalkylprisertt.TTRECID = RECID(kalkylprisertt).
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = KatSubIdvar NO-LOCK NO-ERROR.
      
      THIS-OBJECT:FetchLopar(kalkylkatalogsubtt.KLOGSUBID,?).
      
      FOR EACH kalkylloppostertt WHERE kalkylloppostertt.KLOGSUBID = KatSubIdvar BREAK BY kalkylloppostertt.ARBKOD BY kalkylloppostertt.LOPNR:
         IF LAST-OF(kalkylloppostertt.LOPNR) THEN DO:
            CREATE kalkyllopsubtt.
            ASSIGN
            kalkyllopsubtt.KLOGSUBID = KatSubIdvar
            kalkyllopsubtt.ARBKOD = kalkylloppostertt.ARBKOD
            kalkyllopsubtt.LOPNR = kalkylloppostertt.LOPNR
            kalkyllopsubtt.KPID = nykpid
            kalkyllopsubtt.BENAMNING = kalkylprisertt.BENAMNING
            kalkyllopsubtt.TTRECID = RECID(kalkyllopsubtt).
         END.     
      END. 
      /*
      THIS-OBJECT:KatalogSpara().
      */   
      THIS-OBJECT:LopnrSpara().
      THIS-OBJECT:KatalogSpara().
      /*
      THIS-OBJECT:Control:ValueGridArbKoderCh(?,?).
        */         
   END METHOD.
   METHOD PUBLIC VOID NySubKatalog(OUTPUT nysubkatnr AS INTEGER):
      RUN sistakatsubid_UI IN AppServerHandle (OUTPUT nysubkatnr).
         /*DELKATALOGEN*/
      CREATE kalkylkatalogsubttbuf.
      ASSIGN
      kalkylkatalogsubttbuf.BENAMNING = "< Ny delkatalog >"
      kalkylkatalogsubttbuf.INDATUM = TODAY
      kalkylkatalogsubttbuf.AVSLUTAD = FALSE
      kalkylkatalogsubttbuf.KLOGID  = ?
      kalkylkatalogsubttbuf.KLOGSUBID  = nysubkatnr
      kalkylkatalogsubttbuf.UPPDATBAR = TRUE
      kalkylkatalogsubttbuf.TTRECID = RECID(kalkylkatalogsubttbuf).
      THIS-OBJECT:DelKatSum().
      FIND LAST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.BENAMNING BEGINS "EBR KLG1" USE-INDEX KLOGSUBID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogsubtt THEN DO:
         FIND LAST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.BENAMNING BEGINS "EBR" USE-INDEX KLOGSUBID NO-LOCK NO-ERROR.
      END.   
      IF AVAILABLE kalkylkatalogsubtt THEN DO:
         /*KATALOGPRISER*/
         THIS-OBJECT:DelKatPriser().
         FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID  = kalkylkatalogsubttbuf.KLOGSUBID NO-LOCK:
            IF kalkylprisertt.SOKBENAMNING BEGINS "MASKIN" THEN DO:
               IF kalkylprisertt.EGENKODUPP = FALSE AND kalkylprisertt.EGENPRISUPP = FALSE THEN DELETE kalkylprisertt.
            END.   
         END.         
      END.   
      THIS-OBJECT:KatalogSpara().           
   END METHOD.
   
   METHOD PUBLIC LOGICAL CopySubKatalog(INPUT subkatnr AS INTEGER, OUTPUT nysubkatnr AS INTEGER):
      DEFINE VARIABLE AA AS INTEGER NO-UNDO.
      DEFINE VARIABLE ebrmont AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ebrber AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ebrbervit AS DECIMAL NO-UNDO.   
      DEFINE VARIABLE ebrmask1 AS DECIMAL NO-UNDO.   
      DEFINE VARIABLE ebrmask2 AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ebrmask3 AS DECIMAL NO-UNDO.   
      DEFINE VARIABLE rorlig AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ebrmask1vit AS DECIMAL NO-UNDO.         
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE svarp AS CHARACTER  NO-UNDO.
      
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = subkatnr NO-LOCK NO-ERROR.
      RUN sistakatsubid_UI IN AppServerHandle (OUTPUT nysubkatnr).
            
      dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(244) ," ", System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).   
      IF dResult:ToString() = "Cancel" THEN RETURN FALSE.    
      /*skicka med om det är årets timpriser eller fjolårets timpriser som skall användas*/
      svarp = dResult:ToString().
      /*dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(186)," ", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      /*skicka med om timpriser skall vara avrundarde utan decimaler*/
      svar2 = dResult:ToString().*/
      IF svarp = "Yes" THEN   RUN aretspriserhmt_UI IN SkapApph (OUTPUT ebrmont, OUTPUT ebrber, OUTPUT ebrbervit, OUTPUT ebrmask1, OUTPUT ebrmask2, OUTPUT ebrmask3,OUTPUT rorlig, OUTPUT ebrmask1vit).
      /*DELKATALOGEN*/
      CREATE kalkylkatalogsubttbuf.
      BUFFER-COPY kalkylkatalogsubtt TO kalkylkatalogsubttbuf.
      ASSIGN
      kalkylkatalogsubttbuf.BENAMNING = "< Ny delkatalog >" + kalkylkatalogsubttbuf.BENAMNING
      kalkylkatalogsubttbuf.INDATUM = TODAY
      kalkylkatalogsubttbuf.AVSLUTAD = FALSE
      kalkylkatalogsubttbuf.KLOGID  = ?
      kalkylkatalogsubttbuf.KLOGSUBID  = nysubkatnr
      kalkylkatalogsubttbuf.UPPDATBAR = TRUE
      kalkylkatalogsubttbuf.TTRECID = RECID(kalkylkatalogsubttbuf).
      THIS-OBJECT:DelKatSum().
      /*Lena välj pris från i fjol eller i år*/      
      FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK:
         CREATE kalkylpriserttbuf.
         BUFFER-COPY kalkylprisertt TO kalkylpriserttbuf.
         ASSIGN  
         kalkylpriserttbuf.KLOGSUBID  = kalkylkatalogsubttbuf.KLOGSUBID
         kalkylpriserttbuf.TTRECID = RECID(kalkylpriserttbuf).
         IF svarp = "Yes" THEN DO:
            /*årets timpriser till kopierad katalog*/
            IF kalkylpriserttbuf.SOKBENAMNING = "MONTÖR" THEN kalkylpriserttbuf.PRIS = ebrmont.
            IF kalkylpriserttbuf.SOKBENAMNING = "BEREDARE" THEN kalkylpriserttbuf.PRIS = ebrber.
            IF kalkylpriserttbuf.SOKBENAMNING = "MASKIN1" THEN kalkylpriserttbuf.PRIS = ebrmask1.
            IF kalkylpriserttbuf.SOKBENAMNING = "MASKIN2" THEN kalkylpriserttbuf.PRIS = ebrmask2.
            IF kalkylpriserttbuf.SOKBENAMNING = "MASKIN3" THEN kalkylpriserttbuf.PRIS = ebrmask3.
            IF kalkylpriserttbuf.SOKBENAMNING = "BEREDARE REGION" THEN kalkylpriserttbuf.PRIS = ebrbervit.               
            IF kalkylpriserttbuf.SOKBENAMNING = "RÖRLIGKOSTNAD EA" THEN kalkylpriserttbuf.PRIS = rorlig.
         END.               
      END.
   
      THIS-OBJECT:DelKatfrek().
      
      dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(229) ," ", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).   
      IF dResult:ToString() = "No" THEN.    
      ELSE RUN KopieraSubKatalogerKoder IN AppServerHandle (INPUT kalkylkatalogsubtt.KLOGSUBID, INPUT kalkylkatalogsubttbuf.KLOGSUBID).
      THIS-OBJECT:KatalogSpara().  
      RETURN TRUE.                    
   END METHOD.
    
   /*nya koder*/
   METHOD PUBLIC VOID NyKoder(INPUT KatSubIdvar AS INTEGER, intyp AS INTEGER):
      DEFINE VARIABLE Markningvar AS CHARACTER NO-UNDO.
      /*IF intyp = 0 THEN intyp = 1.   ÄNDRAT till typ 2 default lena 20151111*/
      IF intyp = 0 THEN intyp = 2. 
      Markningvar = TRIM(THIS-OBJECT:ControlShell:GetVisaMarkning()).
      IF Markningvar = "Alla" OR Markningvar = "Ej märkta" THEN Markningvar = "".
      CREATE kalkylarbkodertt.
      ASSIGN 
      kalkylarbkodertt.ARBKOD = "< Ny kod >"
      kalkylarbkodertt.KLOGSUBID = KatSubIdvar
      kalkylarbkodertt.TYPKALK = intyp
      kalkylarbkodertt.MARKNING = Markningvar
      kalkylarbkodertt.BENAMNING = "< Ange Ny Benämning >"
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkoderttbuf).
      THIS-OBJECT:KoderSpara().                             
   END METHOD.
   /*kopiera kod*/
   METHOD PUBLIC VOID CopyKoder(INPUT arbkodvar AS CHARACTER):
      FOR EACH kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = arbkodvar NO-LOCK:
         CREATE kalkylarbkoderttbuf.
         BUFFER-COPY kalkylarbkodertt TO kalkylarbkoderttbuf.
         ASSIGN 
         kalkylarbkoderttbuf.ARBKOD = "< Ny kod >"
         kalkylarbkoderttbuf.TTRECID = RECID(kalkylarbkoderttbuf).
         
         FOR EACH kalkylloppostertt WHERE kalkylloppostertt.ARBKOD = arbkodvar NO-LOCK:
            CREATE kalkyllopposterttbuf.
            BUFFER-COPY kalkylloppostertt TO kalkyllopposterttbuf.
            ASSIGN 
            kalkyllopposterttbuf.ARBKOD = "< Ny kod >"
            kalkyllopposterttbuf.TTRECID = RECID(kalkyllopposterttbuf).  
         END.
         /*LÖPSUB*/
         FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.ARBKOD = arbkodvar NO-LOCK:
            CREATE kalkyllopsubttbuf.
            BUFFER-COPY kalkyllopsubtt TO kalkyllopsubttbuf.
            ASSIGN 
            kalkyllopsubttbuf.ARBKOD = "< Ny kod >"
            kalkyllopsubttbuf.TTRECID = RECID(kalkyllopsubtt).                
         END.
         THIS-OBJECT:LopnrSpara().
      END.
      THIS-OBJECT:KoderSpara().                             
   END METHOD.
   /*BortArbKod*/
   METHOD PUBLIC VOID BortArbKod(INPUT arbkodvar AS CHARACTER):
          /*
      FOR EACH kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = arbkodvar NO-LOCK:
         DELETE kalkylarbkodertt.
      END.
      */   
     FOR EACH kalkylloppostertt WHERE kalkylloppostertt.ARBKOD = arbkodvar NO-LOCK:
        DELETE kalkylloppostertt.
     END.
     FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.ARBKOD = arbkodvar NO-LOCK:
        DELETE kalkyllopsubtt.
     END.
     THIS-OBJECT:LopnrSpara().
     THIS-OBJECT:KoderSpara().                             
   END METHOD.
   
   METHOD PUBLIC VOID BortPris(INPUT KatSubIdvar AS INTEGER, INPUT nykpid AS INTEGER):
     THIS-OBJECT:FetchLopar(KatSubIdvar,?).
     
     FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = KatSubIdvar AND  kalkyllopsubtt.KPID = nykpid NO-LOCK:
        DELETE kalkyllopsubtt.
     END.
     THIS-OBJECT:LopnrSpara().
     /*
     THIS-OBJECT:Control:ValueGridArbKoderCh(?,?).
       */                            
   END METHOD.
   
   /*nya lopnr*/
   METHOD PUBLIC VOID NyLopnum(INPUT KatSubIdvar AS INTEGER, INPUT arbkodvar AS CHARACTER,OUTPUT nylopnr AS INTEGER):
     FIND LAST kalkylloppostertt USE-INDEX LOPNR NO-LOCK NO-ERROR.
     IF AVAILABLE kalkylloppostertt THEN nylopnr = kalkylloppostertt.LOPNR + 1.
     ELSE nylopnr = 10.
      /* INGA PRISER FÖR NYA*/
      /*
      FIND FIRST kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = KatSubIdvar USE-INDEX KLOGSUBID NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylkatalogsubtt THEN DO:
         FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID  = kalkylkatalogsubtt.KLOGSUBID NO-LOCK:
            IF kalkylprisertt.SOKBENAMNING BEGINS "MASKIN" AND kalkylprisertt.EGENKODUPP = FALSE AND kalkylprisertt.EGENPRISUPP = FALSE THEN. 
            ELSE IF                                            kalkylprisertt.EGENKODUPP = FALSE AND kalkylprisertt.EGENPRISUPP = FALSE THEN.
            ELSE DO:   
               CREATE kalkyllopsubttbuf.
               BUFFER-COPY kalkylprisertt TO kalkyllopsubttbuf.
               ASSIGN
               kalkyllopsubttbuf.KLOGSUBID = KatSubIdvar
               kalkyllopsubttbuf.ARBKOD = arbkodvar 
               kalkyllopsubttbuf.LOPNR = nylopnr
               kalkyllopsubttbuf.TIMMAR = 0
               kalkyllopsubttbuf.KOSTNAD = 0
               kalkyllopsubttbuf.TTRECID = RECID(kalkyllopsubttbuf).
            END.   
         END.
      END.
      */            
      CREATE kalkylloppostertt.
      ASSIGN 
      kalkylloppostertt.KLOGSUBID = KatSubIdvar
      kalkylloppostertt.ARBKOD = arbkodvar
      kalkylloppostertt.LOPNR = nylopnr
      kalkylloppostertt.BENAMNING = "< Ange Ny Benämning >"
      kalkylloppostertt.TYPKALK =  THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE
      kalkylloppostertt.TTRECID = RECID(kalkylloppostertt).
      THIS-OBJECT:LopnrSpara().                             
   END METHOD.
   
   /*ändra lopnr pris*/
   METHOD PUBLIC VOID AndLopnum(INPUT KatSubIdvar AS INTEGER, INPUT arbkodvar AS CHARACTER, INPUT beforekolvarde AS INTEGER,INPUT  nylopnr AS INTEGER):
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = KatSubIdvar AND kalkyllopsubtt.ARBKOD = arbkodvar  AND kalkyllopsubtt.LOPNR = beforekolvarde NO-LOCK:
         kalkyllopsubtt.LOPNR = nylopnr.
      END.
                                  
   END METHOD.
   
   /*kopiera löpnummer*/
   METHOD PUBLIC VOID CopyLopnum(INPUT lopnrvar AS INTEGER, OUTPUT nylopnr AS INTEGER):
      FIND LAST kalkylloppostertt USE-INDEX LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylloppostertt THEN nylopnr = kalkylloppostertt.LOPNR + 1.
      ELSE nylopnr = 10.
      FOR EACH kalkylloppostertt WHERE kalkylloppostertt.LOPNR = lopnrvar NO-LOCK:
         CREATE kalkyllopposterttbuf.
         BUFFER-COPY kalkylloppostertt TO kalkyllopposterttbuf.
         ASSIGN 
         kalkyllopposterttbuf.LOPNR = nylopnr
         kalkyllopposterttbuf.TTRECID = RECID(kalkyllopposterttbuf).  
      END.
      /*LÖPSUB*/
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.LOPNR = lopnrvar NO-LOCK:
         CREATE kalkyllopsubttbuf.
         BUFFER-COPY kalkyllopsubtt TO kalkyllopsubttbuf.
         ASSIGN 
         kalkyllopsubttbuf.LOPNR = nylopnr
         kalkyllopsubttbuf.TTRECID = RECID(kalkyllopsubtt).                
      END.
      THIS-OBJECT:LopnrSpara().
                                   
   END METHOD.
   /*Bort löpnummer*/
   METHOD PUBLIC VOID BortLopnum(INPUT lopnrvar AS INTEGER):
     FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.LOPNR = lopnrvar NO-LOCK:
        DELETE kalkyllopsubtt.
     END.
     THIS-OBJECT:LopnrSpara().                                 
   END METHOD.
   /*INGÅR I KATALOG*/  
   METHOD PRIVATE VOID IngarKat():
      CREATE ingkalkylkatalogtt.
      BUFFER-COPY kalkylkatalogttbuf TO ingkalkylkatalogtt.
      ASSIGN 
      ingkalkylkatalogtt.KLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID
      ingkalkylkatalogtt.TTRECID = RECID(ingkalkylkatalogtt).           
   END METHOD.
   /*SUMMERADE DELKATALOGER*/  
   
   METHOD PRIVATE VOID DelKatSum():
      FIND FIRST kalkyldelkatalogtt WHERE kalkyldelkatalogtt.KLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkyldelkatalogtt THEN DO:
         CREATE kalkyldelkatalogtt.
         BUFFER-COPY kalkylkatalogsubttbuf TO kalkyldelkatalogtt.
         kalkyldelkatalogtt.TTRECID = RECID(kalkyldelkatalogtt).
      END.           
   END METHOD.
   /*DelKATALOGPRISER*/
   METHOD PRIVATE VOID DelKatPriser():
      /*obs ej delete...*/
      FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK:
         CREATE kalkylpriserttbuf.
         BUFFER-COPY kalkylprisertt TO kalkylpriserttbuf.
         ASSIGN  
         kalkylpriserttbuf.KLOGSUBID  = kalkylkatalogsubttbuf.KLOGSUBID
         kalkylpriserttbuf.TTRECID = RECID(kalkylpriserttbuf).
      END.
   END METHOD.
     
    /*Kopy frekvens*/
   METHOD PRIVATE VOID DelKatfrek():
      RUN KopieraFrekKataloger IN AppServerHandle (INPUT kalkylkatalogsubtt.KLOGSUBID, INPUT kalkylkatalogsubttbuf.KLOGSUBID).
      
   END METHOD.  
   /*obs  delete...*/
   METHOD PRIVATE VOID Deletefrek(INPUT subkatid AS INTEGER):
      RUN BortKatFrek_UI IN AppServerHandle (INPUT subkatid).      
   END METHOD.    
    /*ARBKODER*/
   METHOD PRIVATE VOID CopyArbkod():
      CREATE kalkylarbkoderttbuf.
      BUFFER-COPY kalkylarbkodertt TO kalkylarbkoderttbuf.
      ASSIGN 
      kalkylarbkoderttbuf.KLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID
      kalkylarbkoderttbuf.TTRECID = RECID(kalkylarbkoderttbuf).      
   END METHOD.
   /*LOPNUMMER*/
   METHOD PRIVATE VOID CopyLopnr():
      CREATE kalkyllopposterttbuf.
      BUFFER-COPY kalkylloppostertt TO kalkyllopposterttbuf.
      ASSIGN 
      kalkyllopposterttbuf.KLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID
      kalkyllopposterttbuf.TTRECID = RECID(kalkyllopposterttbuf).           
   END METHOD.
   /*LOPNUMSUB*/
   METHOD PRIVATE VOID CopyLopnrSub():
      CREATE kalkyllopsubttbuf.
      BUFFER-COPY kalkyllopsubtt TO kalkyllopsubttbuf.
      ASSIGN 
      kalkyllopsubttbuf.KLOGSUBID = kalkylkatalogsubttbuf.KLOGSUBID
      kalkyllopsubttbuf.TTRECID = RECID(kalkyllopsubttbuf).            
   END METHOD.
   METHOD PUBLIC VOID AddPrisLopnrSub():
      DEFINE VARIABLE cc  AS RECID NO-UNDO.
      /*
      MESSAGE cc
      VIEW-AS ALERT-BOX.
      FIND FIRST kalkyllopsubtt  WHERE kalkyllopsubtt.TTRECID = Guru.GlobalaVariabler:TTRECID NO-LOCK NO-ERROR.
      MESSAGE 
      
      "ki" kalkyllopsubtt.KLOGSUBID 
      "a" kalkyllopsubtt.ARBKOD 
      "l" kalkyllopsubtt.LOPNR
      VIEW-AS ALERT-BOX.
      FIND FIRST kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID AND kalkyllopsubtt.ARBKOD = "" NO-LOCK NO-ERROR.
      */
     
      
     
      FIND FIRST kalkyllopsubtt  WHERE kalkyllopsubtt.TTRECID = Guru.GlobalaVariabler:TTRECID NO-LOCK NO-ERROR.
      ASSIGN 
      kalkyllopsubtt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID
      kalkyllopsubtt.ARBKOD = kalkylloppostertt.ARBKOD
      kalkyllopsubtt.LOPNR = kalkylloppostertt.LOPNR
      kalkyllopsubtt.TTRECID = RECID(kalkyllopsubtt).      
         
          
   END METHOD.
   
   METHOD PUBLIC VOID KodKostTotSetGet(INPUT arbkodvar AS CHARACTER,INPUT lopnrvar AS INTEGER, INPUT setget AS LOGICAL):
      FIND FIRST kalkyllopposterttbuf WHERE kalkyllopposterttbuf.ARBKOD = arbkodvar AND kalkyllopposterttbuf.LOPNR = lopnrvar NO-LOCK NO-ERROR.
      IF setget = FALSE THEN KalkylKodTot = kalkyllopposterttbuf.TKOST.
      ELSE kalkyllopposterttbuf.TKOST = KalkylKodTot.
                
   END METHOD.
    
   /*ANVÄNDARE*/
   METHOD PRIVATE VOID CopyAnvSek():
      CREATE kalkanvttbuf.
      BUFFER-COPY kalkanvtt TO kalkanvttbuf.
      ASSIGN 
      kalkanvttbuf.KLOGID  = kalkylkatalogttbuf.KLOGID
      kalkanvttbuf.TTRECID = RECID(kalkanvttbuf).             
   END METHOD.
   /*KOLL OM ARBKOD REDAN FINNS*/
   METHOD PUBLIC LOGICAL ArbKoderKoll(INPUT subkatnr AS INTEGER, INPUT arbkodvar AS CHARACTER, INPUT inttrec AS RECID):
      FIND FIRST kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = arbkodvar AND kalkylarbkodertt.TTRECID NE inttrec NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylarbkodertt THEN RETURN FALSE.  
      FOR EACH ingkalkylkatalogtt WHERE ingkalkylkatalogtt.KLOGSUBID = subkatnr NO-LOCK:
         FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGID = ingkalkylkatalogtt.KLOGID NO-LOCK:
            IF kalkylkatalogsubtt.KLOGSUBID = subkatnr THEN.
            ELSE DO:
               THIS-OBJECT:FetchKoder(kalkylkatalogsubtt.KLOGSUBID).
               FIND FIRST kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = arbkodvar NO-LOCK NO-ERROR.
               IF AVAILABLE kalkylarbkodertt THEN DO:
                  THIS-OBJECT:FetchKoder(subkatnr).
                  RETURN FALSE.
               END.     
            END.   
         END.
      END.
      THIS-OBJECT:FetchKoder(subkatnr).
      RETURN TRUE.
   END METHOD.
   /*fixa löpnummer efter ändring*/
   METHOD PUBLIC VOID ArbLopnrFix(INPUT beforekod AS CHARACTER, INPUT kodvar AS CHARACTER):
      FOR EACH kalkylloppostertt WHERE kalkylloppostertt.ARBKOD = beforekod NO-LOCK:
         kalkylloppostertt.ARBKOD = kodvar. 
      END.
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.ARBKOD = beforekod NO-LOCK:
         kalkyllopsubtt.ARBKOD = kodvar. 
      END.
      THIS-OBJECT:LopnrSpara().
      
      RETURN.
   END METHOD.
   
   
    /*SPARA katalog*/
   METHOD PUBLIC VOID KatalogSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:KatalogTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylKatDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylKatDS:HANDLE).
      RUN SparaProDataSetKalkylKatDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylKatDS:HANDLE).
      THIS-OBJECT:KatalogTracking(TRUE).              
      FIND FIRST kalkylkatalogtt  WHERE NO-LOCK NO-ERROR.            
   END METHOD.
    /*SPARA koder*/
   METHOD PUBLIC VOID KoderSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      DEFINE VARIABLE arbkodvar AS CHARACTER NO-UNDO.
      IF AVAILABLE kalkylarbkodertt THEN arbkodvar = kalkylarbkodertt.ARBKOD.
      THIS-OBJECT:KoderTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylKoderDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylKoderDS:HANDLE).
      RUN SparaProDataSetKalkylKoderDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylKoderDS:HANDLE).
      THIS-OBJECT:KoderTracking(TRUE).
      FIND FIRST kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = arbkodvar NO-LOCK NO-ERROR.            
   END METHOD.
   /*SPARA LOPNR + SUB*/
   METHOD PUBLIC VOID LopnrSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      DEFINE VARIABLE arbkodvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
      IF AVAILABLE kalkylloppostertt THEN DO:
         ASSIGN 
         arbkodvar = kalkylloppostertt.ARBKOD
         lopnrvar = kalkylloppostertt.LOPNR.
      END.   
      THIS-OBJECT:LopnrTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylLoparDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylLoparDS:HANDLE).
      RUN SparaProDataSetKalkylLoparDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylLoparDS:HANDLE).
      THIS-OBJECT:LopnrTracking(TRUE).              
      FIND FIRST kalkylloppostertt  WHERE kalkylloppostertt.ARBKOD = arbkodvar AND kalkylloppostertt.LOPNR = lopnrvar NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylloppostertt THEN DO: 
         FIND FIRST kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = kalkylloppostertt.KLOGSUBID AND kalkyllopsubtt.ARBKOD = kalkylloppostertt.ARBKOD AND 
         kalkyllopsubtt.LOPNR = kalkylloppostertt.LOPNR NO-LOCK NO-ERROR.
      END.             
   END METHOD.
   METHOD PUBLIC VOID FrekSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:FrekTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylFrekDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylFrekDS:HANDLE).
      RUN SparaProDataSetKalkylFrekDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylFrekDS:HANDLE).
      THIS-OBJECT:FrekTracking(TRUE).              
      FIND FIRST frekvenstemp  WHERE NO-LOCK NO-ERROR. 
                
   END METHOD.
   METHOD PUBLIC VOID MallarSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:MallarTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylMallarDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylMallarDS:HANDLE).
      RUN SparaProDataSetKalkylMallarDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylMallarDS:HANDLE).
      THIS-OBJECT:MallarTracking(TRUE).              
      FIND FIRST KalkmallHuvudtt  WHERE NO-LOCK NO-ERROR. 
                
   END METHOD.
   
   METHOD PUBLIC VOID KatalogTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE kalkylkatalogtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkylkatalogsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkylprisertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkvisningtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkanvtt:TRACKING-CHANGES = onoff.                
   END METHOD.    
   
   METHOD PUBLIC VOID KoderTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE kalkylarbkodertt:TRACKING-CHANGES = onoff.                         
   END METHOD.
   METHOD PUBLIC VOID LopnrTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE kalkylloppostertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkyllopsubtt:TRACKING-CHANGES = onoff.                    
   END METHOD. 
   METHOD PUBLIC VOID FrekTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE frekvenstemp:TRACKING-CHANGES = onoff.
                        
   END METHOD.
   
  /* METHOD PUBLIC VOID AdmExpImpTracking(INPUT onoff AS LOGICAL):      
      TEMP-TABLE eikalkylkatalogsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE eikalkylprisertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE eikalkylarbkodertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE eikalkylloppostertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE eikalkyllopsubtt:TRACKING-CHANGES = onoff.                  
   END METHOD.*/  
   
   METHOD PUBLIC VOID MallarTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE KalkmallHuvudtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE KalkmallKodertt:TRACKING-CHANGES = onoff.                  
   END METHOD.  
   
METHOD PUBLIC CHARACTER DirInit(INPUT cfile AS CHARACTER ):
   RETURN SUBSTRING(cFile,1,R-INDEX(cFile,"\") - 1).
END METHOD.   
 METHOD PUBLIC VOID inxmlkat(OUTPUT  inlasstart AS LOGICAL):
      Guru.Konstanter:AmericanSet().      
      DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE dialogVar               AS LOGICAL   NO-UNDO.
      
      THIS-OBJECT:rensa().      
      SYSTEM-DIALOG GET-FILE cFile
         TITLE "Välj den fil som du vill importera för att skapa kostnadskatalog KLG1!"
         FILTERS 'All Files (*.xml*)' '*.xml*'
         INITIAL-DIR initDir
         UPDATE dialogVar.
          
      IF dialogVar = TRUE THEN    DO: 
         initDir = DirInit(cFile).
         inlasstart = TRUE.
         ASSIGN
         cSourceType             = "FILE"
         cReadMode               = "MERGE"
         cSchemaLocation         = ?
         lOverrideDefaultMapping = FALSE.
         DATASET Ebr-DataDS:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).
         RUN startinxmlkat_UI IN SkapApph (INPUT DATASET Ebr-DataDS).
         
          
      END.
      THIS-OBJECT:rensa().
      SYSTEM-DIALOG GET-FILE cFile
         TITLE "Välj den fil som du vill importera för att skapa kostnadskatalog KLG2!"
         FILTERS 'All Files (*.xml*)' '*.xml*'
         INITIAL-DIR initDir
         UPDATE dialogVar.
            
      IF dialogVar = TRUE THEN    DO: 
         initDir = DirInit(cFile).
         inlasstart = TRUE.
         ASSIGN
         cSourceType             = "FILE"
         cReadMode               = "MERGE"
         cSchemaLocation         = ?
         lOverrideDefaultMapping = FALSE.
         DATASET Ebr-DataDS:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).
         RUN startinxmlkat_UI IN SkapApph (INPUT DATASET Ebr-DataDS).
          
      END.
      THIS-OBJECT:rensa(). 
       
      Guru.Konstanter:EuropeanAmericanReset().                 
END METHOD.



METHOD PUBLIC VOID inxmlfrek_UI(INPUT klogsubidvar AS INTEGER, OUTPUT  inlasstart AS LOGICAL):
   Guru.Konstanter:AmericanSet().   
   DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE dialogVar               AS LOGICAL   NO-UNDO.
   THIS-OBJECT:rensa().      
   SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil för frekvens P1P2!"
      FILTERS 'All Files (*.xml*)' '*.xml*'
      INITIAL-DIR initDir
      UPDATE dialogVar.
     
     
   IF dialogVar = TRUE THEN    DO:
      initDir = DirInit(cFile).
      inlasstart = TRUE. 
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      DATASET Ebr-DataP1P2DS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
      RUN startfrekP1P2_UI IN SkapApph (INPUT klogsubidvar , INPUT DATASET Ebr-DataP1P2DS).
      /*ekgp1frekvensTTh:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).
      RUN startfrekP1P2_UI IN SkapApph (INPUT klogsubidvar , INPUT ekgp1frekvensTTh).*/ 
      
   END.  
   THIS-OBJECT:rensa().
   SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil för frekvens P2P3!"
      FILTERS 'All Files (*.xml*)' '*.xml*'
      INITIAL-DIR initDir
      UPDATE dialogVar.
         
   IF dialogVar = TRUE THEN    DO:
      initDir = DirInit(cFile).
      inlasstart = TRUE. 
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      DATASET Ebr-DataP2P3DS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
      RUN startfrekP2P3_UI IN SkapApph (INPUT klogsubidvar , INPUT DATASET Ebr-DataP2P3DS).
      /*ekgp2frekvensTTh:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).
      RUN startfrekP2P3_UI IN SkapApph (INPUT klogsubidvar, INPUT ekgp2frekvensTTh).*/ 
   END.  
   THIS-OBJECT:rensa().
   Guru.Konstanter:EuropeanAmericanReset().
END METHOD.

METHOD PUBLIC VOID inP2komKLG1_UI(INPUT klogsubidvar AS INTEGER, OUTPUT  inlasstart AS LOGICAL):
   Guru.Konstanter:AmericanSet().   
   DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE dialogVar               AS LOGICAL   NO-UNDO.
   THIS-OBJECT:rensa().      
   SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil för P2 kommentarer KLG1!"
      FILTERS 'All Files (*.xml*)' '*.xml*'
      INITIAL-DIR initDir
      UPDATE dialogVar.       
         
    IF dialogVar = TRUE THEN  DO: 
      initDir = DirInit(cFile).
      inlasstart = TRUE.
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      
      DATASET Ebr-DataP2KomDS:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping). 
      RUN startkomP2XML_UI IN SkapApph (INPUT DATASET Ebr-DataP2KomDS, INPUT klogsubidvar ).      
   END.
   /*IF dialogVar = TRUE THEN    DO: 
      inlasstart = TRUE.
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      INPUT FROM VALUE(cFile) NO-ECHO.
      REPEAT:
      
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT DELIMITER ";" tidin   NO-ERROR.            
         END.               
      END.
      
      RUN startkomP2_UI IN SkapApph (INPUT klogsubidvar , input TABLE tidin).
      
   END.*/
   THIS-OBJECT:rensa().
   Guru.Konstanter:EuropeanAmericanReset().
   
END METHOD.

METHOD PUBLIC VOID inP2komKLG2_UI(INPUT klogsubidvar AS INTEGER, OUTPUT  inlasstart AS LOGICAL):
   Guru.Konstanter:AmericanSet().   
   DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cReadMode               AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cSchemaLocation         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE dialogVar               AS LOGICAL   NO-UNDO.
   THIS-OBJECT:rensa().      
   
   SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil för P2 kommentarer KLG2!"
      FILTERS 'All Files (*.xml*)' '*.xml*'
      INITIAL-DIR initDir
      UPDATE dialogVar.       
         
    IF dialogVar = TRUE THEN  DO: 
      initDir = DirInit(cFile).
      inlasstart = TRUE.
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      
      DATASET Ebr-DataP2KomDS:READ-XML (cSourceType, cFile, cReadMode,cSchemaLocation, lOverrideDefaultMapping).        
      RUN startkomP2XMLK2_UI IN SkapApph (INPUT DATASET Ebr-DataP2KomDS, INPUT klogsubidvar ).      
   END.
   
   /*SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil för P2 kommentarer KLG2!"
      FILTERS 'All Files (*.skv*)' '*.skv*'
      INITIAL-DIR initDir
      UPDATE dialogVar.
       
   IF dialogVar = TRUE THEN    DO: 
      inlasstart = TRUE.
      ASSIGN
      cSourceType             = "FILE"
      cReadMode               = "MERGE"
      cSchemaLocation         = ?
      lOverrideDefaultMapping = FALSE.
      INPUT FROM VALUE(cFile) NO-ECHO.
      REPEAT:
      
         DO TRANSACTION: 
            CREATE tidin.
            ASSIGN.
            IMPORT DELIMITER ";" tidin   NO-ERROR.            
         END.               
      END.
      RUN startkomP2K2_UI IN SkapApph (INPUT klogsubidvar , input TABLE tidin).
      
   END.*/
   THIS-OBJECT:rensa().
   Guru.Konstanter:EuropeanAmericanReset().
   
END METHOD.

METHOD PUBLIC VOID indelkatexcel_UI(INPUT klogsubidvar AS INTEGER, OUTPUT  inlasstart AS LOGICAL):
   Guru.Konstanter:AmericanSet().   
   DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
   DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
   DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
   DEFINE VARIABLE svar1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE svar2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE svar3 AS CHARACTER NO-UNDO.      
   dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(185) ," ", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).   
   /*skicka med om det är årets timpriser eller fjolårets timpriser som skall användas*/
   svar1 = dResult:ToString().
   dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(186)," ", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
   /*skicka med om timpriser skall vara avrundarde utan decimaler*/
   svar2 = dResult:ToString().
   dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(187)," ", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
   /*skicka med Arbetskod och löpnr i A -Ja, eller Arbetskod i A och löpnr i B-Nej*/
   svar3 = dResult:ToString().
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj den excelfil som Ni vill läsa in i vald katalog"
      FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
      INITIAL-DIR    fildir
      UPDATE OKvald.      
   IF OKvald = FALSE THEN RETURN.  
   inlasstart = TRUE. 
   tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
   IF svar3 = "Yes" THEN  THIS-OBJECT:ControlShell:ExcelImportLista(1,fildir).
   ELSE THIS-OBJECT:ControlShell:ExcelImportLista(2,fildir).
   /* gjord för fomrmatet ell ejk, euh eaf i filen:*/
   
   RUN startdelkat_UI IN SkapApph (INPUT klogsubidvar, INPUT TABLE KaladmimportTT, INPUT svar1,INPUT svar2).   
   
/*   THIS-OBJECT:ControlShell:FelMeddCheck().*/

   Guru.Konstanter:EuropeanAmericanReset().
   
END METHOD.


  
METHOD PUBLIC VOID rensa():     
   {Ebr-DataRensa.i}
    
   ekgp1frekvensTTh:EMPTY-TEMP-TABLE() NO-ERROR.
   ekgp2frekvensTTh:EMPTY-TEMP-TABLE() NO-ERROR.
   /*EMPTY TEMP-TABLE ekgp1frekvensTT NO-ERROR.
   EMPTY TEMP-TABLE ekgp2frekvensTT NO-ERROR.*/  
   
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   /*EMPTY TEMP-TABLE tidin2 NO-ERROR. 
   EMPTY TEMP-TABLE p2komtt NO-ERROR.*/ 
END METHOD.

METHOD PUBLIC VOID XmlKatExport(INPUT subkatnr AS INTEGER ):  
      DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lWriteBeforeImage   AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.

      cFile = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      FIND FIRST  kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = subkatnr   NO-LOCK NO-ERROR.    
      
      
      cFile = cFile  + STRING(subkatnr)  +  kalkylkatalogsubtt.BENAMNING + ".xml".
      
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = NO
      lMinSchema        = FALSE
      lWriteBeforeImage = FALSE.
      EMPTY TEMP-TABLE eikalkylkatalogsubtt  NO-ERROR.
      EMPTY TEMP-TABLE eikalkylprisertt NO-ERROR.
      EMPTY TEMP-TABLE eikalkylarbkodertt NO-ERROR.
      EMPTY TEMP-TABLE eikalkylloppostertt NO-ERROR.
      EMPTY TEMP-TABLE eikalkyllopsubtt NO-ERROR.            
      
      FOR EACH kalkylkatalogsubtt WHERE kalkylkatalogsubtt.KLOGSUBID = subkatnr NO-LOCK:
         CREATE eikalkylkatalogsubtt.
         BUFFER-COPY kalkylkatalogsubtt TO eikalkylkatalogsubtt.         
      END.      
      FOR EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = subkatnr NO-LOCK:
         CREATE eikalkylprisertt.
         BUFFER-COPY kalkylprisertt TO eikalkylprisertt.               
         eikalkylprisertt.RID = RECID(eikalkylkatalogsubtt).
      END.   
      FOR EACH kalkylarbkodertt WHERE kalkylarbkodertt.KLOGSUBID = subkatnr NO-LOCK:
         CREATE eikalkylarbkodertt.
         BUFFER-COPY kalkylarbkodertt TO eikalkylarbkodertt.
         eikalkylarbkodertt.RID = RECID(eikalkylkatalogsubtt).
      END.      
      
      THIS-OBJECT:FetchLopar(subkatnr,?).                
      FOR EACH kalkylloppostertt WHERE kalkylloppostertt.KLOGSUBID = subkatnr  NO-LOCK:
         CREATE eikalkylloppostertt.
         BUFFER-COPY kalkylloppostertt TO eikalkylloppostertt.
         eikalkylloppostertt.RID = RECID(eikalkylkatalogsubtt).
      END.   
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = subkatnr NO-LOCK:
         CREATE eikalkyllopsubtt.
         BUFFER-COPY kalkyllopsubtt TO eikalkyllopsubtt.
         eikalkyllopsubtt.RID = RECID(eikalkylkatalogsubtt).                        
      END.
      DATASET ExpImpsubDS:WRITE-XML (cTargetType, cFile, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema, lWriteBeforeImage).      
      dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(167) + cFile," ",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).                  
      FIND FIRST  kalkylkatalogtt  WHERE NO-LOCK NO-ERROR.
      FIND FIRST  kalkylkatalogsubtt  WHERE NO-LOCK NO-ERROR.
         
   END METHOD.

   
   METHOD PUBLIC LOGICAL XmlKatImport():
      DO ON ERROR UNDO, LEAVE:   
         DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
         DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
         DEFINE VARIABLE dialogVar AS LOGICAL NO-UNDO.
         DEFINE VARIABLE nysubkatnr AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE eikalkylkatalogsubtt  NO-ERROR.
         EMPTY TEMP-TABLE eikalkylprisertt NO-ERROR.
         EMPTY TEMP-TABLE eikalkylarbkodertt NO-ERROR.
         EMPTY TEMP-TABLE eikalkylloppostertt NO-ERROR.
         EMPTY TEMP-TABLE eikalkyllopsubtt NO-ERROR.
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN initDir = webclienttempdir.
         
         SYSTEM-DIALOG GET-FILE cFile
            TITLE "Välj den fil som du vill importera"
            FILTERS 'All Files (*.xml*)' '*.xml*'
            INITIAL-DIR initDir
            UPDATE dialogVar.
         IF dialogVar EQ TRUE THEN DO.
            
            ASSIGN
            cSourceType             = "FILE"
            cReadMode               = "MERGE"
            cSchemaLocation         = ?
            lOverrideDefaultMapping = FALSE.
            DATASET ExpImpsubDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
            
            RUN sistakatsubid_UI IN AppServerHandle (OUTPUT nysubkatnr).            
            FOR EACH eikalkylkatalogsubtt  NO-LOCK:
               CREATE kalkylkatalogsubtt.
               BUFFER-COPY eikalkylkatalogsubtt TO kalkylkatalogsubtt.
               ASSIGN
               kalkylkatalogsubtt.INDATUM = TODAY
               kalkylkatalogsubtt.AVSLUTAD = FALSE
               kalkylkatalogsubtt.KLOGID  = ?
               kalkylkatalogsubtt.KLOGSUBID  = nysubkatnr
               kalkylkatalogsubtt.UPPDATBAR = TRUE.                        
            END.      
            FOR EACH eikalkylprisertt  NO-LOCK:
               CREATE kalkylprisertt.
               BUFFER-COPY eikalkylprisertt TO kalkylprisertt.
               kalkylprisertt.KLOGSUBID  = nysubkatnr.                              
            END.   
            FOR EACH eikalkylarbkodertt  NO-LOCK:
               CREATE kalkylarbkodertt.
               BUFFER-COPY eikalkylarbkodertt TO kalkylarbkodertt.
               kalkylarbkodertt.KLOGSUBID  = nysubkatnr.
            END.                     
            FOR EACH eikalkylloppostertt  NO-LOCK:
               CREATE kalkylloppostertt.
               BUFFER-COPY eikalkylloppostertt TO kalkylloppostertt.
               kalkylloppostertt.KLOGSUBID  = nysubkatnr.
            END.   
            FOR EACH eikalkyllopsubtt NO-LOCK:
               CREATE kalkyllopsubtt.
               BUFFER-COPY eikalkyllopsubtt TO kalkyllopsubtt.
               kalkyllopsubtt.KLOGSUBID  = nysubkatnr.                                       
            END.
            
            FIND FIRST kalkyldelkatalogtt WHERE kalkyldelkatalogtt.KLOGSUBID = kalkylkatalogsubtt.KLOGSUBID NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kalkyldelkatalogtt THEN DO:
               CREATE kalkyldelkatalogtt.
               BUFFER-COPY kalkylkatalogsubtt TO kalkyldelkatalogtt.
               kalkyldelkatalogtt.TTRECID = RECID(kalkyldelkatalogtt).
            END.       
             
            THIS-OBJECT:KatalogSpara(). 
            THIS-OBJECT:KoderSpara(). 
            THIS-OBJECT:LopnrSpara(). 
            /* ccc
            save
            hämta igen
            tracking
            */
            RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.


   DESTRUCTOR KalkylAdmDb():
      IF VALID-HANDLE(THIS-OBJECT:SkapApph) THEN DELETE PROCEDURE THIS-OBJECT:SkapApph NO-ERROR.
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN avsluta_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
      END.   
   END DESTRUCTOR.
END CLASS.

  