 
/*------------------------------------------------------------------------
   File        : KalkylShell.cls
   Purpose     : Make sure stuff works
   Syntax      : 
   Description : 
   Author(s)   : elpfh
   Created     : Fri Feb 10 12:57:57 CET 2012
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.

USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.
USING Microsoft.Office.Interop.Excel.*.


CLASS Modules.Kalkyl.KalkylShell INHERITS UserControl: 
   {KALKYLKATH.i}
   {KALKYLEXTRATAB.I}
   
   DEFINE PUBLIC VARIABLE KalkHuvudControl                 AS Modules.Kalkyl.KalkHuvudControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkControl                   AS Modules.Kalkyl.KalkylControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkEgnaControl               AS Modules.Kalkyl.KalkEgnaControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkFaktControl                AS Modules.Kalkyl.KalkFaktControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkMtrlControl                AS Modules.Kalkyl.KalkMtrlControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkImpMallControl                AS Modules.Kalkyl.KalkImpMallControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkAvtalControl                AS Modules.Kalkyl.KalkAvtalControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkKopKonvControl             AS Modules.Kalkyl.KalkKopKonvControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkTidlControl             AS Modules.Kalkyl.KalkTidlControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkFelmeddControl             AS Modules.Kalkyl.KalkFelmeddControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkVolymControl             AS Modules.Kalkyl.KalkVolymControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkVisaControl             AS Modules.Kalkyl.KalkVisaControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkStatusControl             AS Modules.Kalkyl.KalkStatusControl                                   NO-UNDO.
   
   DEFINE PUBLIC VARIABLE SlaIhopKalkyler AS LOGICAL NO-UNDO.
   DEFINE VARIABLE slaihoph AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE CellChvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE combolista  AS CHARACTER NO-UNDO EXTENT 10.
   DEFINE PRIVATE VARIABLE imageList1                   AS System.Windows.Forms.ImageList                                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE CurrentMatris                AS INTEGER                                                        NO-UNDO.
      
   DEFINE PRIVATE VARIABLE lastKalknumRowid             AS ROWID  NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelmedd                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKopiera                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabTidlage AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabStatus AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   /* Drag & Dropp*/
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   
   
   
   
   DEFINE PRIVATE VARIABLE TabAvtalskalkyl              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabEgna                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImportMallar              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFaktorer                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkyl                    AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylHuvud               AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVolymber                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMateriel                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager                   AS Infragistics.Win.UltraWinTabControl.UltraTabControl            NO-UNDO. 
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1  AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   
   /* Ribbongrupper */
   DEFINE         VARIABLE RibbonTabKalkyl              AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabKoder               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabEgnaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabMat                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisning             AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisningVisa         AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisningEgenskaper   AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   /* uppföljning*/
   DEFINE         VARIABLE RibbonTabUppfoljning        AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   
   /* Ribbonkontroller */
   /* Dummy, lägg denna där du vill fylla ut */
   DEFINE         VARIABLE RibbonDummy                  AS Infragistics.Win.UltraWinToolbars.LabelTool                    NO-UNDO.
   /* info */
   DEFINE PUBLIC  VARIABLE RibbonToolInfTyp             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolOmrade             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKat                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolMatris             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*
   DEFINE PUBLIC  VARIABLE RibbonToolFriGenerera        AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   */
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnm           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKatalogKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolAngeKodAntal       AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKodSokning         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /* visning */
   
   DEFINE PUBLIC  VARIABLE RibbonToolGenerera           AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
 
   DEFINE PUBLIC  VARIABLE RibbonToolSparaExcel            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUt            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUtBildval     AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisHuvud           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisAnmarkning      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisUtforlig        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaFaktorer    AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaPriser      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisFrekvens        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisKom             AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolVisVisa            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisArbKalk         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisSummera         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGruppera        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisPriser          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatSpec         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatris          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGroupMatris     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolManual             AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   
   /* uppfölj*/
   DEFINE PUBLIC  VARIABLE RibbonToolUppfPriser        AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
   
   /* Materiel */
   DEFINE PUBLIC  VARIABLE RibbonToolLev                AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnr                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolSok                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolEndastmarkmtrl     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
  
     
   /* Kalkyl */
   
   DEFINE PRIVATE VARIABLE KalkylLoaded                 AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC  VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   DEFINE PRIVATE VARIABLE PassedOnMatris               AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE NyKalkyl                     AS LOGICAL                                                        NO-UNDO INITIAL FALSE.
   DEFINE PUBLIC  VARIABLE cSenasteArbKod               AS CHARACTER                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE iSenasteLopNr                AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE iSenasteMatris               AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE cSenasteMtrl                 AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteMtrlMatris           AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE ExelKommando                 AS CHARACTER                                                      NO-UNDO.  
   DEFINE PUBLIC  VARIABLE WasExcelShown                AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE         VARIABLE vismulti                    AS LOGICAL NO-UNDO.
   DEFINE VARIABLE kalkylisinaktiv                     AS LOGICAL NO-UNDO.
   DEFINE VARIABLE friindummy AS LOGICAL NO-UNDO.
   DEFINE PUBLIC PROPERTY KalkNrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      

   CONSTRUCTOR PUBLIC KalkylShell ( INPUT r AS Guru.Root ):
      SUPER ().
     
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar kalkylobjekt..").
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InitializeTTs().
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Gör kopplingar till databasemanager..").
      {KALKYLKATDBH.i Root:DatabaseManager:Kalkyl}
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC KalkylShell ( ):
      /*
      DELETE OBJECT THIS-OBJECT:toolStripVisa.
      THIS-OBJECT:toolStripVisa = ?.   
      DELETE OBJECT THIS-OBJECT:TabManager.
      THIS-OBJECT:TabManager = ?.
      DELETE OBJECT THIS-OBJECT:KalkVisaControl:VisaExcel.
      THIS-OBJECT:KalkVisaControl:VisaExcel = ?.
      */
      DELETE OBJECT THIS-OBJECT:KalkHuvudControl NO-ERROR.
      DELETE OBJECT THIS-OBJECT:KalkControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkEgnaControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkFaktControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkMtrlControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkImpMallControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkAvtalControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkKopKonvControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkTidlControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkFelmeddControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkVolymControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkVisaControl NO-ERROR. 
      DELETE OBJECT THIS-OBJECT:KalkStatusControl NO-ERROR. 

      KalkHuvudControl = ?.
      KalkControl = ?. 
      KalkEgnaControl = ?. 
      KalkFaktControl = ?. 
      KalkMtrlControl = ?. 
      KalkImpMallControl = ?. 
      KalkAvtalControl = ?. 
      KalkKopKonvControl = ?. 
      KalkTidlControl = ?. 
      KalkFelmeddControl = ?. 
      KalkVolymControl = ?. 
      KalkVisaControl = ?. 
      KalkStatusControl = ?. 
   END DESTRUCTOR.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID fillinFriInLop_Load( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.
	  
   METHOD PUBLIC VOID GridVolymBerUpp(INPUT kolnamn  AS CHARACTER, kolvarde  AS CHARACTER):
      THIS-OBJECT:VolymberTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBer().
      THIS-OBJECT:KalkVolymControl:GridVolymBer:ActiveRow:Cells["TOTVOLYM"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:KalkVolymControl:GridVolymBer:ActiveRow:Cells["TILLAGGVOLYM"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:KalkVolymControl:GridVolymBer:ActiveRow:Cells["SKYDDFYLLNING"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE.
      THIS-OBJECT:KalkVolymControl:GridVolymBer:ActiveRow:Cells["YTBELAGGD"]:VALUE = THIS-OBJECT:VolymberTTh:BUFFER-FIELD("YTBELAGGD"):BUFFER-VALUE.
             
      RETURN.

   END METHOD. 
	
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC LOGICAL NewKalkyl():
      /* skapa kalkyl i databas */
      IF THIS-OBJECT:NewKalkStart() = FALSE THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL NewKalkyl(INPUT kalkdatah AS HANDLE):
      /* skapa kalkyl i databas */
     IF THIS-OBJECT:NewKalkStart() = FALSE THEN RETURN FALSE.
      
      kalkdatah:FIND-FIRST() NO-ERROR.
      IF kalkdatah:AVAILABLE THEN DO:
         IF kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = 0 OR kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = ? THEN. 
         ELSE THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE.
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text  = kalkdatah:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         /*v11.2 fix*/
         THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
         IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
            THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("VIBESTID"):BUFFER-VALUE.
         END.
         ELSE DO:
            IF STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = ? THEN THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE. 
            ELSE THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE.      
         END.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KopplatAoPnr(INPUT kalkdatah:BUFFER-FIELD("AONR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("DELNR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("PLANNR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("ARTAL"):BUFFER-VALUE).          
      END.      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL NewKalkStart():
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKalkyl() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      /* Hitta rätt katalog*/
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      /*v11.2 fix*/
      THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:SetIniComboV().
      THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
      THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:VALUE = 1.
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      THIS-OBJECT:LaddaLabels().
      THIS-OBJECT:NewKalkHide().
   END METHOD.
   METHOD PUBLIC VOID NewKalkHide():
      THIS-OBJECT:VisaTabs("ALLA", FALSE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY, TRUE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
      
      THIS-OBJECT:ModuleHandle:RibbonContext:Visible = FALSE.
      
      
   END METHOD.
   METHOD PUBLIC VOID StangaNy(INPUT sender AS System.Object , INPUT e AS System.Windows.Forms.FormClosingEventArgs):
    
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      IF THIS-OBJECT:NyKalkyl = TRUE THEN DO:     
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(65), THIS-OBJECT:Root:LanguageManager:GetString(72), System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).            
         IF rrr:ToString() EQ "Yes" THEN DO:
            IF THIS-OBJECT:ApplyCreatedKalkyl() EQ FALSE THEN  DO:
               e:Cancel = TRUE.
            END.
            ELSE THIS-OBJECT:SparaDefs(?,?). 
         END.      
         IF rrr:ToString() EQ "Cancel" THEN DO:
            e:Cancel = TRUE.
         END.   
         IF rrr:ToString() EQ "No" THEN DO:
         END.       
     END.    
     IF THIS-OBJECT:NyKalkyl = FALSE AND THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE THEN DO:
         IF THIS-OBJECT:SaveHuvud() = FALSE THEN DO:
            e:Cancel = TRUE.
         END.
      END.  
      IF e:Cancel = TRUE THEN.
      ELSE DO:
         THIS-OBJECT:BortBestKund().           
      END.   
   END METHOD.
   
   METHOD PRIVATE LOGICAL SaveHuvud():
      THIS-OBJECT:HuvudTTh:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text).
      /*
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE    =      STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE).
      */
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE       =      STRING(THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:BestKund:VALUE).
         
      IF THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE = FALSE THEN THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkHuvudControl:HuvudAnmarkning:Text).
      DEFINE VARIABLE ErrorText AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ErrorI AS INTEGER NO-UNDO.
      DEFINE VARIABLE IsError   AS LOGICAL   NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SaveHuvud(OUTPUT ErrorText, OUTPUT IsError).
      IF IsError = TRUE THEN DO:
         ErrorI = INTEGER(ErrorText).
         IF ErrorI EQ 6667 THEN 
            System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(66) + Guru.Konstanter:gomrk + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(67)).
         ELSE IF ErrorI EQ 7071 THEN 
            System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(70) + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(71)).
         ELSE
            System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(ErrorI), THIS-OBJECT:Root:LanguageManager:GetString(ErrorI)).
         
         IsError = FALSE.
         RETURN FALSE. 
      END.
      RETURN TRUE. 
   END METHOD.
   
   METHOD PUBLIC LOGICAL ApplyCreatedKalkyl():
      IF THIS-OBJECT:SaveHuvud() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      THIS-OBJECT:NyKalkyl = FALSE.
      ASSIGN 
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:VISIBLE       = TRUE.
      IF THIS-OBJECT:LoadKalkyl(INTEGER(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)),STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE), TRUE) = FALSE THEN RETURN FALSE.
      ELSE RETURN TRUE.
   END METHOD.
   

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonSkrivUt_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	
   METHOD PUBLIC LOGICAL VisaArendStatus(INPUT kalkdatah AS HANDLE,INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      kalkdatah:FIND-FIRST() NO-ERROR.
      IF kalkdatah:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:ArendeStatusHmt(kalkdatah).
         THIS-OBJECT:kalkttidlageTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkttidlageTTh.
         THIS-OBJECT:CreateGridStatus().
         THIS-OBJECT:KalkStatusControl:GridStatus:GuruReopen().
         RETURN TRUE.
      END.
      ELSE RETURN FALSE.
           
   END METHOD.
   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT KalkylimportTTinh AS HANDLE,INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      THIS-OBJECT:KalkylimportTTh = KalkylimportTTinh.
      THIS-OBJECT:LoadKalkyl(INPUT nr, INPUT omr, INPUT nydef).
      THIS-OBJECT:KalkylimportTTh:FIND-FIRST() NO-ERROR. 
      /*KOPIERAR KALKYL FRÅN BEREDNING*/
      IF THIS-OBJECT:KalkylimportTTh:AVAILABLE THEN DO:                   
         THIS-OBJECT:ImportLista().
         THIS-OBJECT:FelMeddCheck().
      END.
      
       RETURN TRUE.
   END METHOD.   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:Logger:WriteLine("Laddar kalkyl #" + STRING(nr) + " ifrån område " + omr + ".").
      THIS-OBJECT:KalkNrvar = nr.
      THIS-OBJECT:Omradevar = omr.
      /* Ladda från databas + DEFAULT*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchKalkyl(nr, omr) = FALSE THEN RETURN FALSE.
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
      IF nydef = FALSE THEN DO:
          /*SÄTTER DEFAULT VÄRDE VID OM INGA DEFULTER FINNS MEN KALK ÄR REDAN SKAPADNYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         THIS-OBJECT:GuruDefaultsTTh:FIND-FIRST() NO-ERROR. 
         IF THIS-OBJECT:GuruDefaultsTTh:AVAILABLE THEN.
         ELSE DO:
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 1 OR THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 2 OR 
            THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 3 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,
               INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE - 1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 5 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(3), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 7 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(4), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
         END.   
      END.   
      THIS-OBJECT:kalkaonrTTh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN DO:
         ASSIGN 
          Guru.GlobalaVariabler:plusaonr = ?
         Guru.GlobalaVariabler:plusdnr  = ?.
      END.
      ELSE DO:
         ASSIGN 
          Guru.GlobalaVariabler:plusaonr = THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE
         Guru.GlobalaVariabler:plusdnr  = THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("DELNR"):BUFFER-VALUE.
      END.   
      
      
      /* Spärra "skapany"-mode */
      ASSIGN
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:ENABLED = FALSE
      THIS-OBJECT:KalkHuvudControl:HuvudOmrade:ENABLED  = FALSE
      THIS-OBJECT:KalkHuvudControl:HuvudTyp:ENABLED     = FALSE.
   /* THIS-OBJECT:KalkHuvudControl:HuvudOmrade:ReadOnly = TRUE funkar inte 
   VVVVSätt alla ändringar på grids, Combos osv som behövs efter laddning*/
      
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      IF nydef = TRUE THEN DO:
         /*SÄTTER DEFAULT VÄRDE VID NYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
            "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
            THIS-OBJECT:MarkningTTh:FIND-FIRST("WHERE markfiltertt.TYP = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) NO-ERROR.
            THIS-OBJECT:RibbonToolFilterTyp:Text = THIS-OBJECT:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
            THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "FilterTyp",STRING(2), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         END. 
      END.
      THIS-OBJECT:ApplyLoadedKalkyl().
      THIS-OBJECT:KalkylLoaded = TRUE.
      
      /*VALUE PÅ DEFAULTVÄRDEN FÅR EFFEKT*/
      THIS-OBJECT:StartDefaulMT().
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID StartDefaulMT():
      THIS-OBJECT:FriInmatningKod(?,?).
      THIS-OBJECT:FriKalkChanged(?,?).
      THIS-OBJECT:FrekvensKomChanged(?,?).
      THIS-OBJECT:KatsKomChanged(?,?).
      
      THIS-OBJECT:KoderAnmChanged(?,?). 
      THIS-OBJECT:ValueArbetsKoderCh(?,?).
      THIS-OBJECT:FriInSet().
   END METHOD.
   
   METHOD PUBLIC VOID FriInEventSub():
      THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Enter:Subscribe(THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Fokus).
      THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:KeyDown:Subscribe(THIS-OBJECT:fillinFriInKodUp).           
      THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Fylld:Subscribe(THIS-OBJECT:fillinFriInKodFylld).         
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Fylld:Subscribe(THIS-OBJECT:fillinFriInLopFylld).         
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:KeyDown:Subscribe(THIS-OBJECT:fillinFriInLopUp).           
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Enter:Subscribe(THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Fokus).
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Enter:Subscribe(THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Fokus).
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KeyPress:Subscribe(THIS-OBJECT:fillinFriInAntalPress). 
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KeyUp:Subscribe(THIS-OBJECT:fillinFriInAntalUp).             
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Pil:Subscribe(THIS-OBJECT:fillinFriInAntalPil).    
   END METHOD.
   
   /*Robin Sjöberg Elpool i Umeå AB  2 apr 2014 11:11:21 
      Events för logik vid TGT-inmatning nedan 
   */
   
   METHOD PUBLIC VOID fillinFriInKodFylld():
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Select().
   END METHOD.
   
   METHOD PUBLIC VOID fillinFriInLopFylld():
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Select().
   END METHOD.
   
   METHOD PUBLIC VOID fillinFriInAntalPil():
        IF THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KnappKontroll = "Up" THEN DO:
            THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Focus().
            THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Select().
            THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:SelectAll().
            RETURN.
         END.
         IF THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KnappKontroll = "Down" THEN DO:
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Focus().
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Select().
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:SelectAll().
            RETURN.
         END.
   END METHOD.
   
   /*Styr tabbeteende för antal*/
   METHOD PUBLIC VOID fillinFriInAntalPress(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyPressEventArgs):
         IF THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KnappKontroll = "TAB" THEN DO:
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Focus().
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Select().
            THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:SelectAll().
            e:Handled = TRUE.
            RETURN.
         END.
   END METHOD.
   
   /*Styr tabbeteende för antal*/
   METHOD PUBLIC VOID FriInKod():
         KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         KalkylimportTTh:BUFFER-CREATE().
         THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
         THIS-OBJECT:ImportLista().
         KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
                 
   END METHOD.
   
   /*Styr enterbeteende för antal*/
   METHOD PUBLIC VOID fillinFriInAntalUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      IF THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KnappKontroll = "Return" THEN DO: 
         e:Handled = TRUE.
         THIS-OBJECT:FriInKod().
         THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Focus().
         THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:KnappKontroll = "".
         RETURN.
      END.  
   END METHOD.
   
   /*Styr enterbeteende för kod*/
   METHOD PUBLIC VOID fillinFriInKodUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      
      keykodestring = e:KEYCODE:ToString().

      IF keykodestring = "Return" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Select().   
      END.
      IF keykodestring = "Up" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Select().   
      END.   
      IF keykodestring = "Down" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Select().   
      END.       
   END METHOD.
   
   /*Styr enterbeteende för lop*/
   METHOD PUBLIC VOID fillinFriInLopUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      keykodestring = e:KEYCODE:ToString().
      IF keykodestring = "Return" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Select().
      END.
      IF keykodestring = "Up" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Select().   
      END.   
      IF keykodestring = "Down" THEN DO:
         THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Select().   
      END.       
   END METHOD.

   METHOD PUBLIC VOID FriInSet():
      IF THIS-OBJECT:GetVisaTyp() = 1 THEN DO:
         IF THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text BEGINS "G" THEN.
         ELSE THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text = "G".
         IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Ej märkta" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAAA").
         ELSE IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Region" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAA").
         ELSE THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAAA").
      END.   
      ELSE IF THIS-OBJECT:GetVisaTyp() = 2 THEN DO:
         IF THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text BEGINS "G" THEN THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text = "".
         IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Ej märkta" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAA").
         ELSE IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Region" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AA").
         ELSE THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAA").
      END.
      ELSE IF THIS-OBJECT:GetVisaTyp() = 3 THEN DO:
         IF THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text BEGINS "G" THEN THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text = "".
         IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Ej märkta" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAA").
         ELSE IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Region" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AA").
         ELSE THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAA").
      END.
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:ValjMask("##").
   END METHOD.
   
   METHOD PUBLIC LOGICAL VisaKalkylLoad(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      /* Ladda från databas + DEFAULT*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchVisaKalkyl(extravaldfastth) = FALSE THEN RETURN FALSE.
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
           
      THIS-OBJECT:KalkylLoaded = TRUE.
      
      /*VALUE PÅ DEFAULTVÄRDEN FÅR EFFEKT*/
      THIS-OBJECT:StartDefaulMT().
      THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)).
      THIS-OBJECT:ApplyLoadedVisaKalkyl().
      
      THIS-OBJECT:VisaTabs("Alla", FALSE).
      THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = TRUE.
      
      THIS-OBJECT:VisningKalkIniMulti().
      queryvar =  "FOR EACH " + extravaldfastth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(extravaldfastth,queryvar).
      /*
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetNumResult(qH) > 1 THEN vismulti = TRUE.
      ELSE vismulti = FALSE.
      */
      
      qH:GET-FIRST().
      rid = extravaldfastth:ROWID. 
      qH:GET-LAST().
      IF rid = extravaldfastth:ROWID THEN vismulti = FALSE.
      ELSE vismulti = TRUE.
     
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL SlaIhopKalkylLoad(INPUT kalkdatah AS HANDLE):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      slaihoph = kalkdatah.
      SlaIhopKalkyler = TRUE. 
      IF THIS-OBJECT:NewKalkyl(INPUT kalkdatah) = FALSE THEN RETURN FALSE. 
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID SlaIhopKalkyler():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      SlaIhopKalkyler = FALSE.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SlaIhopKalkyler(INPUT slaihoph).
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:FelMeddCheck().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(188), THIS-OBJECT:Root:LanguageManager:GetString(189), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:WasExcelShown = FALSE.
   END METHOD. 
   METHOD PUBLIC VOID ApplyLoadedVisaKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:HuvudTTh:FIND-FIRST() NO-ERROR.
            
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      matrisname = THIS-OBJECT:GetMatrisName().
      
      THIS-OBJECT:LaddaLabels().
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName().
      
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 2").
      IF HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE NE 7 THEN DO:
         maxmatris = 100.
         IF Guru.Konstanter:globforetag = "GRAN" THEN maxmatris = 11.
         imatris = 3.
         DO WHILE imatris < maxmatris:
            THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " " + STRING(imatris)).
            imatris = imatris + 1.
         END.   
      END.
      
      THIS-OBJECT:RibbonToolMatris:VALUE = THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolMatris:SelectedIndex = 0.
      THIS-OBJECT:CurrentMatris = -1.
      THIS-OBJECT:RibbonToolVisMatris:ValueList = THIS-OBJECT:RibbonToolMatris:ValueList:Clone().
      THIS-OBJECT:RibbonToolVisMatris:VALUE = THIS-OBJECT:RibbonToolMatris:VALUE.
      /* Visa  */
      THIS-OBJECT:UpdateRibbonShow().
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      
      THIS-OBJECT:KalkFaktControl:GridPriser:GuruReopen().
      THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruReopen().
      THIS-OBJECT:KalkTidlControl:GridTidlage:GuruReopen().
      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:GomaRibbon().
      THIS-OBJECT:ForetagAnpass().
      THIS-OBJECT:TabManager:Tabs["Status"]:VISIBLE   = FALSE.
       
      THIS-OBJECT:Root:WindowManager:Wnd:FormClosing:Subscribe(THIS-OBJECT:StangaNy).
      ASSIGN 
      
      THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE     = FALSE.
   END METHOD.
   /* Fullständiggör formen när kalkyl laddats*/
   METHOD PUBLIC VOID ApplyLoadedKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonTabKalkyl:Caption = Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
      THIS-OBJECT:RibbonTabKoder:Caption = "Arbeta med Koder för " + Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      /*
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFiltrera( "kalkylprisertt.KLOGSUBID = " + STRING( THIS-OBJECT:KatalogTTh:BUFFER-FIELD("HKLOGSUBID"):BUFFER-VALUE ) + " AND kalkylprisertt.EGENKODUPP = TRUE").      
     */
     THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFiltrera("kalkylprisertt.EGENKODUPP = TRUE").
      /* Sätter Combos */ 
      /*GÖR ÄVEN REF PÅ ARBKODER*/
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
      ELSE THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE.
      THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:VALUE  = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE.
      
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems[THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE - 1].
     */
      /*v11.2 fix*/
      THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      IF STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR  STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) =  ? THEN THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      ELSE DO:
         Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
         IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
            THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("VIBESTID"):BUFFER-VALUE.
         END.
         ELSE THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE.  
      END.
      
      THIS-OBJECT:SetIniComboV(). 
      THIS-OBJECT:KalkKopKonvControl:KopHuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:GuruFiltrera().
      THIS-OBJECT:KalkKopKonvControl:KopHuvudKatalog:GuruFiltrera().
      
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      THIS-OBJECT:KalkKopKonvControl:KopHuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
    
       
      /*THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:GuruFiltrera().*/
      
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
         THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      END.
      ELSE DO:
         THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE     = TRUE.
         THIS-OBJECT:KalkHuvudControl:UtfardareBort:VALUE = "Borttagen person " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
         
      END.  
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:KalkHuvudControl:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:KalkHuvudControl:HuvudKatalog:Text.
      /* Hitta kalkylansvarig*/
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
      IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
         THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).              
      END.
      
      /* Filtrera arbetskoder efter kalkyltyp */
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera("kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
      /* Hitta första rows i grejer - behövs detta verkligen ? */
      /* Döp om matris rätt */
      matrisname = THIS-OBJECT:GetMatrisName().
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      /* Sätter värden */
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
      THIS-OBJECT:LaddaLabels().
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName().
      
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 2").
      IF HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE NE 7 THEN DO:
         maxmatris = 100.
         IF Guru.Konstanter:globforetag = "GRAN" THEN maxmatris = 11.
         imatris = 3.
         DO WHILE imatris < maxmatris:
            THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " " + STRING(imatris)).
            imatris = imatris + 1.
         END.   
      END.
      
      THIS-OBJECT:RibbonToolMatris:VALUE = THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolMatris:SelectedIndex = 0.
      THIS-OBJECT:CurrentMatris = -1.
      THIS-OBJECT:RibbonToolVisMatris:ValueList = THIS-OBJECT:RibbonToolMatris:ValueList:Clone().
      THIS-OBJECT:RibbonToolVisMatris:VALUE = THIS-OBJECT:RibbonToolMatris:VALUE.
      /* Visa  */
      THIS-OBJECT:UpdateRibbonShow().
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Beraknaprocalla().
      THIS-OBJECT:KalkFaktControl:GridPriser:GuruReopen().
      THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruReopen().
      THIS-OBJECT:KalkTidlControl:GridTidlage:GuruReopen().
      
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
         kalkylisinaktiv = TRUE.
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.         
      END.    
      ELSE DO:
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
         
         /*Robin Sjöberg Elpool i Umeå AB  16 apr 2014 13:25:28 
         
         Försök till lösning av bugg vid ny kalkyl -> fillin
         
         THIS-OBJECT:TabManager:BeginUpdate().
         THIS-OBJECT:TabManager:SelectedTab = THIS-OBJECT:TabManager:Tabs["Kalkyl"].
         THIS-OBJECT:TabManager:ActiveTab = THIS-OBJECT:TabManager:Tabs["Kalkyl"].
         THIS-OBJECT:TabManager:Focus().
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
         THIS-OBJECT:TabManager:EndUpdate().
         */
      END.
      
      
     
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
   /*
   Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabEgnaKoder).
   Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabUppfoljning).
   */
      
   
   END METHOD.
   METHOD PUBLIC LOGICAL SokAnvUtfardare(INPUT sokanv AS CHARACTER ):
      IF sokanv = "?" OR sokanv = ? OR sokanv = "" THEN RETURN FALSE.
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + sokanv + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID LaddaLabels():
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE) + "'") NO-ERROR.
     
      ASSIGN
      THIS-OBJECT:KalkHuvudControl:LabelBest:TEXT                  = SUBSTRING(Guru.Konstanter:gbestl,1,10)
      THIS-OBJECT:KalkHuvudControl:LabelOmr:TEXT                  = Guru.Konstanter:gomrk
      
      THIS-OBJECT:KalkKopKonvControl:KopLabelOmr:TEXT               = Guru.Konstanter:gomrk
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruLabel:Text  = "Benämning"
      THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text   = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
      THIS-OBJECT:KalkHuvudControl:HuvudAnmarkning:Text           = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE
      /*
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text     = "Kalkylnr."
      */
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text     = "nr"
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text     = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text,TRUE)
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text      = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:ReadOnly   = TRUE           
      THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruLabel:Text        = "Kalkyltyp"
      THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruLabel:Text = "Välj priser för uppföljning"
      THIS-OBJECT:KalkKopKonvControl:KonvTillTyp:GuruLabel:Text     = " Konvertera till Kalkyltyp"
      THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruLabel:Text   = "Ansvarig"
      THIS-OBJECT:KalkControl:fillinFriInKod:GuruLabel:Text   = "Arbkod:"
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruLabel:Text   = "Löpnr:"
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruLabel:Text   = "Antal:".
      
      IF Guru.Konstanter:PersonalTTh:AVAILABLE = FALSE  THEN DO:
         THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text  = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BortTagenPersonal(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE)).      
         Guru.Konstanter:PersonalTTh:FIND-FIRST() NO-ERROR.
      END.   
      ELSE DO: 
         
         THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text = 
         Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
         THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
          
         THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).
      END.
      THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:ReadOnly = TRUE.
     /*
      THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Enabled   = FALSE.
      */  
   END METHOD.
   
   /*anders
METHOD PUBLIC VOID TestIterate(toolgroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
   /*  visning */
   DEFINE VARIABLE iterator     AS INTEGER                                    NO-UNDO.
   DEFINE VARIABLE ToolType     AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE ToolVALUE    AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE ToolKey      AS CHARACTER                                  NO-UNDO.
   DEFINE VARIABLE DasTool      AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO.
   DEFINE VARIABLE DebugMessage AS CHARACTER                                  NO-UNDO.
   REPEAT iterator = 0 TO toolgroup:Tools:Count - 1 :
      DasTool = toolgroup:Tools[iterator].
      ToolKey  = DasTool:KEY.
      ToolType = DasTool:GetType():FullName.
      ToolVALUE = "???".
      IF ToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN 
      DO:
         ToolVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
      END.
        
      IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
         ToolVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
      END.
      DebugMessage = DebugMessage + "~n" + ToolKey + ", " + ToolType + ", " + ToolVALUE.
   END.
   MESSAGE DebugMessage
      VIEW-AS ALERT-BOX.
END METHOD.
*/
   /* Skapar alla griddar */
   METHOD PUBLIC VOID CreateGrids():
      /* Huvud */
      THIS-OBJECT:CreateGridPersonal().
      /* Kalkyl*/
      THIS-OBJECT:CreateGridArbetsKoder().
      THIS-OBJECT:CreateGridLopNummer().
      THIS-OBJECT:CreateGridKalkylKoder().
      THIS-OBJECT:CreateGridFriKoder().
      THIS-OBJECT:CreateGridFrekvens().
      /* Egna koder*/
      THIS-OBJECT:CreateGridEgnaKoder().
      THIS-OBJECT:CreateGridUnderKoder().
      THIS-OBJECT:CreateGridValdaUnderKoder().
      /* Materiel */
      THIS-OBJECT:CreateGridMateriel().
      THIS-OBJECT:CreateGridEgetMateriel().
      THIS-OBJECT:CreateGridFaktorer().
      THIS-OBJECT:CreateGridEgnaPriser().
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridMallarKoder().
      THIS-OBJECT:CreateGridValdaMallar().
      THIS-OBJECT:CreateGridVolymBer().
      THIS-OBJECT:CreateGridFelMedd().
      THIS-OBJECT:CreateGridTidlage().
      
   END METHOD.

   METHOD PUBLIC VOID CreateGridFunctions():
      /* Drag dropp*/
      THIS-OBJECT:DGKoder = NEW Controls.Subclasses.DragDropGroup("Koder").
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:KalkControl:GridLopKoder).
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:KalkControl:GridKalkylKoder).
      THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:KalkEgnaControl:GridUnderKoder).
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder).
      THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:KalkMtrlControl:GridMateriel).
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:KalkMtrlControl:GridEgetMateriel).
      THIS-OBJECT:DGMall = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:KalkImpMallControl:GridMallar).
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:KalkImpMallControl:GridValdaMallar).
      /* Button overs*/
      THIS-OBJECT:KalkControl:KalkylButtonOverMark:GuruKoppla(THIS-OBJECT:KalkControl:GridLopKoder,THIS-OBJECT:KalkControl:GridKalkylKoder).
      THIS-OBJECT:KalkControl:KalkylButtonOverMark:GuruDublett = TRUE.
      THIS-OBJECT:KalkControl:KalkylButtonBackMark:GuruKoppla(THIS-OBJECT:KalkControl:GridKalkylKoder,THIS-OBJECT:KalkControl:GridLopKoder).
      THIS-OBJECT:KalkControl:KalkylButtonBackMark:GuruRemoveFrom = TRUE.  
      /* Egna koder börjar här
      uttonOvermarkPriser:GuruKoppla(prisposter,valdaprisposter).
      buttonOvermarkPriser:GuruDublett = FALSE.
      buttonOverAllaPriser:GuruKoppla(prisposter, valdaprisposter).
      buttonOverAllaPriser:GuruDublett = FALSE.
      buttonOverAllaPriser:GuruAllaover = TRUE.
      */
      THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:GuruKoppla(THIS-OBJECT:KalkEgnaControl:GridUnderKoder,THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder).
      THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:GuruKoppla(THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder, THIS-OBJECT:KalkEgnaControl:GridUnderKoder).
      THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:GuruRemoveFrom = TRUE.
      /*materialtab*/
      THIS-OBJECT:KalkMtrlControl:MatButtonOverMark:GuruKoppla(THIS-OBJECT:KalkMtrlControl:GridMateriel,THIS-OBJECT:KalkMtrlControl:GridEgetMateriel).
      THIS-OBJECT:KalkMtrlControl:MatButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:KalkMtrlControl:MatButtonBackMark:GuruKoppla(THIS-OBJECT:KalkMtrlControl:GridEgetMateriel, THIS-OBJECT:KalkMtrlControl:GridMateriel).
      THIS-OBJECT:KalkMtrlControl:MatButtonBackMark:GuruRemoveFrom = TRUE.
      /*mallar*/
      THIS-OBJECT:KalkImpMallControl:ButtonOverMarkMallar:GuruKoppla(THIS-OBJECT:KalkImpMallControl:GridMallar,THIS-OBJECT:KalkImpMallControl:GridValdaMallar).
      THIS-OBJECT:KalkImpMallControl:ButtonOverMarkMallar:GuruDublett = TRUE.
      
      THIS-OBJECT:KalkImpMallControl:ButtonBackMarkMallar:GuruKoppla(THIS-OBJECT:KalkImpMallControl:GridValdaMallar, THIS-OBJECT:KalkImpMallControl:GridMallar).
      THIS-OBJECT:KalkImpMallControl:ButtonBackMarkMallar:GuruRemoveFrom = TRUE.
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT Guru.Konstanter:PersonalTTh)).
      THIS-OBJECT:KalkHuvudControl:GridPersonal:Text = "ansvarig".
      THIS-OBJECT:KalkHuvudControl:GridPersonal:Text = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:KalkHuvudControl:GridPersonal:Text,TRUE).
           /*egna event*/
      THIS-OBJECT:KalkHuvudControl:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
      
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,125).
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      THIS-OBJECT:KalkControl:GridArbetsKoder:TEXT = "Arbetskoder".      
      THIS-OBJECT:KalkControl:GridArbetsKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:KalkControl:GridArbetsKoder:tBS:PositionChanged:Subscribe(ValueArbetsKoderCh).
          
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      
      THIS-OBJECT:KalkControl:GridLopKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 4, INPUT THIS-OBJECT:LopposterTTh)).
      THIS-OBJECT:KalkControl:GridLopKoder:TEXT = "Löpnummer".
      THIS-OBJECT:KalkControl:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:KalkControl:GridLopKoder:tBS:PositionChanged:Subscribe(ValueLopnrCh).
      THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Columns["ARBKOD"]:Hidden = TRUE.
      
         
      
      
      /*
      DEFINE VARIABLE ultraGridGroup1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup1 = THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Groups:Add("G2","Timmar").
      ultraGridGroup1:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:COLUMNS:Add(KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Columns["ARBKOD"],0,0).
      
      ultraGridGroup1 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Group1",-1).
      
      ultraGridGroup1 = Groups:Add( "Group1", "Contacts" ).
    
      ultraGridGroup1:BANDS:COLUMNS:Add(BANDS:Columns["ARBKOD"],0,0).
      ultraGridGroup1:BANDS:COLUMNS:Add(BANDS:Columns["LOPNR"],0,1).
      ultraGridGroup1:BANDS:COLUMNS:Add(BANDS:Columns["BENAMNING"],0,2).
      ultraGridGroup1:BANDS:COLUMNS:Add(BANDS:Columns["ENHET"],0,3).
      */
      
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("num","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE, 35, "KALKLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55,"DEC", "KALKYL").
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubrikergrid[11] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,80).
      rubrikergrid[12] = NEW Controls.GridRubrik("RISK","Risk i %",FALSE,75,"DEC", "KALKYL").
      rubrikergrid[13] = NEW Controls.GridRubrik("VINST","Vinst i %",FALSE,75,"DEC", "KALKYL").
   
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 13, INPUT THIS-OBJECT:KoderTTh)).
      THIS-OBJECT:KalkControl:GridKalkylKoder:TEXT = "Kalkylkoder".  
       
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      IF Guru.Konstanter:globanv = "celpao" THEN .
      ELSE THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      THIS-OBJECT:KalkControl:GridKalkylKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruNoAutoTitle(TRUE).
      THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:PositionChanged:Subscribe(ValueKalkKoderCh).
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Gå till").
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:GaTillMenuItem_Click).
   
   END METHOD.
   
   METHOD PUBLIC VOID GaTillMenuItem_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:RibbonToolFilterTyp:Text = "Alla".
      THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridArbetsKoder:GuruRepositionto(THIS-OBJECT:ArbetskoderTTh:ROWID).
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkControl:GridLopKoder:GuruRepositionto(THIS-OBJECT:LopposterTTh:ROWID). 
         END.   
      END.
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFrekvens():
     
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("FREKOD"," Arbkod ",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("FREKNR","Löpnr",TRUE,45).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,600).
      
      THIS-OBJECT:KalkControl:GridFrekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:FrekvensTTh)).
      THIS-OBJECT:KalkControl:GridFrekvens:TEXT = "Frekvenskoder". 
      THIS-OBJECT:KalkControl:GridFrekvens:GuruNoAutoTitle(TRUE).
      
      
        
   END METHOD.

   METHOD PUBLIC VOID CreateGridFriKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NUMsubid", "##", TRUE ,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("FRIBENAMNING", "Fri benämning", false, 100).
      rubrikergrid[4] = NEW Controls.GridRubrik("FRITIMMAR", "Fri timmar", false, 50,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("FRIPRIS", "Fri pris/h", false, 75,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("FRIKOSTNAD", "Fri kostnad", false, 75,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 100).
      rubrikergrid[8] = NEW Controls.GridRubrik("TIMMAR", "Timmar", true, 50,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("PRIS", "Pris/h", true, 75,"DEC", "KALKYL").
      rubrikergrid[10] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", true, 75,"DEC", "KALKYL").
       THIS-OBJECT:KalkControl:GridFriKalk:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:ValdaPriserTTh)).
      THIS-OBJECT:KalkControl:GridFriKalk:Text = "Frikalkylkoder".
      THIS-OBJECT:KalkControl:GridFriKalk:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:KalkControl:GridFriKalk:GuruFirstrow().
      /*
      THIS-OBJECT:KalkControl:GridFriKalk:GuruSetColumnBgColor("FRIBENAMNING", System.Drawing.Color:LightBlue).
      THIS-OBJECT:KalkControl:GridFriKalk:GuruSetColumnBgColor("FRIPRIS", System.Drawing.Color:LightBlue).
      THIS-OBJECT:KalkControl:GridFriKalk:GuruSetColumnBgColor("FRIKOSTNAD", System.Drawing.Color:LightBlue).
      THIS-OBJECT:KalkControl:GridFriKalk:GuruSetColumnBgColor("FRITIMMAR", System.Drawing.Color:LightBlue).
     */
      THIS-OBJECT:KalkControl:GridFriKalk:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridFriKalk:DisplayLayout:Bands[0]:Columns["NUMsubid"]:Hidden = TRUE. 
   END METHOD.
   
   /* Egna koder*/
   METHOD PUBLIC VOID CreateGridEgnaKoder():
      {GridRubrikListaVarExtent.i}
      THIS-OBJECT:DontCheck = TRUE.
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","MATRIS",FALSE,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",FALSE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,80,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",false,80).
      /*init av grid*/
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KoderTTh)). 
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:TEXT = "Egna koder".
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.   
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("kalknumtt.ARBKOD = 'EGEN'").
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:tBS:PositionChanged:Subscribe(ValueEgnaKoderCh).
      
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:DontCheck = false.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 250).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", TRUE, 100,"DEC", "KALKYL").
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:PriserTTh)).
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:TEXT = "Underkoder".
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NUMSUBID", "##", TRUE ,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100,"DEC", "KALKYL").
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ValdaPriserTTh)).
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUMSUBID"]:Hidden = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:TEXT = "Valda underkoder".
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
   END METHOD.

   /* Materiel */
   METHOD PUBLIC VOID CreateGridMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      /*
      rubrikergrid[6] = NEW Controls.GridRubrik("KUND","", TRUE ,50 ).
      */
      
      THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT Guru.GlobalaVariabler:MaterielTTh)).
      
      THIS-OBJECT:KalkMtrlControl:GridMateriel:Text = "Materiel".
      IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruOrderbyDefault("BY kund DESCENDING by enr").
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:KalkMtrlControl:GridMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruGridFarg(TRUE).             /*OBSE CASESENSITIVE*/ 
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         
      END. 
      THIS-OBJECT:KalkMtrlControl:GridMateriel:VisaViaInternetMenuItemAddAdd().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MATRIS", "Matris", FALSE ,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("BERKVANT", "Antal", FALSE,50,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("NPRIS", "Nettopris", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      
      THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:kalktmtrlTTh)).
      THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:Text = "Materielspecifikation".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruGridFarg(TRUE).
         THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
      END.  
      THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:VisaViaInternetMenuItemAddAdd().
            
   END METHOD.

   /* Faktorer och priser*/
   METHOD PUBLIC VOID CreateGridFaktorer():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("FAKTOR","Faktor",FALSE,75,"DEC", "KALKYL").
      THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:FaktorerTTh)).  
      THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:KalkFaktControl:GridFaktorer:Text = "Faktorer".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallarKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      THIS-OBJECT:KalkImpMallControl:GridMallarKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:KalkImpMallControl:GridMallarKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:KalkImpMallControl:GridMallarKoder:Text = "Koder för mallen".
   END METHOD.

   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:KalkImpMallControl:GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallHuvudtth)).  
      THIS-OBJECT:KalkImpMallControl:GridMallar:Text = "Mallar".
      THIS-OBJECT:KalkImpMallControl:GridMallar:tBS:PositionChanged:Subscribe(ValueMallarCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:KalkImpMallControl:GridValdaMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallValdtth)).
      THIS-OBJECT:KalkImpMallControl:GridValdaMallar:Text = "Valda mallar".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgnaPriser():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS","Pris",FALSE,75,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("PROCENT","Procentpåslag",FALSE,85,"DEC", "DEC0"). /*enbart hjälp vid räkning*/
     
      THIS-OBJECT:KalkFaktControl:GridPriser:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:EgnaPriserTTh)).
      THIS-OBJECT:KalkFaktControl:GridPriser:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:KalkFaktControl:GridPriser:Text = "Egna priser".
   END METHOD.
   METHOD PUBLIC VOID CreateGridTidlage():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,200).
      rubrikergrid[2] = NEW Controls.GridRubrik("DATUMTID","Datum",FALSE,200,"DATETIME").
      rubrikergrid[3] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      THIS-OBJECT:KalkTidlControl:GridTidlage:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:kalkttidlageTTh)).
      THIS-OBJECT:KalkTidlControl:GridTidlage:TEXT = "Tidlägen".
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridStatus():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("KALKNR","Ärendenr",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("OMRADE",Guru.Konstanter:gomrk,TRUE,100).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,200).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,175).
      rubrikergrid[5] = NEW Controls.GridRubrik("DATUMTID","Datum",TRUE,150,"DATETIME").
      rubrikergrid[6] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      THIS-OBJECT:KalkStatusControl:GridStatus:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:kalkttidlageTTh)).
      THIS-OBJECT:KalkStatusControl:GridStatus:TEXT = "Status".
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridVolymBer():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("SCHAKTLANGD","Schaktlängd i meter",FALSE,100,"DEC", "KALKYL").
      rubrikergrid[2] = NEW Controls.GridRubrik("SCHAKTBREDD","Schaktbredd i centimeter",FALSE,140,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("SCHAKTDJUP","Schaktdjup i centimeter",FALSE,135,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("SVARMARK","Svår mark",FALSE,70,"LOG").
      rubrikergrid[5] = NEW Controls.GridRubrik("RORFOR","Rörförläggning",FALSE,90,"LOG").
      rubrikergrid[6] = NEW Controls.GridRubrik("TOTVOLYM","Totalt m3",TRUE,70,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("TILLAGGVOLYM","Tilläggsfyllning m3",TRUE,110,"DEC", "KALKYL").
      rubrikergrid[8] = NEW Controls.GridRubrik("SKYDDFYLLNING","Skyddsfyllning m3",TRUE,105,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("YTBELAGGD","Ytbeläggning m2",TRUE,100,"DEC", "KALKYL").
      THIS-OBJECT:KalkVolymControl:GridVolymBer:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 9, INPUT THIS-OBJECT:VolymberTTh)).  
      THIS-OBJECT:KalkVolymControl:GridVolymBer:Text = "Volymberäkningar".
   END METHOD.
    
   METHOD PUBLIC VOID CreateGridFelMedd():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("UT","Meddelande",TRUE,600).
      THIS-OBJECT:KalkFelmeddControl:GridFelMedd:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 1, INPUT THIS-OBJECT:tiduth)).
      THIS-OBJECT:KalkFelmeddControl:GridFelMedd:Text = "Fel vid inläsning!".
   END METHOD.
   
   
   /* Skapar alla Combos */
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboTyp().
      THIS-OBJECT:CreatecomboPriserUppf().
      THIS-OBJECT:CreateComboOmrade().
      THIS-OBJECT:CreateComboBestKund().
      THIS-OBJECT:KopCreateComboOmrade().
      THIS-OBJECT:CreateComboKatalog().
      THIS-OBJECT:CreateComboKopKatalog().
      THIS-OBJECT:CreateComboUtfardare().
      
   END METHOD. 
   METHOD PUBLIC VOID BortBestKund():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + STRING(Guru.Konstanter:BestKundTTh:TABLE + " where TTRECID = 0 OR TTRECID = ?").
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.Konstanter:BestKundTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         Guru.Konstanter:BestKundTTh:BUFFER-DELETE ().
         qH:GET-NEXT().
      END.  
   END METHOD.
   METHOD PUBLIC VOID AddBestKund(INPUT omr AS CHARACTER):
      
      Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + omr + "'")  NO-ERROR.
      Guru.Konstanter:BestKundTTh:FIND-FIRST("WHERE BESTID = '" + omr + "'")  NO-ERROR.
      IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN.
      ELSE DO:
         IF Guru.Konstanter:OmradeTTh:AVAILABLE THEN DO:
            THIS-OBJECT:BortBestKund().
            Guru.Konstanter:BestKundTTh:BUFFER-CREATE ().
            Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE.
            Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
            Guru.Konstanter:BestKundTTh:BUFFER-FIELD("VIBESTID"):BUFFER-VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
            THIS-OBJECT:KalkHuvudControl:BestKund:GuruFiltrera().
         END.   
      END.
      
   END METHOD.
   
   METHOD PRIVATE VOID SetIniComboV():
      Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE) + "'")  NO-ERROR. 
      THIS-OBJECT:KalkHuvudControl:HuvudOmrade:SetInitialValue(THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE,STRING(Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE)).
      THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE)).
      Guru.Konstanter:BestKundTTh:FIND-FIRST("WHERE BESTID = '" + STRING(THIS-OBJECT:KalkHuvudControl:BestKund:VALUE) + "'")  NO-ERROR. 
      
      IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkHuvudControl:BestKund:SetInitialValue(THIS-OBJECT:KalkHuvudControl:BestKund:VALUE,STRING(Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE)).
      END.
      ELSE THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:KalkHuvudControl:BestKund:VALUE)).                 
      
   END METHOD.
   
      
   /*v11.2 fix*/
   METHOD PUBLIC VOID SparaComboV(INPUT incombo AS Controls.Combo):
      
      IF incombo:VALUE = ? THEN RETURN.
     
      IF incombo:NAME  = "HuvudOmrade" THEN combolista[1] = THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE.
     
      IF incombo:NAME = "HuvudUtfardare" THEN combolista[2] = THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE.
      IF incombo:NAME  = "KopHuvudOmrade" THEN combolista[3] = THIS-OBJECT:KalkKopKonvControl:KopHuvudOmrade:VALUE.
      IF incombo:NAME = "ComboAvtal" THEN combolista[4] = THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE.
      IF incombo:NAME = "BestKund" THEN combolista[5] = THIS-OBJECT:KalkHuvudControl:BestKund:VALUE.
      
   END METHOD.
   /*v11.2 fix*/
   
   METHOD PRIVATE VOID GetComboV(INPUT incombo AS Controls.Combo):
       
      IF incombo:VALUE NE ? THEN RETURN.
    
      IF incombo:NAME  = "HuvudOmrade" THEN THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = combolista[1].
    
      IF incombo:NAME = "HuvudUtfardare" THEN THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE = combolista[2]. 
      IF incombo:NAME  = "KopHuvudOmrade" THEN THIS-OBJECT:KalkKopKonvControl:KopHuvudOmrade:VALUE =combolista[3].
      IF incombo:NAME = "ComboAvtal" THEN THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE = combolista[4].   
      IF incombo:NAME = "BestKund" THEN THIS-OBJECT:KalkHuvudControl:BestKund:VALUE =  combolista[5].    
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboPriserUppf():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE ufpris:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Priser".
      cmbTyplist:addRubrik(typK). 
      CREATE ufpris.
      ASSIGN 
      ufpris.UTYP = 1
      ufpris.TYPC = "Enligt katalog".
      CREATE ufpris.
      ASSIGN 
      ufpris.UTYP = 2
      ufpris.TYPC = "Eget materiel".
      THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "UTYP", "TYPC").    
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboTyp():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).   
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN DO:
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 1
         typtt.TYPC    = "Typ 1".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 2
         typtt.TYPC    = "Typ 2".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 3
         typtt.TYPC    = "Typ 3".           
      END.
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "cGRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO: 
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 5
         typtt.TYPC    = "Sam B/F".        
      END.
      IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:  
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 7
         typtt.TYPC    = "Nätreg N1".      
      END.
      THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboKonvTyp():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
      EMPTY TEMP-TABLE konvtyptt NO-ERROR. 
      THIS-OBJECT:KalkKopKonvControl:KonvTillTyp:GuruCombo:Clear().
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE konvtyptt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE) ) NO-ERROR.
      IF INTEGER(STRING(THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE)) <= 2002 THEN.
      ELSE DO: 
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "1" THEN DO:
            CREATE konvtyptt.
            ASSIGN 
            konvtyptt.TYPKALK = 2
            konvtyptt.TYPC    = "Typ 2".
            IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:  
               CREATE konvtyptt.
               ASSIGN 
               konvtyptt.TYPKALK = 7
               konvtyptt.TYPC    = "Nätreg N1".      
            END.
         END.
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
            CREATE konvtyptt.
            ASSIGN 
            konvtyptt.TYPKALK = 1
            konvtyptt.TYPC    = "Typ 1".
         END.   
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "2" THEN DO:
            IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO: 
               CREATE konvtyptt.
               ASSIGN 
               konvtyptt.TYPKALK = 3
               konvtyptt.TYPC    = "Typ 3".        
            END.
         END.
      END.    
      FIND FIRST konvtyptt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE konvtyptt THEN DO:
         CREATE konvtyptt.
         ASSIGN 
         konvtyptt.TYPKALK = 99
         konvtyptt.TYPC    = "Det går inte att konvertera denna kalkyl!".  
      END.
      THIS-OBJECT:KalkKopKonvControl:KonvTillTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
      THIS-OBJECT:KalkKopKonvControl:KonvTillTyp:GuruCombo:VALUE = konvtyptt.TYPKALK.
   END METHOD.
   METHOD PUBLIC VOID CreateComboBestKund():
      DEFINE VARIABLE cmbBestlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE BestK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE BestnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbBestlist = NEW Controls.GridRubrikLista().
      cmbBestlist:ttBufferHandle = Guru.Konstanter:BestKundTTh.
     
      BestK = NEW Controls.GridRubrik().
      BestK:Falt = "VIBESTID".
      BestK:Rubrik = Guru.Konstanter:gbestk.
      cmbBestlist:addRubrik(BestK).
     
      BestnamnK = NEW Controls.GridRubrik().
      BestnamnK:Falt = "BESTNAMN".
      BestnamnK:Rubrik = "Namn".
      cmbBestlist:addRubrik(BestnamnK).
     
     
      THIS-OBJECT:KalkHuvudControl:BestKund:GuruInitCombo(cmbBestlist, "VIBESTID", "BESTNAMN").
     
   END METHOD.
   METHOD PUBLIC VOID CreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = Guru.Konstanter:OmradeTTh.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).
        
      THIS-OBJECT:KalkHuvudControl:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").  
   END METHOD.
   
   METHOD PUBLIC VOID KopCreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = Guru.Konstanter:OmradeTTh.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  
      THIS-OBJECT:KalkKopKonvControl:KopHuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
   END METHOD.

   METHOD PUBLIC VOID CreateComboKatalog():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboKopKatalog():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:KalkKopKonvControl:KopHuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboUtfardare():
      DEFINE VARIABLE cmbUtflist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE UtfK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE UtfnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbUtflist = NEW Controls.GridRubrikLista().
      cmbUtflist:ttBufferHandle = Guru.Konstanter:AnvandareTTh.
      IF Guru.Konstanter:globforetag = "ELPA" THEN.
      ELSE cmbUtflist:filter = "anvandartemp.AV-lEVEL > 0".
      UtfK = NEW Controls.GridRubrik().
      UtfK:Falt = "ANVANDARE".
      UtfK:Rubrik = "Användare".
      cmbUtflist:addRubrik(UtfK).
      UtfnamnK = NEW Controls.GridRubrik().
      UtfnamnK:Falt = "AV-NAMN".
      UtfnamnK:Rubrik = "Namn".
      cmbUtflist:addRubrik(UtfnamnK).       
      THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboAvtal():
      DEFINE VARIABLE cmbAvtallist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katar        AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtalar      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtaltxt     AS Controls.GridRubrik      NO-UNDO.
      cmbAvtallist = NEW Controls.GridRubrikLista().
      cmbAvtallist:ttBufferHandle = THIS-OBJECT:Avtalskalktth.
      katar = NEW Controls.GridRubrik().
      katar:Falt = "KATAR".
      katar:Rubrik = "Årtal för KLG".
      cmbAvtallist:addRubrik(katar).
      avtalar = NEW Controls.GridRubrik().
      avtalar:Falt = "AVTALAR".
      avtalar:Rubrik = "Årtal för avtal".
      cmbAvtallist:addRubrik(avtalar).
      avtaltxt = NEW Controls.GridRubrik().
      avtaltxt:Falt = "AVTALTXT".
      avtaltxt:Rubrik = "Välj avtalskalkylmall för din kund.".
      cmbAvtallist:addRubrik(AVTALTXT).  
      THIS-OBJECT:KalkAvtalControl:ComboAvtal:GuruInitCombo(cmbAvtallist,"ID","AVTALTXTHELA").
   END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerEgenKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
    
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN
         THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
      END.
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE
         THEN THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
      END.
   END METHOD.
   
   

   
   

   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   METHOD PUBLIC VOID FilterUnderKoder():
      /* Om det finns en aktiv row..*/
      IF THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:tBS:Count > 0 AND THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow NE ? THEN DO:
         /* Uppdatera valda underkoderna, och återställ kontrollerna*/
         IF TRIM(STRING(THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE)) = "" THEN RETURN.
         IF THIS-OBJECT:DontCheck = FALSE THEN DO:
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING( THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE )).
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFirstrow().
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:Enabled = TRUE.
         END.  
      END.
           
      ELSE IF THIS-OBJECT:KoderTTh:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFirstrow().
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:Enabled = TRUE.
         THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:Enabled = TRUE.
         THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:Enabled = TRUE.

      END.      
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:Enabled = FALSE.
         THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:Enabled = FALSE.
         THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:Enabled = FALSE.
      END.
      RETURN.
   END METHOD.

   


   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      /*
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 THEN DO:
         FilterUnderKoder().
      END.   
      
      RETURN.
      */
   END METHOD.
   




   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell = THIS-OBJECT.
      
      THIS-OBJECT:StartaControl().
      THIS-OBJECT:CreateForm().
      THIS-OBJECT:FriInEventSub().
      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL InitializeArendeStatus():
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell = THIS-OBJECT.    
      THIS-OBJECT:CreateRibbonVS().
      THIS-OBJECT:TabManager:Tabs["Status"]:Selected = TRUE.
       
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID StartaControl():
      KalkHuvudControl = NEW Modules.Kalkyl.KalkHuvudControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:KalkHuvudControl).
      KalkHuvudControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      KalkControl = NEW Modules.Kalkyl.KalkylControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:KalkControl).
      KalkControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkEgnaControl = NEW Modules.Kalkyl.KalkEgnaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:KalkEgnaControl).
      KalkEgnaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkFaktControl = NEW Modules.Kalkyl.KalkFaktControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabFaktorer:Controls:Add(THIS-OBJECT:KalkFaktControl).
      KalkFaktControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkMtrlControl = NEW Modules.Kalkyl.KalkMtrlControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:KalkMtrlControl).
      KalkMtrlControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkImpMallControl = NEW Modules.Kalkyl.KalkImpMallControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:KalkImpMallControl).
      KalkImpMallControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkAvtalControl = NEW Modules.Kalkyl.KalkAvtalControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:KalkAvtalControl).
      KalkAvtalControl:Dock = System.Windows.Forms.DockStyle:FILL.
       
      KalkKopKonvControl = NEW Modules.Kalkyl.KalkKopKonvControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KalkKopKonvControl).
      KalkKopKonvControl:Dock = System.Windows.Forms.DockStyle:FILL.
     
      KalkTidlControl = NEW Modules.Kalkyl.KalkTidlControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabTidlage:Controls:Add(THIS-OBJECT:KalkTidlControl).
      KalkTidlControl:Dock = System.Windows.Forms.DockStyle:FILL.
       
      KalkFelmeddControl = NEW Modules.Kalkyl.KalkFelmeddControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabFelmedd:Controls:Add(THIS-OBJECT:KalkFelmeddControl).
      KalkFelmeddControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkVolymControl = NEW Modules.Kalkyl.KalkVolymControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabVolymber:Controls:Add(THIS-OBJECT:KalkVolymControl).
      KalkVolymControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      KalkVisaControl = NEW Modules.Kalkyl.KalkVisaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:KalkVisaControl).
      KalkVisaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkStatusControl = NEW Modules.Kalkyl.KalkStatusControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabStatus:Controls:Add(THIS-OBJECT:KalkStatusControl).
      KalkStatusControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
   END METHOD. 


   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabKopiera_Paint( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.PaintEventArgs ):
		
      RETURN.

   END METHOD.

   METHOD PUBLIC VOID UpdateMatrisE(sender AS System.Object, args AS System.EventArgs):
      THIS-OBJECT:UpdateMatris().
   END METHOD.

   METHOD PUBLIC VOID UpdateMatris():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO. 
      mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:Text = "Alla" THEN mat = -1.
      IF THIS-OBJECT:RibbonToolMatris:Text = GetMatrisName() + " 1" THEN mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 2" THEN mat = 2.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 3" THEN mat = 3.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 4" THEN mat = 4.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 5" THEN mat = 5.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 6" THEN mat = 6.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 7" THEN mat = 7.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 8" THEN mat = 8.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 9" THEN mat = 9.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 10" THEN mat = 10.
      THIS-OBJECT:CurrentMatris = mat.
      IF THIS-OBJECT:NyKalkyl = FALSE THEN 
      DO:
         IF mat = -1 THEN DO:
            THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruFiltrera("").
            THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("").
         END.
         ELSE DO:
            THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            THIS-OBJECT:KalkControl:KalkylButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:EgenSkapa:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)+ " AND ARBKOD = 'EGEN'" ).
            THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
         END.
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:FilterUnderKoder().
      END.      
   END METHOD.
   METHOD PUBLIC VOID VisUpdateMatrisE(sender AS System.Object, args AS System.EventArgs):
      THIS-OBJECT:VisUpdateMatris().
   END METHOD.

   METHOD PUBLIC VOID VisUpdateMatris():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO. 
      IF THIS-OBJECT:RibbonToolVisMatris:Text = "Alla" THEN.
      ELSE DO:
         THIS-OBJECT:RibbonToolVisGroupMatris:Checked = FALSE.
      END.     
                  
   END METHOD.
   METHOD PUBLIC VOID GomaRibbon():
      IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN DO:
         THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Caption = Guru.GlobalaVariabler:KalkArendeText.
         /*
         THIS-OBJECT:Root:WindowManager:Wnd:GetToolbarManager():Office2007UICompatibility=false.
         */
         THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Visible = FALSE.
         THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Visible = FALSE.
         THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = Guru.GlobalaVariabler:KalkArendeText.
         THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruLabel:Visible = FALSE.
         THIS-OBJECT:KalkHuvudControl:comboPriserUppf:Visible = FALSE.
         THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruLabel:Visible = FALSE.
         THIS-OBJECT:KalkHuvudControl:HuvudTyp:Visible = FALSE.
         /*
         THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:ResetCaption().
         */
         THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:Visible = FALSE.
         
         THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Visible = FALSE.
         THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Clear().
         THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem(Guru.GlobalaVariabler:KalkArendeText)).
         THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens".
         THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:Caption = "Kommentarer".
         THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:Visible = FALSE.
         THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera " + Guru.GlobalaVariabler:KalkArendeText.
         THIS-OBJECT:RibbonTabVisning:Caption = Guru.GlobalaVariabler:KalkArendeText + "visning".
      END.   
   END METHOD.   
   METHOD PUBLIC VOID CreateRibbonVS():
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      THIS-OBJECT:RibbonTabVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      THIS-OBJECT:RibbonTabVisning:Caption = "Ärendestatus".
      /*
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      */
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv Ut".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSkrivUt,INPUT "bilder/kalkyl-print.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSkrivUt,INPUT "bilder/kalkyl-print.gif").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUt,RibbonTabVisning).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtGrid).
   END METHOD. 
   METHOD PUBLIC VOID CreateRibbon():
      /* Skapa ribbongrupp i kalkyltaben */
      
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      THIS-OBJECT:Root:ModuleManager:RibbonTab("RibbonTot","Information").
      THIS-OBJECT:RibbonTabKalkyl = THIS-OBJECT:Root:ModuleManager:RibbonContextManager:Groups:Add("Kalkyl").
      
      THIS-OBJECT:RibbonTabKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Koder").
      THIS-OBJECT:RibbonTabEgnaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Egna Koder").
      THIS-OBJECT:RibbonTabMat = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök materiel").
      THIS-OBJECT:RibbonTabVisaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visning av koder").
      THIS-OBJECT:RibbonTabVisaKoder:Visible = FALSE.
      THIS-OBJECT:RibbonTabVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      THIS-OBJECT:RibbonTabVisningVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Inkludera i visning").
      THIS-OBJECT:RibbonTabVisningEgenskaper = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visningsinställningar").
      /*
      THIS-OBJECT:RibbonTabUppfoljning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Uppföljning").
      */
      /* Skapa verktyg*/
      THIS-OBJECT:RibbonDummy = NEW Infragistics.Win.UltraWinToolbars.LabelTool("RD").
      THIS-OBJECT:RibbonDummy:SharedPropsInternal:Caption = " ". 
      /* Global*/
      THIS-OBJECT:RibbonToolMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("KodMatris").
      THIS-OBJECT:RibbonToolMatris:Key = "KalkylRibbonMatris".
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width = 135.
      
      THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
      THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp".
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width.
      
      DEFINE VARIABLE wTemp AS Infragistics.Win.ValueListItem NO-UNDO.  
      FOR EACH typtt:
         wTemp = NEW Infragistics.Win.ValueListItem().
         wTemp:DisplayText = typtt.TYPC.
         wTemp:DataVALUE = NEW Helpers.ABLDataContainer().
         CAST(wTemp:DataVALUE, Helpers.ABLDataContainer):CreateData("Typ", typtt.TYPKALK).
         THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Add(wTemp).
      END. 
      THIS-OBJECT:RibbonToolFilterTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FilterTyp").
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Caption = "Märkning".
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width.
      /* kalkyl*/
      THIS-OBJECT:RibbonToolInfTyp = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKalkTyp").
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Caption = "Kalkyl".
      THIS-OBJECT:RibbonToolInfTyp:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolOmrade = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoOmrade").
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Caption = Guru.Konstanter:gomrk.
      THIS-OBJECT:RibbonToolOmrade:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:KalkHuvudControl:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:KalkHuvudControl:HuvudKatalog:Text.
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      /* Koder */
      THIS-OBJECT:RibbonToolFriKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FriKalkActive").
      THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:Caption = "Frikalkyl".
      THIS-OBJECT:RibbonToolFriKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /*
      THIS-OBJECT:RibbonToolFriGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("FriGen").
      THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Caption = "Återställ frikalkyl".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolFriGenerera,INPUT "bilder/kalkyl-frikalkyl.gif").
      THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = FALSE.
      */
      
      THIS-OBJECT:RibbonToolKoderAnm = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkning").
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:Caption = "Anm. för koder".
      THIS-OBJECT:RibbonToolKoderAnm:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens enligt katalog".
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolKatalogKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KatKom").
      THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:Caption = "Kommentarer enligt katalog".
      THIS-OBJECT:RibbonToolKatalogKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolAngeKodAntal = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AngeKod").
      THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:Caption = "Fri inmatning".
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:ToolTipText = "Fri inmatning av Koder och Antal".
      
      THIS-OBJECT:RibbonToolKodSokning = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KodSokning").
      THIS-OBJECT:RibbonToolKodSokning:SharedPropsInternal:Caption = "Sökning av Koder".
      THIS-OBJECT:RibbonToolKodSokning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /* Visning */
     
      THIS-OBJECT:RibbonToolGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaGen").
      THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera kalkyl".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolGenerera,INPUT "bilder/kalkyl-generate.gif").
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUtBildval = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv Ut".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 6,INPUT THIS-OBJECT:RibbonToolSkrivUtBildval,INPUT "").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSkrivUt,INPUT "bilder/kalkyl-print.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSkrivUt,INPUT "bilder/kalkyl-print.gif").
      
      THIS-OBJECT:RibbonToolSparaExcel = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SparaExcel").
      THIS-OBJECT:RibbonToolSparaExcel:SharedPropsInternal:Caption = "Spara excel".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaExcel,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaExcel,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:RibbonToolSparaExcel:SharedPropsInternal:Visible = TRUE.
   
      /* inkludera i visning */
      THIS-OBJECT:RibbonToolVisHuvud = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisHuvud").
      THIS-OBJECT:RibbonToolVisHuvud:SharedPropsInternal:Caption = "huvud".
      
      THIS-OBJECT:RibbonToolVisHuvud:SharedPropsInternal:Caption = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:RibbonToolVisHuvud:SharedPropsInternal:Caption,TRUE).
      THIS-OBJECT:RibbonToolVisHuvud:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisAnmarkning = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisAnmarkning").
      THIS-OBJECT:RibbonToolVisAnmarkning:SharedPropsInternal:Caption = "Anmärkning".
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisUtforlig = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisAnmUtf").
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Caption = "Anm. för koder".
      THIS-OBJECT:RibbonToolVisUtforlig:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = FALSE.
      THIS-OBJECT:RibbonToolVisEgnaFaktorer = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisFaktorer").
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Caption = "Egna faktorer".
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisEgnaPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisEgnaPriser").
      THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:Caption = "Egna priser".
      THIS-OBJECT:RibbonToolVisEgnaPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisFrekvens = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisFrekvens").
      THIS-OBJECT:RibbonToolVisFrekvens:SharedPropsInternal:Caption = "Frekvens".
      THIS-OBJECT:RibbonToolVisFrekvens:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisKommentar").
      THIS-OBJECT:RibbonToolVisKom:SharedPropsInternal:Caption = "Kommentarer".
      THIS-OBJECT:RibbonToolVisKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      
      /* visningsinställningar */
      THIS-OBJECT:RibbonToolVisVisa = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisVisafor").
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Grundkalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Frikalkyl")).
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Riskkalkyl GK")).
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Vinstkalkyl GK")).
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Riskkalkyl FK")).
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Vinstkalkyl FK")).
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisPriser = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisPriser").
      THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Caption = "Materiel".
      THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Enligt katalog")).
      THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Eget materiel")).
      THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Tillkommande Eget materiel")).
      THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisMatris").
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Caption = "Matris".
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Width = 140.
      
      
      THIS-OBJECT:RibbonToolVisArbKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisArbKalk").
      THIS-OBJECT:RibbonToolVisArbKalk:SharedPropsInternal:Caption = "Visa som ArbetsKalkyl".
      THIS-OBJECT:RibbonToolVisArbKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      
      THIS-OBJECT:RibbonToolVisSummera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisSummera").
      THIS-OBJECT:RibbonToolVisSummera:SharedPropsInternal:Caption = "Summera koder".
      THIS-OBJECT:RibbonToolVisSummera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGruppera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppera").
      THIS-OBJECT:RibbonToolVisGruppera:SharedPropsInternal:Caption = "Gruppera koder".
      THIS-OBJECT:RibbonToolVisGruppera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGroupMatris = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppMatris").
      THIS-OBJECT:RibbonToolVisGroupMatris:SharedPropsInternal:Caption = "Gruppera matris".
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      THIS-OBJECT:RibbonToolVisMatSpec = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisMatSpec").
      THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Caption = "Mtrl.spec".
      THIS-OBJECT:RibbonToolVisMatSpec:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Caption = "Spara inställningar".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = TRUE.
      
      THIS-OBJECT:RibbonToolManual = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Manual").
      THIS-OBJECT:RibbonToolManual :SharedPropsInternal:Caption = "Manual".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolManual ,INPUT "bilder/kalkyl-manual2.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolManual ,INPUT "bilder/kalkyl-manual2.gif").
      THIS-OBJECT:RibbonToolManual:SharedPropsInternal:Visible = TRUE.
      
     
      /* Materiel */
      THIS-OBJECT:RibbonToolLev = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("MatLever").
      THIS-OBJECT:RibbonToolLev:SharedPropsInternal:Caption = "Leverantör:".
      /* Använd detta för inladdning = THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems[1]:DataVALUE */
      
      THIS-OBJECT:RibbonToolEnr = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatEnr").
      THIS-OBJECT:RibbonToolEnr:SharedPropsInternal:Caption = Guru.Konstanter:genk.
      THIS-OBJECT:RibbonToolSok = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatSoek").
      THIS-OBJECT:RibbonToolSok:SharedPropsInternal:Caption = "Benämning:".
      THIS-OBJECT:RibbonToolEndastmarkmtrl = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Endastmarkmtrl").
      THIS-OBJECT:RibbonToolEndastmarkmtrl:SharedPropsInternal:Caption = "Endast märkt materiel".
      THIS-OBJECT:RibbonToolEndastmarkmtrl:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /* Förbered för ribbon */
      
      
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy,THIS-OBJECT: RibbonTabVisningEgenskaper). /* <- Den här måste preppas en gång på detta sättet*/
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMatris,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAnm,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalogKom,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAngeKodAntal,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKodSokning,THIS-OBJECT:RibbonTabKoder).
      
      /*Anders Olsson Elpool i Umeå AB  21 mar 2014 16:46:00 
      dummy för att få en tom pos i ribbon 
      
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriKalk,THIS-OBJECT:RibbonTabKoder).
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriGenerera,THIS-OBJECT:RibbonTabKoder).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatris,THIS-OBJECT:RibbonTabEgnaKoder). /* Den här är nästan likadan som ovanstående kommentar, alla ribbonverktyg som läggs till måste och får bara preppas 1 gång, därefter måste man lägga till dom manuellt, se nedan */
      
     
                                         /* Så här*/
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris). /*är redan preppad*/                                   
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastmarkmtrl,THIS-OBJECT:RibbonTabMat).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolInfTyp,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat,THIS-OBJECT:RibbonTabKalkyl).    
      
     
      
      
      /* Preppa Visning*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisAnmarkning,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaFaktorer,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaPriser,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisFrekvens,THIS-OBJECT:RibbonTabVisningVisa).  
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisKom,THIS-OBJECT:RibbonTabVisningVisa).
      
     
      
      
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUt).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera,THIS-OBJECT:RibbonTabVisning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUtBildval,THIS-OBJECT:RibbonTabVisning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaExcel,THIS-OBJECT:RibbonTabVisning).
      
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisVisa,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisPriser,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatris).  /*finns ju redan*/
      
      
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisArbKalk,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummera,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGruppera,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGroupMatris,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatSpec,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolManual,THIS-OBJECT:RibbonTabVisningEgenskaper).
     
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
  
      
      /* binda events*/
      THIS-OBJECT:RibbonToolSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolEnr:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolGenerera:ToolClick:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtE).
      THIS-OBJECT:RibbonToolSparaExcel:ToolClick:Subscribe(THIS-OBJECT:SparaExcelE).
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolClick:Subscribe(THIS-OBJECT:VisAnmarkningChanged).
      THIS-OBJECT:RibbonToolVisPriser:ToolValueChanged:Subscribe(THIS-OBJECT:VisPriserChanged).
      THIS-OBJECT:RibbonToolVisPriser:SelectedItem = THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolVisVisa:SelectedItem = THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
      /*
      THIS-OBJECT:RibbonToolFriGenerera:ToolClick:Subscribe(THIS-OBJECT:RestoreFriKoder).
      */
      THIS-OBJECT:RibbonToolFriKalk:ToolClick:Subscribe(THIS-OBJECT:FriKalkChanged).
      
      THIS-OBJECT:RibbonToolKoderAnm:ToolClick:Subscribe(THIS-OBJECT:KoderAnmChanged).
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:FrekvensKomChanged).
      THIS-OBJECT:RibbonToolKatalogKom:ToolClick:Subscribe(THIS-OBJECT:KatsKomChanged).
      
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolClick:Subscribe(THIS-OBJECT:FriInmatningKod).
      THIS-OBJECT:RibbonToolKodSokning:ToolClick:Subscribe(THIS-OBJECT:SokningKod).
      
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder).
      
      THIS-OBJECT:RibbonToolMatris:ToolValueChanged:Subscribe(UpdateMatrisE).
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolClick:Subscribe(THIS-OBJECT:GruppMatrisChanged).
      THIS-OBJECT:RibbonToolVisMatris:ToolValueChanged:Subscribe(VisUpdateMatrisE).
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:Subscribe(SparaDefs).
      THIS-OBJECT:RibbonToolManual:ToolClick:Subscribe(Manual).
      /* splatter */
      THIS-OBJECT:LoadRibbonLevs().
     
      RibbonToolVisArbKalk:SharedPropsInternal:Enabled = TRUE.
     
   END METHOD.
   
   METHOD PUBLIC VOID Manual(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE path AS CHARACTER.
      path = SEARCH("Modules\Kalkyl\kalkmanual.pdf").
      System.Diagnostics.Process:Start(path).
   END METHOD.
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar) .
   END METHOD.

   METHOD PUBLIC VOID VisAnmarkningChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = THIS-OBJECT:RibbonToolVisAnmarkning:Checked.
   END METHOD.
   
   METHOD PUBLIC VOID VisPriserChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisPriser:ValueList:SelectedIndex]*/
     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "0" THEN DO:
         /*IF THIS-OBJECT:RibbonToolVisPriser:Text EQ "Enligt katalog" THEN DO:*/
         THIS-OBJECT:RibbonToolVisMatSpec:CHECKED = FALSE.
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = TRUE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RestoreFriKoder(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchFriKoder(THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar).
      THIS-OBJECT:KalkControl:GridFriKalk:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID SokningKod(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKodSokning:Checked EQ TRUE THEN DO:
        THIS-OBJECT:RibbonToolAngeKodAntal:Checked = FALSE.
        THIS-OBJECT:KalkControl:SplitContainerKalkArbkoder:Panel1Collapsed = TRUE.
        THIS-OBJECT:KalkControl:splitContainerLopInAntal:Panel2Collapsed = FALSE.
        THIS-OBJECT:KalkControl:panelKodSok:Visible = TRUE.
        THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Columns["ARBKOD"]:Hidden = FALSE.
        THIS-OBJECT:Root:WindowManager:Wnd:ActiveControl = THIS-OBJECT:KalkControl:KodSokInput.
        THIS-OBJECT:KalkControl:KodSokInput:Select().
        THIS-OBJECT:KalkControl:KodSokInput:Focus().
      END.
      ELSE DO:
        IF THIS-OBJECT:RibbonToolAngeKodAntal:Checked EQ FALSE THEN DO:
            THIS-OBJECT:KalkControl:splitContainerLopInAntal:Panel2Collapsed = TRUE.
        END.
          
        THIS-OBJECT:KalkControl:SplitContainerKalkArbkoder:Panel1Collapsed = FALSE.
        THIS-OBJECT:KalkControl:panelKodSok:Visible = FALSE.  
        THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Columns["ARBKOD"]:Hidden = TRUE.
      END.
      THIS-OBJECT:RefreshArbKoder(?,?).
   END METHOD.
   
   METHOD PUBLIC VOID ArbetsKalkylGom(INPUT sender AS System.Object, INPUT args AS System.EventArgs): 
      IF THIS-OBJECT:RibbonToolVisArbKalk:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:toolStripButtonSkrivUt:VISIBLE = FALSE.
         THIS-OBJECT:KalkVisaControl:toolStripButtonExporteraExcel:VISIBLE = FALSE.
      END.
      ELSE DO:
         IF THIS-OBJECT:KalkVisaControl:VisaExcel:OutExterntWin = TRUE OR THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE THEN DO:
            THIS-OBJECT:KalkVisaControl:toolStripButtonExporteraExcel:Visible = FALSE.
            THIS-OBJECT:KalkVisaControl:toolStripButtonSkrivUt:Visible = FALSE.
         END.
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:toolStripButtonExporteraExcel:Visible = TRUE.
            THIS-OBJECT:KalkVisaControl:toolStripButtonSkrivUt:Visible = TRUE.
         END.  
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID FriInmatningKod(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolAngeKodAntal:Checked EQ TRUE THEN DO:
          THIS-OBJECT:KalkControl:panelFriIn:Visible = TRUE.
         THIS-OBJECT:RibbonToolKodSokning:Checked = FALSE.
         THIS-OBJECT:KalkControl:splitContainerLopInAntal:Panel2Collapsed = FALSE.
         
        
         /*Dummy-lösning för att hantera bugg för FriIn när kalkylen är ny (16 apr 2014 15:12:51)*/
         IF friindummy = FALSE THEN DO:
            THIS-OBJECT:TabManager:Tabs["Volymber"]:Selected = TRUE.
            THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
            friindummy = TRUE.
         END.   

        THIS-OBJECT:Root:WindowManager:Wnd:ActiveControl = THIS-OBJECT:KalkControl:fillinFriInKod.
        THIS-OBJECT:KalkControl:fillinFriInKod:Select().
        THIS-OBJECT:KalkControl:fillinFriInKod:Focus().
         
      END.    
      ELSE DO:
        IF THIS-OBJECT:RibbonToolKodSokning:Checked EQ FALSE THEN DO:
            THIS-OBJECT:KalkControl:splitContainerLopInAntal:Panel2Collapsed = TRUE.
        END.
        THIS-OBJECT:KalkControl:panelFriIn:Visible = FALSE.
        
      END.
      
      
   END METHOD.
   METHOD PUBLIC VOID SplitBildVisa():
      IF THIS-OBJECT:RibbonToolKatalogKom:Checked EQ TRUE OR THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE OR 
      THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN DO: 
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel2Collapsed = TRUE.
      END.   
      ELSE DO:
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel1Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel2Collapsed = FALSE.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID KatsKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
       IF THIS-OBJECT:RibbonToolKatalogKom:Checked EQ TRUE THEN DO:
         
         THIS-OBJECT:RibbonToolKoderAnm:Checked              = FALSE. 
         THIS-OBJECT:RibbonToolFrekvensKom:Checked           = FALSE. 
         THIS-OBJECT:KalkControl:ValdKodKommentar:VISIBLE                       = TRUE.
         THIS-OBJECT:KalkControl:RubrikKom:VISIBLE                              = TRUE.
         THIS-OBJECT:KalkControl:Rubrikanm:VISIBLE                              = FALSE.
         THIS-OBJECT:KalkControl:KalkylensKoderAnmarkning:VISIBLE               = FALSE.
        
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel1Collapsed = TRUE. 
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel2Collapsed = FALSE.
      END.
      THIS-OBJECT:SplitBildVisa().   
      
   END METHOD.
   METHOD PUBLIC VOID FrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         THIS-OBJECT:RibbonToolKoderAnm:Checked                     = FALSE.
         THIS-OBJECT:RibbonToolKatalogKom:Checked                     = FALSE. 
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel1Collapsed = FALSE. 
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel2Collapsed = TRUE.
         
      END.   
      THIS-OBJECT:SplitBildVisa().
      
     
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN DO:
         THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE.
         THIS-OBJECT:RibbonToolKatalogKom:Checked = FALSE.
        
         IF THIS-OBJECT:KoderTTh:AVAILABLE THEN DO:
             THIS-OBJECT:KalkControl:Rubrikanm:Text = "Anmärkning för " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99").
         END.
         ELSE THIS-OBJECT:KalkControl:Rubrikanm:Text = "Anmärkning".
         THIS-OBJECT:KalkControl:splitContainerKalkKomAnm:VISIBLE = TRUE.
         THIS-OBJECT:KalkControl:KalkylensKoderAnmarkning:VISIBLE = TRUE.   
         THIS-OBJECT:KalkControl:ValdKodKommentar:VISIBLE = FALSE.
         THIS-OBJECT:KalkControl:RubrikKom:VISIBLE                              = FALSE.
         THIS-OBJECT:KalkControl:Rubrikanm:VISIBLE                              = TRUE.  
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel1Collapsed = TRUE. 
         THIS-OBJECT:KalkControl:splitContainerFrekAnmKom:Panel2Collapsed = FALSE.
      END.   
      THIS-OBJECT:SplitBildVisa().        
   END METHOD.
   
   METHOD PUBLIC VOID FriKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     
      THIS-OBJECT:KalkControl:GridKalkylKoder_AfterSelectChange(?,?).
      IF THIS-OBJECT:RibbonToolFriKalk:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkControl:toolStripAllaKoder:VISIBLE = FALSE.
         THIS-OBJECT:KalkControl:toolStripFriKalk:VISIBLE = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = FALSE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = FALSE. 
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 1.
         IF THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE  THEN.
         ELSE DO:
            System.Windows.Forms.MessageBox:Show("Du kan bara gå till Frikalkyl via fliken Kakyl!").
            THIS-OBJECT:RibbonToolFriKalk:Checked = FALSE.
            RETURN.
         END.
         THIS-OBJECT:KalkControl:GridFriKalk:GuruReopen().
         THIS-OBJECT:KalkControl:splitContainerKalk:Panel1Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkKodoFrek:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkFri:Panel2Collapsed = FALSE.
         THIS-OBJECT:VisaTabs("FRIKALK", FALSE).
         THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:VISIBLE = FALSE.
         THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:VISIBLE = FALSE.
         /*    
         THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = TRUE.
         */
      END.
      ELSE DO:
         THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = TRUE.
         IF Guru.Konstanter:globanv = "elpao" THEN DO:
            THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = FALSE.
            THIS-OBJECT:KalkControl:toolStripAllaKoder:VISIBLE = TRUE. 
            THIS-OBJECT:KalkControl:toolStripFriKalk:VISIBLE = FALSE.
         END.    
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 0.
         THIS-OBJECT:KalkControl:splitContainerKalkKodoFrek:Panel2Collapsed = FALSE.
         THIS-OBJECT:KalkControl:splitContainerKalkFri:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalk:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
         THIS-OBJECT:VisaTabs("FRIKALK", TRUE).
         /*
         THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = FALSE.
         */
         THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:KoderAnmChanged(?,?).       
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID GruppMatrisChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:RibbonToolVisGroupMatris:Checked EQ TRUE THEN DO:
         THIS-OBJECT:RibbonToolVisMatris:Text = "Alla".         
      END.
      ELSE DO:
         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KalkVisaControl:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID SparaExcelE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KalkVisaControl:VisaExcel:SaveFileWithDialog().
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtGrid(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KalkStatusControl:GridStatus:SkrivUt(). 
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning():
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
      Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
      CREATE QUERY mQ.
      mQ:SET-BUFFERS(THIS-OBJECT:MarkningTTh).
      mQ:QUERY-PREPARE(Q).
      mQ:QUERY-OPEN().
      mQ:GET-FIRST().
      DO WHILE mQ:QUERY-OFF-END = FALSE:
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add(THIS-OBJECT:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE).
         mQ:GET-NEXT().
      END.
      mQ:QUERY-CLOSE().
      THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[1].
      THIS-OBJECT:RefreshArbKoder(?,?).
   /*THIS-OBJECT:WasMarkningMade = TRUE.*/
   /*END.*/
   END METHOD.
   
   METHOD PUBLIC CHARACTER GetVisaMarkning():
      RETURN THIS-OBJECT:RibbonToolFilterTyp:Text.
   END METHOD.
    
   METHOD PUBLIC VOID RefreshArbKoder(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE Typ        AS INTEGER   NO-UNDO INITIAL 1.
      DEFINE VARIABLE Markning   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE DoAll      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoUnmarked AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoReturn   AS LOGICAL   INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE DoSokning AS LOGICAL INITIAL FALSE NO-UNDO.
      Typ = THIS-OBJECT:GetVisaTyp().
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      THIS-OBJECT:FriInSet().
      
      IF Markning EQ "Alla" OR Markning EQ "" THEN
        DoAll = TRUE.
      ELSE
        DoAll = FALSE.
      
      IF Markning EQ "Ej märkta" THEN
        DoUnmarked = TRUE.
      ELSE
        DoUnmarked = FALSE.
        
      IF THIS-OBJECT:RibbonToolKodSokning:Checked EQ TRUE THEN DO:
        DoSokning = TRUE.
      END.
        
      THIS-OBJECT:KalkControl:GridLopKoder:GuruAvmarkera().
      
     
      IF DoAll EQ TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ)).
         THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFirstrow().
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = ''").
            THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFirstrow().
         END.
         ELSE DO:
            THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "'").
            THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFirstrow().
         END.
         IF THIS-OBJECT:KalkControl:GridArbetsKoder:tBS:Count EQ 0 THEN DO:
            THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
            DoReturn = TRUE.
         END.      
      END.
      IF DoReturn EQ TRUE THEN RETURN.
      IF THIS-OBJECT:ArbetsKoderTTh:AVAILABLE EQ TRUE THEN DO:
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TYPKALK = " + STRING(Typ)) NO-ERROR.
         IF DoSokning EQ TRUE THEN DO:
            /*THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND BENAMNING MATCHES ~"*" + THIS-OBJECT:KalkControl:KodSokInput:Text + "*~"").*/
            IF DoAll EQ TRUE THEN DO:
               THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND " + Helpers.Functions:SkapaMultiSok(THIS-OBJECT:KalkControl:KodSokInput:Text, "BENAMNING")).
               THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
            END.
            ELSE DO:
               IF DoUnmarked EQ TRUE THEN DO:
                  THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '' AND " + Helpers.Functions:SkapaMultiSok(THIS-OBJECT:KalkControl:KodSokInput:Text, "BENAMNING")).
                  THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
               END.
               ELSE DO:
                  THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "' AND " + Helpers.Functions:SkapaMultiSok(THIS-OBJECT:KalkControl:KodSokInput:Text, "BENAMNING")).
                  THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().               
               END.
            END.
         END.
         ELSE DO:
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         END. 
      END.
      ELSE DO:
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         IF THIS-OBJECT:KalkylLoaded EQ TRUE THEN DO:
            System.Windows.Forms.MessageBox:Show("Inga arbetskoder funna!").
         END.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID UpdateRibbonShow():
      THIS-OBJECT:RibbonToolInfTyp:Text = THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:Text.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      i = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count = 0 THEN DO:
         
      END.
      ELSE DO WHILE i < THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count :
         IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[i]:DisplayText = THIS-OBJECT:RibbonToolInfTyp:Text THEN DO:
            THIS-OBJECT:RibbonToolVisaTyp:SelectedItem = THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[i].
            RETURN.
         END.
         i = i + 1.
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID LoadRibbonLevs():
      DEFINE VARIABLE w     AS Infragistics.Win.ValueListItem NO-UNDO.
      DEFINE VARIABLE wData AS Helpers.ABLDataContainer       NO-UNDO.
      DEFINE VARIABLE qH    AS HANDLE                         NO-UNDO.
      DEFINE VARIABLE sw    AS Integer                        NO-UNDO INITIAL 0.
      DEFINE VARIABLE ssw   AS Integer                        NO-UNDO.
      THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Clear().
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.Konstanter:LeverantorTTh, "FOR EACH " + Guru.Konstanter:LeverantorTTh:TABLE + " NO-LOCK").
      qH:GET-FIRST().
      DO WHILE qh:QUERY-OFF-END = FALSE:
         w = NEW Infragistics.Win.ValueListItem().
         w:DisplayText = STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE()).
         wData = NEW Helpers.ABLDataContainer().
         wData:CreateData("LEVKOD", STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE())).
         w:DataVALUE = wData.
         IF STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE()) = THIS-OBJECT:Root:DatabaseManager:Global:HuvudLeverantor THEN DO:
            wData:CreateData("ISHUV", TRUE).
            ssw = sw.
         END.
         ELSE DO:
            wData:CreateData("ISHUV", FALSE).
         END.
         THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Add(w).
         sw = sw + 1 . 
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Add("Alla leverantörer").
      THIS-OBJECT:RibbonToolLev:SelectedIndex = ssw.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
   END METHOD.
   
   /* Ribbon events*/
   METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      
      DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kundvar AS LOGICAL NO-UNDO.
      kundvar = ?.
      IF e:KeyChar  = CHR(13) THEN DO:
         enr = THIS-OBJECT:RibbonToolEnr:Text.
         ben = THIS-OBJECT:RibbonToolSok:Text.
         IF THIS-OBJECT:RibbonToolEndastmarkmtrl:Checked = TRUE THEN kundvar = TRUE.
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruTom().
         THIS-OBJECT:SokMtrl(enr, ben, kundvar).
         IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruOrderby("BY kund DESCENDING by enr").
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruReopen().
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruFirstrow().
         e:Handled = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID SokMtrl(enr AS CHARACTER,ben AS CHARACTER,kundvar AS LOGICAL):
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
      IF STRING(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
      ELSE lev = STRING( CAST(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellVALUE("LEVKOD").*/
      THIS-OBJECT:Root:DatabaseManager:Global:FetchMateriel(lev, enr, ben, kundvar).
      
   END METHOD.
   /*retunerar typ = 1 typ = 2 osv*/
   METHOD PUBLIC INTEGER GetVisaTyp():
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetInteger("Typ").
      END.
      RETURN -1.
   END METHOD.
   /*öppnar arbetskoder ned rätt query för vald typ*/
   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:KalkControl:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         THIS-OBJECT:RefreshMarkning().
      END.
      THIS-OBJECT:FriInSet().
   END METHOD.
   
   METHOD PUBLIC VOID RibbonUpdateUppfPriser(sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE = 1 + THIS-OBJECT:RibbonToolUppfPriser:SelectedIndex.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
   END METHOD.  
    
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkylShell").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
      appearance24 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabFaktorer = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabAvtalskalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKopiera = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabTidlage = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabFelmedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabVolymber = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabStatus = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabKalkylHuvud */
      /*  */
      THIS-OBJECT:TabKalkylHuvud:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKalkylHuvud:Name = "TabKalkylHuvud".
      THIS-OBJECT:TabKalkylHuvud:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabKalkyl */
      /*  */
      THIS-OBJECT:TabKalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKalkyl:Name = "TabKalkyl".
      THIS-OBJECT:TabKalkyl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabEgna */
      /*  */
      THIS-OBJECT:TabEgna:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabEgna:Name = "TabEgna".
      THIS-OBJECT:TabEgna:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabFaktorer */
      /*  */
      THIS-OBJECT:TabFaktorer:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFaktorer:Name = "TabFaktorer".
      THIS-OBJECT:TabFaktorer:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabMateriel */
      /*  */
      THIS-OBJECT:TabMateriel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMateriel:Name = "TabMateriel".
      THIS-OBJECT:TabMateriel:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabImportMallar */
      /*  */
      THIS-OBJECT:TabImportMallar:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabImportMallar:Name = "TabImportMallar".
      THIS-OBJECT:TabImportMallar:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabAvtalskalkyl */
      /*  */
      THIS-OBJECT:TabAvtalskalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabAvtalskalkyl:Name = "TabAvtalskalkyl".
      THIS-OBJECT:TabAvtalskalkyl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabKopiera */
      /*  */
      THIS-OBJECT:TabKopiera:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKopiera:Name = "TabKopiera".
      THIS-OBJECT:TabKopiera:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabKopiera:Paint:Subscribe(THIS-OBJECT:TabKopiera_Paint).
      /*  */
      /* TabTidlage */
      /*  */
      THIS-OBJECT:TabTidlage:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabTidlage:Name = "TabTidlage".
      THIS-OBJECT:TabTidlage:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabFelmedd */
      /*  */
      THIS-OBJECT:TabFelmedd:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFelmedd:Name = "TabFelmedd".
      THIS-OBJECT:TabFelmedd:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabVolymber */
      /*  */
      THIS-OBJECT:TabVolymber:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVolymber:Name = "TabVolymber".
      THIS-OBJECT:TabVolymber:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 553).

      /*  */
      /* TabStatus */
      /*  */
      THIS-OBJECT:TabStatus:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabStatus:Name = "TabStatus".
      THIS-OBJECT:TabStatus:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* imageList1 */
      /*  */
      THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream"), System.Windows.Forms.ImageListStreamer).
      THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:imageList1:Images:SetKeyName(0, "kalkyl-info.jpg").
      THIS-OBJECT:imageList1:Images:SetKeyName(1, "kalkyl-volym.jpg").
      /*  */
      /* TabManager */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance1.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFaktorer).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabMateriel).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabImportMallar).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVolymber).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkylHuvud).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabEgna).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabAvtalskalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFelmedd).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKopiera).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabTidlage).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabStatus).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManager:TabIndex = 44.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance2.
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      ultraTab13:Appearance = appearance3.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:TabKalkylHuvud.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Info".
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance4.
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      ultraTab14:Appearance = appearance5.
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:TabKalkyl.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Kalkylera".
      appearance6:Image = CAST(resources:GetObject("appearance6.Image"), System.Object).
      ultraTab15:ActiveAppearance = appearance6.
      appearance7:Image = CAST(resources:GetObject("appearance7.Image"), System.Object).
      ultraTab15:Appearance = appearance7.
      appearance8:BackColor = System.Drawing.SystemColors:Control.
      ultraTab15:ClientAreaAppearance = appearance8.
      ultraTab15:Key = "Egna koder".
      ultraTab15:TabPage = THIS-OBJECT:TabEgna.
      ultraTab15:Text = "".
      ultraTab15:ToolTipText = "Gör Egna koder".
      appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance9.
      appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
      ultraTab17:Appearance = appearance10.
      ultraTab17:Key = "FaktorerPriser".
      ultraTab17:TabPage = THIS-OBJECT:TabFaktorer.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "Lägg in egna Faktorer/Priser".
      appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance11.
      appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
      ultraTab19:Appearance = appearance12.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget Materiel".
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance13.
      appearance14:BackColor = System.Drawing.Color:Transparent.
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      ultraTab22:Appearance = appearance14.
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "Import/Mallar".
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      ultraTab20:ActiveAppearance = appearance15.
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      ultraTab20:Appearance = appearance16.
      ultraTab20:Key = "Avtalskalkyl".
      ultraTab20:TabPage = THIS-OBJECT:TabAvtalskalkyl.
      ultraTab20:Text = "".
      ultraTab20:ToolTipText = "Avtalskalkyl".
      appearance17:Image = CAST(resources:GetObject("appearance17.Image"), System.Object).
      ultraTab2:ActiveAppearance = appearance17.
      appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
      ultraTab2:Appearance = appearance18.
      ultraTab2:Key = "KopieraKalkyl".
      ultraTab2:TabPage = THIS-OBJECT:TabKopiera.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Kopiera/Konvertera kalkyler".
      appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
      ultraTab3:ActiveAppearance = appearance19.
      appearance20:Image = CAST(resources:GetObject("appearance20.Image"), System.Object).
      ultraTab3:Appearance = appearance20.
      ultraTab3:Key = "Tidlägen".
      ultraTab3:TabPage = THIS-OBJECT:TabTidlage.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Tidlägen".
      appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance21.
      appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
      ultraTab1:Appearance = appearance22.
      ultraTab1:Key = "FelMedd".
      ultraTab1:TabPage = THIS-OBJECT:TabFelmedd.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Felmeddelanden".
      appearance23:Image = CAST(resources:GetObject("appearance23.Image"), System.Object).
      ultraTab24:ActiveAppearance = appearance23.
      appearance24:Image = CAST(resources:GetObject("appearance24.Image"), System.Object).
      ultraTab24:Appearance = appearance24.
      ultraTab24:Key = "Volymber".
      ultraTab24:TabPage = THIS-OBJECT:TabVolymber.
      ultraTab24:Text = "".
      ultraTab24:ToolTipText = "Volymberäkning av schaktmassor".
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance25.
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      ultraTab16:Appearance = appearance26.
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "Visa ".
      appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
      ultraTab4:ActiveAppearance = appearance27.
      appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
      ultraTab4:Appearance = appearance28.
      ultraTab4:Key = "Status".
      ultraTab4:TabPage = THIS-OBJECT:TabStatus.
      ultraTab4:Text = "".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 13 NO-UNDO.
      arrayvar0[1] = ultraTab13.
      arrayvar0[2] = ultraTab14.
      arrayvar0[3] = ultraTab15.
      arrayvar0[4] = ultraTab17.
      arrayvar0[5] = ultraTab19.
      arrayvar0[6] = ultraTab22.
      arrayvar0[7] = ultraTab20.
      arrayvar0[8] = ultraTab2.
      arrayvar0[9] = ultraTab3.
      arrayvar0[10] = ultraTab1.
      arrayvar0[11] = ultraTab24.
      arrayvar0[12] = ultraTab16.
      arrayvar0[13] = ultraTab4.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanged).

      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* KalkylShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "KalkylShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

    METHOD PUBLIC CHARACTER TooltipTextSet(tip AS CHARACTER, framf AS LOGICAL  ):
      
      IF framf = TRUE THEN  RETURN Guru.GlobalaVariabler:KalkArendeText + tip.
      ELSE RETURN tip + LC(Guru.GlobalaVariabler:KalkArendeText).
   END METHOD.
   
   
   /*------------------------------------------------------------------------------
         Purpose:   EFTER DU HAR KLICKAT PÅ DEN NYA.                                                   
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
      
      THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
     
      PROCESS EVENTS.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:   TILL DEN NYA TABBEN                                                   
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.
      
      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabManager:SelectedTab:KEY NO-ERROR.
      
      /*v11.2 fix*/
      
      THIS-OBJECT:GetComboV(KalkHuvudControl:HuvudOmrade).
      THIS-OBJECT:GetComboV(KalkHuvudControl:HuvudUtfardare).
      THIS-OBJECT:GetComboV(KalkAvtalControl:ComboAvtal).
      THIS-OBJECT:GetComboV(KalkKopKonvControl:KopHuvudOmrade).
      THIS-OBJECT:GetComboV(KalkHuvudControl:BestKund).
      
      IF cFrom EQ ? THEN cFrom = "".  
      IF cTo = "Status" THEN DO:
         THIS-OBJECT:RibbonTabVisning:Visible = TRUE.
         THIS-OBJECT:VisaTabs("ALLA", FALSE).
         THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE. 
         RETURN. 
      END.
      IF cFrom = "Kalkylhuvud" THEN DO:        
         IF THIS-OBJECT:NyKalkyl = TRUE THEN DO:
            THIS-OBJECT:ModuleHandle:RibbonContext:Visible = TRUE.
            THIS-OBJECT:Root:WindowManager:Wnd:SelectRibbonTab("Kalkyl").
           
            IF THIS-OBJECT:ApplyCreatedKalkyl() EQ FALSE THEN DO:
               e:Cancel = TRUE.
            END.
            ELSE DO:
               THIS-OBJECT:SparaDefs(?,?).
               IF SlaIhopKalkyler = TRUE THEN DO:
                  
                  THIS-OBJECT:SlaIhopKalkyler().
               END.   
            END.     
                     
         END.
         ELSE DO:
            IF THIS-OBJECT:SaveHuvud() = FALSE THEN DO:
               e:Cancel = TRUE.
            END.   
         END.  
      END.    
      IF cFrom = "Egna koder" THEN DO:
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruTom().
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruTom().
      END.   
      IF cTo = "Visa Kalkyl" THEN DO:
         IF THIS-OBJECT:WasExcelShown = FALSE THEN DO:
            IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 6 THEN DO:
               THIS-OBJECT:GenereraAvtalsKalkyl().            
            END.
            ELSE DO:  
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisArbKalk") = "no" THEN DO: 
                  THIS-OBJECT:VisningKalkIni(OUTPUT path).
                  THIS-OBJECT:GenereraKalkylVisning().
               END.   
               ELSE THIS-OBJECT:KalkVisaControl:GenereraArbKalkylVisning().
               THIS-OBJECT:WasExcelShown = TRUE.   
            END.   
         END.
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.      
      IF cTo = "FaktorerPriser" THEN DO:
      END.      
      IF cTo = "Kalkyl" THEN DO:     
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
         THIS-OBJECT:KalkControl:GridFriKalk:GuruReopen().
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END. 
                
      IF cTo = "Egna koder" THEN DO:

         /*Robin 3/6/13 ändringar till GurufirstRow*/
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruTom().
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruTom().
         
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruReopen().
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFirstRow().
         THIS-OBJECT:KalkEgnaControl:GridUnderKoder:ValueChanged().
         FilterUnderKoder().
         THIS-OBJECT:RibbonTabEgnaKoder:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabEgnaKoder:Visible = FALSE.
      END.      
      IF cTo = "Materiel" THEN DO:
         THIS-OBJECT:RibbonTabMat:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabMat:Visible = FALSE.
      END.
      IF cTo = "ImportMallar" THEN DO:
         THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN.
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:MallarHmt().
            THIS-OBJECT:KalkImpMallControl:GridMallar:GuruReopen().
            THIS-OBJECT:KalkImpMallControl:GridMallar:GuruFirstRow().
         END.   
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.  
      END.
      IF cTo = "Avtalskalkyl" THEN DO:
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 6.
      END.    
      IF cTo = "Volymber" THEN DO:
         THIS-OBJECT:VolymBerTTh:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:VolymBerTTh:AVAILABLE THEN.
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBerHmt().
            THIS-OBJECT:KalkVolymControl:GridVolymBer:GuruReopen().
            THIS-OBJECT:KalkVolymControl:GridVolymBer:GuruFirstRow().
         END.   
      END.
                
      IF cTo = "KopieraKalkyl" THEN THIS-OBJECT:CreateComboKonvTyp().
      IF cTo = "Felmedd" THEN DO:
         THIS-OBJECT:KalkFelmeddControl:GridFelMedd:GuruReopen().
         THIS-OBJECT:KalkFelmeddControl:GridFelMedd:GuruFirstRow().           
      END.
      
      PROCESS EVENTS.
   END METHOD.
   
  METHOD PUBLIC VOID VisningKalkIniMulti():
     DEFINE VARIABLE path  AS CHARACTER NO-UNDO.
     
     THIS-OBJECT:RibbonTabKalkyl:Visible = FALSE.
     THIS-OBJECT:RibbonTabVisningVisa:Visible = FALSE.
     THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = FALSE.
     THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Enabled = FALSE.
    
     THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Enabled = FALSE.
    
     THIS-OBJECT:RibbonToolVisArbKalk:SharedPropsInternal:Enabled = FALSE.
    
     THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.
  END METHOD. 
    
   /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:44:31 
   Sätter UsMsInterrop, skapar en excel fil för skrivning av template 
   startar Interop excel
   */
   
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseInterop(TRUE).
      
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:         
         THIS-OBJECT:SetVisningsBild(0).
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         /*behövs inte
         THIS-OBJECT:KalkVisaControl:VisaExcel:ExcelNamn(path).
         */
         THIS-OBJECT:KalkVisaControl:VisaExcel:InteropInitialize(path). 
         
         

         THIS-OBJECT:ArbetsKalkylGom(?,?).  
      END.   
      ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).    
   END METHOD.
   METHOD PUBLIC VOID Kill().
      
   END METHOD.

   METHOD PRIVATE VOID InitializeForm ():
   /* FINNS I * 2GURUJUNI*/     
   END METHOD.



   METHOD PUBLIC VOID TabRefreshKalkylHuvud():
      
   END METHOD.

   METHOD PRIVATE VOID ValueArbetsKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN  DO:
         THIS-OBJECT:KalkImpMallControl:GridMallarKoder:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:KalkImpMallControl:GridMallarKoder:GuruFirstrow().
      END.
   END METHOD.
   /*används inte?*/
   METHOD PRIVATE INTEGER GetCurrentType():
      RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:ValueList:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer):GetInteger("Typ").
   END METHOD.

   METHOD PUBLIC CHARACTER GetMatrisName():
      DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:HuvudTTh:Buffer-field("TYPKALK"):Buffer-VALUE = 7 THEN mName = "Period".
      ELSE mName = "Matris".
      RETURN mName.
   END METHOD.
   
   
   METHOD PUBLIC VOID ValueLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
     
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmt(INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
         THIS-OBJECT:KalkControl:GridFrekvens:GuruReopen().
         THIS-OBJECT:KalkControl:GridFrekvens:GuruUpdateTitle(" för :" + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).
        
      END.
      IF THIS-OBJECT:RibbonToolKatalogKom:Checked EQ TRUE THEN DO:  
         THIS-OBJECT:KalkControl:RubrikKom:Text = "Kommentar för " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99").
         THIS-OBJECT:KalkControl:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
      END.    
   END METHOD.
   
   METHOD PUBLIC VOID ValueKalkKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      DEFINE VARIABLE q     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE exrid AS RECID     NO-UNDO.
      /*Se filtrera i Grid*/ 
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:Count > 0 THEN DO:
         IF THIS-OBJECT:DontCheck = TRUE THEN.
         ELSE DO: 
           
         END.
         IF KoderTTh:AVAILABLE THEN  DO:
            IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN THIS-OBJECT:KalkControl:Rubrikanm:Text = "Anmärkning för " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99").
            ELSE THIS-OBJECT:KalkControl:RubrikKom:Text = "Kommentar för " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99").
            THIS-OBJECT:KalkControl:KalkylensKoderAnmarkning:Text  = THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
            THIS-OBJECT:KalkControl:ValdKodKommentar:Text = THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
            IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmt(INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
               THIS-OBJECT:KalkControl:GridFrekvens:GuruReopen().
               THIS-OBJECT:KalkControl:GridFrekvens:GuruUpdateTitle(" för :" + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).                                                
          
            END.        
         END.   
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueEgnaKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
      /* ROBIN Avkommenterat 3/6/13: IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 THEN DO:*/
         FilterUnderKoder().
      /*END. */  
   END METHOD.
   
   METHOD PRIVATE VOID ValuePersCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/   
      /*
      THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      */          
   END METHOD.
      
   METHOD PRIVATE VOID VisaTabs (INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[2] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = visavar.
      END.   
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[4] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = visavar.
      END.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Egna koder"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[6] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = visavar.
      END.  
     DEBUGGER:SET-BREAK().
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:KEY = vilka THEN DO:         
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[11] = TRUE THEN DO: 
            THIS-OBJECT:kalkaonrTTh:FIND-FIRST() NO-ERROR.
            IF THIS-OBJECT:kalkaonrTTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN.
               ELSE DO:
                  IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN.
                  ELSE IF STRING(THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE) = "2" THEN DO:
                     IF Guru.konstanter:globforetag = "ELPA" OR Guru.konstanter:globforetag = "GRAN" OR  Guru.konstanter:globforetag = "LIMO" OR  
                     Guru.konstanter:globforetag = "FORS" OR  Guru.konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "Sweo" OR 
                     Guru.Konstanter:globforetag = "LAPP" THEN DO:                       
                        queryvar =  "FOR EACH " + THIS-OBJECT:Avtalskalktth:TABLE. 
                        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Avtalskalktth,queryvar).
                        qH:GET-FIRST().
                        IF THIS-OBJECT:Avtalskalktth:AVAILABLE THEN DO:
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
                           THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
                        END.                                  
                        ELSE DO:
                           KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchAvtal(INPUT THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE ).                                               
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
                           qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Avtalskalktth,queryvar).
                           qH:GET-FIRST().
                           IF THIS-OBJECT:Avtalskalktth:AVAILABLE THEN DO:
                              THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
                              THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Avtalskalkyl")).
                              THIS-OBJECT:CreateComboAvtal().
                           END.
                           DO WHILE qH:QUERY-OFF-END = FALSE:
                              IF THIS-OBJECT:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE = THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE THEN DO:
                                 THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE = STRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE).                              
                              END.   
                              qH:GET-NEXT().
                           END.
                           THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).                          
                        END.
                     END.
                  END.
               END.
            END.            
         END.   
      END.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Materiel"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["ImportMallar"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Felmedd"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:VISIBLE   = visavar.   
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Volymber"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = visavar.
      /*
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Tidlägen"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = visavar.                    
      */
      IF THIS-OBJECT:HuvudTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
            THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE = FALSE. 
            THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
         END.
      END.     
      IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN DO:
         THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = FALSE.
         IF THIS-OBJECT:NyKalkyl = TRUE THEN THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE.
         ELSE THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = TRUE. 
      END. 
      ELSE DO:
         THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Status"]:VISIBLE   = FALSE.
      END.    
       
   END METHOD.   

   METHOD PUBLIC VOID SetVisningsBild(i AS INTEGER):
      /* 
         0 = inget genererat än
         1 = genererar
         2 = resultat
      */
      IF i EQ 0 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Visible = TRUE.
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 1 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Visible = TRUE.
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 2 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Visible = TRUE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:KalkVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
   END METHOD.

   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      /*Anders Olsson Elpool i Umeå AB  17 jun 2014 12:05:28 
      Används inte 
      */
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 6 THEN DO:
         THIS-OBJECT:GenereraAvtalsKalkyl().
         RETURN.
      END.   
     
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:Visible EQ TRUE THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisArbKalk") = "no" THEN DO: 
               THIS-OBJECT:VisningKalkIni(OUTPUT path).
               THIS-OBJECT:GenereraKalkylVisning().
            END.
            ELSE THIS-OBJECT:KalkVisaControl:GenereraArbKalkylVisning().
         END.
      END.
      ELSE DO:
          IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisArbKalk") = "no" THEN DO: 
            THIS-OBJECT:VisningKalkIni(OUTPUT path).
            THIS-OBJECT:GenereraKalkylVisning().
         END.
         ELSE THIS-OBJECT:KalkVisaControl:GenereraArbKalkylVisning().      
      END.
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = FALSE.
      /*behövs inte
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseInterop(TRUE).
      */
      OS-DELETE VALUE(ExelKommando) NO-ERROR.
      IF vad = "VKALK" THEN DO:
         path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xlsx").
         IF path = ? THEN path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
      END.   
     
      path = THIS-OBJECT:ExcelTemplate(INPUT path).
      ExelKommando = path.            
   END METHOD.
   
   METHOD PUBLIC VOID GenereraKalkylVisning():
      THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel1Collapsed = FALSE.
      THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel2Collapsed = TRUE.
      DEFINE VARIABLE utrustrubrik AS LOGICAL NO-UNDO.
      DEFINE VARIABLE path              AS CHARACTER NO-UNDO.
      DEFINE VARIABLE totbermtrl        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE findfel           AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH             AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE link              AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rubrikrad         AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE radefterrubrikrad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE matrisrader       AS CHARACTER NO-UNDO.   
      DEFINE VARIABLE styckpris         AS DECIMAL NO-UNDO.
      DEFINE VARIABLE kostnadstartcol   AS CHARACTER NO-UNDO.      
      DEFINE VARIABLE excelvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE excelvardeDecimal AS DECIMAL NO-UNDO.
      DEFINE VARIABLE kalkylqueryvar AS CHARACTER NO-UNDO.
      /*DEFINE VARIABLE utsmetkost        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE projledkost        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE monttottimusmet        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE totkostusmet        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE kodprojledkost AS DECIMAL NO-UNDO.
      DEFINE VARIABLE allapris AS DECIMAL NO-UNDO.
      DEFINE VARIABLE allakodprojledkost AS DECIMAL NO-UNDO.
      DEFINE VARIABLE kodutsmet AS DECIMAL NO-UNDO.*/
      DEFINE VARIABLE sustcol AS CHARACTER NO-UNDO.
      
      path = ExelKommando.
      ASSIGN 
      cSenasteArbKod     = ""
      iSenasteLopNr      = 0        
      iSenasteMatris     = 0        
      cSenasteMtrl       = ""       
      iSenasteMtrlMatris = 0.
   DEBUGGER:SET-BREAK().
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         IF PROVERSION BEGINS "10" THEN DO:
            Guru.Konstanter:EuropeanSet().            
         END.
         ELSE DO:
            Guru.Konstanter:AmericanSet().
         END.   
         
      END.
      ELSE DO:
         Guru.Konstanter:AmericanSet().
         /*
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         */
      END.   
      THIS-OBJECT:WasExcelShown = TRUE.
      
      /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i slutet också! */
  /*    DEFINE VARIABLE TimerTest AS System.Diagnostics.Stopwatch NO-UNDO.
      TimerTest = NEW System.Diagnostics.Stopwatch().
      TimerTest:Reset().
      TimerTest:Start().*/
     
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum = 1.
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:ScreenUpp(FALSE).
         THIS-OBJECT:SetVisningsBild(1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:SlutOutExterntWin().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Reload(path).         
      END.  
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcel(path).
    
      
      
     /*Anders Olsson Elpool i Umeå AB  6 okt 2014 17:47:02 
        /*INGA FAKTORER OM INTE GRUNDKALKYL*/
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) > 0 THEN DO:
         THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE.
         THIS-OBJECT:RibbonToolVisEgnaPriser:CHECKED = FALSE.          
      END. 
      
     */
      /*INTE GRUPPERA OM BARA EN MATRIS*/
      IF VisMatris() = 0 THEN.
      ELSE DO:
         THIS-OBJECT:RibbonToolVisGroupMatris:CHECKED = FALSE.
      END.   
      /*BERÄKNA KALYLEN*/
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:kalkrubrikTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkrubrikTTh.
      
      {LOGGORc.I}
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      IF link NE ? THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:PageSetUppHeaderImage(INPUT 1, INPUT link). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      END.
      
      
     
      Guru.Konstanter:JurAvdTTh:FIND-FIRST("WHERE AVDELNINGNR = " + Guru.Konstanter:OmradeTTh:BUFFER-FIELD("AVDELNINGNR"):BUFFER-VALUE) NO-ERROR.
      IF Guru.Konstanter:JurAvdTTh:AVAILABLE THEN DO:
         Guru.Konstanter:JurPersTTh:FIND-FIRST("WHERE JUDID = '" + STRING(Guru.Konstanter:JurAvdTTh:BUFFER-FIELD("JUDID"):BUFFER-VALUE) + "'") NO-ERROR.         
      END.
      
      IF Guru.Konstanter:JurPersTTh:AVAILABLE THEN DO:                             
         THIS-OBJECT:KalkVisaControl:VisaExcel:PageSetUppHeader(INPUT 2, INPUT '&C&",Bold"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + CHR(10) +
         '&",Normal"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("GATUADR"):BUFFER-VALUE) + "  " + CHR(10) +
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTNR"):BUFFER-VALUE,"xxx xx") + " " + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTADR"):BUFFER-VALUE) + CHR(10) + 
         "Telefon:" + Guru.Konstanter:JurPersTTh:BUFFER-FIELD("TELVXL"):BUFFER-VALUE + CHR(10)).
      END. 
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:PageSetUppHeader(INPUT 3, INPUT '&"Calibri,Bold"&20' + THIS-OBJECT:RibbonToolVisVisa:Text).
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      /* Skapa huvud */
      IF vismulti = TRUE THEN THIS-OBJECT:ExcelHuvMulti().
      ELSE THIS-OBJECT:ExcelHuv().    
      /*ANMÄRKNING*/
      IF vismulti = TRUE THEN ExcelAnmMulti().
      ELSE THIS-OBJECT:ExcelAnm().
      /* --- Börja göra listan här --- */
      /* Skapa rubrikerna */
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      rubrikrad = THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad. 
     
      
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().  
      radefterrubrikrad = THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad.
      matrisrader = radefterrubrikrad.
      THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdup().  
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kod").
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Antal").
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Enh").
      /*LÄGG UT RUBRIKER FÖR TIMMAR*/
      utrustrubrik = FALSE.
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Utrust" THEN utrustrubrik = TRUE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      kostnadstartcol = "".
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.
         IF kostnadstartcol = "" THEN DO:
            IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "EA" THEN.
            ELSE kostnadstartcol = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.
         END.   
         qH:GET-NEXT(). 
      END.
      
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      /*rubrik*/
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa").
      IF Guru.Konstanter:varforetypchar[8] = "1"  THEN DO:         
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa/st").       
         sustcol = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.           
         
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Utsmetkost().
         /*THIS-OBJECT:Root:DatabaseManager:Kalkyl:Utsmetkost(OUTPUT utsmetkost,OUTPUT projledkost,OUTPUT monttottimusmet,OUTPUT totkostusmet).*/
         
      END.   
      
      
      
      /* Gör rubriker bold */
      THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:Border("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
      THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
      
      /*OM MATRIS*/
      IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE. 
      ELSE DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE + " WHERE kalknumtt.MATRIS = " + STRING(VisMatris()).      
      END.    
      kalkylqueryvar = queryvar.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.       
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().      
         /*    GRUPPERA MATRIS*/
            
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO: 
            THIS-OBJECT:ExcelGruppMatris(INPUT 1, INPUT matrisrader, INPUT kostnadstartcol ).
            IF iSenasteMatris = 0 THEN matrisrader = matrisrader.
            ELSE IF iSenasteMatris NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE THEN DO:
               matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad))).
            END.   
            /*OM NY MATRIS*/
            iSenasteMatris = THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
         END.
         /*GRUPPERA KODER*/
         
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppera") = "yes" THEN DO:
            THIS-OBJECT:ExcelGruppKoder().         
         END.
         /*SUMMERA SAMMA KOD*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes" THEN DO:
            IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         END.    
         ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         /*UT MED ARB KOD MM*/  
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         /*
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE),"0",TRUE).
         */
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
         /*UT MED TIMMARNA*/
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE kalkantal.KALKNR = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) +
         " AND kalkantal.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + 
         " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
       
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE),"0",TRUE).                 
               END.
               ELSE DO:   
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE),"0",TRUE).                                
               END.
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         
         /*UT MED KOSTNAD*/
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.KALKNR = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + 
         " AND kalkkostnad.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
            
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE),"0",TRUE).                  
               END.
               ELSE DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE),"0",TRUE).                  
               END.
                   
            END.
            /*UT MED EA*/
            THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "EA"') NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRIEAMANGD"):BUFFER-VALUE),"0",TRUE).                  
               END.
               ELSE DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE),"0",TRUE).                 
              END.                                 
            END.
            IF Guru.Konstanter:varforetypchar[8] = "1"  THEN DO:                                                   
               /*MESSAGE THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE THIS-OBJECT:KoderTTh:BUFFER-FIELD("STFRITOTKOST"):BUFFER-VALUE sustcol 
               VIEW-AS ALERT-BOX.*/
               IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Å" OR THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Ä" OR THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Ö" THEN .
               ELSE DO:
                  IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:            
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,sustcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("STFRITOTKOST"):BUFFER-VALUE),"0",TRUE).                  
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,sustcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("STTOTKOST"):BUFFER-VALUE),"0",TRUE).                             
                 END.
              END.                                             
            END.
                                                
            subqH:GET-NEXT(NO-LOCK).
         END.           
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).         
         /*SUMMERA HORISONT*/
         THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"0").
         /*RISKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 2 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4  THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:KalkVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"0").               
            END.   
         END.   
         /*VINSTKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 3 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:KalkVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"0").                  
            END.   
         END.  
         qH:GET-NEXT(NO-LOCK).
      END.
      /*GRUPPERA MATRIS*/     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
         THIS-OBJECT:ExcelGruppMatris(INPUT 2,INPUT matrisrader, INPUT kostnadstartcol).       
         matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad)) + 1).                                
      END.
      /*EGET MATRIEL SUMMA*/
        
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN.
      ELSE DO:        
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
            THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  IF VisMatris() = 0 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + 
                     STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(?))).
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),
                     STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(VisMatris()))).
                  END.
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + 
                  STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
                  
               END.                       
            END.            
         END.
      END.
      
      /*Berednings mtrl*/   
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + 
               STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0"). 
              
            END.                   
         END.           
      END.
     
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      
      /*KOLUMNBREDD*/
      THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      DEFINE VARIABLE colnumvar AS INTEGER NO-UNDO.
      colnumvar = 1.
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),17).
      colnumvar = colnumvar  + 1.     
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),40).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),14).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),5).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),13). /*H*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),13). /*n*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),18). 
      colnumvar = colnumvar  + 1.
      IF Guru.Konstanter:varforetypchar[8] = "1"  THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),18). 
         colnumvar = colnumvar  + 1.
      END.   
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:BordHoriz(radefterrubrikrad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:KalkVisaControl:VisaExcel:BordHorizW(radefterrubrikrad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:BordVert(radefterrubrikrad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:KalkVisaControl:VisaExcel:BordVertW(radefterrubrikrad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      /*SUMMERA ALLA KOLUMNER*/
      THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
      THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
     
      REPEAT:
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:SummaM(THIS-OBJECT:KalkVisaControl:VisaExcel:cColname + THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"Summa","A", FALSE,"0",TRUE).
         END.
         ELSE DO:   
            THIS-OBJECT:KalkVisaControl:VisaExcel:SummaV(THIS-OBJECT:KalkVisaControl:VisaExcel:cColname + THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"0",TRUE).
         END.
         IF THIS-OBJECT:KalkVisaControl:VisaExcel:iColnum = THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum THEN DO: 
            excelvarde = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
            excelvarde = STRING(INTEGER(excelvarde),"->>,>>>,>>>,>>>,>>9").
            excelvarde = REPLACE(excelvarde,","," ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"#,##0"). 
            
            THIS-OBJECT:KalkVisaControl:VisaExcel:Center(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,TRIM(excelvarde)).
            
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Kalkylsumma:") > 0 THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Center("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Kalkylsumma:")),4).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Kalkylsumma:")),TRIM(excelvarde)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Kalkylsumma:")),"SEK").
            END.
            LEAVE.
         END.   
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      END.              
      
      /*ANDERS
      Elpool.Helpers:GetRange(THIS-OBJECT:KalkVisaControl:VisaExcel:GetWorkSheet(),rubrikrad , "C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ):Merge( true ).
      */
      
      /*FONTER*/
      THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",16,? ,0,0 ).
      THIS-OBJECT:KalkVisaControl:VisaExcel:Border(rubrikrad, THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)). 
      /*Anders Olsson Elpool i Umeå AB  27 apr 2015 11:17:38 
      Inga bilagor om multivisning 
      */
      IF vismulti = TRUE THEN.
      ELSE DO: 
         /*bilaga anm kod*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmUtf") = "yes" THEN DO:
            THIS-OBJECT:ExcelBilagor(7, kalkylqueryvar).
         END.
         /*BILAGA EGNA PRISER*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
            THIS-OBJECT:ExcelBilagor(1, kalkylqueryvar).
         END.
         /*BILAGA EGNA FAKTORER*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO: 
            THIS-OBJECT:ExcelBilagor(2, kalkylqueryvar).       
         END.   
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth("C1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth("A1")).
         END.   
         /*BILAGA EGNet mtrl*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisMatSpec") = "yes" THEN DO:
            THIS-OBJECT:ExcelBilagor(3, kalkylqueryvar).      
         END.   
         /*BILAGA mtrl från ber*/
         IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
            THIS-OBJECT:ExcelBilagor(4, kalkylqueryvar).      
         END.
         /*BILAGA frekvens*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFrekvens") = "yes" THEN DO:
            THIS-OBJECT:ExcelBilagor(5, kalkylqueryvar).      
         END.   
         /*BILAGA kommentarer*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisKommentar") = "yes" THEN DO:
            THIS-OBJECT:ExcelBilagor(6, kalkylqueryvar).      
         END.
      END.   
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:ScreenUpp(TRUE).
      /*allt skrivs i arket*/
      THIS-OBJECT:KalkVisaControl:VisaExcel:valueDataOutTT().
     
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:SidBrytBredd(2).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ViewType(INPUT 3).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ExcelAppHandle:ActiveWorkbook:Activate().
         /*behövs här för att ta bort ful ramen*/
         THIS-OBJECT:KalkVisaControl:VisaExcel:Fix().
         THIS-OBJECT:SetVisningsBild(2).
         /*
         THIS-OBJECT:KalkVisaControl:VisaExcel:SlutOutExterntWin().
         */
      END.   
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
      
      Guru.Konstanter:EuropeanAmericanReset().
      
      IF vismulti = TRUE OR kalkylisinaktiv = TRUE THEN DO:
         IF Guru.Konstanter:alltidmax = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeMax().
         ELSE THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeNormal().
         /*
         THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeMax().
         IF Guru.Konstanter:alltidmax = FALSE THEN THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeNormal().
         */
      END.  
      
   END METHOD.
  
   METHOD PRIVATE VOID ExcelHuv():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      DEFINE VARIABLE koppladbest AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Katalog").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(KatalogTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().             
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.GlobalaVariabler:KalkArendeText + "-nr.: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonr(OUTPUT koppladtill,OUTPUT koppladbest).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkNrvar) + " " + koppladtill). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.Konstanter:gbestk).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(koppladbest).
         
         IF VisMatris() = 0 THEN. 
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(GetMatrisName() + " " + STRING(VisMatris())).
         END. 
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
         IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN.
         ELSE DO: 
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkylsumma: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkyltyp: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:RibbonToolInfTyp:Text)).
         END.   
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.Konstanter:gomrl + ": ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(THIS-OBJECT:Omradevar) + "'") NO-ERROR.
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:Omradevar) + " " + STRING(Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
       
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkylansv.: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
         IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE) + " " + STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE)).
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().      
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelHuvMulti():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         /*
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:RibbonToolVisVisa:Text).
         */
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().             
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.GlobalaVariabler:KalkArendeText + "-nr.: ").        
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaKalkNrMulti()).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonrMulti()).
               
         IF VisMatris() = 0 THEN. 
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(GetMatrisName() + " " + STRING(VisMatris())).
         END. 
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
         IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN.
         ELSE DO: 
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkylsumma: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkyltyp: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Typ " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
         END.   
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.Konstanter:gomrl + ": ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaKalkOmrMulti()).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().      
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelAnm():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelAnmUtf():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      END.
   END METHOD.
   
   
   
   METHOD PRIVATE VOID ExcelAnmMulti():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaKalkAnmMulti()).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppMatris(vad AS INTEGER,matrisrader AS CHARACTER,kostnadstartcol AS CHARACTER):
      IF vad = 1 THEN DO:
         /*LÄGGER UT MATRIS RADEN*/
         IF VisGruppMatris(THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN DO:
            /*EGET MATRIEL SUMMA*/
            
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
               THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
               THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
                  THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
                  IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
                  END.              
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
                                
               END.
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().            
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName() + " " + STRING(iSenasteMatris)). 
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            /*SUMMERA ALLA KOLUMNER*/
            REPEAT:
               THIS-OBJECT:KalkVisaControl:VisaExcel:SummaV(THIS-OBJECT:KalkVisaControl:VisaExcel:cColname + THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(matrisrader), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"0",TRUE).
               
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:iColnum = THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum THEN LEAVE.
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                                     
         END.
      END.
      /*SISTA MATRISEN?*/
      IF vad = 2 THEN DO:
         /*EGET MATRIEL SUMMA*/
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2" 
            THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
               END.              
               THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
               
            END.
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().            
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName()+ " " + STRING(iSenasteMatris)). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         /*SUMMERA ALLA KOLUMNER*/
         REPEAT:
            THIS-OBJECT:KalkVisaControl:VisaExcel:SummaV(THIS-OBJECT:KalkVisaControl:VisaExcel:cColname + THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(matrisrader), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"0",TRUE).
            
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:iColnum = THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         END.  
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().                
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppKoder():
      IF VisGruppera(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = FALSE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING("Egna Koder")).
         END.   
         ELSE DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
         END.
      END.      
   END METHOD.
   
   METHOD PRIVATE VOID ExcelBilagor(vad AS INTEGER, kalkylqueryvar AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      IF vad = 1 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Egna priser").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Pris").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Egna priser").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Pris").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:EgnaPriserTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:EgnaPriserTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            subqH:GET-NEXT(NO-LOCK).
         END.       
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 2 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Egna faktorer").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Egna faktorer").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:FaktorerTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:FaktorerTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:FaktorerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 3 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("MaterielSpecifikation").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Antal").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(GetMatrisName()).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         END.   
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Pris").
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " BY MATRIS BY ENR". 
         END.
         ELSE IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " BY ENR".
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:kalktmtrlTTh:TABLE + " WHERE MATRIS = " + STRING(VisMatris()) + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalktmtrlTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.
            ELSE DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.   
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE)).         
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE)).        
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalktmtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).            
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 4 THEN DO:
         cSenasteMtrl       = "".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Materieltillägg från beredning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Antal").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Pris").
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         queryvar = "FOR EACH " + THIS-OBJECT:berkalkmtrlttH:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:berkalkmtrltth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF VisSummeraMtrl(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).         
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE)).            
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 5 THEN DO:
         ASSIGN 
         cSenasteArbKod     = ""
         iSenasteLopNr      = 0.
         FrekvensTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Frekvenskoder").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kod").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("FrekvensKod").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Frekvens").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Enh").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
          
         queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmtAppend(INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
            END.
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         queryvar = "FOR EACH " + THIS-OBJECT:FrekvensTTh:TABLE. 
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR BY FREKOD BY FREKNR ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:FrekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("FREKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("FREKNR"):BUFFER-VALUE,">99")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:FrekvensTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         
      END.
      IF vad = 6 THEN DO:
         ASSIGN 
         cSenasteArbKod     = ""
         iSenasteLopNr      = 0.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kommentarer från vald Katalog").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kod").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kommentar").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), "M"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
         queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            /*inga dubbletter*/
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN.
            ELSE IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,FALSE,300,0 ).                     
               THIS-OBJECT:KalkVisaControl:VisaExcel:IhopCeller("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) + ":M" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),2).
               THIS-OBJECT:KalkVisaControl:VisaExcel:TextWrap("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               DEFINE VARIABLE komvar AS CHARACTER NO-UNDO.
               komvar = REPLACE(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE,CHR(13),Guru.Konstanter:globradbrytch).
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(komvar)).
               /* FUNKAR
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkVisaControl:VisaExcel:TextBryt(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE),10)).
               */
               /*
               THIS-OBJECT:KalkVisaControl:VisaExcel:ShrinkToFit("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), FALSE).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkVisaControl:VisaExcel:TextBryt(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE),40)).
                */
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      END.
      
      IF vad = 7 THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kod").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Anmärkning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), "M"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
         queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
         qH:GET-FIRST().
  
         DO WHILE qH:QUERY-OFF-END = FALSE:
              
            IF STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE) NE "" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
               THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,FALSE,50,0 ).                     
               THIS-OBJECT:KalkVisaControl:VisaExcel:IhopCeller("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) + ":M" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),2).
               THIS-OBJECT:KalkVisaControl:VisaExcel:TextWrap("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
             END.  
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
        
      END.
     
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummeraMtrl(cIN AS CHARACTER,iIN AS INTEGER):
      IF iIN = ? THEN DO:
         IF cIN = cSenasteMtrl THEN RETURN TRUE.
         ELSE DO:
            ASSIGN  
            cSenasteMtrl = cIN.         
            RETURN FALSE.
         END.
      END.
      ELSE DO:
         IF cIN = cSenasteMtrl AND iIN = iSenasteMtrlMatris THEN RETURN TRUE.
         ELSE DO:
            ASSIGN  
            cSenasteMtrl       = cIN
            iSenasteMtrlMatris = iIN.
            RETURN FALSE.
         END.
      END.      
   END METHOD.

   METHOD PUBLIC LOGICAL VisSummera(cIN AS CHARACTER,iIN AS INTEGER):
      IF cIN = cSenasteArbKod AND iIN = iSenasteLopNr THEN RETURN TRUE.
      ELSE DO:
         ASSIGN  
         cSenasteArbKod = cIN
         iSenasteLopNr  = iIN.
         RETURN FALSE.
      END.   
   END METHOD.
   METHOD PUBLIC LOGICAL VisGruppera(cIN AS CHARACTER):
      IF cIN = cSenasteArbKod THEN RETURN TRUE.
      ELSE DO:
         ASSIGN  
         cSenasteArbKod = cIN.      
         RETURN FALSE.
      END.   
       
   END METHOD.

   METHOD PUBLIC LOGICAL VisGruppMatris(iIN AS INTEGER):  
      IF iIN = iSenasteMatris THEN RETURN TRUE.
      ELSE DO:
         ASSIGN
         cSenasteArbKod = ""  
         iSenasteLopNr  = 0. 
        
         IF iSenasteMatris = 0 THEN RETURN TRUE.
         ELSE RETURN FALSE.        
      END.       
   END METHOD.
   
   METHOD PUBLIC INTEGER VisMatris():
      DEFINE VARIABLE mCH AS CHARACTER NO-UNDO.
      DEFINE VARIABLE tCH AS CHARACTER NO-UNDO.
      mCH = GetMatrisName().
      tCH = THIS-OBJECT:RibbonToolVisMatris:Text.
      IF THIS-OBJECT:RibbonToolVisMatris:Text = "Alla" THEN RETURN 0.
      ELSE DO:
         RETURN INTEGER(TRIM(SUBSTRING(tCH,LENGTH(mCH) + 1))).         
      END.       
   END METHOD.
   METHOD PUBLIC VOID ExcelImportLista(vad AS INTEGER,filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:KalkVisaControl:VisaExcel:iRadslut:
         invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
         IF invar NE "" THEN DO:
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
               KalkylimportTTh:BUFFER-CREATE().
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
               IF vad = 2 THEN DO:
                  klangd = LENGTH(invar).
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
               END.
               ELSE DO:
                  THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
               END.
            END.
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" AND vad = 1 THEN DO:
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
               invar = REPLACE(invar,",",".").
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
            END.      
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A". 
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         END.            
      END.  
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:ImportLista().
   END METHOD.
   
   METHOD PUBLIC VOID ImportMallar():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.  
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkmallValdtth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkmallValdtth,queryvar).
      qH:GET-FIRST().
      IF THIS-OBJECT:KalkmallValdtth:AVAILABLE THEN.
      ELSE RETURN.
      DO WHILE qH:QUERY-OFF-END = FALSE:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkmallKodertth:TABLE + " WHERE KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallValdtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE).     
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkmallKodertth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            KalkylimportTTh:BUFFER-CREATE().
            KalkylimportTTh:BUFFER-COPY(KalkmallKodertth).
            THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.  
                      
            subqH:GET-NEXT(NO-LOCK).  
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:ImportLista().
      KalkmallValdtth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:KalkImpMallControl:GridValdaMallar:GuruReopen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruAvmarkera().
     /* THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().*/
     
   END METHOD.
   
   METHOD PUBLIC VOID ImportLista():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      /*EGEN ?*/
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkylimportTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkylimportTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE > 0 THEN THIS-OBJECT:CurrentMatris = THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            ELSE DO:
               THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 1.
               THIS-OBJECT:CurrentMatris = -1.
            END.   
            KoderTTh:BUFFER-CREATE().      
            KoderTTh:BUFFER-COPY(KalkylimportTTh).
            KoderTTh:BUFFER-COPY(LopposterTTh).
            lastKalknumRowid = KoderTTh:ROWID.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKodRow(INPUT KoderTTh:RECID, INPUT FALSE).  
                                         
         END.   
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(64)).         
         END. 
         KalkylimportTTh:BUFFER-DELETE() NO-ERROR.  
         qH:GET-NEXT().         
      END.  
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ImportSpara().
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RaknaEgen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:RefreshArbKoder(?,?).
   END METHOD.
   
   METHOD PUBLIC VOID GenereraAvtalsKalkyl():
      THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel1Collapsed = FALSE.
      THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel2Collapsed = TRUE.
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE resid AS INTEGER                           NO-UNDO.
      DEFINE VARIABLE res   AS System.Windows.Forms.DialogResult NO-UNDO. 
      OS-DELETE VALUE(ExelKommando) NO-ERROR.     
      THIS-OBJECT:Avtalskalktth:FIND-FIRST("WHERE ID = "  + STRING(THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE)) NO-ERROR.
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(47) + " " +
            THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXT"):BUFFER-VALUE  +  " Katalog år " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE + 
            " Avtals år " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALAR"):BUFFER-VALUE, "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Information).
      IF res:ToString()  EQ "Yes" THEN.
      ELSE DO:
         MESSAGE "e"
         VIEW-AS ALERT-BOX.
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:Selected = TRUE.
         RETURN.
      END.
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:KalkVisaControl:toolStripButtonExporteraExcel:Visible = FALSE.
      THIS-OBJECT:KalkVisaControl:toolStripButtonSkrivUt:Visible = FALSE.
      THIS-OBJECT:SetVisningsBild(1).
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, TRUE).      
      END.   
      ELSE THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, FALSE).
      THIS-OBJECT:HamtaAvtalsKalkyl(OUTPUT resid, OUTPUT ExelKommando).
      IF resid = ? THEN RETURN.
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
         THIS-OBJECT:EonesAvtal().    
      END.
      ELSE IF Guru.Konstanter:globforetag = "FORS" THEN DO:  
         THIS-OBJECT:InfraAvtal().       
      END. 
      ELSE IF Guru.Konstanter:globforetag = "LIMO" THEN DO:        
         THIS-OBJECT:LimoAvtal().       
      END.   
      ELSE IF Guru.Konstanter:globforetag = "VAST" THEN DO:        
         THIS-OBJECT:VattenAvtal().     
      END.   
      ELSE IF Guru.Konstanter:globforetag = "Sweo" THEN DO:        
         THIS-OBJECT:SwecoAvtal().     
      END. 
      ELSE IF Guru.Konstanter:globforetag = "Lapp" THEN DO:        
         THIS-OBJECT:LappAvtal().     
      END.
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
         IF ExelKommando MATCHES "*eones*" THEN THIS-OBJECT:EonesAvtal(). 
         IF ExelKommando MATCHES "*Infratek*" THEN THIS-OBJECT:InfraAvtal().
         IF ExelKommando MATCHES "*linjemontage*" THEN THIS-OBJECT:LimoAvtal().  
         IF ExelKommando MATCHES "*SWECO*" THEN THIS-OBJECT:SwecoAvtal().  
         IF ExelKommando MATCHES "*Lapp*" THEN THIS-OBJECT:LappAvtal(). 
      END.
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:FelMeddCheck().
    
   END METHOD.
   METHOD PRIVATE CHARACTER  TraffColf(INPUT ordvar AS INTEGER) :
      IF ordvar = 1 THEN RETURN  "G". 
      ELSE IF ordvar = 1 THEN RETURN "G".
         ELSE IF ordvar = 2 THEN RETURN "H".
            ELSE IF ordvar = 3 THEN RETURN "I".
               ELSE IF ordvar = 4 THEN RETURN "J".
                  ELSE IF ordvar = 5 THEN RETURN "K".
                     ELSE IF ordvar = 6 THEN RETURN "L".
                        ELSE IF ordvar = 7 THEN RETURN "M".
                           ELSE IF ordvar = 8 THEN RETURN "N".
                              ELSE IF ordvar = 9 THEN RETURN "O".
                                 ELSE IF ordvar = 10 THEN RETURN "P".
   END METHOD.
   METHOD PRIVATE VOID EonesAvtal():
      DEFINE VARIABLE sterad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:EonesAvtal(INPUT-OUTPUT sterad,INPUT-OUTPUT slerad,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                   
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(6).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(12).
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F4", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F3",SUBSTRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,40)).      
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN 
      DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F7",utfardatvar).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F8",beredarvar).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F9",refvar).
      END. 
      ELSE 
      DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F8",utfardatvar).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F9",beredarvar).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F10",refvar).
      END. 
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(6).
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(12).
         ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "133" AND THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 20 THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " m",INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ESP" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.      
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EBR" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKJ" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKL" THEN DO:            
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         END.
         ELSE traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
         IF traffcol = ? OR traffcol = "" THEN DO:          
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END. 
         ELSE DO:
            IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad( 7).
            ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad( 13).              
            ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad( flik2). 
            traffcol = REPLACE(traffcol,"$","").         
            IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
               traffcol = TraffColf(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 10).
            END.            
            ELSE traffcol = TraffColf(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 7).                            
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
               traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
               traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                
            END.
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad( 6).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad( 12).
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(INPUT flik1). 
      queryvar = "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).              
         IF traffcol NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE). 
               traffcol = REPLACE(traffcol,"$","").
               IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                  traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
               END.
               IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                  traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
               END.
               IF pristypvar = "MATERIEL" THEN DO:     
                  traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.
               IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                  traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.                                    
               IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,"Ledning").                  
               END.
               IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kabel").                  
               END.
               IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,"RUNT.OM.MASK").                  
               END.
               IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,"Lastbil").                  
               END.
               IF pristypvar = "MASKIN5" THEN DO: /*F7*/ 
                  traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  traffcol = "AA" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kranbil").                  
               END.                 
               subqH:GET-NEXT(NO-LOCK).
            END.
         END.
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").            
         END.   
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PRIVATE VOID InfraAvtal():
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
         INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).
      ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2011" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(5).
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(5).
      
      IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2009" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2010" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2011" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2013"  THEN DO:   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("H6", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E4",ortnamn).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E7",kontakt).      
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E6",refvar).      
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E5",refvar). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("H8",arbannsv).
      END.
      ELSE DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("I6", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F10",kontakt).      
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F8",refvar).      
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F9",refvar). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("I11",arbannsv).
      END.          
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN.
            ELSE DO:
               IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN DO:
                  traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B912",TRUE).
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN DO:
                  traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B914",TRUE).               
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
                  traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B1056",TRUE).
               END.         
               ELSE IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2013"  THEN DO:
                  IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" THEN DO:
                      traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).               
                  END.
                  ELSE DO:
                     traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).                  
                  END.
               END.   
               ELSE DO:            
                  IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" THEN DO:
                      traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                  END.
                  ELSE DO:
                     traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).                  
                  END.   
               END.           
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
               END.   
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                   
               END.                
            END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B84","B108",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.            
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.                                 
                  subqH:GET-NEXT(NO-LOCK).
               END.          
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(804 + INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                       
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).
               traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B18","B42",TRUE).
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
               END.   
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                     IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                        traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN DO:     
                        traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                        traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                        traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                        traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                           
                     END.
                     IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                     END.
                     subqH:GET-NEXT(NO-LOCK).           
                  END.            
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).
                  IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN traffcol = "C" + STRING(872 + INTEGER(SUBSTRING(traffcol,2))).
                  ELSE traffcol = "C" + STRING(873 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).            
               END.
            END.        
            ELSE DO:
               /*20112011 och framåt om ej annat framkommer*/
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).           
               IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
                  traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "C" + STRING(sterad2),"C" + STRING(slerad2),TRUE).
               END.
               ELSE DO:
                  traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
               END.        
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
               END.   
               ELSE DO:
                  IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
                     traffcol = REPLACE(traffcol,"$","").            
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                     traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                     
                  END.
                  ELSE DO:
                     traffcol = REPLACE(traffcol,"$","").            
                     traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  END.
                  queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".      
                  subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
                  subqH:GET-FIRST().   
                  DO WHILE subqH:QUERY-OFF-END = FALSE:         
                     pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                     IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
                        IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                           traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                           traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "MATERIEL" THEN DO:     
                           traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                           traffcol = "R" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                        END.                                    
                        IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                           traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                        END.
                        IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                           traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                        END.
                        IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                           traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                        END.
                        IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                           traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                        END.
                     END.
                     ELSE DO:
                        IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                           traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                           traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "MATERIEL" THEN DO:     
                           traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                        END.
                        IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                           traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                        END.                                    
                        IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                           traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                        END.
                        IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                           traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                        END.
                        IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                           traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                        END.
                        IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                           traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                        END.
                     END.      
                     subqH:GET-NEXT(NO-LOCK).           
                  END. 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(5).                     
                  IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
                     /*startrad för "FRI"-koder*/                           
                     traffcol = "C" + STRING(1012 + INTEGER(SUBSTRING(traffcol,2))).     
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  END.
                  ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112012" THEN DO:
                     /*startrad för "FRI"-koder*/               
                     traffcol = "C" + STRING(1013 + INTEGER(SUBSTRING(traffcol,2))).     
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).               
                  END.
                  ELSE IF THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013"  THEN DO:
                     /*startrad för "FRI"-koder*/               
                     traffcol = "C" + STRING(1013 + INTEGER(SUBSTRING(traffcol,2))).     
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).               
                  END.            
                  ELSE  DO:            
                     /*startrad för "FRI"-koder 1037 + 20 = 1057*/
                     traffcol = "D" + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).     
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                  
                  END.   
               END.         
            END.                
         traffcol = "".
         qH:GET-NEXT().        
      END.   
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'EON'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN     DO:   
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B27","B31",TRUE).                    
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            IF traffcol NE "" THEN DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(806 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                          
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN   DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).                
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B4","B13",TRUE).
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").              
            END.
            ELSE 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).
               traffcol = "C" + STRING(879 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                             
            END.
         END.
         ELSE  DO:
            /*20112011 och framåt om ej annat framkommer*/
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad3),INPUT "C" + STRING(slerad3),TRUE).       
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(5).
               traffcol = "C" + STRING(1019 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(5).                          
            END.
         END.                  
         traffcol = "".    
         qH:GET-NEXT().    
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PUBLIC VOID VattenAvtal():
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE filend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO.       
      DEFINE VARIABLE sterad AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad  AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad3 AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE slerad2  AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad3  AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VattenAvtal(INPUT-OUTPUT sterad ,INPUT-OUTPUT slerad ,OUTPUT sterad2 ,INPUT-OUTPUT slerad2 ,INPUT-OUTPUT sterad3 ,
                                INPUT-OUTPUT slerad3 ,INPUT-OUTPUT flik1 ,INPUT-OUTPUT flik2 ,
                                INPUT-OUTPUT beredarvar ,INPUT-OUTPUT utfardatvar  ,INPUT-OUTPUT refvar ,INPUT-OUTPUT ortnamn ,
                                INPUT-OUTPUT kontakt ,INPUT-OUTPUT arbannsv ).
         
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("I9", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("I11",beredarvar).      
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("F6",refvar).      
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:    
         THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR
         THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR
         THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR
         THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR
         THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " +  
            STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         ELSE DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
         END.
         traffcol = "".
         qH:GET-NEXT().     
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik2).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0  THEN.
         ELSE DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"D" + STRING(sterad),"D" + STRING(slerad),TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).        
            traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               IF THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE NE 0 THEN DO:
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.                                 
               END.
               IF THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE NE 0 THEN DO:
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "ENTREP" THEN DO:     
                     traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END. 
                  IF pristypvar = "UTRUSTNING" THEN DO:     
                     traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END. 
               END.
               subqH:GET-NEXT(NO-LOCK).
            END.
         END.
         traffcol = "".
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PRIVATE VOID  SwecoAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SwecoAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
     /*Anders Olsson Elpool i Umeå AB  24 feb 2015 09:37:50 
     nytt avtal
     */
     /*
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(2).
      */
      THIS-OBJECT:KalkVisaControl:VisaExcel:ViewType(INPUT 1).
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(1).
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 2.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J2",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J3","Sweco Energuide AB").
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J5",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         IF STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN. 
         ELSE DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "A8", "A869",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE  DO:   
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                   
            END.
            traffcol = "".
         END.   
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 868.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE ARBKOD = 'EGEN'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = THIS-OBJECT:KalkVisaControl:VisaExcel:iRad + 1.    
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).             
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).                           
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            
            IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE NE 0 THEN DO:
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE  BEGINS "BEREDARE" THEN DO: /*F1*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().      
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MONTÖR" THEN DO:     /*F2*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO: /*F3*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
            END.
            
            subqH:GET-NEXT(NO-LOCK).
         END.  
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:  
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE NE 0 THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE)).
            END.    
            IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE NE 0 THEN DO:
               
               IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ARBETE" THEN DO:   
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
                  
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGKOSTNAD" OR THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE = "UTRUSTNING" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.                                    
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(1).  
     
     /*
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(2).
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 4.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E4",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E5","Sweco Energuide AB").
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E6",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E7",beredarvar). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("E8",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).         
         traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D12", "D931",TRUE).
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END.
         ELSE  DO:   
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(3).                 
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "B" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(1).
      */
   END METHOD.
   
   METHOD PRIVATE VOID  LappAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LappAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
     /*Anders Olsson Elpool i Umeå AB  24 feb 2015 09:37:50 
     nytt avtal
     */
     /*
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(2).
      */
      THIS-OBJECT:KalkVisaControl:VisaExcel:ViewType(INPUT 1).
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(1).
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 2.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J2",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J3","Lapplands Elnät AB").
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("J5",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         IF STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN. 
         ELSE DO:
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "A8", "A869",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE  DO:   
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                   
            END.
            traffcol = "".
         END.   
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 868.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE + " WHERE ARBKOD = 'EGEN'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = THIS-OBJECT:KalkVisaControl:VisaExcel:iRad + 1.    
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().   
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).             
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).                           
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            
            IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE NE 0 THEN DO:
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE  BEGINS "BEREDARE" THEN DO: /*F1*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().      
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MONTÖR" THEN DO:     /*F2*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO: /*F3*/ 
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               END.
            END.
            
            subqH:GET-NEXT(NO-LOCK).
         END.  
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:  
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE NE 0 THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE)).
            END.    
            IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE NE 0 THEN DO:
               
               IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ARBETE" THEN DO:   
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
                  
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGKOSTNAD" OR THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE = "UTRUSTNING" THEN DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.
               ELSE DO:     
                  THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
               END.                                    
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(1).  
     
    
   END METHOD.
   
   
   METHOD PRIVATE VOID  LimoAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LimoAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
      THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).    
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("A3",refvar + " " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("A5",STRING(TODAY,"9999-99-99")). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("C3", Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("C5",beredarvar).
      queryvar =  "FOR EACH " + THIS-OBJECT:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).         
         IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TKOD"  THEN 
         DO: 
            traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT SUBSTRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,6),INPUT "B10", "B950",TRUE).                    
         END.      
         ELSE traffcol = THIS-OBJECT:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "B10", "B950",TRUE).
         IF traffcol = ? OR traffcol = "" THEN 
         DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END.
         ELSE 
         DO:   
            THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(4).                 
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
            IF THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).                    
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PRIVATE VOID  HamtaAvtalsKalkyl(OUTPUT resid AS INTEGER,OUTPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE blobproch AS HANDLE                            NO-UNDO.
      DEFINE VARIABLE kommando  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.   
      RUN DYNBLOBC.P PERSISTENT SET blobproch. 
      RUN blobfil_UI IN blobproch (INPUT THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE, OUTPUT resid,OUTPUT ExelKommandout).
      IF resid = ? THEN DO:                 
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(48) + " ", "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).           
         IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
         RETURN.    
      END.
      kommando = ExcelTemplate(INPUT ExelKommandout).
      OS-DELETE VALUE(ExelKommandout) NO-ERROR.
      ExelKommandout = kommando.
      RUN deleteproc_UI IN blobproch.
      IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
      THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcel(ExelKommandout).
     
   END METHOD.
   
   METHOD PUBLIC CHARACTER ExcelTemplate(INPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE excelversend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
      DEFINE VARIABLE globanv   AS CHARACTER                         NO-UNDO.
      kommando = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      IF SUBSTRING(ExelKommandout, LENGTH(ExelKommandout),1) = "x" THEN excelversend = "x".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando) NO-ERROR.
      
      IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 6 THEN DO:
         IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
            kommando = kommando + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
         END.
         ELSE DO:
            kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
         END.
      END.   
      ELSE DO:
         IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
            kommando = kommando + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
         ELSE DO:
            kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
      END. 
      kommando = TRIM(kommando + excelversend).  
      
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      RETURN kommando.
   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         THIS-OBJECT:KalkMtrlControl:ButtonBerMtrlStrip:VISIBLE = TRUE.
      END.
      ELSE do:
         THIS-OBJECT:KalkMtrlControl:ButtonBerMtrlStrip:VISIBLE = FALSE.
         THIS-OBJECT:KalkMtrlControl:splitContainerMtrlEget:SplitterDistance = 500.
      END.
       
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:tiduth:AVAILABLE THEN DO:
         THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(183),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
      END.   
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
      THIS-OBJECT:KalkEgnaControl:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
   END METHOD.
    
END CLASS.
