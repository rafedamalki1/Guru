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
/*
USING Microsoft.Office.Interop.Excel.*.
*/

CLASS Modules.Kalkyl.KalkylShell INHERITS UserControl: 
   
   {KALKYLEXTRATAB.I}
   DEFINE PRIVATE VARIABLE bindingSourceVisaNU AS Progress.Data.BindingSource NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
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
   DEFINE PUBLIC VARIABLE JmfKalkyler AS LOGICAL NO-UNDO.
   DEFINE VARIABLE JmfKalkylh AS HANDLE NO-UNDO.
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
   DEFINE VARIABLE comboListKonst AS Infragistics.Win.ValueList NO-UNDO.
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   /* Drag & Dropp*/
   
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
    
   
   
   
   
   
   
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
   /*Berkalk*/
   DEFINE         VARIABLE RibbonTabBerKalk              AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabBerKalkVisa            AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabBerKalkMtrlSok            AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   DEFINE         VARIABLE RibbonTabKoder               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabEgnaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabFaktPris           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabMat                 AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisaKoder           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
  
  /*ärende status*/
   DEFINE         VARIABLE RibbonTabVisning             AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisningEgenskaper   AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   /* uppföljning*/
   DEFINE         VARIABLE RibbonTabUppfoljning        AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE         VARIABLE RibbonTabManualSpara        AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   /* Ribbonkontroller */
   /* Dummy, lägg denna där du vill fylla ut */
   DEFINE         VARIABLE RibbonDummy                  AS Infragistics.Win.UltraWinToolbars.LabelTool                    NO-UNDO.
   /* info */
   DEFINE PUBLIC  VARIABLE RibbonToolInfTyp             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolOmrade             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKat                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   
   
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolDelKat             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolMatris             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnm           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKatalogKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolAngeKodAntal       AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKodSokning         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolPlusMinus         AS Infragistics.Win.UltraWinToolbars.ButtonTool              NO-UNDO.
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
   DEFINE PUBLIC  VARIABLE RibbonToolMarkning           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolEndastSumma        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolEndastFaktorSumma  AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonArbSok                  AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonLopSok                  AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
   
   
   
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatris          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisBerSch          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGroupMatris     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolGroupMatris        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolManual             AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.

   /* uppfölj*/
   DEFINE PUBLIC  VARIABLE RibbonToolUppfPriser        AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
   
   /* Materiel */
   DEFINE PUBLIC  VARIABLE RibbonToolLev                AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnr                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolSok                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolLevBerKalk                AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnrBerKalk                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolSokBerKalk                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolEndastmarkmtrl     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*BerKalk*/
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnmBerKalk           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisSummeraBerKalk         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKatalogKomBerKalk        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKodSokningBerKalk         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKonstruktionLabel       AS Infragistics.Win.UltraWinToolbars.LabelTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKonstruktion       AS Infragistics.Win.UltraWinToolbars.LabelTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAndMtrlmBer           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
  
     
   /* Kalkyl */
   DEFINE VARIABLE F2SkapNum AS INTEGER NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylLoaded                 AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC  VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   DEFINE PRIVATE VARIABLE PassedOnMatris               AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE NyKalkyl                     AS LOGICAL                                                        NO-UNDO INITIAL FALSE.
   DEFINE PRIVATE VARIABLE ValjintetabKalkyDummy        AS LOGICAL                                                        NO-UNDO INITIAL FALSE.
   DEFINE PUBLIC  VARIABLE cSenasteArbKod               AS CHARACTER                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE iSenasteLopNr                AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE iSenasteMatris               AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE cSenasteMtrl                 AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteMtrlMatris           AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE lsenastLev                   AS CHARACTER                                                      NO-UNDO.
   
   DEFINE         VARIABLE ExelKommando                 AS CHARACTER                                                      NO-UNDO.  
   DEFINE PUBLIC  VARIABLE WasExcelShown                AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC  VARIABLE vismulti                    AS LOGICAL NO-UNDO.
   DEFINE VARIABLE kalkylisinaktiv                     AS LOGICAL NO-UNDO.
   DEFINE VARIABLE friindummy AS LOGICAL NO-UNDO.
   DEFINE PUBLIC PROPERTY KalkNrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   
      
   DEFINE VARIABLE fonttyp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fontstor1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE fontstor2 AS INTEGER NO-UNDO.
   
   CONSTRUCTOR PUBLIC KalkylShell ( INPUT r AS Guru.Root ):
      SUPER ().
    
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar kalkylobjekt..").
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InitializeTTs().
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Gör kopplingar till databasemanager..").
      fonttyp = "Calibri".
      fontstor1 = 20.
      fontstor2 = 20.
      /*
      {KALKYLKATDBH.i Root:DatabaseManager:Kalkyl}
      */
       
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC KalkylShell ( ):
    
      
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
      /* skapa kalkyl i databas SLÅIHOP OCH JMF */
     IF THIS-OBJECT:NewKalkStart() = FALSE THEN RETURN FALSE.
      kalkdatah:FIND-FIRST("USE-INDEX AONR") NO-ERROR.
   
      IF kalkdatah:AVAILABLE THEN DO:
         IF kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = 0 OR kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = ? THEN. 
         ELSE THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE.
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text  = kalkdatah:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         /*v11.2 fix*/
         THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
         IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
            THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("BESTID"):BUFFER-VALUE.
            
         END.
         ELSE DO:
            IF STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = ? THEN 
            THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE. 
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
      THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
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
         THIS-OBJECT:BortOmr().
         THIS-OBJECT:BortBestKund().     
      END.   
   END METHOD.
   
   METHOD PRIVATE LOGICAL SaveHuvud():
      THIS-OBJECT:KalkdbControl:HuvudTTh:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text).
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text).   
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE).
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE       =      STRING(THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:VALUE).
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE).
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE).
      
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE     =     STRING(THIS-OBJECT:KalkHuvudControl:BestKund:VALUE).
      IF THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE = FALSE THEN THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE).
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkHuvudControl:HuvudAnmarkning:Text).
      THIS-OBJECT:VisaBenKalkNR(). 
      IF STRING(THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE) = "" OR  THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE = ? THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(209), THIS-OBJECT:Root:LanguageManager:GetString(209)).
         RETURN FALSE. 
      END.   
      DEFINE VARIABLE ErrorText AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ErrorI AS INTEGER NO-UNDO.
      DEFINE VARIABLE IsError   AS LOGICAL   NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SaveHuvud(OUTPUT ErrorText, OUTPUT IsError).
      IF IsError = TRUE THEN DO:
         ErrorI = INTEGER(ErrorText).
         IF ErrorI EQ 6667 THEN 
            System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(66) + Guru.Konstanter:gomrk + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(67)).
         ELSE IF ErrorI EQ 7071 THEN 
            System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(70) + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(71)).
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
      THIS-OBJECT:ValjintetabKalkyDummy = TRUE.     
      THIS-OBJECT:NyKalkyl = FALSE.
      ASSIGN 
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
      THIS-OBJECT:KalkHuvudControl:HuvudKalknr:VISIBLE       = TRUE.
      IF THIS-OBJECT:LoadKalkyl(INTEGER(STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)),STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE), TRUE) = FALSE THEN RETURN FALSE.
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
         RETURN TRUE.
      END.
      ELSE RETURN FALSE.
           
   END METHOD.
   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT KalkylimportTTinh AS HANDLE,INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      THIS-OBJECT:KalkdbControl:KalkylimportTTh = KalkylimportTTinh.
      THIS-OBJECT:LoadKalkyl(INPUT nr, INPUT omr, INPUT nydef).
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:FIND-FIRST() NO-ERROR. 
      /*KOPIERAR KALKYL FRÅN BEREDNING*/
      IF THIS-OBJECT:KalkdbControl:KalkylimportTTh:AVAILABLE THEN DO:                   
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
      THIS-OBJECT:KalkdbControl:KalkyAonrBerFinns().
      /* Ladda från databas + DEFAULT*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchKalkyl(nr, omr) = FALSE THEN RETURN FALSE.
     
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
      
      IF nydef = FALSE THEN DO:
        
          /*SÄTTER DEFAULT VÄRDE VID OM INGA DEFULTER FINNS MEN KALK ÄR REDAN SKAPADNYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         THIS-OBJECT:GuruDefaultsTTh:FIND-FIRST() NO-ERROR. 
         IF THIS-OBJECT:GuruDefaultsTTh:AVAILABLE THEN.
         ELSE DO:
            IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 1 OR THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 2 OR 
            THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 3 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,
               INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE - 1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 5 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(3), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 7 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(3), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
         END.   
      END.   
      THIS-OBJECT:KalkdbControl:kalkaonrTTh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:kalkaonrTTh:AVAILABLE THEN DO:
            
         IF THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN DO:
            ASSIGN 
             Guru.GlobalaVariabler:plusaonr = ?
            Guru.GlobalaVariabler:plusdnr  = ?.
         END.
         ELSE DO:
            ASSIGN 
             Guru.GlobalaVariabler:plusaonr = THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE
            Guru.GlobalaVariabler:plusdnr  = THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("DELNR"):BUFFER-VALUE.
         END.   
         IF THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("PLANNR"):BUFFER-VALUE = ? THEN DO:
            ASSIGN 
            Guru.GlobalaVariabler:plusplannr = ?
            Guru.GlobalaVariabler:plusartal  = ?.
         END.
         ELSE DO:
            ASSIGN 
            Guru.GlobalaVariabler:plusplannr = THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("PLANNR"):BUFFER-VALUE
            Guru.GlobalaVariabler:plusartal  = THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE.
         END.
      END.
      /* Spärra "skapany"-mode */

      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         ASSIGN
         THIS-OBJECT:KalkHuvudControl:HuvudKatalog:ENABLED = FALSE
         THIS-OBJECT:KalkHuvudControl:HuvudOmrade:ENABLED  = FALSE
         THIS-OBJECT:KalkHuvudControl:HuvudTyp:ENABLED     = FALSE.
      END.
   /* THIS-OBJECT:KalkHuvudControl:HuvudOmrade:ReadOnly = TRUE funkar inte 
   VVVVSätt alla ändringar på grids, Combos osv som behövs efter laddning*/
      
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:KalkdbControl:KalkyAonrBerFinns().
      IF nydef = TRUE OR STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
         /*SÄTTER DEFAULT VÄRDE VID NYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
            "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         IF STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
            
            THIS-OBJECT:KalkdbControl:MarkningTTh:FIND-FIRST("WHERE markfiltertt.TYP = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) NO-ERROR.
            THIS-OBJECT:RibbonToolFilterTyp:Text = THIS-OBJECT:KalkdbControl:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
            
            THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "FilterTyp",STRING(2), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
         END. 
      END.
      
      THIS-OBJECT:ApplyLoadedKalkyl().
     
      
      /*VALUE PÅ DEFAULTVÄRDEN FÅR EFFEKT*/
      THIS-OBJECT:StartDefaulMT().
      THIS-OBJECT:KalkylLoaded = TRUE.
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaDirekt = TRUE THEN  THIS-OBJECT:VisaDirektBerkalk().  
      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID StartDefaulMT():
      /*
      DEFINE VARIABLE defribkalylval AS INTEGER NO-UNDO.
      defribkalylval = THIS-OBJECT:RibbonToolVisVisa:SelectedIndex.
      */
      THIS-OBJECT:FriInmatningKod(?,?).
      THIS-OBJECT:FriKalkChanged(?,?).
      THIS-OBJECT:FrekvensKomChanged(?,?).
      THIS-OBJECT:KatsKomChangedKalk(?,?).
      
      THIS-OBJECT:EndastSumma(?,?).
      THIS-OBJECT:EgnaFaktorer(?,?).
      THIS-OBJECT:EndastFaktorSumma(?,?).
      
      THIS-OBJECT:KoderAnmChangedKalk(?,?). 
      THIS-OBJECT:ValueArbetsKoderCh(?,?).
      THIS-OBJECT:FriInSet().
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum NE 0 THEN DO:
         THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE  NUM = " + STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum),NO-LOCK) NO-ERROR.
         THIS-OBJECT:BerKalkNum().
      END.
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid NE 0 THEN DO:
         THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE  SID = " + STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid),NO-LOCK) NO-ERROR.
         THIS-OBJECT:BerSchaktSid().
         
      END. 
      IF THIS-OBJECT:KalkdbControl:Bervallbuffh = ? OR THIS-OBJECT:KalkdbControl:HdSchaktbuffh = ? THEN.
      ELSE DO:
         IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid = 0 AND THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum = 0 THEN DO:
             THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE ",NO-LOCK) NO-ERROR.
             THIS-OBJECT:BerKalkNum().
             
             IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN.
             ELSE DO:
                THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE ",NO-LOCK) NO-ERROR.
                THIS-OBJECT:BerSchaktSid().
             END.
                   
         END.   
      END.    
   END METHOD.
   
   METHOD PUBLIC VOID BerKalkNum():
      IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridBerVal:GuruRepositionto(THIS-OBJECT:KalkdbControl:Bervallbuffh:ROWID).
         THIS-OBJECT:ValueBerValCh(?,?).
         
      END.
      THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:KalkControl:splitContainerSchaktBer:Panel2Collapsed = TRUE.
      END.  
   END METHOD.
   METHOD PUBLIC VOID BerSchaktSid():
      IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridSchaktVal:GuruRepositionto(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:ROWID).
         THIS-OBJECT:ValueSchaktValCh(?,?).
      END.
      THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:KalkControl:splitContainerSchaktBer:Panel1Collapsed = TRUE.
      END.
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
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
        
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(THIS-OBJECT:KalkControl:fillinFriInLop:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(THIS-OBJECT:KalkControl:fillinFriInAntal:GuruText:Text) NO-ERROR.
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
         THIS-OBJECT:ImportLista().
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
                 
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
      
      IF THIS-OBJECT:GetVisaTyp() = 1 OR THIS-OBJECT:GetVisaTyp() = 7 THEN DO:
         IF THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text BEGINS "G" THEN.
         ELSE THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text = "G".
         IF THIS-OBJECT:GetVisaTyp() = 7 THEN THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:Text = "".
         IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Ej märkta" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("AAAA").
         ELSE IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Region" THEN  THIS-OBJECT:KalkControl:fillinFriInKod:GuruText:ValjMask("A AA").
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
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaDirekt = TRUE.    
      
      
      /*VALUE PÅ DEFAULTVÄRDEN FÅR EFFEKT*/
      THIS-OBJECT:StartDefaulMT().
      THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)).
      THIS-OBJECT:ApplyLoadedVisaKalkyl().
      
      THIS-OBJECT:VisaTabs("Alla", FALSE).
      THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = TRUE.
      queryvar =  "FOR EACH " + extravaldfastth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(extravaldfastth,queryvar).
      qH:GET-FIRST().
      rid = extravaldfastth:ROWID. 
      qH:GET-LAST().
      IF rid = extravaldfastth:ROWID THEN vismulti = FALSE.
      ELSE vismulti = TRUE.
      THIS-OBJECT:VisningKalkIniMulti().
      THIS-OBJECT:KalkylLoaded = TRUE.
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID VisaDirektBerkalk():
      THIS-OBJECT:VisaTabs("Alla", FALSE).
      THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = TRUE.
      
      vismulti = THIS-OBJECT:Root:DatabaseManager:Kalkyl:HmtBerkalkMult().
      THIS-OBJECT:VisningKalkIniMulti().
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
   
   METHOD PUBLIC LOGICAL JmfKalkylLoad(INPUT kalkdatah AS HANDLE):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      JmfKalkylh = kalkdatah.
      JmfKalkyler = TRUE. 
      IF THIS-OBJECT:NewKalkyl(INPUT kalkdatah) = FALSE THEN RETURN FALSE. 
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID JmfKalkyler():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      JmfKalkyler = FALSE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:JmfKalkyler(INPUT JmfKalkylh).    
      THIS-OBJECT:ImportLista().      
      THIS-OBJECT:FelMeddCheck().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(196), THIS-OBJECT:Root:LanguageManager:GetString(189), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:WasExcelShown = FALSE.
   END METHOD. 
   
   METHOD PUBLIC VOID ApplyLoadedVisaKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkdbControl:HuvudTTh:FIND-FIRST() NO-ERROR.
            
      THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      matrisname = THIS-OBJECT:GetMatrisName().
      
      THIS-OBJECT:LaddaLabels().
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName().
      
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 2").
      IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE NE 7 THEN DO:
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
      THIS-OBJECT:UpdateRibbonKatShow().
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      IF THIS-OBJECT:KalkFaktControl NE ? THEN DO:
         THIS-OBJECT:KalkFaktControl:GridPriser:GuruReopen().
         THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruReopen().
      END.      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RibbonToolDelKatFill().

      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).  
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).

      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabBerKalkVisa).
      END.
   END METHOD.
   
   METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkControl:splitContainerKoderMtrl:Panel2Collapsed = TRUE.   
      END. 
      
      THIS-OBJECT:GomaRibbon().
      THIS-OBJECT:ForetagAnpass().
      THIS-OBJECT:TabManager:Tabs["Status"]:VISIBLE   = FALSE.
      THIS-OBJECT:Root:WindowManager:Wnd:FormClosing:Subscribe(THIS-OBJECT:StangaNy).
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE     = FALSE.
       
   END METHOD.
   /* Fullständiggör formen när kalkyl laddats*/
   METHOD PUBLIC VOID ApplyLoadedKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:KalkdbControl:KalkyAonrBerFinns().
      THIS-OBJECT:RibbonTabKalkyl:Caption = Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
      
      THIS-OBJECT:VisaBenKalkNR().
      
      THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      
      IF THIS-OBJECT:KalkEgnaControl NE ? THEN THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFiltrera("kalkylprisertt.EGENKODUPP = TRUE").
      /* Sätter Combos */ 
      /*GÖR ÄVEN REF PÅ ARBKODER*/
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
         ELSE THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE.
         THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruCombo:VALUE  = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE.
      END.
      
      /*v11.2 fix*/
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = " + QUOTER(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)) NO-ERROR.
         IF Guru.Konstanter:OmradeTTh:AVAILABLE THEN .
         ELSE DO:
            THIS-OBJECT:AddOmr().
         END.
         THIS-OBJECT:KalkHuvudControl:HuvudOmrade:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         IF STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR  STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) =  ? THEN 
         THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         ELSE DO:
            Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
            IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
               THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("BESTID"):BUFFER-VALUE.
            END.
            ELSE DO:
               Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE VIBESTID = '" + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
               IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
                  THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("BESTID"):BUFFER-VALUE.
               END.
               ELSE THIS-OBJECT:KalkHuvudControl:BestKund:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE.
            END.     
         END.
         THIS-OBJECT:SetIniComboV().
         THIS-OBJECT:KalkHuvudControl:HuvudKatalog:GuruFiltrera().
         THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      END.
      IF THIS-OBJECT:KalkKopKonvControl NE ? THEN DO:
         THIS-OBJECT:KalkKopKonvControl:CreateIni().
      END.
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
         IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:KalkHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      END.
      ELSE DO:
         IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
            THIS-OBJECT:KalkHuvudControl:UtfardareBort:VISIBLE     = TRUE.
            THIS-OBJECT:KalkHuvudControl:UtfardareBort:VALUE = "Borttagen person " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
         END.   
      END.  
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:KalkHuvudControl:HuvudOmrade:Text.
         THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:KalkHuvudControl:HuvudKatalog:Text.
      END.
      ELSE DO:
         Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + "'")  NO-ERROR.
         IF Guru.Konstanter:OmradeTTh:AVAILABLE THEN .
         ELSE DO:
            THIS-OBJECT:AddOmr().
         END.         
         THIS-OBJECT:RibbonToolOmrade:Text = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE.
         THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) ) NO-ERROR.
         THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.          
      END.   
      /* Hitta kalkylansvarig*/
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
      IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
         IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).              
      END.
      
      /* Filtrera arbetskoder efter kalkyltyp */
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera("kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
      /* Hitta första rows i grejer - behövs detta verkligen ? */
      /* Döp om matris rätt */
      matrisname = THIS-OBJECT:GetMatrisName().
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      /* Sätter värden */
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      END.
      THIS-OBJECT:LaddaLabels().
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName().
      
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 2").
      IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE NE 7 THEN DO:
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
      THIS-OBJECT:UpdateRibbonKatShow().
      THIS-OBJECT:VisaTabs("ALLA", TRUE).      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Beraknaprocalla().
      IF THIS-OBJECT:KalkFaktControl NE ? THEN DO:
         THIS-OBJECT:KalkFaktControl:GridPriser:GuruReopen().
         THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruReopen().
      END.
      IF THIS-OBJECT:KalkTidlControl NE ? THEN THIS-OBJECT:KalkTidlControl:GridTidlage:GuruReopen().
      
      IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
         kalkylisinaktiv = TRUE.
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.         
      END.    
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  19 aug 2015 10:07:39 
          THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE. gör så att scroll inte funkar i kalkyl första gången.
         */
         IF THIS-OBJECT:ValjintetabKalkyDummy = FALSE THEN DO:
            IF Guru.GlobalaVariabler:NyBerKalkyl = TRUE THEN THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:Selected = TRUE. 
            ELSE THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
         END.   
         THIS-OBJECT:ValjintetabKalkyDummy = FALSE. 
      END.      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).    
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYL",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabBerKalkVisa).
      END.
   END METHOD.
   METHOD PUBLIC VOID VisaBenKalkNR():
      THIS-OBJECT:RibbonTabKoder:Caption = "Arbeta med koder för " + Guru.GlobalaVariabler:KalkArendeText + " " + 
      STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + 
      STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
   END METHOD.
   METHOD PUBLIC LOGICAL SokAnvUtfardare(INPUT sokanv AS CHARACTER ):
      IF sokanv = "?" OR sokanv = ? OR sokanv = "" THEN RETURN FALSE.
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + sokanv + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID LaddaLabels():
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE) + "'") NO-ERROR.
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         ASSIGN
         THIS-OBJECT:KalkHuvudControl:LabelBest:TEXT                  = SUBSTRING(Guru.Konstanter:gbestl,1,10)
         THIS-OBJECT:KalkHuvudControl:LabelOmr:TEXT                  = Guru.Konstanter:gomrk
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruLabel:Text  = "Benämning"
         THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text   = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
         THIS-OBJECT:KalkHuvudControl:HuvudAnmarkning:Text           = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE
         THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text     = "nr"
         THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text     = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruLabel:Text,TRUE)
         THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:Text      = STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
         THIS-OBJECT:KalkHuvudControl:HuvudKalknr:GuruText:ReadOnly   = TRUE           
         THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruLabel:Text        = "Kalkyltyp"
         THIS-OBJECT:KalkHuvudControl:comboPriserUppf:GuruLabel:Text = "Välj priser för uppföljning"
         THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruLabel:Text   = "Ansvarig".
      END.
      IF THIS-OBJECT:KalkKopKonvControl NE ? THEN DO:
         THIS-OBJECT:KalkKopKonvControl:CreateIni().
         THIS-OBJECT:KalkKopKonvControl:HuvudBenamning:GuruLabel:Text  = "Ny Benämning".         
      END.
      ASSIGN 
      THIS-OBJECT:KalkControl:fillinFriInKod:GuruLabel:Text   = "Arbkod:"
      THIS-OBJECT:KalkControl:fillinFriInLop:GuruLabel:Text   = "Löpnr:"
      THIS-OBJECT:KalkControl:fillinFriInAntal:GuruLabel:Text   = "Antal:".
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN DO:
         IF Guru.Konstanter:PersonalTTh:AVAILABLE = FALSE  THEN DO:
            THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text  = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BortTagenPersonal(STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE)).      
            Guru.Konstanter:PersonalTTh:FIND-FIRST() NO-ERROR.
         END.   
         ELSE DO: 
            THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text = 
            Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
            THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
             
            THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).
         END.
         THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:ReadOnly = TRUE.
      END.   
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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE AND THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaDirekt = FALSE THEN DO:
         THIS-OBJECT:CreateGridBerKalk().
      END. 
        
   END METHOD.

   METHOD PUBLIC VOID CreateGridFunctions():
      
      /* Drag dropp*/
      
      /* Button overs*/
      IF THIS-OBJECT:KalkControl NE ? THEN DO:
         THIS-OBJECT:DGKoder = NEW Controls.Subclasses.DragDropGroup("Koder").
         THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:KalkControl:GridLopKoder).
         THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:KalkControl:GridKalkylKoder).
         THIS-OBJECT:KalkControl:KalkylButtonOverMark:GuruKoppla(THIS-OBJECT:KalkControl:GridLopKoder,THIS-OBJECT:KalkControl:GridKalkylKoder).
         THIS-OBJECT:KalkControl:KalkylButtonOverMark:GuruDublett = TRUE.
         THIS-OBJECT:KalkControl:KalkylButtonBackMark:GuruKoppla(THIS-OBJECT:KalkControl:GridKalkylKoder,THIS-OBJECT:KalkControl:GridLopKoder).
         THIS-OBJECT:KalkControl:KalkylButtonBackMark:GuruRemoveFrom = TRUE.  
      END.
     
      IF THIS-OBJECT:KalkEgnaControl NE ? THEN DO:
         THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
         THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:KalkEgnaControl:GridUnderKoder).
         THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder).
         THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:GuruKoppla(THIS-OBJECT:KalkEgnaControl:GridUnderKoder,THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder).
         THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:GuruDublett = FALSE.
         THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:GuruKoppla(THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder, THIS-OBJECT:KalkEgnaControl:GridUnderKoder).
         THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:GuruRemoveFrom = TRUE.
      END.
      
      
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN. 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT Guru.Konstanter:PersonalTTh)).
      THIS-OBJECT:KalkHuvudControl:GridPersonal:Text = "ansvarig".
      THIS-OBJECT:KalkHuvudControl:GridPersonal:Text = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:KalkHuvudControl:GridPersonal:Text,TRUE).
           /*egna event*/
      THIS-OBJECT:KalkHuvudControl:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
      
      THIS-OBJECT:KalkHuvudControl:GridPersonal:GuruFiltrera("PERSMASK = TRUE"). 
     
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,100).
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KalkdbControl:ArbetskoderTTh)).  
      THIS-OBJECT:KalkControl:GridArbetsKoder:TEXT = "Arbetskoder".      
      THIS-OBJECT:KalkControl:GridArbetsKoder:SetViewStyleBand(FALSE).
      THIS-OBJECT:KalkControl:GridArbetsKoder:tBS:PositionChanged:Subscribe(ValueArbetsKoderCh).
          
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,42, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[5] = NEW Controls.GridRubrik("TKOST", "Enhetspris", TRUE , 90,"DEC").
      THIS-OBJECT:KalkControl:GridLopKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:KalkdbControl:LopposterTTh)).
      THIS-OBJECT:KalkControl:GridLopKoder:TEXT = "Löpnummer".
      THIS-OBJECT:KalkControl:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:KalkControl:GridLopKoder:tBS:PositionChanged:Subscribe(ValueLopnrCh).
      THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:Columns["ARBKOD"]:Hidden = TRUE.
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
      DEFINE VARIABLE rubnrvar AS INTEGER NO-UNDO.
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubnrvar = 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("num","#",TRUE,35).
      rubnrvar = rubnrvar + 1.
     
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("Id2","Byggnr",TRUE,50).
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("EXTRA1","Fri id",TRUE,65).
      rubnrvar = rubnrvar + 1. 
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("MATRIS","Matris",Guru.GlobalaVariabler:KalkylSomStartiBer,50,"INT","INT").
      rubnrvar = rubnrvar + 1. 
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("FRANTILL","Från-Till",TRUE,65). 
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE, 42, "KALKLOP").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("ANTAL","",FALSE,45,"DEC","KALKYL").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55,"DEC", "KALKYL").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55,"DEC", "KALKYL").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubnrvar = rubnrvar + 1. 
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("RISK","Risk i %",FALSE,75,"DEC", "KALKYL").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("VINST","Vinst i %",FALSE,75,"DEC", "KALKYL").
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("BERNUM","#B",TRUE,35). 
      rubnrvar = rubnrvar + 1.
      rubrikergrid[rubnrvar] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,150).
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT rubnrvar, INPUT THIS-OBJECT:KalkdbControl:KoderTTh)).
      THIS-OBJECT:KalkControl:GridKalkylKoder:TEXT = "Kalkylkoder".  
       
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MARKNING"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["BERNUM"]:Hidden = TRUE.  
      IF Guru.Konstanter:globanv = "celpao" THEN .
      ELSE THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.  /*cccc*/
      
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Hidden = TRUE.   
         THIS-OBJECT:KalkControl:GridArbetsKoder:SetViewStyleBand(FALSE).
      END.   
      ELSE THIS-OBJECT:KalkControl:GridKalkylKoder:SetViewStyleBand(TRUE).
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = FALSE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["Id2"]:Hidden = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["EXTRA1"]:Hidden = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRANTILL"]:Hidden = TRUE.
      END.
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruNoAutoTitle(TRUE).
      THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:PositionChanged:Subscribe(ValueKalkKoderCh).
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Gå till").
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:GaTillMenuItem_Click).      
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Summera markerade rader").
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:SummeraRader_Click).
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Kopiera valda koder för Konstruktion/Schakt").
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KopieraKoderBerKalk_Click).
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Klistra in valda koder till Konstruktion/Schakt").
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KlistraKoderBerKalk_Click).
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Importera koder från Excel till Konstruktion/Schakt").
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ImpExcKoderBerKalk_Click).
         
         IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "gkal"  THEN DO:
            THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Importera koder från Schaktmall").
            THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ImpExcKoderBerKalkSch_Click).   
         END.         
      END.   
      ELSE DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Importera koder från Excel till kalkyl. Arbetskod i kolumn A, löpnr i B och antal i C").
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ImpExcKoderBerKalk_Click).
      END. 
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruOrderbyDefault("BY ARBKOD BY LOPNR BY MARKSUB BY NUM ").
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtramenuItemAdd("Alla ska ha samma Risk % Vinst%").
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KalkControl:SammaRiskVinst).
      THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:ENABLED = FALSE.        
   END METHOD.
   
   
   
   METHOD PUBLIC VOID CreateGridBerKalk():
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
      comboListKonst = NEW Infragistics.Win.ValueList().
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("Id2","Byggnr",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("EXTRA1","Fri id",TRUE,65).      
      rubrikergrid[3] = NEW Controls.GridRubrik("KTYPKOD","Konstruktion",TRUE, 120).
      rubrikergrid[4] = NEW Controls.GridRubrik("F2","Kablar/Fördelning",FALSE,270).
     
      rubrikergrid[5] = NEW Controls.GridRubrik("BERKALKAND","ÄNDRA",TRUE,100).
      THIS-OBJECT:KalkControl:GridBerVal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkdbControl:Bervallbuffh)).
      THIS-OBJECT:KalkControl:GridBerVal:tBS:PositionChanged:Subscribe(ValueBerValCh).
      THIS-OBJECT:KalkControl:GridBerVal:Click:Subscribe(ValueBerValCh).
      THIS-OBJECT:KalkControl:GridBerVal:TEXT = "Konstruktioner".  
      THIS-OBJECT:KalkControl:GridBerVal:GuruFiltrera("KSKAP = FALSE  by ORD ").
      
      THIS-OBJECT:KalkControl:GridBerVal:DisplayLayout:Bands[0]:Columns["F2"]:Hidden = TRUE.
      THIS-OBJECT:KalkControl:GridBerVal:DisplayLayout:Bands[0]:Columns["BERKALKAND"]:Hidden = TRUE.
      
      THIS-OBJECT:KalkControl:GridBerVal:AfterCellListCloseUp:Subscribe(THIS-OBJECT:GridBerVal_AfterCellListCloseUp).
      THIS-OBJECT:KalkControl:GridBerVal:AfterCellUpdate:Subscribe(THIS-OBJECT:GridBerVal_AfterCellUpdate).
      THIS-OBJECT:KalkControl:GridBerVal:BeforeRowActivate:Subscribe(THIS-OBJECT:GridBerVal_BeforeRowActivate).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE ,55).  
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE, 55).    
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE, 230).
      rubrikergrid[5] = NEW Controls.GridRubrik("LEVKOD","Lev-id",TRUE, 50).
      rubrikergrid[6] = NEW Controls.GridRubrik("SKAPNUM","SKAPNUM-id",TRUE, 50).
      
      THIS-OBJECT:KalkControl:GridBerMtrl:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:KalkdbControl:Bermtrlbuffh)).
      THIS-OBJECT:KalkControl:GridBerMtrl:TEXT = "Materiel".
     
      THIS-OBJECT:KalkControl:GridBerMtrl:DisplayLayout:Bands[0]:Columns["SKAPNUM"]:Hidden = TRUE.
     
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruGridFargM(TRUE).             
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
      END. 
      THIS-OBJECT:KalkControl:GridBerMtrl:VisaViaInternetMenuItemAddAdd().
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridBerMtrl:ExtramenuItemAdd("Kopiera valt materiel för Konstruktion").
         THIS-OBJECT:KalkControl:GridBerMtrl:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KopieraMtrlBerKalk_Click).     
         THIS-OBJECT:KalkControl:GridBerMtrl:ExtramenuItemAdd("Klistra in valda materiel till Konstruktion").
         THIS-OBJECT:KalkControl:GridBerMtrl:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KlistraMtrlBerKalk_Click).
      END.   
      
      rubrikergrid[1] = NEW Controls.GridRubrik("SID","Schakt",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE, 155).
      THIS-OBJECT:KalkControl:GridSchaktVal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkdbControl:HdSchaktbuffh)).
      THIS-OBJECT:KalkControl:GridSchaktVal:tBS:PositionChanged:Subscribe(ValueSchaktValCh).
      THIS-OBJECT:KalkControl:GridSchaktVal:Click:Subscribe(ValueSchaktValCh).
      THIS-OBJECT:KalkControl:GridSchaktVal:TEXT = "Schakt".
      THIS-OBJECT:KalkControl:GridSchaktVal:GuruFiltrera(" by SID "). 
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      /*
      rubrikergrid[6] = NEW Controls.GridRubrik("KUND","", TRUE ,50 ).
      */
      THIS-OBJECT:KalkControl:GridEnrFran:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT Guru.GlobalaVariabler:MaterielTTh)).
      
      THIS-OBJECT:KalkControl:GridEnrFran:Text = "Materiel".
      IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:KalkControl:GridEnrFran:GuruOrderbyDefault("BY kund DESCENDING by enr").
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridEnrFran:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:KalkControl:GridEnrFran:GuruGridFargM(TRUE).             
         THIS-OBJECT:KalkControl:GridEnrFran:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
        
      END. 
      THIS-OBJECT:KalkControl:GridEnrFran:VisaViaInternetMenuItemAddAdd().
      
      THIS-OBJECT:KalkControl:AddButton:Click:Subscribe(THIS-OBJECT:AddButton_Click).
      THIS-OBJECT:KalkControl:RemoveButton:Click:Subscribe(THIS-OBJECT:RemoveButton_Click).
      THIS-OBJECT:KalkControl:ButtonNy:Click:Subscribe(THIS-OBJECT:ButtonNy_Click).
      THIS-OBJECT:KalkControl:ButtonNy:GuruKoppla(THIS-OBJECT:KalkControl:GridEnrFran,THIS-OBJECT:KalkControl:GridBerMtrl).
      THIS-OBJECT:KalkControl:ButtonNy:GuruDublett = TRUE.
      
   END METHOD.
   
   METHOD PUBLIC VOID KopieraMtrlBerKalk_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      /*skapar kopior av markerade poster*/
      IF THIS-OBJECT:KalkControl:GridBerMtrl:GridKopiera() = TRUE THEN DO:
      END.   
   END METHOD.
   METHOD PUBLIC VOID KlistraMtrlBerKalk_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      /*skapar NYA poster av tidigare skapade kopior*/
      IF THIS-OBJECT:KalkControl:GridBerMtrl:GridKlistraIn()  = TRUE THEN DO:
         THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = TRUE.
         queryvar = "FOR EACH " + THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            
            IF THIS-OBJECT:DubbMtrl() = TRUE THEN DO: 
               Guru.GlobalaVariabler:TTRECID = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
               rid = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:ROWID.
            END.
            ELSE DO:
               THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-CREATE().
               THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-COPY( THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra,"NUM").
               THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:RECID.
               THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
               THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = F2SkapNum. 
               Guru.GlobalaVariabler:TTRECID = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
               rid = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:ROWID.
            END.
           
            qH:GET-NEXT().  
         END. 
      END.   
      THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE TTRECID = " + STRING(Guru.GlobalaVariabler:TTRECID) ,NO-LOCK) NO-ERROR.
      THIS-OBJECT:KalkControl:GridBerMtrl:GuruReopen().
      THIS-OBJECT:KalkControl:GridBerMtrl:GuruRepositionto(rid).
      
   END METHOD.
   
   
      
   METHOD PUBLIC VOID KopieraKoderBerKalk_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      /*skapar kopior av markerade poster*/
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:GridKopiera() = TRUE THEN DO:
      END.   
   END METHOD.
   METHOD PUBLIC VOID KlistraKoderBerKalk_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      /*skapar NYA poster av tidigare skapade kopior*/
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:GridKlistraIn()  = TRUE THEN DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkControl:GridKalkylKoder:ttBufferHandleKopieraKlistra:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkControl:GridKalkylKoder:ttBufferHandleKopieraKlistra,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-CREATE().
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-COPY( THIS-OBJECT:KalkControl:GridKalkylKoder:ttBufferHandleKopieraKlistra,"NUM").
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:RECID.
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 0.
            /*
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKlistraKoderRows(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE). 
            */
           
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKlistraKoderRows(THIS-OBJECT:KalkControl:GridKalkylKoder:ttBufferHandleKopieraKlistra:BUFFER-FIELD("NUM"):BUFFER-VALUE). 
            qH:GET-NEXT().  
         END. 
      END.   
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
   END METHOD.
   
   METHOD PRIVATE VOID ImpExcKoderBerKalk_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      THIS-OBJECT:ExcelImportLista(3,fildir).    
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
   END METHOD.
   METHOD PRIVATE VOID ImpExcKoderBerKalkSch_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      THIS-OBJECT:ExcelImportLista(4,fildir).    
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID SummeraRader_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:Count > 0 THEN .
      ELSE RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeraknaMarkerade().

   END METHOD.
   METHOD PUBLIC VOID GaTillMenuItem_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:Count > 0 THEN .
      ELSE RETURN.
      
      THIS-OBJECT:RibbonToolDelKat:Text = "Alla".
      THIS-OBJECT:RibbonToolFilterTyp:Text = "Alla".
      THIS-OBJECT:KalkdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:SetVisaTyp(THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE).
         THIS-OBJECT:KalkControl:GridArbetsKoder:GuruRepositionto(THIS-OBJECT:KalkdbControl:ArbetskoderTTh:ROWID).
         THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkControl:GridLopKoder:GuruRepositionto(THIS-OBJECT:KalkdbControl:LopposterTTh:ROWID). 
         END.   
      END.
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFrekvens():
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("FREKOD"," Arbkod ",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("FREKNR","Löpnr",TRUE,45).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,600).
      
      THIS-OBJECT:KalkControl:GridFrekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkdbControl:FrekvensTTh)).
      THIS-OBJECT:KalkControl:GridFrekvens:TEXT = "Frekvenskoder". 
      THIS-OBJECT:KalkControl:GridFrekvens:GuruNoAutoTitle(TRUE).
        
   END METHOD.

   METHOD PUBLIC VOID CreateGridFriKoder():
      IF THIS-OBJECT:KalkControl = ? THEN RETURN.
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
      THIS-OBJECT:KalkControl:GridFriKalk:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh)).
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
      IF THIS-OBJECT:KalkEgnaControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      THIS-OBJECT:DontCheck = TRUE.
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35,"INT","INT").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",FALSE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,80,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,150).
      /*init av grid*/
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KalkdbControl:KoderTTh)). 
      
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:TEXT = "Egna koder".
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE. 
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MARKNING"]:Hidden = TRUE.  
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("kalknumtt.ARBKOD = 'EGEN'").
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:tBS:PositionChanged:Subscribe(ValueEgnaKoderCh).
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Hidden = TRUE.
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:SetViewStyleBand(FALSE).
      END.
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruOrderbyDefault("BY MATRIS BY ARBKOD  BY LOPNR BY MARKSUB  BY NUM").
      THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:DontCheck = false.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
      IF THIS-OBJECT:KalkEgnaControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 250).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", TRUE, 100,"DEC", "KALKYL").
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:KalkdbControl:PriserTTh)).
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:TEXT = "Priser".
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruOrderby("BY KPID").
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
       IF THIS-OBJECT:KalkEgnaControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NUMSUBID", "##", TRUE ,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100,"DEC", "KALKYL").
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh)).
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUMSUBID"]:Hidden = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:TEXT = "Valda Priser".
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
   END METHOD.

   
   /* Faktorer och priser*/
   
   METHOD PUBLIC VOID CreateGridTidlage():
      IF THIS-OBJECT:KalkTidlControl = ? THEN RETURN.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,200).
      rubrikergrid[2] = NEW Controls.GridRubrik("DATUMTID","Datum",FALSE,200,"DATETIME").
      rubrikergrid[3] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      THIS-OBJECT:KalkTidlControl:GridTidlage:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkdbControl:kalkttidlageTTh)).
      THIS-OBJECT:KalkTidlControl:GridTidlage:TEXT = "Tidlägen".
      
   END METHOD.
   
   /* Skapar alla Combos */
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboTyp().
      THIS-OBJECT:CreatecomboPriserUppf().
      THIS-OBJECT:CreateComboOmrade().
      THIS-OBJECT:CreateComboBestKund().
      
      THIS-OBJECT:CreateComboKatalog().
      THIS-OBJECT:CreateComboUtfardare().
      
   END METHOD. 
   METHOD PUBLIC VOID BortOmr():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + STRING(Guru.Konstanter:OmradeTTh:TABLE + " where TTRECID = 0 OR TTRECID = ?").
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.Konstanter:OmradeTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         Guru.Konstanter:OmradeTTh:BUFFER-DELETE ().
         qH:GET-NEXT().
      END.  
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
   METHOD PUBLIC VOID AddOmr():
       Guru.Konstanter:BorttagnaOmradeTTh:FIND-FIRST("WHERE OMRADE = " + QUOTER(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE )) NO-ERROR.
         IF Guru.Konstanter:BorttagnaOmradeTTh:AVAILABLE THEN DO:
            Guru.Konstanter:OmradeTTh:BUFFER-CREATE().
            Guru.Konstanter:OmradeTTh:BUFFER-COPY (Guru.Konstanter:BorttagnaOmradeTTh). 
            Guru.Konstanter:OmradeTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = ?.
            Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = " + QUOTER(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)) NO-ERROR.
            IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:KalkHuvudControl:HuvudOmrade:GuruFiltrera().
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
   
   METHOD PRIVATE VOID AddButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      /*FRÅN PIL UPP*/
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      IF THIS-OBJECT:KalkControl:GridEnrFran:ActiveRow =  ? THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
         RETURN.
      END.
      THIS-OBJECT:ButtonNy_Click(?,?).
      RETURN.

   END METHOD.
   METHOD PUBLIC LOGICAL DubbMtrl():
    
      IF F2SkapNum = 0 THEN DO:
         THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE NUM = " 
         + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND LEVKOD = " +
         QUOTER(THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) + " AND ENR = " +
         QUOTER(THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra:BUFFER-FIELD("ENR"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
      END.
      ELSE DO:
         THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE NUM = " + 
         STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND SKAPNUM = " +
         STRING(F2SkapNum) 
          + " AND LEVKOD = " +
         QUOTER(THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) + " AND ENR = " +
         QUOTER(THIS-OBJECT:KalkControl:GridBerMtrl:ttBufferHandleKopieraKlistra:BUFFER-FIELD("ENR"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
      END.
      IF THIS-OBJECT:KalkdbControl:Bermtrlbuffh:AVAILABLE THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END METHOD.
   METHOD PRIVATE VOID ButtonNy_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = TRUE.
      IF THIS-OBJECT:KalkControl:ButtonNy:RadFromGrid() = TRUE THEN DO:
         IF THIS-OBJECT:KalkControl:GridEnrFran:ActiveRow NE ? THEN THIS-OBJECT:KalkControl:GridEnrFran:ActiveRow:Selected = TRUE.
         
         IF F2SkapNum = 0 THEN DO:
            THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE NUM = " 
            + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND LEVKOD = " +
            QUOTER(Guru.GlobalaVariabler:MaterielTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) + " AND ENR = " +
            QUOTER(Guru.GlobalaVariabler:MaterielTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         END.
         ELSE DO:
            THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE NUM = " + 
            STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND SKAPNUM = " +
            STRING(F2SkapNum) 
             + " AND LEVKOD = " +
            QUOTER(Guru.GlobalaVariabler:MaterielTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) + " AND ENR = " +
            QUOTER(Guru.GlobalaVariabler:MaterielTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
           
         END.      
        
         IF THIS-OBJECT:KalkdbControl:Bermtrlbuffh:AVAILABLE THEN DO:
            Guru.GlobalaVariabler:TTRECID = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
         END.
         ELSE DO:
            THIS-OBJECT:KalkControl:ButtonNy:GuruOver().      
         END.
         
           
         THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-FIRST("WHERE TTRECID = " + STRING(Guru.GlobalaVariabler:TTRECID) ,NO-LOCK) NO-ERROR.
        
         IF THIS-OBJECT:KalkdbControl:Bermtrlbuffh:AVAILABLE THEN DO:
            rid = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:ROWID.
            THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
            THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = F2SkapNum. 
            IF THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN  THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 1.
         END.
              
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruReOpen().
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruRepositionto(rid).
         IF THIS-OBJECT:KalkControl:GridEnrFran:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:KalkControl:ButtonNy:GuruTogrid:GuruCellvaljare("ANTAL").
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
   END METHOD.
   
   METHOD PRIVATE VOID ButtonBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      
      IF THIS-OBJECT:KalkControl:GridBerMtrl:ActiveRow NE ? THEN DO:
         IF THIS-OBJECT:KalkControl:GridBerMtrl:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:KalkControl:GridBerMtrl:GuruBortrader(FALSE).
            THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = TRUE.
           /* THIS-OBJECT:ControlDataDb:BortMtrl().*/
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
      
      
   END METHOD.
     
   
   METHOD PRIVATE VOID RemoveButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      IF THIS-OBJECT:KalkControl:GridBerMtrl:tBS:Count = 1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(240),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         RETURN.
      END.   
      
      THIS-OBJECT:ButtonBort_Click(?,?).
      RETURN.
      
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
      IF incombo:NAME  = "KopHuvudOmrade" THEN THIS-OBJECT:KalkKopKonvControl:KopHuvudOmrade:VALUE = combolista[3].
      IF incombo:NAME = "ComboAvtal" THEN THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE = combolista[4].   
      IF incombo:NAME = "BestKund" THEN THIS-OBJECT:KalkHuvudControl:BestKund:VALUE =  combolista[5].    
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboPriserUppf():
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN.
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
      ufpris.TYPC = "Enligt kalkyl".
      
      CREATE ufpris.
      ASSIGN 
      ufpris.UTYP = 3
      ufpris.TYPC = "Materiel från beredningen".
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
      IF Guru.Konstanter:varforetypchar[10] = "1" THEN DO:   
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
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboKonvTyp():
      IF THIS-OBJECT:KalkKopKonvControl = ? THEN RETURN.
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
      THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:KalkHuvudControl:HuvudKatalog:VALUE) ) NO-ERROR.
      IF INTEGER(STRING(THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE)) <= 2002 THEN.
      ELSE DO: 
         IF STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "1" THEN DO:
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
         IF STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
            CREATE konvtyptt.
            ASSIGN 
            konvtyptt.TYPKALK = 1
            konvtyptt.TYPC    = "Typ 1".
         END.   
         IF STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "2" THEN DO:
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
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN.
      DEFINE VARIABLE cmbBestlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE BestK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE BestnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbBestlist = NEW Controls.GridRubrikLista().
      cmbBestlist:ttBufferHandle = Guru.Konstanter:BestKundTTh.
      
      BestK = NEW Controls.GridRubrik().
      BestK:Falt = "BESTID".
      BestK:Rubrik = Guru.Konstanter:gbestk.
      BestK:Gomma = TRUE.
      cmbBestlist:addRubrik(BestK).
           
      BestK = NEW Controls.GridRubrik().
      BestK:Falt = "VIBESTID".
      BestK:Rubrik = Guru.Konstanter:gbestk.
      cmbBestlist:addRubrik(BestK).
     
      BestnamnK = NEW Controls.GridRubrik().
      BestnamnK:Falt = "BESTNAMN".
      BestnamnK:Rubrik = "Namn".
      cmbBestlist:addRubrik(BestnamnK).
     
     
      THIS-OBJECT:KalkHuvudControl:BestKund:GuruInitCombo(cmbBestlist, "BESTID", "BESTNAMN").
     
   END METHOD.
   METHOD PUBLIC VOID CreateComboOmrade():
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN.
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
   
   
   METHOD PUBLIC VOID CreateComboKatalog():
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN.
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KalkdbControl:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:KalkHuvudControl:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   
   
   METHOD PUBLIC VOID CreateComboUtfardare():
      IF THIS-OBJECT:KalkHuvudControl = ? THEN RETURN.
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
      IF THIS-OBJECT:KalkAvtalControl = ? THEN RETURN.
      DEFINE VARIABLE cmbAvtallist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katar        AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtalar      AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE avtaltxt     AS Controls.GridRubrik      NO-UNDO.
      cmbAvtallist = NEW Controls.GridRubrikLista().
      cmbAvtallist:ttBufferHandle = THIS-OBJECT:KalkdbControl:Avtalskalktth.
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
      
      avtaltxt:Bredd = 300.
      cmbAvtallist:addRubrik(avtaltxt).  
      THIS-OBJECT:KalkAvtalControl:ComboAvtal:GuruInitCombo(cmbAvtallist,"ID","AVTALTXTHELA").
   END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerAnvEgenKod():
     THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:KalkEgnaControl:GridAnvEgen:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:KalkEgnaControl:GridAnvEgen:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:KalkEgnaControl:GridAnvEgen:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
   END METHOD.
   METHOD PUBLIC VOID BerEgenKod():
      THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
    
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN
         THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
      END.
      IF THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE NE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE
         THEN THIS-OBJECT:KalkControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
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
           
      ELSE IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE = TRUE THEN DO:
         IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFirstrow().
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:Enabled = TRUE.
         END.   
         ELSE DO:
            /* Annars disablea kontrollerna och filtrera efter något dumtBernrVar som döljer rubbet */
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
            THIS-OBJECT:KalkEgnaControl:GridValdaUnderKoder:Enabled = FALSE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonOverMark:Enabled = FALSE.
            THIS-OBJECT:KalkEgnaControl:EgnaButtonBackMark:Enabled = FALSE.
         END.
      END.      
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumtBernrVar som döljer rubbet */
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
      /*
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell = THIS-OBJECT.
      Guru.GlobalaVariabler:tidtagninghelp = ETIME.
      THIS-OBJECT:StartaControl().
      MESSAGE ETIME - Guru.GlobalaVariabler:tidtagninghelp  "3.5" /*THIS-OBJECT:Name  THIS-PROCEDURE:NAME */
      VIEW-AS ALERT-BOX.
      Guru.GlobalaVariabler:tidtagninghelp = ETIME.
      THIS-OBJECT:CreateForm().
       MESSAGE ETIME - Guru.GlobalaVariabler:tidtagninghelp "1.7" /*THIS-OBJECT:Name  THIS-PROCEDURE:NAME*/
   VIEW-AS ALERT-BOX.
      */
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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE  THEN.
      ELSE DO: 
         KalkHuvudControl = NEW Modules.Kalkyl.KalkHuvudControl(THIS-OBJECT:Root).
         THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:KalkHuvudControl).
         KalkHuvudControl:Dock = System.Windows.Forms.DockStyle:FILL.
         
      END.
      KalkControl = NEW Modules.Kalkyl.KalkylControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:KalkControl).
      KalkControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkEgnaControl = NEW Modules.Kalkyl.KalkEgnaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:KalkEgnaControl).
      KalkEgnaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      KalkVisaControl = NEW Modules.Kalkyl.KalkVisaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:KalkVisaControl).
      KalkVisaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      
      
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
      ELSE DO:
         THIS-OBJECT:RibbonToolGroupMatris:Checked = FALSE.
         mat = RibbonToolMatris:SelectedIndex. 
      END.
      THIS-OBJECT:CurrentMatris = mat.
      IF THIS-OBJECT:NyKalkyl = FALSE THEN DO:
         IF mat = -1 THEN DO:
            IF THIS-OBJECT:KalkMtrlControl NE ? THEN THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruFiltrera("").
            THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("").
            IF THIS-OBJECT:RibbonToolGroupMatris:Checked = TRUE THEN DO:
               THIS-OBJECT:KoderGruppMatrisChanged(?,?).
            END.
         END.
         ELSE DO:
            IF THIS-OBJECT:KalkMtrlControl NE ? THEN THIS-OBJECT:KalkMtrlControl:GridEgetMateriel:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
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
               
   END METHOD.   
   METHOD PUBLIC VOID CreateRibbonVS():
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      THIS-OBJECT:RibbonTabVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      THIS-OBJECT:RibbonTabVisning:Caption = "Ärendestatus".
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv ut".
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
      /* Skapa verktyg*/
      THIS-OBJECT:RibbonDummy = NEW Infragistics.Win.UltraWinToolbars.LabelTool("RD").
      THIS-OBJECT:RibbonDummy:SharedPropsInternal:Caption = " ".
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy).
      /*Berkalk*/
      THIS-OBJECT:RibbonToolKoderAnmBerKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkningBerKalk").
      THIS-OBJECT:RibbonToolKoderAnmBerKalk:SharedPropsInternal:Caption = "Anmärkningar för koder".
      THIS-OBJECT:RibbonToolKoderAnmBerKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisSummeraBerKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("SummeraKoderBerKalk").
      THIS-OBJECT:RibbonToolVisSummeraBerKalk:SharedPropsInternal:Caption = "Summera koder".
      THIS-OBJECT:RibbonToolVisSummeraBerKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolKatalogKomBerKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KatKomBerKalk").
      THIS-OBJECT:RibbonToolKatalogKomBerKalk:SharedPropsInternal:Caption = "Kommentarer enligt katalog".
      THIS-OBJECT:RibbonToolKatalogKomBerKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolKodSokningBerKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KodSokningBerKalk").
      THIS-OBJECT:RibbonToolKodSokningBerKalk:SharedPropsInternal:Caption = "Sökning av koder".
      THIS-OBJECT:RibbonToolKodSokningBerKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolKoderAndMtrlmBer = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("MtrlBerKalk").
      THIS-OBJECT:RibbonToolKoderAndMtrlmBer:SharedPropsInternal:Caption = "Ändra Materiel på konstruktion".
      THIS-OBJECT:RibbonToolKoderAndMtrlmBer:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
        
      THIS-OBJECT:RibbonToolKonstruktionLabel = NEW Infragistics.Win.UltraWinToolbars.LabelTool("InfoKonstruktiongLabel").
      THIS-OBJECT:RibbonToolKonstruktionLabel:SharedPropsInternal:Caption = "Vald Konstruktion/Schakt".
      /*
      THIS-OBJECT:RibbonToolKonstruktionLabel:SharedPropsInternal:Width = 492.
      */
      
      THIS-OBJECT:RibbonToolKonstruktion = NEW Infragistics.Win.UltraWinToolbars.LabelTool("InfoKonstruktiong").
      THIS-OBJECT:RibbonToolKonstruktion:SharedPropsInternal:Caption = "Vald Konstruktion/Schakt".
      THIS-OBJECT:RibbonToolKonstruktion:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKonstruktion:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolKonstruktionLabel:SharedPropsInternal:Width.
      
      
      
      THIS-OBJECT:RibbonTabKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Koder").
      THIS-OBJECT:RibbonTabEgnaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Egna koder").
      THIS-OBJECT:RibbonTabFaktPris = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("FaktorerPriser").
      
      THIS-OBJECT:RibbonTabMat = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök materiel").
      THIS-OBJECT:RibbonTabVisaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visning av koder").
      THIS-OBJECT:RibbonTabVisaKoder:Visible = FALSE.
      THIS-OBJECT:RibbonTabVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      /*
      THIS-OBJECT:RibbonTabVisningVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Inkludera i visning").
      */
      THIS-OBJECT:RibbonTabVisningEgenskaper = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visningsinställningar").
      THIS-OBJECT:RibbonTabManualSpara = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara och Manual").

      THIS-OBJECT:RibbonTabVisningEgenskaper:Visible = FALSE.
      /*
      THIS-OBJECT:RibbonTabUppfoljning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Uppföljning").
      */
       
      /* Global*/
      
      THIS-OBJECT:RibbonToolMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("KodMatris").
      THIS-OBJECT:RibbonToolMatris:Key = "KalkylRibbonMatris".
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width = 135.
      
      THIS-OBJECT:RibbonToolDelKat = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("DelKatKod").
      THIS-OBJECT:RibbonToolDelKat:SharedPropsInternal:Caption = "Visa koder från".
      THIS-OBJECT:RibbonToolDelKat:Key = "DelKatKoder".
      THIS-OBJECT:RibbonToolDelKat:SharedPropsInternal:Width = 200.
      
      
      
      
      THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa som".
      THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
      THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp".
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolDelKat:SharedPropsInternal:Width.
      
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
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolDelKat:SharedPropsInternal:Width.
      /* kalkyl*/
      THIS-OBJECT:RibbonToolInfTyp = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKalkTyp").
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Caption = "Kalkyl".
      THIS-OBJECT:RibbonToolInfTyp:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolOmrade = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoOmrade").
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Caption = Guru.Konstanter:gomrk.
      THIS-OBJECT:RibbonToolOmrade:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:KalkHuvudControl:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:KalkHuvudControl:HuvudKatalog:Text.
      
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      /* Koder */
      THIS-OBJECT:RibbonToolFriKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FriKalkActive").
      THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:Caption = "Fri kalkyl".
      THIS-OBJECT:RibbonToolFriKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
        
      THIS-OBJECT:RibbonToolKoderAnm = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkning").
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:Caption = "Anmärkningar för koder".
      THIS-OBJECT:RibbonToolKoderAnm:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:ToolTipText = "Användarens anmärkningar på kalkylkoder".
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens enligt katalog".
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:ToolTipText = "Frekvens enligt katalog för markerad kod i katalog eller i kalkylkod".
      
      THIS-OBJECT:RibbonToolKatalogKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KatKom").
      THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:Caption = "Kommentarer enligt katalog".
      THIS-OBJECT:RibbonToolKatalogKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolKatalogKom:SharedPropsInternal:ToolTipText = "Kommentarer enligt katalog för markerad kod i katalog eller i kalkylkod".
      
      THIS-OBJECT:RibbonToolAngeKodAntal = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AngeKod").
      THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:Caption = "Fri inmatning".
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:ToolTipText = "Fri inmatning av koder och antal".
      
      THIS-OBJECT:RibbonToolKodSokning = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KodSokning").
      THIS-OBJECT:RibbonToolKodSokning:SharedPropsInternal:Caption = "Sökning av koder".
      THIS-OBJECT:RibbonToolKodSokning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      THIS-OBJECT:RibbonToolPlusMinus = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Plusminus").
      THIS-OBJECT:RibbonToolPlusMinus:SharedPropsInternal:Caption = "Byt tecken".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolPlusMinus,INPUT "bilder/bytteckenkalk.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolPlusMinus,INPUT "bilder/bytteckenkalk.gif").
      THIS-OBJECT:RibbonToolPlusminus:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:RibbonToolPlusminus:SharedPropsInternal:ToolTipText = "Muliplicerar alla kalkylkoder med minus ett".
      
    
      /* Visning */
     
      THIS-OBJECT:RibbonToolGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaGen").
      THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera kalkyl".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolGenerera,INPUT "bilder/kalkyl-generate.gif").
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUtBildval = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv ut".
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
      THIS-OBJECT:RibbonToolVisAnmarkning:SharedPropsInternal:Caption = "Anmärkningar".
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisAnmarkning:SharedPropsInternal:ToolTipText = "Anmärkningar på kalkylen i kalkylhuvud".
      
      THIS-OBJECT:RibbonToolVisUtforlig = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisAnmUtf").
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Caption = "Anmärkningar för koder".
      THIS-OBJECT:RibbonToolVisUtforlig:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.    
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:ToolTipText = "Användarens anmärkningar på kalkylkoder".
      /*Anders Olsson Elpool i Umeå AB  10 jun 2015 10:33:37 
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = FALSE. 
      */
      
      
      THIS-OBJECT:RibbonToolVisEgnaFaktorer = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisFaktorer").
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Caption = "Egna faktorer".

      THIS-OBJECT:RibbonToolVisEgnaFaktorer:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisEgnaPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisEgnaPriser").
      THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:Caption = "Egna priser".
      THIS-OBJECT:RibbonToolVisEgnaPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisFrekvens = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisFrekvens").
      THIS-OBJECT:RibbonToolVisFrekvens:SharedPropsInternal:Caption = "Frekvenser".
      THIS-OBJECT:RibbonToolVisFrekvens:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisFrekvens:SharedPropsInternal:ToolTipText = "Frekvenser enligt katalog".
      
      THIS-OBJECT:RibbonToolVisKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisKommentar").
      THIS-OBJECT:RibbonToolVisKom:SharedPropsInternal:Caption = "Kommentarer".
      THIS-OBJECT:RibbonToolVisKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisKom:SharedPropsInternal:ToolTipText = "Kommentarer enligt katalog".
      
      
      
      /* visningsinställningar */
      THIS-OBJECT:RibbonToolVisVisa = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisVisafor").
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Caption = "Visa som".
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
      THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Tillkommande eget materiel")).
      THIS-OBJECT:RibbonToolVisPriser:SharedPropsInternal:Width = 140.
      THIS-OBJECT:RibbonToolVisMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisMatris").
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Caption = "Matris".
      THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Width = 140.
      
      THIS-OBJECT:RibbonToolVisBerSch = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisBerSch").
      THIS-OBJECT:RibbonToolVisBerSch:SharedPropsInternal:Caption = "Matris".
      THIS-OBJECT:RibbonToolVisBerSch:SharedPropsInternal:Width = 140.
      
      
      
      THIS-OBJECT:RibbonToolVisArbKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisArbKalk").
      THIS-OBJECT:RibbonToolVisArbKalk:SharedPropsInternal:Caption = "Visa som arbetskalkyl".
      THIS-OBJECT:RibbonToolVisArbKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolVisArbKalk:Checked = TRUE.
      
      THIS-OBJECT:RibbonToolVisSummera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisSummera").
      THIS-OBJECT:RibbonToolVisSummera:SharedPropsInternal:Caption = "Summera koder".
      THIS-OBJECT:RibbonToolVisSummera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGruppera = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppera").
      THIS-OBJECT:RibbonToolVisGruppera:SharedPropsInternal:Caption = "Gruppera koder".
      THIS-OBJECT:RibbonToolVisGruppera:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolVisGroupMatris = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisGruppMatris").
      THIS-OBJECT:RibbonToolVisGroupMatris:SharedPropsInternal:Caption = "Gruppera matris".
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolGroupMatris = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("GrpGruppMatris").
      THIS-OBJECT:RibbonToolGroupMatris:SharedPropsInternal:Caption = "Gruppera matris".
      THIS-OBJECT:RibbonToolGroupMatris:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      
      THIS-OBJECT:RibbonToolVisMatSpec = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisMatSpec").
      THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Caption = "Materiel specifikation".
      THIS-OBJECT:RibbonToolVisMatSpec:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolMarkning = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("MarkSubMark").
      THIS-OBJECT:RibbonToolMarkning:SharedPropsInternal:Caption = "Intern kommentar".
      THIS-OBJECT:RibbonToolMarkning:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolEndastSumma = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("EndastSumma").
      THIS-OBJECT:RibbonToolEndastSumma:SharedPropsInternal:Caption = "Endast antal och summa".
      THIS-OBJECT:RibbonToolEndastSumma:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolEndastFaktorSumma = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("EndFakSum").
      THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Caption = "Endast Faktor som Rabatt/Påslag".
      THIS-OBJECT:RibbonToolEndastFaktorSumma:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
                                     
      THIS-OBJECT:RibbonArbSok = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("SokArbkod").
      THIS-OBJECT:RibbonArbSok:SharedPropsInternal:Caption = "Sök Arbkod".
      THIS-OBJECT:RibbonArbSok:SharedPropsInternal:Width = 120.
      /*
      THIS-OBJECT:RibbonArbSok:SharedPropsInternal:TabIndex = 22.
      */
      THIS-OBJECT:RibbonLopSok = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("SokLopkod").
      THIS-OBJECT:RibbonLopSok:SharedPropsInternal:Caption = "Löp nr".
      THIS-OBJECT:RibbonLopSok:SharedPropsInternal:Width = 80.
      
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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelKat,THIS-OBJECT:RibbonTabKoder).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp,THIS-OBJECT:RibbonTabKoder).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,THIS-OBJECT:RibbonTabKoder).
         THIS-OBJECT:RibbonTabEgnaKoder:Tools:Add(THIS-OBJECT:RibbonToolDelKat).  /*finns ju redan*/
         THIS-OBJECT:RibbonTabFaktPris:Tools:Add(THIS-OBJECT:RibbonToolDelKat).  /*finns ju redan*/
            
  
      END.
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMatris,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGroupMatris,THIS-OBJECT:RibbonTabKoder).
     /* THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy)..  /*finns ju redan*/
     */
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAnm,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalogKom,THIS-OBJECT:RibbonTabKoder).
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = FALSE THEN THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriKalk,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAngeKodAntal,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKodSokning,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolPlusMinus,THIS-OBJECT:RibbonTabKoder).
      
      /*Anders Olsson Elpool i Umeå AB  21 mar 2014 16:46:00 
      dummy för att få en tom pos i ribbon 
      
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      */

      THIS-OBJECT:RibbonTabEgnaKoder:Tools:Add(THIS-OBJECT:RibbonToolMatris).  /*finns ju redan*/
     
           /* Så här*/
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris). /*är redan preppad*/
                        
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastmarkmtrl,THIS-OBJECT:RibbonTabMat).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolInfTyp,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat,THIS-OBJECT:RibbonTabKalkyl).          
      
       /*ärende STATUS*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera,THIS-OBJECT:RibbonTabVisning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUtBildval,THIS-OBJECT:RibbonTabVisning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaExcel,THIS-OBJECT:RibbonTabVisning).
    
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = FALSE THEN DO:
          /* Preppa Visning*/ 
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatris,THIS-OBJECT:RibbonTabVisningEgenskaper). 
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisVisa,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisPriser,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisAnmarkning,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummera,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGruppera,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGroupMatris,THIS-OBJECT:RibbonTabVisningEgenskaper).
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisFrekvens,THIS-OBJECT:RibbonTabVisningEgenskaper).  
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisKom,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatSpec,THIS-OBJECT:RibbonTabVisningEgenskaper).
   
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaFaktorer,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisEgnaPriser,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMarkning,THIS-OBJECT:RibbonTabVisningEgenskaper).
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastSumma,THIS-OBJECT:RibbonTabVisningEgenskaper).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastFaktorSumma,THIS-OBJECT:RibbonTabVisningEgenskaper).
        
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,THIS-OBJECT:RibbonTabManualSpara).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolManual,THIS-OBJECT:RibbonTabManualSpara).
      END.
     
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         /*Berkalk*/
        
         THIS-OBJECT:RibbonTabBerKalk = THIS-OBJECT:Root:ModuleManager:RibbonContextManager:Groups:Add("Katalog").
         THIS-OBJECT:RibbonTabBerKalkMtrlSok = THIS-OBJECT:Root:ModuleManager:RibbonContextManager:Groups:Add("Mtrl").
         
         THIS-OBJECT:RibbonTabBerKalk:Caption = "Katalog och konstruktion".
          
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelKat,THIS-OBJECT:RibbonTabBerKalk).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp,THIS-OBJECT:RibbonTabBerKalk).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,THIS-OBJECT:RibbonTabBerKalk).
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAnmBerKalk,THIS-OBJECT:RibbonTabBerKalk).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalogKomBerKalk,THIS-OBJECT:RibbonTabBerKalk).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKodSokningBerKalk,THIS-OBJECT:RibbonTabBerKalk).
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKonstruktionLabel,THIS-OBJECT:RibbonTabBerKalk).
         THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN  DO.
            IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaDirekt = FALSE THEN DO:
               THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAndMtrlmBer,THIS-OBJECT:RibbonTabBerKalk).
            END.
         END.
         THIS-OBJECT:RibbonTabBerKalk:Tools:Add(THIS-OBJECT:RibbonDummy).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKonstruktion,THIS-OBJECT:RibbonTabBerKalk).
         /*ENDAST ELPAO FÖR ATT VI SKA KUNNA SE FRI KALK*/
         IF Guru.Konstanter:globanv =  {setuser.I} THEN DO:
           THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriKalk,THIS-OBJECT:RibbonTabBerKalk).
         END.
         
         /*Berkalk sök*/
         THIS-OBJECT:RibbonTabBerKalkMtrlSok:Caption = "Sök Materiel".
         THIS-OBJECT:RibbonToolLevBerKalk = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("MatLeverBer").
         THIS-OBJECT:RibbonToolLevBerKalk:SharedPropsInternal:Caption = "Leverantör:".
         THIS-OBJECT:RibbonToolEnrBerKalk = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatEnrBer").
         THIS-OBJECT:RibbonToolEnrBerKalk:SharedPropsInternal:Caption = Guru.Konstanter:genk.
         THIS-OBJECT:RibbonToolSokBerKalk = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatSoekBer").
         THIS-OBJECT:RibbonToolSokBerKalk:SharedPropsInternal:Caption = "Benämning:".         
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLevBerKalk,THIS-OBJECT:RibbonTabBerKalkMtrlSok).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnrBerKalk,THIS-OBJECT:RibbonTabBerKalkMtrlSok).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSokBerKalk,THIS-OBJECT:RibbonTabBerKalkMtrlSok).
         
         
         
          /* Preppa Visning*/
          
         THIS-OBJECT:RibbonToolVisBerSch:ValueList:ValueListItems:Clear().
         THIS-OBJECT:RibbonToolVisBerSch:SharedPropsInternal:Caption = "Visa Ber/Sch".
         THIS-OBJECT:RibbonToolVisBerSch:ValueList:ValueListItems:Add("Allt").
         THIS-OBJECT:RibbonToolVisBerSch:ValueList:ValueListItems:Add("Beredning").
         THIS-OBJECT:RibbonToolVisBerSch:ValueList:ValueListItems:Add("Schakt").
         THIS-OBJECT:RibbonToolVisBerSch:VALUE = "Allt".
         
         THIS-OBJECT:RibbonToolKoderAnmBerKalk:ToolClick:Subscribe(THIS-OBJECT:KoderAnmChangedBer).
         THIS-OBJECT:RibbonToolKatalogKomBerKalk:ToolClick:Subscribe(THIS-OBJECT:KatsKomChangedBer).
         THIS-OBJECT:RibbonToolKodSokningBerKalk:ToolClick:Subscribe(THIS-OBJECT:SokningKodBer). 
         
         THIS-OBJECT:RibbonToolKoderAndMtrlmBer:ToolClick:Subscribe(THIS-OBJECT:AndMtrlBer).
         
         
         THIS-OBJECT:RibbonTabBerKalkVisa = THIS-OBJECT:Root:ModuleManager:RibbonContextManager:Groups:Add("BeredningsKalkylVis").
         THIS-OBJECT:RibbonTabBerKalkVisa:Caption = "Visning BeredningsKalkyl". 
      
         /* Preppa Visning*/
      
         
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisBerSch,THIS-OBJECT:RibbonTabBerKalkVisa).  
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummeraBerKalk,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisKom,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMarkning,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastSumma,THIS-OBJECT:RibbonTabBerKalkVisa).
        
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonArbSok,THIS-OBJECT:RibbonTabBerKalkVisa).
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonLopSok,THIS-OBJECT:RibbonTabBerKalkVisa).
        
         THIS-OBJECT:RibbonArbSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokArbKod).
         THIS-OBJECT:RibbonLopSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokLopKod).
         THIS-OBJECT:RibbonToolSokBerKalk:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdateBen).
         THIS-OBJECT:RibbonToolEnrBerKalk:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdateEnr).
         
         
         /*
         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastFaktorSumma,THIS-OBJECT:RibbonTabBerKalkVisa).
       */
      END.
      
      /* binda events*/
      THIS-OBJECT:RibbonToolSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdateBen).
      THIS-OBJECT:RibbonToolEnr:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdateEnr).
      THIS-OBJECT:RibbonToolGenerera:ToolClick:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtE).
      THIS-OBJECT:RibbonToolSparaExcel:ToolClick:Subscribe(THIS-OBJECT:SparaExcelE).
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolClick:Subscribe(THIS-OBJECT:VisAnmarkningChanged).
      THIS-OBJECT:RibbonToolVisPriser:ToolValueChanged:Subscribe(THIS-OBJECT:VisPriserChanged).
      THIS-OBJECT:RibbonToolVisPriser:SelectedItem = THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems[0].

      THIS-OBJECT:RibbonToolVisVisa:SelectedItem = THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems[0].

      THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
     
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Enabled = FALSE.
      IF Guru.Konstanter:globforetag = "SNAT"  THEN THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Enabled = TRUE.
      THIS-OBJECT:RibbonToolDelKat:ToolValueChanged:Subscribe(RibbonDelKatVal).
      
      THIS-OBJECT:RibbonToolFriKalk:ToolClick:Subscribe(THIS-OBJECT:FriKalkChanged).
     
      
      THIS-OBJECT:RibbonToolVisEgnaFaktorer:ToolClick:Subscribe(THIS-OBJECT:EgnaFaktorer).
            
      THIS-OBJECT:RibbonToolEndastSumma:ToolClick:Subscribe(THIS-OBJECT:EndastSumma).
      THIS-OBJECT:RibbonToolEndastFaktorSumma:ToolClick:Subscribe(THIS-OBJECT:EndastFaktorSumma).  
      THIS-OBJECT:RibbonToolKoderAnm:ToolClick:Subscribe(THIS-OBJECT:KoderAnmChangedKalk).
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:FrekvensKomChanged).
      THIS-OBJECT:RibbonToolKatalogKom:ToolClick:Subscribe(THIS-OBJECT:KatsKomChangedKalk).
      
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolClick:Subscribe(THIS-OBJECT:FriInmatningKod).
      THIS-OBJECT:RibbonToolKodSokning:ToolClick:Subscribe(THIS-OBJECT:SokningKodKalk).
      
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder).
      THIS-OBJECT:RibbonToolMatris:ToolValueChanged:Subscribe(UpdateMatrisE).
      THIS-OBJECT:RibbonToolGroupMatris:ToolClick:Subscribe(THIS-OBJECT:KoderGruppMatrisChanged).
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolClick:Subscribe(THIS-OBJECT:GruppMatrisChanged).
      THIS-OBJECT:RibbonToolVisMatris:ToolValueChanged:Subscribe(VisUpdateMatrisE).
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:Subscribe(SparaDefs).
      THIS-OBJECT:RibbonToolManual:ToolClick:Subscribe(Manual).
      THIS-OBJECT:RibbonToolPlusMinus:ToolClick:Subscribe(PlusMinus).
      /* splatter */
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:LoadRibbonLevs(TRUE).
      ELSE THIS-OBJECT:LoadRibbonLevs(FALSE).
        
      RibbonToolVisArbKalk:SharedPropsInternal:Enabled = FALSE.
     
   END METHOD.
   
   METHOD PUBLIC VOID Manual(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE path AS CHARACTER.
      path = SEARCH("Manualer\Kap5 Kalkyl.pdf").
      System.Diagnostics.Process:Start(path).
   END METHOD.
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Kalkyl",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar) .
   END METHOD.
   
   METHOD PUBLIC VOID PlusMinus(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Plusminus().
      RETURN.
    
   END METHOD.
   
   METHOD PUBLIC VOID VisAnmarkningChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     /*Anders Olsson Elpool i Umeå AB  10 jun 2015 10:33:50 
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = THIS-OBJECT:RibbonToolVisAnmarkning:Checked. 
     */
     
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
   METHOD PUBLIC VOID SokningKodKalk(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:SokningKod(THIS-OBJECT:RibbonToolKodSokning:Checked).
   END METHOD.
   METHOD PUBLIC VOID SokningKodBer(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:SokningKod(THIS-OBJECT:RibbonToolKodSokningBerKalk:Checked).
   END METHOD.
   METHOD PUBLIC VOID FillcomboListKonst():
      DEFINE VARIABLE queryvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      IF THIS-OBJECT:RibbonToolKoderAndMtrlmBer:CHECKED = FALSE THEN RETURN.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:BervallKskapbuffh:TABLE + " WHERE NUM = " + 
      STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " BY SKAPNUM ".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:BervallKskapbuffh,queryvar).
      comboListKonst:ValueListItems:Clear().
     /*
      comboListKonst:ValueListItems:Add("").
       comboListKonst:ValueListItems:Remove(0).
      */
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboListKonst:ValueListItems:Add(THIS-OBJECT:KalkdbControl:BervallKskapbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE).
         IF THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = "" THEN DO:
            THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:BervallKskapbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE.
         END.
         qH:GET-NEXT().
      END.
      THIS-OBJECT:KalkControl:GridBerVal:GuruComboCellRefill("F2",comboListKonst).
  
      
      
   END METHOD.
   METHOD PUBLIC VOID AndMtrlBer(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      DEFINE VARIABLE spiltdist AS INTEGER NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      DEFINE VARIABLE ridmtrl AS ROWID NO-UNDO.
      spiltdist = 200.
      IF THIS-OBJECT:RibbonToolKoderAndMtrlmBer:CHECKED = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruReadOnly(FALSE,"ANTAL").
         THIS-OBJECT:KalkControl:splitContainerKoderMtrl:Panel2Collapsed = FALSE.
         THIS-OBJECT:RibbonTabBerKalkMtrlSok:Visible = TRUE.
         THIS-OBJECT:KalkControl:splitContainerSchaktBer:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerBerKalkMtrl:SplitterDistance = THIS-OBJECT:KalkControl:splitContainerBerKalkMtrl:SplitterDistance + spiltdist.
         THIS-OBJECT:KalkControl:splitContainerKalkBer:SplitterDistance = THIS-OBJECT:KalkControl:splitContainerKalkBer:SplitterDistance + THIS-OBJECT:KalkControl:splitContainerKalkBer:Height / 3. 
         THIS-OBJECT:KalkControl:GridBerVal:DisplayLayout:Bands[0]:Columns["F2"]:Hidden = FALSE.
         THIS-OBJECT:FillcomboListKonst().  
         IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkdbControl:InkopKoll(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
         END. 
      END.
      ELSE DO:
         F2SkapNum = 0.
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruReadOnly(TRUE,"ANTAL").
         THIS-OBJECT:RibbonTabBerKalkMtrlSok:Visible = FALSE.
         THIS-OBJECT:KalkControl:splitContainerBerKalkMtrl:SplitterDistance = THIS-OBJECT:KalkControl:splitContainerBerKalkMtrl:SplitterDistance - spiltdist.
         THIS-OBJECT:KalkControl:splitContainerKoderMtrl:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkBer:SplitterDistance = THIS-OBJECT:KalkControl:splitContainerKalkBer:SplitterDistance - THIS-OBJECT:KalkControl:splitContainerKalkBer:Height / 3. 
         THIS-OBJECT:KalkControl:GridBerVal:DisplayLayout:Bands[0]:Columns["F2"]:Hidden = TRUE.
         THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN THIS-OBJECT:KalkControl:splitContainerSchaktBer:Panel2Collapsed = FALSE.
      END.  
      ridmtrl = ?.
      IF THIS-OBJECT:KalkdbControl:Bermtrlbuffh:AVAILABLE THEN DO:
         ridmtrl = THIS-OBJECT:KalkdbControl:Bermtrlbuffh:ROWID.
      END.   
      IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
         rid = THIS-OBJECT:KalkdbControl:Bervallbuffh:ROWID.
         THIS-OBJECT:KalkControl:GridBerVal:GuruRepositionto(THIS-OBJECT:KalkdbControl:Bervallbuffh:ROWID).
         THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-BY-ROWID(rid,NO-LOCK) NO-ERROR.
         IF ridmtrl = ? THEN.
         ELSE DO:
            THIS-OBJECT:KalkdbControl:Bermtrlbuffh:FIND-BY-ROWID(ridmtrl,NO-LOCK) NO-ERROR.
         END.    
      END.  
      ridmtrl = ?.           
   END METHOD.
   
   METHOD PUBLIC VOID SokningKod(INPUT KodSokning AS logical):
      IF KodSokning = TRUE THEN DO:
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
         THIS-OBJECT:KalkVisaControl:toolStripButtonTillExcel:VISIBLE = TRUE.

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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:toolStripButtonTillExcel:VISIBLE = FALSE.
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
      IF THIS-OBJECT:RibbonToolKatalogKom:Checked EQ TRUE OR THIS-OBJECT:RibbonToolKatalogKomBerKalk:Checked EQ TRUE OR
      THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE OR 
      THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE  OR  THIS-OBJECT:RibbonToolKoderAnmBerKalk:Checked EQ TRUE THEN DO: 
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkKodoFrek:Panel2Collapsed = FALSE.
      END.   
      ELSE DO:
         THIS-OBJECT:KalkControl:splitContainerKalkKodoFrek:Panel2Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel1Collapsed = TRUE.
         THIS-OBJECT:KalkControl:splitContainerKalkBild:Panel2Collapsed = FALSE.
                 
      END.   
      
   END METHOD.
   METHOD PUBLIC VOID KatsKomChangedKalk(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KatsKomChanged(THIS-OBJECT:RibbonToolKatalogKom:Checked).
      
   END METHOD.
   METHOD PUBLIC VOID KatsKomChangedBer(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KatsKomChanged(THIS-OBJECT:RibbonToolKatalogKomBerKalk:Checked).
   END METHOD.
   METHOD PUBLIC VOID KatsKomChanged(INPUT KatalogKom AS LOGICAL):
       
       IF KatalogKom = TRUE THEN DO:
         THIS-OBJECT:RibbonToolKoderAnm:Checked              = FALSE. 
         THIS-OBJECT:RibbonToolKoderAnmBerKalk:Checked              = FALSE.
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
         IF THIS-OBJECT:KalkControl:GridKalkylKoder:tBS:Count > 0 THEN DO:
            THIS-OBJECT:ValueKalkKoderCh(?,?). 
           
         END.   
         ELSE IF THIS-OBJECT:KalkControl:GridLopKoder:tBS:Count > 0 THEN DO:
            THIS-OBJECT:ValueLopnrCh(?,?).
         END.   
         
      END.   
      THIS-OBJECT:SplitBildVisa().
      
     
   END METHOD.
   METHOD PUBLIC VOID KoderAnmChangedKalk(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:KoderAnmChanged(THIS-OBJECT:RibbonToolKoderAnm:Checked).
   END METHOD.
   METHOD PUBLIC VOID KoderAnmChangedBer(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
       THIS-OBJECT:KoderAnmChanged(THIS-OBJECT:RibbonToolKoderAnmBerKalk:Checked).  
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT KomAnm AS LOGICAL):
      IF KomAnm = TRUE THEN DO:
         THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE.
         THIS-OBJECT:RibbonToolKatalogKom:Checked = FALSE.
         THIS-OBJECT:RibbonToolKatalogKomBerKalk:Checked = FALSE.
         IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
             THIS-OBJECT:KalkControl:Rubrikanm:Text = "Anmärkning för " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99").
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
   
   
   
   METHOD PUBLIC VOID EndastSumma(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:KalkylLoaded = TRUE THEN DO:
         IF THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE AND THIS-OBJECT:RibbonToolEndastSumma:CHECKED = TRUE THEN DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = FALSE.
         END.
      END.
      ELSE DO:
         IF THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE AND THIS-OBJECT:RibbonToolEndastSumma:CHECKED = TRUE THEN DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = FALSE.
         END.
      END.        
   END METHOD.   
   METHOD PUBLIC VOID EgnaFaktorer(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:KalkylLoaded = TRUE THEN DO:
         IF THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE AND THIS-OBJECT:RibbonToolEndastSumma:CHECKED = TRUE THEN DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = FALSE.
         END.
      END.
      ELSE DO:
         IF THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE AND THIS-OBJECT:RibbonToolEndastSumma:CHECKED = TRUE THEN DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolEndastFaktorSumma:SharedPropsInternal:Enabled = FALSE.
         END.
      END.         
   END METHOD.
   METHOD PUBLIC VOID EndastFaktorSumma(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     
      IF THIS-OBJECT:KalkylLoaded = TRUE THEN DO:
         IF THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE THEN DO:
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:CHECKED = FALSE.
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Enabled = FALSE.
         END.
      END.
      ELSE DO:
         IF THIS-OBJECT:RibbonToolEndastFaktorSumma:CHECKED = FALSE THEN DO:
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Enabled = TRUE.
         END.
         ELSE DO:
            THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Enabled = FALSE.
         END.
      END.        
   END METHOD.   
   METHOD PUBLIC VOID FriKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     
      THIS-OBJECT:KalkControl:GridKalkylKoder_AfterSelectChange(?,?).
      IF THIS-OBJECT:RibbonToolFriKalk:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:ENABLED = TRUE.
         THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkControl:toolStripAllaKoder:VISIBLE = FALSE.
         THIS-OBJECT:KalkControl:toolStripFriKalk:VISIBLE = TRUE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = FALSE.
         THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = FALSE. 
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 1.
         IF THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE  THEN.
         ELSE DO:
            System.Windows.Forms.MessageBox:Show("Du kan bara gå till Fri kalkyl via fliken Kalkyl!").
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
         THIS-OBJECT:RibbonToolKodSokning:SharedPropsInternal:VISIBLE = FALSE.
         THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:VISIBLE = FALSE.
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = FALSE.
         
         /*    
         THIS-OBJECT:RibbonToolFriGenerera:SharedPropsInternal:Visible = TRUE.
         */
      END.
      ELSE DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:ExtraToolStripMenuItem:ENABLED = FALSE.   
         THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = TRUE.
         
         /*ENDAST ELPAO GÖRA KALK AV ALLA KODER*/
         IF Guru.Konstanter:globanv = {setuser.I} THEN DO:
            THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = FALSE.
            THIS-OBJECT:KalkControl:toolStripAllaKoder:VISIBLE = TRUE. 
            THIS-OBJECT:KalkControl:toolStripFriKalk:VISIBLE = FALSE.
            THIS-OBJECT:KalkControl:toolStripButtonTomKonst:VISIBLE = FALSE.
            THIS-OBJECT:KalkControl:toolStripButtonHmtGrund:VISIBLE = FALSE.
         END.
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:KalkControl:splitContainerGrundKalk:Panel1Collapsed = FALSE.
            THIS-OBJECT:KalkControl:toolStripAllaKoder:VISIBLE = TRUE.
            THIS-OBJECT:KalkControl:toolStripButtonTomKonst:VISIBLE = TRUE.
            THIS-OBJECT:KalkControl:toolStripButtonHmtGrund:VISIBLE = TRUE.
            THIS-OBJECT:KalkControl:toolStripButtonAllaKoder:VISIBLE = FALSE.
         END.  
         ELSE DO:
            THIS-OBJECT:KalkControl:toolStripButtonTomKonst:VISIBLE = FALSE.
            THIS-OBJECT:KalkControl:toolStripButtonHmtGrund:VISIBLE = FALSE.
         END.   
         /*
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 0.
         */
         THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = INTEGER(THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisVisaFor")).
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
         THIS-OBJECT:RibbonToolKodSokning:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = TRUE.
         THIS-OBJECT:KoderAnmChangedKalk(?,?).       
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID GruppMatrisChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:RibbonToolVisGroupMatris:Checked EQ TRUE THEN DO:
         THIS-OBJECT:RibbonToolVisMatris:Text = "Alla".         
      END.
      
   END METHOD.
   METHOD PUBLIC VOID KoderGruppMatrisChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolGroupMatris:Checked EQ TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("BY MATRIS BY ARBKOD  BY LOPNR BY MARKSUB BY NUM").
         THIS-OBJECT:RibbonToolMatris:Text = "Alla".         
      END.
      ELSE THIS-OBJECT:KalkControl:GridKalkylKoder:AterstallSortMenuItem_Click(?,?).
     
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
      Q = "FOR EACH " + THIS-OBJECT:KalkdbControl:MarkningTTh:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
      CREATE QUERY mQ.
      mQ:SET-BUFFERS(THIS-OBJECT:KalkdbControl:MarkningTTh).
      mQ:QUERY-PREPARE(Q).
      mQ:QUERY-OPEN().
      mQ:GET-FIRST().
      DO WHILE mQ:QUERY-OFF-END = FALSE:
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add(THIS-OBJECT:KalkdbControl:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE).
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
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE ksubid AS INTEGER NO-UNDO.
      DEFINE VARIABLE Typ        AS INTEGER   NO-UNDO INITIAL 1.
      DEFINE VARIABLE Markning   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE DoSokning AS LOGICAL INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE totsok AS CHARACTER NO-UNDO.
     
      ksubid = THIS-OBJECT:GetVisaDelKat().
      IF ksubid > 0 THEN totsok = "KLOGSUBID = "+ STRING(ksubid) + " AND ". 
      Typ = THIS-OBJECT:GetVisaTyp().
      totsok = totsok + "TYPKALK = " + STRING(Typ).
      
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      THIS-OBJECT:FriInSet().
      IF Markning EQ "Alla" OR Markning EQ "" THEN.
      ELSE DO: 
         IF Markning EQ "Ej märkta" THEN totsok = totsok + " AND MARKNING = ''".
         ELSE totsok = totsok + " AND MARKNING = '" + Markning + "'".
      END.  
        
        
      IF THIS-OBJECT:RibbonToolKodSokning:Checked EQ TRUE OR THIS-OBJECT:RibbonToolKodSokningBerKalk:Checked EQ TRUE THEN DO:
        DoSokning = TRUE.
      END.
        
      THIS-OBJECT:KalkControl:GridLopKoder:GuruAvmarkera().
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFiltrera(totsok).
      THIS-OBJECT:KalkControl:GridArbetsKoder:GuruFirstrow().
      
      IF THIS-OBJECT:KalkdbControl:ArbetskoderTTh:AVAILABLE EQ TRUE THEN DO:
        
         IF DoSokning EQ TRUE THEN DO:
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera(totsok + " AND " + Helpers.Functions:SkapaMultiSok(THIS-OBJECT:KalkControl:KodSokInput:Text, "BENAMNING")).
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
            
         END.
         ELSE DO:
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
            THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         END.
       
      END.
      ELSE DO:
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         IF THIS-OBJECT:KalkylLoaded EQ TRUE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(198),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         END.
      END.
   END METHOD.
   METHOD PUBLIC VOID UpdateRibbonShow():
      
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:RibbonToolInfTyp:Text = THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:Text.
      ELSE  THIS-OBJECT:RibbonToolInfTyp:Text = "Typ " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE).
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
   METHOD PUBLIC VOID UpdateRibbonKatShow():
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      i = 0.
      IF THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems:Count = 0 THEN DO:
         
      END.
      ELSE DO WHILE i < THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems:Count :
         IF THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems[i]:DisplayText = "Alla" THEN DO:
            THIS-OBJECT:RibbonToolDelKat:SelectedItem = THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems[i].
            RETURN.
         END.
         i = i + 1.
      END.  
      
          
   END METHOD.
   METHOD PUBLIC VOID LoadRibbonLevs(typ AS LOGICAL):
      DEFINE VARIABLE w     AS Infragistics.Win.ValueListItem NO-UNDO.
      DEFINE VARIABLE wData AS Helpers.ABLDataContainer       NO-UNDO.
      DEFINE VARIABLE qH    AS HANDLE                         NO-UNDO.
      DEFINE VARIABLE sw    AS Integer                        NO-UNDO INITIAL 0.
      DEFINE VARIABLE ssw   AS Integer                        NO-UNDO.
      IF typ = TRUE THEN  THIS-OBJECT:RibbonToolLevBerKalk:ValueList:ValueListItems:Clear().
      ELSE THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Clear().
      
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.Konstanter:LeverantorTTh, "FOR EACH " + Guru.Konstanter:LeverantorTTh:TABLE + " NO-LOCK").
      qH:GET-FIRST().
      DO WHILE qh:QUERY-OFF-END = FALSE:
         w = NEW Infragistics.Win.ValueListItem().
         w:DisplayText = STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE()).
         wData = NEW Helpers.ABLDataContainer().
         wData:CreateData("LEVKOD", STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE())).
         w:DataVALUE = wData.
       
         IF STRING(Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE()) = Guru.Konstanter:HuvudLeverantor THEN DO:
            wData:CreateData("ISHUV", TRUE).
            ssw = sw.
            
         END.
         ELSE DO:
            wData:CreateData("ISHUV", FALSE).
         END.
         IF typ = TRUE THEN THIS-OBJECT:RibbonToolLevBerKalk:ValueList:ValueListItems:Add(w). 
         ELSE THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Add(w).
         sw = sw + 1 . 
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      IF typ = TRUE THEN DO:
         THIS-OBJECT:RibbonToolLevBerKalk:ValueList:ValueListItems:Add("Alla leverantörer").
         THIS-OBJECT:RibbonToolLevBerKalk:SelectedIndex = ssw.
        
      END.
      ELSE DO.
         THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Add("Alla leverantörer").
         THIS-OBJECT:RibbonToolLev:SelectedIndex = ssw.
        
      END.   
      
   END METHOD.
   METHOD PUBLIC VOID RibbonSokKodUpdate(Arbkodvar AS CHARACTER, Lopnrvar AS CHARACTER):
      
      THIS-OBJECT:KalkVisaControl:ArbKalkTTh:FIND-FIRST("WHERE  " ,NO-LOCK) NO-ERROR.
      IF STRING(Lopnrvar) = "" THEN DO:
         THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:FIND-FIRST("WHERE  ARBKOD = " + QUOTER(Arbkodvar) + " USE-INDEX SOK" ,NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:AVAILABLE THEN DO:
            IF Guru.GlobalaVariabler:TTRECID = THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE THEN DO:
               THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:FIND-FIRST("WHERE  ARBKOD = " + QUOTER(Arbkodvar) +  
               " AND  TTRECID > " + STRING(Guru.GlobalaVariabler:TTRECID) + " USE-INDEX SOK",NO-LOCK) NO-ERROR.
            END.      
         END.
      END.
      ELSE DO: 
         THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:FIND-FIRST("WHERE  ARBKOD = " + QUOTER(Arbkodvar) + " AND LOPNR = " + STRING(Lopnrvar) + " USE-INDEX SOK",NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:AVAILABLE THEN DO:
            IF Guru.GlobalaVariabler:TTRECID >= THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE THEN DO:
               THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:FIND-FIRST("WHERE  ARBKOD = " + QUOTER(Arbkodvar) + " AND LOPNR = " + STRING(Lopnrvar) + 
               " AND  INTEGER(TTRECID) > " + STRING(Guru.GlobalaVariabler:TTRECID) + " USE-INDEX SOK" ,NO-LOCK) NO-ERROR.
            END.      
         END.
      END.   
       
      IF THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:AVAILABLE THEN DO:
         /*repo multigrid repo gridjoin*/
         Guru.GlobalaVariabler:TTRECID = THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
         THIS-OBJECT:KalkVisaControl:GridArbVisa:GuruRepositionTo(THIS-OBJECT:KalkVisaControl:ArbKalkKoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE).
      END.
      ELSE DO:
         Guru.GlobalaVariabler:TTRECID = ?.
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(206)).
         RETURN.
         
      END.         
   END METHOD.
   METHOD PUBLIC VOID RibbonSokUpdate(enr AS CHARACTER, ben AS CHARACTER ):
      DEFINE VARIABLE kundvar AS LOGICAL NO-UNDO.
      kundvar = ?.
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN enr = THIS-OBJECT:RibbonToolEnrBerKalk:Text.
      ELSE enr = THIS-OBJECT:RibbonToolEnr:Text.
      IF LENGTH(TRIM(enr + ben)) < 2 THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(207)).
         RETURN.
      END.
      
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:KalkControl:GridEnrFran:GuruTom().
         THIS-OBJECT:SokMtrlBerKalk(enr, ben, kundvar).
         IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:KalkControl:GridEnrFran:GuruOrderby("BY kund DESCENDING by enr").
         THIS-OBJECT:KalkControl:GridEnrFran:GuruReopen().
         THIS-OBJECT:KalkControl:GridEnrFran:GuruFirstrow(). 
      END.   
      ELSE DO.
         IF THIS-OBJECT:RibbonToolEndastmarkmtrl:Checked = TRUE THEN kundvar = TRUE.
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruTom().
         THIS-OBJECT:SokMtrl(enr, ben, kundvar).
         IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruOrderby("BY kund DESCENDING by enr").
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruReopen().
         THIS-OBJECT:KalkMtrlControl:GridMateriel:GuruFirstrow().
      END. 
   END METHOD.
   /* Ribbon events*/
   METHOD PUBLIC VOID RibbonSokUpdateBen(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
      enr = "".
      IF e:KeyChar  = CHR(13) THEN DO:
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN ben = THIS-OBJECT:RibbonToolSokBerKalk:Text.
         ELSE ben = THIS-OBJECT:RibbonToolSok:Text.
         THIS-OBJECT:RibbonSokUpdate(enr,ben).
         e:Handled = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonSokUpdateEnr(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
     
      ben = "".
      IF e:KeyChar  = CHR(13) THEN DO:
         enr = THIS-OBJECT:RibbonToolEnr:Text.
         THIS-OBJECT:RibbonSokUpdate(enr,ben).
         e:Handled = TRUE.         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RibbonSokLopKod(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
     DEFINE VARIABLE blockedKey AS LOGICAL NO-UNDO.
      DEFINE VARIABLE ArbKod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE Lopnr AS CHARACTER NO-UNDO.
     
      IF e:KeyChar  = CHR(13) THEN DO:
         ArbKod = THIS-OBJECT:RibbonArbSok:Text.
         Lopnr = THIS-OBJECT:RibbonLopSok:Text.
         THIS-OBJECT:RibbonSokKodUpdate(ArbKod,Lopnr).
         e:Handled = TRUE.  
         RETURN.       
      END.
      Lopnr = e:KeyChar.
      
      IF ASC(Lopnr) >= 48 AND ASC(Lopnr) <= 57 THEN RETURN.
      THIS-OBJECT:RibbonLopSok:Text = "".
      
         
   END METHOD.
   
   
   
   METHOD PUBLIC VOID RibbonSokArbKod(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
     
      DEFINE VARIABLE ArbKod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE Lopnr AS CHARACTER NO-UNDO.
      IF e:KeyChar  = CHR(13) THEN DO:
      
         ArbKod = THIS-OBJECT:RibbonArbSok:Text.
         Lopnr = THIS-OBJECT:RibbonLopSok:Text.
         THIS-OBJECT:RibbonSokKodUpdate(ArbKod,Lopnr).
         e:Handled = TRUE.         
      END.
  
   END METHOD.
   METHOD PUBLIC VOID SokMtrl(enr AS CHARACTER,ben AS CHARACTER,kundvar AS LOGICAL):
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
     
      IF STRING(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
      ELSE lev = STRING( CAST(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellVALUE("LEVKOD").*/
      THIS-OBJECT:Root:DatabaseManager:Global:FetchMateriel(lev, enr, ben, kundvar).
      
            
   END METHOD.
   METHOD PUBLIC VOID SokMtrlBerKalk(enr AS CHARACTER,ben AS CHARACTER,kundvar AS LOGICAL):
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
      IF STRING(THIS-OBJECT:RibbonToolLevBerKalk:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLevBerKalk:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
      ELSE lev = STRING( CAST(THIS-OBJECT:RibbonToolLevBerKalk:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLevBerKalk:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). 
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
   
   METHOD PUBLIC VOID SetVisaTyp(INPUT val AS INTEGER):
      IF val < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      ELSE THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = val - 1.
      
   END METHOD.
   
   /*öppnar arbetskoder ned rätt query för vald typ*/
   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      
      IF THIS-OBJECT:KalkControl:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:GetVisaTyp() = 7 THEN DO:
        
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Nät").
        
         THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Nät".
      END. 
      ELSE IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         THIS-OBJECT:RefreshMarkning().
       END. 
      THIS-OBJECT:FriInSet().
   END METHOD.
   
   /*öppnar arbetskoder ned rätt query för vald KAT*/
   METHOD PUBLIC VOID RibbonDelKatVal(sender AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE ksubid AS INTEGER NO-UNDO.
      IF THIS-OBJECT:KalkControl:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      
      IF THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems[THIS-OBJECT:RibbonToolDelKat:SelectedIndex]:DisplayText MATCHES "*KLG2*" THEN DO:
        IF THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Region" THEN THIS-OBJECT:RefreshArbKoder(?,?).
        ELSE THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Region".
         
      END.    
      
      ELSE DO:
         IF THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Ej märkta" THEN THIS-OBJECT:RefreshArbKoder(?,?).
         ELSE IF THIS-OBJECT:RibbonToolFilterTyp:TEXT = "ALLA" THEN THIS-OBJECT:RefreshArbKoder(?,?).
         ELSE IF THIS-OBJECT:GetVisaTyp() = 7 THEN THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Nät".
         ELSE THIS-OBJECT:RibbonToolFilterTyp:TEXT = "Ej märkta".
                
      END. 
     
      ksubid = THIS-OBJECT:GetVisaDelKat().
      THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruOrderby("BY KPID").
      
      IF ksubid = -1 OR  ksubid = 0 THEN DO:
         THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFiltrera("kalkylprisertt.EGENKODUPP = TRUE").
         IF KalkFaktControl NE ? THEN DO: 
            THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruOrderby("BY KPID").
            THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruFiltrera("").
         END.   
      END.   
      ELSE DO:
         THIS-OBJECT:KalkEgnaControl:GridUnderKoder:GuruFiltrera("kalkylprisertt.EGENKODUPP = TRUE AND kalkylprisertt.KLOGSUBID = " + STRING(ksubid)).
         IF KalkFaktControl NE ? THEN DO: 
            THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruOrderby("BY KPID").
            THIS-OBJECT:KalkFaktControl:GridFaktorer:GuruFiltrera("kalkfaktorertt.KLOGSUBID = " + STRING(ksubid)).
         END.   
      END.
   END METHOD.
   METHOD PUBLIC INTEGER GetVisaDelKat():
     
      IF THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems:Count = 0 THEN RETURN -1.
      IF THIS-OBJECT:RibbonToolDelKat:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolDelKat:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems:Count > 0 THEN DO:
         RETURN CAST(THIS-OBJECT:RibbonToolDelKat:ValueList:ValueListItems[THIS-OBJECT:RibbonToolDelKat:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetInteger("DelK").
      END.
      RETURN -1.
   END METHOD.
   
   
   METHOD PUBLIC VOID RibbonUpdateUppfPriser(sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE = 1 + THIS-OBJECT:RibbonToolUppfPriser:SelectedIndex.
      THIS-OBJECT:KalkdbControl:KalkSpara().
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
      ultraTab15:ToolTipText = "Skapa egna koder".
      appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance9.
      appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
      ultraTab17:Appearance = appearance10.
      ultraTab17:Key = "FaktorerPriser".
      ultraTab17:TabPage = THIS-OBJECT:TabFaktorer.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "Lägg till egna faktorer/priser".
      appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance11.
      appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
      ultraTab19:Appearance = appearance12.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget materiel".
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
      THIS-OBJECT:v11fix().
      THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      PROCESS EVENTS.
   END METHOD.
   METHOD PUBLIC VOID v11fix():
      /*v11.2 fix*/
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:GetComboV(KalkHuvudControl:HuvudOmrade).
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:GetComboV(KalkHuvudControl:HuvudUtfardare).
      IF THIS-OBJECT:KalkAvtalControl NE ? THEN THIS-OBJECT:GetComboV(KalkAvtalControl:ComboAvtal).
      IF THIS-OBJECT:KalkKopKonvControl NE ? THEN THIS-OBJECT:GetComboV(KalkKopKonvControl:KopHuvudOmrade).
      IF THIS-OBJECT:KalkHuvudControl NE ? THEN THIS-OBJECT:GetComboV(KalkHuvudControl:BestKund).
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
      THIS-OBJECT:v11fix().
      
      IF cFrom EQ ? THEN cFrom = "".  
      IF cTo = "Status" THEN DO:
         THIS-OBJECT:RibbonTabVisning:Visible = TRUE.
         THIS-OBJECT:VisaTabs("ALLA", FALSE).
         THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE. 
         RETURN. 
      END.
      IF cFrom = "Visa Kalkyl" THEN  Guru.GlobalaVariabler:TTRECID = ?.
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
               IF JmfKalkyler = TRUE THEN DO:
                  THIS-OBJECT:JmfKalkyler().
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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         THIS-OBJECT:RibbonTabBerKalk:Visible = FALSE.
         THIS-OBJECT:RibbonTabBerKalkMtrlSok:Visible = FALSE. 
      END.
      IF cTo = "Visa Kalkyl" THEN DO:
         Guru.GlobalaVariabler:TTRECID = ?.
         THIS-OBJECT:RibbonTabVisningEgenskaper:Visible = TRUE.
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Visa"). 
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
         THIS-OBJECT:VisPriserChanged(?,?).
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:RibbonTabBerKalkVisa:Visible = TRUE.
            THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning - Visa " + Guru.GlobalaVariabler:BeredningsNamn).
         END.
         THIS-OBJECT:KalkVisaControl:GenereraArbKalkylVisning().
         THIS-OBJECT:WasExcelShown = TRUE.
         
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
         THIS-OBJECT:RibbonTabVisningEgenskaper:Visible = FALSE.
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:RibbonTabBerKalkVisa:Visible = FALSE.
         END.    
            
      END.
      
      IF cTo = "FaktorerPriser" THEN DO:
         IF KalkFaktControl = ? THEN DO:
            KalkFaktControl = NEW Modules.Kalkyl.KalkFaktControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabFaktorer:Controls:Add(THIS-OBJECT:KalkFaktControl).
            KalkFaktControl:Dock = System.Windows.Forms.DockStyle:FILL.
            THIS-OBJECT:RibbonDelKatVal(?, ?).
         END.   
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Faktorer och priser").
         THIS-OBJECT:RibbonTabFaktPris:VISIBLE = TRUE. 
      END.     
      ELSE DO:
         THIS-OBJECT:RibbonTabFaktPris:VISIBLE = FALSE. 
      END.    
      IF cTo = "Kalkyl" THEN DO: 
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera"). 
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning " + Guru.GlobalaVariabler:BeredningsNamn).
            THIS-OBJECT:RibbonTabBerKalk:Visible = TRUE.
            IF THIS-OBJECT:RibbonToolKoderAndMtrlmBer:CHECKED = TRUE THEN THIS-OBJECT:RibbonTabBerKalkMtrlSok:Visible = TRUE.
         END.    
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
         THIS-OBJECT:KalkControl:GridFriKalk:GuruReopen().
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END. 
      IF cTo = "Egna koder" THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Egna koder").
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning - Egna koder " + Guru.GlobalaVariabler:BeredningsNamn).
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
         IF KalkMtrlControl = ? THEN DO:
            KalkMtrlControl = NEW Modules.Kalkyl.KalkMtrlControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:KalkMtrlControl).
            KalkMtrlControl:Dock = System.Windows.Forms.DockStyle:FILL.
         END.
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Eget materiel").
         THIS-OBJECT:RibbonTabMat:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabMat:Visible = FALSE.
      END.
      IF cTo = "ImportMallar" THEN DO:
         IF KalkImpMallControl = ? THEN DO:
            KalkImpMallControl = NEW Modules.Kalkyl.KalkImpMallControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:KalkImpMallControl).
            KalkImpMallControl:Dock = System.Windows.Forms.DockStyle:FILL.
         END.   
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Import/Mallar").
         THIS-OBJECT:KalkdbControl:KalkmallHuvudtth:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:KalkmallHuvudtth:AVAILABLE THEN.
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:MallarHmt().
            THIS-OBJECT:KalkImpMallControl:GridMallar:GuruReopen().
            THIS-OBJECT:KalkImpMallControl:GridMallar:GuruFirstRow().
         END. 
         
      END.
      IF cTo = "Avtalskalkyl" THEN DO:
         IF KalkAvtalControl = ? THEN DO:
            KalkAvtalControl = NEW Modules.Kalkyl.KalkAvtalControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:KalkAvtalControl).
            KalkAvtalControl:Dock = System.Windows.Forms.DockStyle:FILL.
            THIS-OBJECT:VisaTabs("Avtalskalkyl", TRUE).
            THIS-OBJECT:AvtalsCombFill().
                                         
         END.
         
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Avtalskalkyl").
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Avtalskalkyl " + Guru.GlobalaVariabler:BeredningsNamn).
         /*THIS-OBJECT:RibbonToolVisVisa:SelectedIndex = 6.*/
      END.    
      IF cTo = "Volymber" THEN DO:
         
         IF KalkVolymControl = ? THEN DO:
            KalkVolymControl = NEW Modules.Kalkyl.KalkVolymControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabVolymber:Controls:Add(THIS-OBJECT:KalkVolymControl).
            KalkVolymControl:Dock = System.Windows.Forms.DockStyle:FILL.
         END.
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Volymberäkningar av schaktmassor").
         THIS-OBJECT:KalkdbControl:VolymberTTbuffh:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:VolymberTTbuffh:AVAILABLE THEN.
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBerHmt().
            THIS-OBJECT:KalkVolymControl:GridVolymBer:GuruReopen().
            THIS-OBJECT:KalkVolymControl:GridVolymBer:GuruFirstRow().
         END.
        
      END.
                
      IF cTo = "KopieraKalkyl" THEN DO:
          
         IF KalkKopKonvControl = ? THEN DO:
            KalkKopKonvControl = NEW Modules.Kalkyl.KalkKopKonvControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:KalkKopKonvControl).
            KalkKopKonvControl:Dock = System.Windows.Forms.DockStyle:FILL.
            THIS-OBJECT:CreateComboKonvTyp().
            THIS-OBJECT:KalkKopKonvControl:CreateIni().
         END.
         
         THIS-OBJECT:KalkKopKonvControl:HuvudBenamning:GuruLabel:Text  = "Ny Benämning".
         IF THIS-OBJECT:KalkHuvudControl = ? THEN.
         ELSE THIS-OBJECT:KalkKopKonvControl:HuvudBenamning:GuruText:Text = THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text + " Kopierad-Konverterad".
         
         THIS-OBJECT:GetComboV(KalkKopKonvControl:KopHuvudOmrade).
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kopiera/Konvertera kalkyl").
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning - Kopiera/Konvertera kalkyl " + Guru.GlobalaVariabler:BeredningsNamn ). 
         
         Guru.GlobalaVariabler:NyBerKalkyl = FALSE.
      END.
      IF cTo = "Felmedd" THEN DO:
         IF KalkFelmeddControl = ? THEN DO:
            KalkFelmeddControl = NEW Modules.Kalkyl.KalkFelmeddControl(THIS-OBJECT:Root).
            THIS-OBJECT:TabFelmedd:Controls:Add(THIS-OBJECT:KalkFelmeddControl).
            KalkFelmeddControl:Dock = System.Windows.Forms.DockStyle:FILL.
         END.
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Felmeddelanden").
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning - Felmeddelanden " + Guru.GlobalaVariabler:BeredningsNamn).
         THIS-OBJECT:KalkFelmeddControl:GridFelMedd:GuruReopen().
         THIS-OBJECT:KalkFelmeddControl:GridFelMedd:GuruFirstRow().           
      END.
      IF cTo = "Kalkylhuvud" THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Information").
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Kalkylera i Beredning - Information " + Guru.GlobalaVariabler:BeredningsNamn).
      END.
      
      PROCESS EVENTS.
  END METHOD.
  METHOD PUBLIC VOID AvtalsCombFill():
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     
      THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchAvtal(INPUT THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE ).                                               
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:Avtalskalktth,queryvar).
      qH:GET-FIRST().
      IF THIS-OBJECT:KalkdbControl:Avtalskalktth:AVAILABLE THEN DO:
         THIS-OBJECT:CreateComboAvtal().
      END.
      IF KalkAvtalControl NE ? THEN DO:
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("VISARTAL"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE THEN DO:
               THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE = STRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE).                              
            END.   
            qH:GET-NEXT().
         END.
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
  END METHOD. 
  METHOD PUBLIC VOID VisningKalkIniMulti():
     DEFINE VARIABLE path  AS CHARACTER NO-UNDO.
     
     THIS-OBJECT:RibbonTabKalkyl:Visible = FALSE.
     /*
     THIS-OBJECT:RibbonTabVisningVisa:Visible = FALSE.
     */
     THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = FALSE.
     THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = FALSE.
     
     THIS-OBJECT:RibbonToolVisMatris:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisArbKalk:SharedPropsInternal:Enabled = FALSE.
   
    
     THIS-OBJECT:RibbonToolVisEgnaFaktorer:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisEgnaPriser:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolVisFrekvens:SharedPropsInternal:Enabled = FALSE.  
     THIS-OBJECT:RibbonToolVisKom:SharedPropsInternal:Enabled = FALSE.
     THIS-OBJECT:RibbonToolEndastSumma:SharedPropsInternal:Enabled = FALSE. 
          
     THIS-OBJECT:RibbonToolVisEgnaFaktorer:Checked = FALSE.
     THIS-OBJECT:RibbonToolVisEgnaPriser:Checked = FALSE.
     THIS-OBJECT:RibbonToolVisFrekvens:Checked = FALSE.  
     THIS-OBJECT:RibbonToolVisKom:Checked = FALSE.
     THIS-OBJECT:RibbonToolEndastSumma:CHECKED = FALSE.
     THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.
     IF vismulti = TRUE THEN DO:
        THIS-OBJECT:RibbonTabVisningEgenskaper:Caption = "Visningsinställningar hämtade från " + Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
       /*
        MESSAGE "Visningen av flera kalkyler samtidigt. Visas just nu bara i Excelformat!"
        VIEW-AS ALERT-BOX.
        */
     END.    
     THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel1Collapsed = TRUE.
     THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel2Collapsed = FALSE.

     THIS-OBJECT:SetVisningsBild(0).
      
      
     
  END METHOD. 
    
   /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:44:31 
   Sätter UsMsInterrop, skapar en excel fil för skrivning av template 
   startar Interop excel
   */
   
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE.
    
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:         
         THIS-OBJECT:SetVisningsBild(0).
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
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
      IF THIS-OBJECT:KalkdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:KalkControl:GridLopKoder:GuruFirstrow().
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.
   END METHOD.
   
   
   /*används inte?*/
   METHOD PRIVATE INTEGER GetCurrentType():
      
      RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:ValueList:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer):GetInteger("Typ").
   END METHOD.

   METHOD PUBLIC CHARACTER GetMatrisName():
      DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:KalkdbControl:HuvudTTh:Buffer-field("TYPKALK"):Buffer-VALUE = 7 THEN mName = "Period".
      ELSE mName = "Matris".
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         mName = "Sträcka".
         IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:GForetag = "AKEAUTBI" OR  Guru.konstanter:globforetag = "ATS"  THEN mName ="Byggnr".
      END.    
      RETURN mName.
   END METHOD.
   
   
   METHOD PUBLIC VOID ValueLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmt(INPUT THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
         THIS-OBJECT:KalkControl:GridFrekvens:GuruReopen().
         IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN  DO:
            THIS-OBJECT:KalkControl:GridFrekvens:GuruUpdateTitle(" för :" + STRING(THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).
          END. 
        
      END.
      
      IF THIS-OBJECT:RibbonToolKatalogKom:Checked EQ TRUE OR THIS-OBJECT:RibbonToolKatalogKomBerKalk:Checked EQ TRUE THEN DO:
         IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN  DO:  
            THIS-OBJECT:KalkControl:RubrikKom:Text = "Kommentar för " + STRING(THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99").            
            THIS-OBJECT:KalkControl:ValdKodKommentar:Text = THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         END.   
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
         IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN  DO:
            IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN THIS-OBJECT:KalkControl:Rubrikanm:Text = "Anmärkning för " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99").
            ELSE THIS-OBJECT:KalkControl:RubrikKom:Text = "Kommentar för " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99").
            THIS-OBJECT:KalkControl:KalkylensKoderAnmarkning:Text  = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
            THIS-OBJECT:KalkControl:ValdKodKommentar:Text = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
            IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmt(INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                              INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
               THIS-OBJECT:KalkControl:GridFrekvens:GuruReopen().
               THIS-OBJECT:KalkControl:GridFrekvens:GuruUpdateTitle(" för :" + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).                                                
          
            END.        
         END.   
      END.
   END METHOD.
   
   
   METHOD PRIVATE VOID ValueEgnaKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
      /* ROBIN Avkommenterat 3/6/13: IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 THEN DO:*/
      THIS-OBJECT:FilterUnderKoder().
      /*END. */  
   END METHOD.
   
   METHOD PRIVATE VOID GridBerVal_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString(). 
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = kolvarde.
    
   END METHOD.
   METHOD PRIVATE VOID GridBerVal_BeforeRowActivate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.RowEventArgs ):
       IF THIS-OBJECT:RibbonToolKoderAndMtrlmBer:CHECKED = FALSE  THEN RETURN. 
       IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkdbControl:SparaMtrl().
      END.
      RETURN.

   END METHOD.
   METHOD PUBLIC VOID GridBerVal_AfterCellListCloseUp( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs):
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolvarde = comboListKonst:SelectedItem:ToString() NO-ERROR.
      THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = kolvarde.
      IF INDEX(kolvarde,",") > 0 THEN  F2SkapNum = INTEGER(TRIM(SUBSTRING(kolvarde,1,INDEX(kolvarde,",") - 1))).
      ELSE F2SkapNum = 0. 
      IF F2SkapNum = 0 THEN DO:
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " BY ENR").
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruFirstrow().
      END.   
      ELSE DO.
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + 
         " AND SKAPNUM = " + STRING(F2SkapNum) + 
         " BY ENR").
         THIS-OBJECT:KalkControl:GridBerMtrl:GuruFirstrow().
      END.
      
   END METHOD.
   
   
   METHOD PUBLIC VOID ValueBerValCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
     
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Hidden = TRUE.
      IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
         kolvarde = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE. 
         IF THIS-OBJECT:RibbonToolKoderAndMtrlmBer:CHECKED = TRUE THEN DO:
            IF INDEX(kolvarde,",") > 0 THEN  F2SkapNum = INTEGER(TRIM(SUBSTRING(kolvarde,1,INDEX(kolvarde,",") - 1))).
            ELSE F2SkapNum = 0. 
            IF F2SkapNum = 0 THEN DO:
               THIS-OBJECT:KalkControl:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " BY ENR").
               THIS-OBJECT:KalkControl:GridBerMtrl:GuruFirstrow().
            END.   
            ELSE DO.
               THIS-OBJECT:KalkControl:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + 
               " AND SKAPNUM = " + STRING(F2SkapNum) + 
               " BY ENR").
               THIS-OBJECT:KalkControl:GridBerMtrl:GuruFirstrow().
            END.
            THIS-OBJECT:KalkdbControl:InkopKoll(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
         END.
         ELSE DO:
            THIS-OBJECT:KalkControl:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " BY ENR").
            THIS-OBJECT:KalkControl:GridBerMtrl:GuruFirstrow().
         END.
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " BY ARBKOD BY LOPNR").
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("kalknumtt.ARBKOD = 'EGEN' AND BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstrow().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid = 0.
         THIS-OBJECT:RibbonToolKonstruktion:SharedPropsInternal:Caption = STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)
         + " " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE).
         THIS-OBJECT:FillcomboListKonst().
      END.
      
   END METHOD.
   METHOD PUBLIC VOID ValueSchaktValCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      
     /* IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:GForetag = "AKEAUTBI" OR  Guru.konstanter:globforetag = "ATS" THEN.
      ELSE THIS-OBJECT:KalkControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Hidden = FALSE.*/
      IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFiltrera("SID = " + STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE) + " BY MATRIS BY ARBKOD BY LOPNR").
         THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFiltrera("kalknumtt.ARBKOD = 'EGEN' AND SID = " + STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE)).
         THIS-OBJECT:KalkEgnaControl:GridEgnaKoder:GuruFirstrow().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid = THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum = 0.
         THIS-OBJECT:RibbonToolKonstruktion:SharedPropsInternal:Caption = STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValuePersCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/   
      /*
      THIS-OBJECT:KalkHuvudControl:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      */          
   END METHOD.
      
   METHOD PRIVATE VOID VisaTabs(INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE avtalkfilk AS LOGICAL NO-UNDO.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY = vilka THEN DO:
         IF Guru.konstanter:kalk2sekvar[2] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = visavar.
         ELSE THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         
      END.   
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY = vilka THEN DO:
         IF Guru.konstanter:kalk2sekvar[4] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = visavar.
         ELSE THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
         
      END.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Egna koder"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:KEY = vilka THEN DO:
         IF Guru.konstanter:kalk2sekvar[6] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = visavar.
         ELSE THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = FALSE.
         
      END.  
     
      avtalkfilk = FALSE.
      IF vilka = "ALLA" OR vilka = "FRIKALK" OR THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:KEY = vilka THEN DO:
                  
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[11] = TRUE THEN DO: 
            IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
               IF  Guru.GlobalaVariabler:plusaonr = ? THEN avtalkfilk = FALSE.
               ELSE avtalkfilk = TRUE.               
               IF Guru.Konstanter:varforetypval[62] = 1 THEN.
               ELSE avtalkfilk = FALSE.
               /*IF Guru.konstanter:globforetag = "ELPA" OR  Guru.konstanter:globforetag = "FORS"  OR  
               Guru.konstanter:globforetag = "krin" OR Guru.Konstanter:GForetag = "AKEAUTBI" OR  
               Guru.konstanter:globforetag = "ATS" OR  
               Guru.konstanter:globforetag = "GRAN" OR  
               Guru.konstanter:globforetag = "c4en" OR  
               Guru.konstanter:globforetag = "OPPU" THEN.
               ELSE avtalkfilk = FALSE.*/
            END.   
            ELSE DO:
               THIS-OBJECT:KalkdbControl:kalkaonrTTh:FIND-FIRST() NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:kalkaonrTTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:KalkdbControl:kalkaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN avtalkfilk = FALSE.
                  ELSE avtalkfilk = TRUE.
                  IF avtalkfilk = TRUE THEN DO: 
                     IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN avtalkfilk = FALSE.
                     IF STRING(THIS-OBJECT:KalkHuvudControl:HuvudTyp:GuruCombo:VALUE) = "2" THEN.
                     ELSE avtalkfilk = FALSE.
                  END.  
               END.
            END.
            IF avtalkfilk = TRUE THEN DO:
               IF Guru.Konstanter:varforetypval[61] = 1 THEN DO:
               /*IF Guru.konstanter:globforetag = "ELPA" OR Guru.konstanter:globforetag = "GRAN" OR  Guru.konstanter:globforetag = "LIMO" OR  
               Guru.konstanter:globforetag = "FORS" OR  Guru.konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "Sweo" OR 
               Guru.Konstanter:globforetag = "LAPP" OR Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "WIGA" OR Guru.Konstanter:globforetag = "SKOK"
               OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "ENKA" OR Guru.Konstanter:globforetag = "KEKR" OR Guru.Konstanter:globforetag = "WSP" OR
               Guru.konstanter:globforetag = "AKEA" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "svea" OR  Guru.konstanter:globforetag = "ATS"
               OR  Guru.konstanter:globforetag = "VTGE" OR  Guru.konstanter:globforetag = "C4EN"  OR  Guru.konstanter:globforetag = "OPPU" THEN DO:*/
                  queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:Avtalskalktth:TABLE. 
                  qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:Avtalskalktth,queryvar).
                  qH:GET-FIRST().
                  IF THIS-OBJECT:KalkdbControl:Avtalskalktth:AVAILABLE THEN DO:
                     THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
                     THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
                  END.                                  
                  ELSE DO:
                     THIS-OBJECT:AvtalsCombFill(). 
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
      IF THIS-OBJECT:KalkdbControl:HuvudTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
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
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = TRUE.
         IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaDirekt = TRUE THEN.
         ELSE DO: 
            THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = TRUE.
            THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE = TRUE. 
         END.
         THIS-OBJECT:ModuleHandle:RibbonContext:Visible = FALSE.
         THIS-OBJECT:TabManager:Tabs["Status"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:ToolTipText = "Flytta kalkyl till annat kalkylår/katalog".
         /*
         THIS-OBJECT:TabManager:Tabs["KopieraKalkyl"]:VISIBLE   = FALSE.
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
         */
         
         /*
         THIS-OBJECT:TabManager:Tabs["Felmedd"]:VISIBLE   = FALSE.
         */
         THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = FALSE.
         
      END.   
      ELSE DO:
        THIS-OBJECT:KalkControl:splitContainerKalkBer:Panel1Collapsed = TRUE.
        THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = TRUE.
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
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = FALSE.
      
      OS-DELETE VALUE(ExelKommando) NO-ERROR.
      IF vad = "VKALK" THEN DO:
         path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xlsx").
         IF path = ? THEN path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
      END.   
     
      path = THIS-OBJECT:ExcelTemplate(INPUT path).
      ExelKommando = path.            
   END METHOD.
   
   METHOD PUBLIC VOID GenereraKalkylVisning():
      IF THIS-OBJECT:RibbonToolVisArbKalk:Checked = FALSE THEN DO:
         THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel1Collapsed = FALSE.
         THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel2Collapsed = TRUE.
      END.
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
      DEFINE VARIABLE sustcol AS CHARACTER NO-UNDO.
      
      path = ExelKommando.
      ASSIGN 
      cSenasteArbKod     = ""
      iSenasteLopNr      = 0        
      iSenasteMatris     = 0        
      cSenasteMtrl       = ""       
      iSenasteMtrlMatris = 0.
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         Guru.Konstanter:AmericanSet().
         
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:PageSetUppHeader(INPUT 2, INPUT '&C&",Bold"&8' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + CHR(10) +
         '&",Normal"&8' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("GATUADR"):BUFFER-VALUE) + "  " + CHR(10) +
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
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Utrust" THEN utrustrubrik = TRUE.
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      kostnadstartcol = "".
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname.
         IF kostnadstartcol = "" THEN DO:
            IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "EA" THEN.
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
      END.        
      /* Gör rubriker bold */
      THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:Border("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
      THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
      
      /*OM MATRIS*/
      IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE. 
      ELSE DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE + " WHERE kalknumtt.MATRIS = " + STRING(VisMatris()).      
      END.    
      kalkylqueryvar = queryvar.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR BY MARKSUB".
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR BY MARKSUB".
      END.       
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
         ELSE DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().      
            /*    GRUPPERA MATRIS*/
               
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO: 
               THIS-OBJECT:ExcelGruppMatris(INPUT 1, INPUT matrisrader, INPUT kostnadstartcol ).
               IF iSenasteMatris = 0 THEN matrisrader = matrisrader.
               ELSE IF iSenasteMatris NE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE THEN DO:
                  matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:KalkVisaControl:VisaExcel:Getrad(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad))).
               END.   
               /*OM NY MATRIS*/
               iSenasteMatris = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            END.
            /*GRUPPERA KODER*/
            
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppera") = "yes" THEN DO:
               THIS-OBJECT:ExcelGruppKoder().         
            END.
            /*SUMMERA SAMMA KOD*/
            IF STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes" THEN DO:
               IF VisSummera(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.    
            ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            /*UT MED ARB KOD MM*/  
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            /*
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE),"0",TRUE).
            */
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
            /*UT MED TIMMARNA*/
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " WHERE kalkantal.KALKNR = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) +
            " AND kalkantal.NUM = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + 
            " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).
            subqH:GET-FIRST().   
          
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               findfel = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
                  IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE),"0",TRUE).                 
                  END.
                  ELSE DO:   
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE),"0",TRUE).                                
                  END.
               END.
               subqH:GET-NEXT(NO-LOCK).
            END.
            
            /*UT MED KOSTNAD*/
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.KALKNR = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + 
            " AND kalkkostnad.NUM = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               findfel = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
               
               IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
                  IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE),"0",TRUE).                  
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE),"0",TRUE).                  
                  END.
                      
               END.
               /*UT MED EA*/
               THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "EA"') NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
                  IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRIEAMANGD"):BUFFER-VALUE),"0",TRUE).                  
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE),"0",TRUE).                 
                 END.                                 
               END.
               IF Guru.Konstanter:varforetypchar[8] = "1"  THEN DO:                                                                   
                  IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Å" OR THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Ä" OR THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "Ö" THEN .
                  ELSE DO:
                     IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:            
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,sustcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("STFRITOTKOST"):BUFFER-VALUE),"0",TRUE).                  
                     END.
                     ELSE DO:
                        THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFormat(TRUE,sustcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("STTOTKOST"):BUFFER-VALUE),"0",TRUE).                             
                    END.
                 END.                                             
               END.
                                                   
               subqH:GET-NEXT(NO-LOCK).
            END.           
            THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).         
            /*SUMMERA HORISONT*/
            THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,4,"0").
            /*RISKALKYL*/
            IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 2 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 4  THEN DO:          
               IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
               ELSE DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:KalkVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"0").               
               END.   
            END.   
            /*VINSTKALKYL*/
            IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 3 OR INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:          
               IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
               ELSE DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:KalkVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100))).
                  THIS-OBJECT:KalkVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"0").                  
               END.   
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
               THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:  
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  IF VisMatris() = 0 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + 
                     STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(?))).
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),
                     STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(VisMatris()))).
                  END.
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + 
                  STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
                  
               END.                       
            END.            
         END.
      END.
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
      /*Berednings mtrl*/   
     
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + 
               STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0"). 
              
            END.                   
         END.           
      END.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3" THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               totbermtrl =THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerMtrlPris().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + 
               STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0"). 
            END.                   
         END.           
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      DEFINE VARIABLE minusutr AS INTEGER NO-UNDO.
      minusutr = 0. 
      IF utrustrubrik = TRUE THEN DO:
         fontstor1 = 16.
         fontstor2 = 16.
         minusutr = 2.
      END.   
      /*KOLUMNBREDD*/
      
      THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      DEFINE VARIABLE colnumvar AS INTEGER NO-UNDO.
      colnumvar = 1.
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),19 - minusutr).
      colnumvar = colnumvar  + 1.     
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),45 - minusutr).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),14 - minusutr).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),7 - minusutr).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),14 - minusutr).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),14 - minusutr).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),13 - minusutr).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),13). /*H*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),16 - minusutr).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),16 - minusutr).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),18 - minusutr).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),18 - minusutr).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),16 - minusutr).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),13). /*n*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth(THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),18 - minusutr). 
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
            
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Tot.summa:") > 0 THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Center("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Tot.summa:")),4).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Tot.summa:")),TRIM(excelvarde)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:HittaGetData("A","Tot.summa:")),"SEK").
            END.
            LEAVE.
         END.   
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
      END.              
      
      /*ANDERS
      Elpool.Helpers:GetRange(THIS-OBJECT:KalkVisaControl:VisaExcel:GetWorkSheet(),rubrikrad , "C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ):Merge( true ).
      */
      
      /*FONTER*/
     
      THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor1,? ,0,0 ).
      THIS-OBJECT:KalkVisaControl:VisaExcel:Border(rubrikrad, THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)). 
      /*Anders Olsson Elpool i Umeå AB  27 apr 2015 11:17:38 
      Inga bilagor om multivisning 
      */
      
      /*bilaga anm kod*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmUtf") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(7, kalkylqueryvar).
      END.       
         
      /*BILAGA EGNA PRISER*/
      IF vismulti = FALSE AND THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(1, kalkylqueryvar).
      END.
      /*BILAGA EGNA FAKTORER*/
     
      IF vismulti = FALSE AND THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO: 
         THIS-OBJECT:ExcelBilagor(2, kalkylqueryvar).       
      END.
      IF vismulti = TRUE THEN.   
      ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" OR 
         THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth("C1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColWidth("A1")).
      END.   
      
      /*BILAGA EGNet mtrl*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisMatSpec") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(3, kalkylqueryvar).      
      END.   
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
     Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
     */
      /*BILAGA mtrl från ber*/
     
      IF vismulti = FALSE AND THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:ExcelBilagor(4, kalkylqueryvar).      
      END.
      /*BILAGA frekvens*/
      IF vismulti = FALSE AND THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFrekvens") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(5, kalkylqueryvar).      
      END.   
      /*BILAGA kommentarer*/
      IF vismulti = FALSE AND THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisKommentar") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(6, kalkylqueryvar).      
      END.
         
      /*
      THIS-OBJECT:KalkVisaControl:VisaExcel:ScreenUpp(TRUE).
      */
      
      /*allt skrivs i arket*/
      THIS-OBJECT:KalkVisaControl:VisaExcel:valueDataOutTT().
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:SidBrytBredd(2).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ViewType(INPUT 3).
         THIS-OBJECT:KalkVisaControl:VisaExcel:AktiveraWB().
         /*behövs här för att ta bort ful ramen*/
         THIS-OBJECT:KalkVisaControl:VisaExcel:Fix().
         THIS-OBJECT:SetVisningsBild(2).
      END.   
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:SlutExcel(3).
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:ScreenUpp(TRUE).
      Guru.Konstanter:EuropeanAmericanReset().
      IF vismulti = TRUE OR kalkylisinaktiv = TRUE THEN DO:
         IF Guru.Konstanter:alltidmax = TRUE THEN THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeMax().
         ELSE THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeNormal().
         /*
         THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeMax().
         IF Guru.Konstanter:alltidmax = FALSE THEN THIS-OBJECT:Root:WindowManager:Wnd:WindowSizeNormal().
         */
      END.  
      IF vismulti = TRUE THEN THIS-OBJECT:KalkVisaControl:toolStripButtonGenerera:Visible = FALSE.  
      THIS-OBJECT:KalkVisaControl:toolStripButtonGenerera:Visible = TRUE.
   END METHOD.
  
   METHOD PRIVATE VOID ExcelHuv():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      DEFINE VARIABLE koppladbest AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Katalog").
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
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
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Tot.summa: ").
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
         Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:SaveRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFlerrad(THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaKalkNrMulti()).
         THIS-OBJECT:KalkVisaControl:VisaExcel:SetSaveRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFlerrad(THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonrMulti()).
               
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
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Tot.summa: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Kalkyltyp: ").
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Typ " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
         END.   
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      END.
   END METHOD.
   
   
   
   METHOD PRIVATE VOID ExcelAnmMulti():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOutFlerRad(THIS-OBJECT:Root:DatabaseManager:Kalkyl:VisaKalkAnmMulti()).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppMatris(vad AS INTEGER,matrisrader AS CHARACTER,kostnadstartcol AS CHARACTER):
      IF vad = 1 THEN DO:
         /*LÄGGER UT MATRIS RADEN*/
         IF VisGruppMatris(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN DO:
            /*EGET MATRIEL SUMMA*/
            
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
               THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
               THEN DO:
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
                  THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
                  IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                     THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
                  END.              
                  THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
                                
               END.
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().            
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName() + " " + STRING(iSenasteMatris)). 
            THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
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
               THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(iSenasteMatris))).                  
               END.              
               THIS-OBJECT:KalkVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:irad),THIS-OBJECT:KalkVisaControl:VisaExcel:getcolname(THIS-OBJECT:KalkVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),4,"0").
               
            END.
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().            
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName()+ " " + STRING(iSenasteMatris)).
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
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
      
      IF VisGruppera(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = FALSE THEN DO:
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING("Egna Koder")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         END.   
         ELSE DO:
            THIS-OBJECT:KalkdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:EgnaPriserTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:EgnaPriserTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad).
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:FaktorerTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:FaktorerTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE)).
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
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
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " BY MATRIS BY ENR BY LEVKOD". 
         END.
         ELSE IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " BY ENR BY LEVKOD".
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " WHERE MATRIS = " + STRING(VisMatris()) + " BY ENR BY LEVKOD".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalktmtrlTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE,THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.
            ELSE DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),?,THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.   
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE)).         
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE)).        
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).            
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
      IF vad = 4 THEN DO:
         cSenasteMtrl       = "".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Materieltillägg från beredning").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
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
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:berkalkmtrltth:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:berkalkmtrltth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).         
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:berkalkmtrltth:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE)).            
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
         THIS-OBJECT:KalkdbControl:FrekvensTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut("Frekvenskoder").
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
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
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE: 
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN.        
            ELSE DO:
               IF VisSummera(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmtAppend(INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                                 INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                                 INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
               END.
            END.   
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:FrekvensTTh:TABLE. 
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR BY FREKOD BY FREKNR ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:FrekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("FREKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("FREKNR"):BUFFER-VALUE,">>>99")).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
            THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:FrekvensTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel(THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
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
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            /*inga dubbletter*/
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN.
            ELSE IF VisSummera(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,FALSE,300,0 ).                     
               THIS-OBJECT:KalkVisaControl:VisaExcel:IhopCeller("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) + ":M" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),2).
               THIS-OBJECT:KalkVisaControl:VisaExcel:TextWrap("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               DEFINE VARIABLE komvar AS CHARACTER NO-UNDO.
               komvar = REPLACE(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE,CHR(13),Guru.Konstanter:globradbrytch).
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(komvar)).
               /* FUNKAR
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkVisaControl:VisaExcel:TextBryt(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE),10)).
               */
               /*
               THIS-OBJECT:KalkVisaControl:VisaExcel:ShrinkToFit("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),"C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), FALSE).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(THIS-OBJECT:KalkVisaControl:VisaExcel:TextBryt(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE),40)).
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
         THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,fonttyp,fontstor2,? ,0,0 ).
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad), "M"+ STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
         queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().
  
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE) NE "" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
               THIS-OBJECT:KalkVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) ,THIS-OBJECT:KalkVisaControl:VisaExcel:ColRad,"",0,FALSE,50,0 ).                     
               THIS-OBJECT:KalkVisaControl:VisaExcel:IhopCeller("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad) + ":M" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),2).
               THIS-OBJECT:KalkVisaControl:VisaExcel:TextWrap("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("A" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("B" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               THIS-OBJECT:KalkVisaControl:VisaExcel:CenterUpDown("C" + STRING(THIS-OBJECT:KalkVisaControl:VisaExcel:iRad),1).
               
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99")).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().  
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
             END.  
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
        
      END.
     
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummeraMtrl(cIN AS CHARACTER,iIN AS INTEGER, lIN AS CHARACTER):
      IF iIN = ? THEN DO:
         IF cIN = cSenasteMtrl AND lIN = lsenastLev THEN RETURN TRUE.
         ELSE DO:
            ASSIGN  
            cSenasteMtrl = cIN
            lsenastLev = lIN.         
            RETURN FALSE.
         END.
      END.
      ELSE DO:
         IF cIN = cSenasteMtrl AND iIN = iSenasteMtrlMatris AND lIN = lsenastLev THEN RETURN TRUE.
         ELSE DO:
            ASSIGN  
            cSenasteMtrl       = cIN
            iSenasteMtrlMatris = iIN
            lsenastLev = lIN. 
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
      DEFINE VARIABLE extrakol AS LOGICAL NO-UNDO.
      DEFINE VARIABLE bnum AS CHARACTER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE hjacol AS CHARACTER NO-UNDO.
      DEFINE VARIABLE akod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lkod AS INTEGER NO-UNDO.
      DEFINE VARIABLE blankrad AS INTEGER NO-UNDO.
      
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.         
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:ReadExcelStart(INPUT filnamn).
      extrakol = FALSE.
      DEBUGGER:SET-BREAK().    
      IF vad = 4 THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
         blankrad = 0.
         flik1 = 1.         
         /*infra schaktkalkyl imp*/
         THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 6.
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         
         DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:KalkVisaControl:VisaExcel:iRadslut:
            invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
            IF invar = ? OR invar = "" THEN DO:               
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:                                 
                  
                  IF blankrad > 2 THEN LEAVE.
                  blankrad = blankrad + 1.
               END.               
            END.   
            ELSE IF invar NE "" THEN DO:
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:                  
                  bnum = invar. 
                  IF invar = ? OR invar = "" THEN DO:                     
                     IF blankrad > 4 THEN LEAVE.
                     blankrad = blankrad + 1.
                  END.                                         
               END.               
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D" THEN DO:             
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  /*THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = INTEGER(bnum) NO-ERROR.*/
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.                  
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "F" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "G" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "H" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "I" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "J" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "K" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "L" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "M" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "N" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "O" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "P" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "Q" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "R" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "S" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "T" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "U" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "V" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "W" THEN DO:                                                       
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
                  
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "X" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "Y" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "Z" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AA" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AB" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AC" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AD" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AE" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AF" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AM" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AN" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AO" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AP" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AQ" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AR" THEN DO:
                                                      
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AS" THEN DO:
                                                      
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AT" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AU" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AV" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AW" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AX" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AY" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "AZ" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
               ELSE IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "BA" THEN DO:                                     
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE = bnum NO-ERROR.                        
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("HJCOL"):BUFFER-VALUE = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname NO-ERROR.
               END.
            END.
                                                
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().                        
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "BB" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A". 
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
               
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:iRad > 41 THEN DO:
                  flik1 = flik1 + 1.
                  IF flik1 = 2 THEN DO:
                     DEBUGGER:SET-BREAK().
                  END.   
                  IF flik1 < 8 THEN DO:
                     THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1).
                     THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 6.
                     THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
                     THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
                  END.    
               END.   
            END.                        
               
         END.
         flik1 = 1.         
         
         THIS-OBJECT:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 5.
         THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D".
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
         DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 5:
            invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
            IF invar = ? THEN.
            ELSE IF invar BEGINS "bygg" THEN.
            ELSE IF invar BEGINS "från" THEN.
            ELSE IF invar NE "" THEN DO:            
               ASSIGN 
               hjacol = THIS-OBJECT:KalkVisaControl:VisaExcel:cColname
               akod = SUBSTRING(invar,1,3)
               lkod = INTEGER(SUBSTRING(invar,4,3)).
                
               
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:TABLE + " WHERE HJCOL = '" + hjacol + "' USE-INDEX HJCOL".               
               qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkylimportTTh,queryvar).
               qH:GET-FIRST().
               DO WHILE qH:QUERY-OFF-END = FALSE:                          
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = akod.
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = lkod.   
                  queryvar2 = " WHERE ID2 = " + QUOTER(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE) NO-ERROR.                                 
                  THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST(queryvar2) NO-ERROR.    
                  IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:                  
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.                     
                  END.
                  /*MESSAGE hjacol THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE
               VIEW-AS ALERT-BOX.*/                                  
                  qH:GET-NEXT().
               END.
            END.   
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "BB" THEN DO:
               DEBUGGER:SET-BREAK().
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "D". 
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.   
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:SlutReadExcel().
         Guru.Konstanter:EuropeanAmericanReset().
         THIS-OBJECT:ImportListasch().                 
         THIS-OBJECT:FelMeddCheck().  
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).                     
      END.     
      ELSE DO:
         DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:KalkVisaControl:VisaExcel:iRadslut:
            invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
            IF invar = ? THEN.
            ELSE IF invar NE "" THEN DO:
               IF vad = 3 THEN DO:
                  IF invar = "Matris" THEN extrakol = TRUE.
                  IF extrakol = TRUE THEN DO:
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
                     END.   
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:                                      
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
                     END.
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
                     END.    
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
                        invar = REPLACE(invar,",",".").
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                     END.
                  END.
                  ELSE DO:   
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().                        
                        /*importera beroende på om du har klickat i konstruktion eller schakt grid    Lena 20200528*/
                        IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum > 0 THEN DO:
                           THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum.
                        END.
                        ELSE IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid > 0  THEN DO:
                           THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid.
                        END.   
                        /* MATRIS ANVÄNDS INTE I BEREDNINGSKALKYL BERNUM OCH SID /Lena 20200528
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.*/                  
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
                     END.
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
                     END.    
                     IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                        invar = REPLACE(invar,",",".").
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                     END.
                  END.
               END.      
               ELSE DO:          
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
                     IF vad = 2 THEN DO:
                        klangd = LENGTH(invar).
                      
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
                     END.
                     ELSE DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
                     END.
                  END.
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" AND vad = 1 THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
                  END.    
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                     invar = REPLACE(invar,",",".").
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  END.
               END.                     
            END.
            THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().                        
            IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "F" THEN DO:
               THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A". 
               THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
            END.                        
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:SlutReadExcel().
         Guru.Konstanter:EuropeanAmericanReset().
         THIS-OBJECT:ImportLista().
         THIS-OBJECT:FelMeddCheck().
      END.     
      
   END METHOD.
   
   METHOD PUBLIC VOID ImportMallar():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.  
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkmallValdtth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkmallValdtth,queryvar).
      qH:GET-FIRST().
      IF THIS-OBJECT:KalkdbControl:KalkmallValdtth:AVAILABLE THEN.
      ELSE RETURN.
      DO WHILE qH:QUERY-OFF-END = FALSE:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkmallKodertth:TABLE + " WHERE KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkdbControl:KalkmallValdtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE).     
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkmallKodertth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
            THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KalkmallKodertth).
            THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.  
                      
            subqH:GET-NEXT(NO-LOCK).  
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:KalkdbControl:KalkmallValdtth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:KalkImpMallControl:GridValdaMallar:GuruReopen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruAvmarkera().
     
     
   END METHOD.
   
   
   METHOD PUBLIC VOID ImportLista():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.      
      /*EGEN ?*/
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkylimportTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         
         THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkdbControl:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE > 0 THEN THIS-OBJECT:CurrentMatris = THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            ELSE DO:
               THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 1.
               THIS-OBJECT:CurrentMatris = -1.
            END.                             
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-CREATE().      
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KalkylimportTTh).
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:LopposterTTh).
            lastKalknumRowid = THIS-OBJECT:KalkdbControl:KoderTTh:ROWID.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKodRow(INPUT THIS-OBJECT:KalkdbControl:KoderTTh:RECID, INPUT FALSE).               
         END.   
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(64)).         
         END. 
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-DELETE() NO-ERROR.  
         qH:GET-NEXT().         
      END.
       
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ImportSpara().
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RaknaEgen().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ImportSpara().      
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:RefreshArbKoder(?,?).
       
   END METHOD.
   
   METHOD PUBLIC VOID ImportListaSch():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
      DEFINE VARIABLE numok AS LOGICAL NO-UNDO.
      /*EGEN ?*/
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkylimportTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         
         THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KalkdbControl:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE > 0 THEN THIS-OBJECT:CurrentMatris = THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            ELSE DO:
               THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 1.
               THIS-OBJECT:CurrentMatris = -1.
            END.
            IF THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "KM" THEN DO:
               THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE / 1000.                
            END.   
            numok = FALSE .            
            /*vid import av kalkylkoder på bernum. Skapa ej om bernum ej finns lena 20210517*/            
            IF INTEGER(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE) > 0 THEN DO:                            
               THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST(" WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE) , NO-LOCK) NO-ERROR.    
               IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:                  
                  /*Skapa ej om arbkod lopnr redan finns på bernum  lena 20210517*/
                  queryvar2 = " WHERE BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE + " AND ARBKOD = '" + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)).                                  
                  THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST(queryvar2 , NO-LOCK) NO-ERROR.
                  IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
                     numok = FALSE.
                  END.
                  ELSE numok = TRUE.                     
               END.               
            END.
            ELSE DO:
               /*byggnr finns inte i beredning*/
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE + " " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(243)).
            END.
    
            IF numok = TRUE THEN DO:                 
               THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-CREATE().      
               THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KalkylimportTTh).
               THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:LopposterTTh).
               lastKalknumRowid = THIS-OBJECT:KalkdbControl:KoderTTh:ROWID.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKodRow(INPUT THIS-OBJECT:KalkdbControl:KoderTTh:RECID, INPUT FALSE).
            END.
            ELSE DO:
               /*om byggnr inte finns i beredning redan lagt ut fel*/
               IF INTEGER(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE) = 0 THEN.               
               ELSE DO:
                  /*dublett kalkylkod på byggnr*/
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ID2"):BUFFER-VALUE + " " + THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(242)).
               END.             
            END.   
         END.   
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(64)).         
         END. 
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-DELETE() NO-ERROR.  
         qH:GET-NEXT().         
      END.
       
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ImportSpara().
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RaknaEgen().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ImportSpara().
      /*
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkSpara(). 
      */
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:KalkControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:RefreshArbKoder(?,?).
       
   END METHOD.
   METHOD PUBLIC VOID GenereraAvtalsKalkyl():
      /*THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel1Collapsed = FALSE.
      THIS-OBJECT:KalkVisaControl:splitArbExcelKalk:Panel2Collapsed = TRUE.
      */
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE resid AS INTEGER                           NO-UNDO.
      DEFINE VARIABLE res   AS System.Windows.Forms.DialogResult NO-UNDO. 
    
      THIS-OBJECT:KalkdbControl:Avtalskalktth:FIND-FIRST("WHERE ID = "  + STRING(THIS-OBJECT:KalkAvtalControl:ComboAvtal:VALUE)) NO-ERROR.
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(47) + " " +
            THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXT"):BUFFER-VALUE  +  " Katalog år " + THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("KATAR"):BUFFER-VALUE + 
            " Avtals år " + THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALAR"):BUFFER-VALUE, "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Information).
      IF res:ToString()  EQ "NO" THEN RETURN.
      OS-DELETE VALUE(ExelKommando) NO-ERROR. 
      THIS-OBJECT:KalkdbControl:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = TRUE.
      IF Guru.Konstanter:globforetag = "GRAN" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.
      /*
      THIS-OBJECT:KalkVisaControl:toolStripButtonExporteraExcel:Visible = FALSE.
      THIS-OBJECT:KalkVisaControl:toolStripButtonSkrivUt:Visible = FALSE.
      THIS-OBJECT:KalkVisaControl:toolStripButtonTillExcel:Visible = FALSE.
      THIS-OBJECT:SetVisningsBild(1).
      */
     /* IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, TRUE).      
      END.*/   
    
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:Root:DatabaseManager:Kalkyl:AvtalkalkaoBerSch(INPUT THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, FALSE).
      ELSE THIS-OBJECT:Root:DatabaseManager:Kalkyl:Avtalkalkao(INPUT THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("ID"):BUFFER-VALUE, FALSE).
      THIS-OBJECT:HamtaAvtalsKalkyl(OUTPUT resid, OUTPUT ExelKommando).
      IF resid = ? THEN RETURN.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "SVEA"  THEN DO:
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().
         ELSE IF ExelKommando MATCHES "*Ellevio*" THEN  THIS-OBJECT:KalkAvtalControl:VattenAvtalEllevio().
         ELSE IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:            
            IF ExelKommando MATCHES "*summerad*" THEN THIS-OBJECT:KalkAvtalControl:Infraberavtalsum(ExelKommando).
            ELSE THIS-OBJECT:KalkAvtalControl:Infraberavtal(ExelKommando).               
         END.   
         ELSE THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).    
      END.  
                
      ELSE IF Guru.Konstanter:globforetag = "FORS" THEN DO:  
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().     
         ELSE IF ExelKommando MATCHES "*Ellevio*" THEN  THIS-OBJECT:KalkAvtalControl:VattenAvtalEllevio().    
         ELSE IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            IF ExelKommando MATCHES "*Schaktprotokoll*" THEN THIS-OBJECT:KalkAvtalControl:Infraschakt(ExelKommando).  
            ELSE IF ExelKommando MATCHES "*summerad*" THEN THIS-OBJECT:KalkAvtalControl:Infraberavtalsum(ExelKommando).
            ELSE THIS-OBJECT:KalkAvtalControl:Infraberavtal(ExelKommando).               
         END.
         ELSE THIS-OBJECT:KalkAvtalControl:InfraAvtal(ExelKommando).       
      END.               
      
      ELSE IF Guru.Konstanter:varforetypval[63] = 1 THEN DO:
      /* lägg in de som ej har specialavtalsmallar här Lena 20220425
      ELSE IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "OPPU" OR 
      Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "ENKA" OR Guru.Konstanter:globforetag = "KEKR" OR
      Guru.Konstanter:globforetag = "WSP" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:*/                                                                  
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().
         ELSE  IF ExelKommando MATCHES "*Ellevio*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalEllevio().
         ELSE IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:            
            IF ExelKommando MATCHES "*EON*" AND ExelKommando MATCHES "*summerad*" THEN THIS-OBJECT:KalkAvtalControl:Infraberavtalsum(ExelKommando).
            ELSE IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:Infraberavtal(ExelKommando).
            ELSE IF ExelKommando MATCHES "*Schaktprotokoll*" THEN THIS-OBJECT:KalkAvtalControl:Infraschakt(ExelKommando).               
         END.
         ELSE IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).
      END.
      ELSE IF Guru.Konstanter:globforetag = "KRIN" THEN DO:    
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            IF ExelKommando MATCHES "*mängd*" THEN THIS-OBJECT:KalkAvtalControl:Infraberavtal(ExelKommando).               
         END.
         ELSE THIS-OBJECT:KalkAvtalControl:KraftringenAvtal(INPUT ExelKommando).     
      END.
      /*ELSE IF Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "OPPU" THEN DO:                           
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:            
            THIS-OBJECT:KalkAvtalControl:Infraberavtal(ExelKommando).               
         END.
         ELSE IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).
               
      END.
      
      ELSE IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "ENKA" OR Guru.Konstanter:globforetag = "KEKR" THEN DO:      
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().               
      END.   
      ELSE IF Guru.Konstanter:globforetag = "WSP" THEN DO:  
         IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).    
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().               
      END.
      ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:  
         IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).    
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().               
      END.*/
      ELSE IF Guru.Konstanter:globforetag = "LIMO" THEN DO:        
         THIS-OBJECT:KalkAvtalControl:LimoAvtal().       
      END.   
      ELSE IF Guru.Konstanter:globforetag = "Sweo" THEN DO:        
         THIS-OBJECT:KalkAvtalControl:SwecoAvtal().     
      END. 
      ELSE IF Guru.Konstanter:globforetag = "Lapp" THEN DO:        
         THIS-OBJECT:KalkAvtalControl:LappAvtal().     
      END.
      
      ELSE IF Guru.Konstanter:globforetag = "WIGA" THEN DO:
         THIS-OBJECT:KalkAvtalControl:WigaAvtal().
      END.       
      ELSE IF Guru.Konstanter:globforetag = "AKEA" THEN DO:  
         IF ExelKommando MATCHES "*Specifikation f*" THEN THIS-OBJECT:KalkAvtalControl:AkeaFaktura().
         IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando).                           
      END.
      ELSE IF Guru.Konstanter:globforetag = "C4EN" THEN DO:
         THIS-OBJECT:KalkAvtalControl:C4Avtal(ExelKommando).                                     
      END. 
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
         IF ExelKommando MATCHES "*eon*" THEN THIS-OBJECT:KalkAvtalControl:EonesAvtal(ExelKommando). 
         IF ExelKommando MATCHES "*Infratek*" THEN THIS-OBJECT:KalkAvtalControl:InfraAvtal(ExelKommando).
         IF ExelKommando MATCHES "*linjemontage*" THEN THIS-OBJECT:KalkAvtalControl:LimoAvtal().  
         IF ExelKommando MATCHES "*SWECO*" THEN THIS-OBJECT:KalkAvtalControl:SwecoAvtal().  
         IF ExelKommando MATCHES "*Lapp*" THEN THIS-OBJECT:KalkAvtalControl:LappAvtal().
         IF ExelKommando MATCHES "*wise*" THEN THIS-OBJECT:KalkAvtalControl:WigaAvtal().
         IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:KalkAvtalControl:VattenAvtalVatt().
         IF ExelKommando MATCHES "*Kraftringen*" THEN THIS-OBJECT:KalkAvtalControl:KraftringenAvtal(INPUT ExelKommando).
          
      END.
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:FelMeddCheck().
      
   END METHOD.
   
   
   
   
   
   
   METHOD PRIVATE VOID  HamtaAvtalsKalkyl(OUTPUT resid AS INTEGER,OUTPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE blobproch AS HANDLE                            NO-UNDO.
      DEFINE VARIABLE kommando  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.   
      RUN DYNBLOBB.P PERSISTENT SET blobproch. 
      RUN blobfilC_UI IN blobproch (INPUT THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE, OUTPUT resid,OUTPUT ExelKommandout).
      IF resid = ? THEN DO:                 
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(48) + " ", "Avtalskalkyl", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).           
         RUN deleteproc_UI IN blobproch.
         IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
         
         RETURN.    
      END.
      
      kommando = ExcelTemplate(INPUT ExelKommandout).
      OS-DELETE VALUE(ExelKommandout) NO-ERROR.
      ExelKommandout = kommando.
      RUN deleteproc_UI IN blobproch.
      IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
      
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop EQ TRUE THEN DO:
      
      IF THIS-OBJECT:KalkVisaControl:VisaExcel:isInited EQ TRUE THEN THIS-OBJECT:KalkVisaControl:VisaExcel:Reload(ExelKommandout).
      ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:InteropInitialize(ExelKommandout). 
      END.
      ELSE DO:
         /*autofit förstör denna mall Lena 20160512*/
         IF ExelKommandout MATCHES "*Vattenfall P2*" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcelejautofit(ExelKommandout).
         ELSE IF ExelKommandout MATCHES "*EON*" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcelejautofit(ExelKommandout).
         ELSE IF ExelKommandout MATCHES  "*Referensmall*" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcelejautofit(ExelKommandout).
         ELSE IF ExelKommandout MATCHES  "*Ellevio*" THEN THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcelejautofit(ExelKommandout). 
         ELSE THIS-OBJECT:KalkVisaControl:VisaExcel:OpenExcel(ExelKommandout).
      END.
   END METHOD.
   
   METHOD PUBLIC CHARACTER ExcelTemplate(INPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE excelversend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
      kommando = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      IF SUBSTRING(ExelKommandout, LENGTH(ExelKommandout),1) = "x" THEN excelversend = "x".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando) NO-ERROR.
      
      /*IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 6 THEN DO:*/
      IF THIS-OBJECT:TabManager:SelectedTab = ? THEN DO:
         IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
            kommando = kommando + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
         ELSE DO:
            kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
         /*+ excelversend låg för alla men då blev det xlsxx för dem som hämtades från blob  Lena 20170505*/
         kommando = TRIM(kommando + excelversend).
      END. 
      ELSE IF THIS-OBJECT:TabManager:SelectedTab:KEY EQ "Avtalskalkyl" THEN DO:
         IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
            kommando = kommando + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
         END.
         ELSE DO:
            kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.      
         END.
      END.   
      ELSE DO:
         IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
            kommando = kommando + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
         ELSE DO:
            kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
         END.
         /*+ excelversend låg för alla men då blev det xlsxx för dem som hämtades från blob  Lena 20170505*/
         kommando = TRIM(kommando + excelversend).
      END. 
            
      
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      RETURN kommando.
   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
              
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:KalkdbControl:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:tiduth:AVAILABLE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(183),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
         Guru.GlobalaVariabler:DoNotOpenWin = FALSE.
      END.
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
      THIS-OBJECT:KalkEgnaControl:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
   END METHOD.
    
END CLASS.
