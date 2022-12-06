 
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


CLASS Modules.Arende.ArendeShell INHERITS UserControl: 
   {ARENDEKATH.i}
   {ARENDEEXTRATAB.I}
   DEFINE PUBLIC VARIABLE ArendeJoinControl                 AS Modules.Arende.ArendeJoin                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeHuvudControl                 AS Modules.Arende.ArendeHuvudControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeControl                   AS Modules.Arende.ArendeControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeEgnaControl               AS Modules.Arende.ArendeEgnaControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeMtrlControl                AS Modules.Arende.ArendeMtrlControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeImpMallControl                AS Modules.Arende.ArendeImpMallControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeTidlControl             AS Modules.Arende.ArendeTidlControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeFelmeddControl             AS Modules.Arende.ArendeFelmeddControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeVisaControl             AS Modules.Arende.ArendeVisaControl                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE ArendeStatusControl             AS Modules.Arende.ArendeStatusControl                                   NO-UNDO.
   
   DEFINE PUBLIC VARIABLE CellChvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE combolista  AS CHARACTER NO-UNDO EXTENT 10.
   DEFINE PRIVATE VARIABLE imageList1                   AS System.Windows.Forms.ImageList                                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE CurrentMatris                AS INTEGER                                                        NO-UNDO.
      
   DEFINE PRIVATE VARIABLE lastKalknumRowid             AS ROWID  NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelmedd                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabJoin AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabTidlage AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabStatus AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
  
  
   
   
   DEFINE PUBLIC VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   /* Drag & Dropp*/
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   
   
   
   
   DEFINE PRIVATE VARIABLE TabEgna                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImportMallar              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkyl                    AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylHuvud               AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
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
   DEFINE         VARIABLE RibbonToolOmrade             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolKat                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolMatris             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnm           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAngeKodAntal       AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /* visning */
   
   DEFINE PUBLIC  VARIABLE RibbonToolGenerera           AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUt            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUtBildval     AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisHuvud           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisAnmarkning      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisUtforlig        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisVisa            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisSummera         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGruppera        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisPriser          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatSpec         AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisMatris          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisGroupMatris     AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   
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
   DEFINE         VARIABLE cSenasteArbKod               AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteLopNr                AS INTEGER                                                        NO-UNDO.
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
      

   CONSTRUCTOR PUBLIC ArendeShell ( INPUT r AS Guru.Root ):
      SUPER ().
     
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar Arendeobjek..").
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Arende:InitializeTTs().
      THIS-OBJECT:Root:Logger:WriteLine("Arende: Gör kopplingar till databasemanager..").
      {ARENDEKATDBH.i Root:DatabaseManager:Arende}
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC ArendeShell ( ):
      /*
      DELETE OBJECT THIS-OBJECT:toolStripVisa.
      THIS-OBJECT:toolStripVisa = ?.   
      DELETE OBJECT THIS-OBJECT:TabManager.
      THIS-OBJECT:TabManager = ?.
      DELETE OBJECT THIS-OBJECT:ArendeVisaControl:VisaExcel.
      THIS-OBJECT:ArendeVisaControl:VisaExcel = ?.
      */
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
   METHOD PUBLIC LOGICAL NewArende():
      /* skapa kalkyl i databas */
      
      IF THIS-OBJECT:Root:DatabaseManager:Arende:CreateArende() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      /* Hitta rätt katalog*/
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
      
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      /*v11.2 fix*/
           
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:SetIniComboV().
      THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
      THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruCombo:VALUE = 1.
     
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
      THIS-OBJECT:LaddaLabels().
      THIS-OBJECT:VisaTabs("ALLA", FALSE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY, TRUE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL NewArende(INPUT kalkdatah AS HANDLE):
      /* skapa kalkyl i databas */
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
      IF THIS-OBJECT:Root:DatabaseManager:Arende:CreateArende() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      /* Hitta rätt katalog*/
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      /*v11.2 fix*/
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:SetIniComboV().
      
      THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
      THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruCombo:VALUE = 1.
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
     
      THIS-OBJECT:LaddaLabels().
      kalkdatah:FIND-FIRST() NO-ERROR.
      IF kalkdatah:AVAILABLE THEN DO:
         IF kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = 0 OR kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = ? THEN. 
         ELSE THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE = kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE.
         THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:Text  = kalkdatah:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         /*v11.2 fix*/
         
         THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         
         Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
         IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
            THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("VIBESTID"):BUFFER-VALUE.
         END.
         ELSE DO:
            IF STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR STRING(kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = ? THEN THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE. 
            ELSE THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = kalkdatah:BUFFER-FIELD("BESTID"):BUFFER-VALUE.      
         END.
         THIS-OBJECT:Root:DatabaseManager:Arende:KopplatAoArende(INPUT kalkdatah:BUFFER-FIELD("AONR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("DELNR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("PLANNR"):BUFFER-VALUE,INPUT kalkdatah:BUFFER-FIELD("ARTAL"):BUFFER-VALUE).                    
      END.      
      THIS-OBJECT:VisaTabs("ALLA", FALSE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY, TRUE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
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
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruText:Text).
      /*
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE    =      STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE).
      */
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE       =      STRING(THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruCombo:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE     =     STRING(THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE).
         
      IF THIS-OBJECT:ArendeHuvudControl:UtfardareBort:VISIBLE = FALSE THEN THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:ArendeHuvudControl:HuvudAnmarkning:Text).
      DEFINE VARIABLE ErrorText AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ErrorI AS INTEGER NO-UNDO.
      DEFINE VARIABLE IsError   AS LOGICAL   NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Arende:SaveHuvud(OUTPUT ErrorText, OUTPUT IsError).
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
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:VISIBLE       = TRUE.
      IF THIS-OBJECT:LoadKalkyl(INTEGER(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)),STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE), TRUE) = FALSE THEN RETURN FALSE.
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
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeStatusHmt(kalkdatah).
         THIS-OBJECT:arendetidlageTTh = THIS-OBJECT:Root:DatabaseManager:Arende:arendetidlageTTh.
         THIS-OBJECT:CreateGridStatus().
         THIS-OBJECT:ArendeStatusControl:GridStatus:GuruReopen().
         RETURN TRUE.
      END.
      ELSE RETURN FALSE.
           
   END METHOD.
   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT ArendeimportTTinh AS HANDLE,INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      THIS-OBJECT:ArendeimportTTh = ArendeimportTTinh.
      THIS-OBJECT:LoadKalkyl(INPUT nr, INPUT omr, INPUT nydef).
      THIS-OBJECT:ArendeimportTTh:FIND-FIRST() NO-ERROR. 
      /*KOPIERAR KALKYL FRÅN BEREDNING*/
      IF THIS-OBJECT:ArendeimportTTh:AVAILABLE THEN DO:                   
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
      IF THIS-OBJECT:Root:DatabaseManager:Arende:FetchArende(nr, omr) = FALSE THEN RETURN FALSE.
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
      IF nydef = FALSE THEN DO:
          /*SÄTTER DEFAULT VÄRDE VID OM INGA DEFULTER FINNS MEN KALK ÄR REDAN SKAPADNYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         THIS-OBJECT:GuruDefaultsTTh:FIND-FIRST() NO-ERROR. 
         IF THIS-OBJECT:GuruDefaultsTTh:AVAILABLE THEN.
         ELSE DO:
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 1 OR THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 2 OR 
            THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 3 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE - 1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 5 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(3), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(1), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
            IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 7 THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
               "KalkylRibbonVisaTyp",STRING(4), "Koder", "Infragistics.Win.UltraWinToolbars.ComboBoxTool",TRUE, OUTPUT defToolValue).
            END.
         END.   
      END.   
      THIS-OBJECT:arendeaonrTTh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:arendeaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE = ? THEN DO:
         ASSIGN 
          Guru.GlobalaVariabler:plusaonr = ?
         Guru.GlobalaVariabler:plusdnr  = ?.
      END.
      ELSE DO:
         ASSIGN 
          Guru.GlobalaVariabler:plusaonr = THIS-OBJECT:arendeaonrTTh:BUFFER-FIELD("AONR"):BUFFER-VALUE
         Guru.GlobalaVariabler:plusdnr  = THIS-OBJECT:arendeaonrTTh:BUFFER-FIELD("DELNR"):BUFFER-VALUE.
      END.   
      
      
      /* Spärra "skapany"-mode */
      ASSIGN
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:ENABLED = FALSE
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:ENABLED  = FALSE
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:ENABLED     = FALSE.
      
   /* THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:ReadOnly = TRUE funkar inte 
   VVVVSätt alla ändringar på grids, Combos osv som behövs efter laddning*/
      
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      IF nydef = TRUE THEN DO:
          /*SÄTTER DEFAULT VÄRDE VID NYKALK BARA FÖR ATT INTE HELPERS SKA SKRIVA ÖVER*/
         IF STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) = "7" THEN DO:
            THIS-OBJECT:MarkningTTh:FIND-FIRST("WHERE markfiltertt.TYP = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE) NO-ERROR.
            THIS-OBJECT:RibbonToolFilterTyp:Text = THIS-OBJECT:MarkningTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
            THIS-OBJECT:Root:DatabaseManager:Global:CreateActualDefaultTT(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar,
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
      THIS-OBJECT:FrekvensKomChanged(?,?).
      THIS-OBJECT:KoderAnmChanged(?,?). 
      THIS-OBJECT:ValueArbetsKoderCh(?,?).
      THIS-OBJECT:FriInSet().
   END METHOD.
   
    METHOD PUBLIC VOID FriInEventSub():
      THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Enter:Subscribe(THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Fokus).
      THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:KeyDown:Subscribe(THIS-OBJECT:fillinFriInKodUp).           
      THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Fylld:Subscribe(THIS-OBJECT:fillinFriInKodFylld).         
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Fylld:Subscribe(THIS-OBJECT:fillinFriInLopFylld).         
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:KeyDown:Subscribe(THIS-OBJECT:fillinFriInLopUp).           
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Enter:Subscribe(THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Fokus).
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Enter:Subscribe(THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Fokus).
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KeyPress:Subscribe(THIS-OBJECT:fillinFriInAntalPress). 
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KeyUp:Subscribe(THIS-OBJECT:fillinFriInAntalUp).             
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Pil:Subscribe(THIS-OBJECT:fillinFriInAntalPil).    
   END METHOD.
   
   /*Robin Sjöberg Elpool i Umeå AB  2 apr 2014 11:11:21 
      Events för logik vid TGT-inmatning nedan 
   */
   
   METHOD PUBLIC VOID fillinFriInKodFylld():
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Select().
   END METHOD.
   
   METHOD PUBLIC VOID fillinFriInLopFylld():
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Select().
   END METHOD.
   
   METHOD PUBLIC VOID fillinFriInAntalPil():
        IF THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KnappKontroll = "Up" THEN DO:
            THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Focus().
            THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Select().
            THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:SelectAll().
            RETURN.
         END.
         IF THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KnappKontroll = "Down" THEN DO:
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Focus().
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Select().
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:SelectAll().
            RETURN.
         END.
   END METHOD.
   
   /*Styr tabbeteende för antal*/
   METHOD PUBLIC VOID fillinFriInAntalPress(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyPressEventArgs):
         IF THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KnappKontroll = "TAB" THEN DO:
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Focus().
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Select().
            THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:SelectAll().
            e:Handled = TRUE.
            RETURN.
         END.
   END METHOD.
   
   /*Styr tabbeteende för antal*/
   METHOD PUBLIC VOID FriInKod():
         ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         ArendeimportTTh:BUFFER-CREATE().
         THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Text) NO-ERROR.
         THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Text) NO-ERROR.
         THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Text) NO-ERROR.
         THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
         THIS-OBJECT:ImportLista().
         ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
                  /*problem ccc
         THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruRepositionTo(lastKalknumRowid).
         */
   END METHOD.
   
   /*Styr enterbeteende för antal*/
   METHOD PUBLIC VOID fillinFriInAntalUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      IF THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KnappKontroll = "Return" THEN DO: 
         e:Handled = TRUE.
         THIS-OBJECT:FriInKod().
         THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Focus().
         THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:KnappKontroll = "".
         RETURN.
      END.  
   END METHOD.
   
   /*Styr enterbeteende för kod*/
   METHOD PUBLIC VOID fillinFriInKodUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      
      keykodestring = e:KEYCODE:ToString().

      IF keykodestring = "Return" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Select().   
      END.
      IF keykodestring = "Up" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Select().   
      END.   
      IF keykodestring = "Down" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:Select().   
      END.       
   END METHOD.
   
   /*Styr enterbeteende för lop*/
   METHOD PUBLIC VOID fillinFriInLopUp(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs):
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      keykodestring = e:KEYCODE:ToString().
      IF keykodestring = "Return" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Select().
      END.
      IF keykodestring = "Up" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Select().   
      END.   
      IF keykodestring = "Down" THEN DO:
         THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruText:Select().   
      END.       
   END METHOD.
   
      
   
   METHOD PUBLIC VOID FriInSet():
      IF THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Text BEGINS "G" THEN THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:Text = "".
      IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Ej märkta" THEN  THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:ValjMask("AAA").
      ELSE IF TRIM(THIS-OBJECT:GetVisaMarkning()) = "Region" THEN  THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:ValjMask("AA").
      ELSE THIS-OBJECT:ArendeControl:fillinFriInKod:GuruText:ValjMask("AAA").
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruText:ValjMask("##").
   END METHOD.
   
   METHOD PUBLIC LOGICAL VisaArendeLoad(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      /* Ladda från databas + DEFAULT*/
      IF THIS-OBJECT:Root:DatabaseManager:Arende:FetchVisaArende(extravaldfastth) = FALSE THEN RETURN FALSE.
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
           
      THIS-OBJECT:KalkylLoaded = TRUE.
      
      /*VALUE PÅ DEFAULTVÄRDEN FÅR EFFEKT*/
      THIS-OBJECT:StartDefaulMT().
      THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)).
      THIS-OBJECT:ApplyLoadedVisaKalkyl().
      
      THIS-OBJECT:VisaTabs("Alla", FALSE).
      THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = TRUE.
      
      THIS-OBJECT:VisningKalkIniMulti().
      RETURN TRUE.
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
      THIS-OBJECT:ArendeTidlControl:GridTidlage:GuruReopen().
      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
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
      
      THIS-OBJECT:ArendeHuvudControl:UtfardareBort:VISIBLE     = FALSE.
   END METHOD.
   /* Fullständiggör formen när kalkyl laddats*/
   METHOD PUBLIC VOID ApplyLoadedKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonTabKalkyl:Caption = Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      /*
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:GuruFiltrera( "kalkylprisertt.KLOGSUBID = " + STRING( THIS-OBJECT:KatalogTTh:BUFFER-FIELD("HKLOGSUBID"):BUFFER-VALUE ) + " AND kalkylprisertt.EGENKODUPP = TRUE").      
     */
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:GuruFiltrera("kalkylprisertt.EGENKODUPP = TRUE").
      /* Sätter Combos */ 
      /*GÖR ÄVEN REF PÅ ARBKODER*/
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 6 THEN THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE = 2.
      ELSE THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE.
      THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruCombo:VALUE  = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE.
      
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems[THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE - 1].
     */
      /*v11.2 fix*/
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      IF STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) = "" OR  STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) =  ? THEN THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      ELSE DO:
         Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE) + "'") NO-ERROR.
         IF Guru.Konstanter:BestKundTTH:AVAILABLE THEN DO:
            THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = Guru.Konstanter:BestKundTTH:BUFFER-FIELD("VIBESTID"):BUFFER-VALUE.
         END.
         ELSE THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE = HuvudTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE.  
      END.
      
      THIS-OBJECT:SetIniComboV(). 
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:GuruFiltrera().
      
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
        
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'")  NO-ERROR.
      IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
         THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      END.
      ELSE DO:
         THIS-OBJECT:ArendeHuvudControl:UtfardareBort:VISIBLE     = TRUE.
         THIS-OBJECT:ArendeHuvudControl:UtfardareBort:VALUE = "Borttagen person " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
         
      END.  
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:Text.
      /* Hitta kalkylansvarig*/
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
      IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
         THIS-OBJECT:ArendeHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).              
      END.
      
      /* Filtrera arbetskoder efter kalkyltyp */
      THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFiltrera("kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
      /* Hitta första rows i grejer - behövs detta verkligen ? */
      /* Döp om matris rätt */
      matrisname = THIS-OBJECT:GetMatrisName().
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      /* Sätter värden */
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
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
      THIS-OBJECT:ArendeTidlControl:GridTidlage:GuruReopen().
      
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
         kalkylisinaktiv = TRUE.
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.         
      END.    
      ELSE DO:
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
         
         
      END.
      
      
     
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
   
   
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
      THIS-OBJECT:ArendeHuvudControl:LabelBest:TEXT                  = SUBSTRING(Guru.Konstanter:gbestl,1,10)
      THIS-OBJECT:ArendeHuvudControl:LabelOmr:TEXT                  = Guru.Konstanter:gomrk
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruLabel:Text  = "Benämning"
      THIS-OBJECT:ArendeHuvudControl:HuvudBenamning:GuruText:Text   = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
      THIS-OBJECT:ArendeHuvudControl:HuvudAnmarkning:Text           = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE
      /*
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruLabel:Text     = "Kalkylnr."
      */
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruLabel:Text     = "nr"
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruLabel:Text     = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruLabel:Text,TRUE)
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruText:Text      = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)
      THIS-OBJECT:ArendeHuvudControl:HuvudKalknr:GuruText:ReadOnly   = TRUE           
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruLabel:Text        = "Kalkyltyp"
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruLabel:Visible = FALSE
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:Visible = FALSE
      THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruLabel:Text = "Välj priser för uppföljning"
      THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruLabel:Text   = "Ansvarig"
      THIS-OBJECT:ArendeControl:fillinFriInKod:GuruLabel:Text   = "Arbkod:"
      THIS-OBJECT:ArendeControl:fillinFriInLop:GuruLabel:Text   = "Löpnr:"
      THIS-OBJECT:ArendeControl:fillinFriInAntal:GuruLabel:Text   = "Antal:".
      
      IF Guru.Konstanter:PersonalTTh:AVAILABLE = FALSE  THEN DO:
         THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruText:Text  = THIS-OBJECT:Root:DatabaseManager:Arende:BortTagenPersonal(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE)).      
         Guru.Konstanter:PersonalTTh:FIND-FIRST() NO-ERROR.
      END.   
      ELSE DO: 
         
         THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruText:Text = 
         Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
         THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
          
         THIS-OBJECT:ArendeHuvudControl:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).
      END.
      THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruText:ReadOnly = TRUE.
     /*
      THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruText:Enabled   = FALSE.
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
      /* Egna koder*/
      THIS-OBJECT:CreateGridEgnaKoder().
      THIS-OBJECT:CreateGridUnderKoder().
      THIS-OBJECT:CreateGridValdaUnderKoder().
      /* Materiel */
      THIS-OBJECT:CreateGridMateriel().
      THIS-OBJECT:CreateGridEgetMateriel().
      /* Faktorer, priser mm*/
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridMallarKoder().
      THIS-OBJECT:CreateGridValdaMallar().
      THIS-OBJECT:CreateGridFelMedd().
      THIS-OBJECT:CreateGridTidlage().
      
   END METHOD.

   METHOD PUBLIC VOID CreateGridFunctions():
      /* Drag dropp*/
      THIS-OBJECT:DGKoder = NEW Controls.Subclasses.DragDropGroup("Koder").
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:ArendeControl:GridLopKoder).
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:ArendeControl:GridKalkylKoder).
      THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:ArendeEgnaControl:GridUnderKoder).
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder).
      THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:ArendeMtrlControl:GridMateriel).
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel).
      THIS-OBJECT:DGMall = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:ArendeImpMallControl:GridMallar).
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:ArendeImpMallControl:GridValdaMallar).
      /* Button overs*/
      THIS-OBJECT:ArendeControl:KalkylButtonOverMark:GuruKoppla(THIS-OBJECT:ArendeControl:GridLopKoder,THIS-OBJECT:ArendeControl:GridKalkylKoder).
      THIS-OBJECT:ArendeControl:KalkylButtonOverMark:GuruDublett = TRUE.
      THIS-OBJECT:ArendeControl:KalkylButtonBackMark:GuruKoppla(THIS-OBJECT:ArendeControl:GridKalkylKoder,THIS-OBJECT:ArendeControl:GridLopKoder).
      THIS-OBJECT:ArendeControl:KalkylButtonBackMark:GuruRemoveFrom = TRUE.  
      /* Egna koder börjar här*/
      THIS-OBJECT:ArendeEgnaControl:EgnaButtonOverMark:GuruKoppla(THIS-OBJECT:ArendeEgnaControl:GridUnderKoder,THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder).
      THIS-OBJECT:ArendeEgnaControl:EgnaButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:ArendeEgnaControl:EgnaButtonBackMark:GuruKoppla(THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder, THIS-OBJECT:ArendeEgnaControl:GridUnderKoder).
      THIS-OBJECT:ArendeEgnaControl:EgnaButtonBackMark:GuruRemoveFrom = TRUE.
      /*materialtab*/
      THIS-OBJECT:ArendeMtrlControl:MatButtonOverMark:GuruKoppla(THIS-OBJECT:ArendeMtrlControl:GridMateriel,THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel).
      THIS-OBJECT:ArendeMtrlControl:MatButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:ArendeMtrlControl:MatButtonBackMark:GuruKoppla(THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel, THIS-OBJECT:ArendeMtrlControl:GridMateriel).
      THIS-OBJECT:ArendeMtrlControl:MatButtonBackMark:GuruRemoveFrom = TRUE.
      /*mallar*/
      THIS-OBJECT:ArendeImpMallControl:ButtonOverMarkMallar:GuruKoppla(THIS-OBJECT:ArendeImpMallControl:GridMallar,THIS-OBJECT:ArendeImpMallControl:GridValdaMallar).
      THIS-OBJECT:ArendeImpMallControl:ButtonOverMarkMallar:GuruDublett = TRUE.
      
      THIS-OBJECT:ArendeImpMallControl:ButtonBackMarkMallar:GuruKoppla(THIS-OBJECT:ArendeImpMallControl:GridValdaMallar, THIS-OBJECT:ArendeImpMallControl:GridMallar).
      THIS-OBJECT:ArendeImpMallControl:ButtonBackMarkMallar:GuruRemoveFrom = TRUE.
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      THIS-OBJECT:ArendeHuvudControl:GridPersonal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT Guru.Konstanter:PersonalTTh)).
      THIS-OBJECT:ArendeHuvudControl:GridPersonal:Text = "ansvarig".
      THIS-OBJECT:ArendeHuvudControl:GridPersonal:Text = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:ArendeHuvudControl:GridPersonal:Text,TRUE).
           /*egna event*/
      THIS-OBJECT:ArendeHuvudControl:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
      
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,150).
      THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      THIS-OBJECT:ArendeControl:GridArbetsKoder:TEXT = "Arbetskoder".      
      THIS-OBJECT:ArendeControl:GridArbetsKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:ArendeControl:GridArbetsKoder:tBS:PositionChanged:Subscribe(ValueArbetsKoderCh).
          
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35, "ARANDELOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      THIS-OBJECT:ArendeControl:GridLopKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:LopposterTTh)).
      THIS-OBJECT:ArendeControl:GridLopKoder:TEXT = "Löpnummer".
      THIS-OBJECT:ArendeControl:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:ArendeControl:GridLopKoder:tBS:PositionChanged:Subscribe(ValueLopnrCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("num","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35,"INT","INT").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE, 35, "ARANDELOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55,"DEC", "KALKYL").
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubrikergrid[11] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,150).
      rubrikergrid[12] = NEW Controls.GridRubrik("RISK","Risk i %",FALSE,75,"DEC", "KALKYL").
      rubrikergrid[13] = NEW Controls.GridRubrik("VINST","Vinst i %",FALSE,75,"DEC", "KALKYL").
   
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 13, INPUT THIS-OBJECT:KoderTTh)).
      THIS-OBJECT:ArendeControl:GridKalkylKoder:TEXT = "Kalkylkoder".  
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MARKNING"]:Hidden = TRUE.
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN .
      ELSE THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      THIS-OBJECT:ArendeControl:GridKalkylKoder:SetViewStyleBand(TRUE).
      /*
      THIS-OBJECT:splitContainerKomAnm:VISIBLE = FALSE.
      THIS-OBJECT:KoderAnmarkning:VISIBLE = FALSE.
      */
      /*default filter  sortering*/     
      
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruNoAutoTitle(TRUE).
      
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:ArendeControl:GridKalkylKoder:tBS:PositionChanged:Subscribe(ValueKalkKoderCh).
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruFirstrow().
     
      THIS-OBJECT:ArendeControl:GridKalkylKoder:ExtramenuItemAdd("Gå till").
      THIS-OBJECT:ArendeControl:GridKalkylKoder:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ExtraToolStripMenuItem_Click).
     
   END METHOD.
   
   METHOD PUBLIC VOID ExtraToolStripMenuItem_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:RibbonToolFilterTyp:Text = "Alla".
      THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruRepositionto(THIS-OBJECT:ArbetskoderTTh:ROWID).
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:ArendeControl:GridLopKoder:GuruRepositionto(THIS-OBJECT:LopposterTTh:ROWID). 
         END.   
      END.
     
   END METHOD.
   
   /* Egna koder*/
   METHOD PUBLIC VOID CreateGridEgnaKoder():
      {GridRubrikListaVarExtent.i}
      THIS-OBJECT:DontCheck = TRUE.
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","MATRIS",FALSE,35,"INT","INT").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "ARANDELOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",FALSE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,80,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,150).
      /*init av grid*/
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KoderTTh)). 
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:TEXT = "Egna koder".
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN .
      ELSE THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = FALSE.   
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:SetViewStyleBand(TRUE).
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFiltrera("arendenumtt.ARBKOD = 'EGEN'").
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:tBS:PositionChanged:Subscribe(ValueEgnaKoderCh).
      THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:DontCheck = false.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 250).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", TRUE, 100,"DEC", "KALKYL").
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:PriserTTh)).
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:TEXT = "Underkoder".
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NUMSUBID", "##", TRUE ,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100,"DEC", "KALKYL").
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ValdaPriserTTh)).
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUMSUBID"]:Hidden = TRUE.
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:TEXT = "Valda underkoder".
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
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
      THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT Guru.GlobalaVariabler:MaterielTTh)).
      
      THIS-OBJECT:ArendeMtrlControl:GridMateriel:Text = "Materiel".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruGridFarg(TRUE).             /*OBSE CASESENSITIVE*/ 
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         
      END. 
      THIS-OBJECT:ArendeMtrlControl:GridMateriel:VisaViaInternetMenuItemAddAdd().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MATRIS", "Matris", FALSE ,50,"INT","INT").
      rubrikergrid[2] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("BERKVANT", "Antal", FALSE,50,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("NPRIS", "Nettopris", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:arendemtrlTTh)).
      THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:Text = "Materielspecifikation".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:GuruGridFarg(TRUE).
         THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
      END.  
      THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:VisaViaInternetMenuItemAddAdd().
            
   END METHOD.

   
   METHOD PUBLIC VOID CreateGridMallarKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "ARANDELOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      THIS-OBJECT:ArendeImpMallControl:GridMallarKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:ArendeImpMallControl:GridMallarKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:ArendeImpMallControl:GridMallarKoder:Text = "Koder för mallen".
   END METHOD.

   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:ArendeImpMallControl:GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallHuvudtth)).  
      THIS-OBJECT:ArendeImpMallControl:GridMallar:Text = "Mallar".
      THIS-OBJECT:ArendeImpMallControl:GridMallar:tBS:PositionChanged:Subscribe(ValueMallarCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:ArendeImpMallControl:GridValdaMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallValdtth)).
      THIS-OBJECT:ArendeImpMallControl:GridValdaMallar:Text = "Valda mallar".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridTidlage():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,200).
      rubrikergrid[2] = NEW Controls.GridRubrik("DATUMTID","Datum",FALSE,200,"DATETIME").
      rubrikergrid[3] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      THIS-OBJECT:ArendeTidlControl:GridTidlage:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:arendetidlageTTh)).
      THIS-OBJECT:ArendeTidlControl:GridTidlage:TEXT = "Tidlägen".
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridStatus():
      {GridRubrikListaVarExtent.i}
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ARENDENR","Ärendenr",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("OMRADE",Guru.Konstanter:gomrk,TRUE,100).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,200).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,175).
      rubrikergrid[5] = NEW Controls.GridRubrik("DATUMTID","Datum",TRUE,150,"DATETIME").
      rubrikergrid[6] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      
      THIS-OBJECT:ArendeStatusControl:GridStatus:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:arendetidlageTTh)).
                                      
      THIS-OBJECT:ArendeStatusControl:GridStatus:TEXT = "Status".
     
   END METHOD.
    
   METHOD PUBLIC VOID CreateGridFelMedd():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("UT","Meddelande",TRUE,600).
      THIS-OBJECT:ArendeFelmeddControl:GridFelMedd:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 1, INPUT THIS-OBJECT:tiduth)).
      THIS-OBJECT:ArendeFelmeddControl:GridFelMedd:Text = "Fel vid inläsning!".
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
            THIS-OBJECT:ArendeHuvudControl:BestKund:GuruFiltrera().
         END.   
      END.
      
   END METHOD.
   
   METHOD PRIVATE VOID SetIniComboV():
      Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE) + "'")  NO-ERROR. 
      
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:SetInitialValue(THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE,STRING(Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE)).
      
      THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE)).
      Guru.Konstanter:BestKundTTh:FIND-FIRST("WHERE BESTID = '" + STRING(THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE) + "'")  NO-ERROR. 
      
      IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ArendeHuvudControl:BestKund:SetInitialValue(THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE,STRING(Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE)).
      END.
      ELSE THIS-OBJECT:AddBestKund(STRING(THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE)).                 
      
   END METHOD.
   
      
   /*v11.2 fix*/
   METHOD PUBLIC VOID SparaComboV(INPUT incombo AS Controls.Combo):
      IF incombo:VALUE = ? THEN RETURN.
      IF incombo:NAME  = "HuvudOmrade" THEN combolista[1] = THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE.
      IF incombo:NAME = "HuvudUtfardare" THEN combolista[2] = THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE.
      IF incombo:NAME = "BestKund" THEN combolista[5] = THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE.
      
   END METHOD.
   /*v11.2 fix*/
   
   METHOD PRIVATE VOID GetComboV(INPUT incombo AS Controls.Combo):
       
      IF incombo:VALUE NE ? THEN RETURN.
    
      IF incombo:NAME  = "HuvudOmrade" THEN THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:VALUE = combolista[1].
    
      IF incombo:NAME = "HuvudUtfardare" THEN THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:VALUE = combolista[2]. 
      IF incombo:NAME = "BestKund" THEN THIS-OBJECT:ArendeHuvudControl:BestKund:VALUE =  combolista[5].    
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
      THIS-OBJECT:ArendeHuvudControl:comboPriserUppf:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "UTYP", "TYPC").    
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
      
      /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "cGRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO:*/
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
      THIS-OBJECT:ArendeHuvudControl:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
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
     
     
      THIS-OBJECT:ArendeHuvudControl:BestKund:GuruInitCombo(cmbBestlist, "VIBESTID", "BESTNAMN").
     
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
        
      THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").  
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
      THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
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
      THIS-OBJECT:ArendeHuvudControl:HuvudUtfardare:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
   END METHOD.
   

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerEgenKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
    
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:ArendeControl:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ArendeControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN
         THIS-OBJECT:ArendeControl:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
      IF THIS-OBJECT:ArendeControl:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:ArendeControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE
         THEN THIS-OBJECT:ArendeControl:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
      END.
   END METHOD.
   
   

   
   

   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   METHOD PUBLIC VOID FilterUnderKoder():
      /* Om det finns en aktiv row..*/
      IF THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:tBS:Count > 0 AND THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow NE ? THEN DO:
         /* Uppdatera valda underkoderna, och återställ kontrollerna*/
         IF TRIM(STRING(THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE)) = "" THEN RETURN.
         IF THIS-OBJECT:DontCheck = FALSE THEN DO:
            THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING( THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE )).
            THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFirstrow().
            THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:ArendeEgnaControl:EgnaButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:ArendeEgnaControl:EgnaButtonBackMark:Enabled = TRUE.
         END.  
      END.
           
      ELSE IF THIS-OBJECT:KoderTTh:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFirstrow().
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:Enabled = TRUE.
         THIS-OBJECT:ArendeEgnaControl:EgnaButtonOverMark:Enabled = TRUE.
         THIS-OBJECT:ArendeEgnaControl:EgnaButtonBackMark:Enabled = TRUE.

      END.      
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:Enabled = FALSE.
         THIS-OBJECT:ArendeEgnaControl:EgnaButtonOverMark:Enabled = FALSE.
         THIS-OBJECT:ArendeEgnaControl:EgnaButtonBackMark:Enabled = FALSE.
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
      THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell = THIS-OBJECT.
      THIS-OBJECT:StartaControl().
      THIS-OBJECT:CreateForm().
      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL InitializeArendeStatus():
      THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell = THIS-OBJECT.
      THIS-OBJECT:StartaControl().    
      THIS-OBJECT:CreateRibbonVS().
      THIS-OBJECT:TabManager:Tabs["Status"]:Selected = TRUE.
       
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID StartaControl():
      ArendeHuvudControl = NEW Modules.Arende.ArendeHuvudControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:ArendeHuvudControl).
      ArendeHuvudControl:Dock = System.Windows.Forms.DockStyle:FILL. 
      
      ArendeControl = NEW Modules.Arende.ArendeControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:ArendeControl).
      ArendeControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeEgnaControl = NEW Modules.Arende.ArendeEgnaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:ArendeEgnaControl).
      ArendeEgnaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeMtrlControl = NEW Modules.Arende.ArendeMtrlControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:ArendeMtrlControl).
      ArendeMtrlControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeImpMallControl = NEW Modules.Arende.ArendeImpMallControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:ArendeImpMallControl).
      ArendeImpMallControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeTidlControl = NEW Modules.Arende.ArendeTidlControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabTidlage:Controls:Add(THIS-OBJECT:ArendeTidlControl).
      ArendeTidlControl:Dock = System.Windows.Forms.DockStyle:FILL.
       
      ArendeFelmeddControl = NEW Modules.Arende.ArendeFelmeddControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabFelmedd:Controls:Add(THIS-OBJECT:ArendeFelmeddControl).
      ArendeFelmeddControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeVisaControl = NEW Modules.Arende.ArendeVisaControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:ArendeVisaControl).
      ArendeVisaControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      ArendeStatusControl = NEW Modules.Arende.ArendeStatusControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabStatus:Controls:Add(THIS-OBJECT:ArendeStatusControl).
      ArendeStatusControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
    
      ArendeJoinControl = NEW Modules.Arende.ArendeJoin(THIS-OBJECT:Root).
      THIS-OBJECT:TabJoin:Controls:Add(THIS-OBJECT:ArendeJoinControl).
      ArendeJoinControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
   END METHOD. 



   METHOD PUBLIC VOID UpdateMatrisE(sender AS System.Object, args AS System.EventArgs):
      THIS-OBJECT:UpdateMatris().
   END METHOD.

   METHOD PUBLIC VOID UpdateMatris():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO. 
      mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:Text = "Alla" THEN mat = -1.
      ELSE DO:
         mat = RibbonToolMatris:SelectedIndex. 
      END.
      /*
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
      */
      THIS-OBJECT:CurrentMatris = mat.
     
      IF THIS-OBJECT:NyKalkyl = FALSE THEN DO:
         IF mat = -1 THEN DO:
            THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:GuruFiltrera("").
            THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruFiltrera("").
         END.
         ELSE DO:
            THIS-OBJECT:ArendeMtrlControl:GridEgetMateriel:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            THIS-OBJECT:ArendeControl:KalkylButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:ArendeEgnaControl:EgenSkapa:Enabled = TRUE.
            THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)+ " AND ARBKOD = 'EGEN'" ).
            THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
         END.
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:FilterUnderKoder().
      END.      
   END METHOD.
   METHOD PUBLIC VOID GomaRibbon():
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem(Guru.GlobalaVariabler:KalkArendeText)).
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Kommentarer".
      THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera " + Guru.GlobalaVariabler:KalkArendeText.
      THIS-OBJECT:RibbonTabVisning:Caption = Guru.GlobalaVariabler:KalkArendeText + "visning".   
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
      THIS-OBJECT:RibbonTabKalkyl = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkyl").
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
      THIS-OBJECT:RibbonToolFilterTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FilterTyp").
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Caption = "Märkning".
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Width.
      /* kalkyl*/
      THIS-OBJECT:RibbonToolOmrade = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoOmrade").
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Caption = Guru.Konstanter:gomrk.
      THIS-OBJECT:RibbonToolOmrade:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:ArendeHuvudControl:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:ArendeHuvudControl:HuvudKatalog:Text.
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      /* Koder */
      THIS-OBJECT:RibbonToolKoderAnm = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkning").
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:Caption = "Anm. för koder".
      THIS-OBJECT:RibbonToolKoderAnm:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens-kommentar".
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAngeKodAntal = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AngeKod").
      THIS-OBJECT:RibbonToolAngeKodAntal:SharedPropsInternal:Caption = "Fri inmatning av Koder och antal".
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
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
      /* visningsinställningar */
      THIS-OBJECT:RibbonToolVisVisa = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisVisafor").
      THIS-OBJECT:RibbonToolVisVisa:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Grundkalkyl")).
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
      
      /* Uppföljning */
      /*
      THIS-OBJECT:RibbonToolUppfPriser = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("UppfPriser").
      THIS-OBJECT:RibbonToolUppfPriser:SharedPropsInternal:Caption = "Priser".
      THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Enligt katalog")).
      THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Eget materiel")).
      THIS-OBJECT:RibbonToolUppfPriser:SharedPropsInternal:Width = 130.
      */
      /*
      THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Från beredning")).
      */
     
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
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy). /* <- Den här måste preppas en gång på detta sättet, och sen läggas till med den andra metoden varje gång den används*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMatris,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKoderAnm,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAngeKodAntal,THIS-OBJECT:RibbonTabKoder).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy). /*redan perppad*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatris,THIS-OBJECT:RibbonTabEgnaKoder). /* Den här är nästan likadan som ovanstående kommentar, alla ribbonverktyg som läggs till måste och får bara preppas 1 gång, därefter måste man lägga till dom manuellt, se nedan */
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris).         /*redan perppad*/                                     /* Så här*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastmarkmtrl,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat,THIS-OBJECT:RibbonTabKalkyl).    
      
     
      
      /*fix inför piteå ej klara nya*/ 
      /* Preppa Visning*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera,THIS-OBJECT:RibbonTabVisning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUtBildval,THIS-OBJECT:RibbonTabVisning).
      
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisAnmarkning,THIS-OBJECT:RibbonTabVisningVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig,THIS-OBJECT:RibbonTabVisningVisa).
      
      /*se piteå*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisVisa,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisPriser,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatris). /*redan preppad*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummera,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGruppera,THIS-OBJECT:RibbonTabVisningEgenskaper).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGroupMatris,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatSpec,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonDummy). /*redan preppad*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,THIS-OBJECT:RibbonTabVisningEgenskaper).
      
      
      
      /* binda events*/
      THIS-OBJECT:RibbonToolSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolEnr:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolGenerera:ToolClick:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtE).
      THIS-OBJECT:RibbonToolVisAnmarkning:ToolClick:Subscribe(THIS-OBJECT:VisAnmarkningChanged).
      THIS-OBJECT:RibbonToolVisPriser:ToolValueChanged:Subscribe(THIS-OBJECT:VisPriserChanged).
      THIS-OBJECT:RibbonToolVisPriser:SelectedItem = THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolVisVisa:SelectedItem = THIS-OBJECT:RibbonToolVisVisa:ValueList:ValueListItems[0].
      
      THIS-OBJECT:RibbonToolKoderAnm:ToolClick:Subscribe(THIS-OBJECT:KoderAnmChanged).
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:FrekvensKomChanged).
      THIS-OBJECT:RibbonToolAngeKodAntal:ToolClick:Subscribe(THIS-OBJECT:FriInmatningKod).
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder).
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolUppfPriser:ToolValueChanged:Subscribe(RibbonUpdateUppfPriser).
      */
      THIS-OBJECT:RibbonToolMatris:ToolValueChanged:Subscribe(UpdateMatrisE).
      THIS-OBJECT:RibbonToolVisGroupMatris:ToolClick:Subscribe(THIS-OBJECT:GruppMatrisChanged).
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:Subscribe(SparaDefs).
      /* splatter */
      THIS-OBJECT:LoadRibbonLevs().    
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
   
   METHOD PUBLIC VOID FriInmatningKod(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolAngeKodAntal:Checked EQ TRUE THEN DO:
         THIS-OBJECT:ArendeControl:splitContainerLopInAntal:Panel2Collapsed = FALSE.
         /*Dummy-lösning för att hantera bugg för FriIn när kalkylen är ny (16 apr 2014 15:12:51)*/
         IF friindummy = FALSE THEN DO:
            THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
            THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
            friindummy = TRUE.
         END.   
         
      END.    
      ELSE THIS-OBJECT:ArendeControl:splitContainerLopInAntal:Panel2Collapsed = TRUE.
      
      
   END METHOD.
   METHOD PUBLIC VOID FrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         ASSIGN
         THIS-OBJECT:ArendeControl:Rubrikanm:Text                                 = "Kommentar"
         THIS-OBJECT:RibbonToolKoderAnm:Checked                                   = FALSE 
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE               = FALSE
         THIS-OBJECT:ArendeControl:ValdKodKommentar:VISIBLE                       = TRUE
         THIS-OBJECT:ArendeControl:KalkylensKoderAnmarkning:VISIBLE               = FALSE.
         THIS-OBJECT:ArendeControl:GridKalkylKoder:VISIBLE                        = FALSE.
         THIS-OBJECT:ArendeControl:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         THIS-OBJECT:ValueLopnrCh(?,?).
         THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE = TRUE. 
         IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN DO:
            THIS-OBJECT:ArendeControl:splitContainerKalkKodoFrek:Panel1Collapsed = TRUE.            
         END.  
         THIS-OBJECT:ArendeControl:splitContainerKalkLopnr:Panel2Collapsed = TRUE. 
      END.   
      ELSE DO:
         ASSIGN 
         THIS-OBJECT:ArendeControl:splitContainerKalkLopnr:Panel2Collapsed = FALSE
         THIS-OBJECT:ArendeControl:Rubrikanm:Text = "Anmärkning"
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = TRUE
         THIS-OBJECT:ArendeControl:GridKalkylKoder:VISIBLE                        = TRUE
         THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE               = FALSE.
         THIS-OBJECT:ArendeControl:splitContainerKalkKodoFrek:Panel1Collapsed = FALSE.
         IF THIS-OBJECT:RibbonToolKoderAnm:Checked = FALSE THEN DO:
            THIS-OBJECT:ArendeControl:KalkylensKoderAnmarkning:VISIBLE            = FALSE.
         END.
         ELSE DO:
            THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE            = TRUE.
            THIS-OBJECT:ArendeControl:KalkylensKoderAnmarkning:VISIBLE           = TRUE.
         END.   
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN DO:
         THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE = TRUE.
         THIS-OBJECT:ArendeControl:KalkylensKoderAnmarkning:VISIBLE = TRUE.   
         THIS-OBJECT:ArendeControl:ValdKodKommentar:VISIBLE = FALSE.               
      END.   
      ELSE DO:
         IF THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE THEN DO:
            THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE = FALSE.
            THIS-OBJECT:ArendeControl:ValdKodKommentar:VISIBLE = FALSE.
         END. 
         ELSE DO:
            THIS-OBJECT:ArendeControl:splitContainerKalkKomAnm:VISIBLE = TRUE.
            THIS-OBJECT:ArendeControl:ValdKodKommentar:VISIBLE = TRUE.
         END.                      
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
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID SparaExcelE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:ArendeVisaControl:VisaExcel:SaveFileWithDialog().
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtGrid(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:ArendeStatusControl:GridStatus:SkrivUt(). 
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning():
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
      Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " WHERE TYP = 2" + " NO-LOCK".
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
      DEFINE VARIABLE Markning   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE DoAll      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoUnmarked AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoReturn   AS LOGICAL   INITIAL FALSE NO-UNDO.
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      IF Markning EQ "Alla" OR Markning EQ "" THEN DoAll = TRUE.
      ELSE DoAll = FALSE.
      IF Markning EQ "Ej märkta" THEN DoUnmarked = TRUE.
      ELSE DoUnmarked = FALSE.
      THIS-OBJECT:ArendeControl:GridLopKoder:GuruAvmarkera(). 
      IF DoAll EQ TRUE THEN DO:
         THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFirstrow().
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFiltrera(" MARKNING = ''").
            THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFirstrow().
         END.
         ELSE DO:
            THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFiltrera(" MARKNING = '" + Markning + "'").
            THIS-OBJECT:ArendeControl:GridArbetsKoder:GuruFirstrow().
         END.
         IF THIS-OBJECT:ArendeControl:GridArbetsKoder:tBS:Count EQ 0 THEN DO:
            THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
            DoReturn = TRUE.
         END.      
      END.
      IF DoReturn EQ TRUE THEN RETURN.
      IF THIS-OBJECT:ArbetsKoderTTh:AVAILABLE EQ TRUE THEN DO:
         THIS-OBJECT:LopposterTTh:FIND-FIRST() NO-ERROR.
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFiltrera(" ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFirstrow(). 
      END.
      ELSE DO:
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFirstrow().
         IF THIS-OBJECT:KalkylLoaded EQ TRUE THEN DO:
            System.Windows.Forms.MessageBox:Show("Inga arbetskoder funna!").
         END.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID UpdateRibbonShow():
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      i = 0.
        
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
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruTom().
         THIS-OBJECT:SokMtrl(enr, ben, kundvar).
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruReopen().
         THIS-OBJECT:ArendeMtrlControl:GridMateriel:GuruFirstrow().
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
   /*öppnar arbetskoder ned rätt query för vald typ*/
   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:ArendeControl:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      THIS-OBJECT:RefreshMarkning().
      THIS-OBJECT:FriInSet().
   END METHOD.
   
   METHOD PUBLIC VOID RibbonUpdateUppfPriser(sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE = 1 + THIS-OBJECT:RibbonToolUppfPriser:SelectedIndex.
      THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
   END METHOD.  
    
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeShell").
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
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabTidlage = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabFelmedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabStatus = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabJoin = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
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
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabStatus */
      /*  */
      THIS-OBJECT:TabStatus:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabStatus:Name = "TabStatus".
      THIS-OBJECT:TabStatus:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabJoin */
      /*  */
      THIS-OBJECT:TabJoin:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabJoin:Name = "TabJoin".
      THIS-OBJECT:TabJoin:Size = NEW System.Drawing.Size(1145, 553).
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
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabMateriel).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabImportMallar).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkylHuvud).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabEgna).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFelmedd).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabTidlage).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabStatus).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabJoin).
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
      ultraTab19:ActiveAppearance = appearance9.
      appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
      ultraTab19:Appearance = appearance10.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget Materiel".
      appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance11.
      appearance12:BackColor = System.Drawing.Color:Transparent.
      appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
      ultraTab22:Appearance = appearance12.
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "Import/Mallar".
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      ultraTab3:ActiveAppearance = appearance13.
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      ultraTab3:Appearance = appearance14.
      ultraTab3:Key = "Tidlägen".
      ultraTab3:TabPage = THIS-OBJECT:TabTidlage.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Tidlägen".
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance15.
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      ultraTab1:Appearance = appearance16.
      ultraTab1:Key = "FelMedd".
      ultraTab1:TabPage = THIS-OBJECT:TabFelmedd.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Felmeddelanden".
      appearance17:Image = CAST(resources:GetObject("appearance17.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance17.
      appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
      ultraTab16:Appearance = appearance18.
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "Visa ".
      appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
      ultraTab4:ActiveAppearance = appearance19.
      appearance20:Image = CAST(resources:GetObject("appearance20.Image"), System.Object).
      ultraTab4:Appearance = appearance20.
      ultraTab4:Key = "Status".
      ultraTab4:TabPage = THIS-OBJECT:TabStatus.
      ultraTab4:Text = "".
      ultraTab2:Key = "Join".
      ultraTab2:TabPage = THIS-OBJECT:TabJoin.
      ultraTab2:Text = "Join".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 10 NO-UNDO.
      arrayvar0[1] = ultraTab13.
      arrayvar0[2] = ultraTab14.
      arrayvar0[3] = ultraTab15.
      arrayvar0[4] = ultraTab19.
      arrayvar0[5] = ultraTab22.
      arrayvar0[6] = ultraTab3.
      arrayvar0[7] = ultraTab1.
      arrayvar0[8] = ultraTab16.
      arrayvar0[9] = ultraTab4.
      arrayvar0[10] = ultraTab2.
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
      /* ArendeShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "ArendeShell".
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
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
      
      THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
     
      PROCESS EVENTS.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
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
      
      THIS-OBJECT:GetComboV(ArendeHuvudControl:HuvudOmrade).
      THIS-OBJECT:GetComboV(ArendeHuvudControl:HuvudUtfardare).
      THIS-OBJECT:GetComboV(ArendeHuvudControl:BestKund).
      
      IF cFrom EQ ? THEN cFrom = "".  
      IF cTo = "Status" THEN DO:
         THIS-OBJECT:RibbonTabVisning:Visible = TRUE.
         THIS-OBJECT:VisaTabs("ALLA", FALSE).
         THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE. 
         RETURN. 
      END.
      IF cFrom = "Kalkylhuvud" THEN DO:        
         IF THIS-OBJECT:NyKalkyl = TRUE THEN DO:
           
            IF THIS-OBJECT:ApplyCreatedKalkyl() EQ FALSE THEN DO:
               e:Cancel = TRUE.
            END.
            ELSE THIS-OBJECT:SparaDefs(?,?).           
         END.
         ELSE DO:
            IF THIS-OBJECT:SaveHuvud() = FALSE THEN DO:
               e:Cancel = TRUE.
            END.   
         END.  
      END.    
      IF cFrom = "Egna koder" THEN DO:
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruTom().
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruTom().
      END.   
      IF cTo = "Visa Kalkyl" THEN DO:
         IF THIS-OBJECT:WasExcelShown = FALSE THEN DO:
            THIS-OBJECT:VisningKalkIni(OUTPUT path).
            THIS-OBJECT:GenereraKalkylVisning().
            THIS-OBJECT:WasExcelShown = TRUE.                 
         END.
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.      
      IF cTo = "FaktorerPriser" THEN DO:
      END.      
      IF cTo = "Kalkyl" THEN DO:     
         THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruReopen().
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END. 
                
      IF cTo = "Egna koder" THEN DO:
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruTom().
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruTom().
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruReopen().
         THIS-OBJECT:ArendeEgnaControl:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:ArendeEgnaControl:GridValdaUnderKoder:GuruFirstRow().
         THIS-OBJECT:ArendeEgnaControl:GridUnderKoder:ValueChanged().
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
            THIS-OBJECT:Root:DatabaseManager:Arende:MallarHmt().
            THIS-OBJECT:ArendeImpMallControl:GridMallar:GuruReopen().
            THIS-OBJECT:ArendeImpMallControl:GridMallar:GuruFirstRow().
         END.   
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.  
      END.
      IF cTo = "Felmedd" THEN DO:
         THIS-OBJECT:ArendeFelmeddControl:GridFelMedd:GuruReopen().
         THIS-OBJECT:ArendeFelmeddControl:GridFelMedd:GuruFirstRow().           
      END.
      IF cTo = "Join" THEN DO:
         THIS-OBJECT:ArendeJoinControl:StartJoin().
                    
      END.
      PROCESS EVENTS.
   END METHOD.
   
  METHOD PUBLIC VOID VisningKalkIniMulti():
     DEFINE VARIABLE path  AS CHARACTER NO-UNDO. 
     vismulti = TRUE.
     THIS-OBJECT:RibbonTabKalkyl:Visible = FALSE.
     THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = FALSE.
     THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.
  END METHOD. 
    
   /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:44:31 
   Sätter UsMsInterrop, skapar en excel fil för skrivning av template 
   startar Interop excel
   */
   
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
      THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = TRUE.
      
      IF THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:         
         THIS-OBJECT:SetVisningsBild(0).
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         /*behövs inte
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ExcelNamn(path).
         */
         THIS-OBJECT:ArendeVisaControl:VisaExcel:InteropInitialize(path). 
         
         /*Robin Sjöberg Elpool i Umeå AB  16 jun 2014 16:17:03 
         Döljer knappar för spara och skrivut om excel 
         */
         IF THIS-OBJECT:ArendeVisaControl:VisaExcel:OutExterntWin = TRUE OR THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = FALSE THEN DO:
            THIS-OBJECT:ArendeVisaControl:toolStripButtonExporteraExcel:Visible = FALSE.
            THIS-OBJECT:ArendeVisaControl:toolStripButtonSkrivUt:Visible = FALSE.
         END.
         ELSE DO:
            THIS-OBJECT:ArendeVisaControl:toolStripButtonExporteraExcel:Visible = TRUE.
            THIS-OBJECT:ArendeVisaControl:toolStripButtonSkrivUt:Visible = TRUE.
         END.   
           
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
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:ArendeControl:GridLopKoder:GuruFirstrow().
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN  DO:
         THIS-OBJECT:ArendeImpMallControl:GridMallarKoder:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:ArendeImpMallControl:GridMallarKoder:GuruFirstrow().
      END.
   END METHOD.
   
   METHOD PRIVATE CHARACTER GetMatrisName():
      DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:HuvudTTh:Buffer-field("TYPKALK"):Buffer-VALUE = 7 THEN mName = "Period".
      ELSE mName = "Matris".
      RETURN mName.
   END METHOD.

   METHOD PRIVATE VOID ValueLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):  
                     
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         THIS-OBJECT:ArendeControl:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         
      END.   
   /*
   THIS-OBJECT:FrekvensKomChanged(?,?).
   */
   END METHOD.
   
   METHOD PRIVATE VOID ValueKalkKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      DEFINE VARIABLE q     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE exrid AS RECID     NO-UNDO.
      /*Se filtrera i Grid*/ 
      IF THIS-OBJECT:ArendeControl:GridKalkylKoder:tBS:Count > 0 THEN DO:
         IF THIS-OBJECT:DontCheck = TRUE THEN.
         ELSE DO: 
           /*     
            IF STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN DO: 
               exrid = INTEGER(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE)).           
               THIS-OBJECT:GridEgnaKoder:GuruRepositionTo( THIS-OBJECT:KoderTTh:ROWID /* ROWID(kalknumtt) */).
               q = STRING("WHERE kalknumtt.TTRECID = " + STRING(exrid)).
               THIS-OBJECT:KoderTTh:FIND-FIRST(q, NO-LOCK) NO-ERROR.
               /*FIND FIRST kalknumtt WHERE kalknumtt.TTRECID = exrid NO-LOCK NO-ERROR.*/
               THIS-OBJECT:FilterUnderKoder().    
                        
            END. 
            */
         END.
         IF KoderTTh:AVAILABLE THEN THIS-OBJECT:ArendeControl:KalkylensKoderAnmarkning:Text  = THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
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
      THIS-OBJECT:ArendeHuvudControl:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
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
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Materiel"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["ImportMallar"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Felmedd"]:KEY = vilka THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:VISIBLE   = visavar.   
      
      IF THIS-OBJECT:HuvudTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
            THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE = FALSE. 
            THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = FALSE.
            THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = FALSE.            
         END.
      END.    
      IF THIS-OBJECT:NyKalkyl = TRUE THEN THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = FALSE.
      ELSE THIS-OBJECT:TabManager:Tabs["Tidlägen"]:VISIBLE   = TRUE.  
   END METHOD.   

   METHOD PUBLIC VOID SetVisningsBild(i AS INTEGER):
      /* 
         0 = inget genererat än
         1 = genererar
         2 = resultat
      */
      IF i EQ 0 THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Visible = TRUE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 1 THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Visible = TRUE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 2 THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Visible = TRUE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:ArendeVisaControl:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
   END METHOD.

   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      /*Anders Olsson Elpool i Umeå AB  17 jun 2014 12:05:28 
      Används inte 
      */
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ArendeVisaControl:VisaExcel:Visible EQ TRUE THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN DO:
            THIS-OBJECT:VisningKalkIni(OUTPUT path).
            THIS-OBJECT:GenereraKalkylVisning().   
         END.
      END.
      ELSE DO:
         THIS-OBJECT:VisningKalkIni(OUTPUT path).
         THIS-OBJECT:GenereraKalkylVisning().
      END.
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
      THIS-OBJECT:ArendeVisaControl:VisaExcel:direkticell = FALSE.
      
      OS-DELETE VALUE(ExelKommando) NO-ERROR.
      IF vad = "VKALK" THEN DO:
         path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xlsx").
         IF path = ? THEN path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
      END.   
     
      path = THIS-OBJECT:ExcelTemplate(INPUT path).
      ExelKommando = path.            
   END METHOD.
   
   METHOD PUBLIC VOID GenereraKalkylVisning():
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
      DEFINE VARIABLE kostnadstartcol   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE excelvarde AS CHARACTER NO-UNDO.
      path = ExelKommando.
      ASSIGN 
      cSenasteArbKod     = ""
      iSenasteLopNr      = 0        
      iSenasteMatris     = 0        
      cSenasteMtrl       = ""       
      iSenasteMtrlMatris = 0.
   
      IF THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         Guru.Konstanter:AmericanSet().
         /*
         IF THIS-OBJECT:WasExcelShown = FALSE THEN THIS-OBJECT:VisningKalkIni(OUTPUT path).
         ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         */
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
     
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum = 1.
      IF THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ScreenUpp(FALSE).
         THIS-OBJECT:SetVisningsBild(1).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:SlutOutExterntWin().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Reload(path).         
      END.  
      ELSE THIS-OBJECT:ArendeVisaControl:VisaExcel:OpenExcel(path).
      /*INTE GRUPPERA OM BARA EN MATRIS*/
      IF VisMatris() = 0 THEN.
      ELSE DO:
         THIS-OBJECT:RibbonToolVisGroupMatris:CHECKED = FALSE.
      END.   
      /*BERÄKNA KALYLEN*/
      THIS-OBJECT:Root:DatabaseManager:Arende:CalculateSum().
      THIS-OBJECT:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Arende:kalkantalTTh.
      THIS-OBJECT:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Arende:KalkkostnadTTh.
      THIS-OBJECT:kalkrubrikTTh = THIS-OBJECT:Root:DatabaseManager:Arende:kalkrubrikTTh.
      
      {LOGGORc.I}
      THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
      IF link NE ? THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:PageSetUppHeaderImage(INPUT 1, INPUT link). 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad = 1.
      END.
      
      Guru.Konstanter:JurAvdTTh:FIND-FIRST("WHERE AVDELNINGNR = " + Guru.Konstanter:OmradeTTh:BUFFER-FIELD("AVDELNINGNR"):BUFFER-VALUE) NO-ERROR.
      IF Guru.Konstanter:JurAvdTTh:AVAILABLE THEN DO:
         Guru.Konstanter:JurPersTTh:FIND-FIRST("WHERE JUDID = '" + STRING(Guru.Konstanter:JurAvdTTh:BUFFER-FIELD("JUDID"):BUFFER-VALUE) + "'") NO-ERROR.         
      END.
      
      IF Guru.Konstanter:JurPersTTh:AVAILABLE THEN DO:                             
         THIS-OBJECT:ArendeVisaControl:VisaExcel:PageSetUppHeader(INPUT 2, INPUT '&C&",Bold"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + CHR(10) +
         '&",Normal"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("GATUADR"):BUFFER-VALUE) + "  " + CHR(10) +
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTNR"):BUFFER-VALUE,"xxx xx") + " " + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTADR"):BUFFER-VALUE) + CHR(10) + 
         "Telefon:" + Guru.Konstanter:JurPersTTh:BUFFER-FIELD("TELVXL"):BUFFER-VALUE + CHR(10)).
      END. 
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:PageSetUppHeader(INPUT 3, INPUT '&"Calibri,Bold"&20' + THIS-OBJECT:RibbonToolVisVisa:Text).
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
      /* Skapa huvud */
      THIS-OBJECT:ExcelHuv().    
      /*ANMÄRKNING*/
      THIS-OBJECT:ExcelAnm().
      /* --- Börja göra listan här --- */
      /* Skapa rubrikerna */
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
      rubrikrad = THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad. 
     
      
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().  
      radefterrubrikrad = THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad.
      matrisrader = radefterrubrikrad.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdup().  
      THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Kod").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Benämning").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Antal").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Enh").
      /*LÄGG UT RUBRIKER FÖR TIMMAR*/
      utrustrubrik = FALSE.
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Utrust" THEN utrustrubrik = TRUE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      kostnadstartcol = "".
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname.
         IF kostnadstartcol = "" THEN DO:
            IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "EA" THEN.
            ELSE kostnadstartcol = THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname.
         END.   
         qH:GET-NEXT(). 
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      /*rubrik*/
      THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Summa").
      /* Gör rubriker bold */
      THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad) ,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Border("A"+ STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
      /*OM MATRIS*/
      IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE. 
      ELSE DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE + " WHERE kalknumtt.MATRIS = " + STRING(VisMatris()).      
      END.    
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.       
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:           
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().      
         /*    GRUPPERA MATRIS*/
            
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO: 
            THIS-OBJECT:ExcelGruppMatris(INPUT 1, INPUT matrisrader, INPUT kostnadstartcol ).
            IF iSenasteMatris = 0 THEN matrisrader = matrisrader.
            ELSE IF iSenasteMatris NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE THEN DO:
               matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad))).
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
            IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         END.    
         ELSE THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         /*UT MED ARB KOD MM*/  
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
         /*UT MED TIMMARNA*/
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE kalkantal.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE),"0",TRUE).               
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         /*UT MED KOSTNAD*/
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE),"0",TRUE).
              
            END.
            /*UT MED EA*/
            THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "EA"') NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOutFormat(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE),"0",TRUE).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.           
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).         
         /*SUMMERA HORISONT*/
         THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:irad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,4,"0").
        
         /*RISKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 2 THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:ArendeVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100))).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"0").
            END.   
         END.   
         /*VINSTKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 3 THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(DECIMAL(THIS-OBJECT:ArendeVisaControl:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100))).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:KolumnFormat(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"0").
            END.   
         END.  
         qH:GET-NEXT(NO-LOCK).
      END.
      /*GRUPPERA MATRIS*/     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
         THIS-OBJECT:ExcelGruppMatris(INPUT 2,INPUT matrisrader, INPUT kostnadstartcol).       
         matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad)) + 1).                                
      END.
      /*EGET MATRIEL SUMMA*/
        
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN.
      ELSE DO:        
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
            THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:  
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  IF VisMatris() = 0 THEN DO:
                     THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + 
                     STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(?))).
                  END.
                  ELSE DO:
                     THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),
                     STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(VisMatris()))).
                  END.
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:irad),THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),4,"0").
                  
               END.                       
            END.            
         END.
      END.
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen körs ej i ärende! 
        
      /*Berednings mtrl*/   
      IF THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "B".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:irad),THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum) + 
               STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),4,"0").       
               
            END.                   
         END.           
      END.
     */
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      
      /*KOLUMNBREDD*/
      THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      DEFINE VARIABLE colnumvar AS INTEGER NO-UNDO.
      colnumvar = 1.
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),17).
      colnumvar = colnumvar  + 1.     
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),45).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),14).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),5).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),13). /*H*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),12).
      colnumvar = colnumvar  + 1.
      IF utrustrubrik = TRUE THEN DO: 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),13). /*n*/
         colnumvar = colnumvar  + 1.
      END.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth(THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(colnumvar) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),18). 
      colnumvar = colnumvar  + 1.
      
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:BordHoriz(radefterrubrikrad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:BordHorizW(radefterrubrikrad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:BordVert(radefterrubrikrad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:ArendeVisaControl:VisaExcel:BordVertW(radefterrubrikrad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      /*SUMMERA ALLA KOLUMNER*/
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "E".
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
      
      REPEAT:
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:SummaM(THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname + THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,4,"Summa","A",FALSE,"0",TRUE).
         END.
         ELSE DO:   
            THIS-OBJECT:ArendeVisaControl:VisaExcel:SummaV(THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname + THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,4,"0",TRUE).
         END. 
         IF THIS-OBJECT:ArendeVisaControl:VisaExcel:iColnum = THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum THEN DO: 
            excelvarde = THIS-OBJECT:ArendeVisaControl:VisaExcel:GetData(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
            excelvarde = STRING(INTEGER(excelvarde),">>,>>>,>>>,>>>,>>9").
            excelvarde = REPLACE(excelvarde,",","").
            IF THIS-OBJECT:ArendeVisaControl:VisaExcel:HittaGetData("A","Ärendesumma:") > 0 THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("C" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:HittaGetData("A","Ärendesumma:")),TRIM(excelvarde)).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("D" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:HittaGetData("A","Ärendesumma:")),"SEK").
               
            END.
            LEAVE.
         END.   
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
      END.              
      
      /*ANDERS
      Elpool.Helpers:GetRange(THIS-OBJECT:ArendeVisaControl:VisaExcel:GetWorkSheet(),rubrikrad , "C" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad) ):Merge( true ).
      */
      
      /*FONTER*/
      THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"Garamond",16,? ,0,0 ).
      THIS-OBJECT:ArendeVisaControl:VisaExcel:Border(rubrikrad, THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad)). 
      
      /*BILAGA EGNA PRISER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(1).
      END.
      /*BILAGA EGNA FAKTORER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO: 
         THIS-OBJECT:ExcelBilagor(2).       
      END.   
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" OR 
         /*BILAGA EGNA Priser*/
         THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth("C1",THIS-OBJECT:ArendeVisaControl:VisaExcel:ColWidth("A1")).
      END.   
      /*BILAGA EGNet mtrl*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisMatSpec") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(3).      
      END.   
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen körs ej i ärende
        
      /*BILAGA mtrl från ber*/
      IF THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:ExcelBilagor(4).      
      END.
      */
      
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ScreenUpp(TRUE).
      /*allt skrivs i arket*/
      THIS-OBJECT:ArendeVisaControl:VisaExcel:valueDataOutTT().
     
      IF THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:SidBrytBredd(2).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ViewType(INPUT 3).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ExcelAppHandle:ActiveWorkbook:Activate().
         /*behövs här för att ta bort ful ramen*/
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Fix().
         THIS-OBJECT:SetVisningsBild(2).
         /*
         THIS-OBJECT:ArendeVisaControl:VisaExcel:SlutOutExterntWin().
         */
      END.   
      ELSE THIS-OBJECT:ArendeVisaControl:VisaExcel:SlutExcel(3).
      
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
      
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().             
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(Guru.GlobalaVariabler:KalkArendeText + "-nr.: ").        
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeAonr(OUTPUT koppladtill,OUTPUT koppladbest).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkNrvar) + " " + koppladtill). 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().   
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(Guru.Konstanter:gbestk).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(koppladbest).
        
        
         IF VisMatris() = 0 THEN. 
         ELSE DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(GetMatrisName() + " " + STRING(VisMatris())).
         END. 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
            
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().   
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(Guru.Konstanter:gomrl + ": ").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(THIS-OBJECT:Omradevar) + "'") NO-ERROR.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:Omradevar) + " " + STRING(Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE)).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Ärendeansv.: ").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
         IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE) + " " + STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE)).
         END.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "D".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().      
      END.      
      
   END METHOD.
   
   METHOD PRIVATE VOID ExcelAnm():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
        
         
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel("A1",THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
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
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
                  THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
                  IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                     THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                     THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(iSenasteMatris))).                  
                  END.              
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:irad),THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),4,"0").
                  
             
               END.
            END.
            THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().            
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName() + " " + STRING(iSenasteMatris)). 
            THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "E".
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
            /*SUMMERA ALLA KOLUMNER*/
            REPEAT:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:SummaV(THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname + THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(matrisrader), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,4,"0",TRUE).
               IF THIS-OBJECT:ArendeVisaControl:VisaExcel:iColnum = THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum THEN LEAVE.
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            END.
            THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().                                     
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
               THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(iSenasteMatris))).                  
               END.              
               THIS-OBJECT:ArendeVisaControl:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:irad),THIS-OBJECT:ArendeVisaControl:VisaExcel:getcolname(THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad),4,"0").
               
            END.
         END.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().            
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Summa " + GetMatrisName()+ " " + STRING(iSenasteMatris)). 
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "E".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
         /*SUMMERA ALLA KOLUMNER*/
         REPEAT:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:SummaV(THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname + THIS-OBJECT:ArendeVisaControl:VisaExcel:Getrad(matrisrader), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,4,"0",TRUE).
            IF THIS-OBJECT:ArendeVisaControl:VisaExcel:iColnum = THIS-OBJECT:ArendeVisaControl:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         END.  
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().                
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppKoder():
      IF VisGruppera(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = FALSE THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING("Egna Koder")).
         END.   
         ELSE DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
         END.
      END.      
   END METHOD.
   
   METHOD PRIVATE VOID ExcelBilagor(vad AS INTEGER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      
      IF vad = 3 THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("MaterielSpecifikation").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Antal").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(GetMatrisName()).
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         END.   
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Pris").
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         END.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Summa").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " BY MATRIS BY ENR". 
         END.
         ELSE IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " BY ENR".
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " WHERE MATRIS = " + STRING(VisMatris()) + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:arendemtrlTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
            END.
            ELSE DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
            END.   
            THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE)).         
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE)).        
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            END.   
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE)).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).            
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 4 THEN DO:
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().                    
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Materieltillägg från beredning").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:FontExcel(THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().         
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Antal").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Pris").
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         END.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut("Summa").
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ArendeVisaControl:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad), THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRad).
         queryvar = "FOR EACH " + THIS-OBJECT:berkalkmtrlttH:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:berkalkmtrltth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF VisSummeraMtrl(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().         
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).         
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
               THIS-OBJECT:ArendeVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE)).            
               THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
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

   METHOD PRIVATE LOGICAL VisSummera(cIN AS CHARACTER,iIN AS INTEGER):
      IF cIN = cSenasteArbKod AND iIN = iSenasteLopNr THEN RETURN TRUE.
      ELSE DO:
         ASSIGN  
         cSenasteArbKod = cIN
         iSenasteLopNr  = iIN.
         RETURN FALSE.
      END.   
   END METHOD.
   METHOD PRIVATE LOGICAL VisGruppera(cIN AS CHARACTER):
      IF cIN = cSenasteArbKod THEN RETURN TRUE.
      ELSE DO:
         ASSIGN  
         cSenasteArbKod = cIN.      
         RETURN FALSE.
      END.   
       
   END METHOD.

   METHOD PRIVATE LOGICAL VisGruppMatris(iIN AS INTEGER):  
      IF iIN = iSenasteMatris THEN RETURN TRUE.
      ELSE DO:
         ASSIGN
         cSenasteArbKod = ""  
         iSenasteLopNr  = 0. 
        
         IF iSenasteMatris = 0 THEN RETURN TRUE.
         ELSE RETURN FALSE.        
      END.       
   END METHOD.
   
   METHOD PRIVATE INTEGER VisMatris():
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
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:ArendeVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ArendeVisaControl:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:ArendeVisaControl:VisaExcel:iRad <= THIS-OBJECT:ArendeVisaControl:VisaExcel:iRadslut:
         invar = THIS-OBJECT:ArendeVisaControl:VisaExcel:GetData().
         IF invar NE "" THEN DO:
            IF THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A" THEN DO:
               ArendeimportTTh:BUFFER-CREATE().
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
               IF vad = 2 THEN DO:
                  klangd = LENGTH(invar).
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
               END.
               ELSE DO:
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
               END.
            END.
            IF THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "B" AND vad = 1 THEN DO:
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "C" THEN DO:
               invar = REPLACE(invar,",",".").
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
            END.      
         END.
         THIS-OBJECT:ArendeVisaControl:VisaExcel:ColRight().
         IF THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "D" THEN DO:
            THIS-OBJECT:ArendeVisaControl:VisaExcel:cColname = "A". 
            THIS-OBJECT:ArendeVisaControl:VisaExcel:Rowdown().
         END.            
      END.  
      THIS-OBJECT:ArendeVisaControl:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:ImportLista().
   END METHOD.
   
   METHOD PUBLIC VOID ImportMallar():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.  
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
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
            ArendeimportTTh:BUFFER-CREATE().
            ArendeimportTTh:BUFFER-COPY(KalkmallKodertth).
            THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.  
                      
            subqH:GET-NEXT(NO-LOCK).  
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:ImportLista().
      KalkmallValdtth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArendeImpMallControl:GridValdaMallar:GuruReopen().
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruAvmarkera().
     /* THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruReopen().*/
     
   END METHOD.
   
   METHOD PUBLIC VOID ImportLista():   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      
      queryvar =  "FOR EACH " + THIS-OBJECT:ArendeimportTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArendeimportTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'") NO-ERROR.
            THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TRIM(ARBKOD) = '"  + TRIM(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND LOPNR = " + THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
            IF THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE > 0 THEN THIS-OBJECT:CurrentMatris = THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
            ELSE DO:
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 1.
               THIS-OBJECT:CurrentMatris = -1.
            END.   
            KoderTTh:BUFFER-CREATE().      
            KoderTTh:BUFFER-COPY(ArendeimportTTh).
            KoderTTh:BUFFER-COPY(LopposterTTh).
            lastKalknumRowid = KoderTTh:ROWID.
            THIS-OBJECT:Root:DatabaseManager:Arende:CreateKodRow(INPUT KoderTTh:RECID, INPUT FALSE).  
                                         
         END.   
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Arende:TidutHmt(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(64)).         
         END. 
         ArendeimportTTh:BUFFER-DELETE() NO-ERROR.  
         qH:GET-NEXT().         
      END.  
      
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:Root:DatabaseManager:Arende:ImportSpara().
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:ArendeControl:GridKalkylKoder:GuruFirstrow().
      THIS-OBJECT:RefreshArbKoder(?,?).
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
      
      IF  Guru.GlobalaVariabler:plusaonr = ? THEN  DO:                                        
         kommando = kommando + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE) + " " + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
      END.
      ELSE DO:
         kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".      
      END.
    
      kommando = TRIM(kommando + excelversend).  
      
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      RETURN kommando.
   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
       
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:tiduth:AVAILABLE THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
      THIS-OBJECT:ArendeEgnaControl:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
   END METHOD.
    
END CLASS.
