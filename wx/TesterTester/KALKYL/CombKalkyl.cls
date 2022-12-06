 
/*------------------------------------------------------------------------
   File        : Kalkyl.cls
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


CLASS Tester.Kalkyl.CombKalkyl INHERITS UserControl: 
   DEFINE PRIVATE PROPERTY OmradeTTh AS HANDLE NO-UNDO
   PRIVATE GET.    PRIVATE SET.
   DEFINE {&STATIC} TEMP-TABLE omrtemp NO-UNDO
   FIELD ANVANDARE AS CHARACTER
   FIELD PANDRA AS LOGICAL
   FIELD AVDELNINGNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD KALKYLSIST AS INTEGER
   FIELD KALKYLINT1 AS INTEGER
   FIELD KALKYLINT2 AS INTEGER
   FIELD TTRECID AS RECID
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.
   DEFINE TEMP-TABLE typtt NO-UNDO
      FIELD ATAT    AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
DEFINE TEMP-TABLE konvtyptt NO-UNDO
   FIELD ATAT    AS CHARACTER
   FIELD TYPKALK AS INTEGER
   FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
   
DEFINE TEMP-TABLE ufpris NO-UNDO
   FIELD UTYP AS INTEGER
   FIELD TYPC AS CHARACTER LABEL "Pris".  
   DEFINE VARIABLE combolista  AS CHARACTER NO-UNDO EXTENT 10.
   DEFINE PRIVATE VARIABLE comboPriserUppf              AS Controls.ComboSimpleLabel                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE KopLabelOmr                  AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE CurrentMatris                AS INTEGER                                                        NO-UNDO.
      
   DEFINE PRIVATE VARIABLE LabelUtf                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelOmr                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelKatalog                 AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelAnm                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   
   DEFINE PRIVATE VARIABLE splitContainerAvtal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgetMtrlBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaVald       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaKoder      AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaArbKoder   AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFaktorerPriser AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFaktPrisBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFelMedd AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerHuv            AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerHuvPerslista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImport AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImpMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImpKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImpBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImpVal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalk           AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkKodoFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkFri AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkArbkoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKopiera AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKopKonv AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrl           AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlEget AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStatus AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerTidlage AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerVolym AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelmedd                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKopiera                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
  
  
   
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFriKalk                  AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridPersonal                 AS Controls.Grid                                                  NO-UNDO.   
   /* Drag & Dropp*/
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudAnmarkning              AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.  
   
   DEFINE PRIVATE VARIABLE HuvudTyp                     AS Controls.ComboSimpleLabel                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudKatalog                 AS Controls.ComboSimple                                           NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudUtfardare               AS Controls.Combo                                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudOmrade                  AS Controls.Combo                                                 NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudKalknr                  AS Controls.FillinInteger                                         NO-UNDO.
   DEFINE PUBLIC  VARIABLE HuvudBenamning               AS Controls.FillinChar                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnsvarig                AS Controls.FillinChar                                            NO-UNDO.
   
   
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
   DEFINE         VARIABLE RibbonToolInfTyp             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolOmrade             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolKat                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolMatris             AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFriGenerera        AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKoderAnm           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /* visning */
   
   DEFINE PUBLIC  VARIABLE RibbonToolGenerera           AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUt            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUtBildval     AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisHuvud           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisAnmarkning      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisUtforlig        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaFaktorer    AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisEgnaPriser      AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
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
   
   /* Material */
   DEFINE PUBLIC  VARIABLE RibbonToolLev                AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnr                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE         VARIABLE RibbonToolSok                AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   
       /* ttHandles ifrån stuff */
   DEFINE         VARIABLE KalkDummy                    AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
     
   /* Kalkyl */
   
   DEFINE PRIVATE VARIABLE KalkylLoaded                 AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   DEFINE PRIVATE VARIABLE PassedOnMatris               AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   DEFINE PRIVATE VARIABLE NyKalkyl                     AS LOGICAL                                                        NO-UNDO INITIAL FALSE.
   DEFINE         VARIABLE cSenasteArbKod               AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteLopNr                AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE iSenasteMatris               AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE cSenasteMtrl                 AS CHARACTER                                                      NO-UNDO.
   DEFINE         VARIABLE iSenasteMtrlMatris           AS INTEGER                                                        NO-UNDO.
   DEFINE         VARIABLE ExelKommando                 AS CHARACTER                                                      NO-UNDO.   
   DEFINE         VARIABLE WasExcelShown                AS LOGICAL                                                        INITIAL FALSE NO-UNDO.
   
   
   DEFINE PUBLIC PROPERTY KalkNrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      

   CONSTRUCTOR PUBLIC CombKalkyl ( INPUT r AS Guru.Root ):
      SUPER ().
      THIS-OBJECT:Root = r.
      
      InitializeComponent().
      
      
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC CombKalkyl ( ):

   END DESTRUCTOR.

















	







	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID HuvudOmrade_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:SparaComboV(HuvudOmrade).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID HuvudUtfardare_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:SparaComboV(HuvudUtfardare).
		RETURN.

	END METHOD.

	

	
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC LOGICAL NewKalkyl():
      /* skapa kalkyl i databas */
      /* Hitta rätt katalog*/
      ApplyLoadedKalkyl().
   END METHOD.
   
   METHOD PUBLIC LOGICAL NewKalkyl(INPUT kalkdatah AS HANDLE):
   END METHOD.
   
   /*Robins start*/
   METHOD PUBLIC VOID StangaNy(INPUT sender AS System.Object , INPUT e AS System.Windows.Forms.FormClosingEventArgs):
   END METHOD.
   /*Robins slut*/
   
   METHOD PRIVATE LOGICAL SaveHuvud():
      RETURN TRUE. 
   END METHOD.
   
   METHOD PUBLIC LOGICAL ApplyCreatedKalkyl():
      IF THIS-OBJECT:SaveHuvud() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      THIS-OBJECT:NyKalkyl = FALSE.
      RETURN TRUE.
   END METHOD.
   
   
   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT KalkylimportTTinh AS HANDLE,INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
       RETURN TRUE.
   END METHOD.   
   METHOD PUBLIC LOGICAL LoadKalkyl(INPUT nr AS INTEGER, INPUT omr AS CHARACTER, INPUT nydef AS LOGICAL):
      DEFINE VARIABLE defToolValue AS CHARACTER NO-UNDO.
      RETURN TRUE.
   END METHOD.

   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:OmradeTTh = TEMP-TABLE omrtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
      EMPTY TEMP-TABLE omrtemp NO-ERROR. 
      CREATE omrtemp.
      ASSIGN 
      omrtemp.OMRADE = "1"
      omrtemp.namn  = "ELPOOL".
      CREATE omrtemp.
      ASSIGN 
      omrtemp.OMRADE = "2"
      omrtemp.namn  = "PROGRESS".
      CREATE omrtemp.
      ASSIGN 
      omrtemp.OMRADE = "3"
      omrtemp.namn  = "OPEN EDGE".
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:GomaRibbon().
      THIS-OBJECT:ForetagAnpass().
      THIS-OBJECT:TabManager:Tabs["Status"]:VISIBLE   = FALSE.
       /*Robin start*/
      THIS-OBJECT:Root:WindowManager:Wnd:FormClosing:Subscribe(THIS-OBJECT:StangaNy).
      /*Robin slut*/
   END METHOD.
   /* Fullständiggör formen när kalkyl laddats*/
   METHOD PUBLIC VOID ApplyLoadedKalkyl():
      DEFINE VARIABLE imatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxmatris AS INTEGER NO-UNDO.
      DEFINE VARIABLE matrisname AS CHARACTER NO-UNDO.
     
      /* Sätter Combos */ 
      /*GÖR ÄVEN REF PÅ ARBKODER*/
      THIS-OBJECT:HuvudTyp:GuruCombo:VALUE = 2.
      OmradeTTh:FIND-FIRST()  NO-ERROR.
      THIS-OBJECT:HuvudOmrade:VALUE = THIS-OBJECT:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      
      /* Ribbongrejjer börjar här! */
      /* Uppdatera matris*/
      
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      
      THIS-OBJECT:RefreshMarkning().
      
   END METHOD.
   
   
   
   /* Skapar alla griddar */
   METHOD PUBLIC VOID CreateGrids():
      /* Huvud */
      
   END METHOD.

   METHOD PUBLIC VOID CreateGridFunctions():
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      DEFINE VARIABLE personallist  AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE personalPKOD  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalFNAMN AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalENAMN AS Controls.GridRubrik      NO-UNDO. 
      personallist = NEW Controls.GridRubrikLista().
      /*
      personallist:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Global:PersonalTTh.
      */
      personallist:ttBufferHandle = Guru.Konstanter:PersonalTTh.            
      personalPKOD = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      personallist:addRubrik(personalPKOD).
      personalFNAMN = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      personallist:addRubrik(personalFNAMN).
      personalENAMN = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      personallist:addRubrik(personalENAMN).
      THIS-OBJECT:GridPersonal:GuruInitGrid(personallist).  
      THIS-OBJECT:GridPersonal:Text = "ansvarig".
      THIS-OBJECT:GridPersonal:Text = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:GridPersonal:Text,TRUE).
           /*egna event*/
      THIS-OBJECT:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
   END METHOD.

   METHOD PUBLIC VOID CreateGridFrekvens():
   END METHOD.

   METHOD PUBLIC VOID CreateGridFriKoder():
   END METHOD.
   
   /* Egna koder*/
   METHOD PUBLIC VOID CreateGridEgnaKoder():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
   END METHOD.

   /* Materiel */
   METHOD PUBLIC VOID CreateGridMateriel():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
            
   END METHOD.

   /* Faktorer och priser*/
   METHOD PUBLIC VOID CreateGridFaktorer():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallarKoder():
   END METHOD.

   METHOD PUBLIC VOID CreateGridMallar():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgnaPriser():
   END METHOD.
   METHOD PUBLIC VOID CreateGridTidlage():
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridStatus():
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridVolymBer():
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFelMedd():
   END METHOD.
   
   
   /* Skapar alla Combos */
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboTyp().
      THIS-OBJECT:CreatecomboPriserUppf().
      DEBUGGER:SET-BREAK().
      THIS-OBJECT:CreateComboOmrade().
      THIS-OBJECT:KopCreateComboOmrade().
      THIS-OBJECT:CreateComboKatalog().
      THIS-OBJECT:CreateComboKopKatalog().
      THIS-OBJECT:CreateComboUtfardare().
      
   END METHOD. 
   /*v11.2 fix*/
   METHOD PRIVATE VOID SparaComboV(INPUT incombo AS Controls.Combo):
      RETURN.
   END METHOD.
   /*v11.2 fix*/
   METHOD PRIVATE VOID GetComboV(INPUT incombo AS Controls.Combo):
      RETURN.
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboPriserUppf():
      
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
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN DO: 
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
      THIS-OBJECT:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
   END METHOD.
   METHOD PUBLIC VOID CreateComboKonvTyp():
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = OmradeTTh.
      
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = "område".
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).
        
      THIS-OBJECT:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
      
      
      
   END METHOD.
   METHOD PUBLIC VOID KopCreateComboOmrade():
      
   END METHOD.

   METHOD PUBLIC VOID CreateComboKatalog():
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboKopKatalog():
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboUtfardare():
      
   END METHOD.
   METHOD PUBLIC VOID CreateComboAvtal():
      
   END METHOD.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerEgenKod():
      
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      
   END METHOD.

   METHOD PRIVATE VOID ButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
   END METHOD.
   METHOD PRIVATE VOID ButtonOvermark_Click(INPUT antalvar AS DECIMAL):
   END METHOD.





   
   METHOD PUBLIC VOID FilterUnderKoder():
   END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFriKalk_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudTyp_GuruValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      RETURN.

   END METHOD.

   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:CreateForm().
      
      RETURN TRUE.
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
  
   END METHOD.

   METHOD PUBLIC VOID UpdateMatris():
   END METHOD.
   METHOD PUBLIC VOID GomaRibbon():
         
   END METHOD.   
   METHOD PUBLIC VOID CreateRibbonVS():
      
   END METHOD. 
   METHOD PUBLIC VOID CreateRibbon():
         THIS-OBJECT:LoadRibbonLevs().
    
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
      THIS-OBJECT:GridFriKalk:GuruReopen().
   END METHOD.
   
   METHOD PUBLIC VOID FrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
    
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID FriKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID GruppMatrisChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtGrid(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning():
   END METHOD.
   
   METHOD PUBLIC CHARACTER GetVisaMarkning():
   END METHOD.
    
   METHOD PUBLIC VOID RefreshArbKoder(s AS System.Object, e AS System.EventArgs):
   END METHOD.
   
   METHOD PUBLIC VOID UpdateRibbonShow():
   END METHOD.
   
   METHOD PUBLIC VOID LoadRibbonLevs():

   END METHOD.
   
   /* Ribbon events*/
   METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
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
   END METHOD.
   
   METHOD PUBLIC VOID RibbonUpdateUppfPriser(sender AS System.Object, e AS System.EventArgs):
   END METHOD.  
    
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
      appearance66 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
      appearance67 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
      appearance68 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
      appearance69 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
      appearance70 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
      appearance71 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
      appearance72 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
      appearance73 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
      appearance74 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
      appearance75 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
      appearance76 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance115 AS Infragistics.Win.Appearance NO-UNDO.
      appearance115 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance116 AS Infragistics.Win.Appearance NO-UNDO.
      appearance116 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance117 AS Infragistics.Win.Appearance NO-UNDO.
      appearance117 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance118 AS Infragistics.Win.Appearance NO-UNDO.
      appearance118 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance119 AS Infragistics.Win.Appearance NO-UNDO.
      appearance119 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance120 AS Infragistics.Win.Appearance NO-UNDO.
      appearance120 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance121 AS Infragistics.Win.Appearance NO-UNDO.
      appearance121 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance122 AS Infragistics.Win.Appearance NO-UNDO.
      appearance122 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance123 AS Infragistics.Win.Appearance NO-UNDO.
      appearance123 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance124 AS Infragistics.Win.Appearance NO-UNDO.
      appearance124 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance125 AS Infragistics.Win.Appearance NO-UNDO.
      appearance125 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance126 AS Infragistics.Win.Appearance NO-UNDO.
      appearance126 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Tester.Kalkyl.CombKalkyl").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
      appearance79 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance80 AS Infragistics.Win.Appearance NO-UNDO.
      appearance80 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance81 AS Infragistics.Win.Appearance NO-UNDO.
      appearance81 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance82 AS Infragistics.Win.Appearance NO-UNDO.
      appearance82 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance83 AS Infragistics.Win.Appearance NO-UNDO.
      appearance83 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance84 AS Infragistics.Win.Appearance NO-UNDO.
      appearance84 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance85 AS Infragistics.Win.Appearance NO-UNDO.
      appearance85 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance86 AS Infragistics.Win.Appearance NO-UNDO.
      appearance86 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance87 AS Infragistics.Win.Appearance NO-UNDO.
      appearance87 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance88 AS Infragistics.Win.Appearance NO-UNDO.
      appearance88 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance89 AS Infragistics.Win.Appearance NO-UNDO.
      appearance89 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance90 AS Infragistics.Win.Appearance NO-UNDO.
      appearance90 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance128 AS Infragistics.Win.Appearance NO-UNDO.
      appearance128 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance127 AS Infragistics.Win.Appearance NO-UNDO.
      appearance127 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance130 AS Infragistics.Win.Appearance NO-UNDO.
      appearance130 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance129 AS Infragistics.Win.Appearance NO-UNDO.
      appearance129 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance132 AS Infragistics.Win.Appearance NO-UNDO.
      appearance132 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance131 AS Infragistics.Win.Appearance NO-UNDO.
      appearance131 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance134 AS Infragistics.Win.Appearance NO-UNDO.
      appearance134 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance133 AS Infragistics.Win.Appearance NO-UNDO.
      appearance133 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance136 AS Infragistics.Win.Appearance NO-UNDO.
      appearance136 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance135 AS Infragistics.Win.Appearance NO-UNDO.
      appearance135 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance138 AS Infragistics.Win.Appearance NO-UNDO.
      appearance138 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance137 AS Infragistics.Win.Appearance NO-UNDO.
      appearance137 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance140 AS Infragistics.Win.Appearance NO-UNDO.
      appearance140 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance139 AS Infragistics.Win.Appearance NO-UNDO.
      appearance139 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance142 AS Infragistics.Win.Appearance NO-UNDO.
      appearance142 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance141 AS Infragistics.Win.Appearance NO-UNDO.
      appearance141 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance144 AS Infragistics.Win.Appearance NO-UNDO.
      appearance144 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance143 AS Infragistics.Win.Appearance NO-UNDO.
      appearance143 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance161 AS Infragistics.Win.Appearance NO-UNDO.
      appearance161 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance162 AS Infragistics.Win.Appearance NO-UNDO.
      appearance162 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance146 AS Infragistics.Win.Appearance NO-UNDO.
      appearance146 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance145 AS Infragistics.Win.Appearance NO-UNDO.
      appearance145 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance148 AS Infragistics.Win.Appearance NO-UNDO.
      appearance148 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance147 AS Infragistics.Win.Appearance NO-UNDO.
      appearance147 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance163 AS Infragistics.Win.Appearance NO-UNDO.
      appearance163 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerHuv = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:comboPriserUppf = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:HuvudKalknr = NEW Controls.FillinInteger().
      THIS-OBJECT:HuvudUtfardare = NEW Controls.Combo().
      THIS-OBJECT:HuvudAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:LabelAnm = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudAnsvarig = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudKatalog = NEW Controls.ComboSimple().
      THIS-OBJECT:LabelOmr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudOmrade = NEW Controls.Combo().
      THIS-OBJECT:LabelUtf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:LabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:splitContainerHuvPerslista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridPersonal = NEW Controls.Grid().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkArbkoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkFri = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKodoFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:GridFriKalk = NEW Controls.Grid().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerEgnaBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaArbKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaVald = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabFaktorer = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerFaktPrisBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerFaktorerPriser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerMtrlValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerMtrlEget = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgetMtrlBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerImport = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpVal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabAvtalskalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerAvtal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabKopiera = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKopiera = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKopKonv = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KopLabelOmr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerTidlage = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabFelmedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerFelMedd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabVolymber = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerVolym = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerStatus = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:TabKalkylHuvud:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:SuspendLayout().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuvPerslista:SuspendLayout().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkyl:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBild:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabEgna:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:SuspendLayout().
      THIS-OBJECT:TabFaktorer:SuspendLayout().
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFaktPrisBild:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:SuspendLayout().
      THIS-OBJECT:TabMateriel:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:SuspendLayout().
      THIS-OBJECT:splitContainerEgetMtrlBild:SuspendLayout().
      THIS-OBJECT:TabImportMallar:SuspendLayout().
      THIS-OBJECT:splitContainerImport:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImport:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImport:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:SuspendLayout().
      THIS-OBJECT:splitContainerImpVal:SuspendLayout().
      THIS-OBJECT:splitContainerImpBild:SuspendLayout().
      THIS-OBJECT:TabAvtalskalkyl:SuspendLayout().
      THIS-OBJECT:splitContainerAvtal:SuspendLayout().
      THIS-OBJECT:TabKopiera:SuspendLayout().
      THIS-OBJECT:splitContainerKopiera:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopiera:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonv:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
      THIS-OBJECT:splitContainerTidlage:SuspendLayout().
      THIS-OBJECT:TabFelmedd:SuspendLayout().
      THIS-OBJECT:splitContainerFelMedd:SuspendLayout().
      THIS-OBJECT:TabVolymber:SuspendLayout().
      THIS-OBJECT:splitContainerVolym:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
      THIS-OBJECT:splitContainerStatus:SuspendLayout().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabKalkylHuvud */
      /*  */
      THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:splitContainerHuv).
      THIS-OBJECT:TabKalkylHuvud:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKalkylHuvud:Name = "TabKalkylHuvud".
      THIS-OBJECT:TabKalkylHuvud:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerHuv */
      /*  */
      THIS-OBJECT:splitContainerHuv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerHuv:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerHuv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerHuv:Name = "splitContainerHuv".
      /*  */
      /* splitContainerHuv.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:comboPriserUppf).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudKalknr).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudUtfardare).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudAnmarkning).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelAnm).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudAnsvarig).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudKatalog).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelOmr).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudBenamning).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudOmrade).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelUtf).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudTyp).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelKatalog).
      /*  */
      /* splitContainerHuv.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel2:Controls:Add(THIS-OBJECT:splitContainerHuvPerslista).
      THIS-OBJECT:splitContainerHuv:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerHuv:SplitterDistance = 383.
      THIS-OBJECT:splitContainerHuv:TabIndex = 43.
      /*  */
      /* comboPriserUppf */
      /*  */
      THIS-OBJECT:comboPriserUppf:Location = NEW System.Drawing.Point(13, 247).
      THIS-OBJECT:comboPriserUppf:Name = "comboPriserUppf".
      THIS-OBJECT:comboPriserUppf:Size = NEW System.Drawing.Size(360, 43).
      THIS-OBJECT:comboPriserUppf:TabIndex = 43.
      /*  */
      /* HuvudKalknr */
      /*  */
      THIS-OBJECT:HuvudKalknr:Location = NEW System.Drawing.Point(13, 6).
      THIS-OBJECT:HuvudKalknr:Name = "HuvudKalknr".
      THIS-OBJECT:HuvudKalknr:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudKalknr:TabIndex = 39.
      /*  */
      /* HuvudUtfardare */
      /*  */
      THIS-OBJECT:HuvudUtfardare:CheckedListSettings:CheckStateMember = "".
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Appearance = appearance50.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxRowScrollRegions = 1.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudUtfardare:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudUtfardare:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudUtfardare:Location = NEW System.Drawing.Point(97, 171).
      THIS-OBJECT:HuvudUtfardare:Name = "HuvudUtfardare".
      THIS-OBJECT:HuvudUtfardare:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudUtfardare:TabIndex = 41.
      THIS-OBJECT:HuvudUtfardare:ValueChanged:Subscribe(THIS-OBJECT:HuvudUtfardare_ValueChanged).
      /*  */
      /* HuvudAnmarkning */
      /*  */
      THIS-OBJECT:HuvudAnmarkning:Location = NEW System.Drawing.Point(13, 325).
      THIS-OBJECT:HuvudAnmarkning:Multiline = TRUE.
      THIS-OBJECT:HuvudAnmarkning:Name = "HuvudAnmarkning".
      THIS-OBJECT:HuvudAnmarkning:Size = NEW System.Drawing.Size(361, 108).
      THIS-OBJECT:HuvudAnmarkning:TabIndex = 32.
      THIS-OBJECT:HuvudAnmarkning:Text = " ".
      /*  */
      /* LabelAnm */
      /*  */
      THIS-OBJECT:LabelAnm:Location = NEW System.Drawing.Point(13, 296).
      THIS-OBJECT:LabelAnm:Name = "LabelAnm".
      THIS-OBJECT:LabelAnm:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelAnm:TabIndex = 33.
      THIS-OBJECT:LabelAnm:Text = "Anmärkning".
      /*  */
      /* HuvudAnsvarig */
      /*  */
      THIS-OBJECT:HuvudAnsvarig:Location = NEW System.Drawing.Point(13, 210).
      THIS-OBJECT:HuvudAnsvarig:Name = "HuvudAnsvarig".
      THIS-OBJECT:HuvudAnsvarig:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudAnsvarig:TabIndex = 40.
      /*  */
      /* HuvudKatalog */
      /*  */
      THIS-OBJECT:HuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:HuvudKatalog:Location = NEW System.Drawing.Point(97, 104).
      THIS-OBJECT:HuvudKatalog:Name = "HuvudKatalog".
      THIS-OBJECT:HuvudKatalog:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:HuvudKatalog:TabIndex = 42.
      /*  */
      /* LabelOmr */
      /*  */
      appearance65:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelOmr:Appearance = appearance65.
      THIS-OBJECT:LabelOmr:Location = NEW System.Drawing.Point(13, 40).
      THIS-OBJECT:LabelOmr:Name = "LabelOmr".
      THIS-OBJECT:LabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelOmr:TabIndex = 38.
      /*  */
      /* HuvudBenamning */
      /*  */
      THIS-OBJECT:HuvudBenamning:Location = NEW System.Drawing.Point(13, 137).
      THIS-OBJECT:HuvudBenamning:Name = "HuvudBenamning".
      THIS-OBJECT:HuvudBenamning:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudBenamning:TabIndex = 35.
      THIS-OBJECT:HuvudBenamning:Tag = "".
      /*  */
      /* HuvudOmrade */
      /*  */
      THIS-OBJECT:HuvudOmrade:CheckedListSettings:CheckStateMember = "".
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Appearance = appearance115.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance116:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance116:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance116:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance116:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance116.
      appearance117:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance117.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance118:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance118:BackColor2 = System.Drawing.SystemColors:Control.
      appearance118:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance118:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance118.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance119:BackColor = System.Drawing.SystemColors:Window.
      appearance119:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance119.
      appearance120:BackColor = System.Drawing.SystemColors:Highlight.
      appearance120:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance120.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance121.
      appearance122:BorderColor = System.Drawing.Color:Silver.
      appearance122:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellAppearance = appearance122.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance123:BackColor = System.Drawing.SystemColors:Control.
      appearance123:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance123:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance123:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance123:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance123.
      appearance124:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance124.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance125:BackColor = System.Drawing.SystemColors:Window.
      appearance125:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowAppearance = appearance125.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance126.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudOmrade:Location = NEW System.Drawing.Point(97, 40).
      THIS-OBJECT:HuvudOmrade:Name = "HuvudOmrade".
      THIS-OBJECT:HuvudOmrade:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudOmrade:TabIndex = 37.
      THIS-OBJECT:HuvudOmrade:ValueChanged:Subscribe(THIS-OBJECT:HuvudOmrade_ValueChanged).
      /*  */
      /* LabelUtf */
      /*  */
      THIS-OBJECT:LabelUtf:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:LabelUtf:Location = NEW System.Drawing.Point(13, 171).
      THIS-OBJECT:LabelUtf:Name = "LabelUtf".
      THIS-OBJECT:LabelUtf:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:LabelUtf:TabIndex = 30.
      THIS-OBJECT:LabelUtf:Text = "Utfärdare".
      /*  */
      /* HuvudTyp */
      /*  */
      THIS-OBJECT:HuvudTyp:Location = NEW System.Drawing.Point(7, 70).
      THIS-OBJECT:HuvudTyp:Name = "HuvudTyp".
      THIS-OBJECT:HuvudTyp:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudTyp:TabIndex = 36.
      THIS-OBJECT:HuvudTyp:GuruValueChanged:Subscribe(THIS-OBJECT:HuvudTyp_GuruValueChanged).
      /*  */
      /* LabelKatalog */
      /*  */
      appearance54:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelKatalog:Appearance = appearance54.
      THIS-OBJECT:LabelKatalog:Location = NEW System.Drawing.Point(13, 108).
      THIS-OBJECT:LabelKatalog:Name = "LabelKatalog".
      THIS-OBJECT:LabelKatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelKatalog:TabIndex = 31.
      THIS-OBJECT:LabelKatalog:Text = "Katalog".
      /*  */
      /* splitContainerHuvPerslista */
      /*  */
      THIS-OBJECT:splitContainerHuvPerslista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerHuvPerslista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerHuvPerslista:Name = "splitContainerHuvPerslista".
      /*  */
      /* splitContainerHuvPerslista.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:Controls:Add(THIS-OBJECT:GridPersonal).
      /*  */
      /* splitContainerHuvPerslista.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHuvPerslista:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerHuvPerslista.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerHuvPerslista:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerHuvPerslista:Size = NEW System.Drawing.Size(758, 553).
      THIS-OBJECT:splitContainerHuvPerslista:SplitterDistance = 390.
      THIS-OBJECT:splitContainerHuvPerslista:TabIndex = 35.
      /*  */
      /* GridPersonal */
      /*  */
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPersonal:DisplayLayout:Appearance = appearance79.
      THIS-OBJECT:GridPersonal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPersonal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance80:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance80:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance80:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:Appearance = appearance80.
      appearance81:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance81.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance82:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance82:BackColor2 = System.Drawing.SystemColors:Control.
      appearance82:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance82:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:PromptAppearance = appearance82.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      appearance83:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveCellAppearance = appearance83.
      appearance84:BackColor = System.Drawing.SystemColors:Highlight.
      appearance84:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveRowAppearance = appearance84.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CardAreaAppearance = appearance85.
      appearance86:BorderColor = System.Drawing.Color:Silver.
      appearance86:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellAppearance = appearance86.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellPadding = 0.
      appearance87:BackColor = System.Drawing.SystemColors:Control.
      appearance87:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance87:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance87:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance87:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:GroupByRowAppearance = appearance87.
      appearance88:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderAppearance = appearance88.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowAppearance = appearance89.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:TemplateAddRowAppearance = appearance90.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPersonal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPersonal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridPersonal:GuruContext = ?.
      THIS-OBJECT:GridPersonal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPersonal:Name = "GridPersonal".
      THIS-OBJECT:GridPersonal:Size = NEW System.Drawing.Size(390, 553).
      THIS-OBJECT:GridPersonal:TabIndex = 34.
      THIS-OBJECT:GridPersonal:Text = "grid1".
      /*  */
      /* TabKalkyl */
      /*  */
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:splitContainerKalk).
      THIS-OBJECT:TabKalkyl:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabKalkyl:Name = "TabKalkyl".
      THIS-OBJECT:TabKalkyl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerKalk */
      /*  */
      THIS-OBJECT:splitContainerKalk:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalk:Name = "splitContainerKalk".
      /*  */
      /* splitContainerKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkArbkoder).
      /*  */
      /* splitContainerKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkFri).
      THIS-OBJECT:splitContainerKalk:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerKalk:SplitterDistance = 500.
      THIS-OBJECT:splitContainerKalk:TabIndex = 11.
      /*  */
      /* splitContainerKalkArbkoder */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkArbkoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkArbkoder:Name = "splitContainerKalkArbkoder".
      THIS-OBJECT:splitContainerKalkArbkoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkArbkoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkLopnr).
      THIS-OBJECT:splitContainerKalkArbkoder:Size = NEW System.Drawing.Size(498, 551).
      THIS-OBJECT:splitContainerKalkArbkoder:SplitterDistance = 252.
      THIS-OBJECT:splitContainerKalkArbkoder:TabIndex = 0.
      /*  */
      /* splitContainerKalkLopnr */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKalkLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkLopnr:Name = "splitContainerKalkLopnr".
      THIS-OBJECT:splitContainerKalkLopnr:Size = NEW System.Drawing.Size(498, 295).
      THIS-OBJECT:splitContainerKalkLopnr:SplitterDistance = 463.
      THIS-OBJECT:splitContainerKalkLopnr:TabIndex = 0.
      /*  */
      /* splitContainerKalkFri */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkFri:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkFri:Name = "splitContainerKalkFri".
      /*  */
      /* splitContainerKalkFri.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkKodoFrek).
      /*  */
      /* splitContainerKalkFri.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Panel2:Controls:Add(THIS-OBJECT:GridFriKalk).
      THIS-OBJECT:splitContainerKalkFri:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerKalkFri:Size = NEW System.Drawing.Size(639, 551).
      THIS-OBJECT:splitContainerKalkFri:SplitterDistance = 400.
      THIS-OBJECT:splitContainerKalkFri:TabIndex = 4.
      /*  */
      /* splitContainerKalkKodoFrek */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKodoFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKodoFrek:Name = "splitContainerKalkKodoFrek".
      /*  */
      /* splitContainerKalkKodoFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkBild).
      THIS-OBJECT:splitContainerKalkKodoFrek:Size = NEW System.Drawing.Size(639, 551).
      THIS-OBJECT:splitContainerKalkKodoFrek:SplitterDistance = 400.
      THIS-OBJECT:splitContainerKalkKodoFrek:TabIndex = 36.
      /*  */
      /* splitContainerKalkBild */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkBild:Name = "splitContainerKalkBild".
      /*  */
      /* splitContainerKalkBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      /*  */
      /* splitContainerKalkBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKalkBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerKalkBild:Size = NEW System.Drawing.Size(235, 551).
      THIS-OBJECT:splitContainerKalkBild:SplitterDistance = 100.
      THIS-OBJECT:splitContainerKalkBild:TabIndex = 0.
      /*  */
      /* splitContainerKalkKomAnm */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKomAnm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKomAnm:Name = "splitContainerKalkKomAnm".
      THIS-OBJECT:splitContainerKalkKomAnm:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkKomAnm.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(100, 551).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 37.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:ReadOnly = TRUE.
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(100, 510).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 34.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
      /*  */
      /* GridFriKalk */
      /*  */
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      appearance8:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Appearance = appearance8.
      THIS-OBJECT:GridFriKalk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFriKalk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance5:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:Appearance = appearance5.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance7:BackColor2 = System.Drawing.SystemColors:Control.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance11:BackColor = System.Drawing.SystemColors:Highlight.
      appearance11:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveRowAppearance = appearance11.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CardAreaAppearance = appearance10.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellPadding = 0.
      appearance13:BackColor = System.Drawing.SystemColors:Control.
      appearance13:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance13:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance13:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance13:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:GroupByRowAppearance = appearance13.
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderAppearance = appearance15.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowAppearance = appearance14.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFriKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFriKalk:GuruContext = ?.
      THIS-OBJECT:GridFriKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFriKalk:Name = "GridFriKalk".
      THIS-OBJECT:GridFriKalk:Size = NEW System.Drawing.Size(96, 100).
      THIS-OBJECT:GridFriKalk:TabIndex = 0.
      THIS-OBJECT:GridFriKalk:Text = "grid1".
      THIS-OBJECT:GridFriKalk:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFriKalk_AfterCellUpdate).
      /*  */
      /* TabEgna */
      /*  */
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:splitContainerEgnaBild).
      THIS-OBJECT:TabEgna:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabEgna:Name = "TabEgna".
      THIS-OBJECT:TabEgna:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerEgnaBild */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaBild:Name = "splitContainerEgnaBild".
      /*  */
      /* splitContainerEgnaBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaKoder).
      /*  */
      /* splitContainerEgnaBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerEgnaBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerEgnaBild:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerEgnaBild:SplitterDistance = 800.
      THIS-OBJECT:splitContainerEgnaBild:TabIndex = 19.
      /*  */
      /* splitContainerEgnaKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaKoder:Name = "splitContainerEgnaKoder".
      THIS-OBJECT:splitContainerEgnaKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaArbKoder).
      /*  */
      /* splitContainerEgnaKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaValj).
      THIS-OBJECT:splitContainerEgnaKoder:Size = NEW System.Drawing.Size(800, 553).
      THIS-OBJECT:splitContainerEgnaKoder:SplitterDistance = 200.
      THIS-OBJECT:splitContainerEgnaKoder:TabIndex = 18.
      /*  */
      /* splitContainerEgnaArbKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaArbKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaArbKoder:Name = "splitContainerEgnaArbKoder".
      THIS-OBJECT:splitContainerEgnaArbKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      THIS-OBJECT:splitContainerEgnaArbKoder:Size = NEW System.Drawing.Size(800, 200).
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterDistance = 25.
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerEgnaArbKoder:TabIndex = 14.
      /*  */
      /* splitContainerEgnaValj */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerEgnaValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaValj:Name = "splitContainerEgnaValj".
      /*  */
      /* splitContainerEgnaValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaVald).
      THIS-OBJECT:splitContainerEgnaValj:Size = NEW System.Drawing.Size(800, 349).
      THIS-OBJECT:splitContainerEgnaValj:SplitterDistance = 266.
      THIS-OBJECT:splitContainerEgnaValj:TabIndex = 0.
      /*  */
      /* splitContainerEgnaVald */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaVald:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaVald:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaVald:Name = "splitContainerEgnaVald".
      THIS-OBJECT:splitContainerEgnaVald:Size = NEW System.Drawing.Size(528, 347).
      THIS-OBJECT:splitContainerEgnaVald:SplitterDistance = 29.
      THIS-OBJECT:splitContainerEgnaVald:TabIndex = 0.
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabFaktorer */
      /*  */
      THIS-OBJECT:TabFaktorer:Controls:Add(THIS-OBJECT:splitContainerFaktPrisBild).
      THIS-OBJECT:TabFaktorer:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFaktorer:Name = "TabFaktorer".
      THIS-OBJECT:TabFaktorer:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerFaktPrisBild */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFaktPrisBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFaktPrisBild:Name = "splitContainerFaktPrisBild".
      /*  */
      /* splitContainerFaktPrisBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerFaktorerPriser).
      /*  */
      /* splitContainerFaktPrisBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerFaktPrisBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerFaktPrisBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerFaktPrisBild:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerFaktPrisBild:SplitterDistance = 800.
      THIS-OBJECT:splitContainerFaktPrisBild:TabIndex = 3.
      /*  */
      /* splitContainerFaktorerPriser */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFaktorerPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFaktorerPriser:Name = "splitContainerFaktorerPriser".
      THIS-OBJECT:splitContainerFaktorerPriser:Size = NEW System.Drawing.Size(800, 553).
      THIS-OBJECT:splitContainerFaktorerPriser:SplitterDistance = 417.
      THIS-OBJECT:splitContainerFaktorerPriser:TabIndex = 2.
      /*  */
      /* TabMateriel */
      /*  */
      THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:splitContainerMtrl).
      THIS-OBJECT:TabMateriel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMateriel:Name = "TabMateriel".
      THIS-OBJECT:TabMateriel:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerMtrl */
      /*  */
      THIS-OBJECT:splitContainerMtrl:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrl:Name = "splitContainerMtrl".
      /*  */
      /* splitContainerMtrl.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel1:Controls:Add(THIS-OBJECT:splitContainerMtrlValj).
      /*  */
      /* splitContainerMtrl.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel2:Controls:Add(THIS-OBJECT:splitContainerMtrlEget).
      THIS-OBJECT:splitContainerMtrl:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerMtrl:SplitterDistance = 520.
      THIS-OBJECT:splitContainerMtrl:TabIndex = 19.
      /*  */
      /* splitContainerMtrlValj */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlValj:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerMtrlValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlValj:Name = "splitContainerMtrlValj".
      THIS-OBJECT:splitContainerMtrlValj:Size = NEW System.Drawing.Size(518, 551).
      THIS-OBJECT:splitContainerMtrlValj:SplitterDistance = 489.
      THIS-OBJECT:splitContainerMtrlValj:TabIndex = 1.
      /*  */
      /* splitContainerMtrlEget */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlEget:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlEget:Name = "splitContainerMtrlEget".
      /*  */
      /* splitContainerMtrlEget.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerMtrlEget.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerMtrlEget:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerMtrlEget:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgetMtrlBild).
      THIS-OBJECT:splitContainerMtrlEget:Size = NEW System.Drawing.Size(619, 551).
      THIS-OBJECT:splitContainerMtrlEget:SplitterDistance = 500.
      THIS-OBJECT:splitContainerMtrlEget:TabIndex = 3.
      /*  */
      /* splitContainerEgetMtrlBild */
      /*  */
      THIS-OBJECT:splitContainerEgetMtrlBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgetMtrlBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgetMtrlBild:Name = "splitContainerEgetMtrlBild".
      THIS-OBJECT:splitContainerEgetMtrlBild:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgetMtrlBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerEgetMtrlBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerEgetMtrlBild:Size = NEW System.Drawing.Size(115, 551).
      THIS-OBJECT:splitContainerEgetMtrlBild:SplitterDistance = 25.
      THIS-OBJECT:splitContainerEgetMtrlBild:TabIndex = 4.
      /*  */
      /* TabImportMallar */
      /*  */
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:splitContainerImport).
      THIS-OBJECT:TabImportMallar:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabImportMallar:Name = "TabImportMallar".
      THIS-OBJECT:TabImportMallar:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerImport */
      /*  */
      THIS-OBJECT:splitContainerImport:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:splitContainerImport:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImport:Name = "splitContainerImport".
      /*  */
      /* splitContainerImport.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImport:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpMallar).
      /*  */
      /* splitContainerImport.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImport:Panel2:Controls:Add(THIS-OBJECT:splitContainerImpBild).
      THIS-OBJECT:splitContainerImport:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerImport:SplitterDistance = 700.
      THIS-OBJECT:splitContainerImport:TabIndex = 17.
      /*  */
      /* splitContainerImpMallar */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpMallar:Name = "splitContainerImpMallar".
      /*  */
      /* splitContainerImpMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpKoder).
      THIS-OBJECT:splitContainerImpMallar:Size = NEW System.Drawing.Size(700, 553).
      THIS-OBJECT:splitContainerImpMallar:SplitterDistance = 350.
      THIS-OBJECT:splitContainerImpMallar:TabIndex = 0.
      /*  */
      /* splitContainerImpKoder */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpKoder:Name = "splitContainerImpKoder".
      THIS-OBJECT:splitContainerImpKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerImpKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpVal).
      THIS-OBJECT:splitContainerImpKoder:Size = NEW System.Drawing.Size(350, 553).
      THIS-OBJECT:splitContainerImpKoder:SplitterDistance = 272.
      THIS-OBJECT:splitContainerImpKoder:TabIndex = 0.
      /*  */
      /* splitContainerImpVal */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpVal:Name = "splitContainerImpVal".
      THIS-OBJECT:splitContainerImpVal:Size = NEW System.Drawing.Size(350, 272).
      THIS-OBJECT:splitContainerImpVal:SplitterDistance = 321.
      THIS-OBJECT:splitContainerImpVal:TabIndex = 0.
      /*  */
      /* splitContainerImpBild */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpBild:Name = "splitContainerImpBild".
      /*  */
      /* splitContainerImpBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerImpBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerImpBild:Size = NEW System.Drawing.Size(441, 553).
      THIS-OBJECT:splitContainerImpBild:SplitterDistance = 88.
      THIS-OBJECT:splitContainerImpBild:TabIndex = 0.
      /*  */
      /* TabAvtalskalkyl */
      /*  */
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:splitContainerAvtal).
      THIS-OBJECT:TabAvtalskalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabAvtalskalkyl:Name = "TabAvtalskalkyl".
      THIS-OBJECT:TabAvtalskalkyl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerAvtal */
      /*  */
      THIS-OBJECT:splitContainerAvtal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAvtal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAvtal:Name = "splitContainerAvtal".
      /*  */
      /* splitContainerAvtal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAvtal:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerAvtal.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerAvtal:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerAvtal:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerAvtal:SplitterDistance = 700.
      THIS-OBJECT:splitContainerAvtal:TabIndex = 44.
      /*  */
      /* TabKopiera */
      /*  */
      THIS-OBJECT:TabKopiera:Controls:Add(THIS-OBJECT:splitContainerKopiera).
      THIS-OBJECT:TabKopiera:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKopiera:Name = "TabKopiera".
      THIS-OBJECT:TabKopiera:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabKopiera:Paint:Subscribe(THIS-OBJECT:TabKopiera_Paint).
      /*  */
      /* splitContainerKopiera */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopiera:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopiera:Name = "splitContainerKopiera".
      /*  */
      /* splitContainerKopiera.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Panel1:Controls:Add(THIS-OBJECT:splitContainerKopKonv).
      /*  */
      /* splitContainerKopiera.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKopiera.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKopiera:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerKopiera:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerKopiera:SplitterDistance = 918.
      THIS-OBJECT:splitContainerKopiera:TabIndex = 49.
      /*  */
      /* splitContainerKopKonv */
      /*  */
      THIS-OBJECT:splitContainerKopKonv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopKonv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopKonv:Name = "splitContainerKopKonv".
      /*  */
      /* splitContainerKopKonv.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopKonv:Panel1:Controls:Add(THIS-OBJECT:KopLabelOmr).
      THIS-OBJECT:splitContainerKopKonv:Size = NEW System.Drawing.Size(918, 553).
      THIS-OBJECT:splitContainerKopKonv:SplitterDistance = 457.
      THIS-OBJECT:splitContainerKopKonv:TabIndex = 0.
      /*  */
      /* KopLabelOmr */
      /*  */
      appearance29:TextHAlignAsString = "Left".
      THIS-OBJECT:KopLabelOmr:Appearance = appearance29.
      THIS-OBJECT:KopLabelOmr:Location = NEW System.Drawing.Point(19, 53).
      THIS-OBJECT:KopLabelOmr:Name = "KopLabelOmr".
      THIS-OBJECT:KopLabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:KopLabelOmr:TabIndex = 46.
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:splitContainerTidlage).
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerTidlage */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerTidlage:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerTidlage:Name = "splitContainerTidlage".
      /*  */
      /* splitContainerTidlage.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerTidlage.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerTidlage:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerTidlage:SplitterDistance = 763.
      THIS-OBJECT:splitContainerTidlage:TabIndex = 0.
      /*  */
      /* TabFelmedd */
      /*  */
      THIS-OBJECT:TabFelmedd:Controls:Add(THIS-OBJECT:splitContainerFelMedd).
      THIS-OBJECT:TabFelmedd:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFelmedd:Name = "TabFelmedd".
      THIS-OBJECT:TabFelmedd:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerFelMedd */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFelMedd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFelMedd:Name = "splitContainerFelMedd".
      /*  */
      /* splitContainerFelMedd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerFelMedd.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerFelMedd:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerFelMedd:SplitterDistance = 860.
      THIS-OBJECT:splitContainerFelMedd:TabIndex = 1.
      /*  */
      /* TabVolymber */
      /*  */
      THIS-OBJECT:TabVolymber:Controls:Add(THIS-OBJECT:splitContainerVolym).
      THIS-OBJECT:TabVolymber:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVolymber:Name = "TabVolymber".
      THIS-OBJECT:TabVolymber:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerVolym */
      /*  */
      THIS-OBJECT:splitContainerVolym:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVolym:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVolym:Name = "splitContainerVolym".
      /*  */
      /* splitContainerVolym.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVolym:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerVolym.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerVolym:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerVolym:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerVolym:SplitterDistance = 958.
      THIS-OBJECT:splitContainerVolym:TabIndex = 1.
      /*  */
      /* ultraTabPageControl2 */
      /*  */
      THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:splitContainerStatus).
      THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2".
      THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerStatus */
      /*  */
      THIS-OBJECT:splitContainerStatus:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStatus:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStatus:Name = "splitContainerStatus".
      /*  */
      /* splitContainerStatus.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerStatus.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerStatus:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerStatus:SplitterDistance = 583.
      THIS-OBJECT:splitContainerStatus:TabIndex = 0.
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabManager */
      /*  */
      appearance2:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance2.
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
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManager:TabIndex = 44.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance128:Image = CAST(resources:GetObject("appearance128.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance128.
      appearance127:Image = CAST(resources:GetObject("appearance127.Image"), System.Object).
      ultraTab13:Appearance = appearance127.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:TabKalkylHuvud.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Info".
      appearance130:Image = CAST(resources:GetObject("appearance130.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance130.
      appearance129:Image = CAST(resources:GetObject("appearance129.Image"), System.Object).
      ultraTab14:Appearance = appearance129.
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:TabKalkyl.
      ultraTab14:Text = "".
      appearance132:Image = CAST(resources:GetObject("appearance132.Image"), System.Object).
      ultraTab15:ActiveAppearance = appearance132.
      appearance131:Image = CAST(resources:GetObject("appearance131.Image"), System.Object).
      ultraTab15:Appearance = appearance131.
      appearance3:BackColor = System.Drawing.SystemColors:Control.
      ultraTab15:ClientAreaAppearance = appearance3.
      ultraTab15:Key = "Egna koder".
      ultraTab15:TabPage = THIS-OBJECT:TabEgna.
      ultraTab15:Text = "".
      ultraTab15:ToolTipText = "Gör Egna koder".
      appearance134:Image = CAST(resources:GetObject("appearance134.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance134.
      appearance133:Image = CAST(resources:GetObject("appearance133.Image"), System.Object).
      ultraTab16:Appearance = appearance133.
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "Visa ".
      appearance136:Image = CAST(resources:GetObject("appearance136.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance136.
      appearance135:Image = CAST(resources:GetObject("appearance135.Image"), System.Object).
      ultraTab17:Appearance = appearance135.
      ultraTab17:Key = "FaktorerPriser".
      ultraTab17:TabPage = THIS-OBJECT:TabFaktorer.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "Lägg in egna Faktorer/Priser".
      appearance138:Image = CAST(resources:GetObject("appearance138.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance138.
      appearance137:Image = CAST(resources:GetObject("appearance137.Image"), System.Object).
      ultraTab19:Appearance = appearance137.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget Materiel".
      appearance140:Image = CAST(resources:GetObject("appearance140.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance140.
      appearance139:BackColor = System.Drawing.Color:Transparent.
      appearance139:Image = CAST(resources:GetObject("appearance139.Image"), System.Object).
      ultraTab22:Appearance = appearance139.
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "Import/Mallar".
      appearance142:Image = CAST(resources:GetObject("appearance142.Image"), System.Object).
      ultraTab20:ActiveAppearance = appearance142.
      appearance141:Image = CAST(resources:GetObject("appearance141.Image"), System.Object).
      ultraTab20:Appearance = appearance141.
      ultraTab20:Key = "Avtalskalkyl".
      ultraTab20:TabPage = THIS-OBJECT:TabAvtalskalkyl.
      ultraTab20:Text = "".
      ultraTab20:ToolTipText = "Avtalskalkyl".
      appearance144:Image = CAST(resources:GetObject("appearance144.Image"), System.Object).
      ultraTab2:ActiveAppearance = appearance144.
      appearance143:Image = CAST(resources:GetObject("appearance143.Image"), System.Object).
      ultraTab2:Appearance = appearance143.
      ultraTab2:Key = "KopieraKalkyl".
      ultraTab2:TabPage = THIS-OBJECT:TabKopiera.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Kopiera/Konvertera kalkyler".
      appearance161:Image = CAST(resources:GetObject("appearance161.Image"), System.Object).
      ultraTab3:ActiveAppearance = appearance161.
      appearance162:Image = CAST(resources:GetObject("appearance162.Image"), System.Object).
      ultraTab3:Appearance = appearance162.
      ultraTab3:Key = "Tidlägen".
      ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl1.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Tidlägen".
      appearance146:Image = CAST(resources:GetObject("appearance146.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance146.
      appearance145:Image = CAST(resources:GetObject("appearance145.Image"), System.Object).
      ultraTab1:Appearance = appearance145.
      ultraTab1:Key = "FelMedd".
      ultraTab1:TabPage = THIS-OBJECT:TabFelmedd.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Felmeddelanden".
      appearance148:Image = CAST(resources:GetObject("appearance148.Image"), System.Object).
      ultraTab24:ActiveAppearance = appearance148.
      appearance147:Image = CAST(resources:GetObject("appearance147.Image"), System.Object).
      ultraTab24:Appearance = appearance147.
      ultraTab24:Key = "Volymber".
      ultraTab24:TabPage = THIS-OBJECT:TabVolymber.
      ultraTab24:Text = "".
      ultraTab24:ToolTipText = "Volymberäkning av schaktmassor".
      appearance163:Image = CAST(resources:GetObject("appearance163.Image"), System.Object).
      ultraTab4:ActiveAppearance = appearance163.
      ultraTab4:Key = "Status".
      ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl2.
      ultraTab4:Text = "".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 13 NO-UNDO.
      arrayvar0[1] = ultraTab13.
      arrayvar0[2] = ultraTab14.
      arrayvar0[3] = ultraTab15.
      arrayvar0[4] = ultraTab16.
      arrayvar0[5] = ultraTab17.
      arrayvar0[6] = ultraTab19.
      arrayvar0[7] = ultraTab22.
      arrayvar0[8] = ultraTab20.
      arrayvar0[9] = ultraTab2.
      arrayvar0[10] = ultraTab3.
      arrayvar0[11] = ultraTab1.
      arrayvar0[12] = ultraTab24.
      arrayvar0[13] = ultraTab4.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanged).
      /*  */
      /* CombKalkyl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "CombKalkyl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabKalkylHuvud:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuvPerslista:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabEgna:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:ResumeLayout(FALSE).
      THIS-OBJECT:TabFaktorer:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktPrisBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:ResumeLayout(FALSE).
      THIS-OBJECT:TabMateriel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlEget:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlEget:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgetMtrlBild:ResumeLayout(FALSE).
      THIS-OBJECT:TabImportMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpVal:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpBild:ResumeLayout(FALSE).
      THIS-OBJECT:TabAvtalskalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAvtal:ResumeLayout(FALSE).
      THIS-OBJECT:TabKopiera:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopiera:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopiera:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonv:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerTidlage:ResumeLayout(FALSE).
      THIS-OBJECT:TabFelmedd:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFelMedd:ResumeLayout(FALSE).
      THIS-OBJECT:TabVolymber:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVolym:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStatus:ResumeLayout(FALSE).
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
      THIS-OBJECT:GetComboV(HuvudOmrade).
      THIS-OBJECT:GetComboV(HuvudUtfardare).
      MESSAGE  "Combbox HuvudOmrade = " THIS-OBJECT:HuvudOmrade:value 
      VIEW-AS ALERT-BOX.
      IF cFrom EQ ? THEN cFrom = "".  
      IF cFrom = "Kalkylhuvud" THEN DO:        
         IF THIS-OBJECT:NyKalkyl = TRUE THEN DO:
         END.
      END.       
      IF cTo = "Kalkyl" THEN DO:         
      END.
      PROCESS EVENTS.
   END METHOD.
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
   END METHOD.
   METHOD PUBLIC VOID Kill().
      
   END METHOD.

   METHOD PRIVATE VOID InitializeForm ():
   /* FINNS I * 2GURUJUNI*/     
   END METHOD.



   METHOD PUBLIC VOID TabRefreshKalkylHuvud():
      
   END METHOD.

   METHOD PRIVATE VOID ValueArbetsKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
   
   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
   END METHOD.
   /*används inte?*/
   METHOD PRIVATE INTEGER GetCurrentType():

   END METHOD.

   METHOD PRIVATE CHARACTER GetMatrisName():
   END METHOD.

   METHOD PRIVATE VOID ValueLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
   
   METHOD PRIVATE VOID ValueKalkKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
   
   METHOD PRIVATE VOID ValueEgnaKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
   
   METHOD PRIVATE VOID ValuePersCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
      
   METHOD PRIVATE VOID VisaTabs (INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      
               
   END METHOD.   

   METHOD PUBLIC VOID SetVisningsBild(i AS INTEGER):
   END METHOD.

   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
   END METHOD.   
   METHOD PUBLIC VOID GenereraKalkylVisning():
   END METHOD.

   METHOD PRIVATE VOID ExcelHuv():
   END METHOD.
   METHOD PRIVATE VOID ExcelAnm():
   END METHOD.

   METHOD PRIVATE VOID ExcelGruppMatris(vad AS INTEGER,matrisrader AS CHARACTER,kostnadstartcol AS CHARACTER):
   END METHOD.
   
   METHOD PRIVATE VOID ExcelBilagor(vad AS INTEGER):
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummeraMtrl(cIN AS CHARACTER,iIN AS INTEGER):
   END METHOD.

   METHOD PRIVATE LOGICAL VisSummera(cIN AS CHARACTER,iIN AS INTEGER):
   END METHOD.
   METHOD PRIVATE LOGICAL VisGruppera(cIN AS CHARACTER):
   END METHOD.

   METHOD PRIVATE LOGICAL VisGruppMatris(iIN AS INTEGER):  
   END METHOD.
   
   METHOD PRIVATE INTEGER VisMatris():
   END METHOD.
   METHOD PRIVATE VOID ExcelImportLista(vad AS INTEGER,filnamn AS CHARACTER):
   END METHOD.
   
   METHOD PRIVATE VOID ImportMallar():   
   END METHOD.
   
   METHOD PRIVATE VOID ImportLista():   
   END METHOD.
   METHOD PUBLIC VOID GenereraAvtalsKalkyl():
   END METHOD.
   METHOD PRIVATE CHARACTER  TraffColf(INPUT ordvar AS INTEGER) :
   END METHOD.
   METHOD PRIVATE VOID EonesAvtal():
   END METHOD.
   
   METHOD PRIVATE VOID InfraAvtal():
   END METHOD.
   
   METHOD PRIVATE VOID  LimoAvtal():
   END METHOD.
   
   METHOD PRIVATE VOID  HamtaAvtalsKalkyl(OUTPUT resid AS INTEGER,OUTPUT ExelKommandout AS CHARACTER):
   END METHOD.
   
   METHOD PUBLIC CHARACTER ExcelTemplate(INPUT ExelKommandout AS CHARACTER):

   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
       
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
   END METHOD.
      
END CLASS.
