 
/*------------------------------------------------------------------------
   File        : Arende.cls
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


CLASS Modules.Arende.Arende INHERITS UserControl: 
   {ARENDEKATH.i}
   {ARENDEEXTRATAB.I}
   DEFINE PRIVATE VARIABLE ButtonMallarStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonExcelStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE VARIABLE combolista  AS CHARACTER NO-UNDO EXTENT 10.
   DEFINE PRIVATE VARIABLE GridStatus AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE imageList1                   AS System.Windows.Forms.ImageList                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylensKoderAnmarkning AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE MallarlStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE CurrentMatris                AS INTEGER                                                        NO-UNDO.
      
   DEFINE PRIVATE VARIABLE LabelUtf                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelOmr                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelKatalog                 AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelAnm                     AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
   
   DEFINE PRIVATE VARIABLE Rubrikanm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgetMtrlBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaVald       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaKoder      AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerEgnaArbKoder   AS System.Windows.Forms.SplitContainer                            NO-UNDO.
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
   DEFINE PRIVATE VARIABLE splitContainerMtrl           AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlEget AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlValj       AS System.Windows.Forms.SplitContainer                            NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStatus AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerTidlage AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabControlsPage AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelmedd                   AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TidStatus AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE Tabtidlage AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE vantapakalkyl AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaExcel                    AS Controls.GuruExcelEmbed                                        NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE components                   AS System.ComponentModel.IContainer                               NO-UNDO.
   
  
   DEFINE PRIVATE VARIABLE EgnaKoderStrip               AS System.Windows.Forms.ToolStrip                                 NO-UNDO.
  
   DEFINE PRIVATE VARIABLE EgenTaBort                   AS System.Windows.Forms.ToolStripButton                           NO-UNDO.
   DEFINE PRIVATE VARIABLE EgenSkapa                    AS System.Windows.Forms.ToolStripButton                           NO-UNDO. 
   
   
   DEFINE PRIVATE VARIABLE MatButtonOverMark            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE MatButtonBackMark            AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonOverMark           AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE EgnaButtonBackMark           AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonOverMark         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylButtonBackMark         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOverMarkMallar         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBackMarkMallar         AS Controls.ButtonOver                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridTidlage                  AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridValdaMallar              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMallarKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMallar                   AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridMateriel                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridEgetMateriel             AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridEgnaKoder                AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridLopKoder                 AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridKalkylKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridArbetsKoder              AS Controls.Grid                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridValdaUnderKoder          AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridUnderKoder               AS Controls.Grid                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE GridPersonal                 AS Controls.Grid                                                  NO-UNDO.   
   DEFINE PRIVATE VARIABLE GridFelMedd                  AS Controls.Grid                                                  NO-UNDO.
   /* Drag & Dropp*/
   DEFINE         VARIABLE DGKoder                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGEgna                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudAnmarkning              AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.  
   
   DEFINE PRIVATE VARIABLE HuvudKatalog                 AS Controls.ComboSimple                                           NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudUtfardare               AS Controls.Combo                                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudOmrade                  AS Controls.Combo                                                 NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HuvudKalknr                  AS Controls.FillinInteger                                         NO-UNDO.
   DEFINE PUBLIC  VARIABLE HuvudBenamning               AS Controls.FillinChar                                            NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnsvarig                AS Controls.FillinChar                                            NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE TabEgna                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImportMallar              AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkyl                    AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylHuvud               AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa                      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMateriel                  AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager                   AS Infragistics.Win.UltraWinTabControl.UltraTabControl            NO-UNDO. 
   
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
   DEFINE PRIVATE VARIABLE VisaPanelEmpty               AS System.Windows.Forms.Panel                                     NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelWaiting             AS System.Windows.Forms.Panel                                     NO-UNDO.
   
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
      

   CONSTRUCTOR PUBLIC Arende ( INPUT r AS Guru.Root ):
      SUPER ().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar Arendeobjekt..").
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Arende:InitializeTTs().
      THIS-OBJECT:Root:Logger:WriteLine("Arende: Gör kopplingar till databasemanager..").
      {ARENDEKATDBH.i Root:DatabaseManager:Arende}
      
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC Arende ( ):

   END DESTRUCTOR.






	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonExcelStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
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
      
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(50) + "      " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(51), THIS-OBJECT:Root:LanguageManager:GetString(52), System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).
      
      tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      IF res:ToString() = "Yes" THEN THIS-OBJECT:ExcelImportLista(1,fildir).
      IF res:ToString() = "No" THEN THIS-OBJECT:ExcelImportLista(2,fildir).
      IF res:ToString() = "Cancel" THEN RETURN.
      THIS-OBJECT:FelMeddCheck().

	END METHOD.












   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*Robin, Om inga kvar ändrat 4/6/13*/
      IF THIS-OBJECT:GridValdaMallar:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ButtonBackMarkMallar:GuruOver().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetString(75)).
      END.
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonMallarStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ImportMallar().
      THIS-OBJECT:FelMeddCheck().
      RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ButtonOverMarkMallar:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:ButtonOverMarkMallar:GuruOver().
      END.
      ELSE rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), THIS-OBJECT:Root:LanguageManager:GetString(74)).
      RETURN.

   END METHOD.




	
   /*------------------------------------------------------------------------------
      Purpose:  																	  
      Notes:  																	  
------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaMallar THEN 
      DO:
         THIS-OBJECT:ButtonBackMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridMateriel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridEgetMateriel THEN 
      DO:
         THIS-OBJECT:MatButtonBackMark_Click(?,?).
      END.
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridPersonal_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
	   THIS-OBJECT:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
		RETURN.

	END METHOD.



	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridPersonal_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		MESSAGE LAST-EVENT:LABEL "keydown"
      VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridTidlage_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE datvar AS DATE NO-UNDO.
      DEFINE VARIABLE tidvar AS INTEGER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE NE kolvarde THEN DO:
         THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD("ANVANDARE1"):BUFFER-VALUE = Guru.Konstanter:globanv.
         Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
         IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
            ASSIGN
            THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD("NAMNANVANDARE1"):BUFFER-VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
         END.
         IF kolnamn = "DATUMTID" THEN DO:
            Helpers.Functions:GetDateTime(INPUT THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE, OUTPUT datvar, OUTPUT tidvar).
            IF tidvar = 0 THEN DO:
               THIS-OBJECT:arendetidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = DATETIME(datvar, MTIME ).
            END.   
         END.   
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().       
      END.
     
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMallar THEN DO:
         THIS-OBJECT:ButtonOverMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.


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
   @VisualDesigner.
   METHOD PRIVATE VOID KoderAnmarkning_AfterExitEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF KoderTTh:AVAILABLE THEN DO:   
         THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkylensKoderAnmarkning:Text).
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
      END.
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
      THIS-OBJECT:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      /*v11.2 fix*/
      /*
      THIS-OBJECT:HuvudOmrade:Text = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      
      */
      THIS-OBJECT:HuvudOmrade:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
      THIS-OBJECT:LaddaLabels().
      THIS-OBJECT:VisaTabs("ALLA", FALSE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY, TRUE).
      THIS-OBJECT:VisaTabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL NewArende(INPUT kalkdatah AS HANDLE):
      /* skapa kalkyl i databas */
      IF THIS-OBJECT:Root:DatabaseManager:Arende:CreateArende() = FALSE THEN DO:
         RETURN FALSE.
      END.   
      /* Hitta rätt katalog*/
      THIS-OBJECT:HuvudKatalog:GuruFiltrera().
      KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      /*v11.2 fix*/
     /*
      THIS-OBJECT:HuvudOmrade:Text = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      */
      
      THIS-OBJECT:HuvudOmrade:VALUE = Guru.Konstanter:OmradeTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.      
      THIS-OBJECT:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
      THIS-OBJECT:LaddaLabels().
      kalkdatah:FIND-FIRST() NO-ERROR.
      IF kalkdatah:AVAILABLE THEN DO:
         IF kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = 0 OR kalkdatah:BUFFER-FIELD("TYP"):BUFFER-VALUE = ? THEN. 
         THIS-OBJECT:HuvudBenamning:GuruText:Text  = kalkdatah:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         /*v11.2 fix*/
         /*
         THIS-OBJECT:HuvudOmrade:TEXT = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         
         */
         THIS-OBJECT:HuvudOmrade:VALUE = kalkdatah:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
         
         /*THIS-OBJECT:HuvudOmrade:ENABLED  = FALSE.*/
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
   END METHOD.
   
   METHOD PRIVATE LOGICAL SaveHuvud():
      THIS-OBJECT:HuvudTTh:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:HuvudBenamning:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudKalknr:GuruText:Text).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE    =      STRING(Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(1).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudOmrade:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:HuvudKatalog:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:HuvudUtfardare:VALUE).
      THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:HuvudAnmarkning:Text).
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
      THIS-OBJECT:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)
      THIS-OBJECT:HuvudKalknr:VISIBLE       = TRUE. 
      IF THIS-OBJECT:LoadKalkyl(INTEGER(STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)),STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE), TRUE) = FALSE THEN RETURN FALSE.
      ELSE RETURN TRUE.
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
         THIS-OBJECT:GridStatus:GuruReopen().
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
      THIS-OBJECT:HuvudKatalog:ENABLED = FALSE
      THIS-OBJECT:HuvudOmrade:ENABLED  = FALSE.
   /* VVVVSätt alla ändringar på grids, Combos osv som behövs efter laddning*/
      
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
      THIS-OBJECT:FrekvensKomChanged(?,?).
      THIS-OBJECT:KoderAnmChanged(?,?). 
      THIS-OBJECT:ValueArbetsKoderCh(?,?).
      
      RETURN TRUE.
   END METHOD.

   METHOD PUBLIC VOID CreateForm():
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
      THIS-OBJECT:KalkNrvar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE.
      THIS-OBJECT:Omradevar = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonTabKalkyl:Caption = Guru.GlobalaVariabler:KalkArendeText + " " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE).
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE) NO-ERROR.
      
      THIS-OBJECT:GridUnderKoder:GuruFiltrera( "kalkylprisertt.KLOGSUBID = " + STRING( THIS-OBJECT:KatalogTTh:BUFFER-FIELD("HKLOGSUBID"):BUFFER-VALUE ) + " AND kalkylprisertt.EGENKODUPP = TRUE").      
     
      /* Sätter Combos */ 
      /*
      THIS-OBJECT:RibbonToolUppfPriser:SelectedItem = THIS-OBJECT:RibbonToolUppfPriser:ValueList:ValueListItems[THIS-OBJECT:HuvudTTh:BUFFER-FIELD("UTYP"):BUFFER-VALUE - 1].
     */
      /*v11.2 fix*/
      THIS-OBJECT:HuvudOmrade:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      /*
      THIS-OBJECT:HuvudOmrade:text = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
    */
      THIS-OBJECT:HuvudKatalog:GuruFiltrera().
      
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
    
      Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'")  NO-ERROR. 
      /*THIS-OBJECT:HuvudUtfardare:GuruFiltrera().*/
      THIS-OBJECT:HuvudUtfardare:VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
      /* Hitta kalkylansvarig*/
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR. 
      IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:                   
         THIS-OBJECT:GridPersonal:GuruRepositionTo(Guru.Konstanter:PersonalTTh:ROWID).              
      END.
      /* Filtrera arbetskoder efter kalkyltyp */
      THIS-OBJECT:GridArbetsKoder:GuruFiltrera("kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE)).
      /* Hitta första rows i grejer - behövs detta verkligen ? */
      /* Döp om matris rätt */
      matrisname = THIS-OBJECT:GetMatrisName().
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["MATRIS"]:Header:Caption = matrisname.
      /* Sätter värden */
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      
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
      THIS-OBJECT:VisaTabs("ALLA", TRUE).
      THIS-OBJECT:GridTidlage:GuruReopen().
      IF THIS-OBJECT:HuvudTTh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE THEN DO:
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:Selected = TRUE.         
      END.    
      ELSE THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
      
      THIS-OBJECT:RefreshMarkning().
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      /*ANDERS flyttad*/
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabKoder).      
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningVisa).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"Arende",THIS-OBJECT:KalkNrvar, THIS-OBJECT:Omradevar,THIS-OBJECT:RibbonTabVisningEgenskaper).
   
   END METHOD.
   METHOD PUBLIC VOID LaddaLabels():
      Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR.
      IF Guru.Konstanter:PersonalTTh:AVAILABLE = FALSE  THEN DO:
         Guru.Konstanter:PersonalTTh:FIND-FIRST() NO-ERROR.
      END.   
      ASSIGN
      THIS-OBJECT:LabelOmr:TEXT                  = Guru.Konstanter:gomrk
      THIS-OBJECT:HuvudBenamning:GuruLabel:Text  = "Benämning"
      THIS-OBJECT:HuvudBenamning:GuruText:Text   = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
      THIS-OBJECT:HuvudAnmarkning:Text           = THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE
      
      THIS-OBJECT:HuvudKalknr:GuruLabel:Text     = "nr."
      THIS-OBJECT:HuvudKalknr:GuruLabel:Text     = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:HuvudKalknr:GuruLabel:Text,TRUE)
      THIS-OBJECT:HuvudKalknr:GuruText:Text      = STRING(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE)
      THIS-OBJECT:HuvudKalknr:GuruText:Enabled   = FALSE          
      THIS-OBJECT:HuvudAnsvarig:GuruLabel:Text   = "Ansvarig"
      THIS-OBJECT:HuvudAnsvarig:GuruText:Text    = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
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
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridLopKoder).
      THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridUnderKoder).
      THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridMateriel).
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridEgetMateriel).
      THIS-OBJECT:DGMall = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridMallar).
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridValdaMallar).
      /* Button overs*/
      THIS-OBJECT:KalkylButtonOverMark:GuruKoppla(THIS-OBJECT:GridLopKoder,THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:KalkylButtonOverMark:GuruDublett = TRUE.
      THIS-OBJECT:KalkylButtonBackMark:GuruKoppla(THIS-OBJECT:GridKalkylKoder,THIS-OBJECT:GridLopKoder).
      THIS-OBJECT:KalkylButtonBackMark:GuruRemoveFrom = TRUE.  
      /* Egna koder börjar här*/
      THIS-OBJECT:EgnaButtonOverMark:GuruKoppla(THIS-OBJECT:GridUnderKoder,THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:EgnaButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:EgnaButtonBackMark:GuruKoppla(THIS-OBJECT:GridValdaUnderKoder, THIS-OBJECT:GridUnderKoder).
      THIS-OBJECT:EgnaButtonBackMark:GuruRemoveFrom = TRUE.
      /*materialtab*/
      THIS-OBJECT:MatButtonOverMark:GuruKoppla(THIS-OBJECT:GridMateriel,THIS-OBJECT:GridEgetMateriel).
      THIS-OBJECT:MatButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:MatButtonBackMark:GuruKoppla(THIS-OBJECT:GridEgetMateriel, THIS-OBJECT:GridMateriel).
      THIS-OBJECT:MatButtonBackMark:GuruRemoveFrom = TRUE.
      /*mallar*/
      THIS-OBJECT:ButtonOverMarkMallar:GuruKoppla(THIS-OBJECT:GridMallar,THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:ButtonOverMarkMallar:GuruDublett = TRUE.
      
      THIS-OBJECT:ButtonBackMarkMallar:GuruKoppla(THIS-OBJECT:GridValdaMallar, THIS-OBJECT:GridMallar).
      THIS-OBJECT:ButtonBackMarkMallar:GuruRemoveFrom = TRUE.
   END METHOD.

   /* Huvud */
   METHOD PUBLIC VOID CreateGridPersonal():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      GridPersonal:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT Guru.Konstanter:PersonalTTh)).
      THIS-OBJECT:GridPersonal:Text = "ansvarig".
      THIS-OBJECT:GridPersonal:Text = THIS-OBJECT:TooltipTextSet(THIS-OBJECT:GridPersonal:Text,TRUE).
           /*egna event*/
      THIS-OBJECT:GridPersonal:tBS:PositionChanged:Subscribe(ValuePersCh).
      
   END METHOD.

   /* Kalkyl*/
   METHOD PUBLIC VOID CreateGridArbetsKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,125).
      GridArbetsKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      GridArbetsKoder:TEXT = "Arbetskoder".      
      GridArbetsKoder:tBS:PositionChanged:Subscribe(ValueArbetsKoderCh).
          
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopNummer():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35, "ARANDELOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      GridLopKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:LopposterTTh)).
      THIS-OBJECT:GridLopKoder:TEXT = "Löpnummer".
      THIS-OBJECT:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridLopKoder:tBS:PositionChanged:Subscribe(ValueLopnrCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKalkylKoder():
      {GridRubrikListaVarExtent.i}
      
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55,"DEC").      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "ARANDELOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubrikergrid[11] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,80).
      rubrikergrid[12] = NEW Controls.GridRubrik("RISK","Risk i %",FALSE,75,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("VINST","Vinst i %",FALSE,75,"DEC").
      THIS-OBJECT:GridKalkylKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 13, INPUT THIS-OBJECT:KoderTTh)).
      THIS-OBJECT:GridKalkylKoder:TEXT = "Kalkylkoder".  
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      
      /*
      THIS-OBJECT:splitContainerKomAnm:VISIBLE = FALSE.
      THIS-OBJECT:KoderAnmarkning:VISIBLE = FALSE.
      */
      /*default filter  sortering*/     
      THIS-OBJECT:GridKalkylKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridKalkylKoder:tBS:PositionChanged:Subscribe(ValueKalkKoderCh).
      THIS-OBJECT:GridKalkylKoder:GuruFirstrow().
   END METHOD.

   
   /* Egna koder*/
   METHOD PUBLIC VOID CreateGridEgnaKoder():
      {GridRubrikListaVarExtent.i}
      THIS-OBJECT:DontCheck = TRUE.
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("MATRIS","MATRIS",FALSE,35,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55,"DEC").      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "ARANDELOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,250).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,80,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",false,80).
      /*init av grid*/
      GridEgnaKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KoderTTh)). 
      GridEgnaKoder:TEXT = "Egna koder".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.   
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridEgnaKoder:GuruFiltrera("arendenumtt.ARBKOD = 'EGEN'").
      THIS-OBJECT:GridEgnaKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridEgnaKoder:tBS:PositionChanged:Subscribe(ValueEgnaKoderCh).
      THIS-OBJECT:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:DontCheck = false.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 250).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", TRUE, 100, "DEC").
      GridUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:PriserTTh)).
      THIS-OBJECT:GridUnderKoder:TEXT = "Underkoder".
      THIS-OBJECT:GridUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridUnderKoder:GuruFirstrow().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaUnderKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NUMSUBID", "##", TRUE ,35).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100,"DEC").
      GridValdaUnderKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ValdaPriserTTh)).
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Bands[0]:Columns["NUMSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:TEXT = "Valda underkoder".
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
   END METHOD.

   /* Materiel */
   METHOD PUBLIC VOID CreateGridMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75, "DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100, "DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50, "DEC").
      GridMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT Guru.Konstanter:MaterialTTh)).
      
      THIS-OBJECT:GridMateriel:Text = "Materiel".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:GridMateriel:GuruGridFarg(TRUE).
         THIS-OBJECT:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
      END. 
      THIS-OBJECT:GridMateriel:VisaViaInternetMenuItemAddAdd().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MATRIS", "Matris.", FALSE ,50,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50, "DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BERKVANT", "Antal", FALSE,50, "DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("NPRIS", "Nettopris", FALSE ,100, "DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50, "DEC").
      GridEgetMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:arendemtrlTTh)).
      THIS-OBJECT:GridEgetMateriel:Text = "Materielspecifikation".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridEgetMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:GridEgetMateriel:GuruGridFarg(TRUE).
         THIS-OBJECT:GridEgetMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
      END.  
      THIS-OBJECT:GridEgetMateriel:VisaViaInternetMenuItemAddAdd().
            
   END METHOD.

   
   METHOD PUBLIC VOID CreateGridMallarKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55,"DEC").      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "ARANDELOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      GridMallarKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:GridMallarKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridMallarKoder:Text = "Koder för mallen".
   END METHOD.

   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallHuvudtth)).  
      THIS-OBJECT:GridMallar:Text = "Mallar".
      GridMallar:tBS:PositionChanged:Subscribe(ValueMallarCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      GridValdaMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkmallValdtth)).
      THIS-OBJECT:GridValdaMallar:Text = "Valda mallar".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridTidlage():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,200).
      rubrikergrid[2] = NEW Controls.GridRubrik("DATUMTID","Datum",FALSE,200,"DATETIME").
      rubrikergrid[3] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      GridTidlage:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:arendetidlageTTh)).
      THIS-OBJECT:GridTidlage:TEXT = "Tidlägen".
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridStatus():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARENDENR","Ärendenr",TRUE,100,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("OMRADE",Guru.Konstanter:gomrk,TRUE,100).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,200).
      rubrikergrid[4] = NEW Controls.GridRubrik("TIDLAGE","Status",TRUE,175).
      rubrikergrid[5] = NEW Controls.GridRubrik("DATUMTID","Datum",TRUE,150,"DATETIME").
      rubrikergrid[6] = NEW Controls.GridRubrik("NAMNANVANDARE1","Ändrat av",TRUE,200).
      GridStatus:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:arendetidlageTTh)).
      THIS-OBJECT:GridStatus:TEXT = "Status".
     
   END METHOD.
   
   
   METHOD PUBLIC VOID CreateGridFelMedd():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("UT","Meddelande",TRUE,400).
      GridFelMedd:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 1, INPUT THIS-OBJECT:tiduth)).
      THIS-OBJECT:GridFelMedd:Text = "Fel vid inläsning!".
   END METHOD.
   
   
   /* Skapar alla Combos */
   METHOD PUBLIC VOID CreateCombos():
      THIS-OBJECT:CreateComboTyp().
      THIS-OBJECT:CreatecomboPriserUppf().
      THIS-OBJECT:CreateComboOmrade().
      THIS-OBJECT:CreateComboKatalog().
      THIS-OBJECT:CreateComboUtfardare().
      
   END METHOD. 
   /*v11.2 fix*/
   METHOD PRIVATE VOID SparaComboV(INPUT incombo AS Controls.Combo):
      
      IF incombo:VALUE = ? THEN RETURN.
      IF incombo:NAME  = "HuvudOmrade" THEN combolista[1] = THIS-OBJECT:HuvudOmrade:VALUE.
      IF incombo:NAME = "HuvudUtfardare" THEN combolista[2] = THIS-OBJECT:HuvudUtfardare:VALUE.
      
   END METHOD.
   /*v11.2 fix*/
   METHOD PRIVATE VOID GetComboV(INPUT incombo AS Controls.Combo):
       
      IF incombo:VALUE NE ? THEN RETURN.
      IF incombo:NAME  = "HuvudOmrade" THEN THIS-OBJECT:HuvudOmrade:VALUE = combolista[1].
      IF incombo:NAME = "HuvudUtfardare" THEN THIS-OBJECT:HuvudUtfardare:VALUE = combolista[2]. 
      
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
        
      THIS-OBJECT:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
      
      
      
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
      THIS-OBJECT:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
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
      THIS-OBJECT:HuvudUtfardare:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
   END METHOD.
   


   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   METHOD PUBLIC VOID BerEgenKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK)  NO-ERROR.
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN      
         THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
    
   END METHOD.

   METHOD PUBLIC VOID BerKalkKod():
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE THEN
         THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["TOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
      IF THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE THEN DO:
         IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE
            THEN THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["FRITOTKOST"]:VALUE = THIS-OBJECT:KoderTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
      END.
   END METHOD.

   METHOD PRIVATE VOID ButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*Robin, Om inga kvar ändrat 4/6/13*/
      IF THIS-OBJECT:GridKalkylKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:Arende:RemoveKoderSelected().
         THIS-OBJECT:KalkylButtonBackMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
         THIS-OBJECT:DontCheck = FALSE.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*Robin, Om inga kvar ändrat 4/6/13*/
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridValdaUnderKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:Arende:RemoveValdaSelectedEgna().
         THIS-OBJECT:EgnaButtonBackMark:GuruOver().      
         THIS-OBJECT:BerEgenKod().
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
         THIS-OBJECT:DontCheck = FALSE.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOvermark_ClickE( INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:ButtonOvermark_Click(1).
      RETURN.
   END METHOD.

   METHOD PRIVATE VOID ButtonOvermark_Click(INPUT antalvar AS DECIMAL):
      /*ROBIN ändring 28/5/13*/
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:KalkylButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:KalkylButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:CreateKoderRows(antalvar).
         THIS-OBJECT:KalkylButtonOverMark:GuruOverefteruppdat().
         IF THIS-OBJECT:GridKalkylKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:KalkylButtonOverMark:GuruTogrid:GuruCellvaljare("antal").
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridEgnaKoder:ActiveRow.
      /*ROBIN ändring 29/5/13*/
      IF THIS-OBJECT:EgnaButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:EgnaButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:CreateValdaRows().      
         THIS-OBJECT:EgnaButtonOverMark:GuruOverefteruppdat().
         IF THIS-OBJECT:GridUnderKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:EgnaButtonOverMark:GuruTogrid:GuruCellvaljare("TIMMAR").         
         THIS-OBJECT:BerEgenKod().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgetMateriel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMateriel THEN DO:
         THIS-OBJECT:MatButtonOverMark_Click(?,?).
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKod().
      THIS-OBJECT:BerEgenKod().        
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_AfterSelectChange ( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN DO:
         
      END.
      RETURN.
    END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   METHOD PUBLIC VOID FilterUnderKoder():
      /* Om det finns en aktiv row..*/
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 AND THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN DO:
         /* Uppdatera valda underkoderna, och återställ kontrollerna*/
         IF TRIM(STRING(THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE)) = "" THEN RETURN.
         IF THIS-OBJECT:DontCheck = FALSE THEN DO:
            THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE )).
            THIS-OBJECT:GridValdaUnderKoder:GuruFirstrow().
            THIS-OBJECT:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:EgnaButtonBackMark:Enabled = TRUE.
         END.  
      END.
           
      ELSE IF THIS-OBJECT:KoderTTh:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
         THIS-OBJECT:GridValdaUnderKoder:GuruFirstrow().
         THIS-OBJECT:GridValdaUnderKoder:Enabled = TRUE.
         THIS-OBJECT:EgnaButtonOverMark:Enabled = TRUE.
         THIS-OBJECT:EgnaButtonBackMark:Enabled = TRUE.

      END.      
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
         THIS-OBJECT:GridValdaUnderKoder:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonOverMark:Enabled = FALSE.
         THIS-OBJECT:EgnaButtonBackMark:Enabled = FALSE.
      END.
      RETURN.
   END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:  	skickar data till db efter cell uppdat																  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKod().
      THIS-OBJECT:BerKalkKod().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.

      kolnamn = e:Cell:Column:ToString().     
      kolvarde = e:Cell:VALUE:ToString().
      /*Robin extra sync 4/6/13*/
      THIS-OBJECT:GridValdaUnderKoder:SyncMan().
      THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKodSub().
      THIS-OBJECT:BerEgenKod().
      /*
      THIS-OBJECT:ValdaPriserTTh:FIND-FIRST("WHERE NUM = " + STRING(THIS-OBJECT:GridValdaUnderKoder:ActiveRow:Cells["NUM"]:VALUE) + 
      " AND NUMSUBID = " + STRING(THIS-OBJECT:GridValdaUnderKoder:ActiveRow:Cells["NUMSUBID"]:VALUE), NO-LOCK) NO-ERROR.
      
      THIS-OBJECT:ValdaPriserTTh:FIND-FIRST("WHERE NUM = " + STRING(Typ)) NO-ERROR.
      THIS-OBJECT:GridValdaUnderKoder:Valuechanged(). 
      */
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
   
   /*------------------------------------------------------------------------------
         Purpose:    gör då samma som buttenover                                                  
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKalkylKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopKoder THEN DO:
         THIS-OBJECT:ButtonOvermark_ClickE(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridLopKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKalkylKoder THEN DO:
         THIS-OBJECT:ButtonBackmark_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaUnderKoder THEN DO:
         THIS-OBJECT:ButtonBackmarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridUnderKoder THEN DO:
         THIS-OBJECT:ButtonOvermarkPriser_Click(?,?).
      END.
      RETURN.

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
      THIS-OBJECT:Root:DatabaseManager:Arende:Control = THIS-OBJECT.      
      THIS-OBJECT:CreateForm().
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC LOGICAL InitializeArendeStatus():
      THIS-OBJECT:Root:DatabaseManager:Arende:Control = THIS-OBJECT.    
      THIS-OBJECT:CreateRibbonVS().
      THIS-OBJECT:TabManager:Tabs["Status"]:Selected = TRUE.
       
      RETURN TRUE.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*Robin, Om inga kvar ändrat 4/6/13*/
      IF THIS-OBJECT:GridEgetMateriel:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:MatButtonBackMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonOverMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      /*ROBIN ändring 28/5/13*/
      IF THIS-OBJECT:MatButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:MatButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:CreatevalMaterial(THIS-OBJECT:KalkNrvar,THIS-OBJECT:Omradevar).
         THIS-OBJECT:GridEgetMateriel:GuruTom().
         THIS-OBJECT:MatButtonOverMark:GuruOverefteruppdat().
      END.
      ELSE DO:
         DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
            
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
            THIS-OBJECT:GridEgetMateriel:GuruFiltrera("").
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("").
         END.
         ELSE DO:
            THIS-OBJECT:GridEgetMateriel:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            THIS-OBJECT:KalkylButtonOverMark:Enabled = TRUE.
            THIS-OBJECT:EgenSkapa:Enabled = TRUE.
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)+ " AND ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
         END.
         THIS-OBJECT:GridEgnaKoder:GuruFirstRow().
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
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog".
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      /* Koder */
      THIS-OBJECT:RibbonToolKoderAnm = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KoderAnmarkning").
      THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:Caption = "Anm. för koder".
      THIS-OBJECT:RibbonToolKoderAnm:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens-kommentar".
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
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
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Caption = "Utförlig anm.".
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
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatris,THIS-OBJECT:RibbonTabEgnaKoder). /* Den här är nästan likadan som ovanstående kommentar, alla ribbonverktyg som läggs till måste och får bara preppas 1 gång, därefter måste man lägga till dom manuellt, se nedan */
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris).                                              /* Så här*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEndastmarkmtrl,THIS-OBJECT:RibbonTabMat).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade,THIS-OBJECT:RibbonTabKalkyl).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat,THIS-OBJECT:RibbonTabKalkyl).    
      
      /*Above is guuut*/
      
      /*fix inför piteå ej klara nya*/ 
      /* Preppa Visning*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUt).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisHuvud).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisAnmarkning).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisUtforlig).
      /*se piteå*/
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisSummera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGruppera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisPriser).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisMatSpec).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisGroupMatris).
      /*
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolUppfPriser).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs).
      
      
      THIS-OBJECT:RibbonTabVisning:Tools:Add(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:RibbonTabVisning:Tools:Add(THIS-OBJECT:RibbonToolSkrivUtBildval).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisHuvud).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisAnmarkning).
      THIS-OBJECT:RibbonTabVisningVisa:Tools:Add(THIS-OBJECT:RibbonToolVisUtforlig).
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisVisa).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisPriser).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatris).
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisSummera).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisGruppera).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisGroupMatris).   
      
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolVisMatSpec).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:RibbonTabVisningEgenskaper:Tools:Add(THIS-OBJECT:RibbonToolSparaDefs).
      /*
      THIS-OBJECT:RibbonTabUppfoljning:Tools:Add(THIS-OBJECT:RibbonToolUppfPriser).
      */
      /*fix inför piteå*/
      
      /* Lägg till verktygen i gruppen ordning */
      /*THIS-OBJECT:RibbonTabKoder:Tools:Add( THIS-OBJECT:RibbonToolMatris ).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolVisaTyp).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFilterTyp).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolKoderAnm).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFrekvensKom).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFriKalk).
      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFriGenerera).*/
      
     /* THIS-OBJECT:RibbonTabEgnaKoder:Tools:Add( THIS-OBJECT:RibbonToolMatris ).
      
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolEnr).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolSok).
     
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolLev).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolInfTyp).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolOmrade).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolKat).*/
      
      
      
      
      
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
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "Arende",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT THIS-OBJECT:KalkNrvar,INPUT THIS-OBJECT:Omradevar) .
   END METHOD.

   METHOD PUBLIC VOID VisAnmarkningChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
     
      THIS-OBJECT:RibbonToolVisUtforlig:SharedPropsInternal:Enabled = THIS-OBJECT:RibbonToolVisAnmarkning:Checked.
   END METHOD.
   
   METHOD PUBLIC VOID VisPriserChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*THIS-OBJECT:RibbonToolVisPriser:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisPriser:ValueList:SelectedIndex]*/
     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "0" THEN DO:
         /*IF THIS-OBJECT:RibbonToolVisPriser:Text EQ "Enligt katalog" THEN DO:*/
         THIS-OBJECT:RibbonToolVisMatSpec:CHECKED = FALSE.
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonToolVisMatSpec:SharedPropsInternal:Enabled = TRUE.
      END.
   END METHOD.
   
   
   
   METHOD PUBLIC VOID FrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
    
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         ASSIGN
         THIS-OBJECT:Rubrikanm:Text                                 = "Kommentar"
         THIS-OBJECT:RibbonToolKoderAnm:Checked                     = FALSE 
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = FALSE
         THIS-OBJECT:ValdKodKommentar:VISIBLE                       = TRUE
         THIS-OBJECT:KalkylensKoderAnmarkning:VISIBLE               = FALSE.
         THIS-OBJECT:GridKalkylKoder:VISIBLE                        = FALSE.
         THIS-OBJECT:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         THIS-OBJECT:ValueLopnrCh(?,?).
         THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE = TRUE. 
         THIS-OBJECT:splitContainerKalkKodoFrek:Panel1Collapsed = TRUE.
          
      END.   
      ELSE DO:
         ASSIGN 
         THIS-OBJECT:Rubrikanm:Text = "Anmärkning"
         THIS-OBJECT:RibbonToolKoderAnm:SharedPropsInternal:VISIBLE = TRUE
         THIS-OBJECT:GridKalkylKoder:VISIBLE                        = TRUE
         THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE               = FALSE.
         THIS-OBJECT:splitContainerKalkKodoFrek:Panel1Collapsed = FALSE.
         IF THIS-OBJECT:RibbonToolKoderAnm:Checked = FALSE THEN DO:
            THIS-OBJECT:KalkylensKoderAnmarkning:VISIBLE            = FALSE.
         END.
         ELSE DO:
            THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE            = TRUE.
            THIS-OBJECT:KalkylensKoderAnmarkning:VISIBLE           = TRUE.
         END.   
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID KoderAnmChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKoderAnm:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE = TRUE.
         THIS-OBJECT:KalkylensKoderAnmarkning:VISIBLE = TRUE.   
         THIS-OBJECT:ValdKodKommentar:VISIBLE = FALSE.               
      END.   
      ELSE DO:
         IF THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE THEN DO:
            THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE = FALSE.
            THIS-OBJECT:ValdKodKommentar:VISIBLE = FALSE.
         END. 
         ELSE DO:
            THIS-OBJECT:splitContainerKalkKomAnm:VISIBLE = TRUE.
            THIS-OBJECT:ValdKodKommentar:VISIBLE = TRUE.
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
      THIS-OBJECT:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtGrid(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:GridStatus:SkrivUt(). 
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning():
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
      Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " NO-LOCK".
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
      GridLopKoder:GuruAvmarkera(). 
      IF DoAll EQ TRUE THEN DO:
         THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera(" MARKNING = ''").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         ELSE DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera(" MARKNING = '" + Markning + "'").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         IF THIS-OBJECT:GridArbetsKoder:tBS:Count EQ 0 THEN DO:
            THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
            DoReturn = TRUE.
         END.      
      END.
      IF DoReturn EQ TRUE THEN RETURN.
      IF THIS-OBJECT:ArbetsKoderTTh:AVAILABLE EQ TRUE THEN DO:
         THIS-OBJECT:LopposterTTh:FIND-FIRST() NO-ERROR.
         THIS-OBJECT:GridLopKoder:GuruFiltrera(" ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         GridLopKoder:GuruFirstrow(). 
      END.
      ELSE DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:GridLopKoder:GuruFirstrow().
         IF THIS-OBJECT:KalkylLoaded EQ TRUE THEN DO:
            System.Windows.Forms.MessageBox:Show("Inga arbetskoder funna!").
         END.
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
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
      DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kundvar AS LOGICAL NO-UNDO.
      kundvar = ?.
      IF e:KeyChar  = CHR(13) THEN DO:
         IF STRING(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
         ELSE lev = STRING( CAST(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellVALUE("LEVKOD").*/
         enr = THIS-OBJECT:RibbonToolEnr:Text.
         ben = THIS-OBJECT:RibbonToolSok:Text.
         IF THIS-OBJECT:RibbonToolEndastmarkmtrl:Checked = TRUE THEN kundvar = TRUE.
         THIS-OBJECT:GridMateriel:GuruTom().
         THIS-OBJECT:Root:DatabaseManager:Global:FetchMateriel(lev, enr, ben,kundvar).
         THIS-OBJECT:GridMateriel:GuruReopen().
         THIS-OBJECT:GridMateriel:GuruFirstrow().
         e:Handled = TRUE.
      END.
   END METHOD.
   /*öppnar arbetskoder ned rätt query för vald typ*/
   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:GridArbetsKoder:Initierad = FALSE THEN RETURN.
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
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
      appearance24 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.Arende").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
      appearance34 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
      appearance35 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
      appearance36 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
      appearance37 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
      appearance38 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
      appearance39 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
      appearance40 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
      appearance41 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
      appearance42 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
      appearance43 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
      appearance44 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
      appearance45 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
      appearance46 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
      appearance47 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
      appearance48 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
      appearance49 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
      appearance55 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
      appearance56 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
      appearance57 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
      appearance58 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance59 AS Infragistics.Win.Appearance NO-UNDO.
      appearance59 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance60 AS Infragistics.Win.Appearance NO-UNDO.
      appearance60 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance61 AS Infragistics.Win.Appearance NO-UNDO.
      appearance61 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
      appearance62 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
      appearance63 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
      appearance64 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance77 AS Infragistics.Win.Appearance NO-UNDO.
      appearance77 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
      appearance78 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance91 AS Infragistics.Win.Appearance NO-UNDO.
      appearance91 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance92 AS Infragistics.Win.Appearance NO-UNDO.
      appearance92 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance93 AS Infragistics.Win.Appearance NO-UNDO.
      appearance93 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance94 AS Infragistics.Win.Appearance NO-UNDO.
      appearance94 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance95 AS Infragistics.Win.Appearance NO-UNDO.
      appearance95 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance96 AS Infragistics.Win.Appearance NO-UNDO.
      appearance96 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance97 AS Infragistics.Win.Appearance NO-UNDO.
      appearance97 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance98 AS Infragistics.Win.Appearance NO-UNDO.
      appearance98 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
      appearance99 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
      appearance100 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
      appearance105 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance106 AS Infragistics.Win.Appearance NO-UNDO.
      appearance106 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance107 AS Infragistics.Win.Appearance NO-UNDO.
      appearance107 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance108 AS Infragistics.Win.Appearance NO-UNDO.
      appearance108 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance109 AS Infragistics.Win.Appearance NO-UNDO.
      appearance109 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance110 AS Infragistics.Win.Appearance NO-UNDO.
      appearance110 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance111 AS Infragistics.Win.Appearance NO-UNDO.
      appearance111 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
      appearance112 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
      appearance113 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance114 AS Infragistics.Win.Appearance NO-UNDO.
      appearance114 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance127 AS Infragistics.Win.Appearance NO-UNDO.
      appearance127 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance128 AS Infragistics.Win.Appearance NO-UNDO.
      appearance128 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance129 AS Infragistics.Win.Appearance NO-UNDO.
      appearance129 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance130 AS Infragistics.Win.Appearance NO-UNDO.
      appearance130 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance131 AS Infragistics.Win.Appearance NO-UNDO.
      appearance131 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance132 AS Infragistics.Win.Appearance NO-UNDO.
      appearance132 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance133 AS Infragistics.Win.Appearance NO-UNDO.
      appearance133 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance134 AS Infragistics.Win.Appearance NO-UNDO.
      appearance134 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance135 AS Infragistics.Win.Appearance NO-UNDO.
      appearance135 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance136 AS Infragistics.Win.Appearance NO-UNDO.
      appearance136 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance137 AS Infragistics.Win.Appearance NO-UNDO.
      appearance137 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance138 AS Infragistics.Win.Appearance NO-UNDO.
      appearance138 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance139 AS Infragistics.Win.Appearance NO-UNDO.
      appearance139 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance140 AS Infragistics.Win.Appearance NO-UNDO.
      appearance140 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance141 AS Infragistics.Win.Appearance NO-UNDO.
      appearance141 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance142 AS Infragistics.Win.Appearance NO-UNDO.
      appearance142 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance143 AS Infragistics.Win.Appearance NO-UNDO.
      appearance143 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance144 AS Infragistics.Win.Appearance NO-UNDO.
      appearance144 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance145 AS Infragistics.Win.Appearance NO-UNDO.
      appearance145 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance146 AS Infragistics.Win.Appearance NO-UNDO.
      appearance146 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance147 AS Infragistics.Win.Appearance NO-UNDO.
      appearance147 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance148 AS Infragistics.Win.Appearance NO-UNDO.
      appearance148 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance149 AS Infragistics.Win.Appearance NO-UNDO.
      appearance149 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance150 AS Infragistics.Win.Appearance NO-UNDO.
      appearance150 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance151 AS Infragistics.Win.Appearance NO-UNDO.
      appearance151 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance152 AS Infragistics.Win.Appearance NO-UNDO.
      appearance152 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance153 AS Infragistics.Win.Appearance NO-UNDO.
      appearance153 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance154 AS Infragistics.Win.Appearance NO-UNDO.
      appearance154 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance155 AS Infragistics.Win.Appearance NO-UNDO.
      appearance155 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance156 AS Infragistics.Win.Appearance NO-UNDO.
      appearance156 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance157 AS Infragistics.Win.Appearance NO-UNDO.
      appearance157 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance158 AS Infragistics.Win.Appearance NO-UNDO.
      appearance158 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance159 AS Infragistics.Win.Appearance NO-UNDO.
      appearance159 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance160 AS Infragistics.Win.Appearance NO-UNDO.
      appearance160 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance161 AS Infragistics.Win.Appearance NO-UNDO.
      appearance161 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance162 AS Infragistics.Win.Appearance NO-UNDO.
      appearance162 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance163 AS Infragistics.Win.Appearance NO-UNDO.
      appearance163 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance164 AS Infragistics.Win.Appearance NO-UNDO.
      appearance164 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance165 AS Infragistics.Win.Appearance NO-UNDO.
      appearance165 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance166 AS Infragistics.Win.Appearance NO-UNDO.
      appearance166 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance167 AS Infragistics.Win.Appearance NO-UNDO.
      appearance167 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance168 AS Infragistics.Win.Appearance NO-UNDO.
      appearance168 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance169 AS Infragistics.Win.Appearance NO-UNDO.
      appearance169 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance170 AS Infragistics.Win.Appearance NO-UNDO.
      appearance170 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance171 AS Infragistics.Win.Appearance NO-UNDO.
      appearance171 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance172 AS Infragistics.Win.Appearance NO-UNDO.
      appearance172 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance173 AS Infragistics.Win.Appearance NO-UNDO.
      appearance173 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance174 AS Infragistics.Win.Appearance NO-UNDO.
      appearance174 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance175 AS Infragistics.Win.Appearance NO-UNDO.
      appearance175 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance176 AS Infragistics.Win.Appearance NO-UNDO.
      appearance176 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance177 AS Infragistics.Win.Appearance NO-UNDO.
      appearance177 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance178 AS Infragistics.Win.Appearance NO-UNDO.
      appearance178 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance179 AS Infragistics.Win.Appearance NO-UNDO.
      appearance179 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance180 AS Infragistics.Win.Appearance NO-UNDO.
      appearance180 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance181 AS Infragistics.Win.Appearance NO-UNDO.
      appearance181 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance182 AS Infragistics.Win.Appearance NO-UNDO.
      appearance182 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance183 AS Infragistics.Win.Appearance NO-UNDO.
      appearance183 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance184 AS Infragistics.Win.Appearance NO-UNDO.
      appearance184 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance185 AS Infragistics.Win.Appearance NO-UNDO.
      appearance185 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance186 AS Infragistics.Win.Appearance NO-UNDO.
      appearance186 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance187 AS Infragistics.Win.Appearance NO-UNDO.
      appearance187 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance188 AS Infragistics.Win.Appearance NO-UNDO.
      appearance188 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance189 AS Infragistics.Win.Appearance NO-UNDO.
      appearance189 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance190 AS Infragistics.Win.Appearance NO-UNDO.
      appearance190 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance191 AS Infragistics.Win.Appearance NO-UNDO.
      appearance191 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance192 AS Infragistics.Win.Appearance NO-UNDO.
      appearance192 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance193 AS Infragistics.Win.Appearance NO-UNDO.
      appearance193 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance194 AS Infragistics.Win.Appearance NO-UNDO.
      appearance194 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance195 AS Infragistics.Win.Appearance NO-UNDO.
      appearance195 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance196 AS Infragistics.Win.Appearance NO-UNDO.
      appearance196 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance197 AS Infragistics.Win.Appearance NO-UNDO.
      appearance197 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance198 AS Infragistics.Win.Appearance NO-UNDO.
      appearance198 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance199 AS Infragistics.Win.Appearance NO-UNDO.
      appearance199 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance200 AS Infragistics.Win.Appearance NO-UNDO.
      appearance200 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance201 AS Infragistics.Win.Appearance NO-UNDO.
      appearance201 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance202 AS Infragistics.Win.Appearance NO-UNDO.
      appearance202 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance203 AS Infragistics.Win.Appearance NO-UNDO.
      appearance203 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance204 AS Infragistics.Win.Appearance NO-UNDO.
      appearance204 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance205 AS Infragistics.Win.Appearance NO-UNDO.
      appearance205 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance206 AS Infragistics.Win.Appearance NO-UNDO.
      appearance206 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance207 AS Infragistics.Win.Appearance NO-UNDO.
      appearance207 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance208 AS Infragistics.Win.Appearance NO-UNDO.
      appearance208 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance209 AS Infragistics.Win.Appearance NO-UNDO.
      appearance209 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance210 AS Infragistics.Win.Appearance NO-UNDO.
      appearance210 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance211 AS Infragistics.Win.Appearance NO-UNDO.
      appearance211 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance212 AS Infragistics.Win.Appearance NO-UNDO.
      appearance212 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance213 AS Infragistics.Win.Appearance NO-UNDO.
      appearance213 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance214 AS Infragistics.Win.Appearance NO-UNDO.
      appearance214 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance215 AS Infragistics.Win.Appearance NO-UNDO.
      appearance215 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance216 AS Infragistics.Win.Appearance NO-UNDO.
      appearance216 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance217 AS Infragistics.Win.Appearance NO-UNDO.
      appearance217 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance218 AS Infragistics.Win.Appearance NO-UNDO.
      appearance218 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance219 AS Infragistics.Win.Appearance NO-UNDO.
      appearance219 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance220 AS Infragistics.Win.Appearance NO-UNDO.
      appearance220 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance221 AS Infragistics.Win.Appearance NO-UNDO.
      appearance221 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance222 AS Infragistics.Win.Appearance NO-UNDO.
      appearance222 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance223 AS Infragistics.Win.Appearance NO-UNDO.
      appearance223 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance224 AS Infragistics.Win.Appearance NO-UNDO.
      appearance224 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance225 AS Infragistics.Win.Appearance NO-UNDO.
      appearance225 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance226 AS Infragistics.Win.Appearance NO-UNDO.
      appearance226 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance227 AS Infragistics.Win.Appearance NO-UNDO.
      appearance227 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance228 AS Infragistics.Win.Appearance NO-UNDO.
      appearance228 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance229 AS Infragistics.Win.Appearance NO-UNDO.
      appearance229 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance230 AS Infragistics.Win.Appearance NO-UNDO.
      appearance230 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance231 AS Infragistics.Win.Appearance NO-UNDO.
      appearance231 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance232 AS Infragistics.Win.Appearance NO-UNDO.
      appearance232 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance233 AS Infragistics.Win.Appearance NO-UNDO.
      appearance233 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance234 AS Infragistics.Win.Appearance NO-UNDO.
      appearance234 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerHuv = NEW System.Windows.Forms.SplitContainer().
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
      THIS-OBJECT:LabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:splitContainerHuvPerslista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridPersonal = NEW Controls.Grid().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkArbkoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbetsKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridLopKoder = NEW Controls.Grid().
      THIS-OBJECT:KalkylButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerKalkFri = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKodoFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKalkylKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:Rubrikanm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:KalkylensKoderAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerEgnaBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaArbKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaKoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:EgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:EgenTaBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridEgnaKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaVald = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridValdaUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:VisaPanelEmpty = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:VisaPanelWaiting = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:vantapakalkyl = NEW System.Windows.Forms.Label().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerMtrlValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMateriel = NEW Controls.Grid().
      THIS-OBJECT:MatButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:MatButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerMtrlEget = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridEgetMateriel = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgetMtrlBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerImport = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpVal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMallar = NEW Controls.Grid().
      THIS-OBJECT:ButtonBackMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonOverMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:GridMallarKoder = NEW Controls.Grid().
      THIS-OBJECT:GridValdaMallar = NEW Controls.Grid().
      THIS-OBJECT:splitContainerImpBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:MallarlStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonMallarStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonExcelStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:Tabtidlage = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerTidlage = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridTidlage = NEW Controls.Grid().
      THIS-OBJECT:TabFelmedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerFelMedd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFelMedd = NEW Controls.Grid().
      THIS-OBJECT:TidStatus = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerStatus = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridStatus = NEW Controls.Grid().
      THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:TabControlsPage = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabKalkylHuvud:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerHuv, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHuv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:SuspendLayout().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerHuvPerslista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuvPerslista:SuspendLayout().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkyl:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:SuspendLayout().
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:SuspendLayout().
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkFri:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:SuspendLayout().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabEgna:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:SuspendLayout().
      THIS-OBJECT:EgnaKoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:SuspendLayout().
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabVisa:SuspendLayout().
      THIS-OBJECT:VisaPanelWaiting:SuspendLayout().
      THIS-OBJECT:TabMateriel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMtrlValj, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrlValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:SuspendLayout().
      CAST(THIS-OBJECT:GridMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerMtrlEget, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrlEget:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:SuspendLayout().
      CAST(THIS-OBJECT:GridEgetMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgetMtrlBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgetMtrlBild:SuspendLayout().
      THIS-OBJECT:TabImportMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImport, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImport:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImport:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImport:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpVal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpVal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpVal:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpVal:SuspendLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerImpBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpBild:SuspendLayout().
      THIS-OBJECT:MallarlStrip:SuspendLayout().
      THIS-OBJECT:Tabtidlage:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerTidlage, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerTidlage:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerTidlage:SuspendLayout().
      CAST(THIS-OBJECT:GridTidlage, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabFelmedd:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerFelMedd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFelMedd:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFelMedd:SuspendLayout().
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TidStatus:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerStatus, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStatus:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStatus:SuspendLayout().
      CAST(THIS-OBJECT:GridStatus, System.ComponentModel.ISupportInitialize):BeginInit().
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
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelKatalog).
      /*  */
      /* splitContainerHuv.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel2:Controls:Add(THIS-OBJECT:splitContainerHuvPerslista).
      THIS-OBJECT:splitContainerHuv:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerHuv:SplitterDistance = 383.
      THIS-OBJECT:splitContainerHuv:TabIndex = 43.
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
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxRowScrollRegions = 1.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
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
      appearance13:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelOmr:Appearance = appearance13.
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
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Appearance = appearance14.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance15:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance15:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance15:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance15:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance15.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance16.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance17:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance17:BackColor2 = System.Drawing.SystemColors:Control.
      appearance17:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance17.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance18:BackColor = System.Drawing.SystemColors:Window.
      appearance18:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance18.
      appearance19:BackColor = System.Drawing.SystemColors:Highlight.
      appearance19:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance19.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance20:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance20.
      appearance21:BorderColor = System.Drawing.Color:Silver.
      appearance21:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellAppearance = appearance21.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance22:BackColor = System.Drawing.SystemColors:Control.
      appearance22:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance22:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance22:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance22:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance22.
      appearance23:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance23.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance24:BackColor = System.Drawing.SystemColors:Window.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowAppearance = appearance24.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance25:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance25.
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
      /* LabelKatalog */
      /*  */
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelKatalog:Appearance = appearance26.
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
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPersonal:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridPersonal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPersonal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
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
      THIS-OBJECT:GridPersonal:DoubleClickRow:Subscribe(THIS-OBJECT:GridPersonal_DoubleClickRow).
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
      /* splitContainerKalkArbkoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:Controls:Add(THIS-OBJECT:GridArbetsKoder).
      /*  */
      /* splitContainerKalkArbkoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkLopnr).
      THIS-OBJECT:splitContainerKalkArbkoder:Size = NEW System.Drawing.Size(498, 551).
      THIS-OBJECT:splitContainerKalkArbkoder:SplitterDistance = 252.
      THIS-OBJECT:splitContainerKalkArbkoder:TabIndex = 0.
      /*  */
      /* GridArbetsKoder */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbetsKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbetsKoder:GuruContext = ?.
      THIS-OBJECT:GridArbetsKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbetsKoder:Name = "GridArbetsKoder".
      THIS-OBJECT:GridArbetsKoder:Size = NEW System.Drawing.Size(498, 252).
      THIS-OBJECT:GridArbetsKoder:TabIndex = 4.
      THIS-OBJECT:GridArbetsKoder:Text = "grid1".
      /*  */
      /* splitContainerKalkLopnr */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKalkLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkLopnr:Name = "splitContainerKalkLopnr".
      /*  */
      /* splitContainerKalkLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:Controls:Add(THIS-OBJECT:GridLopKoder).
      /*  */
      /* splitContainerKalkLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonOverMark).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonBackMark).
      THIS-OBJECT:splitContainerKalkLopnr:Size = NEW System.Drawing.Size(498, 295).
      THIS-OBJECT:splitContainerKalkLopnr:SplitterDistance = 463.
      THIS-OBJECT:splitContainerKalkLopnr:TabIndex = 0.
      /*  */
      /* GridLopKoder */
      /*  */
      THIS-OBJECT:GridLopKoder:AllowDrop = TRUE.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridLopKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopKoder:GuruContext = ?.
      THIS-OBJECT:GridLopKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopKoder:Name = "GridLopKoder".
      THIS-OBJECT:GridLopKoder:Size = NEW System.Drawing.Size(463, 295).
      THIS-OBJECT:GridLopKoder:TabIndex = 5.
      THIS-OBJECT:GridLopKoder:Text = "grid2".
      THIS-OBJECT:GridLopKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopKoder_GuruAfterDropped).
      /*  */
      /* KalkylButtonOverMark */
      /*  */
      appearance63:Image = CAST(resources:GetObject("appearance63.Image"), System.Object).
      THIS-OBJECT:KalkylButtonOverMark:Appearance = appearance63.
      THIS-OBJECT:KalkylButtonOverMark:Location = NEW System.Drawing.Point(2, 34).
      THIS-OBJECT:KalkylButtonOverMark:Name = "KalkylButtonOverMark".
      THIS-OBJECT:KalkylButtonOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonOverMark:TabIndex = 7.
      THIS-OBJECT:KalkylButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:KalkylButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOvermark_ClickE).
      /*  */
      /* KalkylButtonBackMark */
      /*  */
      appearance64:Image = CAST(resources:GetObject("appearance64.Image"), System.Object).
      THIS-OBJECT:KalkylButtonBackMark:Appearance = appearance64.
      THIS-OBJECT:KalkylButtonBackMark:Location = NEW System.Drawing.Point(2, 65).
      THIS-OBJECT:KalkylButtonBackMark:Name = "KalkylButtonBackMark".
      THIS-OBJECT:KalkylButtonBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylButtonBackMark:TabIndex = 9.
      THIS-OBJECT:KalkylButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:KalkylButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMark_Click).
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
      THIS-OBJECT:splitContainerKalkFri:Size = NEW System.Drawing.Size(639, 551).
      THIS-OBJECT:splitContainerKalkFri:SplitterDistance = 610.
      THIS-OBJECT:splitContainerKalkFri:TabIndex = 4.
      /*  */
      /* splitContainerKalkKodoFrek */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKodoFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKodoFrek:Name = "splitContainerKalkKodoFrek".
      /*  */
      /* splitContainerKalkKodoFrek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:Controls:Add(THIS-OBJECT:GridKalkylKoder).
      /*  */
      /* splitContainerKalkKodoFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkBild).
      THIS-OBJECT:splitContainerKalkKodoFrek:Size = NEW System.Drawing.Size(610, 551).
      THIS-OBJECT:splitContainerKalkKodoFrek:SplitterDistance = 381.
      THIS-OBJECT:splitContainerKalkKodoFrek:TabIndex = 36.
      /*  */
      /* GridKalkylKoder */
      /*  */
      THIS-OBJECT:GridKalkylKoder:AllowDrop = TRUE.
      appearance65:BackColor = System.Drawing.SystemColors:Window.
      appearance65:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Appearance = appearance65.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKalkylKoder:GuruContext = ?.
      THIS-OBJECT:GridKalkylKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridKalkylKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridKalkylKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKalkylKoder:Name = "GridKalkylKoder".
      THIS-OBJECT:GridKalkylKoder:Size = NEW System.Drawing.Size(381, 551).
      THIS-OBJECT:GridKalkylKoder:TabIndex = 3.
      THIS-OBJECT:GridKalkylKoder:Text = "grid3".
      THIS-OBJECT:GridKalkylKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKalkylKoder_GuruAfterDropped).
      THIS-OBJECT:GridKalkylKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterCellUpdate).
      THIS-OBJECT:GridKalkylKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterSelectChange).
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
      THIS-OBJECT:splitContainerKalkBild:Size = NEW System.Drawing.Size(225, 551).
      THIS-OBJECT:splitContainerKalkBild:SplitterDistance = 95.
      THIS-OBJECT:splitContainerKalkBild:TabIndex = 0.
      /*  */
      /* splitContainerKalkKomAnm */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKomAnm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKomAnm:Name = "splitContainerKalkKomAnm".
      THIS-OBJECT:splitContainerKalkKomAnm:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkKomAnm.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:Controls:Add(THIS-OBJECT:Rubrikanm).
      /*  */
      /* splitContainerKalkKomAnm.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:KalkylensKoderAnmarkning).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(95, 551).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 37.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* Rubrikanm */
      /*  */
      THIS-OBJECT:Rubrikanm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Rubrikanm:Enabled = FALSE.
      THIS-OBJECT:Rubrikanm:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:Rubrikanm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Rubrikanm:Name = "Rubrikanm".
      THIS-OBJECT:Rubrikanm:Size = NEW System.Drawing.Size(95, 33).
      THIS-OBJECT:Rubrikanm:TabIndex = 0.
      THIS-OBJECT:Rubrikanm:Text = "Anmärkning".
      /*  */
      /* KalkylensKoderAnmarkning */
      /*  */
      THIS-OBJECT:KalkylensKoderAnmarkning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KalkylensKoderAnmarkning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KalkylensKoderAnmarkning:Multiline = TRUE.
      THIS-OBJECT:KalkylensKoderAnmarkning:Name = "KalkylensKoderAnmarkning".
      THIS-OBJECT:KalkylensKoderAnmarkning:Size = NEW System.Drawing.Size(95, 510).
      THIS-OBJECT:KalkylensKoderAnmarkning:TabIndex = 33.
      THIS-OBJECT:KalkylensKoderAnmarkning:Text = " ".
      THIS-OBJECT:KalkylensKoderAnmarkning:AfterExitEditMode:Subscribe(THIS-OBJECT:KoderAnmarkning_AfterExitEditMode).
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:ReadOnly = TRUE.
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(95, 510).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 34.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
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
      /*  */
      /* splitContainerEgnaArbKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:Controls:Add(THIS-OBJECT:EgnaKoderStrip).
      /*  */
      /* splitContainerEgnaArbKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:Controls:Add(THIS-OBJECT:GridEgnaKoder).
      THIS-OBJECT:splitContainerEgnaArbKoder:Size = NEW System.Drawing.Size(800, 200).
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterDistance = 25.
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerEgnaArbKoder:TabIndex = 14.
      /*  */
      /* EgnaKoderStrip */
      /*  */
      THIS-OBJECT:EgnaKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:EgenSkapa.
      arrayvar0[2] = THIS-OBJECT:EgenTaBort.
      THIS-OBJECT:EgnaKoderStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:EgnaKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:EgnaKoderStrip:Name = "EgnaKoderStrip".
      THIS-OBJECT:EgnaKoderStrip:Size = NEW System.Drawing.Size(800, 25).
      THIS-OBJECT:EgnaKoderStrip:TabIndex = 13.
      THIS-OBJECT:EgnaKoderStrip:Text = "toolStrip1".
      /*  */
      /* EgenSkapa */
      /*  */
      THIS-OBJECT:EgenSkapa:Image = CAST(resources:GetObject("EgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenSkapa:Name = "EgenSkapa".
      THIS-OBJECT:EgenSkapa:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:EgenSkapa:Text = "Lägg till egen".
      THIS-OBJECT:EgenSkapa:Click:Subscribe(THIS-OBJECT:SkapaNyEgen_Click).
      /*  */
      /* EgenTaBort */
      /*  */
      THIS-OBJECT:EgenTaBort:Image = CAST(resources:GetObject("EgenTaBort.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenTaBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenTaBort:Name = "EgenTaBort".
      THIS-OBJECT:EgenTaBort:Size = NEW System.Drawing.Size(94, 22).
      THIS-OBJECT:EgenTaBort:Text = "Ta bort egen".
      THIS-OBJECT:EgenTaBort:Click:Subscribe(THIS-OBJECT:TaBortEgen_Click).
      /*  */
      /* GridEgnaKoder */
      /*  */
      appearance77:BackColor = System.Drawing.SystemColors:Window.
      appearance77:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Appearance = appearance77.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance78:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance78:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance78:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:Appearance = appearance78.
      appearance79:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance79.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance80:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance80:BackColor2 = System.Drawing.SystemColors:Control.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance80:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance80.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      appearance81:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveCellAppearance = appearance81.
      appearance82:BackColor = System.Drawing.SystemColors:Highlight.
      appearance82:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveRowAppearance = appearance82.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CardAreaAppearance = appearance83.
      appearance84:BorderColor = System.Drawing.Color:Silver.
      appearance84:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellAppearance = appearance84.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellPadding = 0.
      appearance85:BackColor = System.Drawing.SystemColors:Control.
      appearance85:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance85:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance85:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance85:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:GroupByRowAppearance = appearance85.
      appearance86:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderAppearance = appearance86.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowAppearance = appearance87.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance88.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgnaKoder:GuruContext = ?.
      THIS-OBJECT:GridEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgnaKoder:Name = "GridEgnaKoder".
      THIS-OBJECT:GridEgnaKoder:Size = NEW System.Drawing.Size(800, 174).
      THIS-OBJECT:GridEgnaKoder:TabIndex = 12.
      THIS-OBJECT:GridEgnaKoder:Text = "grid1".
      THIS-OBJECT:GridEgnaKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellUpdate).
      /*  */
      /* splitContainerEgnaValj */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerEgnaValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaValj:Name = "splitContainerEgnaValj".
      /*  */
      /* splitContainerEgnaValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel1:Controls:Add(THIS-OBJECT:GridUnderKoder).
      /*  */
      /* splitContainerEgnaValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaVald).
      THIS-OBJECT:splitContainerEgnaValj:Size = NEW System.Drawing.Size(800, 349).
      THIS-OBJECT:splitContainerEgnaValj:SplitterDistance = 266.
      THIS-OBJECT:splitContainerEgnaValj:TabIndex = 0.
      /*  */
      /* GridUnderKoder */
      /*  */
      THIS-OBJECT:GridUnderKoder:AllowDrop = TRUE.
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Appearance = appearance89.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance90:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance90:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance90.
      appearance91:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance91.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance92:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance92:BackColor2 = System.Drawing.SystemColors:Control.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance92:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance92.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      appearance93:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance93.
      appearance94:BackColor = System.Drawing.SystemColors:Highlight.
      appearance94:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance94.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance95.
      appearance96:BorderColor = System.Drawing.Color:Silver.
      appearance96:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellAppearance = appearance96.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance97:BackColor = System.Drawing.SystemColors:Control.
      appearance97:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance97:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance97:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance97:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance97.
      appearance98:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance98.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowAppearance = appearance99.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance100.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUnderKoder:Name = "GridUnderKoder".
      THIS-OBJECT:GridUnderKoder:Size = NEW System.Drawing.Size(264, 347).
      THIS-OBJECT:GridUnderKoder:TabIndex = 10.
      THIS-OBJECT:GridUnderKoder:Text = "grid1".
      THIS-OBJECT:GridUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUnderKoder_GuruAfterDropped).
      /*  */
      /* splitContainerEgnaVald */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaVald:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaVald:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaVald:Name = "splitContainerEgnaVald".
      /*  */
      /* splitContainerEgnaVald.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonOverMark).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonBackMark).
      /*  */
      /* splitContainerEgnaVald.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel2:Controls:Add(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:splitContainerEgnaVald:Size = NEW System.Drawing.Size(528, 347).
      THIS-OBJECT:splitContainerEgnaVald:SplitterDistance = 29.
      THIS-OBJECT:splitContainerEgnaVald:TabIndex = 0.
      /*  */
      /* EgnaButtonOverMark */
      /*  */
      appearance101:Image = CAST(resources:GetObject("appearance101.Image"), System.Object).
      THIS-OBJECT:EgnaButtonOverMark:Appearance = appearance101.
      THIS-OBJECT:EgnaButtonOverMark:Location = NEW System.Drawing.Point(4, 34).
      THIS-OBJECT:EgnaButtonOverMark:Name = "EgnaButtonOverMark".
      THIS-OBJECT:EgnaButtonOverMark:Size = NEW System.Drawing.Size(23, 25).
      THIS-OBJECT:EgnaButtonOverMark:TabIndex = 13.
      THIS-OBJECT:EgnaButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:EgnaButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOverMarkPriser_Click).
      /*  */
      /* EgnaButtonBackMark */
      /*  */
      appearance102:Image = CAST(resources:GetObject("appearance102.Image"), System.Object).
      THIS-OBJECT:EgnaButtonBackMark:Appearance = appearance102.
      THIS-OBJECT:EgnaButtonBackMark:Location = NEW System.Drawing.Point(4, 65).
      THIS-OBJECT:EgnaButtonBackMark:Name = "EgnaButtonBackMark".
      THIS-OBJECT:EgnaButtonBackMark:Size = NEW System.Drawing.Size(23, 23).
      THIS-OBJECT:EgnaButtonBackMark:TabIndex = 15.
      THIS-OBJECT:EgnaButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:EgnaButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMarkPriser_Click).
      /*  */
      /* GridValdaUnderKoder */
      /*  */
      THIS-OBJECT:GridValdaUnderKoder:AllowDrop = TRUE.
      appearance103:BackColor = System.Drawing.SystemColors:Window.
      appearance103:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Appearance = appearance103.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance104:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance104:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance104:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance104:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance104.
      appearance105:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance105.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance106:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance106:BackColor2 = System.Drawing.SystemColors:Control.
      appearance106:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance106:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance106.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance107:BackColor = System.Drawing.SystemColors:Window.
      appearance107:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance107.
      appearance108:BackColor = System.Drawing.SystemColors:Highlight.
      appearance108:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance108.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance109:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance109.
      appearance110:BorderColor = System.Drawing.Color:Silver.
      appearance110:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellAppearance = appearance110.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance111:BackColor = System.Drawing.SystemColors:Control.
      appearance111:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance111:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance111:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance111:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance111.
      appearance112:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance112.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance113:BackColor = System.Drawing.SystemColors:Window.
      appearance113:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowAppearance = appearance113.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance114:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance114.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridValdaUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaUnderKoder:Name = "GridValdaUnderKoder".
      THIS-OBJECT:GridValdaUnderKoder:Size = NEW System.Drawing.Size(495, 347).
      THIS-OBJECT:GridValdaUnderKoder:TabIndex = 11.
      THIS-OBJECT:GridValdaUnderKoder:Text = "grid1".
      THIS-OBJECT:GridValdaUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaUnderKoder_GuruAfterDropped).
      THIS-OBJECT:GridValdaUnderKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdaUnderKoder_AfterCellUpdate).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaExcel).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaPanelEmpty).
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:VisaPanelWaiting).
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(358, 44).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(522, 330).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      /*  */
      /* VisaPanelEmpty */
      /*  */
      THIS-OBJECT:VisaPanelEmpty:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:VisaPanelEmpty:Location = NEW System.Drawing.Point(40, 293).
      THIS-OBJECT:VisaPanelEmpty:Name = "VisaPanelEmpty".
      THIS-OBJECT:VisaPanelEmpty:Size = NEW System.Drawing.Size(279, 221).
      THIS-OBJECT:VisaPanelEmpty:TabIndex = 2.
      /*  */
      /* VisaPanelWaiting */
      /*  */
      THIS-OBJECT:VisaPanelWaiting:BackColor = System.Drawing.SystemColors:Info.
      THIS-OBJECT:VisaPanelWaiting:Controls:Add(THIS-OBJECT:vantapakalkyl).
      THIS-OBJECT:VisaPanelWaiting:Location = NEW System.Drawing.Point(30, 110).
      THIS-OBJECT:VisaPanelWaiting:Name = "VisaPanelWaiting".
      THIS-OBJECT:VisaPanelWaiting:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:VisaPanelWaiting:TabIndex = 1.
      /*  */
      /* vantapakalkyl */
      /*  */
      THIS-OBJECT:vantapakalkyl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:vantapakalkyl:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Italic, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:vantapakalkyl:Image = CAST(resources:GetObject("vantapakalkyl.Image"), System.Drawing.Image).
      THIS-OBJECT:vantapakalkyl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:vantapakalkyl:Name = "vantapakalkyl".
      THIS-OBJECT:vantapakalkyl:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:vantapakalkyl:TabIndex = 0.
      THIS-OBJECT:vantapakalkyl:Text = "Genererar kalkyl, var god vänta..".
      THIS-OBJECT:vantapakalkyl:TextAlign = System.Drawing.ContentAlignment:MiddleCenter.
      THIS-OBJECT:vantapakalkyl:UseCompatibleTextRendering = TRUE.
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
      /*  */
      /* splitContainerMtrlValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel1:Controls:Add(THIS-OBJECT:GridMateriel).
      /*  */
      /* splitContainerMtrlValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonOverMark).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonBackMark).
      THIS-OBJECT:splitContainerMtrlValj:Size = NEW System.Drawing.Size(518, 551).
      THIS-OBJECT:splitContainerMtrlValj:SplitterDistance = 489.
      THIS-OBJECT:splitContainerMtrlValj:TabIndex = 1.
      /*  */
      /* GridMateriel */
      /*  */
      THIS-OBJECT:GridMateriel:AllowDrop = TRUE.
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMateriel:DisplayLayout:Appearance = appearance115.
      THIS-OBJECT:GridMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance116:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance116:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance116:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance116:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:Appearance = appearance116.
      appearance117:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance117.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance118:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance118:BackColor2 = System.Drawing.SystemColors:Control.
      appearance118:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance118:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance118.
      THIS-OBJECT:GridMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance119:BackColor = System.Drawing.SystemColors:Window.
      appearance119:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance119.
      appearance120:BackColor = System.Drawing.SystemColors:Highlight.
      appearance120:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance120.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CardAreaAppearance = appearance121.
      appearance122:BorderColor = System.Drawing.Color:Silver.
      appearance122:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellAppearance = appearance122.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance123:BackColor = System.Drawing.SystemColors:Control.
      appearance123:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance123:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance123:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance123:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance123.
      appearance124:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderAppearance = appearance124.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance125:BackColor = System.Drawing.SystemColors:Window.
      appearance125:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:RowAppearance = appearance125.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance126.
      THIS-OBJECT:GridMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMateriel:GuruContext = ?.
      THIS-OBJECT:GridMateriel:GuruDraggable = TRUE.
      THIS-OBJECT:GridMateriel:GuruDroppable = TRUE.
      THIS-OBJECT:GridMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMateriel:Name = "GridMateriel".
      THIS-OBJECT:GridMateriel:Size = NEW System.Drawing.Size(489, 551).
      THIS-OBJECT:GridMateriel:TabIndex = 0.
      THIS-OBJECT:GridMateriel:Text = "grid1".
      THIS-OBJECT:GridMateriel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMateriel_GuruAfterDropped).
      /*  */
      /* MatButtonOverMark */
      /*  */
      appearance127:Image = CAST(resources:GetObject("appearance127.Image"), System.Object).
      THIS-OBJECT:MatButtonOverMark:Appearance = appearance127.
      THIS-OBJECT:MatButtonOverMark:Location = NEW System.Drawing.Point(-1, 3).
      THIS-OBJECT:MatButtonOverMark:Name = "MatButtonOverMark".
      THIS-OBJECT:MatButtonOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:MatButtonOverMark:TabIndex = 10.
      THIS-OBJECT:MatButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:MatButtonOverMark:Click:Subscribe(THIS-OBJECT:MatButtonOverMark_Click).
      /*  */
      /* MatButtonBackMark */
      /*  */
      appearance128:Image = CAST(resources:GetObject("appearance128.Image"), System.Object).
      THIS-OBJECT:MatButtonBackMark:Appearance = appearance128.
      THIS-OBJECT:MatButtonBackMark:Location = NEW System.Drawing.Point(-1, 34).
      THIS-OBJECT:MatButtonBackMark:Name = "MatButtonBackMark".
      THIS-OBJECT:MatButtonBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:MatButtonBackMark:TabIndex = 11.
      THIS-OBJECT:MatButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:MatButtonBackMark:Click:Subscribe(THIS-OBJECT:MatButtonBackMark_Click).
      /*  */
      /* splitContainerMtrlEget */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlEget:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlEget:Name = "splitContainerMtrlEget".
      /*  */
      /* splitContainerMtrlEget.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Panel1:Controls:Add(THIS-OBJECT:GridEgetMateriel).
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
      /* GridEgetMateriel */
      /*  */
      THIS-OBJECT:GridEgetMateriel:AllowDrop = TRUE.
      appearance129:BackColor = System.Drawing.SystemColors:Window.
      appearance129:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Appearance = appearance129.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance130:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance130:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance130:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance130:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:Appearance = appearance130.
      appearance131:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance131.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance132:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance132:BackColor2 = System.Drawing.SystemColors:Control.
      appearance132:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance132:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance132.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance133:BackColor = System.Drawing.SystemColors:Window.
      appearance133:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance133.
      appearance134:BackColor = System.Drawing.SystemColors:Highlight.
      appearance134:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance134.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance135:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CardAreaAppearance = appearance135.
      appearance136:BorderColor = System.Drawing.Color:Silver.
      appearance136:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellAppearance = appearance136.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance137:BackColor = System.Drawing.SystemColors:Control.
      appearance137:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance137:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance137:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance137:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance137.
      appearance138:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderAppearance = appearance138.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance139:BackColor = System.Drawing.SystemColors:Window.
      appearance139:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:RowAppearance = appearance139.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance140:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance140.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgetMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgetMateriel:GuruContext = ?.
      THIS-OBJECT:GridEgetMateriel:GuruDraggable = TRUE.
      THIS-OBJECT:GridEgetMateriel:GuruDroppable = TRUE.
      THIS-OBJECT:GridEgetMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgetMateriel:Name = "GridEgetMateriel".
      THIS-OBJECT:GridEgetMateriel:Size = NEW System.Drawing.Size(500, 551).
      THIS-OBJECT:GridEgetMateriel:TabIndex = 1.
      THIS-OBJECT:GridEgetMateriel:Text = "grid1".
      THIS-OBJECT:GridEgetMateriel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridEgetMateriel_GuruAfterDropped).
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
      /*  */
      /* splitContainerImpMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Panel2:Controls:Add(THIS-OBJECT:GridValdaMallar).
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
      /*  */
      /* splitContainerImpKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Panel2:Controls:Add(THIS-OBJECT:GridMallarKoder).
      THIS-OBJECT:splitContainerImpKoder:Size = NEW System.Drawing.Size(350, 553).
      THIS-OBJECT:splitContainerImpKoder:SplitterDistance = 272.
      THIS-OBJECT:splitContainerImpKoder:TabIndex = 0.
      /*  */
      /* splitContainerImpVal */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpVal:Name = "splitContainerImpVal".
      /*  */
      /* splitContainerImpVal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Panel1:Controls:Add(THIS-OBJECT:GridMallar).
      /*  */
      /* splitContainerImpVal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Panel2:Controls:Add(THIS-OBJECT:ButtonBackMarkMallar).
      THIS-OBJECT:splitContainerImpVal:Panel2:Controls:Add(THIS-OBJECT:ButtonOverMarkMallar).
      THIS-OBJECT:splitContainerImpVal:Size = NEW System.Drawing.Size(350, 272).
      THIS-OBJECT:splitContainerImpVal:SplitterDistance = 321.
      THIS-OBJECT:splitContainerImpVal:TabIndex = 0.
      /*  */
      /* GridMallar */
      /*  */
      THIS-OBJECT:GridMallar:AllowDrop = TRUE.
      appearance141:BackColor = System.Drawing.SystemColors:Window.
      appearance141:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallar:DisplayLayout:Appearance = appearance141.
      THIS-OBJECT:GridMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance142:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance142:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance142:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance142:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:Appearance = appearance142.
      appearance143:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance143.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance144:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance144:BackColor2 = System.Drawing.SystemColors:Control.
      appearance144:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance144:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance144.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance145:BackColor = System.Drawing.SystemColors:Window.
      appearance145:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveCellAppearance = appearance145.
      appearance146:BackColor = System.Drawing.SystemColors:Highlight.
      appearance146:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveRowAppearance = appearance146.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance147:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CardAreaAppearance = appearance147.
      appearance148:BorderColor = System.Drawing.Color:Silver.
      appearance148:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellAppearance = appearance148.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellPadding = 0.
      appearance149:BackColor = System.Drawing.SystemColors:Control.
      appearance149:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance149:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance149:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance149:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:GroupByRowAppearance = appearance149.
      appearance150:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderAppearance = appearance150.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance151:BackColor = System.Drawing.SystemColors:Window.
      appearance151:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowAppearance = appearance151.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance152:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance152.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallar:GuruContext = ?.
      THIS-OBJECT:GridMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallar:Name = "GridMallar".
      THIS-OBJECT:GridMallar:Size = NEW System.Drawing.Size(321, 272).
      THIS-OBJECT:GridMallar:TabIndex = 0.
      THIS-OBJECT:GridMallar:Text = "grid1".
      THIS-OBJECT:GridMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMallar_GuruAfterDropped).
      /*  */
      /* ButtonBackMarkMallar */
      /*  */
      appearance153:Image = CAST(resources:GetObject("appearance153.Image"), System.Object).
      THIS-OBJECT:ButtonBackMarkMallar:Appearance = appearance153.
      THIS-OBJECT:ButtonBackMarkMallar:Location = NEW System.Drawing.Point(0, 54).
      THIS-OBJECT:ButtonBackMarkMallar:Name = "ButtonBackMarkMallar".
      THIS-OBJECT:ButtonBackMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackMarkMallar:TabIndex = 13.
      THIS-OBJECT:ButtonBackMarkMallar:Text = "Markerade bort".
      THIS-OBJECT:ButtonBackMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonBackMarkMallar_Click).
      /*  */
      /* ButtonOverMarkMallar */
      /*  */
      appearance154:Image = CAST(resources:GetObject("appearance154.Image"), System.Object).
      THIS-OBJECT:ButtonOverMarkMallar:Appearance = appearance154.
      THIS-OBJECT:ButtonOverMarkMallar:Location = NEW System.Drawing.Point(0, 23).
      THIS-OBJECT:ButtonOverMarkMallar:Name = "ButtonOverMarkMallar".
      THIS-OBJECT:ButtonOverMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverMarkMallar:TabIndex = 11.
      THIS-OBJECT:ButtonOverMarkMallar:Text = "Markerade väljs".
      THIS-OBJECT:ButtonOverMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonOverMarkMallar_Click).
      /*  */
      /* GridMallarKoder */
      /*  */
      appearance155:BackColor = System.Drawing.SystemColors:Window.
      appearance155:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Appearance = appearance155.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance156:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance156:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance156:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance156:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:Appearance = appearance156.
      appearance157:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance157.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance158:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance158:BackColor2 = System.Drawing.SystemColors:Control.
      appearance158:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance158:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance158.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance159:BackColor = System.Drawing.SystemColors:Window.
      appearance159:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveCellAppearance = appearance159.
      appearance160:BackColor = System.Drawing.SystemColors:Highlight.
      appearance160:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveRowAppearance = appearance160.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance161:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CardAreaAppearance = appearance161.
      appearance162:BorderColor = System.Drawing.Color:Silver.
      appearance162:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellAppearance = appearance162.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellPadding = 0.
      appearance163:BackColor = System.Drawing.SystemColors:Control.
      appearance163:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance163:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance163:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance163:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:GroupByRowAppearance = appearance163.
      appearance164:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderAppearance = appearance164.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance165:BackColor = System.Drawing.SystemColors:Window.
      appearance165:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowAppearance = appearance165.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance166:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance166.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallarKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallarKoder:GuruContext = ?.
      THIS-OBJECT:GridMallarKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallarKoder:Name = "GridMallarKoder".
      THIS-OBJECT:GridMallarKoder:Size = NEW System.Drawing.Size(350, 277).
      THIS-OBJECT:GridMallarKoder:TabIndex = 2.
      THIS-OBJECT:GridMallarKoder:Text = "grid1".
      /*  */
      /* GridValdaMallar */
      /*  */
      THIS-OBJECT:GridValdaMallar:AllowDrop = TRUE.
      appearance167:BackColor = System.Drawing.SystemColors:Window.
      appearance167:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Appearance = appearance167.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance168:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance168:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance168:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance168:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:Appearance = appearance168.
      appearance169:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance169.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance170:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance170:BackColor2 = System.Drawing.SystemColors:Control.
      appearance170:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance170:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance170.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance171:BackColor = System.Drawing.SystemColors:Window.
      appearance171:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveCellAppearance = appearance171.
      appearance172:BackColor = System.Drawing.SystemColors:Highlight.
      appearance172:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveRowAppearance = appearance172.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance173:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CardAreaAppearance = appearance173.
      appearance174:BorderColor = System.Drawing.Color:Silver.
      appearance174:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellAppearance = appearance174.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellPadding = 0.
      appearance175:BackColor = System.Drawing.SystemColors:Control.
      appearance175:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance175:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance175:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance175:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:GroupByRowAppearance = appearance175.
      appearance176:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderAppearance = appearance176.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance177:BackColor = System.Drawing.SystemColors:Window.
      appearance177:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowAppearance = appearance177.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance178:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance178.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaMallar:GuruContext = ?.
      THIS-OBJECT:GridValdaMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaMallar:Name = "GridValdaMallar".
      THIS-OBJECT:GridValdaMallar:Size = NEW System.Drawing.Size(346, 553).
      THIS-OBJECT:GridValdaMallar:TabIndex = 1.
      THIS-OBJECT:GridValdaMallar:Text = "grid1".
      THIS-OBJECT:GridValdaMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaMallar_GuruAfterDropped).
      /*  */
      /* splitContainerImpBild */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpBild:Name = "splitContainerImpBild".
      /*  */
      /* splitContainerImpBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Panel1:Controls:Add(THIS-OBJECT:MallarlStrip).
      /*  */
      /* splitContainerImpBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerImpBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerImpBild:Size = NEW System.Drawing.Size(441, 553).
      THIS-OBJECT:splitContainerImpBild:SplitterDistance = 88.
      THIS-OBJECT:splitContainerImpBild:TabIndex = 0.
      /*  */
      /* MallarlStrip */
      /*  */
      THIS-OBJECT:MallarlStrip:ImageScalingSize = NEW System.Drawing.Size(32, 32).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:ButtonMallarStrip.
      arrayvar1[2] = THIS-OBJECT:ButtonExcelStrip.
      THIS-OBJECT:MallarlStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:MallarlStrip:LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle:Table.
      THIS-OBJECT:MallarlStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:MallarlStrip:Name = "MallarlStrip".
      THIS-OBJECT:MallarlStrip:Size = NEW System.Drawing.Size(88, 78).
      THIS-OBJECT:MallarlStrip:TabIndex = 17.
      THIS-OBJECT:MallarlStrip:Text = "toolStrip1".
      /*  */
      /* ButtonMallarStrip */
      /*  */
      THIS-OBJECT:ButtonMallarStrip:Image = CAST(resources:GetObject("ButtonMallarStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonMallarStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonMallarStrip:Name = "ButtonMallarStrip".
      THIS-OBJECT:ButtonMallarStrip:Size = NEW System.Drawing.Size(116, 36).
      THIS-OBJECT:ButtonMallarStrip:Text = "Koppla mallar".
      THIS-OBJECT:ButtonMallarStrip:Click:Subscribe(THIS-OBJECT:ButtonMallarStrip_Click).
      /*  */
      /* ButtonExcelStrip */
      /*  */
      THIS-OBJECT:ButtonExcelStrip:Image = CAST(resources:GetObject("ButtonExcelStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonExcelStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonExcelStrip:Name = "ButtonExcelStrip".
      THIS-OBJECT:ButtonExcelStrip:Size = NEW System.Drawing.Size(108, 36).
      THIS-OBJECT:ButtonExcelStrip:Text = "Import Excel".
      THIS-OBJECT:ButtonExcelStrip:Click:Subscribe(THIS-OBJECT:ButtonExcelStrip_Click).
      /*  */
      /* Tabtidlage */
      /*  */
      THIS-OBJECT:Tabtidlage:Controls:Add(THIS-OBJECT:splitContainerTidlage).
      THIS-OBJECT:Tabtidlage:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:Tabtidlage:Name = "Tabtidlage".
      THIS-OBJECT:Tabtidlage:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerTidlage */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerTidlage:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerTidlage:Name = "splitContainerTidlage".
      /*  */
      /* splitContainerTidlage.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Panel1:Controls:Add(THIS-OBJECT:GridTidlage).
      /*  */
      /* splitContainerTidlage.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerTidlage.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerTidlage:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerTidlage:SplitterDistance = 763.
      THIS-OBJECT:splitContainerTidlage:TabIndex = 0.
      /*  */
      /* GridTidlage */
      /*  */
      appearance179:BackColor = System.Drawing.SystemColors:Window.
      appearance179:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridTidlage:DisplayLayout:Appearance = appearance179.
      THIS-OBJECT:GridTidlage:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridTidlage:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance180:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance180:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance180:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance180:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:Appearance = appearance180.
      appearance181:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:BandLabelAppearance = appearance181.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance182:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance182:BackColor2 = System.Drawing.SystemColors:Control.
      appearance182:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance182:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:PromptAppearance = appearance182.
      THIS-OBJECT:GridTidlage:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridTidlage:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance183:BackColor = System.Drawing.SystemColors:Window.
      appearance183:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveCellAppearance = appearance183.
      appearance184:BackColor = System.Drawing.SystemColors:Highlight.
      appearance184:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveRowAppearance = appearance184.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance185:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CardAreaAppearance = appearance185.
      appearance186:BorderColor = System.Drawing.Color:Silver.
      appearance186:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellAppearance = appearance186.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellPadding = 0.
      appearance187:BackColor = System.Drawing.SystemColors:Control.
      appearance187:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance187:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance187:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance187:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:GroupByRowAppearance = appearance187.
      appearance188:TextHAlignAsString = "Left".
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderAppearance = appearance188.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance189:BackColor = System.Drawing.SystemColors:Window.
      appearance189:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:RowAppearance = appearance189.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance190:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:TemplateAddRowAppearance = appearance190.
      THIS-OBJECT:GridTidlage:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridTidlage:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridTidlage:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridTidlage:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridTidlage:GuruContext = ?.
      THIS-OBJECT:GridTidlage:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridTidlage:Name = "GridTidlage".
      THIS-OBJECT:GridTidlage:Size = NEW System.Drawing.Size(763, 553).
      THIS-OBJECT:GridTidlage:TabIndex = 0.
      THIS-OBJECT:GridTidlage:Text = "grid1".
      THIS-OBJECT:GridTidlage:AfterCellUpdate:Subscribe(THIS-OBJECT:GridTidlage_AfterCellUpdate).
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
      /* splitContainerFelMedd.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Panel1:Controls:Add(THIS-OBJECT:GridFelMedd).
      /*  */
      /* splitContainerFelMedd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerFelMedd.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerFelMedd:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerFelMedd:SplitterDistance = 860.
      THIS-OBJECT:splitContainerFelMedd:TabIndex = 1.
      /*  */
      /* GridFelMedd */
      /*  */
      appearance191:BackColor = System.Drawing.SystemColors:Window.
      appearance191:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Appearance = appearance191.
      THIS-OBJECT:GridFelMedd:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFelMedd:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance192:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance192:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance192:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance192:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:Appearance = appearance192.
      appearance193:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BandLabelAppearance = appearance193.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance194:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance194:BackColor2 = System.Drawing.SystemColors:Control.
      appearance194:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance194:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:PromptAppearance = appearance194.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance195:BackColor = System.Drawing.SystemColors:Window.
      appearance195:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveCellAppearance = appearance195.
      appearance196:BackColor = System.Drawing.SystemColors:Highlight.
      appearance196:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveRowAppearance = appearance196.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance197:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CardAreaAppearance = appearance197.
      appearance198:BorderColor = System.Drawing.Color:Silver.
      appearance198:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellAppearance = appearance198.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellPadding = 0.
      appearance199:BackColor = System.Drawing.SystemColors:Control.
      appearance199:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance199:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance199:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance199:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:GroupByRowAppearance = appearance199.
      appearance200:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderAppearance = appearance200.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance201:BackColor = System.Drawing.SystemColors:Window.
      appearance201:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowAppearance = appearance201.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance202:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:TemplateAddRowAppearance = appearance202.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFelMedd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFelMedd:GuruContext = ?.
      THIS-OBJECT:GridFelMedd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFelMedd:Name = "GridFelMedd".
      THIS-OBJECT:GridFelMedd:Size = NEW System.Drawing.Size(860, 553).
      THIS-OBJECT:GridFelMedd:TabIndex = 0.
      THIS-OBJECT:GridFelMedd:Text = "grid1".
      /*  */
      /* TidStatus */
      /*  */
      THIS-OBJECT:TidStatus:Controls:Add(THIS-OBJECT:splitContainerStatus).
      THIS-OBJECT:TidStatus:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TidStatus:Name = "TidStatus".
      THIS-OBJECT:TidStatus:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerStatus */
      /*  */
      THIS-OBJECT:splitContainerStatus:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStatus:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStatus:Name = "splitContainerStatus".
      /*  */
      /* splitContainerStatus.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStatus:Panel1:Controls:Add(THIS-OBJECT:GridStatus).
      /*  */
      /* splitContainerStatus.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerStatus.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerStatus:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerStatus:SplitterDistance = 583.
      THIS-OBJECT:splitContainerStatus:TabIndex = 0.
      /*  */
      /* GridStatus */
      /*  */
      appearance203:BackColor = System.Drawing.SystemColors:Window.
      appearance203:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridStatus:DisplayLayout:Appearance = appearance203.
      THIS-OBJECT:GridStatus:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridStatus:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance204:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance204:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance204:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance204:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:Appearance = appearance204.
      appearance205:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:BandLabelAppearance = appearance205.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance206:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance206:BackColor2 = System.Drawing.SystemColors:Control.
      appearance206:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance206:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:PromptAppearance = appearance206.
      THIS-OBJECT:GridStatus:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridStatus:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance207:BackColor = System.Drawing.SystemColors:Window.
      appearance207:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveCellAppearance = appearance207.
      appearance208:BackColor = System.Drawing.SystemColors:Highlight.
      appearance208:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveRowAppearance = appearance208.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance209:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CardAreaAppearance = appearance209.
      appearance210:BorderColor = System.Drawing.Color:Silver.
      appearance210:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellAppearance = appearance210.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellPadding = 0.
      appearance211:BackColor = System.Drawing.SystemColors:Control.
      appearance211:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance211:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance211:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance211:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:GroupByRowAppearance = appearance211.
      appearance212:TextHAlignAsString = "Left".
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderAppearance = appearance212.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance213:BackColor = System.Drawing.SystemColors:Window.
      appearance213:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:RowAppearance = appearance213.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance214:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:TemplateAddRowAppearance = appearance214.
      THIS-OBJECT:GridStatus:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridStatus:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridStatus:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridStatus:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridStatus:GuruContext = ?.
      THIS-OBJECT:GridStatus:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridStatus:Name = "GridStatus".
      THIS-OBJECT:GridStatus:Size = NEW System.Drawing.Size(583, 553).
      THIS-OBJECT:GridStatus:TabIndex = 0.
      THIS-OBJECT:GridStatus:Text = "grid1".
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
      appearance215:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance215.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabControlsPage).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabMateriel).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabImportMallar).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkylHuvud).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabEgna).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFelmedd).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:Tabtidlage).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TidStatus).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:TabControlsPage.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManager:TabIndex = 44.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance216:Image = CAST(resources:GetObject("appearance216.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance216.
      appearance217:Image = CAST(resources:GetObject("appearance217.Image"), System.Object).
      ultraTab13:Appearance = appearance217.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:TabKalkylHuvud.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Info".
      appearance218:Image = CAST(resources:GetObject("appearance218.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance218.
      appearance219:Image = CAST(resources:GetObject("appearance219.Image"), System.Object).
      ultraTab14:Appearance = appearance219.
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:TabKalkyl.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Ärende".
      appearance220:Image = CAST(resources:GetObject("appearance220.Image"), System.Object).
      ultraTab15:ActiveAppearance = appearance220.
      appearance221:Image = CAST(resources:GetObject("appearance221.Image"), System.Object).
      ultraTab15:Appearance = appearance221.
      appearance222:BackColor = System.Drawing.SystemColors:Control.
      ultraTab15:ClientAreaAppearance = appearance222.
      ultraTab15:Key = "Egna koder".
      ultraTab15:TabPage = THIS-OBJECT:TabEgna.
      ultraTab15:Text = "".
      ultraTab15:ToolTipText = "Gör Egna koder".
      appearance223:Image = CAST(resources:GetObject("appearance223.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance223.
      appearance224:Image = CAST(resources:GetObject("appearance224.Image"), System.Object).
      ultraTab16:Appearance = appearance224.
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "Visa ".
      appearance225:Image = CAST(resources:GetObject("appearance225.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance225.
      appearance226:Image = CAST(resources:GetObject("appearance226.Image"), System.Object).
      ultraTab19:Appearance = appearance226.
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "Eget Materiel".
      appearance227:Image = CAST(resources:GetObject("appearance227.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance227.
      appearance228:BackColor = System.Drawing.Color:Transparent.
      appearance228:Image = CAST(resources:GetObject("appearance228.Image"), System.Object).
      ultraTab22:Appearance = appearance228.
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "Import/Mallar".
      appearance229:Image = CAST(resources:GetObject("appearance229.Image"), System.Object).
      ultraTab3:ActiveAppearance = appearance229.
      appearance230:Image = CAST(resources:GetObject("appearance230.Image"), System.Object).
      ultraTab3:Appearance = appearance230.
      ultraTab3:Key = "Tidlägen".
      ultraTab3:TabPage = THIS-OBJECT:Tabtidlage.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Tidlägen".
      appearance231:Image = CAST(resources:GetObject("appearance231.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance231.
      appearance232:Image = CAST(resources:GetObject("appearance232.Image"), System.Object).
      ultraTab1:Appearance = appearance232.
      ultraTab1:Key = "FelMedd".
      ultraTab1:TabPage = THIS-OBJECT:TabFelmedd.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Felmeddelanden".
      appearance233:Image = CAST(resources:GetObject("appearance233.Image"), System.Object).
      ultraTab4:ActiveAppearance = appearance233.
      appearance234:Image = CAST(resources:GetObject("appearance234.Image"), System.Object).
      ultraTab4:Appearance = appearance234.
      ultraTab4:Key = "Status".
      ultraTab4:TabPage = THIS-OBJECT:TidStatus.
      ultraTab4:Text = "".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 9 NO-UNDO.
      arrayvar2[1] = ultraTab13.
      arrayvar2[2] = ultraTab14.
      arrayvar2[3] = ultraTab15.
      arrayvar2[4] = ultraTab16.
      arrayvar2[5] = ultraTab19.
      arrayvar2[6] = ultraTab22.
      arrayvar2[7] = ultraTab3.
      arrayvar2[8] = ultraTab1.
      arrayvar2[9] = ultraTab4.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar2).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanged).
      /*  */
      /* TabControlsPage */
      /*  */
      THIS-OBJECT:TabControlsPage:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabControlsPage:Name = "TabControlsPage".
      THIS-OBJECT:TabControlsPage:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* Arende */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "Arende".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabKalkylHuvud:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerHuv, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuv:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerHuvPerslista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuvPerslista:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkArbkoder:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkLopnr:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkFri:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkFri:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabEgna:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabVisa:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelWaiting:ResumeLayout(FALSE).
      THIS-OBJECT:TabMateriel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrlValj, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlEget:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlEget:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrlEget, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlEget:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridEgetMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:splitContainerEgetMtrlBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgetMtrlBild:ResumeLayout(FALSE).
      THIS-OBJECT:TabImportMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImport, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImport:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpVal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpVal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpVal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpVal:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpBild:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerImpBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpBild:ResumeLayout(FALSE).
      THIS-OBJECT:MallarlStrip:ResumeLayout(FALSE).
      THIS-OBJECT:MallarlStrip:PerformLayout().
      THIS-OBJECT:Tabtidlage:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerTidlage:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerTidlage, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerTidlage:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridTidlage, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabFelmedd:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFelMedd:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFelMedd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFelMedd:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TidStatus:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStatus:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStatus, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStatus:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridStatus, System.ComponentModel.ISupportInitialize):EndInit().
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
         THIS-OBJECT:GridEgnaKoder:GuruTom().
         THIS-OBJECT:GridValdaUnderKoder:GuruTom().
      END.   
      IF cTo = "Visa Kalkyl" THEN DO:
         IF THIS-OBJECT:WasExcelShown NE TRUE THEN DO:
            THIS-OBJECT:VisningKalkIni(OUTPUT path).
            THIS-OBJECT:GenereraKalkylVisning().
            THIS-OBJECT:WasExcelShown = TRUE.                 
              
         END.         
         THIS-OBJECT:RibbonTabVisning:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabVisning:Visible = FALSE.
      END.      
      IF cTo = "Kalkyl" THEN DO:         
         THIS-OBJECT:GridKalkylKoder:GuruReopen().
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END. 
                
      IF cTo = "Egna koder" THEN DO:

         /*Robin 3/6/13 ändringar till GurufirstRow*/
         THIS-OBJECT:GridEgnaKoder:GuruTom().
         THIS-OBJECT:GridValdaUnderKoder:GuruTom().
         THIS-OBJECT:GridEgnaKoder:GuruReopen().
         THIS-OBJECT:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:GridValdaUnderKoder:GuruFirstRow().
         THIS-OBJECT:GridUnderKoder:ValueChanged().
         FilterUnderKoder().
         /*
         THIS-OBJECT:GridEgnaKoder:ValueChanged().
         THIS-OBJECT:GridValdaUnderKoder:ValueChanged().
         */
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
            THIS-OBJECT:GridMallar:GuruReopen().
            THIS-OBJECT:GridMallar:GuruFirstRow().
         END.   
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.  
      END.
                
      IF cTo = "Felmedd" THEN DO:
         THIS-OBJECT:GridFelMedd:GuruReopen().
         THIS-OBJECT:GridFelMedd:GuruFirstRow().           
      END.
      
      PROCESS EVENTS.
   END METHOD.
   METHOD PUBLIC VOID VisningKalkIni( OUTPUT path AS CHARACTER).
      IF THIS-OBJECT:VisaExcel:UseMsInterop = TRUE THEN DO:         
         THIS-OBJECT:SetVisningsBild(0).
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
         THIS-OBJECT:VisaExcel:InteropInitialize(path).  
         /*      
         THIS-OBJECT:VisaExcel:Fix().
         */
      END.   
      ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).    
   END METHOD.
   METHOD PUBLIC VOID Kill().
      
   END METHOD.

   METHOD PRIVATE VOID InitializeForm ():
   /* FINNS I * 2GURUJUNI*/     
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID SkapaNyEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      THIS-OBJECT:DontCheck = TRUE.
      /*Robin 31/5/13 syncman*/
      THIS-OBJECT:GridEgnaKoder:SyncMan().
      rid = THIS-OBJECT:Root:DatabaseManager:Arende:CreateEgenKod().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).
      THIS-OBJECT:GridEgnaKoder:GuruCellValjare("BENAMNING").       
      THIS-OBJECT:DontCheck= FALSE.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      /*Robin 31/5/13 syncman*/
      /*Robin felhantering om ingen row aktiv 3/6/13*/
      THIS-OBJECT:GridEgnaKoder:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN DO:
         THIS-OBJECT:GridEgnaKoder:SyncMan().
         /*Robin, Om inga kvar ändrat 4/6/13*/
         IF THIS-OBJECT:GridEgnaKoder:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:Root:DatabaseManager:Arende:RemoveEgenKod().
            GridEgnaKoder:GuruBortrader(FALSE).
            THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
            THIS-OBJECT:FilterUnderKoder().
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

   METHOD PUBLIC VOID TabRefreshKalkylHuvud():
      
   END METHOD.

   METHOD PRIVATE VOID ValueArbetsKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:GridLopKoder:GuruFirstrow().
         THIS-OBJECT:ValueLopnrCh(?,?). 
      END.
   END METHOD.
   
   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN  DO:
         THIS-OBJECT:GridMallarKoder:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:GridMallarKoder:GuruFirstrow().
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
         THIS-OBJECT:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueKalkKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      DEFINE VARIABLE q     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE exrid AS RECID     NO-UNDO.
      /*Se filtrera i Grid*/ 
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN DO:
         IF THIS-OBJECT:DontCheck = TRUE THEN.
         ELSE DO: 
         END.
         IF KoderTTh:AVAILABLE THEN THIS-OBJECT:KalkylensKoderAnmarkning:Text  = THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
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
      THIS-OBJECT:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      */          
   END METHOD.
      
   METHOD PRIVATE VOID VisaTabs (INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[2] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = visavar.
      END.   
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkyl"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[4] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = visavar.
      END.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Egna koder"]:KEY = vilka THEN  THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE   = visavar.
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:KEY = vilka THEN DO:
         THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = FALSE.
         IF Guru.konstanter:kalk2sekvar[6] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = visavar.
      END.   
      IF vilka = "ALLA" THEN DO:         
            
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
         THIS-OBJECT:VisaPanelEmpty:Visible = TRUE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaExcel:Visible = FALSE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 1 THEN DO:
         THIS-OBJECT:VisaPanelWaiting:Visible = TRUE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaExcel:Visible = FALSE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
      IF i EQ 2 THEN DO:
         THIS-OBJECT:VisaExcel:Visible = TRUE.
         THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:FILL.
         THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
         THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
         THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
      END.
   END METHOD.

   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
         
      IF THIS-OBJECT:VisaExcel:Visible EQ TRUE THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN DO:
            THIS-OBJECT:GenereraKalkylVisning().   
         END.
      END.
      ELSE DO:
         THIS-OBJECT:GenereraKalkylVisning().
      END.
   END METHOD.
    
   METHOD PUBLIC VOID SetVisningExceldoc(INPUT vad AS CHARACTER, OUTPUT path AS CHARACTER):
      THIS-OBJECT:VisaExcel:direkticell = FALSE. 
      THIS-OBJECT:VisaExcel:UseMsInterop = TRUE.        
      OS-DELETE VALUE(ExelKommando) NO-ERROR.
      IF vad = "VKALK" THEN DO:
         path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xlsx").
         IF path = ? THEN path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
      END.   
      path = ExcelTemplate(INPUT path).
      ExelKommando = path.            
   END METHOD.
   
   METHOD PUBLIC VOID GenereraKalkylVisning():
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
      ASSIGN 
         cSenasteArbKod     = ""
         iSenasteLopNr      = 0        
         iSenasteMatris     = 0        
         cSenasteMtrl       = ""       
         iSenasteMtrlMatris = 0.
        
      
      IF THIS-OBJECT:VisaExcel:UseMsInterop = TRUE THEN DO:
         IF PROVERSION BEGINS "10" THEN DO:
            Guru.Konstanter:EuropeanSet().            
         END.
         ELSE DO:
            Guru.Konstanter:AmericanSet().
         END.   
         IF THIS-OBJECT:WasExcelShown = FALSE THEN THIS-OBJECT:VisningKalkIni(OUTPUT path).
         ELSE THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
      END.
      ELSE DO:
         Guru.Konstanter:AmericanSet().
         THIS-OBJECT:SetVisningExceldoc(INPUT "VKALK", OUTPUT path).
      END.   
      THIS-OBJECT:WasExcelShown = TRUE.
      
      /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i slutet också! */
  /*    DEFINE VARIABLE TimerTest AS System.Diagnostics.Stopwatch NO-UNDO.
      TimerTest = NEW System.Diagnostics.Stopwatch().
      TimerTest:Reset().
      TimerTest:Start().*/
     
      
      THIS-OBJECT:VisaExcel:iSistaColnum = 1.
      IF THIS-OBJECT:VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:VisaExcel:ScreenUpp(FALSE).
         THIS-OBJECT:SetVisningsBild(1).
         THIS-OBJECT:VisaExcel:Reload(path).         
      END.  
      ELSE THIS-OBJECT:VisaExcel:OpenExcel(path).
     
     
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
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      IF link NE ? THEN DO:
         THIS-OBJECT:VisaExcel:PageSetUppHeaderImage(INPUT 1, INPUT link). 
         THIS-OBJECT:VisaExcel:iRad = 1.
      END.
     Guru.Konstanter:JurAvdTTh:FIND-FIRST("WHERE AVDELNINGNR = " + Guru.Konstanter:OmradeTTh:BUFFER-FIELD("AVDELNINGNR"):BUFFER-VALUE) NO-ERROR.
      IF Guru.Konstanter:JurAvdTTh:AVAILABLE THEN DO:
         Guru.Konstanter:JurPersTTh:FIND-FIRST("WHERE JUDID = '" + STRING(Guru.Konstanter:JurAvdTTh:BUFFER-FIELD("JUDID"):BUFFER-VALUE) + "'") NO-ERROR.         
      END.
      
      IF Guru.Konstanter:JurPersTTh:AVAILABLE THEN DO:                             
         THIS-OBJECT:VisaExcel:PageSetUppHeader(INPUT 2, INPUT '&C&",Bold"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + CHR(10) +
         '&",Normal"' + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("GATUADR"):BUFFER-VALUE) + " " + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTADR"):BUFFER-VALUE) + " " + STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTNR"):BUFFER-VALUE) + CHR(10) + 
         "Telefon:" + Guru.Konstanter:JurPersTTh:BUFFER-FIELD("TELVXL"):BUFFER-VALUE + CHR(10)).
      END.   
      THIS-OBJECT:VisaExcel:PageSetUppHeader(INPUT 3, INPUT '&"Calibri,Bold"&20' + THIS-OBJECT:RibbonToolVisVisa:Text).
      THIS-OBJECT:VisaExcel:ColumnRad().
      /* Skapa huvud */
      THIS-OBJECT:ExcelHuv().    
      /*ANMÄRKNING*/
      THIS-OBJECT:ExcelAnm().
      /* --- Börja göra listan här --- */
      /* Skapa rubrikerna */
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      rubrikrad = THIS-OBJECT:VisaExcel:ColRad. 
      
      THIS-OBJECT:VisaExcel:Rowdown().  
      radefterrubrikrad = THIS-OBJECT:VisaExcel:ColRad.
      matrisrader = radefterrubrikrad.
      THIS-OBJECT:VisaExcel:Rowdup().  
      THIS-OBJECT:VisaExcel:DataOut("Kod").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Benämning").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Antal").
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Enh").
      /*LÄGG UT RUBRIKER FÖR TIMMAR*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = THIS-OBJECT:VisaExcel:cColname.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      kostnadstartcol = "".
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = THIS-OBJECT:VisaExcel:cColname.
         IF kostnadstartcol = "" THEN 
         DO:
            IF THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "EA" THEN.
            ELSE kostnadstartcol = THIS-OBJECT:VisaExcel:cColname.
         END.   
         qH:GET-NEXT(). 
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:DataOut("Summa").
      /* Gör rubriker bold */
      THIS-OBJECT:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:VisaExcel:iRad) ,THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ). 
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:Border("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
      THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
      /*OM MATRIS*/
      IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE. 
      ELSE 
      DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE + " WHERE arendenumtt.MATRIS = " + STRING(VisMatris()).      
      END.    
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.       
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
      qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:           
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().      
         /*    GRUPPERA MATRIS*/    
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO: 
            THIS-OBJECT:ExcelGruppMatris(INPUT 1, INPUT matrisrader, INPUT kostnadstartcol ).
            IF iSenasteMatris = 0 THEN matrisrader = matrisrader.
            ELSE IF iSenasteMatris NE THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE THEN DO:
               matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:VisaExcel:Getrad(THIS-OBJECT:VisaExcel:ColRad))).
            END.   
            /*OM NY MATRIS*/
            iSenasteMatris = THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
         END.
         /*GRUPPERA KODER*/
         
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppera") = "yes" THEN DO:
            THIS-OBJECT:ExcelGruppKoder().         
         END.
         /*SUMMERA SAMMA KOD*/
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes" THEN DO:
            IF VisSummera(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
         END.    
         ELSE THIS-OBJECT:VisaExcel:Rowdown().
         /*UT MED ARB KOD MM*/  
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
         /*UT MED TIMMARNA*/
         queryvar =  "FOR EACH " + THIS-OBJECT:kalkantalTTh:TABLE + " WHERE kalkantal.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN
               THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).                  
            subqH:GET-NEXT(NO-LOCK).
         END.
         /*UT MED KOSTNAD*/
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            findfel = THIS-OBJECT:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:
                  THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE)).
               END.
               ELSE THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).    
            END.
            /*UT MED EA*/
            THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "EA"') NO-ERROR.
            IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
               IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 1 THEN DO:
                  THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("FRIEAMANGD"):BUFFER-VALUE)).
               END.
               ELSE THIS-OBJECT:VisaExcel:DataOut(TRUE,THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE)).             
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.           
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).         
         /*SUMMERA HORISONT*/
         THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
         THIS-OBJECT:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad), THIS-OBJECT:VisaExcel:ColRad,0).
         /*RISKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 2 THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(ROUND(DECIMAL(THIS-OBJECT:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100),0))).
            END.   
         END.   
         /*VINSTKALKYL*/
         IF INTEGER(THIS-OBJECT:RibbonToolVisVisa:SelectedIndex) = 3 THEN DO:          
            IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(ROUND(DECIMAL(THIS-OBJECT:VisaExcel:GetData()) * (1 + (THIS-OBJECT:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100),0))).
            END.   
         END.  
         qH:GET-NEXT(NO-LOCK).
      END.
      /*GRUPPERA MATRIS*/     
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
         THIS-OBJECT:ExcelGruppMatris(INPUT 2,INPUT matrisrader, INPUT kostnadstartcol).       
         matrisrader = "A" + STRING(INTEGER(THIS-OBJECT:VisaExcel:Getrad(THIS-OBJECT:VisaExcel:ColRad)) + 1).                                
      END.
      /*EGET MATRIEL SUMMA*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN.
      ELSE DO:        
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
            THEN DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:  
                  THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  IF VisMatris() = 0 THEN                             
                     THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(?))).
                  ELSE THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl + THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(VisMatris()))).
                  THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
               END.                       
            END.            
         END.
      END.
      /*Berednings mtrl*/   
      IF THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:VisaExcel:cColname = "B".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:DataOut("Materielkostnad från beredning").
         THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
         IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlHmt(OUTPUT totbermtrl).
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(totbermtrl)).
               THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).              
            END.                   
         END.           
      END.
      
      
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      /* 
    Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(),"C" + STRING(THIS-OBJECT:VisaExcel:iRad) , "N" + STRING(THIS-OBJECT:VisaExcel:iRad) ):Merge( true ).
    */
      
      /*KOLUMNBREDD*/
      THIS-OBJECT:kalkrubrikTTh:FIND-LAST("WHERE KOSTTIMM = TRUE  USE-INDEX ORDNING").
      THIS-OBJECT:VisaExcel:cColname = THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE.
      THIS-OBJECT:VisaExcel:ColRight().
      THIS-OBJECT:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:VisaExcel:iRad),45).
      THIS-OBJECT:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:VisaExcel:iRad),10).
      THIS-OBJECT:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:VisaExcel:iRad),10).
      THIS-OBJECT:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:VisaExcel:iRad),15).
      THIS-OBJECT:VisaExcel:BordHoriz(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:VisaExcel:BordHorizW(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlBorderWeight:xlThin"). 
      THIS-OBJECT:VisaExcel:BordVert(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlLineStyle:xlDot").
      THIS-OBJECT:VisaExcel:BordVertW(radefterrubrikrad,THIS-OBJECT:VisaExcel:ColRad,"XlBorderWeight:xlThin").
      
      /*SUMMERA ALLA KOLUMNER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN.
      ELSE DO:
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "E".
         THIS-OBJECT:VisaExcel:ColumnRad().
         REPEAT:
            THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(radefterrubrikrad), THIS-OBJECT:VisaExcel:ColRad,0).
            IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:VisaExcel:ColRight().
         END.              
      END.
      /*ANDERS
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(),rubrikrad , "C" + STRING(THIS-OBJECT:VisaExcel:iRad) ):Merge( true ).
      */
      
      /*FONTER*/
      THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
      THIS-OBJECT:VisaExcel:Border(rubrikrad, THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad)). 
      
         
      /*BILAGA EGNet mtrl*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisMatSpec") = "yes" THEN DO:
         THIS-OBJECT:ExcelBilagor(3).      
      END.   
      /*BILAGA mtrl från ber*/
      IF THIS-OBJECT:Root:DatabaseManager:Arende:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:ExcelBilagor(4).      
      END.
      THIS-OBJECT:VisaExcel:ScreenUpp(TRUE).
      THIS-OBJECT:VisaExcel:valueDataOutTT().
      
      IF VisaExcel:UseMsInterop = TRUE THEN DO:
         THIS-OBJECT:VisaExcel:ViewType(INPUT 3).
         THIS-OBJECT:VisaExcel:ExcelAppHandle:ActiveWorkbook:Activate().
         THIS-OBJECT:VisaExcel:Fix().
         THIS-OBJECT:SetVisningsBild(2).
      END.   
      ELSE THIS-OBJECT:VisaExcel:SlutExcel(3).
      Guru.Konstanter:EuropeanAmericanReset().
      
      /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i början också! */
     /* MESSAGE TimerTest:ElapsedMilliseconds
         VIEW-AS ALERT-BOX.*/ 
   END METHOD.

   METHOD PRIVATE VOID ExcelHuv():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         /*
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:RibbonToolVisVisa:Text).
         */
         THIS-OBJECT:VisaExcel:Rowdown().             
         THIS-OBJECT:VisaExcel:DataOut(Guru.GlobalaVariabler:KalkArendeText + "-nr.: ").        
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkNrvar)).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:Root:DatabaseManager:Arende:ArendeAonr()).
         IF VisMatris() = 0 THEN. 
         ELSE DO:
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(GetMatrisName() + " " + STRING(VisMatris())).
         END. 
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).   
            
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:Rowdown().   
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:gomrl + ": ").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:Omradevar)).
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().      
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelAnm():
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN 
      DO:
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:DataOut("Anmärkning: ").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
         THIS-OBJECT:VisaExcel:FontExcel("A1",THIS-OBJECT:VisaExcel:ColRad,"",0,TRUE ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
      END.
   END METHOD.

   METHOD PRIVATE VOID ExcelGruppMatris(vad AS INTEGER,matrisrader AS CHARACTER,kostnadstartcol AS CHARACTER):
      IF vad = 1 THEN DO:
         /*LÄGGER UT MATRIS RADEN*/
         IF VisGruppMatris(THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN DO:
            /*EGET MATRIEL SUMMA*/
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
               THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3"
               THEN DO:
                  THIS-OBJECT:VisaExcel:Rowdown().
                  THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
                  IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                     THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                     THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(iSenasteMatris))).                  
                  END.              
                  THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
               END.
            END.
            THIS-OBJECT:VisaExcel:Rowdown().            
            THIS-OBJECT:VisaExcel:DataOut("Summa " + GetMatrisName() + " " + STRING(iSenasteMatris)). 
            THIS-OBJECT:VisaExcel:cColname = "E".
            THIS-OBJECT:VisaExcel:ColumnRad().
            /*SUMMERA ALLA KOLUMNER*/
            REPEAT:
               THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(matrisrader), THIS-OBJECT:VisaExcel:ColRad,0).
               IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
               THIS-OBJECT:VisaExcel:ColRight().
            END.
            THIS-OBJECT:VisaExcel:Rowdown().
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().                                     
         END.
      END.
      /*SISTA MATRISEN?*/
      IF vad = 2 THEN DO:
         /*EGET MATRIEL SUMMA*/
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
            THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3" 
            THEN DO:
               THIS-OBJECT:VisaExcel:Rowdown().
               THIS-OBJECT:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
               IF THIS-OBJECT:kalkrubrikTTh:AVAILABLE THEN DO:                                               
                  THIS-OBJECT:VisaExcel:DataOut("B" + STRING(THIS-OBJECT:VisaExcel:iRad),"Materielkostnad från eget materiel").
                  THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE + STRING(THIS-OBJECT:VisaExcel:iRad),STRING(THIS-OBJECT:Root:DatabaseManager:Arende:MtrlKost(iSenasteMatris))).                  
               END.              
               THIS-OBJECT:VisaExcel:Summah(kostnadstartcol + STRING(THIS-OBJECT:VisaExcel:irad),THIS-OBJECT:VisaExcel:getcolname(THIS-OBJECT:VisaExcel:iSistaColnum) + STRING(THIS-OBJECT:VisaExcel:iRad),0).
            END.
         END.
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().
         THIS-OBJECT:VisaExcel:Rowdown().            
         THIS-OBJECT:VisaExcel:DataOut("Summa " + GetMatrisName()+ " " + STRING(iSenasteMatris)). 
         THIS-OBJECT:VisaExcel:cColname = "E".
         THIS-OBJECT:VisaExcel:ColumnRad().
         /*SUMMERA ALLA KOLUMNER*/
         REPEAT:
            THIS-OBJECT:VisaExcel:Summa(THIS-OBJECT:VisaExcel:cColname + THIS-OBJECT:VisaExcel:Getrad(matrisrader), THIS-OBJECT:VisaExcel:ColRad,0).
            IF THIS-OBJECT:VisaExcel:iColnum = THIS-OBJECT:VisaExcel:iSistaColnum THEN LEAVE.
            THIS-OBJECT:VisaExcel:ColRight().
         END.  
         THIS-OBJECT:VisaExcel:Rowdown().                
      END.
   END METHOD.
   METHOD PRIVATE VOID ExcelGruppKoder():
      IF VisGruppera(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = FALSE THEN DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         IF THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            THIS-OBJECT:VisaExcel:DataOut(STRING("Egna Koder")).
         END.   
         ELSE DO:
            THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
         END.
      END.      
   END METHOD.
   
   METHOD PRIVATE VOID ExcelBilagor(vad AS INTEGER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO. 
      IF vad = 3 THEN DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("MaterielSpecifikation").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().         
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Antal").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            THIS-OBJECT:VisaExcel:DataOut(GetMatrisName()).
            THIS-OBJECT:VisaExcel:ColRight().
         END.   
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:VisaExcel:DataOut("Pris").
            THIS-OBJECT:VisaExcel:ColRight().
         END.
         THIS-OBJECT:VisaExcel:DataOut("Summa").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " BY MATRIS BY ENR". 
         END.
         ELSE IF VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " BY ENR".
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE + " WHERE MATRIS = " + STRING(VisMatris()) + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:arendemtrlTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            END.
            ELSE DO:
               IF VisSummeraMtrl(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            END.   
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().         
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE)).         
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:VisaExcel:ColRight().
            IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE)).        
               THIS-OBJECT:VisaExcel:ColRight().
            END.   
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE)).
               THIS-OBJECT:VisaExcel:ColRight().
               THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:arendemtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).            
               THIS-OBJECT:VisaExcel:ColRight().
            END.   
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      IF vad = 4 THEN DO:
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().                    
         THIS-OBJECT:VisaExcel:DataOut("Materieltillägg från beredning").
         THIS-OBJECT:VisaExcel:FontExcel(THIS-OBJECT:VisaExcel:ColRad,THIS-OBJECT:VisaExcel:ColRad,"Garamond",18,? ,0,0 ).
         THIS-OBJECT:VisaExcel:Rowdown().
         THIS-OBJECT:VisaExcel:cColname = "A".
         THIS-OBJECT:VisaExcel:ColumnRad().         
         THIS-OBJECT:VisaExcel:DataOut(Guru.Konstanter:genk).
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Benämning").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Antal").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:DataOut("Enhet").
         THIS-OBJECT:VisaExcel:ColRight().
         IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
         ELSE DO:
            THIS-OBJECT:VisaExcel:DataOut("Pris").
            THIS-OBJECT:VisaExcel:ColRight().
         END.
         THIS-OBJECT:VisaExcel:DataOut("Summa").
         THIS-OBJECT:VisaExcel:ColRight().
         THIS-OBJECT:VisaExcel:Linedubble("A"+ STRING(THIS-OBJECT:VisaExcel:iRad), THIS-OBJECT:VisaExcel:ColRad).
         queryvar = "FOR EACH " + THIS-OBJECT:berkalkmtrlttH:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:berkalkmtrltth,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            IF VisSummeraMtrl(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE),?) = FALSE THEN THIS-OBJECT:VisaExcel:Rowdown().
            THIS-OBJECT:VisaExcel:cColname = "A".
            THIS-OBJECT:VisaExcel:ColumnRad().         
            THIS-OBJECT:VisaExcel:DataOut(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).         
            THIS-OBJECT:VisaExcel:ColRight().
            THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            THIS-OBJECT:VisaExcel:ColRight().
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:VisaExcel:DataOut(STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
               THIS-OBJECT:VisaExcel:ColRight().
               THIS-OBJECT:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:berkalkmtrltth:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE)).            
               THIS-OBJECT:VisaExcel:ColRight().
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
   METHOD PRIVATE VOID ExcelImportLista(vad AS INTEGER,filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT filnamn).   
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF invar NE "" THEN DO:
            IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               ArendeimportTTh:BUFFER-CREATE().
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
               IF vad = 2 THEN DO:
                  klangd = LENGTH(invar).
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
               END.
               ELSE DO:
                  THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar).
               END.
            END.
            IF THIS-OBJECT:VisaExcel:cColname = "B" AND vad = 1 THEN DO:
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
               invar = REPLACE(invar,",",".").
               THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
            END.      
         END.
         THIS-OBJECT:VisaExcel:ColRight().
         IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:
            THIS-OBJECT:VisaExcel:cColname = "A". 
            THIS-OBJECT:VisaExcel:Rowdown().
         END.            
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:ImportLista().
   END METHOD.
   
   METHOD PRIVATE VOID ImportMallar():   
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
      THIS-OBJECT:GridValdaMallar:GuruReopen().
      THIS-OBJECT:GridKalkylKoder:GuruAvmarkera().
     /* THIS-OBJECT:GridKalkylKoder:GuruReopen().*/
     
   END METHOD.
   
   METHOD PRIVATE VOID ImportLista():   
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
            
            THIS-OBJECT:Root:DatabaseManager:Arende:CreateKodRow(INPUT KoderTTh:RECID).                       
         END.   
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Arende:TidutHmt(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:ArendeimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(64)).         
         END.   
         qH:GET-NEXT().         
      END.  
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:GridKalkylKoder:GuruFirstrow().
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
      DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
      DEFINE VARIABLE globanv   AS CHARACTER                         NO-UNDO.
      kommando = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando) NO-ERROR.
      kommando = kommando + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + THIS-OBJECT:RibbonToolVisVisa:Text + ".xls".               
            
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      RETURN kommando.
   END METHOD.
   
   METHOD PUBLIC VOID ForetagAnpass():
      THIS-OBJECT:splitContainerMtrlEget:SplitterDistance = 500.
       
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      THIS-OBJECT:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:tiduth:AVAILABLE THEN THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.
      
   END METHOD.
   METHOD PUBLIC VOID ApplyLanguage():
      THIS-OBJECT:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
   END METHOD.
    
END CLASS.
