 
 /*------------------------------------------------------------------------
    File        : KalkylAdm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 13:32:37 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.

 

CLASS Modules.Kalkyl.KalkylAdm INHERITS UserControl: 
   {KALKYLKATH.i}
   {KALKYLEXTRATAB.I}
   DEFINE VARIABLE CellChvar AS LOGICAL NO-UNDO.
   DEFINE PRIVATE VARIABLE ArbkodMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBackAnvMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBackHuvKat AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBackMall AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortPrisStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortSubKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopSubKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyDelStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyPrisStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOverAnvMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverHuvKat AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE DelKatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE GridArbKodMall AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOverMall AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFrekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKatSek AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridAnvSek AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridKatalog AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridMallar AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridLopMall AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridKatHuvDel AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSubKatDel AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSubMall AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridValdaAnvSek AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridValdMallar AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KatBildDelStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE KatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE KoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE LopnrMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE LopNrStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE LopSubBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE NyDelKatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE NyKopMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE PrisStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   /*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   
   /*grids*/
   DEFINE PRIVATE VARIABLE GridArbKoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridLopnumsub AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridLopnr AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSubKatKoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSubKatalog AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridKatPris AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridVisning AS Controls.Grid NO-UNDO.
   
	/*fönster*/   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE Rubrikanm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerArbkodMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerArbkodMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerBildSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerBtnOverMall AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatalogSubKatStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatBildDelStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatHuvKop AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatHuvKopStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKnappAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopnrMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopnrMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopsubBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerGrundKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopsubBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopSubFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMallarBas AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopSubKat AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatalog AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPrisKopDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPrisStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatKoderStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatPrisDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerValdMallarBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerValdMallarBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerVisningBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerVisningBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE SubKatKodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE SubKatMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE TabSek AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKoder AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabDelKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylAdm AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   
   /* Ribbongrupper */
   
   DEFINE         VARIABLE RibbonGroupAdm                  AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupKatalog              AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupDelKatalog           AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupPrisKatalog          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupVisaPristyp          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE         VARIABLE RibbonGroupArbetskoder          AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   DEFINE         VARIABLE RibbonGroupSpara                AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   /*ribbon ellement*/
   DEFINE         VARIABLE RibbonDummy                  AS Infragistics.Win.UltraWinToolbars.LabelTool                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmPriser            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmSek            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmFrek              AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAdmKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE RibbonToolKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
	DEFINE PUBLIC  VARIABLE RibbonToolArtal            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSek            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolStyrande            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkapad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAvslutad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*BEHÖVS EJ
   DEFINE PUBLIC  VARIABLE RibbonToolDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolKopplaDelKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   */
   DEFINE PUBLIC  VARIABLE RibbonToolDelSkapad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolDelAvslutad            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolPrisKatalog            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO. 
   DEFINE PUBLIC  VARIABLE RibbonToolSokBen            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTaEgnaKoder            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTaEgnaPriser            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE RibbonToolVisningRegel            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.   
	
	DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp            AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFilterTyp          AS Infragistics.Win.UltraWinToolbars.ComboBoxTool                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFrekvensKom        AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolTypKalk            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolLopnr              AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*Drag Dropp*/
   DEFINE         VARIABLE DGAnvand                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
   DEFINE         VARIABLE DGHuvkat                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
	DEFINE         VARIABLE DGLopMallar                      AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
	
	DEFINE PUBLIC  VARIABLE RibbonToolSparaDefs          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.	
   DEFINE PRIVATE VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE ValdMallarBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE VisningBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
	CONSTRUCTOR PUBLIC KalkylAdm ( INPUT r AS Guru.Root ):
	   SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:InitializeTTs().
      THIS-OBJECT:KalkAdmdbControl = THIS-OBJECT:Root:DatabaseManager:KalkylAdm.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
       
	END CONSTRUCTOR.

	
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBackAnvMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridValdaAnvSek:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:DontCheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveAnvSelected().
      THIS-OBJECT:ButtonBackAnvMark:GuruOver().
      THIS-OBJECT:DontCheck = FALSE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBackMall_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		/*OM INGA KVAR*/
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridValdMallar:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:DontCheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveKodMallarSelected().
      THIS-OBJECT:ButtonBackMall:GuruOver().
      THIS-OBJECT:DontCheck = FALSE.
		
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE okbort AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortKatalog(THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(60)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END.   
      THIS-OBJECT:KatalogAfterBort(). 
      THIS-OBJECT:GridKatalog:GuruBortrader(FALSE).
      THIS-OBJECT:AfterCopyKat().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:GridSubKatDel:GuruReopen().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridArbKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortArbKod(STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE)). 
      THIS-OBJECT:GridArbKoder:GuruBortrader(FALSE).
     /*
      THIS-OBJECT:RefreshArbKoder(?,?).
      THIS-OBJECT:GridArbKoder:GuruReopen().
      */
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridLopnr:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortLopnum(INPUT INTEGER(STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE))). 
      THIS-OBJECT:GridLopnr:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.
		
	END METHOD.




	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridMallar:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortMallar(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE). 
      THIS-OBJECT:GridMallar:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortPrisStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridArbKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortPris(INPUT THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,INPUT INTEGER(STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KPID"]:VALUE))). 
      THIS-OBJECT:GridKatPris:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().   
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortSubKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE okbort AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      IF THIS-OBJECT:GridSubKatDel:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortSubKatalog(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(60)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END.   
      THIS-OBJECT:KatalogAfterBort().
      THIS-OBJECT:GridSubKatDel:GuruBortrader(FALSE).
      THIS-OBJECT:AfterCopyKat().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
            
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nykatid AS INTEGER NO-UNDO.
      /*
      THIS-OBJECT:ButtonKopKat:Text = "Kopierar Vänta!".
      */
      IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyKatalog(THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE,OUTPUT nykatid). 
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(nykatid)) NO-ERROR.
      THIS-OBJECT:GridKatalog:GuruRepositionTo( THIS-OBJECT:KatalogTTh:ROWID).
      THIS-OBJECT:GridSubKatDel:GuruReopen().
      THIS-OBJECT:AfterCopyKat(). 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      /*
      THIS-OBJECT:ButtonKopKat:Text = "Kopiera Katalog".
      */
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:GridArbKoder:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyKoder(INPUT STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE)).
      THIS-OBJECT:RefreshArbKoder(?,?).
      THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.
      THIS-OBJECT:GridArbKoder:GuruRepositionTo( THIS-OBJECT:ArbetskoderTTh:ROWID).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nylopnr AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridLopnr:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyLopnum(INPUT INTEGER(STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE)),OUTPUT nylopnr).
      THIS-OBJECT:GridLopnr:GuruReopen().
      THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE LOPNR = " + STRING(nylopnr))  NO-ERROR.
      THIS-OBJECT:GridLopnr:GuruRepositionTo(THIS-OBJECT:LopposterTTh:ROWID).
      RETURN.


	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nymallar AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridMallar:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyMallar(INPUT INTEGER(STRING(THIS-OBJECT:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE)),OUTPUT nymallar).
      THIS-OBJECT:GridMallar:GuruReopen().
      THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(nymallar))  NO-ERROR.
      THIS-OBJECT:GridMallar:GuruRepositionTo(THIS-OBJECT:KalkmallHuvudtth:ROWID).
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopSubKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nysubkatid AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridSubKatDel:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopySubKatalog(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGsubID"):BUFFER-VALUE, OUTPUT nysubkatid). 
      THIS-OBJECT:GridSubKatDel:GuruReopen().
      THIS-OBJECT:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(nysubkatid)) NO-ERROR.
      THIS-OBJECT:GridSubKatDel:GuruRepositionTo( THIS-OBJECT:KatalogDeltth:ROWID).
      THIS-OBJECT:AfterCopyKat().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyDelStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nysubkatid AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NySubKatalog(OUTPUT nysubkatid). 
      THIS-OBJECT:GridSubKatDel:GuruReopen().
      THIS-OBJECT:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(nysubkatid)) NO-ERROR.
      THIS-OBJECT:GridSubKatDel:GuruRepositionTo( THIS-OBJECT:KatalogDeltth:ROWID).
      THIS-OBJECT:AfterCopyKat().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nykatid AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyKatalog(OUTPUT nykatid). 
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(nykatid)) NO-ERROR.
      THIS-OBJECT:GridKatalog:GuruRepositionTo(THIS-OBJECT:KatalogTTh:ROWID).
      THIS-OBJECT:GridSubKatDel:GuruReopen().
      THIS-OBJECT:AfterCopyKat(). 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyKoder(INPUT THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, INPUT THIS-OBJECT:GetVisaTyp()).
      THIS-OBJECT:RefreshArbKoder(?,?).
      THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.
      THIS-OBJECT:GridArbKoder:GuruRepositionTo( THIS-OBJECT:ArbetskoderTTh:ROWID).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nylopnr AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyLopnum(INPUT THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,OUTPUT nylopnr).
      THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE LOPNR = " + STRING(nylopnr))  NO-ERROR.
      THIS-OBJECT:GridLopnr:GuruRepositionTo(THIS-OBJECT:LopsubTTh:ROWID).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nymallnr AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyMall(OUTPUT nymallnr).
      THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(nymallnr))  NO-ERROR.
      THIS-OBJECT:GridMallar:GuruRepositionTo(THIS-OBJECT:KalkmallHuvudtth:ROWID).
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyPrisStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nykpid AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyPris(INPUT THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,OUTPUT nykpid).
      THIS-OBJECT:PriserTTh:FIND-FIRST("WHERE KPID = " + STRING(nykpid) + " AND KLOGSUBID = " + STRING(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE))  NO-ERROR.
      THIS-OBJECT:GridKatPris:GuruRepositionTo(THIS-OBJECT:PriserTTh:ROWID).
      
      RETURN.
	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverAnvMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:GridAnvSek:Posn() = -1 THEN RETURN.
		/*ROBIN ändring 29/5/13*/
       IF THIS-OBJECT:ButtonOverAnvMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:ButtonOverAnvMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateAnvRows().
         THIS-OBJECT:ButtonOverAnvMark:GuruOverefteruppdat().
      END.
      ELSE DO:
         DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBackHuvKat_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridKatHuvDel:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:DontCheck = TRUE.
      THIS-OBJECT:IngKatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE) +
      " AND KLOGSUBID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveKopKat().
      THIS-OBJECT:ButtonBackHuvKat:GuruOver().
      THIS-OBJECT:DontCheck = FALSE.
		
		RETURN.

	END METHOD.





	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverHuvKat_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN. 
		IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KollaKoppKatRows(INTEGER(STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE)),INTEGER(STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE))) = TRUE THEN DO:
		   THIS-OBJECT:ButtonOverHuvKat:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateKoppKatRows().
         THIS-OBJECT:ButtonOverHuvKat:GuruOverefteruppdat().
      END.
      ELSE DO:  
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(61)," ",System.Windows.Forms.MessageBoxButtons:Ok).
      END.      
		RETURN.

	END METHOD.
  /*Drag dropp*/

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverMall_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   
		IF THIS-OBJECT:GridLopMall:Posn() = -1 THEN RETURN.
      THIS-OBJECT:ButtonOverMall:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateKoderMallarRows().
      THIS-OBJECT:ButtonOverMall:GuruOverefteruppdat().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnvSek_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaAnvSek THEN DO:
         THIS-OBJECT:ButtonBackAnvMark_Click(?,?).
      END.
		RETURN.
	END METHOD.
	
	METHOD PRIVATE VOID AfterCopyKat( ):
      THIS-OBJECT:GridSubKatKoder:GuruReopen().
      THIS-OBJECT:GridSubKatKoder:GuruFirstrow().
      THIS-OBJECT:ValueGridSubKatKoderCh(?,?).
       
      RETURN.
   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString().
		RETURN.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFrekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:FrekvensTTh:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE) +
      " AND ARBKOD = '" + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["ARBKOD"]:VALUE ) + 
      "' AND LOPNR = " + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["LOPNR"]:VALUE ) +
      " AND FREKOD = '" + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["FREKOD"]:VALUE ) + 
      "' AND FREKNR = " + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["FREKNR"]:VALUE )) NO-ERROR.
      THIS-OBJECT:FrekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FrekSpara().
    	RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatHuvDel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKatalog THEN DO:
         THIS-OBJECT:ButtonOverHuvKat_Click(?,?).
      END.
		RETURN.
 
	END METHOD.
 
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatalog_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.
   
   METHOD PRIVATE VOID GridKatalog_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridKatalog:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" OR kolnamn = "SEKRETESS" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatalog_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKatHuvDel THEN DO:
         THIS-OBJECT:ButtonBackHuvKat_Click(?,?).
      END.
		RETURN.
   END METHOD.

	

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopMall_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdMallar THEN DO:
         THIS-OBJECT:ButtonBackMall_Click(?,?).
      END.
     
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridMallar_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:MallarUppdate(INPUT STRING(THIS-OBJECT:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdMallar_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      
      THIS-OBJECT:MallarValdaUppdate(INPUT STRING(THIS-OBJECT:GridValdMallar:ActiveRow:Cells["ARBKOD"]:VALUE),INPUT STRING(THIS-OBJECT:GridValdMallar:ActiveRow:Cells["LOPNR"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopMall THEN DO:
         THIS-OBJECT:ButtonOverMall_Click(?,?).
      END.
     
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MallarUppdate(INPUT mallvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      
      THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(mallvar)) NO-ERROR.
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      THIS-OBJECT:GridMallar:GuruReopen().
      
      RETURN.
   END METHOD.
	
	METHOD PUBLIC VOID MallarValdaUppdate(INPUT arbvar AS CHARACTER, INPUT lopvar AS CHARACTER,INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      
      THIS-OBJECT:KalkmallKodertth:FIND-FIRST("WHERE ARBKOD = '" + STRING(arbvar) + "' AND LOPNR = " + STRING(lopvar)) NO-ERROR.
      IF THIS-OBJECT:KalkmallKodertth:AVAILABLE THEN DO:
         THIS-OBJECT:KalkmallKodertth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      THIS-OBJECT:GridValdMallar:GuruReopen().
      
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogUppdate(INPUT klogidvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(klogidvar)) NO-ERROR.
      IF THIS-OBJECT:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KatalogTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.
      queryvar = "FOR EACH " + THIS-OBJECT:IngKatalogTTh:TABLE + " WHERE KLOGID = " + STRING(klogidvar). 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:IngKatalogTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:IngKatalogTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:GridKatHuvDel:GuruReopen().
      THIS-OBJECT:GridKatSek:GuruReopen().      
      
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogAfterBort():
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogSubUppdate(INPUT klogsubidvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(klogsubidvar)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:AVAILABLE THEN THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(klogsubidvar)) NO-ERROR.
      IF THIS-OBJECT:KatalogDeltth:AVAILABLE THEN THIS-OBJECT:KatalogDeltth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID KatalogPrisUppdate(INPUT inqueryvar AS CHARACTER, INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      queryvar = "FOR EACH " + THIS-OBJECT:PriserTTh:TABLE + " WHERE " + inqueryvar. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:PriserTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:PriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
         qH:GET-NEXT().
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      IF kolnamn = "BENAMNING" OR kolnamn = "PRIS" THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:LopsubTTh:TABLE + " WHERE " + inqueryvar. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:LopsubTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:LopsubTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.      
            qH:GET-NEXT().
         END.
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      END.
     
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      RETURN.
   END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		 /*
      IF THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("UPPDATBAR"):BUFFER-VALUE = FALSE THEN RETURN.
      */
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE inklogsubid AS INTEGER NO-UNDO.
      DEFINE VARIABLE inrecid AS RECID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF kolnamn = "ARBKOD" THEN THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
      ELSE THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE) + "'") NO-ERROR.
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         inklogsubid = THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
         inrecid = THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
      END.
      ELSE RETURN.   
      IF kolnamn = "ARBKOD" THEN DO:
         IF kolvarde = "< Ny kod >" OR kolvarde = "" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE = beforekolvarde.
            RETURN.
         END.  
         ELSE DO:
            IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
            IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbKoderKoll(INPUT inklogsubid,INPUT kolvarde,INPUT inrecid) = FALSE THEN DO:
               THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:ArbetskoderTTh:ROWID).
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
               RETURN.
            END.
            ELSE DO:
               THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbLopnrFix(INPUT beforekolvarde,INPUT kolvarde).  
               THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
               THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().
               THIS-OBJECT:GridArbKoder:GuruTom().
               THIS-OBJECT:RefreshArbKoder(?,?).
               THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + kolvarde + "'") NO-ERROR.
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:ArbetskoderTTh:ROWID).             
                           
               /*
               THIS-OBJECT:RefreshArbKoder(?,?).
               THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKoder(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               THIS-OBJECT:RefreshArbKoder(?,?).
                THIS-OBJECT:GridArbKoder:Valuechanged().
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:ArbetskoderTTh:ROWID).
               */
               
            END.                    
         END.    
      END.
      ELSE DO:   
         IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
         THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().        
      END.
		
	END METHOD.

   
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatHuvDel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
     RETURN.
	END METHOD.
	
	METHOD PRIVATE VOID GridKatHuvDel_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridKatHuvDel:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" OR kolnamn = "SEKRETESS" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatPris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN DO:
         queryvar = "KLOGSUBID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KLOGSUBID"]:VALUE) + " AND " + 
                    "KPID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KPID"]:VALUE).
         THIS-OBJECT:KatalogPrisUppdate(INPUT queryvar, INPUT kolnamn, INPUT kolvarde).
      END.
      
		RETURN.
	END METHOD.
	METHOD PRIVATE VOID GridKatPris_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridKatSek:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "EGENKODUPP" OR kolnamn = "EGENPRISUPP" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         queryvar = "KLOGSUBID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KLOGSUBID"]:VALUE) + " AND " + 
                 "KPID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KPID"]:VALUE).
         THIS-OBJECT:KatalogPrisUppdate(INPUT queryvar, INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatSek_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.
	END METHOD.
   
   METHOD PRIVATE VOID GridKatSek_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridKatSek:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" OR kolnamn = "SEKRETESS" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnr_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      /*
      IF THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("UPPDATBAR"):BUFFER-VALUE = FALSE THEN RETURN.
      */
      
      THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING( THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE ) + "' AND LOPNR = " + 
      STRING( THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE ) + " AND KLOGSUBID = " + STRING( THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE )) NO-ERROR.
      THIS-OBJECT:LopposterTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
		RETURN.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnumsub_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
         queryvar = "WHERE ARBKOD = '" + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) +           
                                           "' AND LOPNR = " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) +
                                           " AND KPID = " + STRING(THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["KPID"]:VALUE ) +
                                           " AND KLOGSUBID = " + STRING( THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["KLOGSUBID"]:VALUE ).
         
         THIS-OBJECT:LopsubTTh:FIND-FIRST(queryvar) NO-ERROR.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot - THIS-OBJECT:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE.
         
         THIS-OBJECT:LopsubTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         
         THIS-OBJECT:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:LopsubTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE +     
         THIS-OBJECT:LopsubTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE * THIS-OBJECT:LopsubTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE,0).  
         STRING(THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["POSTKOST"]:VALUE ) = STRING(THIS-OBJECT:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE).     
         
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot + THIS-OBJECT:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
         INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT TRUE).
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
         THIS-OBJECT:GridLopnumSubTitle().
      END.      
		RETURN. 

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatalog_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatalog:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.
   METHOD PRIVATE VOID GridSubKatalog_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridSubKatalog:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatalog:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatDel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.
   
   METHOD PRIVATE VOID GridSubKatDel_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridSubKatDel:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
      RETURN.
   END METHOD.
   
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF CellChvar = FALSE THEN THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.
   
   METHOD PRIVATE VOID GridSubKatKoder_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridSubKatKoder:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "AVSLUTAD" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         CellChvar = TRUE.
      END. 
      RETURN.
   END METHOD.
   
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdaAnvSek_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridAnvSek THEN DO:
         THIS-OBJECT:ButtonOverAnvMark_Click(?,?).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridVisning_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      
      THIS-OBJECT:VisningTTh:FIND-FIRST("WHERE KVID = '" + STRING( THIS-OBJECT:GridVisning:ActiveRow:Cells["KVID"]:VALUE )) NO-ERROR.
      THIS-OBJECT:VisningTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      IF CellChvar = FALSE THEN THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
		RETURN.

	END METHOD.
	
	METHOD PRIVATE VOID GridVisning_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      IF THIS-OBJECT:GridVisning:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      CellChvar = FALSE.
      IF kolnamn = "EABER" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:VisningTTh:FIND-FIRST("WHERE KVID = '" + STRING( THIS-OBJECT:GridVisning:ActiveRow:Cells["KVID"]:VALUE )) NO-ERROR.
         THIS-OBJECT:VisningTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
         CellChvar = TRUE.
      END. 
      RETURN.
   END METHOD.

   METHOD PUBLIC LOGICAL Initialize():
       
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:Control = THIS-OBJECT. 
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKatalog(?).
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarHmt(0).
      
      THIS-OBJECT:CreateForm().
      /*
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylAdmDef().
      */  
      
      RETURN TRUE.
   END METHOD.
   METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkylAdm").
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
      DEFINE VARIABLE appearance216 AS Infragistics.Win.Appearance NO-UNDO.
      appearance216 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance217 AS Infragistics.Win.Appearance NO-UNDO.
      appearance217 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance218 AS Infragistics.Win.Appearance NO-UNDO.
      appearance218 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance219 AS Infragistics.Win.Appearance NO-UNDO.
      appearance219 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance223 AS Infragistics.Win.Appearance NO-UNDO.
      appearance223 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance224 AS Infragistics.Win.Appearance NO-UNDO.
      appearance224 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance225 AS Infragistics.Win.Appearance NO-UNDO.
      appearance225 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance226 AS Infragistics.Win.Appearance NO-UNDO.
      appearance226 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance227 AS Infragistics.Win.Appearance NO-UNDO.
      appearance227 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance228 AS Infragistics.Win.Appearance NO-UNDO.
      appearance228 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance229 AS Infragistics.Win.Appearance NO-UNDO.
      appearance229 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance230 AS Infragistics.Win.Appearance NO-UNDO.
      appearance230 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance231 AS Infragistics.Win.Appearance NO-UNDO.
      appearance231 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance232 AS Infragistics.Win.Appearance NO-UNDO.
      appearance232 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance233 AS Infragistics.Win.Appearance NO-UNDO.
      appearance233 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance234 AS Infragistics.Win.Appearance NO-UNDO.
      appearance234 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance235 AS Infragistics.Win.Appearance NO-UNDO.
      appearance235 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance236 AS Infragistics.Win.Appearance NO-UNDO.
      appearance236 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance237 AS Infragistics.Win.Appearance NO-UNDO.
      appearance237 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance238 AS Infragistics.Win.Appearance NO-UNDO.
      appearance238 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance239 AS Infragistics.Win.Appearance NO-UNDO.
      appearance239 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabDelKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerSubKatDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatPrisDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatHuvKopStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridKatalog = NEW Controls.Grid().
      THIS-OBJECT:splitContainerPrisKopDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatHuvKop = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopSubKat = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:NyDelKatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyDelStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopSubKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortSubKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridSubKatDel = NEW Controls.Grid().
      THIS-OBJECT:buttonOverHuvKat = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackHuvKat = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerKatBildDelStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatBildDelStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridKatHuvDel = NEW Controls.Grid().
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKatalog = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatalogSubKatStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:DelKatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridSubKatalog = NEW Controls.Grid().
      THIS-OBJECT:splitContainerPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPrisStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:PrisStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyPrisStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortPrisStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridKatPris = NEW Controls.Grid().
      THIS-OBJECT:splitContainerVisningBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerVisningBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:VisningBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridVisning = NEW Controls.Grid().
      THIS-OBJECT:TabKoder = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerGrundKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatKoderStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:SubKatKodStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridSubKatKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerNyKopKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridArbKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopNrStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridLopnr = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopsubBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopsubBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopSubBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:splitContainerLopSubFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFrekvens = NEW Controls.Grid().
      THIS-OBJECT:GridLopnumsub = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:Rubrikanm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerMallarBas = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:NyKopMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridMallar = NEW Controls.Grid().
      THIS-OBJECT:splitContainerSubKatMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:SubKatMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridSubMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerArbkodMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerArbkodMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ArbkodMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridArbKodMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnrMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopnrMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopnrMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridLopMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerBtnOverMall = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonOverMall = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackMall = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerValdMallarBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerValdMallarBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ValdMallarBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridValdMallar = NEW Controls.Grid().
      THIS-OBJECT:TabSek = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKatSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKatSek = NEW Controls.Grid().
      THIS-OBJECT:splitContainerAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAnvSek = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKnappAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonOverAnvMark = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackAnvMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerBildSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridValdaAnvSek = NEW Controls.Grid().
      THIS-OBJECT:TabKalkylAdm = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabDelKataloger:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatPrisDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatHuvKopStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:SuspendLayout().
      THIS-OBJECT:KatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerPrisKopDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatHuvKop, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKop:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopSubKat, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:SuspendLayout().
      THIS-OBJECT:NyDelKatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatDel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKatBildDelStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatHuvDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKataloger:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatalog:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatalogSubKatStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPrisStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPrisStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPrisStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPrisStrip:SuspendLayout().
      THIS-OBJECT:PrisStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatPris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerVisningBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisningBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerVisningBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridVisning, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerGrundKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerGrundKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatKoderStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerNyKopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:SuspendLayout().
      THIS-OBJECT:KoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:SuspendLayout().
      THIS-OBJECT:LopNrStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopsubBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopsubBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopsubBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopSubFrek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:SuspendLayout().
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridLopnumsub, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMallarBas, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMallarBas:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMallarBas:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMallarBas:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopMallar:SuspendLayout().
      THIS-OBJECT:NyKopMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerSubKatMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerArbkodMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerArbkodMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridArbKodMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopnrMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopnrMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridLopMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerBtnOverMall, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBtnOverMall:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdMallarBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdMallarBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridValdMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabSek:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:SuspendLayout().
      CAST(THIS-OBJECT:GridKatSek, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:SuspendLayout().
      CAST(THIS-OBJECT:GridAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKnappAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBildSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBildSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBildSek:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkylAdm:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabDelKataloger */
      /*  */
      THIS-OBJECT:TabDelKataloger:Controls:Add(THIS-OBJECT:splitContainerSubKatDel).
      THIS-OBJECT:TabDelKataloger:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabDelKataloger:Name = "TabDelKataloger".
      THIS-OBJECT:TabDelKataloger:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* splitContainerSubKatDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatDel:Name = "splitContainerSubKatDel".
      /*  */
      /* splitContainerSubKatDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatPrisDel).
      /*  */
      /* splitContainerSubKatDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerSubKatDel.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerSubKatDel:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerSubKatDel:Size = NEW System.Drawing.Size(1049, 506).
      THIS-OBJECT:splitContainerSubKatDel:SplitterDistance = 968.
      THIS-OBJECT:splitContainerSubKatDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatDel:TabIndex = 0.
      /*  */
      /* splitContainerSubKatPrisDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatPrisDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatPrisDel:Name = "splitContainerSubKatPrisDel".
      /*  */
      /* splitContainerSubKatPrisDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatHuvKopStrip).
      /*  */
      /* splitContainerSubKatPrisDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerPrisKopDel).
      THIS-OBJECT:splitContainerSubKatPrisDel:Size = NEW System.Drawing.Size(968, 506).
      THIS-OBJECT:splitContainerSubKatPrisDel:SplitterDistance = 354.
      THIS-OBJECT:splitContainerSubKatPrisDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatPrisDel:TabIndex = 0.
      /*  */
      /* splitContainerKatHuvKopStrip */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatHuvKopStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatHuvKopStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Name = "splitContainerKatHuvKopStrip".
      THIS-OBJECT:splitContainerKatHuvKopStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatHuvKopStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:Controls:Add(THIS-OBJECT:KatStrip).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerKatHuvKopStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatalog).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Size = NEW System.Drawing.Size(354, 506).
      THIS-OBJECT:splitContainerKatHuvKopStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerKatHuvKopStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatHuvKopStrip:TabIndex = 4.
      /*  */
      /* KatStrip */
      /*  */
      THIS-OBJECT:KatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonNyKatStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonKopKatStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonBortKatStrip.
      THIS-OBJECT:KatStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:KatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatStrip:Name = "KatStrip".
      THIS-OBJECT:KatStrip:Size = NEW System.Drawing.Size(354, 25).
      THIS-OBJECT:KatStrip:TabIndex = 4.
      THIS-OBJECT:KatStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyKatStrip */
      /*  */
      THIS-OBJECT:ButtonNyKatStrip:Image = CAST(resources:GetObject("ButtonNyKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyKatStrip:Name = "ButtonNyKatStrip".
      THIS-OBJECT:ButtonNyKatStrip:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:ButtonNyKatStrip:Text = "Lägg till katalog".
      THIS-OBJECT:ButtonNyKatStrip:Click:Subscribe(THIS-OBJECT:ButtonNyKatStrip_Click).
      /*  */
      /* ButtonKopKatStrip */
      /*  */
      THIS-OBJECT:ButtonKopKatStrip:Image = CAST(resources:GetObject("ButtonKopKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopKatStrip:Name = "ButtonKopKatStrip".
      THIS-OBJECT:ButtonKopKatStrip:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:ButtonKopKatStrip:Text = "Kopiera katalog".
      THIS-OBJECT:ButtonKopKatStrip:Click:Subscribe(THIS-OBJECT:ButtonKopKatStrip_Click).
      /*  */
      /* ButtonBortKatStrip */
      /*  */
      THIS-OBJECT:ButtonBortKatStrip:Image = CAST(resources:GetObject("ButtonBortKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortKatStrip:Name = "ButtonBortKatStrip".
      THIS-OBJECT:ButtonBortKatStrip:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:ButtonBortKatStrip:Text = "Ta bort katalog".
      THIS-OBJECT:ButtonBortKatStrip:Click:Subscribe(THIS-OBJECT:ButtonBortKatStrip_Click).
      /*  */
      /* GridKatalog */
      /*  */
      THIS-OBJECT:GridKatalog:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatalog:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridKatalog:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatalog:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridKatalog:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatalog:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridKatalog:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatalog:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatalog:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatalog:GuruContext = ?.
      THIS-OBJECT:GridKatalog:GuruDraggable = TRUE.
      THIS-OBJECT:GridKatalog:GuruDroppable = TRUE.
      THIS-OBJECT:GridKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatalog:Name = "GridKatalog".
      THIS-OBJECT:GridKatalog:Size = NEW System.Drawing.Size(354, 480).
      THIS-OBJECT:GridKatalog:TabIndex = 3.
      THIS-OBJECT:GridKatalog:Text = "Grid1".
      THIS-OBJECT:GridKatalog:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatalog_AfterCellUpdate).
      THIS-OBJECT:GridKatalog:CellChange:Subscribe(THIS-OBJECT:GridKatalog_CellChange).
      /*  */
      /* splitContainerPrisKopDel */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPrisKopDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPrisKopDel:Name = "splitContainerPrisKopDel".
      /*  */
      /* splitContainerPrisKopDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatHuvKop).
      /*  */
      /* splitContainerPrisKopDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerKatBildDelStrip).
      THIS-OBJECT:splitContainerPrisKopDel:Size = NEW System.Drawing.Size(613, 506).
      THIS-OBJECT:splitContainerPrisKopDel:SplitterDistance = 441.
      THIS-OBJECT:splitContainerPrisKopDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPrisKopDel:TabIndex = 2.
      /*  */
      /* splitContainerKatHuvKop */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatHuvKop:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatHuvKop:Name = "splitContainerKatHuvKop".
      /*  */
      /* splitContainerKatHuvKop.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopSubKat).
      /*  */
      /* splitContainerKatHuvKop.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:Controls:Add(THIS-OBJECT:buttonOverHuvKat).
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:Controls:Add(THIS-OBJECT:ButtonBackHuvKat).
      THIS-OBJECT:splitContainerKatHuvKop:Size = NEW System.Drawing.Size(441, 506).
      THIS-OBJECT:splitContainerKatHuvKop:SplitterDistance = 397.
      THIS-OBJECT:splitContainerKatHuvKop:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatHuvKop:TabIndex = 0.
      /*  */
      /* splitContainerNyKopSubKat */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopSubKat:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopSubKat:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopSubKat:Name = "splitContainerNyKopSubKat".
      THIS-OBJECT:splitContainerNyKopSubKat:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopSubKat.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:Controls:Add(THIS-OBJECT:NyDelKatStrip).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopSubKat.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:Controls:Add(THIS-OBJECT:GridSubKatDel).
      THIS-OBJECT:splitContainerNyKopSubKat:Size = NEW System.Drawing.Size(397, 506).
      THIS-OBJECT:splitContainerNyKopSubKat:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNyKopSubKat:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopSubKat:TabIndex = 1.
      /*  */
      /* NyDelKatStrip */
      /*  */
      THIS-OBJECT:NyDelKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:ButtonNyDelStrip.
      arrayvar1[2] = THIS-OBJECT:ButtonKopSubKatStrip.
      arrayvar1[3] = THIS-OBJECT:ButtonBortSubKatStrip.
      THIS-OBJECT:NyDelKatStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:NyDelKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:NyDelKatStrip:Name = "NyDelKatStrip".
      THIS-OBJECT:NyDelKatStrip:Size = NEW System.Drawing.Size(397, 25).
      THIS-OBJECT:NyDelKatStrip:TabIndex = 2.
      THIS-OBJECT:NyDelKatStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyDelStrip */
      /*  */
      THIS-OBJECT:ButtonNyDelStrip:Image = CAST(resources:GetObject("ButtonNyDelStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyDelStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyDelStrip:Name = "ButtonNyDelStrip".
      THIS-OBJECT:ButtonNyDelStrip:Size = NEW System.Drawing.Size(127, 22).
      THIS-OBJECT:ButtonNyDelStrip:Text = "Lägg till delkatalog".
      THIS-OBJECT:ButtonNyDelStrip:Click:Subscribe(THIS-OBJECT:ButtonNyDelStrip_Click).
      /*  */
      /* ButtonKopSubKatStrip */
      /*  */
      THIS-OBJECT:ButtonKopSubKatStrip:Image = CAST(resources:GetObject("ButtonKopSubKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopSubKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopSubKatStrip:Name = "ButtonKopSubKatStrip".
      THIS-OBJECT:ButtonKopSubKatStrip:Size = NEW System.Drawing.Size(125, 22).
      THIS-OBJECT:ButtonKopSubKatStrip:Text = "Kopiera delkatalog".
      THIS-OBJECT:ButtonKopSubKatStrip:Click:Subscribe(THIS-OBJECT:ButtonKopSubKatStrip_Click).
      /*  */
      /* ButtonBortSubKatStrip */
      /*  */
      THIS-OBJECT:ButtonBortSubKatStrip:Image = CAST(resources:GetObject("ButtonBortSubKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortSubKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortSubKatStrip:Name = "ButtonBortSubKatStrip".
      THIS-OBJECT:ButtonBortSubKatStrip:Size = NEW System.Drawing.Size(123, 22).
      THIS-OBJECT:ButtonBortSubKatStrip:Text = "Ta bort delkatalog".
      THIS-OBJECT:ButtonBortSubKatStrip:Click:Subscribe(THIS-OBJECT:ButtonBortSubKatStrip_Click).
      /*  */
      /* GridSubKatDel */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatDel:GuruContext = ?.
      THIS-OBJECT:GridSubKatDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatDel:Name = "GridSubKatDel".
      THIS-OBJECT:GridSubKatDel:Size = NEW System.Drawing.Size(397, 480).
      THIS-OBJECT:GridSubKatDel:TabIndex = 0.
      THIS-OBJECT:GridSubKatDel:Text = "grid1".
      THIS-OBJECT:GridSubKatDel:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatDel_AfterCellUpdate).
      /*  */
      /* buttonOverHuvKat */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:buttonOverHuvKat:Appearance = appearance25.
      THIS-OBJECT:buttonOverHuvKat:Location = NEW System.Drawing.Point(5, 150).
      THIS-OBJECT:buttonOverHuvKat:Name = "buttonOverHuvKat".
      THIS-OBJECT:buttonOverHuvKat:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverHuvKat:TabIndex = 12.
      THIS-OBJECT:buttonOverHuvKat:Text = "Markerade väljs".
      THIS-OBJECT:buttonOverHuvKat:Click:Subscribe(THIS-OBJECT:ButtonOverHuvKat_Click).
      /*  */
      /* ButtonBackHuvKat */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:ButtonBackHuvKat:Appearance = appearance26.
      THIS-OBJECT:ButtonBackHuvKat:Location = NEW System.Drawing.Point(5, 180).
      THIS-OBJECT:ButtonBackHuvKat:Name = "ButtonBackHuvKat".
      THIS-OBJECT:ButtonBackHuvKat:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackHuvKat:TabIndex = 13.
      THIS-OBJECT:ButtonBackHuvKat:Text = "Markerade bort".
      THIS-OBJECT:ButtonBackHuvKat:Click:Subscribe(THIS-OBJECT:ButtonBackHuvKat_Click).
      /*  */
      /* splitContainerKatBildDelStrip */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatBildDelStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatBildDelStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatBildDelStrip:Name = "splitContainerKatBildDelStrip".
      THIS-OBJECT:splitContainerKatBildDelStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatBildDelStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:Controls:Add(THIS-OBJECT:KatBildDelStrip).
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerKatBildDelStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatHuvDel).
      THIS-OBJECT:splitContainerKatBildDelStrip:Size = NEW System.Drawing.Size(171, 506).
      THIS-OBJECT:splitContainerKatBildDelStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerKatBildDelStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatBildDelStrip:TabIndex = 3.
      /*  */
      /* KatBildDelStrip */
      /*  */
      THIS-OBJECT:KatBildDelStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatBildDelStrip:Name = "KatBildDelStrip".
      THIS-OBJECT:KatBildDelStrip:Size = NEW System.Drawing.Size(171, 25).
      THIS-OBJECT:KatBildDelStrip:TabIndex = 4.
      THIS-OBJECT:KatBildDelStrip:Text = "toolStrip2".
      /*  */
      /* GridKatHuvDel */
      /*  */
      THIS-OBJECT:GridKatHuvDel:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatHuvDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatHuvDel:GuruContext = ?.
      THIS-OBJECT:GridKatHuvDel:GuruDraggable = TRUE.
      THIS-OBJECT:GridKatHuvDel:GuruDroppable = TRUE.
      THIS-OBJECT:GridKatHuvDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatHuvDel:Name = "GridKatHuvDel".
      THIS-OBJECT:GridKatHuvDel:Size = NEW System.Drawing.Size(171, 480).
      THIS-OBJECT:GridKatHuvDel:TabIndex = 2.
      THIS-OBJECT:GridKatHuvDel:Text = "Grid1".
      THIS-OBJECT:GridKatHuvDel:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatHuvDel_AfterCellUpdate).
      THIS-OBJECT:GridKatHuvDel:CellChange:Subscribe(THIS-OBJECT:GridKatHuvDel_CellChange).
      /*  */
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:splitContainerKatalog).
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* splitContainerKatalog */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalog:Name = "splitContainerKatalog".
      /*  */
      /* splitContainerKatalog.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatalogSubKatStrip).
      /*  */
      /* splitContainerKatalog.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel2:Controls:Add(THIS-OBJECT:splitContainerPris).
      THIS-OBJECT:splitContainerKatalog:Size = NEW System.Drawing.Size(1049, 506).
      THIS-OBJECT:splitContainerKatalog:SplitterDistance = 496.
      THIS-OBJECT:splitContainerKatalog:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatalog:TabIndex = 0.
      /*  */
      /* splitContainerKatalogSubKatStrip */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Name = "splitContainerKatalogSubKatStrip".
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatalogSubKatStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:Controls:Add(THIS-OBJECT:DelKatStrip).
      /*  */
      /* splitContainerKatalogSubKatStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubKatalog).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Size = NEW System.Drawing.Size(496, 506).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:TabIndex = 1.
      /*  */
      /* DelKatStrip */
      /*  */
      THIS-OBJECT:DelKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:DelKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:DelKatStrip:Name = "DelKatStrip".
      THIS-OBJECT:DelKatStrip:Size = NEW System.Drawing.Size(496, 25).
      THIS-OBJECT:DelKatStrip:TabIndex = 2.
      THIS-OBJECT:DelKatStrip:Text = "toolStrip1".
      /*  */
      /* GridSubKatalog */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatalog:GuruContext = ?.
      THIS-OBJECT:GridSubKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatalog:Name = "GridSubKatalog".
      THIS-OBJECT:GridSubKatalog:Size = NEW System.Drawing.Size(496, 480).
      THIS-OBJECT:GridSubKatalog:TabIndex = 0.
      THIS-OBJECT:GridSubKatalog:Text = "Grid1".
      THIS-OBJECT:GridSubKatalog:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatalog_AfterCellUpdate).
      /*  */
      /* splitContainerPris */
      /*  */
      THIS-OBJECT:splitContainerPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPris:Name = "splitContainerPris".
      /*  */
      /* splitContainerPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerPrisStrip).
      /*  */
      /* splitContainerPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPris:Panel2:Controls:Add(THIS-OBJECT:splitContainerVisningBild).
      THIS-OBJECT:splitContainerPris:Size = NEW System.Drawing.Size(552, 506).
      THIS-OBJECT:splitContainerPris:SplitterDistance = 270.
      THIS-OBJECT:splitContainerPris:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPris:TabIndex = 0.
      /*  */
      /* splitContainerPrisStrip */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPrisStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerPrisStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPrisStrip:Name = "splitContainerPrisStrip".
      THIS-OBJECT:splitContainerPrisStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerPrisStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Panel1:Controls:Add(THIS-OBJECT:PrisStrip).
      THIS-OBJECT:splitContainerPrisStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerPrisStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatPris).
      THIS-OBJECT:splitContainerPrisStrip:Size = NEW System.Drawing.Size(270, 506).
      THIS-OBJECT:splitContainerPrisStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerPrisStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPrisStrip:TabIndex = 1.
      /*  */
      /* PrisStrip */
      /*  */
      THIS-OBJECT:PrisStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:ButtonNyPrisStrip.
      arrayvar2[2] = THIS-OBJECT:ButtonBortPrisStrip.
      THIS-OBJECT:PrisStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:PrisStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:PrisStrip:Name = "PrisStrip".
      THIS-OBJECT:PrisStrip:Size = NEW System.Drawing.Size(270, 25).
      THIS-OBJECT:PrisStrip:TabIndex = 3.
      THIS-OBJECT:PrisStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyPrisStrip */
      /*  */
      THIS-OBJECT:ButtonNyPrisStrip:Image = CAST(resources:GetObject("ButtonNyPrisStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyPrisStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyPrisStrip:Name = "ButtonNyPrisStrip".
      THIS-OBJECT:ButtonNyPrisStrip:Size = NEW System.Drawing.Size(91, 22).
      THIS-OBJECT:ButtonNyPrisStrip:Text = "Lägg till pris".
      THIS-OBJECT:ButtonNyPrisStrip:Click:Subscribe(THIS-OBJECT:ButtonNyPrisStrip_Click).
      /*  */
      /* ButtonBortPrisStrip */
      /*  */
      THIS-OBJECT:ButtonBortPrisStrip:Image = CAST(resources:GetObject("ButtonBortPrisStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortPrisStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortPrisStrip:Name = "ButtonBortPrisStrip".
      THIS-OBJECT:ButtonBortPrisStrip:Size = NEW System.Drawing.Size(87, 22).
      THIS-OBJECT:ButtonBortPrisStrip:Text = "Ta bort pris".
      THIS-OBJECT:ButtonBortPrisStrip:Click:Subscribe(THIS-OBJECT:ButtonBortPrisStrip_Click).
      /*  */
      /* GridKatPris */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatPris:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridKatPris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatPris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridKatPris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatPris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridKatPris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatPris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatPris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatPris:GuruContext = ?.
      THIS-OBJECT:GridKatPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatPris:Name = "GridKatPris".
      THIS-OBJECT:GridKatPris:Size = NEW System.Drawing.Size(270, 480).
      THIS-OBJECT:GridKatPris:TabIndex = 0.
      THIS-OBJECT:GridKatPris:Text = "Grid1".
      THIS-OBJECT:GridKatPris:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatPris_AfterCellUpdate).
      /*  */
      /* splitContainerVisningBild */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisningBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisningBild:Name = "splitContainerVisningBild".
      /*  */
      /* splitContainerVisningBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerVisningBildStrip).
      /*  */
      /* splitContainerVisningBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerVisningBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerVisningBild:Size = NEW System.Drawing.Size(281, 506).
      THIS-OBJECT:splitContainerVisningBild:SplitterDistance = 243.
      THIS-OBJECT:splitContainerVisningBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerVisningBild:TabIndex = 0.
      /*  */
      /* splitContainerVisningBildStrip */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisningBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerVisningBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisningBildStrip:Name = "splitContainerVisningBildStrip".
      THIS-OBJECT:splitContainerVisningBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerVisningBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:Controls:Add(THIS-OBJECT:VisningBildStrip).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerVisningBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:Controls:Add(THIS-OBJECT:GridVisning).
      THIS-OBJECT:splitContainerVisningBildStrip:Size = NEW System.Drawing.Size(243, 506).
      THIS-OBJECT:splitContainerVisningBildStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerVisningBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerVisningBildStrip:TabIndex = 1.
      /*  */
      /* VisningBildStrip */
      /*  */
      THIS-OBJECT:VisningBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisningBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisningBildStrip:Name = "VisningBildStrip".
      THIS-OBJECT:VisningBildStrip:Size = NEW System.Drawing.Size(243, 25).
      THIS-OBJECT:VisningBildStrip:TabIndex = 4.
      THIS-OBJECT:VisningBildStrip:Text = "toolStrip1".
      /*  */
      /* GridVisning */
      /*  */
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridVisning:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:GridVisning:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridVisning:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:GridVisning:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridVisning:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:GridVisning:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridVisning:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridVisning:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridVisning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridVisning:GuruContext = ?.
      THIS-OBJECT:GridVisning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridVisning:Name = "GridVisning".
      THIS-OBJECT:GridVisning:Size = NEW System.Drawing.Size(243, 480).
      THIS-OBJECT:GridVisning:TabIndex = 0.
      THIS-OBJECT:GridVisning:Text = "Grid1".
      THIS-OBJECT:GridVisning:AfterCellUpdate:Subscribe(THIS-OBJECT:GridVisning_AfterCellUpdate).
      /*  */
      /* TabKoder */
      /*  */
      THIS-OBJECT:TabKoder:Controls:Add(THIS-OBJECT:splitContainerGrundKoder).
      THIS-OBJECT:TabKoder:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKoder:Name = "TabKoder".
      THIS-OBJECT:TabKoder:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* splitContainerGrundKoder */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundKoder:Name = "splitContainerGrundKoder".
      /*  */
      /* splitContainerGrundKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatKoder).
      /*  */
      /* splitContainerGrundKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnr).
      THIS-OBJECT:splitContainerGrundKoder:Size = NEW System.Drawing.Size(1049, 506).
      THIS-OBJECT:splitContainerGrundKoder:SplitterDistance = 484.
      THIS-OBJECT:splitContainerGrundKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerGrundKoder:TabIndex = 0.
      /*  */
      /* splitContainerSubKatKoder */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatKoder:Name = "splitContainerSubKatKoder".
      /*  */
      /* splitContainerSubKatKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatKoderStrip).
      /*  */
      /* splitContainerSubKatKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerNyKopKoder).
      THIS-OBJECT:splitContainerSubKatKoder:Size = NEW System.Drawing.Size(484, 506).
      THIS-OBJECT:splitContainerSubKatKoder:SplitterDistance = 204.
      THIS-OBJECT:splitContainerSubKatKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatKoder:TabIndex = 0.
      /*  */
      /* splitContainerSubKatKoderStrip */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Name = "splitContainerSubKatKoderStrip".
      THIS-OBJECT:splitContainerSubKatKoderStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSubKatKoderStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:Controls:Add(THIS-OBJECT:SubKatKodStrip).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerSubKatKoderStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubKatKoder).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Size = NEW System.Drawing.Size(204, 506).
      THIS-OBJECT:splitContainerSubKatKoderStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerSubKatKoderStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatKoderStrip:TabIndex = 2.
      /*  */
      /* SubKatKodStrip */
      /*  */
      THIS-OBJECT:SubKatKodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:SubKatKodStrip:Name = "SubKatKodStrip".
      THIS-OBJECT:SubKatKodStrip:Size = NEW System.Drawing.Size(204, 25).
      THIS-OBJECT:SubKatKodStrip:TabIndex = 1.
      THIS-OBJECT:SubKatKodStrip:Text = "toolStrip1".
      /*  */
      /* GridSubKatKoder */
      /*  */
      THIS-OBJECT:GridSubKatKoder:AlphaBlendMode = Infragistics.Win.AlphaBlendMode:Disabled.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Appearance = appearance75.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance76:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance76:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance76:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:Appearance = appearance76.
      appearance77:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance77.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance78:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance78:BackColor2 = System.Drawing.SystemColors:Control.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance78:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance78.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveCellAppearance = appearance79.
      appearance80:BackColor = System.Drawing.SystemColors:Highlight.
      appearance80:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveRowAppearance = appearance80.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CardAreaAppearance = appearance81.
      appearance82:BorderColor = System.Drawing.Color:Silver.
      appearance82:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellAppearance = appearance82.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellPadding = 0.
      appearance83:BackColor = System.Drawing.SystemColors:Control.
      appearance83:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance83:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance83:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance83:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:GroupByRowAppearance = appearance83.
      appearance84:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderAppearance = appearance84.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      appearance85:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:RowAppearance = appearance85.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance86:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance86.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatKoder:GuruContext = ?.
      THIS-OBJECT:GridSubKatKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatKoder:Name = "GridSubKatKoder".
      THIS-OBJECT:GridSubKatKoder:Size = NEW System.Drawing.Size(204, 480).
      THIS-OBJECT:GridSubKatKoder:TabIndex = 1.
      THIS-OBJECT:GridSubKatKoder:Text = "Grid1".
      THIS-OBJECT:GridSubKatKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatKoder_AfterCellUpdate).
      /*  */
      /* splitContainerNyKopKoder */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopKoder:Name = "splitContainerNyKopKoder".
      THIS-OBJECT:splitContainerNyKopKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:Controls:Add(THIS-OBJECT:KoderStrip).
      THIS-OBJECT:splitContainerNyKopKoder:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:Controls:Add(THIS-OBJECT:GridArbKoder).
      THIS-OBJECT:splitContainerNyKopKoder:Size = NEW System.Drawing.Size(279, 506).
      THIS-OBJECT:splitContainerNyKopKoder:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNyKopKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopKoder:TabIndex = 2.
      /*  */
      /* KoderStrip */
      /*  */
      THIS-OBJECT:KoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:ButtonNyKoderStrip.
      arrayvar3[2] = THIS-OBJECT:ButtonKopKoderStrip.
      arrayvar3[3] = THIS-OBJECT:ButtonBortKoderStrip.
      THIS-OBJECT:KoderStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:KoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KoderStrip:Name = "KoderStrip".
      THIS-OBJECT:KoderStrip:Size = NEW System.Drawing.Size(279, 25).
      THIS-OBJECT:KoderStrip:TabIndex = 0.
      THIS-OBJECT:KoderStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyKoderStrip */
      /*  */
      THIS-OBJECT:ButtonNyKoderStrip:Image = CAST(resources:GetObject("ButtonNyKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyKoderStrip:Name = "ButtonNyKoderStrip".
      THIS-OBJECT:ButtonNyKoderStrip:Size = NEW System.Drawing.Size(92, 22).
      THIS-OBJECT:ButtonNyKoderStrip:Text = "Lägg till kod".
      THIS-OBJECT:ButtonNyKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonNyKoderStrip_Click).
      /*  */
      /* ButtonKopKoderStrip */
      /*  */
      THIS-OBJECT:ButtonKopKoderStrip:Image = CAST(resources:GetObject("ButtonKopKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopKoderStrip:Name = "ButtonKopKoderStrip".
      THIS-OBJECT:ButtonKopKoderStrip:Size = NEW System.Drawing.Size(90, 22).
      THIS-OBJECT:ButtonKopKoderStrip:Text = "Kopiera kod".
      THIS-OBJECT:ButtonKopKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonKopKoderStrip_Click).
      /*  */
      /* ButtonBortKoderStrip */
      /*  */
      THIS-OBJECT:ButtonBortKoderStrip:Image = CAST(resources:GetObject("ButtonBortKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortKoderStrip:Name = "ButtonBortKoderStrip".
      THIS-OBJECT:ButtonBortKoderStrip:Size = NEW System.Drawing.Size(88, 20).
      THIS-OBJECT:ButtonBortKoderStrip:Text = "Ta bort kod".
      THIS-OBJECT:ButtonBortKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonBortKoderStrip_Click).
      /*  */
      /* GridArbKoder */
      /*  */
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Appearance = appearance87.
      THIS-OBJECT:GridArbKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance88:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance88:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance88:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:Appearance = appearance88.
      appearance89:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance89.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance90:BackColor2 = System.Drawing.SystemColors:Control.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance90:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance90.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveCellAppearance = appearance91.
      appearance92:BackColor = System.Drawing.SystemColors:Highlight.
      appearance92:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveRowAppearance = appearance92.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CardAreaAppearance = appearance93.
      appearance94:BorderColor = System.Drawing.Color:Silver.
      appearance94:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellAppearance = appearance94.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellPadding = 0.
      appearance95:BackColor = System.Drawing.SystemColors:Control.
      appearance95:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance95:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance95:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance95:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:GroupByRowAppearance = appearance95.
      appearance96:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderAppearance = appearance96.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      appearance97:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowAppearance = appearance97.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance98:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance98.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbKoder:GuruContext = ?.
      THIS-OBJECT:GridArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbKoder:Name = "GridArbKoder".
      THIS-OBJECT:GridArbKoder:Size = NEW System.Drawing.Size(279, 480).
      THIS-OBJECT:GridArbKoder:TabIndex = 1.
      THIS-OBJECT:GridArbKoder:Text = "Grid1".
      THIS-OBJECT:GridArbKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_AfterCellUpdate).
      THIS-OBJECT:GridArbKoder:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_BeforeCellUpdate).
      /*  */
      /* splitContainerLopnr */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnr:Name = "splitContainerLopnr".
      /*  */
      /* splitContainerLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopLopnr).
      /*  */
      /* splitContainerLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopsubBild).
      THIS-OBJECT:splitContainerLopnr:Size = NEW System.Drawing.Size(564, 506).
      THIS-OBJECT:splitContainerLopnr:SplitterDistance = 216.
      THIS-OBJECT:splitContainerLopnr:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnr:TabIndex = 0.
      /*  */
      /* splitContainerNyKopLopnr */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopLopnr:Name = "splitContainerNyKopLopnr".
      THIS-OBJECT:splitContainerNyKopLopnr:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:Controls:Add(THIS-OBJECT:LopNrStrip).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:Controls:Add(THIS-OBJECT:GridLopnr).
      THIS-OBJECT:splitContainerNyKopLopnr:Size = NEW System.Drawing.Size(216, 506).
      THIS-OBJECT:splitContainerNyKopLopnr:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNyKopLopnr:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopLopnr:TabIndex = 2.
      /*  */
      /* LopNrStrip */
      /*  */
      THIS-OBJECT:LopNrStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:ButtonNyLopnrStrip.
      arrayvar4[2] = THIS-OBJECT:ButtonKopLopnrStrip.
      arrayvar4[3] = THIS-OBJECT:ButtonBortLopnrStrip.
      THIS-OBJECT:LopNrStrip:Items:AddRange(arrayvar4).
      THIS-OBJECT:LopNrStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopNrStrip:Name = "LopNrStrip".
      THIS-OBJECT:LopNrStrip:Size = NEW System.Drawing.Size(216, 25).
      THIS-OBJECT:LopNrStrip:TabIndex = 2.
      THIS-OBJECT:LopNrStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonNyLopnrStrip:Image = CAST(resources:GetObject("ButtonNyLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyLopnrStrip:Name = "ButtonNyLopnrStrip".
      THIS-OBJECT:ButtonNyLopnrStrip:Size = NEW System.Drawing.Size(100, 22).
      THIS-OBJECT:ButtonNyLopnrStrip:Text = "Lägg till löpnr".
      THIS-OBJECT:ButtonNyLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonNyLopnrStrip_Click).
      /*  */
      /* ButtonKopLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonKopLopnrStrip:Image = CAST(resources:GetObject("ButtonKopLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopLopnrStrip:Name = "ButtonKopLopnrStrip".
      THIS-OBJECT:ButtonKopLopnrStrip:Size = NEW System.Drawing.Size(98, 20).
      THIS-OBJECT:ButtonKopLopnrStrip:Text = "Kopiera löpnr".
      THIS-OBJECT:ButtonKopLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonKopLopnrStrip_Click).
      /*  */
      /* ButtonBortLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonBortLopnrStrip:Image = CAST(resources:GetObject("ButtonBortLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortLopnrStrip:Name = "ButtonBortLopnrStrip".
      THIS-OBJECT:ButtonBortLopnrStrip:Size = NEW System.Drawing.Size(96, 20).
      THIS-OBJECT:ButtonBortLopnrStrip:Text = "Ta bort löpnr".
      THIS-OBJECT:ButtonBortLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonBortLopnrStrip_Click).
      /*  */
      /* GridLopnr */
      /*  */
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopnr:DisplayLayout:Appearance = appearance99.
      THIS-OBJECT:GridLopnr:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopnr:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance100:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance100:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance100:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:Appearance = appearance100.
      appearance101:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:BandLabelAppearance = appearance101.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance102:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance102:BackColor2 = System.Drawing.SystemColors:Control.
      appearance102:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance102:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:PromptAppearance = appearance102.
      THIS-OBJECT:GridLopnr:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopnr:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance103:BackColor = System.Drawing.SystemColors:Window.
      appearance103:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveCellAppearance = appearance103.
      appearance104:BackColor = System.Drawing.SystemColors:Highlight.
      appearance104:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveRowAppearance = appearance104.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance105:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CardAreaAppearance = appearance105.
      appearance106:BorderColor = System.Drawing.Color:Silver.
      appearance106:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellAppearance = appearance106.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellPadding = 0.
      appearance107:BackColor = System.Drawing.SystemColors:Control.
      appearance107:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance107:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance107:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance107:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:GroupByRowAppearance = appearance107.
      appearance108:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderAppearance = appearance108.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance109:BackColor = System.Drawing.SystemColors:Window.
      appearance109:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:RowAppearance = appearance109.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance110:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:TemplateAddRowAppearance = appearance110.
      THIS-OBJECT:GridLopnr:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopnr:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopnr:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopnr:GuruContext = ?.
      THIS-OBJECT:GridLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopnr:Name = "GridLopnr".
      THIS-OBJECT:GridLopnr:Size = NEW System.Drawing.Size(216, 480).
      THIS-OBJECT:GridLopnr:TabIndex = 1.
      THIS-OBJECT:GridLopnr:Text = "Grid1".
      THIS-OBJECT:GridLopnr:AfterCellUpdate:Subscribe(THIS-OBJECT:GridLopnr_AfterCellUpdate).
      /*  */
      /* splitContainerLopsubBild */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopsubBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopsubBild:Name = "splitContainerLopsubBild".
      /*  */
      /* splitContainerLopsubBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerLopsubBildStrip).
      /*  */
      /* splitContainerLopsubBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerLopsubBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerLopsubBild:Size = NEW System.Drawing.Size(347, 506).
      THIS-OBJECT:splitContainerLopsubBild:SplitterDistance = 261.
      THIS-OBJECT:splitContainerLopsubBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopsubBild:TabIndex = 0.
      /*  */
      /* splitContainerLopsubBildStrip */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopsubBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopsubBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopsubBildStrip:Name = "splitContainerLopsubBildStrip".
      THIS-OBJECT:splitContainerLopsubBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopsubBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:Controls:Add(THIS-OBJECT:LopSubBildStrip).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerLopsubBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopSubFrek).
      THIS-OBJECT:splitContainerLopsubBildStrip:Size = NEW System.Drawing.Size(261, 506).
      THIS-OBJECT:splitContainerLopsubBildStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerLopsubBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopsubBildStrip:TabIndex = 4.
      /*  */
      /* LopSubBildStrip */
      /*  */
      THIS-OBJECT:LopSubBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:LopSubBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopSubBildStrip:Name = "LopSubBildStrip".
      THIS-OBJECT:LopSubBildStrip:Size = NEW System.Drawing.Size(261, 25).
      THIS-OBJECT:LopSubBildStrip:TabIndex = 2.
      THIS-OBJECT:LopSubBildStrip:Text = "toolStrip1".
      /*  */
      /* splitContainerLopSubFrek */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopSubFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopSubFrek:Name = "splitContainerLopSubFrek".
      /*  */
      /* splitContainerLopSubFrek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:Controls:Add(THIS-OBJECT:GridFrekvens).
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:Controls:Add(THIS-OBJECT:GridLopnumsub).
      /*  */
      /* splitContainerLopSubFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      THIS-OBJECT:splitContainerLopSubFrek:Size = NEW System.Drawing.Size(261, 480).
      THIS-OBJECT:splitContainerLopSubFrek:SplitterDistance = 187.
      THIS-OBJECT:splitContainerLopSubFrek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopSubFrek:TabIndex = 3.
      /*  */
      /* GridFrekvens */
      /*  */
      appearance111:BackColor = System.Drawing.SystemColors:Window.
      appearance111:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Appearance = appearance111.
      THIS-OBJECT:GridFrekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFrekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance112:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance112:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance112:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance112:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:Appearance = appearance112.
      appearance113:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance113.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance114:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance114:BackColor2 = System.Drawing.SystemColors:Control.
      appearance114:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance114:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance114.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveCellAppearance = appearance115.
      appearance116:BackColor = System.Drawing.SystemColors:Highlight.
      appearance116:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveRowAppearance = appearance116.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance117:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CardAreaAppearance = appearance117.
      appearance118:BorderColor = System.Drawing.Color:Silver.
      appearance118:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellAppearance = appearance118.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellPadding = 0.
      appearance119:BackColor = System.Drawing.SystemColors:Control.
      appearance119:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance119:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance119:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance119:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:GroupByRowAppearance = appearance119.
      appearance120:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderAppearance = appearance120.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      appearance121:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowAppearance = appearance121.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance122:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance122.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFrekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFrekvens:GuruContext = ?.
      THIS-OBJECT:GridFrekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFrekvens:Name = "GridFrekvens".
      THIS-OBJECT:GridFrekvens:Size = NEW System.Drawing.Size(187, 480).
      THIS-OBJECT:GridFrekvens:TabIndex = 2.
      THIS-OBJECT:GridFrekvens:Text = "grid1".
      THIS-OBJECT:GridFrekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFrekvens_AfterCellUpdate).
      /*  */
      /* GridLopnumsub */
      /*  */
      appearance123:BackColor = System.Drawing.SystemColors:Window.
      appearance123:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Appearance = appearance123.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance124:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance124:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance124:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance124:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:Appearance = appearance124.
      appearance125:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:BandLabelAppearance = appearance125.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance126:BackColor2 = System.Drawing.SystemColors:Control.
      appearance126:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance126:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:PromptAppearance = appearance126.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance127:BackColor = System.Drawing.SystemColors:Window.
      appearance127:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveCellAppearance = appearance127.
      appearance128:BackColor = System.Drawing.SystemColors:Highlight.
      appearance128:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveRowAppearance = appearance128.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance129:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CardAreaAppearance = appearance129.
      appearance130:BorderColor = System.Drawing.Color:Silver.
      appearance130:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellAppearance = appearance130.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellPadding = 0.
      appearance131:BackColor = System.Drawing.SystemColors:Control.
      appearance131:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance131:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance131:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance131:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:GroupByRowAppearance = appearance131.
      appearance132:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderAppearance = appearance132.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance133:BackColor = System.Drawing.SystemColors:Window.
      appearance133:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:RowAppearance = appearance133.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance134:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:TemplateAddRowAppearance = appearance134.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopnumsub:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopnumsub:GuruContext = ?.
      THIS-OBJECT:GridLopnumsub:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopnumsub:Name = "GridLopnumsub".
      THIS-OBJECT:GridLopnumsub:Size = NEW System.Drawing.Size(187, 480).
      THIS-OBJECT:GridLopnumsub:TabIndex = 1.
      THIS-OBJECT:GridLopnumsub:Text = "Grid1".
      THIS-OBJECT:GridLopnumsub:AfterCellUpdate:Subscribe(THIS-OBJECT:GridLopnumsub_AfterCellUpdate).
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
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(73, 480).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 51.
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* Rubrikanm */
      /*  */
      THIS-OBJECT:Rubrikanm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Rubrikanm:Enabled = FALSE.
      THIS-OBJECT:Rubrikanm:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:Rubrikanm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Rubrikanm:Name = "Rubrikanm".
      THIS-OBJECT:Rubrikanm:Size = NEW System.Drawing.Size(73, 33).
      THIS-OBJECT:Rubrikanm:TabIndex = 1.
      THIS-OBJECT:Rubrikanm:Text = "Kommentar".
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(73, 428).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 34.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
      THIS-OBJECT:ValdKodKommentar:ValueChanged:Subscribe(THIS-OBJECT:ValdKodKommentar_ValueChanged).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:splitContainerMallarBas).
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* splitContainerMallarBas */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMallarBas:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMallarBas:Name = "splitContainerMallarBas".
      /*  */
      /* splitContainerMallarBas.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopMallar).
      /*  */
      /* splitContainerMallarBas.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Panel2:Controls:Add(THIS-OBJECT:splitContainerSubKatMallar).
      THIS-OBJECT:splitContainerMallarBas:Size = NEW System.Drawing.Size(1049, 506).
      THIS-OBJECT:splitContainerMallarBas:SplitterDistance = 161.
      THIS-OBJECT:splitContainerMallarBas:SplitterWidth = 1.
      THIS-OBJECT:splitContainerMallarBas:TabIndex = 0.
      /*  */
      /* splitContainerNyKopMallar */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopMallar:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopMallar:Name = "splitContainerNyKopMallar".
      THIS-OBJECT:splitContainerNyKopMallar:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:Controls:Add(THIS-OBJECT:NyKopMallarStrip).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:Controls:Add(THIS-OBJECT:GridMallar).
      THIS-OBJECT:splitContainerNyKopMallar:Size = NEW System.Drawing.Size(161, 506).
      THIS-OBJECT:splitContainerNyKopMallar:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNyKopMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopMallar:TabIndex = 0.
      /*  */
      /* NyKopMallarStrip */
      /*  */
      THIS-OBJECT:NyKopMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar5[1] = THIS-OBJECT:ButtonNyMallStrip.
      arrayvar5[2] = THIS-OBJECT:ButtonKopMallStrip.
      arrayvar5[3] = THIS-OBJECT:ButtonBortMallStrip.
      THIS-OBJECT:NyKopMallarStrip:Items:AddRange(arrayvar5).
      THIS-OBJECT:NyKopMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:NyKopMallarStrip:Name = "NyKopMallarStrip".
      THIS-OBJECT:NyKopMallarStrip:Size = NEW System.Drawing.Size(161, 25).
      THIS-OBJECT:NyKopMallarStrip:TabIndex = 1.
      THIS-OBJECT:NyKopMallarStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyMallStrip */
      /*  */
      THIS-OBJECT:ButtonNyMallStrip:Image = CAST(resources:GetObject("ButtonNyMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyMallStrip:Name = "ButtonNyMallStrip".
      THIS-OBJECT:ButtonNyMallStrip:Size = NEW System.Drawing.Size(95, 22).
      THIS-OBJECT:ButtonNyMallStrip:Text = "Lägg till mall".
      THIS-OBJECT:ButtonNyMallStrip:Click:Subscribe(THIS-OBJECT:ButtonNyMallStrip_Click).
      /*  */
      /* ButtonKopMallStrip */
      /*  */
      THIS-OBJECT:ButtonKopMallStrip:Image = CAST(resources:GetObject("ButtonKopMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopMallStrip:Name = "ButtonKopMallStrip".
      THIS-OBJECT:ButtonKopMallStrip:Size = NEW System.Drawing.Size(93, 20).
      THIS-OBJECT:ButtonKopMallStrip:Text = "Kopiera mall".
      THIS-OBJECT:ButtonKopMallStrip:Click:Subscribe(THIS-OBJECT:ButtonKopMallStrip_Click).
      /*  */
      /* ButtonBortMallStrip */
      /*  */
      THIS-OBJECT:ButtonBortMallStrip:Image = CAST(resources:GetObject("ButtonBortMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortMallStrip:Name = "ButtonBortMallStrip".
      THIS-OBJECT:ButtonBortMallStrip:Size = NEW System.Drawing.Size(91, 20).
      THIS-OBJECT:ButtonBortMallStrip:Text = "Ta bort mall".
      THIS-OBJECT:ButtonBortMallStrip:Click:Subscribe(THIS-OBJECT:ButtonBortMallStrip_Click).
      /*  */
      /* GridMallar */
      /*  */
      appearance135:BackColor = System.Drawing.SystemColors:Window.
      appearance135:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallar:DisplayLayout:Appearance = appearance135.
      THIS-OBJECT:GridMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance136:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance136:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance136:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance136:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:Appearance = appearance136.
      appearance137:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance137.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance138:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance138:BackColor2 = System.Drawing.SystemColors:Control.
      appearance138:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance138:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance138.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance139:BackColor = System.Drawing.SystemColors:Window.
      appearance139:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveCellAppearance = appearance139.
      appearance140:BackColor = System.Drawing.SystemColors:Highlight.
      appearance140:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveRowAppearance = appearance140.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance141:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CardAreaAppearance = appearance141.
      appearance142:BorderColor = System.Drawing.Color:Silver.
      appearance142:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellAppearance = appearance142.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellPadding = 0.
      appearance143:BackColor = System.Drawing.SystemColors:Control.
      appearance143:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance143:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance143:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance143:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:GroupByRowAppearance = appearance143.
      appearance144:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderAppearance = appearance144.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance145:BackColor = System.Drawing.SystemColors:Window.
      appearance145:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowAppearance = appearance145.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance146:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance146.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallar:GuruContext = ?.
      THIS-OBJECT:GridMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallar:Name = "GridMallar".
      THIS-OBJECT:GridMallar:Size = NEW System.Drawing.Size(161, 480).
      THIS-OBJECT:GridMallar:TabIndex = 0.
      THIS-OBJECT:GridMallar:Text = "grid1".
      THIS-OBJECT:GridMallar:AfterCellUpdate:Subscribe(THIS-OBJECT:GridMallar_AfterCellUpdate).
      /*  */
      /* splitContainerSubKatMallar */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatMallar:Name = "splitContainerSubKatMallar".
      /*  */
      /* splitContainerSubKatMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatMallarStrip).
      /*  */
      /* splitContainerSubKatMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerArbkodMallar).
      THIS-OBJECT:splitContainerSubKatMallar:Size = NEW System.Drawing.Size(887, 506).
      THIS-OBJECT:splitContainerSubKatMallar:SplitterDistance = 172.
      THIS-OBJECT:splitContainerSubKatMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatMallar:TabIndex = 0.
      /*  */
      /* splitContainerSubKatMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerSubKatMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Name = "splitContainerSubKatMallarStrip".
      THIS-OBJECT:splitContainerSubKatMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSubKatMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:Controls:Add(THIS-OBJECT:SubKatMallarStrip).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerSubKatMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubMall).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Size = NEW System.Drawing.Size(172, 506).
      THIS-OBJECT:splitContainerSubKatMallarStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerSubKatMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatMallarStrip:TabIndex = 1.
      /*  */
      /* SubKatMallarStrip */
      /*  */
      THIS-OBJECT:SubKatMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:SubKatMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:SubKatMallarStrip:Name = "SubKatMallarStrip".
      THIS-OBJECT:SubKatMallarStrip:Size = NEW System.Drawing.Size(172, 25).
      THIS-OBJECT:SubKatMallarStrip:TabIndex = 3.
      THIS-OBJECT:SubKatMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridSubMall */
      /*  */
      appearance147:BackColor = System.Drawing.SystemColors:Window.
      appearance147:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubMall:DisplayLayout:Appearance = appearance147.
      THIS-OBJECT:GridSubMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance148:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance148:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance148:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance148:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:Appearance = appearance148.
      appearance149:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance149.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance150:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance150:BackColor2 = System.Drawing.SystemColors:Control.
      appearance150:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance150:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:PromptAppearance = appearance150.
      THIS-OBJECT:GridSubMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance151:BackColor = System.Drawing.SystemColors:Window.
      appearance151:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveCellAppearance = appearance151.
      appearance152:BackColor = System.Drawing.SystemColors:Highlight.
      appearance152:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveRowAppearance = appearance152.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance153:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CardAreaAppearance = appearance153.
      appearance154:BorderColor = System.Drawing.Color:Silver.
      appearance154:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellAppearance = appearance154.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellPadding = 0.
      appearance155:BackColor = System.Drawing.SystemColors:Control.
      appearance155:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance155:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance155:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance155:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:GroupByRowAppearance = appearance155.
      appearance156:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderAppearance = appearance156.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance157:BackColor = System.Drawing.SystemColors:Window.
      appearance157:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:RowAppearance = appearance157.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance158:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance158.
      THIS-OBJECT:GridSubMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubMall:GuruContext = ?.
      THIS-OBJECT:GridSubMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubMall:Name = "GridSubMall".
      THIS-OBJECT:GridSubMall:Size = NEW System.Drawing.Size(172, 480).
      THIS-OBJECT:GridSubMall:TabIndex = 0.
      THIS-OBJECT:GridSubMall:Text = "grid1".
      /*  */
      /* splitContainerArbkodMallar */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbkodMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbkodMallar:Name = "splitContainerArbkodMallar".
      /*  */
      /* splitContainerArbkodMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerArbkodMallarStrip).
      /*  */
      /* splitContainerArbkodMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnrMallar).
      THIS-OBJECT:splitContainerArbkodMallar:Size = NEW System.Drawing.Size(714, 506).
      THIS-OBJECT:splitContainerArbkodMallar:SplitterDistance = 173.
      THIS-OBJECT:splitContainerArbkodMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerArbkodMallar:TabIndex = 0.
      /*  */
      /* splitContainerArbkodMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbkodMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerArbkodMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Name = "splitContainerArbkodMallarStrip".
      THIS-OBJECT:splitContainerArbkodMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerArbkodMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:Controls:Add(THIS-OBJECT:ArbkodMallarStrip).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerArbkodMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridArbKodMall).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Size = NEW System.Drawing.Size(173, 506).
      THIS-OBJECT:splitContainerArbkodMallarStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerArbkodMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerArbkodMallarStrip:TabIndex = 1.
      /*  */
      /* ArbkodMallarStrip */
      /*  */
      THIS-OBJECT:ArbkodMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ArbkodMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ArbkodMallarStrip:Name = "ArbkodMallarStrip".
      THIS-OBJECT:ArbkodMallarStrip:Size = NEW System.Drawing.Size(173, 25).
      THIS-OBJECT:ArbkodMallarStrip:TabIndex = 4.
      THIS-OBJECT:ArbkodMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridArbKodMall */
      /*  */
      appearance159:BackColor = System.Drawing.SystemColors:Window.
      appearance159:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Appearance = appearance159.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance160:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance160:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance160:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance160:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:Appearance = appearance160.
      appearance161:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance161.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance162:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance162:BackColor2 = System.Drawing.SystemColors:Control.
      appearance162:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance162:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:PromptAppearance = appearance162.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance163:BackColor = System.Drawing.SystemColors:Window.
      appearance163:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveCellAppearance = appearance163.
      appearance164:BackColor = System.Drawing.SystemColors:Highlight.
      appearance164:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveRowAppearance = appearance164.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance165:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CardAreaAppearance = appearance165.
      appearance166:BorderColor = System.Drawing.Color:Silver.
      appearance166:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellAppearance = appearance166.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellPadding = 0.
      appearance167:BackColor = System.Drawing.SystemColors:Control.
      appearance167:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance167:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance167:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance167:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:GroupByRowAppearance = appearance167.
      appearance168:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderAppearance = appearance168.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance169:BackColor = System.Drawing.SystemColors:Window.
      appearance169:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:RowAppearance = appearance169.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance170:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance170.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbKodMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbKodMall:GuruContext = ?.
      THIS-OBJECT:GridArbKodMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbKodMall:Name = "GridArbKodMall".
      THIS-OBJECT:GridArbKodMall:Size = NEW System.Drawing.Size(173, 480).
      THIS-OBJECT:GridArbKodMall:TabIndex = 0.
      THIS-OBJECT:GridArbKodMall:Text = "grid1".
      /*  */
      /* splitContainerLopnrMallar */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnrMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnrMallar:Name = "splitContainerLopnrMallar".
      /*  */
      /* splitContainerLopnrMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerLopnrMallarStrip).
      /*  */
      /* splitContainerLopnrMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerBtnOverMall).
      THIS-OBJECT:splitContainerLopnrMallar:Size = NEW System.Drawing.Size(540, 506).
      THIS-OBJECT:splitContainerLopnrMallar:SplitterDistance = 217.
      THIS-OBJECT:splitContainerLopnrMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnrMallar:TabIndex = 0.
      /*  */
      /* splitContainerLopnrMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnrMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopnrMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Name = "splitContainerLopnrMallarStrip".
      THIS-OBJECT:splitContainerLopnrMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopnrMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:Controls:Add(THIS-OBJECT:LopnrMallarStrip).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerLopnrMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridLopMall).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Size = NEW System.Drawing.Size(217, 506).
      THIS-OBJECT:splitContainerLopnrMallarStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerLopnrMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnrMallarStrip:TabIndex = 1.
      /*  */
      /* LopnrMallarStrip */
      /*  */
      THIS-OBJECT:LopnrMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:LopnrMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopnrMallarStrip:Name = "LopnrMallarStrip".
      THIS-OBJECT:LopnrMallarStrip:Size = NEW System.Drawing.Size(217, 25).
      THIS-OBJECT:LopnrMallarStrip:TabIndex = 5.
      THIS-OBJECT:LopnrMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridLopMall */
      /*  */
      THIS-OBJECT:GridLopMall:AllowDrop = TRUE.
      appearance171:BackColor = System.Drawing.SystemColors:Window.
      appearance171:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopMall:DisplayLayout:Appearance = appearance171.
      THIS-OBJECT:GridLopMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance172:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance172:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance172:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance172:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:Appearance = appearance172.
      appearance173:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance173.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance174:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance174:BackColor2 = System.Drawing.SystemColors:Control.
      appearance174:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance174:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:PromptAppearance = appearance174.
      THIS-OBJECT:GridLopMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance175:BackColor = System.Drawing.SystemColors:Window.
      appearance175:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveCellAppearance = appearance175.
      appearance176:BackColor = System.Drawing.SystemColors:Highlight.
      appearance176:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveRowAppearance = appearance176.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance177:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CardAreaAppearance = appearance177.
      appearance178:BorderColor = System.Drawing.Color:Silver.
      appearance178:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellAppearance = appearance178.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellPadding = 0.
      appearance179:BackColor = System.Drawing.SystemColors:Control.
      appearance179:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance179:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance179:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance179:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:GroupByRowAppearance = appearance179.
      appearance180:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderAppearance = appearance180.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance181:BackColor = System.Drawing.SystemColors:Window.
      appearance181:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:RowAppearance = appearance181.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance182:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance182.
      THIS-OBJECT:GridLopMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopMall:GuruContext = ?.
      THIS-OBJECT:GridLopMall:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopMall:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopMall:Name = "GridLopMall".
      THIS-OBJECT:GridLopMall:Size = NEW System.Drawing.Size(217, 480).
      THIS-OBJECT:GridLopMall:TabIndex = 0.
      THIS-OBJECT:GridLopMall:Text = "grid1".
      THIS-OBJECT:GridLopMall:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopMall_GuruAfterDropped).
      /*  */
      /* splitContainerBtnOverMall */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBtnOverMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBtnOverMall:Name = "splitContainerBtnOverMall".
      /*  */
      /* splitContainerBtnOverMall.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:Controls:Add(THIS-OBJECT:ButtonOverMall).
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:Controls:Add(THIS-OBJECT:ButtonBackMall).
      /*  */
      /* splitContainerBtnOverMall.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:Controls:Add(THIS-OBJECT:splitContainerValdMallarBild).
      THIS-OBJECT:splitContainerBtnOverMall:Size = NEW System.Drawing.Size(322, 506).
      THIS-OBJECT:splitContainerBtnOverMall:SplitterDistance = 31.
      THIS-OBJECT:splitContainerBtnOverMall:SplitterWidth = 1.
      THIS-OBJECT:splitContainerBtnOverMall:TabIndex = 0.
      /*  */
      /* ButtonOverMall */
      /*  */
      appearance183:Image = CAST(resources:GetObject("appearance183.Image"), System.Object).
      THIS-OBJECT:ButtonOverMall:Appearance = appearance183.
      THIS-OBJECT:ButtonOverMall:Location = NEW System.Drawing.Point(5, 150).
      THIS-OBJECT:ButtonOverMall:Name = "ButtonOverMall".
      THIS-OBJECT:ButtonOverMall:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverMall:TabIndex = 14.
      THIS-OBJECT:ButtonOverMall:Text = "Markerade väljs".
      THIS-OBJECT:ButtonOverMall:Click:Subscribe(THIS-OBJECT:ButtonOverMall_Click).
      /*  */
      /* ButtonBackMall */
      /*  */
      appearance184:Image = CAST(resources:GetObject("appearance184.Image"), System.Object).
      THIS-OBJECT:ButtonBackMall:Appearance = appearance184.
      THIS-OBJECT:ButtonBackMall:Location = NEW System.Drawing.Point(5, 180).
      THIS-OBJECT:ButtonBackMall:Name = "ButtonBackMall".
      THIS-OBJECT:ButtonBackMall:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackMall:TabIndex = 15.
      THIS-OBJECT:ButtonBackMall:Text = "Markerade bort".
      THIS-OBJECT:ButtonBackMall:Click:Subscribe(THIS-OBJECT:ButtonBackMall_Click).
      /*  */
      /* splitContainerValdMallarBild */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdMallarBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdMallarBild:Name = "splitContainerValdMallarBild".
      /*  */
      /* splitContainerValdMallarBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerValdMallarBildStrip).
      /*  */
      /* splitContainerValdMallarBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerValdMallarBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerValdMallarBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerValdMallarBild:Size = NEW System.Drawing.Size(290, 506).
      THIS-OBJECT:splitContainerValdMallarBild:SplitterDistance = 224.
      THIS-OBJECT:splitContainerValdMallarBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerValdMallarBild:TabIndex = 0.
      /*  */
      /* splitContainerValdMallarBildStrip */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdMallarBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerValdMallarBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Name = "splitContainerValdMallarBildStrip".
      THIS-OBJECT:splitContainerValdMallarBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerValdMallarBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:Controls:Add(THIS-OBJECT:ValdMallarBildStrip).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerValdMallarBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:Controls:Add(THIS-OBJECT:GridValdMallar).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Size = NEW System.Drawing.Size(224, 506).
      THIS-OBJECT:splitContainerValdMallarBildStrip:SplitterDistance = 25.
      THIS-OBJECT:splitContainerValdMallarBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerValdMallarBildStrip:TabIndex = 1.
      /*  */
      /* ValdMallarBildStrip */
      /*  */
      THIS-OBJECT:ValdMallarBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdMallarBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdMallarBildStrip:Name = "ValdMallarBildStrip".
      THIS-OBJECT:ValdMallarBildStrip:Size = NEW System.Drawing.Size(224, 25).
      THIS-OBJECT:ValdMallarBildStrip:TabIndex = 6.
      THIS-OBJECT:ValdMallarBildStrip:Text = "toolStrip1".
      /*  */
      /* GridValdMallar */
      /*  */
      THIS-OBJECT:GridValdMallar:AllowDrop = TRUE.
      appearance185:BackColor = System.Drawing.SystemColors:Window.
      appearance185:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Appearance = appearance185.
      THIS-OBJECT:GridValdMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance186:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance186:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance186:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance186:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:Appearance = appearance186.
      appearance187:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance187.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance188:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance188:BackColor2 = System.Drawing.SystemColors:Control.
      appearance188:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance188:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance188.
      THIS-OBJECT:GridValdMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance189:BackColor = System.Drawing.SystemColors:Window.
      appearance189:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveCellAppearance = appearance189.
      appearance190:BackColor = System.Drawing.SystemColors:Highlight.
      appearance190:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveRowAppearance = appearance190.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance191:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CardAreaAppearance = appearance191.
      appearance192:BorderColor = System.Drawing.Color:Silver.
      appearance192:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellAppearance = appearance192.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellPadding = 0.
      appearance193:BackColor = System.Drawing.SystemColors:Control.
      appearance193:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance193:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance193:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance193:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:GroupByRowAppearance = appearance193.
      appearance194:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderAppearance = appearance194.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance195:BackColor = System.Drawing.SystemColors:Window.
      appearance195:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:RowAppearance = appearance195.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance196:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance196.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdMallar:GuruContext = ?.
      THIS-OBJECT:GridValdMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdMallar:Name = "GridValdMallar".
      THIS-OBJECT:GridValdMallar:Size = NEW System.Drawing.Size(224, 480).
      THIS-OBJECT:GridValdMallar:TabIndex = 0.
      THIS-OBJECT:GridValdMallar:Text = "grid1".
      THIS-OBJECT:GridValdMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdMallar_GuruAfterDropped).
      THIS-OBJECT:GridValdMallar:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdMallar_AfterCellUpdate).
      /*  */
      /* TabSek */
      /*  */
      THIS-OBJECT:TabSek:Controls:Add(THIS-OBJECT:splitContainerKatSek).
      THIS-OBJECT:TabSek:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabSek:Name = "TabSek".
      THIS-OBJECT:TabSek:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* splitContainerKatSek */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatSek:Name = "splitContainerKatSek".
      /*  */
      /* splitContainerKatSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Panel1:Controls:Add(THIS-OBJECT:GridKatSek).
      /*  */
      /* splitContainerKatSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvSek).
      THIS-OBJECT:splitContainerKatSek:Size = NEW System.Drawing.Size(1049, 506).
      THIS-OBJECT:splitContainerKatSek:SplitterDistance = 376.
      THIS-OBJECT:splitContainerKatSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatSek:TabIndex = 0.
      /*  */
      /* GridKatSek */
      /*  */
      appearance197:BackColor = System.Drawing.SystemColors:Window.
      appearance197:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatSek:DisplayLayout:Appearance = appearance197.
      THIS-OBJECT:GridKatSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance198:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance198:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance198:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance198:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:Appearance = appearance198.
      appearance199:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance199.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance200:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance200:BackColor2 = System.Drawing.SystemColors:Control.
      appearance200:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance200:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:PromptAppearance = appearance200.
      THIS-OBJECT:GridKatSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance201:BackColor = System.Drawing.SystemColors:Window.
      appearance201:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveCellAppearance = appearance201.
      appearance202:BackColor = System.Drawing.SystemColors:Highlight.
      appearance202:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveRowAppearance = appearance202.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance203:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CardAreaAppearance = appearance203.
      appearance204:BorderColor = System.Drawing.Color:Silver.
      appearance204:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellAppearance = appearance204.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellPadding = 0.
      appearance205:BackColor = System.Drawing.SystemColors:Control.
      appearance205:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance205:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance205:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance205:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:GroupByRowAppearance = appearance205.
      appearance206:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderAppearance = appearance206.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance207:BackColor = System.Drawing.SystemColors:Window.
      appearance207:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:RowAppearance = appearance207.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance208:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance208.
      THIS-OBJECT:GridKatSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatSek:GuruContext = ?.
      THIS-OBJECT:GridKatSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatSek:Name = "GridKatSek".
      THIS-OBJECT:GridKatSek:Size = NEW System.Drawing.Size(376, 506).
      THIS-OBJECT:GridKatSek:TabIndex = 0.
      THIS-OBJECT:GridKatSek:Text = "Grid1".
      THIS-OBJECT:GridKatSek:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatSek_AfterCellUpdate).
      /*  */
      /* splitContainerAnvSek */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvSek:Name = "splitContainerAnvSek".
      /*  */
      /* splitContainerAnvSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Panel1:Controls:Add(THIS-OBJECT:GridAnvSek).
      /*  */
      /* splitContainerAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKnappAnvSek).
      THIS-OBJECT:splitContainerAnvSek:Size = NEW System.Drawing.Size(672, 506).
      THIS-OBJECT:splitContainerAnvSek:SplitterDistance = 220.
      THIS-OBJECT:splitContainerAnvSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerAnvSek:TabIndex = 0.
      /*  */
      /* GridAnvSek */
      /*  */
      THIS-OBJECT:GridAnvSek:AllowDrop = TRUE.
      appearance209:BackColor = System.Drawing.SystemColors:Window.
      appearance209:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Appearance = appearance209.
      THIS-OBJECT:GridAnvSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnvSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance210:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance210:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance210:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance210:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:Appearance = appearance210.
      appearance211:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance211.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance212:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance212:BackColor2 = System.Drawing.SystemColors:Control.
      appearance212:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance212:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:PromptAppearance = appearance212.
      THIS-OBJECT:GridAnvSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnvSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance213:BackColor = System.Drawing.SystemColors:Window.
      appearance213:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveCellAppearance = appearance213.
      appearance214:BackColor = System.Drawing.SystemColors:Highlight.
      appearance214:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveRowAppearance = appearance214.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance215:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CardAreaAppearance = appearance215.
      appearance216:BorderColor = System.Drawing.Color:Silver.
      appearance216:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellAppearance = appearance216.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellPadding = 0.
      appearance217:BackColor = System.Drawing.SystemColors:Control.
      appearance217:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance217:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance217:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance217:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:GroupByRowAppearance = appearance217.
      appearance218:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderAppearance = appearance218.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance219:BackColor = System.Drawing.SystemColors:Window.
      appearance219:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:RowAppearance = appearance219.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance220:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance220.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnvSek:GuruContext = ?.
      THIS-OBJECT:GridAnvSek:GuruDraggable = TRUE.
      THIS-OBJECT:GridAnvSek:GuruDroppable = TRUE.
      THIS-OBJECT:GridAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnvSek:Name = "GridAnvSek".
      THIS-OBJECT:GridAnvSek:Size = NEW System.Drawing.Size(220, 506).
      THIS-OBJECT:GridAnvSek:TabIndex = 1.
      THIS-OBJECT:GridAnvSek:Text = "Grid1".
      THIS-OBJECT:GridAnvSek:GuruAfterDropped:Subscribe(THIS-OBJECT:GridAnvSek_GuruAfterDropped).
      /*  */
      /* splitContainerKnappAnvSek */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKnappAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKnappAnvSek:Name = "splitContainerKnappAnvSek".
      /*  */
      /* splitContainerKnappAnvSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:Controls:Add(THIS-OBJECT:ButtonOverAnvMark).
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:Controls:Add(THIS-OBJECT:ButtonBackAnvMark).
      /*  */
      /* splitContainerKnappAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerBildSek).
      THIS-OBJECT:splitContainerKnappAnvSek:Size = NEW System.Drawing.Size(451, 506).
      THIS-OBJECT:splitContainerKnappAnvSek:SplitterDistance = 31.
      THIS-OBJECT:splitContainerKnappAnvSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKnappAnvSek:TabIndex = 0.
      /*  */
      /* ButtonOverAnvMark */
      /*  */
      appearance221:Image = CAST(resources:GetObject("appearance221.Image"), System.Object).
      THIS-OBJECT:ButtonOverAnvMark:Appearance = appearance221.
      THIS-OBJECT:ButtonOverAnvMark:Location = NEW System.Drawing.Point(0, 137).
      THIS-OBJECT:ButtonOverAnvMark:Name = "ButtonOverAnvMark".
      THIS-OBJECT:ButtonOverAnvMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverAnvMark:TabIndex = 10.
      THIS-OBJECT:ButtonOverAnvMark:Text = "Markerade väljs".
      THIS-OBJECT:ButtonOverAnvMark:Click:Subscribe(THIS-OBJECT:ButtonOverAnvMark_Click).
      /*  */
      /* ButtonBackAnvMark */
      /*  */
      appearance222:Image = CAST(resources:GetObject("appearance222.Image"), System.Object).
      THIS-OBJECT:ButtonBackAnvMark:Appearance = appearance222.
      THIS-OBJECT:ButtonBackAnvMark:Location = NEW System.Drawing.Point(0, 168).
      THIS-OBJECT:ButtonBackAnvMark:Name = "ButtonBackAnvMark".
      THIS-OBJECT:ButtonBackAnvMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackAnvMark:TabIndex = 11.
      THIS-OBJECT:ButtonBackAnvMark:Text = "Markerade bort".
      THIS-OBJECT:ButtonBackAnvMark:Click:Subscribe(THIS-OBJECT:ButtonBackAnvMark_Click).
      /*  */
      /* splitContainerBildSek */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBildSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBildSek:Name = "splitContainerBildSek".
      /*  */
      /* splitContainerBildSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Panel1:Controls:Add(THIS-OBJECT:GridValdaAnvSek).
      /*  */
      /* splitContainerBildSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerBildSek.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerBildSek:Size = NEW System.Drawing.Size(419, 506).
      THIS-OBJECT:splitContainerBildSek:SplitterDistance = 222.
      THIS-OBJECT:splitContainerBildSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerBildSek:TabIndex = 0.
      /*  */
      /* GridValdaAnvSek */
      /*  */
      THIS-OBJECT:GridValdaAnvSek:AllowDrop = TRUE.
      appearance223:BackColor = System.Drawing.SystemColors:Window.
      appearance223:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Appearance = appearance223.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance224:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance224:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance224:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance224:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:Appearance = appearance224.
      appearance225:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance225.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance226:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance226:BackColor2 = System.Drawing.SystemColors:Control.
      appearance226:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance226:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:PromptAppearance = appearance226.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance227:BackColor = System.Drawing.SystemColors:Window.
      appearance227:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveCellAppearance = appearance227.
      appearance228:BackColor = System.Drawing.SystemColors:Highlight.
      appearance228:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveRowAppearance = appearance228.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance229:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CardAreaAppearance = appearance229.
      appearance230:BorderColor = System.Drawing.Color:Silver.
      appearance230:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellAppearance = appearance230.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellPadding = 0.
      appearance231:BackColor = System.Drawing.SystemColors:Control.
      appearance231:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance231:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance231:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance231:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:GroupByRowAppearance = appearance231.
      appearance232:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderAppearance = appearance232.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance233:BackColor = System.Drawing.SystemColors:Window.
      appearance233:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:RowAppearance = appearance233.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance234:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance234.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaAnvSek:GuruContext = ?.
      THIS-OBJECT:GridValdaAnvSek:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaAnvSek:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaAnvSek:Name = "GridValdaAnvSek".
      THIS-OBJECT:GridValdaAnvSek:Size = NEW System.Drawing.Size(222, 506).
      THIS-OBJECT:GridValdaAnvSek:TabIndex = 2.
      THIS-OBJECT:GridValdaAnvSek:Text = "Grid1".
      THIS-OBJECT:GridValdaAnvSek:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaAnvSek_GuruAfterDropped).
      /*  */
      /* TabKalkylAdm */
      /*  */
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabDelKataloger).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKoder).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabSek).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
      THIS-OBJECT:TabKalkylAdm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabKalkylAdm:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabKalkylAdm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabKalkylAdm:Name = "TabKalkylAdm".
      THIS-OBJECT:TabKalkylAdm:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabKalkylAdm:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:TabKalkylAdm:TabIndex = 0.
      THIS-OBJECT:TabKalkylAdm:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance235:Image = CAST(resources:GetObject("appearance235.Image"), System.Object).
      ultraTab2:Appearance = appearance235.
      ultraTab2:Key = "Delkataloger".
      ultraTab2:TabPage = THIS-OBJECT:TabDelKataloger.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Delkataloger".
      appearance236:Image = CAST(resources:GetObject("appearance236.Image"), System.Object).
      ultraTab1:Appearance = appearance236.
      ultraTab1:Key = "Kataloger".
      ultraTab1:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Kataloger".
      appearance237:Image = CAST(resources:GetObject("appearance237.Image"), System.Object).
      ultraTab3:Appearance = appearance237.
      ultraTab3:Key = "Koder".
      ultraTab3:TabPage = THIS-OBJECT:TabKoder.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Arbetskoder".
      ultraTab4:Key = "Visa".
      ultraTab4:TabPage = THIS-OBJECT:TabVisa.
      ultraTab4:Text = "Visa".
      ultraTab4:ToolTipText = "Visa kataloger".
      appearance238:Image = CAST(resources:GetObject("appearance238.Image"), System.Object).
      ultraTab6:Appearance = appearance238.
      ultraTab6:Key = "Mallar".
      ultraTab6:TabPage = THIS-OBJECT:ultraTabPageControl1.
      ultraTab6:Text = "".
      ultraTab6:ToolTipText = "Mallar".
      appearance239:Image = CAST(resources:GetObject("appearance239.Image"), System.Object).
      ultraTab5:Appearance = appearance239.
      ultraTab5:Key = "Sekretess".
      ultraTab5:TabPage = THIS-OBJECT:TabSek.
      ultraTab5:Text = "".
      ultraTab5:ToolTipText = "Sekretess".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 6 NO-UNDO.
      arrayvar6[1] = ultraTab2.
      arrayvar6[2] = ultraTab1.
      arrayvar6[3] = ultraTab3.
      arrayvar6[4] = ultraTab4.
      arrayvar6[5] = ultraTab6.
      arrayvar6[6] = ultraTab5.
      THIS-OBJECT:TabKalkylAdm:Tabs:AddRange(arrayvar6).
      THIS-OBJECT:TabKalkylAdm:SelectedTabChanging:Subscribe(THIS-OBJECT:TabKalkylAdm_SelectedTabChanging).
      THIS-OBJECT:TabKalkylAdm:SelectedTabChanged:Subscribe(THIS-OBJECT:TabKalkylAdm_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1049, 506).
      /*  */
      /* KalkylAdm */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabKalkylAdm).
      THIS-OBJECT:Name = "KalkylAdm".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:TabDelKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatDel:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatPrisDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatPrisDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatHuvKopStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatHuvKopStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatStrip:PerformLayout().
      CAST(THIS-OBJECT:GridKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPrisKopDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisKopDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatHuvKop, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatHuvKop:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopSubKat, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopSubKat:ResumeLayout(FALSE).
      THIS-OBJECT:NyDelKatStrip:ResumeLayout(FALSE).
      THIS-OBJECT:NyDelKatStrip:PerformLayout().
      CAST(THIS-OBJECT:GridSubKatDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatBildDelStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKatHuvDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatalog:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatalogSubKatStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSubKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerPrisStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPrisStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisStrip:ResumeLayout(FALSE).
      THIS-OBJECT:PrisStrip:ResumeLayout(FALSE).
      THIS-OBJECT:PrisStrip:PerformLayout().
      CAST(THIS-OBJECT:GridKatPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisningBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisningBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBildStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridVisning, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerGrundKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatKoderStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatKoderStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSubKatKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopKoder:ResumeLayout(FALSE).
      THIS-OBJECT:KoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:LopNrStrip:ResumeLayout(FALSE).
      THIS-OBJECT:LopNrStrip:PerformLayout().
      CAST(THIS-OBJECT:GridLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopsubBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopsubBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBildStrip:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopSubFrek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopSubFrek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridLopnumsub, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMallarBas:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMallarBas:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMallarBas, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMallarBas:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopMallar:ResumeLayout(FALSE).
      THIS-OBJECT:NyKopMallarStrip:ResumeLayout(FALSE).
      THIS-OBJECT:NyKopMallarStrip:PerformLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSubMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerArbkodMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerArbkodMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbKodMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnrMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnrMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBtnOverMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBtnOverMall:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValdMallarBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdMallarBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValdMallarBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdMallarBildStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKatSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKnappAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKnappAnvSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBildSek:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBildSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBildSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkylAdm:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	

   METHOD PUBLIC VOID StartKalkAdm():
      
   END METHOD.   
   METHOD PUBLIC VOID CreateForm():
     
      THIS-OBJECT:CreateComboTyptt().
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateFunctions().
      THIS-OBJECT:CreateRibbon().
      THIS-OBJECT:ValueGridSubKatKoderCh(?,?).
      THIS-OBJECT:ForetagAnpass().
      THIS-OBJECT:TabKalkylAdm:Tabs["Koder"]:Selected = TRUE.
      
   END METHOD.
   METHOD PUBLIC VOID CreateFunctions():
       /*Drag dropp*/
      THIS-OBJECT:DGAnvand = NEW Controls.Subclasses.DragDropGroup("Anvandare").
      THIS-OBJECT:DGAnvand:AddChild(THIS-OBJECT:GridAnvSek).
      THIS-OBJECT:DGAnvand:AddChild(THIS-OBJECT:GridValdaAnvSek).
      
      THIS-OBJECT:DGHuvkat = NEW Controls.Subclasses.DragDropGroup("Kataloger").
      THIS-OBJECT:DGHuvkat:AddChild(THIS-OBJECT:GridKatalog).
      THIS-OBJECT:DGHuvkat:AddChild(THIS-OBJECT:GridKatHuvDel).
      
      THIS-OBJECT:DGLopMallar = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGLopMallar:AddChild(THIS-OBJECT:GridLopMall).
      THIS-OBJECT:DGLopMallar:AddChild(THIS-OBJECT:GridValdMallar). 
      
       /* Button overs*/
      THIS-OBJECT:ButtonOverAnvMark:GuruKoppla(THIS-OBJECT:GridAnvSek,THIS-OBJECT:GridValdaAnvSek).
      THIS-OBJECT:ButtonOverAnvMark:GuruDublett = FALSE.
      THIS-OBJECT:ButtonBackAnvMark:GuruKoppla(THIS-OBJECT:GridValdaAnvSek,THIS-OBJECT:GridAnvSek).
      THIS-OBJECT:ButtonBackAnvMark:GuruRemoveFrom = TRUE.  
       /* Button overs*/
      THIS-OBJECT:ButtonOverHuvKat:GuruKoppla(THIS-OBJECT:GridKatalog,THIS-OBJECT:GridKatHuvDel).
      THIS-OBJECT:ButtonOverHuvKat:GuruDublett = FALSE.
      THIS-OBJECT:ButtonBackHuvKat:GuruKoppla(THIS-OBJECT:GridKatHuvDel,THIS-OBJECT:GridKatalog).
      THIS-OBJECT:ButtonBackHuvKat:GuruRemoveFrom = TRUE.  
      /* Button overs*/
      THIS-OBJECT:ButtonOverMall:GuruKoppla(THIS-OBJECT:GridLopMall,THIS-OBJECT:GridValdMallar).
      THIS-OBJECT:ButtonOverMall:GuruDublett = FALSE.
      THIS-OBJECT:ButtonBackMall:GuruKoppla(THIS-OBJECT:GridValdMallar,THIS-OBJECT:GridLopMall).
      THIS-OBJECT:ButtonBackMall:GuruRemoveFrom = TRUE.  
   END METHOD.
   METHOD PUBLIC VOID CreateComboTyptt():
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
         typtt.TYPKALK = 0
         typtt.TYPC    = "Alla".
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
     
   METHOD PUBLIC VOID CreateRibbon():
        /*första ribbon tab*/
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      /*öriag tabbar om inte i Guru.ModuleManager*/
      /*     THIS-OBJECT:Root:ModuleManager:RibbonTab("kkkc","pcpp"). */
      /* Skapa ribbongrupp i kalkyltaben */
      
      THIS-OBJECT:RibbonGroupAdm = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Välj administration").
      THIS-OBJECT:RibbonGroupKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Katalog"). /**/
      THIS-OBJECT:RibbonGroupKatalog:Caption = "Kataloger".
      THIS-OBJECT:RibbonGroupDelKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Delkatalog").   /**/
      THIS-OBJECT:RibbonGroupDelKatalog:Caption = "Delkataloger".
      THIS-OBJECT:RibbonGroupPrisKatalog = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("PrisKatalog"). /**/
      THIS-OBJECT:RibbonGroupPrisKatalog:Caption = "Katalogpriser".
      THIS-OBJECT:RibbonGroupVisaPristyp = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visningsregeler").
      THIS-OBJECT:RibbonGroupArbetskoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Arbetskoder").
      
      THIS-OBJECT:RibbonGroupSpara = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Spara inställningar").
      
      THIS-OBJECT:RibbonDummy = NEW Infragistics.Win.UltraWinToolbars.LabelTool("RD").
      THIS-OBJECT:RibbonDummy:SharedPropsInternal:Caption = " ".
      /*adm*/
      THIS-OBJECT:RibbonToolAdmKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmKod").
      THIS-OBJECT:RibbonToolAdmKoder:SharedPropsInternal:Caption = "Koder".
      THIS-OBJECT:RibbonToolAdmKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmKatalog").
      THIS-OBJECT:RibbonToolAdmKatalog:SharedPropsInternal:Caption = "Kataloger".
      THIS-OBJECT:RibbonToolAdmKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmDelKatalog").
      THIS-OBJECT:RibbonToolAdmDelKatalog:SharedPropsInternal:Caption = "Delkataloger".
      THIS-OBJECT:RibbonToolAdmDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmPriser").
      THIS-OBJECT:RibbonToolAdmPriser:SharedPropsInternal:Caption = "Priser".
      THIS-OBJECT:RibbonToolAdmPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmSek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmSek").
      THIS-OBJECT:RibbonToolAdmSek:SharedPropsInternal:Caption = "Sekretess".
      THIS-OBJECT:RibbonToolAdmSek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAdmFrek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("AdmFrek").
      THIS-OBJECT:RibbonToolAdmFrek:SharedPropsInternal:Caption = "Frekvenstabeller".
      THIS-OBJECT:RibbonToolAdmFrek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*Kataloger*/
      THIS-OBJECT:RibbonToolKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Katalog").
      THIS-OBJECT:RibbonToolKatalog:SharedPropsInternal:Caption = "Kataloger".
      THIS-OBJECT:RibbonToolKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolArtal = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaartal").
      THIS-OBJECT:RibbonToolArtal:SharedPropsInternal:Caption = "Visa årtal".
      THIS-OBJECT:RibbonToolArtal:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSek = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visasek").
      THIS-OBJECT:RibbonToolSek:SharedPropsInternal:Caption = "Visa sekretess".
      THIS-OBJECT:RibbonToolSek:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolStyrande = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visadelk").
      THIS-OBJECT:RibbonToolStyrande:SharedPropsInternal:Caption = "Visa delkatalognr".
      THIS-OBJECT:RibbonToolStyrande:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSkapad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaskapad").
      THIS-OBJECT:RibbonToolSkapad:SharedPropsInternal:Caption = "Visa skapad".
      THIS-OBJECT:RibbonToolSkapad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolAvslutad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visaavsl").
      THIS-OBJECT:RibbonToolAvslutad:SharedPropsInternal:Caption = "Visa avslutad".
      THIS-OBJECT:RibbonToolAvslutad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*DelKataloger*/
      /* BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Delkatalog").
      THIS-OBJECT:RibbonToolDelKatalog:SharedPropsInternal:Caption = "Delkatalog".
      THIS-OBJECT:RibbonToolDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolKopplaDelKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("KopplaDelKatalog").
      THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Caption = "Koppla delkataloger".
      THIS-OBJECT:RibbonToolKopplaDelKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      */
      THIS-OBJECT:RibbonToolDelSkapad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visadelskapad").
      THIS-OBJECT:RibbonToolDelSkapad:SharedPropsInternal:Caption = "Visa skapad".
      THIS-OBJECT:RibbonToolDelSkapad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolDelAvslutad = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Visdelavsl").
      THIS-OBJECT:RibbonToolDelAvslutad:SharedPropsInternal:Caption = "Visa avslutad".
      THIS-OBJECT:RibbonToolDelAvslutad:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*prislistor*/
      THIS-OBJECT:RibbonToolPrisKatalog = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("PrisKatalog").
      THIS-OBJECT:RibbonToolPrisKatalog:SharedPropsInternal:Caption = "Katalogpriser".
      THIS-OBJECT:RibbonToolPrisKatalog:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolSokBen = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("SokBenPris").
      THIS-OBJECT:RibbonToolSokBen:SharedPropsInternal:Caption = "Visa sökbenämning".
      THIS-OBJECT:RibbonToolSokBen:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolTaEgnaKoder = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("TaEgnaKoder").
      THIS-OBJECT:RibbonToolTaEgnaKoder:SharedPropsInternal:Caption = "Visa egna koder".
      THIS-OBJECT:RibbonToolTaEgnaKoder:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      THIS-OBJECT:RibbonToolTaEgnaPriser = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("TaEgnaPriser").
      THIS-OBJECT:RibbonToolTaEgnaPriser:SharedPropsInternal:Caption = "Visa egna priser".
      THIS-OBJECT:RibbonToolTaEgnaPriser:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*visnings regler*/
      THIS-OBJECT:RibbonToolVisningRegel = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisRegel").
      THIS-OBJECT:RibbonToolVisningRegel:SharedPropsInternal:Caption = "Visningsregler".
      THIS-OBJECT:RibbonToolVisningRegel:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      /*arbkoder*/
      THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa för".
      THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
      THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp".
      THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width = 135.
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
      THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      
      THIS-OBJECT:RibbonToolLopnr = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaLopnr").
      THIS-OBJECT:RibbonToolLopnr:SharedPropsInternal:Caption = "Visa löpnummerpriser".
      THIS-OBJECT:RibbonToolLopnr:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolLopnr:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolFrekvensKom = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FrekvensKom").
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Caption = "Frekvens-kommentar".
      THIS-OBJECT:RibbonToolFrekvensKom:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolFrekvensKom:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolTypKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("VisaTypKalk").
      THIS-OBJECT:RibbonToolTypKalk:SharedPropsInternal:Caption = "Visa kalkyltyp".
      THIS-OBJECT:RibbonToolTypKalk:SharedPropsInternal:Width = THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Width.
      THIS-OBJECT:RibbonToolTypKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /*spara*/
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("SaveDefs").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Caption = "Spara inställningar".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolSparaDefs,INPUT "bilder/kalkyl-save.gif").
      THIS-OBJECT:RibbonToolSparaDefs:SharedPropsInternal:Visible = TRUE.
      /*prep*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonDummy).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmKoder,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmKatalog,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmDelKatalog,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmPriser,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmSek,THIS-OBJECT:RibbonGroupAdm).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAdmFrek,THIS-OBJECT:RibbonGroupAdm).
                
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKatalog,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolArtal,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSek,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkapad,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAvslutad,RibbonGroupKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolStyrande,RibbonGroupKatalog).
      /*BEHÖVS EJ
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelKatalog,RibbonGroupDelKatalog).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelSkapad,RibbonGroupDelKatalog).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelAvslutad,RibbonGroupDelKatalog).
      /*BEHÖVS EJ
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKopplaDelKatalog,RibbonGroupDelKatalog).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolPrisKatalog,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSokBen,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTaEgnaKoder,RibbonGroupPrisKatalog).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTaEgnaPriser,RibbonGroupPrisKatalog).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisningRegel,RibbonGroupVisaPristyp).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolTypKalk,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLopnr,RibbonGroupArbetskoder).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFrekvensKom,RibbonGroupArbetskoder).
      /*
      THIS-OBJECT:RibbonGroupArbetskoder:Tools:Add(THIS-OBJECT:RibbonDummy).
      */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSparaDefs,RibbonGroupSpara).
      /*nästan alla start med true*/
      ASSIGN 
      THIS-OBJECT:RibbonToolAdmKoder:Checked = FALSE
      THIS-OBJECT:RibbonToolAdmKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmDelKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmPriser:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmSek:Checked = TRUE
      THIS-OBJECT:RibbonToolAdmFrek:Checked = TRUE
      
      
      THIS-OBJECT:RibbonToolKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolArtal:Checked = TRUE
      THIS-OBJECT:RibbonToolSek:Checked = TRUE
      THIS-OBJECT:RibbonToolStyrande:Checked = TRUE
      THIS-OBJECT:RibbonToolSkapad:Checked = TRUE
      THIS-OBJECT:RibbonToolAvslutad:Checked = TRUE
      /*BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolKopplaDelKatalog:Checked = TRUE
      */
      THIS-OBJECT:RibbonToolDelSkapad:Checked = TRUE
      THIS-OBJECT:RibbonToolDelAvslutad:Checked = TRUE
      
      THIS-OBJECT:RibbonToolPrisKatalog:Checked = TRUE
      THIS-OBJECT:RibbonToolSokBen:Checked = TRUE
      THIS-OBJECT:RibbonToolTaEgnaKoder:Checked = TRUE
      THIS-OBJECT:RibbonToolTaEgnaPriser:Checked = TRUE
      
      THIS-OBJECT:RibbonToolVisningRegel:Checked = TRUE
      
      THIS-OBJECT:RibbonToolLopnr:Checked = FALSE.
      THIS-OBJECT:RibbonToolFrekvensKom:Checked = TRUE.
      THIS-OBJECT:RibbonToolTypKalk:Checked = TRUE.
      
      /*eventS triggers*/
      THIS-OBJECT:RibbonToolKatalog:ToolClick:SUBSCRIBE(RibbonKatalogChanged).
      THIS-OBJECT:RibbonToolArtal:ToolClick:SUBSCRIBE(RibbonArtalChanged).
      THIS-OBJECT:RibbonToolSek:ToolClick:SUBSCRIBE(RibbonSekKatalogChanged).
      THIS-OBJECT:RibbonToolStyrande:ToolClick:SUBSCRIBE(RibbonStyrandeChanged).
      THIS-OBJECT:RibbonToolSkapad:ToolClick:SUBSCRIBE(RibbonSkapKatalogChanged).
      THIS-OBJECT:RibbonToolAvslutad:ToolClick:SUBSCRIBE(RibbonAvsKatalogChanged).
      /*BEHÖVS EJ
      THIS-OBJECT:RibbonToolDelKatalog:ToolClick:SUBSCRIBE(RibbonDelKatalogChanged).
      THIS-OBJECT:RibbonToolKopplaDelKatalog:ToolClick:SUBSCRIBE(RibbonKopplaDelKatalogChanged).
      */
      THIS-OBJECT:RibbonToolDelSkapad:ToolClick:SUBSCRIBE(RibbonSkapDelKatalogChanged).
      THIS-OBJECT:RibbonToolDelAvslutad:ToolClick:SUBSCRIBE(RibbonAvsDelKatalogChanged).
      
      THIS-OBJECT:RibbonToolPrisKatalog:ToolClick:SUBSCRIBE(RibbonPrisKatChanged).
      THIS-OBJECT:RibbonToolSokBen:ToolClick:SUBSCRIBE(RibbonSokBenChanged).
      THIS-OBJECT:RibbonToolTaEgnaKoder:ToolClick:SUBSCRIBE(RibbonTaEgnaKoderChanged).
      THIS-OBJECT:RibbonToolTaEgnaPriser:ToolClick:SUBSCRIBE(RibbonTaEgnaPriserChanged).
      
      THIS-OBJECT:RibbonToolVisningRegel:ToolClick:SUBSCRIBE(RibbonVisningRegelChanged).
      
      THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
      THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder). 
      THIS-OBJECT:RibbonToolFrekvensKom:ToolClick:Subscribe(THIS-OBJECT:RibbonFrekvensKomChanged).
      THIS-OBJECT:RibbonToolTypKalk:ToolClick:Subscribe(THIS-OBJECT:RibbonTypKalkChanged).
      
      THIS-OBJECT:RibbonToolLopnr:ToolClick:Subscribe(THIS-OBJECT:RibbonLopnrChanged).
      THIS-OBJECT:RibbonToolSparaDefs:ToolClick:SUBSCRIBE(SparaDefs).
      
      /* Skapa defaults om det behövs OCH SÄTTER VÄRDE*/
      THIS-OBJECT:RefreshMarkning(TRUE).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupAdm).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupDelKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupPrisKatalog).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupVisaPristyp).
      Helpers.Functions:CreateDefaults(THIS-OBJECT:Root,"KALKYLADM",0,"",THIS-OBJECT:RibbonGroupArbetskoder).
      
   END METHOD.

   METHOD PUBLIC VOID RibbonKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolKatalog:Checked EQ TRUE THEN DO:
         /*THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = FALSE.
         THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = FALSE.*/
                  
      END.
      ELSE DO:
         /*IF THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE.*/
        
      END.
    
   END METHOD.
   METHOD PUBLIC VOID RibbonLopnrChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolLopnr:Checked = TRUE THEN THIS-OBJECT:RibbonToolFrekvensKom:Checked = FALSE.   
      IF THIS-OBJECT:RibbonToolLopnr:Checked = FALSE THEN THIS-OBJECT:RibbonToolFrekvensKom:Checked = TRUE.
      THIS-OBJECT:splitContainerLopsubBild:Panel1Collapsed = FALSE.      
   END METHOD.
   
   METHOD PUBLIC VOID RibbonFrekvensKomChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
         ASSIGN
         THIS-OBJECT:RibbonToolLopnr:Checked                     = FALSE 
         THIS-OBJECT:GridFrekvens:VISIBLE                           = TRUE
         THIS-OBJECT:GridLopnumsub:VISIBLE                        = FALSE
         THIS-OBJECT:splitContainerLopSubFrek:Panel2Collapsed = FALSE.
         THIS-OBJECT:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
         THIS-OBJECT:ValueGridLopnrCh(?,?).             
      END.   
      ELSE DO:
         ASSIGN
         THIS-OBJECT:splitContainerLopSubFrek:Panel2Collapsed = TRUE
         THIS-OBJECT:GridFrekvens:VISIBLE                           = FALSE
         THIS-OBJECT:GridLopnumsub:VISIBLE                        = TRUE.
      END.      
      THIS-OBJECT:splitContainerLopsubBild:Panel1Collapsed = FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID RibbonTypKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTypKalk:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridArbKoder:DisplayLayout:Bands[0]:Columns["TYPKALK"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridArbKoder:DisplayLayout:Bands[0]:Columns["TYPKALK"]:HIDDEN =  TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonArtalChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolArtal:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = FALSE.                  
      END.
      ELSE DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["VISARTAL"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonSekKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSek:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["SEKRETESS"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonStyrandeChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolStyrande:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["HKLOGSUBID"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonSkapKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSkapad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonAvsKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolAvslutad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatHuvDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:GridKatSek:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RibbonDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*
      IF THIS-OBJECT:RibbonToolDelKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = FALSE.
         THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = FALSE.
         IF THIS-OBJECT:RibbonToolKatalog:Checked = FALSE THEN THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = FALSE.
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerKatalogSubKat:Panel1Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerKatalogSubKat:Panel2Collapsed = TRUE.
         
         IF THIS-OBJECT:splitContainerSubKatKoder:Panel2Collapsed = TRUE THEN  THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerSubKatKoder:Panel1Collapsed = TRUE.
      END.
      */
      
   END METHOD.
   METHOD PUBLIC VOID RibbonKopplaDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      /*BEHÖVS EJ
      IF THIS-OBJECT:RibbonToolKopplaDelKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerPrisKopDel:Panel2Collapsed = FALSE. 
         THIS-OBJECT:splitContainerSubKatPrisDel:Panel2Collapsed = FALSE.       
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerPrisKopDel:Panel1Collapsed = TRUE THEN THIS-OBJECT:splitContainerSubKatPrisDel:Panel2Collapsed = TRUE.
         THIS-OBJECT:splitContainerPrisKopDel:Panel2Collapsed = TRUE.
      END.
      */
   END METHOD.
   METHOD PUBLIC VOID RibbonSkapDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolDelSkapad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:GridSubKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
         THIS-OBJECT:GridSubKatDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:GridSubKatalog:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
         THIS-OBJECT:GridSubKatDel:DisplayLayout:Bands[0]:Columns["INDATUM"]:HIDDEN = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonAvsDelKatalogChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolDelAvslutad:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
         THIS-OBJECT:GridSubKatDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridSubKatKoder:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:GridSubKatalog:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
         THIS-OBJECT:GridSubKatDel:DisplayLayout:Bands[0]:Columns["AVSLUTAD"]:HIDDEN = TRUE.
      END.
   END METHOD.

   METHOD PUBLIC VOID RibbonPrisKatChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolPrisKatalog:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerKatalog:Panel2Collapsed = FALSE.
         THIS-OBJECT:splitContainerPris:Panel1Collapsed = FALSE.
         THIS-OBJECT:splitContainerLopsubBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:splitContainerSubKatPrisDel:Panel2Collapsed = FALSE.              
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerPris:Panel2Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel2Collapsed = TRUE.
         THIS-OBJECT:splitContainerPris:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerLopsubBild:Panel1Collapsed = TRUE.
                 
      END.
      
   END METHOD.
   METHOD PUBLIC VOID RibbonSokBenChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolSokBen:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["SOKBENAMNING"]:HIDDEN = FALSE.      
      END.
      ELSE DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["SOKBENAMNING"]:HIDDEN = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonTaEgnaKoderChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTaEgnaKoder:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENKODUPP"]:HIDDEN = FALSE.         
      END.
      ELSE DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENKODUPP"]:HIDDEN = TRUE.         
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonTaEgnaPriserChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolTaEgnaPriser:Checked EQ TRUE THEN DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENPRISUPP"]:HIDDEN = FALSE.         
      END.
      ELSE DO:
         THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["EGENPRISUPP"]:HIDDEN = TRUE.        
      END.
   END METHOD.
   METHOD PUBLIC VOID RibbonVisningRegelChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      IF THIS-OBJECT:RibbonToolVisningRegel:Checked EQ TRUE THEN DO:
         THIS-OBJECT:splitContainerVisningBild:Panel1Collapsed = FALSE.
         THIS-OBJECT:splitContainerKatalog:Panel2Collapsed = FALSE.
      END.
      ELSE DO:
         IF THIS-OBJECT:splitContainerVisningBild:Panel1Collapsed = TRUE THEN  THIS-OBJECT:splitContainerKatalog:Panel2Collapsed = TRUE.
         THIS-OBJECT:splitContainerVisningBild:Panel1Collapsed = TRUE.
      END.
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
      IF THIS-OBJECT:GridArbKoder:Initierad = FALSE THEN RETURN.                    
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 THEN DO:
         THIS-OBJECT:RefreshArbKoder(?,?).
      END.
   END METHOD.
   
   METHOD PUBLIC VOID RefreshMarkning(INPUT ladda AS LOGICAL):
      DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
      IF ladda = TRUE THEN DO:
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
         /*
         IF THIS-OBJECT:GetVisaTyp() = 0 THEN Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " NO-LOCK".
         ELSE Q = "FOR EACH " + THIS-OBJECT:MarkningTTh:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
         */
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
         THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
      END.   
      
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
     
      Typ = THIS-OBJECT:GetVisaTyp().
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      IF Markning EQ "Alla" OR Markning EQ "" THEN DoAll = TRUE.
      ELSE DoAll = FALSE.
      IF Markning EQ "Ej märkta" THEN DoUnmarked = TRUE.
      ELSE DoUnmarked = FALSE.
      THIS-OBJECT:GridLopnr:GuruTom().
      THIS-OBJECT:GridLopnumsub:GuruTom().
      IF DoAll EQ TRUE THEN DO:
         IF Typ = 0 THEN THIS-OBJECT:GridArbKoder:GuruFiltrera("").
         ELSE THIS-OBJECT:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ)).
         THIS-OBJECT:GridArbKoder:GuruFirstrow().
         THIS-OBJECT:ValueGridArbKoderCh(?,?).
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            IF Typ = 0 THEN THIS-OBJECT:GridArbKoder:GuruFiltrera("MARKNING = ''").
            ELSE THIS-OBJECT:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = ''").
            THIS-OBJECT:GridArbKoder:GuruFirstrow().
            THIS-OBJECT:ValueGridArbKoderCh(?,?).
         END.
         ELSE DO:
            IF Typ = 0 THEN THIS-OBJECT:GridArbKoder:GuruFiltrera("MARKNING = '" + Markning + "'").
            ELSE THIS-OBJECT:GridArbKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "'").
            THIS-OBJECT:GridArbKoder:GuruFirstrow().
            THIS-OBJECT:ValueGridArbKoderCh(?,?).
         END.
         IF THIS-OBJECT:GridArbKoder:tBS:Count EQ 0 THEN DO:
            THIS-OBJECT:GridLopnr:GuruTom().
            THIS-OBJECT:GridLopnumsub:GuruTom().
            RETURN.
         END.      
      END.
      
      /*
      IF THIS-OBJECT:ArbetsKoderTTh:AVAILABLE EQ TRUE THEN DO:
         IF Typ = 0 THEN DO:
            THIS-OBJECT:LopposterTTh:FIND-FIRST() NO-ERROR.
            THIS-OBJECT:GridLopnr:GuruFiltrera("ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         END.   
         ELSE DO:
           THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE TYPKALK = " + STRING(Typ)) NO-ERROR.
           THIS-OBJECT:GridLopnr:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         END.
         GridLopnr:GuruFirstrow(). 
      END.
      ELSE DO:
         THIS-OBJECT:GridLopnr:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:GridLopnr:GuruFirstrow().
      END.
      */
   END METHOD.
   
   METHOD PUBLIC VOID SparaDefs(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:Root:DatabaseManager:Global:SaveDefaultVALUEs(INPUT "KalkylAdm",INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT 0,INPUT "") .
   END METHOD.
   METHOD PUBLIC VOID ForetagAnpass():
     THIS-OBJECT:RibbonGroupAdm:VISIBLE   = FALSE.
     THIS-OBJECT:TabKalkylAdm:Tabs["Visa"]:VISIBLE   = FALSE.
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridKatalog().  
      THIS-OBJECT:CreateGridSubKatalog().
      THIS-OBJECT:CreateGridKatPris().
      THIS-OBJECT:CreateGridVisning().
      
      THIS-OBJECT:CreateGridArbKoder().
      THIS-OBJECT:CreateGridLopnr().
      THIS-OBJECT:CreateGridLopnumsub().
      THIS-OBJECT:CreateGridAnvSek().
      THIS-OBJECT:CreateGridFrekvens().
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridValdMallar().
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:GridKatalog:GuruFirstrow().  
      THIS-OBJECT:ValueGridKatalogCh(?,?).
      THIS-OBJECT:GridSubKatalog:GuruFirstrow().
      THIS-OBJECT:ValueGridSubKatalogCh(?,?).
      THIS-OBJECT:GridVisning:GuruReopen().
      THIS-OBJECT:GridAnvSek:GuruReopen().
      
      THIS-OBJECT:GridMallar:GuruReopen().
      THIS-OBJECT:GridMallar:GuruFirstrow().
      THIS-OBJECT:ValueGridMallarCh(?,?). 
      THIS-OBJECT:ValueGridSubMallCh(?,?).
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MALLNR"," Mallnr ",TRUE,30).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("TYP","Typ",FALSE,30).
      GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkmallHuvudtth)).
      THIS-OBJECT:GridMallar:TEXT = "Mallar".
      GridMallar:tBS:PositionChanged:Subscribe(ValueGridMallarCh).
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridFrekvens():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("FREKOD"," Arbkod ",TRUE,55).
      rubrikergrid[2] = NEW Controls.GridRubrik("FREKNR","Löpnr",TRUE,45).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,600).
      GridFrekvens:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:FrekvensTTh)).
      THIS-OBJECT:GridFrekvens:TEXT = "Frekvenskoder".  
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKatalog():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[2] = NEW Controls.GridRubrik("KATALOGTYP","Kat.typ",FALSE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("VISARTAL","Årtal",FALSE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("SEKRETESS","Sekretess",FALSE,63,"LOG").
      rubrikergrid[5] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,60).
      rubrikergrid[6] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",FALSE,60,"LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("HKLOGSUBID","Styrande delkatalog",FALSE,60).
      rubrikergrid[8] = NEW Controls.GridRubrik("KLOGID","NUM",TRUE,60).
      GridKatSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:KatalogTTh)).  
      GridKatSek:TEXT = "Välj katalog för sekretess".      
      GridKatSek:tBS:PositionChanged:Subscribe(ValueGridKatSekCh).   
      GridKatalog:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:KatalogTTh)).  
      GridKatalog:TEXT = "Kataloger - markera katalog du vill koppla till delkatalog".
      GridKatalog:tBS:PositionChanged:Subscribe(ValueGridKatalogCh). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridSubKatalog():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,60).
      rubrikergrid[4] = NEW Controls.GridRubrik("AVSLUTAD","Avslutad",FALSE,60,"LOG").
      /*sub kat som ingår i katalog*/ 
      GridSubKatalog:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KalkAdmdbControl:Katalogsubtth)).  
      GridSubKatalog:TEXT = "Delkataloger".      
      GridSubKatalog:tBS:PositionChanged:Subscribe(ValueGridSubKatalogCh).
      /* subkat vid arbetskoder*/
      GridSubKatKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KatalogDeltth)).  
      GridSubKatKoder:TEXT = "Delkataloger". 
      GridSubKatKoder:tBS:PositionChanged:Subscribe(ValueGridSubKatKoderCh).
      /*ny kop bort*/
      GridSubKatDel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KatalogDeltth)).  
      GridSubKatDel:TEXT = "Delkataloger".
      
      GridKatHuvDel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:KalkAdmdbControl:Katalogsubtth)).  
      GridKatHuvDel:TEXT = "Vald katalog består av följande delkataloger".
      
      rubrikergrid[1] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("INDATUM","Skapad",TRUE,60).
      
      GridSubMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KatalogDeltth)).  
      GridSubMall:TEXT = "Delkataloger".
      GridSubMall:tBS:PositionChanged:Subscribe(ValueGridSubMallCh).
   END METHOD.
   METHOD PUBLIC VOID CreateGridKatPris():
      {GridRubrikListaVarExtent.i}  
      rubrikergrid[1] = NEW Controls.GridRubrik("KPID","PrisId",TRUE,60).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("PRIS","Pris",FALSE,60,"DEC").
      rubrikergrid[4] = NEW Controls.GridRubrik("SOKBENAMNING","Sökbenämning",TRUE,160).
      rubrikergrid[5] = NEW Controls.GridRubrik("EGENKODUPP","Egna koder",FALSE,90).
      rubrikergrid[6] = NEW Controls.GridRubrik("EGENPRISUPP","Egna priser",FALSE,90).
      rubrikergrid[7] = NEW Controls.GridRubrik("KVID","Visningsregel",FALSE,80).
      rubrikergrid[8] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      GridKatPris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:PriserTTh)).  
      GridKatPris:TEXT = "Katalogpriser".  
      THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["KPID"]:Hidden = false.
      THIS-OBJECT:GridKatPris:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = false.
   END METHOD.
   METHOD PUBLIC VOID CreateGridVisning():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("KVID","Visningsregel",TRUE,60).
      rubrikergrid[2] = NEW Controls.GridRubrik("EABER","EA-mängd",FALSE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("TIMTYP","Timmar",FALSE,80).
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTTYP","Kostnad",TRUE,80).
      rubrikergrid[5] = NEW Controls.GridRubrik("ORDNINGTIMMAR","Ord.timmar",FALSE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("ORDNINGKOSTNAD","Ord.kostnad",FALSE,70).
      GridVisning:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:VisningTTh)).  
      GridVisning:TEXT = "Visningsregler".      
        
   END METHOD.
   METHOD PUBLIC VOID CreateGridArbKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",FALSE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("TYPKALK","Typ",FALSE,50).
     /*
      rubrikergrid[5] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      */
      THIS-OBJECT:GridArbKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      GridArbKoder:TEXT = "Arbetskoder".
      GridArbKoder:tBS:PositionChanged:Subscribe(ValueGridArbKoderCh).  
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,230).
      THIS-OBJECT:GridArbKodMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      GridArbKodMall:TEXT = "Arbetskoder".
      GridArbKodMall:tBS:PositionChanged:Subscribe(ValueGridArbKodMallCh).
                 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridLopnr():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",FALSE,35, "KALKLOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,325).
      rubrikergrid[3] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      THIS-OBJECT:GridLopnr:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:LopposterTTh)).
      THIS-OBJECT:GridLopnr:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridLopnr:TEXT = "Löpnummer".
      THIS-OBJECT:GridLopnr:GuruOrderby("BY ARBKOD BY LOPNR").
      GridLopnr:tBS:PositionChanged:Subscribe(ValueGridLopnrCh).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35, "KALKLOP").
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,325).
      rubrikergrid[3] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      
      THIS-OBJECT:GridLopMall:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:LopposterTTh)).
      THIS-OBJECT:GridLopMall:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridLopMall:TEXT = "Löpnummer".
      THIS-OBJECT:GridLopMall:GuruOrderby("BY ARBKOD BY LOPNR").
         
   END METHOD.   
   METHOD PUBLIC VOID CreateGridValdMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("MALLNR","Mallar",TRUE,45).
      THIS-OBJECT:GridValdMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:GridValdMallar:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridValdMallar:Text = "Kalkylkoder för mallen".      
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridLopnumsub():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 125).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris", TRUE, 100,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("TIMMAR", "Timmar", FALSE, 50,"DEC").
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE, 75,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("POSTKOST", "Tot.Kostnad", TRUE, 90,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("KPID","PrisId",TRUE,60).
      rubrikergrid[7] = NEW Controls.GridRubrik("KLOGSUBID","Delkatalog",TRUE,50).
      GridLopnumsub:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:LopsubTTh)).  
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Bands[0]:Columns["KPID"]:Hidden = TRUE.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Bands[0]:Columns["KLOGSUBID"]:Hidden = TRUE.
      
      GridLopnumsub:TEXT = "Pris och timmar per löpnummer Totalkostnad ". 
      THIS-OBJECT:GridLopnumsub:SetTitle(GridLopnumsub:TEXT).
      THIS-OBJECT:GridLopnumsub:GuruNoAutoTitle(TRUE).
                   
   END METHOD.
   METHOD PUBLIC VOID GridLopnumSubTitle():
      THIS-OBJECT:GridLopnumsub:TEXT = THIS-OBJECT:GridLopnumsub:TITLE + " " + STRING(ROUND(THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot,0)).
      
   END METHOD.   
   METHOD PUBLIC VOID CreateGridAnvSek():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ANVANDARE", "Användare", TRUE, 100).
      rubrikergrid[2] = NEW Controls.GridRubrik("AV-NAMN", "Namn", TRUE, 200).
      rubrikergrid[3] = NEW Controls.GridRubrik("AV-LEVEL", "Anv.Niv", TRUE, 50).
      GridAnvSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT Guru.Konstanter:AnvandareTTh)).  
      GridAnvSek:GuruFiltrera("anvandartemp.AV-LEVEL NE " + STRING(0)).
      
      GridAnvSek:TEXT = "Användare".
      GridValdaAnvSek:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:kalkanvtth)).  
      GridValdaAnvSek:TEXT = "Valda användare".     
               
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
	METHOD PRIVATE VOID TabKalkylAdm_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
		DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
		cTo = e:TAB:KEY.	
		IF cTo = "Koder" THEN DO:
		   THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
		  
		   THIS-OBJECT:ValueGridSubKatKoderCh(?,?).
		            
		END.
		IF cTo = "Kataloger" THEN DO:
		   THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE)) NO-ERROR.
      END.
      IF cTo = "Delkataloger" THEN DO:
         THIS-OBJECT:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.         
      END.
      IF cTo = "Mallar" THEN DO:
         THIS-OBJECT:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(THIS-OBJECT:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE)) NO-ERROR.  
         THIS-OBJECT:ValueGridSubMallCh(?,?).    
      END.
      IF cTo = "Sekretess" THEN DO:
         THIS-OBJECT:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE)) NO-ERROR.
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabKalkylAdm_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.
      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabKalkylAdm:SelectedTab:KEY NO-ERROR.
      
      
     /*BEHÖVS EJ
      IF cTo = "Delkataloger" THEN THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Visible = TRUE. 
      ELSE THIS-OBJECT:RibbonToolKopplaDelKatalog:SharedPropsInternal:Visible = FALSE.
      */
        
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ValdKodKommentar_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE = THIS-OBJECT:ValdKodKommentar:Text. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
      END. 
		RETURN.

	END METHOD.
                     
   METHOD PRIVATE VOID ValueGridSubKatalogCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:AVAILABLE THEN DO:
         THIS-OBJECT:GridKatPris:GuruTom().
         THIS-OBJECT:GridKatPris:GuruFiltrera("kalkylprisertt.KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)).
         THIS-OBJECT:GridKatPris:GuruFirstrow(). 
                
      END.
      ELSE DO:
         THIS-OBJECT:GridKatPris:GuruTom().
         THIS-OBJECT:GridArbKoder:GuruTom().
      END.     
   END METHOD.
   
   /*ROB*/
   METHOD PRIVATE VOID ValueGridKatalogCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      IF THIS-OBJECT:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridKatHuvDel:GuruTom().
         THIS-OBJECT:GridKatHuvDel:GuruFiltrera("kalkylkatalogsubtt.KLOGID = " + THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE).
         THIS-OBJECT:GridKatHuvDel:GuruFirstrow().      
      END. 
      ELSE DO:
         THIS-OBJECT:GridKatHuvDel:GuruTom().
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridSubKatKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KatalogDeltth:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKoder(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
         THIS-OBJECT:RefreshArbKoder(?,?).
         THIS-OBJECT:ValueGridArbKoderCh(?,?).               
      END.
      ELSE DO:
         THIS-OBJECT:GridArbKoder:GuruTom().
         THIS-OBJECT:GridLopnr:GuruTom().
         THIS-OBJECT:GridLopnumsub:GuruTom().
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridArbKodMallCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
         THIS-OBJECT:GridLopMall:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
         THIS-OBJECT:GridLopMall:GuruFirstrow().
                           
      END.
      ELSE DO:
         
         THIS-OBJECT:GridLopMall:GuruTom().         
      END.    
   END METHOD.
   METHOD PUBLIC VOID ValueGridArbKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
         THIS-OBJECT:GridLopnr:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
         THIS-OBJECT:GridLopnr:GuruFirstrow(). 
         THIS-OBJECT:ValueGridLopnrCh(?,?).           
      END.
      ELSE DO:
         THIS-OBJECT:GridLopnr:GuruTom().
         THIS-OBJECT:GridLopnumsub:GuruTom().
      END.    
   END METHOD.
   METHOD PRIVATE VOID ValueGridLopnrCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      
      IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridLopnumsub:GuruFiltrera("kalkyllopsubtt.KLOGSUBID = " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
         "kalkyllopsubtt.ARBKOD = '" + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "' AND kalkyllopsubtt.LOPNR = " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)).
         THIS-OBJECT:GridLopnumsub:GuruFirstrow().
         IF THIS-OBJECT:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:ValdKodKommentar:Text = THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchFrekvens(INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
            INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
            THIS-OBJECT:GridFrekvens:GuruReopen().
         END. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
         INPUT THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT FALSE).
         THIS-OBJECT:GridLopnumSubTitle().    
      END.
      ELSE DO:
         THIS-OBJECT:GridLopnumsub:GuruTom().
         THIS-OBJECT:GridFrekvens:GuruTom().
         THIS-OBJECT:ValdKodKommentar:Text = "".
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      IF THIS-OBJECT:KalkmallHuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:GridValdMallar:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:GridValdMallar:GuruFirstrow().
      END.
      ELSE DO:
         THIS-OBJECT:GridValdMallar:GuruTom().            
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridSubMallCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KatalogDeltth:AVAILABLE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKoder(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
         THIS-OBJECT:GridArbKodMall:GuruFiltrera("kalkylarbkodertt.KLOGSUBID = " + STRING(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)).
         THIS-OBJECT:GridArbKodMall:GuruFirstrow().
         THIS-OBJECT:ValueGridArbKodMallCh(?,?).
         IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchLopar(THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE).
            THIS-OBJECT:GridLopMall:GuruFiltrera("kalkylloppostertt.KLOGSUBID = " + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE) + " AND " +
            "kalkylloppostertt.ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").              
            THIS-OBJECT:GridLopMall:GuruFirstrow(). 
         END. 
         ELSE THIS-OBJECT:GridLopMall:GuruTom(). 
      END.
      ELSE DO:
         THIS-OBJECT:GridArbKodMall:GuruTom().
         THIS-OBJECT:GridLopMall:GuruTom().      
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID ValueGridKatSekCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      
      IF THIS-OBJECT:KatalogTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridValdaAnvSek:GuruFiltrera("kalkanvtt.KLOGID = " + STRING(THIS-OBJECT:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE)).
         THIS-OBJECT:GridValdaAnvSek:GuruFirstrow().        
      END.
      ELSE DO:
         THIS-OBJECT:GridValdaAnvSek:GuruTom().                 
      END.        
   END METHOD.
   
   METHOD PUBLIC VOID LasGriddar():
      MESSAGE "GRIDDAR LÅSTA"
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
   END METHOD.
   
	DESTRUCTOR PUBLIC KalkylAdm ( ):
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.