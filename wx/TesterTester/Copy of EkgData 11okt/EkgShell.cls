 
 /*------------------------------------------------------------------------
    File        : EkgShell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jul 01 11:11:11 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgShell INHERITS UserControl: 
	
	DEFINE PUBLIC VARIABLE EkgExport AS Modules.EkgData.EkgExport NO-UNDO.
   DEFINE PUBLIC VARIABLE P1Control AS Modules.EkgData.EkgP1Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P2Control AS Modules.EkgData.EkgP2Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P3Control AS Modules.EkgData.EkgP3Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P4Control AS Modules.EkgData.EkgP4Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P5Control AS Modules.EkgData.EkgP5Control NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyNiva AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyPaslag AS Controls.Button NO-UNDO.
   
   DEFINE PRIVATE VARIABLE buttonNyPaslag AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyResurspris AS System.Windows.Forms.ToolStripButton NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE buttonCancelNyP1arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP2 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP1 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP3arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP2arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP4 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP3 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyResurs AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP5 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP1arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP2 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP1 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP3arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP2arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP4 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP3 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP5 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyResurs AS Controls.Button NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverResurspaslagBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverResurspaslag AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverResurspris AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonNyNiva AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonTaBortResurs AS Controls.Button NO-UNDO.
   DEFINE PUBLIC VARIABLE comboNiva AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC  VARIABLE comboP1ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE comboP2ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC  VARIABLE comboP3ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   
   DEFINE PUBLIC VARIABLE fillinArtNr AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP2arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP1arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP2LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP1LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP3arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP3LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP4arbkod AS Controls.FillinChar NO-UNDO.   
   DEFINE PUBLIC VARIABLE fillinPaslagBenamning AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinResursBenamning AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinResursNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP5arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE gridKataloger AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridMateriel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridRegel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForResurspris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursniva AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurspris AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogerValjButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogKatalogSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE KopieraKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillNivaButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE miniToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlImportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlExportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE mtrlMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyMtrlPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyMtrlPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyNivaPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyNivaPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP1arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP1arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP1Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP2arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP2arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP2Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP1Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP3arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP3arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP3Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP2Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP4Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP3Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP4Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP5Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyP5Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyPaslagPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyPaslagPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE nyResursPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyResursPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE paslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE paslagToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE regelMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE regeltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE resurserMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resurserMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ResursertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE resurspaslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resursPaslagtoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE resurspristoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
	/*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlWinLg                 AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlDataDb                AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   /*Drag Dropp*/
   
   DEFINE PUBLIC VARIABLE splitContainerBasresurser AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNiva AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResursPrisLista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResursPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPaslagOver AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPaslag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerTotalExport AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE TabDelad AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE TabExport AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerEkg AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManagerResurspriser AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMtrl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortNiva AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortResursPrisButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TabPopups AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResurser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResursPriser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPPaslag AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPNiva AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPBasresurser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPRegel AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripBasresurser AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripNiva AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPagePaslagNiv AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE uppdateraMtrlKostButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   
	CONSTRUCTOR PUBLIC EkgShell ( INPUT r AS Guru.Root ):	
		
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      
           
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   
   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell = THIS-OBJECT. 
      THIS-OBJECT:ControlWinLg = NEW Modules.EkgData.EkgWinLg(THIS-OBJECT:Root).
      
      THIS-OBJECT:ControlWinLg:Initialize().
      
      RETURN TRUE.
   END METHOD.
   
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyMtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinArtNr:GuruText:Text = "".
      THIS-OBJECT:nyMtrlPop:Close().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyNivaPop:Close().
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyPaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinPaslagBenamning:GuruText:Text = "".
      THIS-OBJECT:nyPaslagPop:Close().
      RETURN.

   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP3arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP3arbkodPop:Close().
      RETURN.

   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP1arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP1arbkodPop:Close().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP1LopNr:GuruText:Text = "".
      THIS-OBJECT:nyP1Pop:Close().
      RETURN.

   END METHOD.
   
     /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP2arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP2arbkodPop:Close().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP2LopNr:GuruText:Text = "".
      THIS-OBJECT:nyP2Pop:Close().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyP3_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinP3LopNr:GuruText:Text = "".
		THIS-OBJECT:nyP3Pop:Close().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP4_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP4arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP4Pop:Close().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyP5_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinP5arbkod:GuruText:Text = "".
		THIS-OBJECT:nyP5Pop:Close().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyResurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinResursBenamning:GuruText:Text = "".
	   THIS-OBJECT:fillinResursNr:GuruText:Text = "".
		THIS-OBJECT:nyResursPop:Close().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyMtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyMtrl().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:Nyniva().
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
   Purpose:
   Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyPaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyPaslag().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP1().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP1ArbKod().
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP2().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP2ArbKod().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyP3_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyP3().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP3ArbKod().
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP4_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP4().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyP5_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyP5().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyResurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyResurs().
		RETURN.

	END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResurspaslagBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverResurspaslagBort(). 
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverResurspaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:buttonOverResurspaslag().
		RETURN.
	END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridKataloger_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde). 
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridKataloger_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
		RETURN.
	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "LASTA" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE". 
         THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde).
      END. 
      IF kolnamn = "ANVANDS" THEN DO:
         IF beforekolvarde = "TRUE" THEN DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOff(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
         ELSE DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOn(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
      END. 
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.           
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridregel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:RegelUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridpaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:PaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN. 
         
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresursniva_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursNivaUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.         
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursPaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursprisUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridmateriel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:MaterielUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
	 
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).
      RETURN.

   END METHOD.
 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		{AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslagBort_Click(?,?).
      END.
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridResursPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		{AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslag_Click(?,?).
      END.
		RETURN.

	END METHOD.
     
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KatalogerValjButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:gridKataloger:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).   
      RETURN.

   END METHOD.

/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KopieraKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:ControlWinLg:KopieraKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      RETURN.
 

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillNivaButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   
		THIS-OBJECT:nyNivaPop:Show().		

		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyPaslagPop:Show().
      THIS-OBJECT:fillinPaslagBenamning:GuruText:Select().
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillMtrlButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyMtrlPop:Show().
		THIS-OBJECT:fillinArtNr:GuruText:Select().
		RETURN.

	END METHOD.



	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyResursPop:Show().		
		THIS-OBJECT:fillinResursNr:GuruText:Select().
		/*THIS-OBJECT:fillinResursBenamning:GuruText:Select().*/
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID StartEkgShell():
      
   END METHOD.   
   
	METHOD PRIVATE VOID InitializeComponent(  ):
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgShell").
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
      DEFINE VARIABLE ultraTab21 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab21 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab23 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab23 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab18 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab18 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
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
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
      appearance112 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
      appearance113 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance114 AS Infragistics.Win.Appearance NO-UNDO.
      appearance114 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance115 AS Infragistics.Win.Appearance NO-UNDO.
      appearance115 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance116 AS Infragistics.Win.Appearance NO-UNDO.
      appearance116 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance117 AS Infragistics.Win.Appearance NO-UNDO.
      appearance117 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance118 AS Infragistics.Win.Appearance NO-UNDO.
      appearance118 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance119 AS Infragistics.Win.Appearance NO-UNDO.
      appearance119 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance120 AS Infragistics.Win.Appearance NO-UNDO.
      appearance120 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance121 AS Infragistics.Win.Appearance NO-UNDO.
      appearance121 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance122 AS Infragistics.Win.Appearance NO-UNDO.
      appearance122 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance123 AS Infragistics.Win.Appearance NO-UNDO.
      appearance123 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance124 AS Infragistics.Win.Appearance NO-UNDO.
      appearance124 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance125 AS Infragistics.Win.Appearance NO-UNDO.
      appearance125 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance126 AS Infragistics.Win.Appearance NO-UNDO.
      appearance126 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance127 AS Infragistics.Win.Appearance NO-UNDO.
      appearance127 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab25 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab25 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      THIS-OBJECT:TabRPPaslag = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerPaslag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPaslagOver = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagToolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridPaslag = NEW Controls.Grid().
      THIS-OBJECT:buttonOverResurspaslagBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverResurspaslag = NEW Controls.ButtonOver().
      THIS-OBJECT:resurspaslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resursPaslagtoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridResursPaslag = NEW Controls.Grid().
      THIS-OBJECT:TabRPNiva = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerNiva = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripNiva = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillNivaButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortNiva = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResursniva = NEW Controls.Grid().
      THIS-OBJECT:TabRPRegel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:regelMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:regeltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridRegel = NEW Controls.Grid().
      THIS-OBJECT:TabRPBasresurser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerBasresurser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripBasresurser = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:buttonNyResurspris = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResursForResurspris = NEW Controls.Grid().
      THIS-OBJECT:TabResurser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:resurserMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurserMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ResursertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurs = NEW Controls.Grid().
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:KatalogMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogKatalogSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:KatalogerValjButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:KopieraKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridKataloger = NEW Controls.Grid().
      THIS-OBJECT:TabResursPriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerResursPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerResursPrisLista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurspristoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:taBortResursPrisButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurspris = NEW Controls.Grid().
      THIS-OBJECT:TabManagerResurspriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabMtrl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:mtrlMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrlMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:uppdateraMtrlKostButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlImportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlExportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridMateriel = NEW Controls.Grid().
      THIS-OBJECT:TabP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabExport = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerTotalExport = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabPopups = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:buttonOverResurspris = NEW Controls.ButtonOver().
      THIS-OBJECT:nyPaslagPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyPaslag = NEW Controls.Button().
      THIS-OBJECT:fillinPaslagBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyPaslag = NEW Controls.Button().
      THIS-OBJECT:nyMtrlPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyMtrl = NEW Controls.Button().
      THIS-OBJECT:fillinArtNr = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyMtrl = NEW Controls.Button().
      THIS-OBJECT:nyP1arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP1arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP1arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP1arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP2arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP2arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP2arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP2arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP3arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP3arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP3arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP3arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP1Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP1ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP1LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP1 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP1 = NEW Controls.Button().
      THIS-OBJECT:nyP2Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP2ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP2LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP2 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP2 = NEW Controls.Button().
      THIS-OBJECT:nyP3Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP3ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP3LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP3 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP3 = NEW Controls.Button().
      THIS-OBJECT:nyP4Panel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP4 = NEW Controls.Button().
      THIS-OBJECT:fillinP4arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP4 = NEW Controls.Button().
      THIS-OBJECT:nyP5Panel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP5 = NEW Controls.Button().
      THIS-OBJECT:fillinP5arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP5 = NEW Controls.Button().
      THIS-OBJECT:nyResursPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyResurs = NEW Controls.Button().
      THIS-OBJECT:fillinResursNr = NEW Controls.FillinInteger().
      THIS-OBJECT:fillinResursBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyResurs = NEW Controls.Button().
      THIS-OBJECT:nyNivaPanel = NEW Controls.PopPanel().
      THIS-OBJECT:comboNiva = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:buttonCancelNyNiva = NEW Controls.Button().
      THIS-OBJECT:buttonNyNiva = NEW Controls.Button().
      THIS-OBJECT:laggTillPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:miniToolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:TabDelad = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabManagerEkg = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:nyResursPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP5Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP4Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP3Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP2Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP1Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP3arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP2arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP1arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyPaslagPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyMtrlPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyNivaPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:TabRPPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslagOver:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:SuspendLayout().
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:paslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPNiva:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNiva:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:SuspendLayout().
      THIS-OBJECT:toolStripNiva:SuspendLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPRegel:SuspendLayout().
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:regelMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:regelMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:regelMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPBasresurser:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBasresurser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:SuspendLayout().
      THIS-OBJECT:toolStripBasresurser:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabResurser:SuspendLayout().
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMainSC:SuspendLayout().
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:SuspendLayout().
      THIS-OBJECT:ResursertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKataloger:SuspendLayout().
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogMainSC:SuspendLayout().
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogKatalogSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:SuspendLayout().
      THIS-OBJECT:KatalogertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabResursPriser:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:SuspendLayout().
      THIS-OBJECT:resurspristoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerResurspriser:SuspendLayout().
      THIS-OBJECT:TabMtrl:SuspendLayout().
      CAST(THIS-OBJECT:mtrlMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMainSC:SuspendLayout().
      CAST(THIS-OBJECT:mtrlMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:SuspendLayout().
      THIS-OBJECT:mtrltoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabExport:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerTotalExport, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerTotalExport:SuspendLayout().
      THIS-OBJECT:TabPopups:SuspendLayout().
      THIS-OBJECT:nyPaslagPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyPaslagPanel:SuspendLayout().
      THIS-OBJECT:nyMtrlPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyMtrlPanel:SuspendLayout().
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP1arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP2arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP3arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP1Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP1Panel:SuspendLayout().
      THIS-OBJECT:nyP2Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP2Panel:SuspendLayout().
      THIS-OBJECT:nyP3Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP3Panel:SuspendLayout().
      THIS-OBJECT:nyP4Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP4Panel:SuspendLayout().
      THIS-OBJECT:nyP5Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP5Panel:SuspendLayout().
      THIS-OBJECT:nyResursPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyResursPanel:SuspendLayout().
      THIS-OBJECT:nyNivaPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyNivaPanel:SuspendLayout().
      CAST(THIS-OBJECT:TabManagerEkg, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerEkg:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabRPPaslag */
      /*  */
      THIS-OBJECT:TabRPPaslag:Controls:Add(THIS-OBJECT:splitContainerPaslag).
      THIS-OBJECT:TabRPPaslag:Location = NEW System.Drawing.Point(1, 23).
      THIS-OBJECT:TabRPPaslag:Name = "TabRPPaslag".
      THIS-OBJECT:TabRPPaslag:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerPaslag */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslag:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslag:Name = "splitContainerPaslag".
      /*  */
      /* splitContainerPaslag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel1:Controls:Add(THIS-OBJECT:splitContainerPaslagOver).
      /*  */
      /* splitContainerPaslag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel2:Controls:Add(THIS-OBJECT:resurspaslagMenuSC).
      THIS-OBJECT:splitContainerPaslag:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerPaslag:SplitterDistance = 362.
      THIS-OBJECT:splitContainerPaslag:TabIndex = 0.
      /*  */
      /* splitContainerPaslagOver */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslagOver:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslagOver:Name = "splitContainerPaslagOver".
      /*  */
      /* splitContainerPaslagOver.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel1:Controls:Add(THIS-OBJECT:paslagMenuSC).
      /*  */
      /* splitContainerPaslagOver.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslagBort).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslag).
      THIS-OBJECT:splitContainerPaslagOver:Size = NEW System.Drawing.Size(362, 527).
      THIS-OBJECT:splitContainerPaslagOver:SplitterDistance = 328.
      THIS-OBJECT:splitContainerPaslagOver:TabIndex = 0.
      /*  */
      /* paslagMenuSC */
      /*  */
      THIS-OBJECT:paslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:paslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:paslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagMenuSC:Name = "paslagMenuSC".
      THIS-OBJECT:paslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* paslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:paslagToolStrip).
      /*  */
      /* paslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridPaslag).
      THIS-OBJECT:paslagMenuSC:Size = NEW System.Drawing.Size(328, 527).
      THIS-OBJECT:paslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:paslagMenuSC:TabIndex = 1.
      /*  */
      /* paslagToolStrip */
      /*  */
      THIS-OBJECT:paslagToolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagToolStrip:Name = "paslagToolStrip".
      THIS-OBJECT:paslagToolStrip:Size = NEW System.Drawing.Size(328, 25).
      THIS-OBJECT:paslagToolStrip:TabIndex = 1.
      THIS-OBJECT:paslagToolStrip:Text = "toolStrip1".
      /*  */
      /* gridPaslag */
      /*  */
      THIS-OBJECT:gridPaslag:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridPaslag:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridPaslag:GuruContext = ?.
      THIS-OBJECT:gridPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridPaslag:Name = "gridPaslag".
      THIS-OBJECT:gridPaslag:Size = NEW System.Drawing.Size(328, 498).
      THIS-OBJECT:gridPaslag:TabIndex = 0.
      THIS-OBJECT:gridPaslag:Text = "grid1".
      THIS-OBJECT:gridPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridPaslag_GuruAfterDropped).
      THIS-OBJECT:gridPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridPaslag_AfterCellUpdate).
      /*  */
      /* buttonOverResurspaslagBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslagBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverResurspaslagBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverResurspaslagBort:Name = "buttonOverResurspaslagBort".
      THIS-OBJECT:buttonOverResurspaslagBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslagBort:TabIndex = 7.
      THIS-OBJECT:buttonOverResurspaslagBort:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslagBort_Click).
      /*  */
      /* buttonOverResurspaslag */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslag:Appearance = appearance14.
      THIS-OBJECT:buttonOverResurspaslag:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverResurspaslag:Name = "buttonOverResurspaslag".
      THIS-OBJECT:buttonOverResurspaslag:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslag:TabIndex = 6.
      THIS-OBJECT:buttonOverResurspaslag:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslag_Click).
      /*  */
      /* resurspaslagMenuSC */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurspaslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurspaslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspaslagMenuSC:Name = "resurspaslagMenuSC".
      THIS-OBJECT:resurspaslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurspaslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:resursPaslagtoolStrip).
      /*  */
      /* resurspaslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResursPaslag).
      THIS-OBJECT:resurspaslagMenuSC:Size = NEW System.Drawing.Size(296, 527).
      THIS-OBJECT:resurspaslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurspaslagMenuSC:TabIndex = 2.
      /*  */
      /* resursPaslagtoolStrip */
      /*  */
      THIS-OBJECT:resursPaslagtoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resursPaslagtoolStrip:Name = "resursPaslagtoolStrip".
      THIS-OBJECT:resursPaslagtoolStrip:Size = NEW System.Drawing.Size(296, 25).
      THIS-OBJECT:resursPaslagtoolStrip:TabIndex = 1.
      THIS-OBJECT:resursPaslagtoolStrip:Text = "toolStrip1".
      /*  */
      /* gridResursPaslag */
      /*  */
      THIS-OBJECT:gridResursPaslag:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursPaslag:GuruContext = ?.
      THIS-OBJECT:gridResursPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursPaslag:Name = "gridResursPaslag".
      THIS-OBJECT:gridResursPaslag:Size = NEW System.Drawing.Size(296, 498).
      THIS-OBJECT:gridResursPaslag:TabIndex = 0.
      THIS-OBJECT:gridResursPaslag:Text = "grid1".
      THIS-OBJECT:gridResursPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursPaslag_GuruAfterDropped).
      THIS-OBJECT:gridResursPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurspaslag_AfterCellUpdate).
      /*  */
      /* TabRPNiva */
      /*  */
      THIS-OBJECT:TabRPNiva:Controls:Add(THIS-OBJECT:splitContainerNiva).
      THIS-OBJECT:TabRPNiva:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPNiva:Name = "TabRPNiva".
      THIS-OBJECT:TabRPNiva:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerNiva */
      /*  */
      THIS-OBJECT:splitContainerNiva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNiva:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNiva:Name = "splitContainerNiva".
      THIS-OBJECT:splitContainerNiva:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNiva.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel1:Controls:Add(THIS-OBJECT:toolStripNiva).
      /*  */
      /* splitContainerNiva.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel2:Controls:Add(THIS-OBJECT:gridResursniva).
      THIS-OBJECT:splitContainerNiva:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerNiva:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNiva:TabIndex = 0.
      /*  */
      /* toolStripNiva */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillNivaButton.
      arrayvar0[2] = THIS-OBJECT:taBortNiva.
      THIS-OBJECT:toolStripNiva:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripNiva:Name = "toolStripNiva".
      THIS-OBJECT:toolStripNiva:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:toolStripNiva:TabIndex = 0.
      THIS-OBJECT:toolStripNiva:Text = "toolStrip1".
      /*  */
      /* laggTillNivaButton */
      /*  */
      THIS-OBJECT:laggTillNivaButton:Image = CAST(resources:GetObject("laggTillNivaButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillNivaButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillNivaButton:Name = "laggTillNivaButton".
      THIS-OBJECT:laggTillNivaButton:Size = NEW System.Drawing.Size(110, 22).
      THIS-OBJECT:laggTillNivaButton:Text = "Lägg till ny nivå".
      THIS-OBJECT:laggTillNivaButton:ToolTipText = "Lägg till resursen på ny nivå".
      THIS-OBJECT:laggTillNivaButton:Click:Subscribe(THIS-OBJECT:laggTillNivaButton_Click).
      /*  */
      /* taBortNiva */
      /*  */
      THIS-OBJECT:taBortNiva:Image = CAST(resources:GetObject("taBortNiva.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortNiva:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortNiva:Name = "taBortNiva".
      THIS-OBJECT:taBortNiva:Size = NEW System.Drawing.Size(90, 22).
      THIS-OBJECT:taBortNiva:Text = "Ta bort nivå".
      THIS-OBJECT:taBortNiva:ToolTipText = "Ta bort resursen för nivå".
      THIS-OBJECT:taBortNiva:Click:Subscribe(THIS-OBJECT:taBortNiva_Click).
      /*  */
      /* gridResursniva */
      /*  */
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursniva:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridResursniva:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursniva:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursniva:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursniva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursniva:GuruContext = ?.
      THIS-OBJECT:gridResursniva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursniva:Name = "gridResursniva".
      THIS-OBJECT:gridResursniva:Size = NEW System.Drawing.Size(662, 498).
      THIS-OBJECT:gridResursniva:TabIndex = 2.
      THIS-OBJECT:gridResursniva:Text = "grid1".
      THIS-OBJECT:gridResursniva:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResursniva_AfterCellUpdate).
      /*  */
      /* TabRPRegel */
      /*  */
      THIS-OBJECT:TabRPRegel:Controls:Add(THIS-OBJECT:regelMenuSC).
      THIS-OBJECT:TabRPRegel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPRegel:Name = "TabRPRegel".
      THIS-OBJECT:TabRPRegel:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* regelMenuSC */
      /*  */
      THIS-OBJECT:regelMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:regelMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:regelMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regelMenuSC:Name = "regelMenuSC".
      THIS-OBJECT:regelMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* regelMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel1:Controls:Add(THIS-OBJECT:regeltoolStrip).
      /*  */
      /* regelMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridRegel).
      THIS-OBJECT:regelMenuSC:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:regelMenuSC:SplitterDistance = 25.
      THIS-OBJECT:regelMenuSC:TabIndex = 1.
      /*  */
      /* regeltoolStrip */
      /*  */
      THIS-OBJECT:regeltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regeltoolStrip:Name = "regeltoolStrip".
      THIS-OBJECT:regeltoolStrip:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:regeltoolStrip:TabIndex = 0.
      THIS-OBJECT:regeltoolStrip:Text = "toolStrip2".
      /*  */
      /* gridRegel */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridRegel:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:gridRegel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridRegel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridRegel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridRegel:GuruContext = ?.
      THIS-OBJECT:gridRegel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridRegel:Name = "gridRegel".
      THIS-OBJECT:gridRegel:Size = NEW System.Drawing.Size(787, 498).
      THIS-OBJECT:gridRegel:TabIndex = 3.
      THIS-OBJECT:gridRegel:Text = "grid2".
      THIS-OBJECT:gridRegel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridRegel_AfterCellUpdate).
      /*  */
      /* TabRPBasresurser */
      /*  */
      THIS-OBJECT:TabRPBasresurser:Controls:Add(THIS-OBJECT:splitContainerBasresurser).
      THIS-OBJECT:TabRPBasresurser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPBasresurser:Name = "TabRPBasresurser".
      THIS-OBJECT:TabRPBasresurser:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerBasresurser */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBasresurser:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBasresurser:Name = "splitContainerBasresurser".
      THIS-OBJECT:splitContainerBasresurser:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerBasresurser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel1:Controls:Add(THIS-OBJECT:toolStripBasresurser).
      /*  */
      /* splitContainerBasresurser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel2:Controls:Add(THIS-OBJECT:gridResursForResurspris).
      THIS-OBJECT:splitContainerBasresurser:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerBasresurser:SplitterDistance = 25.
      THIS-OBJECT:splitContainerBasresurser:TabIndex = 0.
      /*  */
      /* toolStripBasresurser */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:buttonNyResurspris.
      THIS-OBJECT:toolStripBasresurser:Items:AddRange(arrayvar1).
      THIS-OBJECT:toolStripBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripBasresurser:Name = "toolStripBasresurser".
      THIS-OBJECT:toolStripBasresurser:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:toolStripBasresurser:TabIndex = 0.
      THIS-OBJECT:toolStripBasresurser:Text = "toolStrip2".
      /*  */
      /* buttonNyResurspris */
      /*  */
      THIS-OBJECT:buttonNyResurspris:Image = CAST(resources:GetObject("buttonNyResurspris.Image"), System.Drawing.Image).
      THIS-OBJECT:buttonNyResurspris:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:buttonNyResurspris:Name = "buttonNyResurspris".
      THIS-OBJECT:buttonNyResurspris:Size = NEW System.Drawing.Size(151, 22).
      THIS-OBJECT:buttonNyResurspris:Text = "Lägg till resurs i katalog".
      THIS-OBJECT:buttonNyResurspris:ToolTipText = "Lägg till den markerade resursern i katalogen".
      THIS-OBJECT:buttonNyResurspris:Click:Subscribe(THIS-OBJECT:buttonNyResurspris_Click).
      /*  */
      /* gridResursForResurspris */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForResurspris:GuruContext = ?.
      THIS-OBJECT:gridResursForResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForResurspris:Name = "gridResursForResurspris".
      THIS-OBJECT:gridResursForResurspris:Size = NEW System.Drawing.Size(662, 498).
      THIS-OBJECT:gridResursForResurspris:TabIndex = 1.
      THIS-OBJECT:gridResursForResurspris:Text = "grid1".
      /*  */
      /* TabResurser */
      /*  */
      THIS-OBJECT:TabResurser:Controls:Add(THIS-OBJECT:resurserMainSC).
      THIS-OBJECT:TabResurser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResurser:Name = "TabResurser".
      THIS-OBJECT:TabResurser:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* resurserMainSC */
      /*  */
      THIS-OBJECT:resurserMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMainSC:Name = "resurserMainSC".
      /*  */
      /* resurserMainSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMainSC:Panel1:Controls:Add(THIS-OBJECT:resurserMenuSC).
      THIS-OBJECT:resurserMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:resurserMainSC:SplitterDistance = 897.
      THIS-OBJECT:resurserMainSC:TabIndex = 0.
      /*  */
      /* resurserMenuSC */
      /*  */
      THIS-OBJECT:resurserMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMenuSC:Name = "resurserMenuSC".
      THIS-OBJECT:resurserMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurserMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel1:Controls:Add(THIS-OBJECT:ResursertoolStrip).
      /*  */
      /* resurserMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResurs).
      THIS-OBJECT:resurserMenuSC:Size = NEW System.Drawing.Size(897, 553).
      THIS-OBJECT:resurserMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurserMenuSC:TabIndex = 3.
      /*  */
      /* ResursertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:laggTillResursButton.
      arrayvar2[2] = THIS-OBJECT:TaBortResursButton.
      THIS-OBJECT:ResursertoolStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:ResursertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ResursertoolStrip:Name = "ResursertoolStrip".
      THIS-OBJECT:ResursertoolStrip:Size = NEW System.Drawing.Size(897, 25).
      THIS-OBJECT:ResursertoolStrip:TabIndex = 2.
      THIS-OBJECT:ResursertoolStrip:Text = "ResursertoolStrip".
      /*  */
      /* laggTillResursButton */
      /*  */
      THIS-OBJECT:laggTillResursButton:Image = CAST(resources:GetObject("laggTillResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillResursButton:Name = "laggTillResursButton".
      THIS-OBJECT:laggTillResursButton:Size = NEW System.Drawing.Size(103, 22).
      THIS-OBJECT:laggTillResursButton:Text = "Lägg till resurs".
      THIS-OBJECT:laggTillResursButton:Click:Subscribe(THIS-OBJECT:laggTillResursButton_Click).
      /*  */
      /* TaBortResursButton */
      /*  */
      THIS-OBJECT:TaBortResursButton:Image = CAST(resources:GetObject("TaBortResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortResursButton:Name = "TaBortResursButton".
      THIS-OBJECT:TaBortResursButton:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:TaBortResursButton:Text = "Ta bort resurs".
      THIS-OBJECT:TaBortResursButton:Click:Subscribe(THIS-OBJECT:TaBortResursButton_Click).
      /*  */
      /* gridResurs */
      /*  */
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurs:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:gridResurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurs:GuruContext = ?.
      THIS-OBJECT:gridResurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurs:Name = "gridResurs".
      THIS-OBJECT:gridResurs:Size = NEW System.Drawing.Size(897, 524).
      THIS-OBJECT:gridResurs:TabIndex = 0.
      THIS-OBJECT:gridResurs:Text = "grid1".
      THIS-OBJECT:gridResurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurs_AfterCellUpdate).
      /*  */
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:KatalogMainSC).
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* KatalogMainSC */
      /*  */
      THIS-OBJECT:KatalogMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogMainSC:Name = "KatalogMainSC".
      /*  */
      /* KatalogMainSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel1:Controls:Add(THIS-OBJECT:KatalogKatalogSC).
      /*  */
      /* KatalogMainSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImage = CAST(resources:GetObject("KatalogMainSC.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:KatalogMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:KatalogMainSC:SplitterDistance = 497.
      THIS-OBJECT:KatalogMainSC:TabIndex = 1.
      /*  */
      /* KatalogKatalogSC */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogKatalogSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:KatalogKatalogSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogKatalogSC:Name = "KatalogKatalogSC".
      THIS-OBJECT:KatalogKatalogSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* KatalogKatalogSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel1:Controls:Add(THIS-OBJECT:KatalogertoolStrip).
      /*  */
      /* KatalogKatalogSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel2:Controls:Add(THIS-OBJECT:gridKataloger).
      THIS-OBJECT:KatalogKatalogSC:Size = NEW System.Drawing.Size(497, 553).
      THIS-OBJECT:KatalogKatalogSC:SplitterDistance = 25.
      THIS-OBJECT:KatalogKatalogSC:TabIndex = 0.
      /*  */
      /* KatalogertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:KatalogerValjButton.
      arrayvar3[2] = THIS-OBJECT:KopieraKatalogButton.
      arrayvar3[3] = THIS-OBJECT:TaBortKatalogButton.
      THIS-OBJECT:KatalogertoolStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:KatalogertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogertoolStrip:Name = "KatalogertoolStrip".
      THIS-OBJECT:KatalogertoolStrip:Size = NEW System.Drawing.Size(497, 25).
      THIS-OBJECT:KatalogertoolStrip:TabIndex = 1.
      THIS-OBJECT:KatalogertoolStrip:Text = "toolStrip1".
      /*  */
      /* KatalogerValjButton */
      /*  */
      THIS-OBJECT:KatalogerValjButton:Image = CAST(resources:GetObject("KatalogerValjButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KatalogerValjButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KatalogerValjButton:Name = "KatalogerValjButton".
      THIS-OBJECT:KatalogerValjButton:Size = NEW System.Drawing.Size(88, 22).
      THIS-OBJECT:KatalogerValjButton:Text = "Välj katalog".
      THIS-OBJECT:KatalogerValjButton:Click:Subscribe(THIS-OBJECT:KatalogerValjButton_Click).
      /*  */
      /* KopieraKatalogButton */
      /*  */
      THIS-OBJECT:KopieraKatalogButton:Image = CAST(resources:GetObject("KopieraKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KopieraKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KopieraKatalogButton:Name = "KopieraKatalogButton".
      THIS-OBJECT:KopieraKatalogButton:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:KopieraKatalogButton:Text = "Kopiera katalog".
      THIS-OBJECT:KopieraKatalogButton:Click:Subscribe(THIS-OBJECT:KopieraKatalogButton_Click).
      /*  */
      /* TaBortKatalogButton */
      /*  */
      THIS-OBJECT:TaBortKatalogButton:Image = CAST(resources:GetObject("TaBortKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortKatalogButton:Name = "TaBortKatalogButton".
      THIS-OBJECT:TaBortKatalogButton:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:TaBortKatalogButton:Text = "Ta bort katalog".
      THIS-OBJECT:TaBortKatalogButton:Click:Subscribe(THIS-OBJECT:TaBortKatalogButton_Click).
      /*  */
      /* gridKataloger */
      /*  */
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridKataloger:DisplayLayout:Appearance = appearance75.
      THIS-OBJECT:gridKataloger:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridKataloger:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance76:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance76:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance76:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:Appearance = appearance76.
      appearance77:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BandLabelAppearance = appearance77.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance78:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance78:BackColor2 = System.Drawing.SystemColors:Control.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance78:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:PromptAppearance = appearance78.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveCellAppearance = appearance79.
      appearance80:BackColor = System.Drawing.SystemColors:Highlight.
      appearance80:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveRowAppearance = appearance80.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CardAreaAppearance = appearance81.
      appearance82:BorderColor = System.Drawing.Color:Silver.
      appearance82:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellAppearance = appearance82.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellPadding = 0.
      appearance83:BackColor = System.Drawing.SystemColors:Control.
      appearance83:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance83:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance83:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance83:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:GroupByRowAppearance = appearance83.
      appearance84:TextHAlignAsString = "Left".
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderAppearance = appearance84.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      appearance85:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowAppearance = appearance85.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance86:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:TemplateAddRowAppearance = appearance86.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridKataloger:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridKataloger:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridKataloger:GuruContext = ?.
      THIS-OBJECT:gridKataloger:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridKataloger:Name = "gridKataloger".
      THIS-OBJECT:gridKataloger:Size = NEW System.Drawing.Size(497, 524).
      THIS-OBJECT:gridKataloger:TabIndex = 0.
      THIS-OBJECT:gridKataloger:Text = "grid1".
      THIS-OBJECT:gridKataloger:AfterCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_AfterCellUpdate).
      THIS-OBJECT:gridKataloger:CellChange:Subscribe(THIS-OBJECT:gridKataloger_CellChange).
      THIS-OBJECT:gridKataloger:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_BeforeCellUpdate).
      THIS-OBJECT:gridKataloger:DoubleClickRow:Subscribe(THIS-OBJECT:gridKataloger_DoubleClickRow).
      /*  */
      /* TabResursPriser */
      /*  */
      THIS-OBJECT:TabResursPriser:Controls:Add(THIS-OBJECT:splitContainerResursPris).
      THIS-OBJECT:TabResursPriser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResursPriser:Name = "TabResursPriser".
      THIS-OBJECT:TabResursPriser:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerResursPris */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPris:Name = "splitContainerResursPris".
      /*  */
      /* splitContainerResursPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerResursPrisLista).
      /*  */
      /* splitContainerResursPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel2:Controls:Add(THIS-OBJECT:TabManagerResurspriser).
      THIS-OBJECT:splitContainerResursPris:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerResursPris:SplitterDistance = 475.
      THIS-OBJECT:splitContainerResursPris:TabIndex = 4.
      /*  */
      /* splitContainerResursPrisLista */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPrisLista:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerResursPrisLista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPrisLista:Name = "splitContainerResursPrisLista".
      THIS-OBJECT:splitContainerResursPrisLista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerResursPrisLista.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:Controls:Add(THIS-OBJECT:resurspristoolStrip).
      /*  */
      /* splitContainerResursPrisLista.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:Controls:Add(THIS-OBJECT:gridResurspris).
      THIS-OBJECT:splitContainerResursPrisLista:Size = NEW System.Drawing.Size(475, 553).
      THIS-OBJECT:splitContainerResursPrisLista:SplitterDistance = 25.
      THIS-OBJECT:splitContainerResursPrisLista:TabIndex = 0.
      /*  */
      /* resurspristoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:taBortResursPrisButton.
      THIS-OBJECT:resurspristoolStrip:Items:AddRange(arrayvar4).
      THIS-OBJECT:resurspristoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspristoolStrip:Name = "resurspristoolStrip".
      THIS-OBJECT:resurspristoolStrip:Size = NEW System.Drawing.Size(475, 25).
      THIS-OBJECT:resurspristoolStrip:TabIndex = 0.
      THIS-OBJECT:resurspristoolStrip:Text = "toolStrip3".
      /*  */
      /* taBortResursPrisButton */
      /*  */
      THIS-OBJECT:taBortResursPrisButton:Image = CAST(resources:GetObject("taBortResursPrisButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortResursPrisButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortResursPrisButton:Name = "taBortResursPrisButton".
      THIS-OBJECT:taBortResursPrisButton:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:taBortResursPrisButton:Text = "Ta bort resurs".
      THIS-OBJECT:taBortResursPrisButton:ToolTipText = "Tar bort den markerade resursen från katalogen".
      THIS-OBJECT:taBortResursPrisButton:Click:Subscribe(THIS-OBJECT:taBortResursPrisButton_Click).
      /*  */
      /* gridResurspris */
      /*  */
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurspris:DisplayLayout:Appearance = appearance87.
      THIS-OBJECT:gridResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance88:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance88:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance88:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:Appearance = appearance88.
      appearance89:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance89.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance90:BackColor2 = System.Drawing.SystemColors:Control.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance90:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance90.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance91.
      appearance92:BackColor = System.Drawing.SystemColors:Highlight.
      appearance92:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance92.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CardAreaAppearance = appearance93.
      appearance94:BorderColor = System.Drawing.Color:Silver.
      appearance94:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellAppearance = appearance94.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance95:BackColor = System.Drawing.SystemColors:Control.
      appearance95:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance95:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance95:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance95:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance95.
      appearance96:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderAppearance = appearance96.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      appearance97:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowAppearance = appearance97.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance98:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance98.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurspris:GuruContext = ?.
      THIS-OBJECT:gridResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurspris:Name = "gridResurspris".
      THIS-OBJECT:gridResurspris:Size = NEW System.Drawing.Size(475, 524).
      THIS-OBJECT:gridResurspris:TabIndex = 1.
      THIS-OBJECT:gridResurspris:Text = "grid1".
      THIS-OBJECT:gridResurspris:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurspris_AfterCellUpdate).
      /*  */
      /* TabManagerResurspriser */
      /*  */
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPPaslag).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPNiva).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPRegel).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPBasresurser).
      THIS-OBJECT:TabManagerResurspriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerResurspriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerResurspriser:Name = "TabManagerResurspriser".
      THIS-OBJECT:TabManagerResurspriser:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv.
      THIS-OBJECT:TabManagerResurspriser:Size = NEW System.Drawing.Size(666, 553).
      THIS-OBJECT:TabManagerResurspriser:TabIndex = 0.
      ultraTab21:TabPage = THIS-OBJECT:TabRPPaslag.
      ultraTab21:Text = "Påslag".
      ultraTab23:TabPage = THIS-OBJECT:TabRPNiva.
      ultraTab23:Text = "Nivåer".
      ultraTab18:TabPage = THIS-OBJECT:TabRPRegel.
      ultraTab18:Text = "Regel".
      ultraTab24:Key = "Basresurser".
      ultraTab24:TabPage = THIS-OBJECT:TabRPBasresurser.
      ultraTab24:Text = "Basresurser".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
      arrayvar5[1] = ultraTab21.
      arrayvar5[2] = ultraTab23.
      arrayvar5[3] = ultraTab18.
      arrayvar5[4] = ultraTab24.
      THIS-OBJECT:TabManagerResurspriser:Tabs:AddRange(arrayvar5).
      /*  */
      /* ultraTabSharedControlsPagePaslagNiv */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Name = "ultraTabSharedControlsPagePaslagNiv".
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* TabMtrl */
      /*  */
      THIS-OBJECT:TabMtrl:Controls:Add(THIS-OBJECT:mtrlMainSC).
      THIS-OBJECT:TabMtrl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMtrl:Name = "TabMtrl".
      THIS-OBJECT:TabMtrl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* mtrlMainSC */
      /*  */
      THIS-OBJECT:mtrlMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlMainSC:Name = "mtrlMainSC".
      /*  */
      /* mtrlMainSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlMainSC:Panel1:Controls:Add(THIS-OBJECT:mtrlMenuSC).
      THIS-OBJECT:mtrlMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:mtrlMainSC:SplitterDistance = 576.
      THIS-OBJECT:mtrlMainSC:TabIndex = 2.
      /*  */
      /* mtrlMenuSC */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:mtrlMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlMenuSC:Name = "mtrlMenuSC".
      THIS-OBJECT:mtrlMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* mtrlMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Panel1:Controls:Add(THIS-OBJECT:mtrltoolStrip).
      /*  */
      /* mtrlMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridMateriel).
      THIS-OBJECT:mtrlMenuSC:Size = NEW System.Drawing.Size(576, 553).
      THIS-OBJECT:mtrlMenuSC:SplitterDistance = 25.
      THIS-OBJECT:mtrlMenuSC:TabIndex = 0.
      /*  */
      /* mtrltoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS System.Windows.Forms.ToolStripItem EXTENT 5 NO-UNDO.
      arrayvar6[1] = THIS-OBJECT:laggTillMtrlButton.
      arrayvar6[2] = THIS-OBJECT:taBortMtrlButton.
      arrayvar6[3] = THIS-OBJECT:uppdateraMtrlKostButton.
      arrayvar6[4] = THIS-OBJECT:mtrlImportButton.
      arrayvar6[5] = THIS-OBJECT:mtrlExportButton.
      THIS-OBJECT:mtrltoolStrip:Items:AddRange(arrayvar6).
      THIS-OBJECT:mtrltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrltoolStrip:Name = "mtrltoolStrip".
      THIS-OBJECT:mtrltoolStrip:Size = NEW System.Drawing.Size(576, 25).
      THIS-OBJECT:mtrltoolStrip:TabIndex = 1.
      THIS-OBJECT:mtrltoolStrip:Text = "toolStrip1".
      /*  */
      /* laggTillMtrlButton */
      /*  */
      THIS-OBJECT:laggTillMtrlButton:Image = CAST(resources:GetObject("laggTillMtrlButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillMtrlButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillMtrlButton:Name = "laggTillMtrlButton".
      THIS-OBJECT:laggTillMtrlButton:Size = NEW System.Drawing.Size(115, 22).
      THIS-OBJECT:laggTillMtrlButton:Text = "Lägg till materiel".
      THIS-OBJECT:laggTillMtrlButton:Click:Subscribe(THIS-OBJECT:laggTillMtrlButton_Click).
      /*  */
      /* taBortMtrlButton */
      /*  */
      THIS-OBJECT:taBortMtrlButton:Image = CAST(resources:GetObject("taBortMtrlButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortMtrlButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortMtrlButton:Name = "taBortMtrlButton".
      THIS-OBJECT:taBortMtrlButton:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:taBortMtrlButton:Text = "Ta bort materiel".
      THIS-OBJECT:taBortMtrlButton:Click:Subscribe(THIS-OBJECT:taBortMtrlButton_Click).
      /*  */
      /* uppdateraMtrlKostButton */
      /*  */
      THIS-OBJECT:uppdateraMtrlKostButton:Image = CAST(resources:GetObject("uppdateraMtrlKostButton.Image"), System.Drawing.Image).
      THIS-OBJECT:uppdateraMtrlKostButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:uppdateraMtrlKostButton:Name = "uppdateraMtrlKostButton".
      THIS-OBJECT:uppdateraMtrlKostButton:Size = NEW System.Drawing.Size(180, 22).
      THIS-OBJECT:uppdateraMtrlKostButton:Text = "Uppdatera materielkostnader".
      THIS-OBJECT:uppdateraMtrlKostButton:Visible = FALSE.
      THIS-OBJECT:uppdateraMtrlKostButton:Click:Subscribe(THIS-OBJECT:uppdateraMtrlKostButton_Click).
      /*  */
      /* mtrlImportButton */
      /*  */
      THIS-OBJECT:mtrlImportButton:Image = CAST(resources:GetObject("mtrlImportButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlImportButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlImportButton:Name = "mtrlImportButton".
      THIS-OBJECT:mtrlImportButton:Size = NEW System.Drawing.Size(125, 22).
      THIS-OBJECT:mtrlImportButton:Text = "Importera materiel".
      /*  */
      /* mtrlExportButton */
      /*  */
      THIS-OBJECT:mtrlExportButton:Image = CAST(resources:GetObject("mtrlExportButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlExportButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlExportButton:Name = "mtrlExportButton".
      THIS-OBJECT:mtrlExportButton:Size = NEW System.Drawing.Size(122, 22).
      THIS-OBJECT:mtrlExportButton:Text = "Exportera materiel".
      /*  */
      /* gridMateriel */
      /*  */
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridMateriel:DisplayLayout:Appearance = appearance99.
      THIS-OBJECT:gridMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance100:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance100:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance100:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:Appearance = appearance100.
      appearance101:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance101.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance102:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance102:BackColor2 = System.Drawing.SystemColors:Control.
      appearance102:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance102:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance102.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance103:BackColor = System.Drawing.SystemColors:Window.
      appearance103:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance103.
      appearance104:BackColor = System.Drawing.SystemColors:Highlight.
      appearance104:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance104.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance105:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CardAreaAppearance = appearance105.
      appearance106:BorderColor = System.Drawing.Color:Silver.
      appearance106:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellAppearance = appearance106.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance107:BackColor = System.Drawing.SystemColors:Control.
      appearance107:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance107:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance107:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance107:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance107.
      appearance108:TextHAlignAsString = "Left".
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderAppearance = appearance108.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance109:BackColor = System.Drawing.SystemColors:Window.
      appearance109:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowAppearance = appearance109.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance110:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance110.
      THIS-OBJECT:gridMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridMateriel:GuruContext = ?.
      THIS-OBJECT:gridMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridMateriel:Name = "gridMateriel".
      THIS-OBJECT:gridMateriel:Size = NEW System.Drawing.Size(576, 524).
      THIS-OBJECT:gridMateriel:TabIndex = 0.
      THIS-OBJECT:gridMateriel:Text = "grid1".
      THIS-OBJECT:gridMateriel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridMateriel_AfterCellUpdate).
      /*  */
      /* TabP5 */
      /*  */
      THIS-OBJECT:TabP5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP5:Name = "TabP5".
      THIS-OBJECT:TabP5:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabP4 */
      /*  */
      THIS-OBJECT:TabP4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4:Name = "TabP4".
      THIS-OBJECT:TabP4:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabP3 */
      /*  */
      THIS-OBJECT:TabP3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP3:Name = "TabP3".
      THIS-OBJECT:TabP3:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabP2 */
      /*  */
      THIS-OBJECT:TabP2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2:Name = "TabP2".
      THIS-OBJECT:TabP2:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabP1 */
      /*  */
      THIS-OBJECT:TabP1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP1:Name = "TabP1".
      THIS-OBJECT:TabP1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabExport */
      /*  */
      THIS-OBJECT:TabExport:Controls:Add(THIS-OBJECT:splitContainerTotalExport).
      THIS-OBJECT:TabExport:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabExport:Name = "TabExport".
      THIS-OBJECT:TabExport:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerTotalExport */
      /*  */
      THIS-OBJECT:splitContainerTotalExport:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerTotalExport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerTotalExport:Name = "splitContainerTotalExport".
      THIS-OBJECT:splitContainerTotalExport:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerTotalExport:SplitterDistance = 381.
      THIS-OBJECT:splitContainerTotalExport:TabIndex = 0.
      /*  */
      /* TabPopups */
      /*  */
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:buttonOverResurspris).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyPaslagPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyMtrlPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP1arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP2arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP3arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP1Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP2Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP3Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP4Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP5Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyResursPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyNivaPanel).
      THIS-OBJECT:TabPopups:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabPopups:Name = "TabPopups".
      THIS-OBJECT:TabPopups:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* buttonOverResurspris */
      /*  */
      THIS-OBJECT:buttonOverResurspris:Location = NEW System.Drawing.Point(1022, 386).
      THIS-OBJECT:buttonOverResurspris:Name = "buttonOverResurspris".
      THIS-OBJECT:buttonOverResurspris:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonOverResurspris:TabIndex = 13.
      THIS-OBJECT:buttonOverResurspris:Text = "buttonOver1".
      /*  */
      /* nyPaslagPanel */
      /*  */
      THIS-OBJECT:nyPaslagPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyPaslagPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyPaslag).
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinPaslagBenamning).
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyPaslag).
      THIS-OBJECT:nyPaslagPanel:Location = NEW System.Drawing.Point(3, 398).
      THIS-OBJECT:nyPaslagPanel:Name = "nyPaslagPanel".
      THIS-OBJECT:nyPaslagPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyPaslagPanel:TabIndex = 12.
      THIS-OBJECT:nyPaslagPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyPaslag */
      /*  */
      THIS-OBJECT:buttonCancelNyPaslag:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyPaslag:Name = "buttonCancelNyPaslag".
      THIS-OBJECT:buttonCancelNyPaslag:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyPaslag:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyPaslag:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyPaslag:Click:Subscribe(THIS-OBJECT:buttonCancelNyPaslag_Click).
      /*  */
      /* fillinPaslagBenamning */
      /*  */
      THIS-OBJECT:fillinPaslagBenamning:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinPaslagBenamning:Name = "fillinPaslagBenamning".
      THIS-OBJECT:fillinPaslagBenamning:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinPaslagBenamning:TabIndex = 1.
      /*  */
      /* buttonNyPaslag */
      /*  */
      THIS-OBJECT:buttonNyPaslag:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyPaslag:Name = "buttonNyPaslag".
      THIS-OBJECT:buttonNyPaslag:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyPaslag:TabIndex = 3.
      THIS-OBJECT:buttonNyPaslag:Text = "Lägg till".
      THIS-OBJECT:buttonNyPaslag:Click:Subscribe(THIS-OBJECT:buttonNyPaslag_Click).
      /*  */
      /* nyMtrlPanel */
      /*  */
      THIS-OBJECT:nyMtrlPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyMtrlPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyMtrl).
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinArtNr).
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyMtrl).
      THIS-OBJECT:nyMtrlPanel:Location = NEW System.Drawing.Point(267, 3).
      THIS-OBJECT:nyMtrlPanel:Name = "nyMtrlPanel".
      THIS-OBJECT:nyMtrlPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyMtrlPanel:TabIndex = 11.
      THIS-OBJECT:nyMtrlPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyMtrl */
      /*  */
      THIS-OBJECT:buttonCancelNyMtrl:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyMtrl:Name = "buttonCancelNyMtrl".
      THIS-OBJECT:buttonCancelNyMtrl:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyMtrl:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyMtrl:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyMtrl:Click:Subscribe(THIS-OBJECT:buttonCancelNyMtrl_Click).
      /*  */
      /* fillinArtNr */
      /*  */
      THIS-OBJECT:fillinArtNr:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinArtNr:Name = "fillinArtNr".
      THIS-OBJECT:fillinArtNr:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinArtNr:TabIndex = 1.
      /*  */
      /* buttonNyMtrl */
      /*  */
      THIS-OBJECT:buttonNyMtrl:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyMtrl:Name = "buttonNyMtrl".
      THIS-OBJECT:buttonNyMtrl:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyMtrl:TabIndex = 3.
      THIS-OBJECT:buttonNyMtrl:Text = "Lägg till".
      THIS-OBJECT:buttonNyMtrl:Click:Subscribe(THIS-OBJECT:buttonNyMtrl_Click).
      /*  */
      /* nyP1arbkodPanel */
      /*  */
      THIS-OBJECT:nyP1arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP1arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:Location = NEW System.Drawing.Point(809, 51).
      THIS-OBJECT:nyP1arbkodPanel:Name = "nyP1arbkodPanel".
      THIS-OBJECT:nyP1arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP1arbkodPanel:TabIndex = 10.
      THIS-OBJECT:nyP1arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP1arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP1arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP1arbkod:Name = "buttonCancelNyP1arbkod".
      THIS-OBJECT:buttonCancelNyP1arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP1arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP1arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP1arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP1arbkod_Click).
      /*  */
      /* fillinP1arbkod */
      /*  */
      THIS-OBJECT:fillinP1arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP1arbkod:Name = "fillinP1arbkod".
      THIS-OBJECT:fillinP1arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP1arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP1arbkod */
      /*  */
      THIS-OBJECT:buttonNyP1arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP1arbkod:Name = "buttonNyP1arbkod".
      THIS-OBJECT:buttonNyP1arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP1arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP1arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP1arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP1arbkod_Click).
      /*  */
      /* nyP2arbkodPanel */
      /*  */
      THIS-OBJECT:nyP2arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP2arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:Location = NEW System.Drawing.Point(533, 157).
      THIS-OBJECT:nyP2arbkodPanel:Name = "nyP2arbkodPanel".
      THIS-OBJECT:nyP2arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP2arbkodPanel:TabIndex = 9.
      THIS-OBJECT:nyP2arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP2arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP2arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP2arbkod:Name = "buttonCancelNyP2arbkod".
      THIS-OBJECT:buttonCancelNyP2arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP2arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP2arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP2arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP2arbkod_Click).
      /*  */
      /* fillinP2arbkod */
      /*  */
      THIS-OBJECT:fillinP2arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP2arbkod:Name = "fillinP2arbkod".
      THIS-OBJECT:fillinP2arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP2arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP2arbkod */
      /*  */
      THIS-OBJECT:buttonNyP2arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP2arbkod:Name = "buttonNyP2arbkod".
      THIS-OBJECT:buttonNyP2arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP2arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP2arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP2arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP2arbkod_Click).
      /*  */
      /* nyP3arbkodPanel */
      /*  */
      THIS-OBJECT:nyP3arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP3arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:Location = NEW System.Drawing.Point(533, 51).
      THIS-OBJECT:nyP3arbkodPanel:Name = "nyP3arbkodPanel".
      THIS-OBJECT:nyP3arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP3arbkodPanel:TabIndex = 8.
      THIS-OBJECT:nyP3arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP3arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP3arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP3arbkod:Name = "buttonCancelNyP3arbkod".
      THIS-OBJECT:buttonCancelNyP3arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP3arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP3arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP3arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP3arbkod_Click).
      /*  */
      /* fillinP3arbkod */
      /*  */
      THIS-OBJECT:fillinP3arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP3arbkod:Name = "fillinP3arbkod".
      THIS-OBJECT:fillinP3arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP3arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP3arbkod */
      /*  */
      THIS-OBJECT:buttonNyP3arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP3arbkod:Name = "buttonNyP3arbkod".
      THIS-OBJECT:buttonNyP3arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP3arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP3arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP3arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP3arbkod_Click).
      /*  */
      /* nyP1Panel */
      /*  */
      THIS-OBJECT:nyP1Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP1Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP1ArbKod).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP1LopNr).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP1).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP1).
      THIS-OBJECT:nyP1Panel:Location = NEW System.Drawing.Point(533, 361).
      THIS-OBJECT:nyP1Panel:Name = "nyP1Panel".
      THIS-OBJECT:nyP1Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP1Panel:TabIndex = 7.
      THIS-OBJECT:nyP1Panel:Visible = FALSE.
      /*  */
      /* comboP1ArbKod */
      /*  */
      THIS-OBJECT:comboP1ArbKod:Location = NEW System.Drawing.Point(4, 38).
      THIS-OBJECT:comboP1ArbKod:Name = "comboP1ArbKod".
      THIS-OBJECT:comboP1ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP1ArbKod:TabIndex = 8.
      /*  */
      /* fillinP1LopNr */
      /*  */
      THIS-OBJECT:fillinP1LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP1LopNr:Name = "fillinP1LopNr".
      THIS-OBJECT:fillinP1LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP1LopNr:TabIndex = 7.
      /*  */
      /* buttonCancelNyP1 */
      /*  */
      THIS-OBJECT:buttonCancelNyP1:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP1:Name = "buttonCancelNyP1".
      THIS-OBJECT:buttonCancelNyP1:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP1:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP1:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP1:Click:Subscribe(THIS-OBJECT:buttonCancelNyP1_Click).
      /*  */
      /* buttonNyP1 */
      /*  */
      THIS-OBJECT:buttonNyP1:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP1:Name = "buttonNyP1".
      THIS-OBJECT:buttonNyP1:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP1:TabIndex = 3.
      THIS-OBJECT:buttonNyP1:Text = "Lägg till".
      THIS-OBJECT:buttonNyP1:Click:Subscribe(THIS-OBJECT:buttonNyP1_Click).
      /*  */
      /* nyP2Panel */
      /*  */
      THIS-OBJECT:nyP2Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP2Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP2ArbKod).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP2LopNr).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP2).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP2).
      THIS-OBJECT:nyP2Panel:Location = NEW System.Drawing.Point(267, 180).
      THIS-OBJECT:nyP2Panel:Name = "nyP2Panel".
      THIS-OBJECT:nyP2Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP2Panel:TabIndex = 6.
      THIS-OBJECT:nyP2Panel:Visible = FALSE.
      /*  */
      /* comboP2ArbKod */
      /*  */
      THIS-OBJECT:comboP2ArbKod:Location = NEW System.Drawing.Point(4, 30).
      THIS-OBJECT:comboP2ArbKod:Name = "comboP2ArbKod".
      THIS-OBJECT:comboP2ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP2ArbKod:TabIndex = 7.
      /*  */
      /* fillinP2LopNr */
      /*  */
      THIS-OBJECT:fillinP2LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP2LopNr:Name = "fillinP2LopNr".
      THIS-OBJECT:fillinP2LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP2LopNr:TabIndex = 6.
      /*  */
      /* buttonCancelNyP2 */
      /*  */
      THIS-OBJECT:buttonCancelNyP2:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP2:Name = "buttonCancelNyP2".
      THIS-OBJECT:buttonCancelNyP2:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP2:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP2:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP2:Click:Subscribe(THIS-OBJECT:buttonCancelNyP2_Click).
      /*  */
      /* buttonNyP2 */
      /*  */
      THIS-OBJECT:buttonNyP2:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP2:Name = "buttonNyP2".
      THIS-OBJECT:buttonNyP2:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP2:TabIndex = 3.
      THIS-OBJECT:buttonNyP2:Text = "Lägg till".
      THIS-OBJECT:buttonNyP2:Click:Subscribe(THIS-OBJECT:buttonNyP2_Click).
      /*  */
      /* nyP3Panel */
      /*  */
      THIS-OBJECT:nyP3Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP3Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP3ArbKod).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP3LopNr).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP3).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP3).
      THIS-OBJECT:nyP3Panel:Location = NEW System.Drawing.Point(267, 361).
      THIS-OBJECT:nyP3Panel:Name = "nyP3Panel".
      THIS-OBJECT:nyP3Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP3Panel:TabIndex = 5.
      THIS-OBJECT:nyP3Panel:Visible = FALSE.
      /*  */
      /* comboP3ArbKod */
      /*  */
      THIS-OBJECT:comboP3ArbKod:Location = NEW System.Drawing.Point(4, 38).
      THIS-OBJECT:comboP3ArbKod:Name = "comboP3ArbKod".
      THIS-OBJECT:comboP3ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP3ArbKod:TabIndex = 6.
      /*  */
      /* fillinP3LopNr */
      /*  */
      THIS-OBJECT:fillinP3LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP3LopNr:Name = "fillinP3LopNr".
      THIS-OBJECT:fillinP3LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP3LopNr:TabIndex = 5.
      /*  */
      /* buttonCancelNyP3 */
      /*  */
      THIS-OBJECT:buttonCancelNyP3:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP3:Name = "buttonCancelNyP3".
      THIS-OBJECT:buttonCancelNyP3:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP3:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP3:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP3:Click:Subscribe(THIS-OBJECT:buttonCancelNyP3_Click).
      /*  */
      /* buttonNyP3 */
      /*  */
      THIS-OBJECT:buttonNyP3:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP3:Name = "buttonNyP3".
      THIS-OBJECT:buttonNyP3:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP3:TabIndex = 3.
      THIS-OBJECT:buttonNyP3:Text = "Lägg till".
      THIS-OBJECT:buttonNyP3:Click:Subscribe(THIS-OBJECT:buttonNyP3_Click).
      /*  */
      /* nyP4Panel */
      /*  */
      THIS-OBJECT:nyP4Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP4Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP4).
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP4arbkod).
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP4).
      THIS-OBJECT:nyP4Panel:Location = NEW System.Drawing.Point(1, 290).
      THIS-OBJECT:nyP4Panel:Name = "nyP4Panel".
      THIS-OBJECT:nyP4Panel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP4Panel:TabIndex = 4.
      THIS-OBJECT:nyP4Panel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP4 */
      /*  */
      THIS-OBJECT:buttonCancelNyP4:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP4:Name = "buttonCancelNyP4".
      THIS-OBJECT:buttonCancelNyP4:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP4:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP4:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP4:Click:Subscribe(THIS-OBJECT:buttonCancelNyP4_Click).
      /*  */
      /* fillinP4arbkod */
      /*  */
      THIS-OBJECT:fillinP4arbkod:Location = NEW System.Drawing.Point(4, 10).
      THIS-OBJECT:fillinP4arbkod:Name = "fillinP4arbkod".
      THIS-OBJECT:fillinP4arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP4arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP4 */
      /*  */
      THIS-OBJECT:buttonNyP4:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP4:Name = "buttonNyP4".
      THIS-OBJECT:buttonNyP4:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP4:TabIndex = 3.
      THIS-OBJECT:buttonNyP4:Text = "Lägg till".
      THIS-OBJECT:buttonNyP4:Click:Subscribe(THIS-OBJECT:buttonNyP4_Click).
      /*  */
      /* nyP5Panel */
      /*  */
      THIS-OBJECT:nyP5Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP5Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP5).
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP5arbkod).
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP5).
      THIS-OBJECT:nyP5Panel:Location = NEW System.Drawing.Point(1, 184).
      THIS-OBJECT:nyP5Panel:Name = "nyP5Panel".
      THIS-OBJECT:nyP5Panel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP5Panel:TabIndex = 3.
      THIS-OBJECT:nyP5Panel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP5 */
      /*  */
      THIS-OBJECT:buttonCancelNyP5:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP5:Name = "buttonCancelNyP5".
      THIS-OBJECT:buttonCancelNyP5:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP5:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP5:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP5:Click:Subscribe(THIS-OBJECT:buttonCancelNyP5_Click).
      /*  */
      /* fillinP5arbkod */
      /*  */
      THIS-OBJECT:fillinP5arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP5arbkod:Name = "fillinP5arbkod".
      THIS-OBJECT:fillinP5arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP5arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP5 */
      /*  */
      THIS-OBJECT:buttonNyP5:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP5:Name = "buttonNyP5".
      THIS-OBJECT:buttonNyP5:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP5:TabIndex = 3.
      THIS-OBJECT:buttonNyP5:Text = "Lägg till".
      THIS-OBJECT:buttonNyP5:Click:Subscribe(THIS-OBJECT:buttonNyP5_Click).
      /*  */
      /* nyResursPanel */
      /*  */
      THIS-OBJECT:nyResursPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyResursPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyResurs).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinResursNr).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinResursBenamning).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyResurs).
      THIS-OBJECT:nyResursPanel:Location = NEW System.Drawing.Point(1, 3).
      THIS-OBJECT:nyResursPanel:Name = "nyResursPanel".
      THIS-OBJECT:nyResursPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyResursPanel:TabIndex = 2.
      THIS-OBJECT:nyResursPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyResurs */
      /*  */
      THIS-OBJECT:buttonCancelNyResurs:Location = NEW System.Drawing.Point(168, 62).
      THIS-OBJECT:buttonCancelNyResurs:Name = "buttonCancelNyResurs".
      THIS-OBJECT:buttonCancelNyResurs:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyResurs:TabIndex = 5.
      THIS-OBJECT:buttonCancelNyResurs:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyResurs:Click:Subscribe(THIS-OBJECT:buttonCancelNyResurs_Click).
      /*  */
      /* fillinResursNr */
      /*  */
      THIS-OBJECT:fillinResursNr:Location = NEW System.Drawing.Point(4, 10).
      THIS-OBJECT:fillinResursNr:Name = "fillinResursNr".
      THIS-OBJECT:fillinResursNr:Size = NEW System.Drawing.Size(240, 26).
      THIS-OBJECT:fillinResursNr:TabIndex = 1.
      /*  */
      /* fillinResursBenamning */
      /*  */
      THIS-OBJECT:fillinResursBenamning:Location = NEW System.Drawing.Point(4, 37).
      THIS-OBJECT:fillinResursBenamning:Name = "fillinResursBenamning".
      THIS-OBJECT:fillinResursBenamning:Size = NEW System.Drawing.Size(240, 26).
      THIS-OBJECT:fillinResursBenamning:TabIndex = 2.
      /*  */
      /* buttonNyResurs */
      /*  */
      THIS-OBJECT:buttonNyResurs:Location = NEW System.Drawing.Point(91, 62).
      THIS-OBJECT:buttonNyResurs:Name = "buttonNyResurs".
      THIS-OBJECT:buttonNyResurs:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyResurs:TabIndex = 2.
      THIS-OBJECT:buttonNyResurs:Text = "Lägg till".
      THIS-OBJECT:buttonNyResurs:Click:Subscribe(THIS-OBJECT:buttonNyResurs_Click).
      /*  */
      /* nyNivaPanel */
      /*  */
      THIS-OBJECT:nyNivaPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyNivaPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:comboNiva).
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyNiva).
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyNiva).
      THIS-OBJECT:nyNivaPanel:Location = NEW System.Drawing.Point(812, 157).
      THIS-OBJECT:nyNivaPanel:Name = "nyNivaPanel".
      THIS-OBJECT:nyNivaPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyNivaPanel:TabIndex = 12.
      THIS-OBJECT:nyNivaPanel:Visible = FALSE.
      /*  */
      /* comboNiva */
      /*  */
      THIS-OBJECT:comboNiva:Location = NEW System.Drawing.Point(4, 14).
      THIS-OBJECT:comboNiva:Name = "comboNiva".
      THIS-OBJECT:comboNiva:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboNiva:TabIndex = 8.
      /*  */
      /* buttonCancelNyNiva */
      /*  */
      THIS-OBJECT:buttonCancelNyNiva:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyNiva:Name = "buttonCancelNyNiva".
      THIS-OBJECT:buttonCancelNyNiva:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyNiva:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyNiva:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyNiva:Click:Subscribe(THIS-OBJECT:buttonCancelNyNiva_Click).
      /*  */
      /* buttonNyNiva */
      /*  */
      THIS-OBJECT:buttonNyNiva:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyNiva:Name = "buttonNyNiva".
      THIS-OBJECT:buttonNyNiva:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyNiva:TabIndex = 3.
      THIS-OBJECT:buttonNyNiva:Text = "Lägg till".
      THIS-OBJECT:buttonNyNiva:Click:Subscribe(THIS-OBJECT:buttonNyNiva_Click).
      /*  */
      /* laggTillPaslagButton */
      /*  */
      THIS-OBJECT:laggTillPaslagButton:Image = CAST(resources:GetObject("laggTillPaslagButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillPaslagButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillPaslagButton:Name = "laggTillPaslagButton".
      THIS-OBJECT:laggTillPaslagButton:Size = NEW System.Drawing.Size(106, 22).
      THIS-OBJECT:laggTillPaslagButton:Text = "Lägg till påslag".
      THIS-OBJECT:laggTillPaslagButton:Click:Subscribe(THIS-OBJECT:laggTillPaslagButton_Click).
      /*  */
      /* taBortPaslagButton */
      /*  */
      THIS-OBJECT:taBortPaslagButton:Image = CAST(resources:GetObject("taBortPaslagButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortPaslagButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortPaslagButton:Name = "taBortPaslagButton".
      THIS-OBJECT:taBortPaslagButton:Size = NEW System.Drawing.Size(102, 22).
      THIS-OBJECT:taBortPaslagButton:Text = "Ta bort påslag".
      THIS-OBJECT:taBortPaslagButton:Click:Subscribe(THIS-OBJECT:taBortPaslagButton_Click).
      /*  */
      /* miniToolStrip */
      /*  */
      THIS-OBJECT:miniToolStrip:AutoSize = FALSE.
      THIS-OBJECT:miniToolStrip:CanOverflow = FALSE.
      THIS-OBJECT:miniToolStrip:Dock = System.Windows.Forms.DockStyle:None.
      THIS-OBJECT:miniToolStrip:GripStyle = System.Windows.Forms.ToolStripGripStyle:Hidden.
      THIS-OBJECT:miniToolStrip:Location = NEW System.Drawing.Point(55, 3).
      THIS-OBJECT:miniToolStrip:Name = "miniToolStrip".
      THIS-OBJECT:miniToolStrip:Size = NEW System.Drawing.Size(592, 25).
      THIS-OBJECT:miniToolStrip:TabIndex = 2.
      /*  */
      /* TabDelad */
      /*  */
      THIS-OBJECT:TabDelad:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabDelad:Name = "TabDelad".
      THIS-OBJECT:TabDelad:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabManagerEkg */
      /*  */
      appearance111:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManagerEkg:Appearance = appearance111.
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabDelad).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabResurser).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP2).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP3).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP4).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP1).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP5).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabMtrl).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabResursPriser).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabExport).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabPopups).
      THIS-OBJECT:TabManagerEkg:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerEkg:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManagerEkg:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerEkg:Name = "TabManagerEkg".
      THIS-OBJECT:TabManagerEkg:SharedControlsPage = THIS-OBJECT:TabDelad.
      THIS-OBJECT:TabManagerEkg:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManagerEkg:TabIndex = 45.
      THIS-OBJECT:TabManagerEkg:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance112:Image = CAST(resources:GetObject("appearance112.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance112.
      appearance113:Image = CAST(resources:GetObject("appearance113.Image"), System.Object).
      ultraTab14:Appearance = appearance113.
      ultraTab14:Key = "Resurser".
      ultraTab14:TabPage = THIS-OBJECT:TabResurser.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Resurser".
      appearance114:Image = CAST(resources:GetObject("appearance114.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance114.
      appearance115:Image = CAST(resources:GetObject("appearance115.Image"), System.Object).
      ultraTab13:Appearance = appearance115.
      ultraTab13:Key = "Kataloger".
      ultraTab13:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Kataloger".
      appearance116:Image = CAST(resources:GetObject("appearance116.Image"), System.Object).
      ultraTab2:Appearance = appearance116.
      ultraTab2:Key = "Resurspris".
      ultraTab2:TabPage = THIS-OBJECT:TabResursPriser.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Resurspris".
      appearance117:Image = CAST(resources:GetObject("appearance117.Image"), System.Object).
      ultraTab3:Appearance = appearance117.
      ultraTab3:Key = "Materiel".
      ultraTab3:TabPage = THIS-OBJECT:TabMtrl.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Materiel".
      appearance118:Image = CAST(resources:GetObject("appearance118.Image"), System.Object).
      ultraTab20:ActiveAppearance = appearance118.
      appearance119:Image = CAST(resources:GetObject("appearance119.Image"), System.Object).
      ultraTab20:Appearance = appearance119.
      ultraTab20:Key = "P5".
      ultraTab20:TabPage = THIS-OBJECT:TabP5.
      ultraTab20:Text = "".
      ultraTab20:ToolTipText = "P5".
      appearance120:Image = CAST(resources:GetObject("appearance120.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance120.
      appearance121:Image = CAST(resources:GetObject("appearance121.Image"), System.Object).
      ultraTab22:Appearance = appearance121.
      ultraTab22:Key = "P4".
      ultraTab22:TabPage = THIS-OBJECT:TabP4.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "P4".
      appearance122:Image = CAST(resources:GetObject("appearance122.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance122.
      appearance123:Image = CAST(resources:GetObject("appearance123.Image"), System.Object).
      ultraTab19:Appearance = appearance123.
      ultraTab19:Key = "P3".
      ultraTab19:TabPage = THIS-OBJECT:TabP3.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "P3".
      appearance124:Image = CAST(resources:GetObject("appearance124.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance124.
      appearance125:Image = CAST(resources:GetObject("appearance125.Image"), System.Object).
      ultraTab17:Appearance = appearance125.
      ultraTab17:Key = "P2".
      ultraTab17:TabPage = THIS-OBJECT:TabP2.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "P2".
      appearance126:Image = CAST(resources:GetObject("appearance126.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance126.
      appearance127:Image = CAST(resources:GetObject("appearance127.Image"), System.Object).
      ultraTab16:Appearance = appearance127.
      ultraTab16:Key = "P1".
      ultraTab16:TabPage = THIS-OBJECT:TabP1.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "P1".
      ultraTab25:Key = "Export".
      ultraTab25:TabPage = THIS-OBJECT:TabExport.
      ultraTab25:Text = "Exportera".
      ultraTab4:Key = "TabPopups".
      ultraTab4:TabPage = THIS-OBJECT:TabPopups.
      ultraTab4:Text = "TabPopups".
      ultraTab4:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 11 NO-UNDO.
      arrayvar7[1] = ultraTab14.
      arrayvar7[2] = ultraTab13.
      arrayvar7[3] = ultraTab2.
      arrayvar7[4] = ultraTab3.
      arrayvar7[5] = ultraTab20.
      arrayvar7[6] = ultraTab22.
      arrayvar7[7] = ultraTab19.
      arrayvar7[8] = ultraTab17.
      arrayvar7[9] = ultraTab16.
      arrayvar7[10] = ultraTab25.
      arrayvar7[11] = ultraTab4.
      THIS-OBJECT:TabManagerEkg:Tabs:AddRange(arrayvar7).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanging).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanged:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanged).
      /*  */
      /* nyResursPop */
      /*  */
      THIS-OBJECT:nyResursPop:PopupControl = THIS-OBJECT:nyResursPanel.
      /*  */
      /* nyP5Pop */
      /*  */
      THIS-OBJECT:nyP5Pop:PopupControl = THIS-OBJECT:nyP5Panel.
      /*  */
      /* nyP4Pop */
      /*  */
      THIS-OBJECT:nyP4Pop:PopupControl = THIS-OBJECT:nyP4Panel.
      /*  */
      /* nyP3Pop */
      /*  */
      THIS-OBJECT:nyP3Pop:PopupControl = THIS-OBJECT:nyP3Panel.
      /*  */
      /* nyP2Pop */
      /*  */
      THIS-OBJECT:nyP2Pop:PopupControl = THIS-OBJECT:nyP2Panel.
      /*  */
      /* nyP1Pop */
      /*  */
      THIS-OBJECT:nyP1Pop:PopupControl = THIS-OBJECT:nyP1Panel.
      /*  */
      /* nyP3arbkodPop */
      /*  */
      THIS-OBJECT:nyP3arbkodPop:PopupControl = THIS-OBJECT:nyP3arbkodPanel.
      /*  */
      /* nyP2arbkodPop */
      /*  */
      THIS-OBJECT:nyP2arbkodPop:PopupControl = THIS-OBJECT:nyP2arbkodPanel.
      /*  */
      /* nyP1arbkodPop */
      /*  */
      THIS-OBJECT:nyP1arbkodPop:PopupControl = THIS-OBJECT:nyP1arbkodPanel.
      /*  */
      /* nyPaslagPop */
      /*  */
      THIS-OBJECT:nyPaslagPop:PopupControl = THIS-OBJECT:nyPaslagPanel.
      /*  */
      /* nyMtrlPop */
      /*  */
      THIS-OBJECT:nyMtrlPop:PopupControl = THIS-OBJECT:nyMtrlPanel.
      /*  */
      /* nyNivaPop */
      /*  */
      THIS-OBJECT:nyNivaPop:PopupControl = THIS-OBJECT:nyNivaPanel.
      /*  */
      /* EkgShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManagerEkg).
      THIS-OBJECT:Name = "EkgShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabRPPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslagOver:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:paslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurspaslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPNiva:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:PerformLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPRegel:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:regelMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:regelMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBasresurser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBasresurser:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripBasresurser:PerformLayout().
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabResurser:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:PerformLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogKatalogSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabResursPriser:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPrisLista:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerResurspriser:ResumeLayout(FALSE).
      THIS-OBJECT:TabMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:mtrlMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrltoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:mtrltoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabExport:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerTotalExport, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerTotalExport:ResumeLayout(FALSE).
      THIS-OBJECT:TabPopups:ResumeLayout(FALSE).
      THIS-OBJECT:nyPaslagPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyPaslagPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyMtrlPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyMtrlPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP4Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP4Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP5Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP5Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyResursPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyResursPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyNivaPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyNivaPanel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:TabManagerEkg, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerEkg:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManagerEkg_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
	  /*
	  IF VALID-OBJECT(TabManagerEkg:SelectedTab) THEN DO:
	     MESSAGE "changed" THIS-OBJECT:TabManagerEkg:SelectedTab:KEY e:TAB:KEY
	     THIS-OBJECT:TabManagerEkg:Tabs[e:TAB:KEY]:Selected
        VIEW-AS ALERT-BOX.
     END. 
       
	  
	  PROCESS EVENTS.
     */
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManagerEkg_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		IF VALID-OBJECT(TabManagerEkg:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabEkg_SelectedTabChanging(INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY).    
      END.        
		PROCESS EVENTS.
	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TaBortKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:TaBortKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID taBortNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   	   
		THIS-OBJECT:ControlWinLg:TaBortresursniva(INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["RESURSNR"]:Text), INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["NIVA"]:Text)).      
		RETURN.

	END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortMtrlButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortMtrl().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID taBortPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:TaBortPaslag().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TaBortResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   
		THIS-OBJECT:ControlDataDb:TaBortResurs(INTEGER(THIS-OBJECT:GridResurs:ActiveRow:Cells["RESURSNR"]:Text)).
      THIS-OBJECT:GridResurs:GuruFiltrera().
		RETURN.

	END METHOD.
	
	  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortResursprisButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortResurspris(INTEGER(THIS-OBJECT:GridResurspris:ActiveRow:Cells["RESURSNR"]:Text)).            
      RETURN.

   END METHOD.

 /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyResurspris_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):      
      THIS-OBJECT:ControlWinLg:NyResurspris().      

      RETURN.

   END METHOD.
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID uppdateraMtrlKostButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):   
      /*bör inte behöva användas -allt uppdateras direkt*/   
      THIS-OBJECT:ControlWinLg:UppdateraMtrlKost().      
      RETURN.
   END METHOD.
	
	METHOD PUBLIC VOID StartaExport():   
      EkgExport = NEW Modules.EkgData.EkgExport(THIS-OBJECT:Root).
      THIS-OBJECT:splitContainerTotalExport:Panel1:Controls:Add(THIS-OBJECT:EkgExport).
   END METHOD.
   
   METHOD PUBLIC VOID StartaPControl():   
      /*P1*/
      P1Control = NEW Modules.EkgData.EkgP1Control(THIS-OBJECT:Root).
      THIS-OBJECT:TabP1:Controls:Add(THIS-OBJECT:P1Control).
      P1Control:Dock = System.Windows.Forms.DockStyle:FILL.
      /*P2*/
      P2Control = NEW Modules.EkgData.EkgP2Control(THIS-OBJECT:Root).
      THIS-OBJECT:TabP2:Controls:Add(THIS-OBJECT:P2Control).
      P2Control:Dock = System.Windows.Forms.DockStyle:FILL.
      /*P3*/
      P3Control = NEW Modules.EkgData.EkgP3Control(THIS-OBJECT:Root).
      THIS-OBJECT:TabP3:Controls:Add(THIS-OBJECT:P3Control).
      P3Control:Dock = System.Windows.Forms.DockStyle:FILL.
      
      /*P4*/
      P4Control = NEW Modules.EkgData.EkgP4Control(THIS-OBJECT:Root).
      THIS-OBJECT:TabP4:Controls:Add(THIS-OBJECT:P4Control).
      P4Control:Dock = System.Windows.Forms.DockStyle:FILL.
      
      /*P5*/
      P5Control = NEW Modules.EkgData.EkgP5Control(THIS-OBJECT:Root).
      THIS-OBJECT:TabP5:Controls:Add(THIS-OBJECT:P5Control).
      P5Control:Dock = System.Windows.Forms.DockStyle:FILL.
   END METHOD.
   
   
	DESTRUCTOR PUBLIC EkgShell ( ):
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.
	END DESTRUCTOR.

END CLASS.