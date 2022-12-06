 
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
	{EKGTH.I}
	DEFINE PUBLIC VARIABLE ExportControl AS Modules.EkgData.EkgExportControl NO-UNDO.
   DEFINE PUBLIC VARIABLE katalogSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE P1Control AS Modules.EkgData.EkgP1Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P2Control AS Modules.EkgData.EkgP2Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P3Control AS Modules.EkgData.EkgP3Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P4Control AS Modules.EkgData.EkgP4Control NO-UNDO.
   DEFINE PUBLIC VARIABLE P5Control AS Modules.EkgData.EkgP5Control NO-UNDO.
   DEFINE PUBLIC VARIABLE MaterielControl AS Modules.EkgData.EkgMaterielControl NO-UNDO.
   DEFINE PUBLIC VARIABLE RPControl AS Modules.EkgData.EkgRPControl NO-UNDO.
   DEFINE PUBLIC VARIABLE KatalogerControl AS Modules.EkgData.EkgKatalogerControl NO-UNDO.
   DEFINE PUBLIC VARIABLE ResursControl AS Modules.EkgData.EkgResursControl NO-UNDO.
   DEFINE PUBLIC VARIABLE JamforControl AS Modules.EkgData.EkgJamforControl NO-UNDO.
   DEFINE PUBLIC VARIABLE P2KomControl AS Modules.EkgData.EkgP2Kom NO-UNDO.
   
   
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyNiva AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyPaslag AS Controls.Button NO-UNDO.
   
   DEFINE PRIVATE VARIABLE buttonNyPaslag AS Controls.Button NO-UNDO.
   
   
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
   DEFINE PRIVATE VARIABLE laggTillPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE miniToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
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
	/*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlWinLg                 AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlDataDb                AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE PUBLIC VARIABLE shellmainPanel AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE startSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerHolder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE TabDelad AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE TabExport AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabJamfor AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerEkg AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerMain AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMtrl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2Kom AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabPopups AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResurser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResursPriser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl9 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl8 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl7 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl6 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage2 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   
	CONSTRUCTOR PUBLIC EkgShell ( INPUT r AS Guru.Root ):	
		
      SUPER().
           
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      {EKGDBH.i ControlDataDb}
      
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
      IF THIS-OBJECT:ControlWinLg:tablage = "Katalog" THEN DO:
         IF VALID-OBJECT(TabManagerEkg:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
            THIS-OBJECT:ControlWinLg:TabEkg_SelectedTabChanging(INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY).    
         END.        
         PROCESS EVENTS.
      END.
      IF THIS-OBJECT:ControlWinLg:tablage = "Start" THEN DO:
         IF VALID-OBJECT(TabManagerMain:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
            THIS-OBJECT:ControlWinLg:TabEkg_SelectedTabChanging(INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerMain:SelectedTab:KEY).    
         END.        
         PROCESS EVENTS.
      END.
      ELSE RETURN.
   END METHOD.


   
   METHOD PUBLIC VOID StartaPControl(): 
      /*Anders Olsson Elpool i Umeå AB  21 nov 2013 12:47:12 
      Vid ny kontroll lägg in i destructor också! 
      */
      /*Kataloger*/
      KatalogerControl = NEW Modules.EkgData.EkgKatalogerControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:KatalogerControl).
      KatalogerControl:Dock = System.Windows.Forms.DockStyle:FILL.  
      
      /*Jamfor*/
      JamforControl = NEW Modules.EkgData.EkgJamforControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabJamfor:Controls:Add(THIS-OBJECT:JamforControl).
      JamforControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      /*Resurs*/
      ResursControl = NEW Modules.EkgData.EkgResursControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabResurser:Controls:Add(THIS-OBJECT:ResursControl).
      ResursControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
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
      
      /*Materiel*/
      MaterielControl = NEW Modules.EkgData.EkgMaterielControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabMtrl:Controls:Add(THIS-OBJECT:MaterielControl).
      MaterielControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
      /*ResursPris*/
      RPControl = NEW Modules.EkgData.EkgRPControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabResursPriser:Controls:Add(THIS-OBJECT:RPControl).
      RPControl:Dock = System.Windows.Forms.DockStyle:FILL.
     
      /*p2Kom*/
      P2KomControl = NEW Modules.EkgData.EkgP2Kom(THIS-OBJECT:Root).
      THIS-OBJECT:TabP2Kom:Controls:Add(THIS-OBJECT:P2KomControl).
      P2KomControl:Dock = System.Windows.Forms.DockStyle:FILL.
      
       
      /*Export*/
      ExportControl = NEW Modules.EkgData.EkgExportControl(THIS-OBJECT:Root).
      THIS-OBJECT:TabExport:Controls:Add(THIS-OBJECT:ExportControl).
      ExportControl:Dock = System.Windows.Forms.DockStyle:FILL.

     
      
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
   METHOD PRIVATE VOID taBortPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortPaslag().
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgShell").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab8 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab8 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab9 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab9 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab10 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab10 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab11 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab11 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab12 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab12 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab18 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab18 = NEW Infragistics.Win.UltraWinTabControl.UltraTab(TRUE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab21 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab21 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab23 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab23 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab(TRUE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabResursPriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabMtrl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabP2Kom = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabExport = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabPopups = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
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
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabResurser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabJamfor = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:laggTillPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:miniToolStrip = NEW System.Windows.Forms.ToolStrip().
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
      THIS-OBJECT:shellmainPanel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:TabManagerEkg = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:TabDelad = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:ultraTabSharedControlsPage2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:startSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabManagerMain = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:splitContainerHolder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:katalogSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl6 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl7 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl8 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl9 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
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
      CAST(THIS-OBJECT:startSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:startSC:Panel1:SuspendLayout().
      THIS-OBJECT:startSC:SuspendLayout().
      CAST(THIS-OBJECT:TabManagerMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerHolder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHolder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHolder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerHolder:SuspendLayout().
      CAST(THIS-OBJECT:katalogSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:katalogSC:Panel1:SuspendLayout().
      THIS-OBJECT:katalogSC:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabResursPriser */
      /*  */
      THIS-OBJECT:TabResursPriser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResursPriser:Name = "TabResursPriser".
      THIS-OBJECT:TabResursPriser:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabMtrl */
      /*  */
      THIS-OBJECT:TabMtrl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMtrl:Name = "TabMtrl".
      THIS-OBJECT:TabMtrl:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabP5 */
      /*  */
      THIS-OBJECT:TabP5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP5:Name = "TabP5".
      THIS-OBJECT:TabP5:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabP4 */
      /*  */
      THIS-OBJECT:TabP4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4:Name = "TabP4".
      THIS-OBJECT:TabP4:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabP3 */
      /*  */
      THIS-OBJECT:TabP3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP3:Name = "TabP3".
      THIS-OBJECT:TabP3:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabP2 */
      /*  */
      THIS-OBJECT:TabP2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2:Name = "TabP2".
      THIS-OBJECT:TabP2:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabP1 */
      /*  */
      THIS-OBJECT:TabP1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP1:Name = "TabP1".
      THIS-OBJECT:TabP1:Size = NEW System.Drawing.Size(758, 553).
      THIS-OBJECT:TabP1:Paint:Subscribe(THIS-OBJECT:TabP1_Paint).
      /*  */
      /* TabP2Kom */
      /*  */
      THIS-OBJECT:TabP2Kom:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabP2Kom:Name = "TabP2Kom".
      THIS-OBJECT:TabP2Kom:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabExport */
      /*  */
      THIS-OBJECT:TabExport:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabExport:Name = "TabExport".
      THIS-OBJECT:TabExport:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* TabPopups */
      /*  */
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
      THIS-OBJECT:TabPopups:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabPopups:Name = "TabPopups".
      THIS-OBJECT:TabPopups:Size = NEW System.Drawing.Size(758, 553).
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
      THIS-OBJECT:fillinPaslagBenamning:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinArtNr:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinP1arbkod:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinP2arbkod:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinP3arbkod:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinP4arbkod:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinP5arbkod:Rubrik = "GuruLabel".
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
      THIS-OBJECT:fillinResursBenamning:Rubrik = "GuruLabel".
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
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(379, 553).
      /*  */
      /* TabResurser */
      /*  */
      THIS-OBJECT:TabResurser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResurser:Name = "TabResurser".
      THIS-OBJECT:TabResurser:Size = NEW System.Drawing.Size(379, 553).
      /*  */
      /* TabJamfor */
      /*  */
      THIS-OBJECT:TabJamfor:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabJamfor:Name = "TabJamfor".
      THIS-OBJECT:TabJamfor:Size = NEW System.Drawing.Size(379, 553).
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
      /* shellmainPanel */
      /*  */
      THIS-OBJECT:shellmainPanel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:shellmainPanel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:shellmainPanel:Name = "shellmainPanel".
      THIS-OBJECT:shellmainPanel:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:shellmainPanel:TabIndex = 49.
      /*  */
      /* TabManagerEkg */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManagerEkg:Appearance = appearance1.
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabDelad).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP2).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP3).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP4).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP1).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP5).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabMtrl).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabResursPriser).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabExport).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabPopups).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP2Kom).
      THIS-OBJECT:TabManagerEkg:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerEkg:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManagerEkg:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerEkg:Name = "TabManagerEkg".
      THIS-OBJECT:TabManagerEkg:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage2.
      THIS-OBJECT:TabManagerEkg:Size = NEW System.Drawing.Size(762, 603).
      THIS-OBJECT:TabManagerEkg:TabIndex = 45.
      THIS-OBJECT:TabManagerEkg:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraTab8:Appearance = appearance2.
      ultraTab8:Key = "Resurspris".
      ultraTab8:TabPage = THIS-OBJECT:TabResursPriser.
      ultraTab8:Text = "".
      ultraTab8:ToolTipText = "Resurskostnad".
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      ultraTab9:Appearance = appearance3.
      ultraTab9:Key = "Materiel".
      ultraTab9:TabPage = THIS-OBJECT:TabMtrl.
      ultraTab9:Text = "".
      ultraTab9:ToolTipText = "Materiel".
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      ultraTab10:ActiveAppearance = appearance4.
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      ultraTab10:Appearance = appearance5.
      ultraTab10:Key = "P5".
      ultraTab10:TabPage = THIS-OBJECT:TabP5.
      ultraTab10:Text = "".
      ultraTab10:ToolTipText = "P5".
      appearance6:Image = CAST(resources:GetObject("appearance6.Image"), System.Object).
      ultraTab11:ActiveAppearance = appearance6.
      appearance7:Image = CAST(resources:GetObject("appearance7.Image"), System.Object).
      ultraTab11:Appearance = appearance7.
      ultraTab11:Key = "P4".
      ultraTab11:TabPage = THIS-OBJECT:TabP4.
      ultraTab11:Text = "".
      ultraTab11:ToolTipText = "P4".
      appearance8:Image = CAST(resources:GetObject("appearance8.Image"), System.Object).
      ultraTab12:ActiveAppearance = appearance8.
      appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
      ultraTab12:Appearance = appearance9.
      ultraTab12:Key = "P3".
      ultraTab12:TabPage = THIS-OBJECT:TabP3.
      ultraTab12:Text = "".
      ultraTab12:ToolTipText = "P3".
      appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
      ultraTab15:ActiveAppearance = appearance10.
      appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
      ultraTab15:Appearance = appearance11.
      ultraTab15:Key = "P2".
      ultraTab15:TabPage = THIS-OBJECT:TabP2.
      ultraTab15:Text = "".
      ultraTab15:ToolTipText = "P2".
      appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
      ultraTab2:Appearance = appearance12.
      ultraTab2:Key = "P2Kom".
      ultraTab2:TabPage = THIS-OBJECT:TabP2Kom.
      ultraTab2:Text = " ".
      ultraTab2:ToolTipText = "P2 Kommentarer".
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      ultraTab18:ActiveAppearance = appearance13.
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      ultraTab18:Appearance = appearance14.
      ultraTab18:Key = "P1".
      ultraTab18:TabPage = THIS-OBJECT:TabP1.
      ultraTab18:Text = "".
      ultraTab18:ToolTipText = "P1".
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      ultraTab21:ActiveAppearance = appearance15.
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      ultraTab21:Appearance = appearance16.
      ultraTab21:Key = "Export".
      ultraTab21:TabPage = THIS-OBJECT:TabExport.
      ultraTab21:Text = "".
      ultraTab21:ToolTipText = "Exportera".
      ultraTab23:Key = "TabPopups".
      ultraTab23:TabPage = THIS-OBJECT:TabPopups.
      ultraTab23:Text = "TabPopups".
      ultraTab23:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 10 NO-UNDO.
      arrayvar0[1] = ultraTab8.
      arrayvar0[2] = ultraTab9.
      arrayvar0[3] = ultraTab10.
      arrayvar0[4] = ultraTab11.
      arrayvar0[5] = ultraTab12.
      arrayvar0[6] = ultraTab15.
      arrayvar0[7] = ultraTab2.
      arrayvar0[8] = ultraTab18.
      arrayvar0[9] = ultraTab21.
      arrayvar0[10] = ultraTab23.
      THIS-OBJECT:TabManagerEkg:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanging).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanged:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanged).
      /*  */
      /* TabDelad */
      /*  */
      THIS-OBJECT:TabDelad:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabDelad:Name = "TabDelad".
      THIS-OBJECT:TabDelad:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabSharedControlsPage2 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage2:Name = "ultraTabSharedControlsPage2".
      THIS-OBJECT:ultraTabSharedControlsPage2:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* startSC */
      /*  */
      THIS-OBJECT:startSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:startSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:startSC:Name = "startSC".
      /*  */
      /* startSC.Panel1 */
      /*  */
      THIS-OBJECT:startSC:Panel1:Controls:Add(THIS-OBJECT:TabManagerMain).
      /*  */
      /* startSC.Panel2 */
      /*  */
      THIS-OBJECT:startSC:Panel2:BackgroundImage = CAST(resources:GetObject("startSC.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:startSC:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:startSC:Panel2Collapsed = TRUE.
      THIS-OBJECT:startSC:Size = NEW System.Drawing.Size(383, 603).
      THIS-OBJECT:startSC:SplitterDistance = 127.
      THIS-OBJECT:startSC:TabIndex = 0.
      /*  */
      /* TabManagerMain */
      /*  */
      appearance17:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManagerMain:Appearance = appearance17.
      THIS-OBJECT:TabManagerMain:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManagerMain:Controls:Add(THIS-OBJECT:TabResurser).
      THIS-OBJECT:TabManagerMain:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabManagerMain:Controls:Add(THIS-OBJECT:TabJamfor).
      THIS-OBJECT:TabManagerMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerMain:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManagerMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerMain:Name = "TabManagerMain".
      THIS-OBJECT:TabManagerMain:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManagerMain:Size = NEW System.Drawing.Size(383, 603).
      THIS-OBJECT:TabManagerMain:TabIndex = 46.
      THIS-OBJECT:TabManagerMain:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance18.
      appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
      ultraTab13:Appearance = appearance19.
      ultraTab13:Key = "Kataloger".
      ultraTab13:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Kataloger".
      appearance20:Image = CAST(resources:GetObject("appearance20.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance20.
      appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
      ultraTab14:Appearance = appearance21.
      ultraTab14:Key = "Resurser".
      ultraTab14:TabPage = THIS-OBJECT:TabResurser.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Resurser".
      appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance22.
      appearance23:Image = CAST(resources:GetObject("appearance23.Image"), System.Object).
      ultraTab1:Appearance = appearance23.
      ultraTab1:Key = "Jämför".
      ultraTab1:TabPage = THIS-OBJECT:TabJamfor.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Jämför".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar1[1] = ultraTab13.
      arrayvar1[2] = ultraTab14.
      arrayvar1[3] = ultraTab1.
      THIS-OBJECT:TabManagerMain:Tabs:AddRange(arrayvar1).
      THIS-OBJECT:TabManagerMain:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(379, 553).
      /*  */
      /* splitContainerHolder */
      /*  */
      THIS-OBJECT:splitContainerHolder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerHolder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerHolder:Name = "splitContainerHolder".
      /*  */
      /* splitContainerHolder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerHolder:Panel1:Controls:Add(THIS-OBJECT:startSC).
      /*  */
      /* splitContainerHolder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHolder:Panel2:Controls:Add(THIS-OBJECT:katalogSC).
      THIS-OBJECT:splitContainerHolder:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:splitContainerHolder:SplitterDistance = 383.
      THIS-OBJECT:splitContainerHolder:TabIndex = 48.
      THIS-OBJECT:splitContainerHolder:Visible = FALSE.
      /*  */
      /* katalogSC */
      /*  */
      THIS-OBJECT:katalogSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:katalogSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:katalogSC:Name = "katalogSC".
      /*  */
      /* katalogSC.Panel1 */
      /*  */
      THIS-OBJECT:katalogSC:Panel1:Controls:Add(THIS-OBJECT:TabManagerEkg).
      THIS-OBJECT:katalogSC:Panel2Collapsed = TRUE.
      THIS-OBJECT:katalogSC:Size = NEW System.Drawing.Size(762, 603).
      THIS-OBJECT:katalogSC:SplitterDistance = 254.
      THIS-OBJECT:katalogSC:TabIndex = 46.
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl2 */
      /*  */
      THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2".
      THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl3 */
      /*  */
      THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3".
      THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl4 */
      /*  */
      THIS-OBJECT:ultraTabPageControl4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4".
      THIS-OBJECT:ultraTabPageControl4:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl5 */
      /*  */
      THIS-OBJECT:ultraTabPageControl5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl5:Name = "ultraTabPageControl5".
      THIS-OBJECT:ultraTabPageControl5:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl6 */
      /*  */
      THIS-OBJECT:ultraTabPageControl6:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl6:Name = "ultraTabPageControl6".
      THIS-OBJECT:ultraTabPageControl6:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl7 */
      /*  */
      THIS-OBJECT:ultraTabPageControl7:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:ultraTabPageControl7:Name = "ultraTabPageControl7".
      THIS-OBJECT:ultraTabPageControl7:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl8 */
      /*  */
      THIS-OBJECT:ultraTabPageControl8:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl8:Name = "ultraTabPageControl8".
      THIS-OBJECT:ultraTabPageControl8:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* ultraTabPageControl9 */
      /*  */
      THIS-OBJECT:ultraTabPageControl9:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl9:Name = "ultraTabPageControl9".
      THIS-OBJECT:ultraTabPageControl9:Size = NEW System.Drawing.Size(758, 553).
      /*  */
      /* EkgShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerHolder).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:shellmainPanel).
      THIS-OBJECT:Name = "EkgShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
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
      THIS-OBJECT:startSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:startSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:startSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:TabManagerMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHolder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHolder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerHolder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHolder:ResumeLayout(FALSE).
      THIS-OBJECT:katalogSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:katalogSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:katalogSC:ResumeLayout(FALSE).
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
	METHOD PRIVATE VOID TabP1_Paint( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.PaintEventArgs ):
		
		RETURN.

	END METHOD.
	
   METHOD PUBLIC VOID Timer():
      
      MESSAGE "TIMER"
      VIEW-AS ALERT-BOX.

   END METHOD.
   
	DESTRUCTOR PUBLIC EkgShell ( ):
	   DELETE OBJECT  THIS-OBJECT:ControlWinLg NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:KatalogerControl NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:JamforControl NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:ResursControl NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:P1Control NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:P2Control NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:P3Control NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:P4Control NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:P5Control NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:MaterielControl NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:RPControl NO-ERROR.
      DELETE OBJECT  THIS-OBJECT:ExportControl NO-ERROR.
      IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.
	END DESTRUCTOR.

END CLASS.