 
 /*------------------------------------------------------------------------
    File        : StorningShell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jun 12 10:36:56 CEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Storning.StorningShell INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImpStor AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFelMedd AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabControlStorning AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                AS Modules.Storning.StorningDb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlStorImp                AS Modules.Storning.StorningImpCon NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlStorFel                AS Modules.Storning.StorFelmeddCon NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.	
	DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
	
	CONSTRUCTOR PUBLIC StorningShell (INPUT r AS Guru.Root  ):
		SUPER().
      THIS-OBJECT:Root = r.
      InitializeComponent ( ).
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:Storning.
      THIS-OBJECT:ControlDataDb:ControlShell = THIS-OBJECT.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      
      THIS-OBJECT:ControlDataDb:InitializeTTs().
    
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	METHOD PUBLIC VOID CreateForm():
	   THIS-OBJECT:StartaControl("").
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateRibbon().
      
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      IF THIS-OBJECT:ControlStorImp NE ? THEN 
      THIS-OBJECT:ControlStorImp:CreateGridStorImp().
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateRibbon():
      
   END METHOD.
   METHOD PUBLIC VOID StartaControl(INPUT vad AS CHARACTER):
      IF vad = "" OR vad = "imp" THEN DO:
        
         THIS-OBJECT:ControlStorImp = NEW Modules.Storning.StorningImpCon(THIS-OBJECT:Root).
         THIS-OBJECT:TabImpStor:Controls:Add(THIS-OBJECT:ControlStorImp).
         THIS-OBJECT:ControlStorImp:Dock = System.Windows.Forms.DockStyle:FILL.
        
      END.
      IF vad = "fel" THEN DO:
         THIS-OBJECT:ControlStorFel = NEW Modules.Storning.StorFelmeddCon(THIS-OBJECT:Root).
         THIS-OBJECT:TabFelMedd:Controls:Add(THIS-OBJECT:ControlStorFel).
         THIS-OBJECT:ControlStorFel:Dock = System.Windows.Forms.DockStyle:FILL.
      END.
   END METHOD.
   METHOD PUBLIC LOGICAL Initialize():
      
      THIS-OBJECT:CreateForm().
      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID FelMeddCheck():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:ControlDataDb:Storfelmedtth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:Storfelmedtth:AVAILABLE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(183),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         THIS-OBJECT:TabControlStorning:Tabs["TabFelMedd"]:Selected = TRUE.
      END.
      ELSE rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(217),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
   END METHOD.
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Storning.StorningShell").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabImpStor = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabFelMedd = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabControlStorning = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      CAST(THIS-OBJECT:TabControlStorning, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabControlStorning:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabImpStor */
      /*  */
      THIS-OBJECT:TabImpStor:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabImpStor:Name = "TabImpStor".
      THIS-OBJECT:TabImpStor:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabFelMedd */
      /*  */
      THIS-OBJECT:TabFelMedd:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFelMedd:Name = "TabFelMedd".
      THIS-OBJECT:TabFelMedd:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabControlStorning */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabControlStorning:Appearance = appearance1.
      THIS-OBJECT:TabControlStorning:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabControlStorning:Controls:Add(THIS-OBJECT:TabImpStor).
      THIS-OBJECT:TabControlStorning:Controls:Add(THIS-OBJECT:TabFelMedd).
      THIS-OBJECT:TabControlStorning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabControlStorning:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabControlStorning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabControlStorning:Name = "TabControlStorning".
      THIS-OBJECT:TabControlStorning:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabControlStorning:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabControlStorning:TabIndex = 0.
      THIS-OBJECT:TabControlStorning:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance2.
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      ultraTab1:Appearance = appearance3.
      ultraTab1:Key = "TabImpStor".
      ultraTab1:TabPage = THIS-OBJECT:TabImpStor.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Import av Störning".
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      ultraTab2:ActiveAppearance = appearance4.
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      ultraTab2:Appearance = appearance5.
      ultraTab2:Key = "TabFelMedd".
      ultraTab2:TabPage = THIS-OBJECT:TabFelMedd.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Felmeddelande vid import".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar0[1] = ultraTab1.
      arrayvar0[2] = ultraTab2.
      THIS-OBJECT:TabControlStorning:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabControlStorning:SelectedTabChanging:Subscribe(THIS-OBJECT:Storning_SelectedTabChanging).
      THIS-OBJECT:TabControlStorning:SelectedTabChanged:Subscribe(THIS-OBJECT:Storning_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* StorningShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabControlStorning).
      THIS-OBJECT:Name = "StorningShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      CAST(THIS-OBJECT:TabControlStorning, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabControlStorning:ResumeLayout(FALSE).
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
	METHOD PRIVATE VOID Storning_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Storning_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
	   DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.
      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabControlStorning:SelectedTab:KEY NO-ERROR.
		 IF cTo = "TabFelmedd" THEN DO:
         IF ControlStorFel = ? THEN DO:
            THIS-OBJECT:StartaControl("fel").            
         END.
         THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Felmeddelanden").
         THIS-OBJECT:ControlStorFel:GridFelMedd:GuruReopen().
         THIS-OBJECT:ControlStorFel:GridFelMedd:GuruFirstRow().           
      END.
		RETURN.

	END METHOD.

	
	DESTRUCTOR PUBLIC StorningShell ( ):

	END DESTRUCTOR.

END CLASS.