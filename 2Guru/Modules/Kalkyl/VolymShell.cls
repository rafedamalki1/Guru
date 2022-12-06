 
 /*------------------------------------------------------------------------
    File        : VolymShell
    Purpose     : är ett skal till KalkVolymSCControl.cls
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue May 08 11:26:29 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Modules.Kalkyl.VolymShell INHERITS UserControl: 
	{KALKYLEXTRATAB.I}
   DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkVolymSCControl             AS Modules.Kalkyl.KalkVolymSCControl                                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.	
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.	
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVolymber AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE VARIABLE strcTTh AS HANDLE NO-UNDO.
	CONSTRUCTOR PUBLIC VolymShell ( INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InitializeTTs().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	METHOD PUBLIC LOGICAL Initialize(INPUT strcTThIN AS HANDLE):
	   strcTTh = strcTThIN.
	   /*
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlVolymShell = THIS-OBJECT.
      */
      RETURN TRUE.
   END METHOD.
   /*sparas volymds*/
   METHOD PUBLIC LOGICAL Initialize():
      
      RETURN TRUE.
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
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.VolymShell").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabVolymber = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabVolymber */
      /*  */
      THIS-OBJECT:TabVolymber:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabVolymber:Name = "TabVolymber".
      THIS-OBJECT:TabVolymber:Size = NEW System.Drawing.Size(944, 250).
      /*  */
      /* TabManager */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance1.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVolymber).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(948, 300).
      THIS-OBJECT:TabManager:TabIndex = 45.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraTab24:ActiveAppearance = appearance2.
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      ultraTab24:Appearance = appearance3.
      ultraTab24:Key = "Volymber".
      ultraTab24:TabPage = THIS-OBJECT:TabVolymber.
      ultraTab24:Text = "".
      ultraTab24:ToolTipText = "Volymberäkning av schaktmassor".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
      arrayvar0[1] = ultraTab24.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManager_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:TabManager_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(944, 250).
      /*  */
      /* VolymShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "VolymShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(948, 300).
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
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
	METHOD PRIVATE VOID TabManager_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
		THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManager_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
	
		IF KalkVolymSCControl = ? THEN DO:
         KalkVolymSCControl = NEW Modules.Kalkyl.KalkVolymSCControl(THIS-OBJECT:Root).
         THIS-OBJECT:TabVolymber:Controls:Add(THIS-OBJECT:KalkVolymSCControl).
         KalkVolymSCControl:Dock = System.Windows.Forms.DockStyle:FILL.
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetTitle("Volymberäkningar av schaktmassor").
      THIS-OBJECT:KalkdbControl:VolymberTTbuffh:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:VolymberTTbuffh:AVAILABLE THEN.
      ELSE DO:  
         THIS-OBJECT:KalkVolymSCControl:GridVolymBer:GuruReopen().
         THIS-OBJECT:KalkVolymSCControl:GridVolymBer:GuruFirstRow().
      END.  
      PROCESS EVENTS.
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC VolymShell ( ):
      
      DELETE OBJECT THIS-OBJECT:KalkVolymSCControl NO-ERROR. 

      KalkVolymSCControl = ?. 
      
	END DESTRUCTOR.

END CLASS.