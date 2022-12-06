 
 /*------------------------------------------------------------------------
    File        : cust
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri Nov 30 12:40:19 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS cust INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE cust_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.

		
	CONSTRUCTOR PUBLIC cust (  ):
		
		
      SUPER().
      InitializeComponent().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("mybuttonlarge").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("mybuttonlarge").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("cust").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:cust_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:cust_Fill_Panel:SuspendLayout().
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabControl1:SuspendLayout().
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* cust_Fill_Panel */
      /*  */
      THIS-OBJECT:cust_Fill_Panel:Controls:Add(THIS-OBJECT:ultraTabControl1).
      THIS-OBJECT:cust_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:cust_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:cust_Fill_Panel:Location = NEW System.Drawing.Point(8, 158).
      THIS-OBJECT:cust_Fill_Panel:Name = "cust_Fill_Panel".
      THIS-OBJECT:cust_Fill_Panel:Size = NEW System.Drawing.Size(701, 246).
      THIS-OBJECT:cust_Fill_Panel:TabIndex = 0.
      /*  */
      /* ultraTabControl1 */
      /*  */
      THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(246, 57).
      THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1".
      THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(200, 100).
      THIS-OBJECT:ultraTabControl1:TabIndex = 0.
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(1, 20).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(196, 77).
      /*  */
      /* ultraToolbarsManager1 */
      /*  */
      THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
      THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
      THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
      ribbonTab1:Caption = "ribbon1".
      ribbonGroup1:Caption = "ribbonGroup1".
      buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar0[1] = buttonTool3.
      ribbonGroup1:Tools:AddRange(arrayvar0).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar1[1] = ribbonGroup1.
      ribbonTab1:Groups:AddRange(arrayvar1).
      ribbonTab2:Caption = "ribbon2".
      ribbonGroup2:Caption = "ribbonGroup1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar2[1] = ribbonGroup2.
      ribbonTab2:Groups:AddRange(arrayvar2).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 2 NO-UNDO.
      arrayvar3[1] = ribbonTab1.
      arrayvar3[2] = ribbonTab2.
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar3).
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
      appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
      buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance22.
      appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
      buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance21.
      buttonTool4:SharedPropsInternal:Caption = "mybuttonlarge".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar4[1] = buttonTool4.
      THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar4).
      /*  */
      /* m_cust_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 158).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Name = "m_cust_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 246).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_cust_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(709, 158).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Name = "m_cust_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 246).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_cust_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:Name = "m_cust_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(717, 158).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_cust_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 404).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Name = "m_cust_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(717, 8).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* cust */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(717, 412).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:cust_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Name = "cust".
      THIS-OBJECT:Text = "cust".
      THIS-OBJECT:cust_Fill_Panel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID ShowModalDialog(  ):
      
      WAIT-FOR THIS-OBJECT:ShowDialog().

   END METHOD.
	DESTRUCTOR PUBLIC cust ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.