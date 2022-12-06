 
 /*------------------------------------------------------------------------
    File        : HuvMeny
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Mar 01 10:14:40 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS Start.HuvMeny INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE m_HuvMeny_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_HuvMeny_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_HuvMeny_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_HuvMeny_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvMeny_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PRIVATE VARIABLE TabShared AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE tabPage2 AS System.Windows.Forms.TabPage NO-UNDO.
   DEFINE PRIVATE VARIABLE tabPage1 AS System.Windows.Forms.TabPage NO-UNDO.
   DEFINE PRIVATE VARIABLE tabControl1 AS System.Windows.Forms.TabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

		
	CONSTRUCTOR PUBLIC HuvMeny (  ):
		
		
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Start.HuvMeny").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("Guru").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
      appearance43 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab3 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab3 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon3").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ok").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("cancel").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup4 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("ComboBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ok").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
      appearance41 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("cancel").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
      appearance42 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("ComboBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
      valueList1 = NEW Infragistics.Win.ValueList(0).
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:tabControl1 = NEW System.Windows.Forms.TabControl().
      THIS-OBJECT:tabPage1 = NEW System.Windows.Forms.TabPage().
      THIS-OBJECT:tabPage2 = NEW System.Windows.Forms.TabPage().
      THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:TabShared = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:HuvMeny_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:tabControl1:SuspendLayout().
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabControl1:SuspendLayout().
      THIS-OBJECT:HuvMeny_Fill_Panel:SuspendLayout().
      THIS-OBJECT:ultraPanel1:SuspendLayout().
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(401, 165).
      /*  */
      /* ultraTabPageControl2 */
      /*  */
      THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2".
      THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(401, 165).
      /*  */
      /* tabControl1 */
      /*  */
      THIS-OBJECT:tabControl1:Controls:Add(THIS-OBJECT:tabPage1).
      THIS-OBJECT:tabControl1:Controls:Add(THIS-OBJECT:tabPage2).
      THIS-OBJECT:tabControl1:Location = NEW System.Drawing.Point(12, 12).
      THIS-OBJECT:tabControl1:Name = "tabControl1".
      THIS-OBJECT:tabControl1:SelectedIndex = 0.
      THIS-OBJECT:tabControl1:Size = NEW System.Drawing.Size(402, 231).
      THIS-OBJECT:tabControl1:TabIndex = 0.
      /*  */
      /* tabPage1 */
      /*  */
      THIS-OBJECT:tabPage1:BackgroundImage = CAST(resources:GetObject("tabPage1.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:tabPage1:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:tabPage1:Location = NEW System.Drawing.Point(4, 22).
      THIS-OBJECT:tabPage1:Name = "tabPage1".
      THIS-OBJECT:tabPage1:Padding = NEW System.Windows.Forms.Padding(3).
      THIS-OBJECT:tabPage1:Size = NEW System.Drawing.Size(394, 205).
      THIS-OBJECT:tabPage1:TabIndex = 0.
      THIS-OBJECT:tabPage1:Text = "guru".
      THIS-OBJECT:tabPage1:UseVisualStyleBackColor = TRUE.
      /*  */
      /* tabPage2 */
      /*  */
      THIS-OBJECT:tabPage2:BackgroundImage = CAST(resources:GetObject("tabPage2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:tabPage2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:tabPage2:Location = NEW System.Drawing.Point(4, 22).
      THIS-OBJECT:tabPage2:Name = "tabPage2".
      THIS-OBJECT:tabPage2:Padding = NEW System.Windows.Forms.Padding(3).
      THIS-OBJECT:tabPage2:Size = NEW System.Drawing.Size(394, 205).
      THIS-OBJECT:tabPage2:TabIndex = 1.
      THIS-OBJECT:tabPage2:Text = "aonrh".
      THIS-OBJECT:tabPage2:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:tabPage2:Click:Subscribe(THIS-OBJECT:tabPage2_Click).
      /*  */
      /* ultraTabControl1 */
      /*  */
      THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:TabShared).
      THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
      THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
      THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(16, 249).
      THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1".
      THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:TabShared.
      THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(405, 191).
      THIS-OBJECT:ultraTabControl1:TabIndex = 1.
      appearance10:Image = CAST(resources:GetObject("appearance10.Image"), System.Object).
      ultraTab3:ActiveAppearance = appearance10.
      appearance3:BackColor = System.Drawing.Color:Red.
      ultraTab3:Appearance = appearance3.
      appearance6:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128))).
      ultraTab3:ClientAreaAppearance = appearance6.
      ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl1.
      ultraTab3:Text = "gurh".
      appearance8:ImageBackground = CAST(resources:GetObject("appearance8.ImageBackground"), System.Drawing.Image).
      ultraTab4:Appearance = appearance8.
      ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl2.
      ultraTab4:Text = "aonr".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar0[1] = ultraTab3.
      arrayvar0[2] = ultraTab4.
      THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
      /*  */
      /* TabShared */
      /*  */
      THIS-OBJECT:TabShared:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabShared:Name = "TabShared".
      THIS-OBJECT:TabShared:Size = NEW System.Drawing.Size(401, 165).
      THIS-OBJECT:TabShared:Paint:Subscribe(THIS-OBJECT:ultraTabSharedControlsPage1_Paint).
      /*  */
      /* HuvMeny_Fill_Panel */
      /*  */
      THIS-OBJECT:HuvMeny_Fill_Panel:Controls:Add(THIS-OBJECT:ultraPanel1).
      THIS-OBJECT:HuvMeny_Fill_Panel:Controls:Add(THIS-OBJECT:ultraTabControl1).
      THIS-OBJECT:HuvMeny_Fill_Panel:Controls:Add(THIS-OBJECT:tabControl1).
      THIS-OBJECT:HuvMeny_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:HuvMeny_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:HuvMeny_Fill_Panel:Location = NEW System.Drawing.Point(4, 148).
      THIS-OBJECT:HuvMeny_Fill_Panel:Name = "HuvMeny_Fill_Panel".
      THIS-OBJECT:HuvMeny_Fill_Panel:Size = NEW System.Drawing.Size(571, 476).
      THIS-OBJECT:HuvMeny_Fill_Panel:TabIndex = 0.
      /*  */
      /* ultraPanel1 */
      /*  */
      THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraPanel1:Name = "ultraPanel1".
      THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(571, 476).
      THIS-OBJECT:ultraPanel1:TabIndex = 2.
      /*  */
      /* m_HuvMeny_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 4.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(575, 148).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:Name = "m_HuvMeny_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(4, 476).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* ultraToolbarsManager1 */
      /*  */
      THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
      THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
      THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
      THIS-OBJECT:ultraToolbarsManager1:FormDisplayStyle = Infragistics.Win.UltraWinToolbars.FormDisplayStyle:RoundedSizable.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar1[1] = buttonTool5.
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenu:ToolAreaLeft:NonInheritedTools:AddRange(arrayvar1).
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage"), System.Drawing.Image).
      ribbonTab1:Caption = "".
      ribbonGroup1:Caption = "ribbonGroup1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar2[1] = ribbonGroup1.
      ribbonTab1:Groups:AddRange(arrayvar2).
      appearance43:Image = CAST(resources:GetObject("appearance43.Image"), System.Object).
      ribbonTab1:Settings:TabItemAppearance = appearance43.
      ribbonTab2:Caption = "ribbon2".
      ribbonGroup2:Caption = "ribbonGroup1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar3[1] = ribbonGroup2.
      ribbonTab2:Groups:AddRange(arrayvar3).
      ribbonTab3:Caption = "".
      ribbonGroup3:Caption = "ribbonGroup1".
      buttonTool2:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar4[1] = buttonTool1.
      arrayvar4[2] = buttonTool2.
      ribbonGroup3:Tools:AddRange(arrayvar4).
      ribbonGroup4:Caption = "ribbonGroup2".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar5[1] = comboBoxTool1.
      ribbonGroup4:Tools:AddRange(arrayvar5).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 2 NO-UNDO.
      arrayvar6[1] = ribbonGroup3.
      arrayvar6[2] = ribbonGroup4.
      ribbonTab3:Groups:AddRange(arrayvar6).
      appearance17:Image = CAST(resources:GetObject("appearance17.Image"), System.Object).
      appearance17:ImageBackgroundDisabled = CAST(resources:GetObject("appearance17.ImageBackgroundDisabled"), System.Drawing.Image).
      ribbonTab3:Settings:ActiveTabItemAppearance = appearance17.
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      ribbonTab3:Settings:Appearance = appearance15.
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      ribbonTab3:Settings:TabItemAppearance = appearance16.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 3 NO-UNDO.
      arrayvar7[1] = ribbonTab1.
      arrayvar7[2] = ribbonTab2.
      arrayvar7[3] = ribbonTab3.
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar7).
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
      appearance41:Image = CAST(resources:GetObject("appearance41.Image"), System.Object).
      buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance41.
      buttonTool3:SharedPropsInternal:Caption = "ok".
      appearance42:Image = CAST(resources:GetObject("appearance42.Image"), System.Object).
      buttonTool4:SharedPropsInternal:AppearancesLarge:Appearance = appearance42.
      buttonTool4:SharedPropsInternal:Caption = "cancel".
      buttonTool6:SharedPropsInternal:Caption = "ButtonTool1".
      comboBoxTool2:SharedPropsInternal:Caption = "ComboBoxTool1".
      comboBoxTool2:ValueList = valueList1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar8 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
      arrayvar8[1] = buttonTool3.
      arrayvar8[2] = buttonTool4.
      arrayvar8[3] = buttonTool6.
      arrayvar8[4] = comboBoxTool2.
      THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar8).
      /*  */
      /* m_HuvMeny_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 4.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 148).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:Name = "m_HuvMeny_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(4, 476).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_HuvMeny_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 4.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 624).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:Name = "m_HuvMeny_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(579, 4).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_HuvMeny_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:Name = "m_HuvMeny_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(579, 148).
      THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* HuvMeny */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(579, 628).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:HuvMeny_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_HuvMeny_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Name = "HuvMeny".
      THIS-OBJECT:Text = "Elpool i Umeå AB ".
      THIS-OBJECT:tabControl1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
      THIS-OBJECT:HuvMeny_Fill_Panel:ResumeLayout(FALSE).
      THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID tabPage2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraTabSharedControlsPage1_Paint( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.PaintEventArgs ):
		
		RETURN.

	END METHOD.



	DESTRUCTOR PUBLIC HuvMeny ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.