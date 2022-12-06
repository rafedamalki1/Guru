 /*------------------------------------------------------------------------
    File        : cust
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu Nov 29 16:28:37 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
USING Progress.Lang.*.
USING Guru.Module.  

CLASS  Modules.Kalkyl.cust INHERITS Form   : 

    DEFINE PRIVATE VARIABLE grid1 AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_cust_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonCancel AS System.Windows.Forms.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   DEFINE PRIVATE VARIABLE cust_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
    
	CONSTRUCTOR PUBLIC cust( ):
	
		SUPER().
		InitializeComponent ( ).
		CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC cust( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.	

    METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
		THIS-OBJECT:Close ( ). 
	END METHOD.

	METHOD PRIVATE VOID cancelButton_Click (sender AS System.Object, e AS System.EventArgs):
		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel.
		THIS-OBJECT:Close ( ).
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.cust").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("nyar").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("spara").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ångra").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab2 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab2 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("spara").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ångra").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      THIS-OBJECT:buttonOk = NEW System.Windows.Forms.Button().
      THIS-OBJECT:buttonCancel = NEW System.Windows.Forms.Button().
      THIS-OBJECT:cust_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:grid1 = NEW Controls.Grid().
      THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:cust_Fill_Panel:SuspendLayout().
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabControl1:SuspendLayout().
      CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* buttonOk */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
      THIS-OBJECT:buttonOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:buttonOk:Location = NEW System.Drawing.Point(627, 109).
      THIS-OBJECT:buttonOk:Name = "buttonOk".
      THIS-OBJECT:buttonOk:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonOk:TabIndex = 0.
      THIS-OBJECT:buttonOk:Text = "OK".
      THIS-OBJECT:buttonOk:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:buttonOk:Click:Subscribe(THIS-OBJECT:okButton_Click).
      /*  */
      /* buttonCancel */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
      THIS-OBJECT:buttonCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:buttonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
      THIS-OBJECT:buttonCancel:Location = NEW System.Drawing.Point(711, 263).
      THIS-OBJECT:buttonCancel:Name = "buttonCancel".
      THIS-OBJECT:buttonCancel:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonCancel:TabIndex = 1.
      THIS-OBJECT:buttonCancel:Text = "Cancel".
      THIS-OBJECT:buttonCancel:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:buttonCancel:Click:Subscribe(THIS-OBJECT:cancelButton_Click).
      /*  */
      /* cust_Fill_Panel */
      /*  */
      THIS-OBJECT:cust_Fill_Panel:Controls:Add(THIS-OBJECT:grid1).
      THIS-OBJECT:cust_Fill_Panel:Controls:Add(THIS-OBJECT:ultraTabControl1).
      THIS-OBJECT:cust_Fill_Panel:Controls:Add(THIS-OBJECT:buttonCancel).
      THIS-OBJECT:cust_Fill_Panel:Controls:Add(THIS-OBJECT:buttonOk).
      THIS-OBJECT:cust_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:cust_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:cust_Fill_Panel:Location = NEW System.Drawing.Point(8, 189).
      THIS-OBJECT:cust_Fill_Panel:Name = "cust_Fill_Panel".
      THIS-OBJECT:cust_Fill_Panel:Size = NEW System.Drawing.Size(800, 281).
      THIS-OBJECT:cust_Fill_Panel:TabIndex = 0.
      /*  */
      /* grid1 */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:grid1:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:grid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:grid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:grid1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:grid1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:grid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:grid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:grid1:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:grid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:grid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:grid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:grid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:grid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:grid1:GuruContext = ?.
      THIS-OBJECT:grid1:Location = NEW System.Drawing.Point(26, 63).
      THIS-OBJECT:grid1:Name = "grid1".
      THIS-OBJECT:grid1:Size = NEW System.Drawing.Size(550, 80).
      THIS-OBJECT:grid1:TabIndex = 3.
      THIS-OBJECT:grid1:Text = "grid1".
      /*  */
      /* ultraTabControl1 */
      /*  */
      THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(422, 63).
      THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1".
      THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(200, 100).
      THIS-OBJECT:ultraTabControl1:TabIndex = 2.
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
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ultraToolbarsManager1.Ribbon.ApplicationMenuButtonImage"), System.Drawing.Image).
      ribbonTab1:Caption = "nyar".
      ribbonGroup1:Caption = "ribbonGroup1".
      buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      buttonTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar0[1] = buttonTool1.
      arrayvar0[2] = buttonTool3.
      ribbonGroup1:Tools:AddRange(arrayvar0).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar1[1] = ribbonGroup1.
      ribbonTab1:Groups:AddRange(arrayvar1).
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      ribbonTab1:GroupSettings:Appearance = appearance13.
      ribbonTab2:Caption = "NU DU".
      ribbonGroup2:Caption = "ribbonGroup1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
      arrayvar2[1] = buttonTool5.
      ribbonGroup2:Tools:AddRange(arrayvar2).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar3[1] = ribbonGroup2.
      ribbonTab2:Groups:AddRange(arrayvar3).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 2 NO-UNDO.
      arrayvar4[1] = ribbonTab1.
      arrayvar4[2] = ribbonTab2.
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar4).
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:RibbonAreaAppearance = appearance14.
      THIS-OBJECT:ultraToolbarsManager1:Ribbon:Visible = TRUE.
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      buttonTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance15.
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      buttonTool2:SharedPropsInternal:AppearancesSmall:Appearance = appearance16.
      buttonTool2:SharedPropsInternal:Caption = "spara".
      appearance17:Image = CAST(resources:GetObject("appearance17.Image"), System.Object).
      buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance17.
      buttonTool4:SharedPropsInternal:Caption = "ångra".
      buttonTool6:SharedPropsInternal:Caption = "ButtonTool1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
      arrayvar5[1] = buttonTool2.
      arrayvar5[2] = buttonTool4.
      arrayvar5[3] = buttonTool6.
      THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar5).
      /*  */
      /* m_cust_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 189).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Name = "m_cust_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 281).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_cust_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(808, 189).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Name = "m_cust_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 281).
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
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(816, 189).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* m_cust_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 470).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Name = "m_cust_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(816, 8).
      THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
      /*  */
      /* cust */
      /*  */
      THIS-OBJECT:AcceptButton = THIS-OBJECT:buttonOk.
      THIS-OBJECT:CancelButton = THIS-OBJECT:buttonCancel.
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(816, 478).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:cust_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_cust_Toolbars_Dock_Area_Top).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "cust".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "cust".
      THIS-OBJECT:cust_Fill_Panel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):EndInit().
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


 
END CLASS.
