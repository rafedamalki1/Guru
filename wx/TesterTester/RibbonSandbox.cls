 
 /*------------------------------------------------------------------------
    File        : RibbonSandbox
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue Jun 19 12:34:05 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS RibbonSandbox INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE m_RibbonSandbox_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_RibbonSandbox_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_RibbonSandbox_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_RibbonSandbox_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE RibbonSandbox_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PRIVATE VARIABLE Ribbon AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraChart1 AS Infragistics.Win.UltraWinChart.UltraChart NO-UNDO.

		
	CONSTRUCTOR PUBLIC RibbonSandbox (  ):
		
		
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
      DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("vad är detta").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("vad").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbon1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool1 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool1 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Kalkylhuvud", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool2 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool2 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Anmärkning", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool3 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool3 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Utförlig", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool4 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool4 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Egna priser", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool5 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool5 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Faktorer", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generera ").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool3").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool3 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("Visa som").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool13 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool13 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Grupppera koder", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool14 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool14 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Summera koder", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("Material").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool11 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool11 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Materialspecifikation", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGroup3").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("LabelTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("LabelTool2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("TextBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTaskPaneToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraTaskPaneToolbar NO-UNDO.
      ultraTaskPaneToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraTaskPaneToolbar("UltraToolbar1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE taskPaneTool2 AS Infragistics.Win.UltraWinToolbars.TaskPaneTool NO-UNDO.
      taskPaneTool2 = NEW Infragistics.Win.UltraWinToolbars.TaskPaneTool("TaskPaneTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool6 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool6 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Kalkylhuvud", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool7 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool7 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Anmärkning", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool8 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool8 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Utförlig", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool9 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool9 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Egna priser", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool10 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool10 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Faktorer", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("RibbonSandbox").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generera").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("Material").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
      valueList1 = NEW Infragistics.Win.ValueList(0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool12 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool12 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Materialspecifikation", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool4 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool4 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool5 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool5 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Gruppera").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool6 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool6 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("Summera").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool15 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool15 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Grupppera koder", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE stateButtonTool16 AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.
      stateButtonTool16 = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Summera koder", "").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool4 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("Visa som").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueList2 AS Infragistics.Win.ValueList NO-UNDO.
      valueList2 = NEW Infragistics.Win.ValueList(0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Generera ").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool3").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE storbild11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      storbild11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("storbild").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("vad är detta").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("vad").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool3 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool3 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("LabelTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool7 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool7 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("LabelTool2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool2 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("TextBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE paintElement1 AS Infragistics.UltraChart.Resources.Appearance.PaintElement NO-UNDO.
      paintElement1 = NEW Infragistics.UltraChart.Resources.Appearance.PaintElement().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE gradientEffect1 AS Infragistics.UltraChart.Resources.Appearance.GradientEffect NO-UNDO.
      gradientEffect1 = NEW Infragistics.UltraChart.Resources.Appearance.GradientEffect().
      THIS-OBJECT:Ribbon = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:RibbonSandbox_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:ultraChart1 = NEW Infragistics.Win.UltraWinChart.UltraChart().
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      CAST(THIS-OBJECT:Ribbon, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:RibbonSandbox_Fill_Panel:SuspendLayout().
      CAST(THIS-OBJECT:ultraChart1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* Ribbon */
      /*  */
      THIS-OBJECT:Ribbon:DesignerFlags = 1.
      THIS-OBJECT:Ribbon:DockWithinContainer = THIS-OBJECT.
      THIS-OBJECT:Ribbon:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar0[1] = buttonTool12.
      arrayvar0[2] = buttonTool13.
      THIS-OBJECT:Ribbon:MiniToolbar:NonInheritedTools:AddRange(arrayvar0).
      THIS-OBJECT:Ribbon:MiniToolbar:ToolRowCount = 33.
      ribbonTab1:Caption = "Visning".
      ribbonGroup1:Caption = "Visa".
      stateButtonTool1:Checked = TRUE.
      stateButtonTool1:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool2:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool3:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool4:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool5:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
      stateButtonTool5:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
      stateButtonTool5:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      buttonTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
      buttonTool6:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      buttonTool7:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 9 NO-UNDO.
      arrayvar1[1] = stateButtonTool1.
      arrayvar1[2] = stateButtonTool2.
      arrayvar1[3] = stateButtonTool3.
      arrayvar1[4] = stateButtonTool4.
      arrayvar1[5] = stateButtonTool5.
      arrayvar1[6] = buttonTool5.
      arrayvar1[7] = buttonTool1.
      arrayvar1[8] = buttonTool6.
      arrayvar1[9] = buttonTool7.
      ribbonGroup1:Tools:AddRange(arrayvar1).
      ribbonGroup2:Caption = "Egenskaper".
      stateButtonTool13:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool14:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool11:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 5 NO-UNDO.
      arrayvar2[1] = comboBoxTool3.
      arrayvar2[2] = stateButtonTool13.
      arrayvar2[3] = stateButtonTool14.
      arrayvar2[4] = comboBoxTool1.
      arrayvar2[5] = stateButtonTool11.
      ribbonGroup2:Tools:AddRange(arrayvar2).
      ribbonGroup3:Caption = "ribbonGroup3".
      textBoxTool1:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      textBoxTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 3 NO-UNDO.
      arrayvar3[1] = labelTool1.
      arrayvar3[2] = labelTool2.
      arrayvar3[3] = textBoxTool1.
      ribbonGroup3:Tools:AddRange(arrayvar3).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
      arrayvar4[1] = ribbonGroup1.
      arrayvar4[2] = ribbonGroup2.
      arrayvar4[3] = ribbonGroup3.
      ribbonTab1:Groups:AddRange(arrayvar4).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
      arrayvar5[1] = ribbonTab1.
      THIS-OBJECT:Ribbon:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar5).
      THIS-OBJECT:Ribbon:Ribbon:Visible = TRUE.
      THIS-OBJECT:Ribbon:ShowFullMenusDelay = 500.
      ultraTaskPaneToolbar1:DockedColumn = 0.
      ultraTaskPaneToolbar1:DockedRow = 0.
      ultraTaskPaneToolbar1:Text = "UltraToolbar1".
      ultraTaskPaneToolbar1:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
      arrayvar6[1] = ultraTaskPaneToolbar1.
      THIS-OBJECT:Ribbon:Toolbars:AddRange(arrayvar6).
      taskPaneTool2:SharedPropsInternal:Caption = "TaskPaneTool1".
      stateButtonTool6:Checked = TRUE.
      stateButtonTool6:SharedPropsInternal:Caption = "Kalkylhuvud".
      stateButtonTool6:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool7:SharedPropsInternal:Caption = "Anmärkning".
      stateButtonTool7:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool8:SharedPropsInternal:Caption = "Utförlig".
      stateButtonTool8:SharedPropsInternal:Enabled = FALSE.
      stateButtonTool8:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool9:SharedPropsInternal:Caption = "Egna priser".
      stateButtonTool9:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
      stateButtonTool10:SharedPropsInternal:AppearancesLarge:Appearance = appearance1.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      stateButtonTool10:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
      stateButtonTool10:SharedPropsInternal:Caption = "Faktorer".
      stateButtonTool10:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      buttonTool2:SharedPropsInternal:Caption = "Generera".
      comboBoxTool2:SharedPropsInternal:Caption = "Priser".
      comboBoxTool2:ValueList = valueList1.
      stateButtonTool12:SharedPropsInternal:Caption = "Materialspecifikation".
      stateButtonTool12:SharedPropsInternal:Enabled = FALSE.
      stateButtonTool12:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      labelTool5:SharedPropsInternal:Caption = "Gruppera".
      labelTool6:SharedPropsInternal:Caption = "Summera".
      stateButtonTool15:SharedPropsInternal:Caption = "Gruppera koder".
      stateButtonTool15:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      stateButtonTool16:SharedPropsInternal:Caption = "Summera koder".
      stateButtonTool16:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      comboBoxTool4:SharedPropsInternal:Caption = "Visa som".
      comboBoxTool4:ValueList = valueList2.
      buttonTool4:SharedPropsInternal:Caption = "Generera ".
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
      buttonTool3:SharedPropsInternal:Caption = "anders".
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance4.
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
      buttonTool8:SharedPropsInternal:Caption = "anders2".
      appearance6:Image = CAST(resources:GetObject("appearance6.Image"), System.Object).
      buttonTool9:SharedPropsInternal:AppearancesLarge:Appearance = appearance6.
      buttonTool9:SharedPropsInternal:Caption = "andersl3".
      appearance7:Image = CAST(resources:GetObject("appearance7.Image"), System.Object).
      appearance7:ImageBackground = CAST(resources:GetObject("appearance7.ImageBackground"), System.Drawing.Image).
      appearance7:ImageBackgroundOrigin = Infragistics.Win.ImageBackgroundOrigin:Form.
      appearance7:ImageHAlign = Infragistics.Win.HAlign:Left.
      appearance7:ImageVAlign = Infragistics.Win.VAlign:Middle.
      storbild11:SharedPropsInternal:AppearancesLarge:Appearance = appearance7.
      buttonTool14:SharedPropsInternal:Caption = "vad är detta".
      buttonTool15:SharedPropsInternal:Caption = "vad".
      appearance8:Image = CAST(resources:GetObject("appearance8.Image"), System.Object).
      labelTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
      labelTool3:SharedPropsInternal:Caption = "LabelTool1".
      labelTool3:SharedPropsInternal:CustomizerCaption = "ANDERS".
      labelTool7:SharedPropsInternal:Caption = "LabelTool2".
      labelTool7:SharedPropsInternal:CustomizerCaption = "AS".
      appearance9:Image = CAST(resources:GetObject("appearance9.Image"), System.Object).
      textBoxTool2:SharedPropsInternal:AppearancesLarge:Appearance = appearance9.
      textBoxTool2:SharedPropsInternal:Caption = "TextBoxTool1".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 25 NO-UNDO.
      arrayvar7[1] = taskPaneTool2.
      arrayvar7[2] = stateButtonTool6.
      arrayvar7[3] = stateButtonTool7.
      arrayvar7[4] = stateButtonTool8.
      arrayvar7[5] = stateButtonTool9.
      arrayvar7[6] = stateButtonTool10.
      arrayvar7[7] = buttonTool2.
      arrayvar7[8] = comboBoxTool2.
      arrayvar7[9] = stateButtonTool12.
      arrayvar7[10] = labelTool4.
      arrayvar7[11] = labelTool5.
      arrayvar7[12] = labelTool6.
      arrayvar7[13] = stateButtonTool15.
      arrayvar7[14] = stateButtonTool16.
      arrayvar7[15] = comboBoxTool4.
      arrayvar7[16] = buttonTool4.
      arrayvar7[17] = buttonTool3.
      arrayvar7[18] = buttonTool8.
      arrayvar7[19] = buttonTool9.
      arrayvar7[20] = storbild11.
      arrayvar7[21] = buttonTool14.
      arrayvar7[22] = buttonTool15.
      arrayvar7[23] = labelTool3.
      arrayvar7[24] = labelTool7.
      arrayvar7[25] = textBoxTool2.
      THIS-OBJECT:Ribbon:Tools:AddRange(arrayvar7).
      THIS-OBJECT:Ribbon:ToolClick:Subscribe(THIS-OBJECT:Ribbon_ToolClick).
      /*  */
      /* RibbonSandbox_Fill_Panel */
      /*  */
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Controls:Add(THIS-OBJECT:ultraChart1).
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Location = NEW System.Drawing.Point(8, 189).
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Name = "RibbonSandbox_Fill_Panel".
      THIS-OBJECT:RibbonSandbox_Fill_Panel:Size = NEW System.Drawing.Size(971, 338).
      THIS-OBJECT:RibbonSandbox_Fill_Panel:TabIndex = 0.
      /*  */
      /* ultraChart1 */
      /*  */
      THIS-OBJECT:ultraChart1:Axis:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(248)), System.Convert:ToInt32(System.Convert:ToByte(220))).
      paintElement1:ElementType = Infragistics.UltraChart.Shared.Styles.PaintElementType:None.
      paintElement1:Fill = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(248)), System.Convert:ToInt32(System.Convert:ToByte(220))).
      THIS-OBJECT:ultraChart1:Axis:PE = paintElement1.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:X:Labels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:ItemFormatString = "<ITEM_LABEL>".
      THIS-OBJECT:ultraChart1:Axis:X:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:X:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:X:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Far.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:ItemFormatString = "<ITEM_LABEL>".
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:X2:Labels:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:X2:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:X2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:X2:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:HorizontalAlign = System.Drawing.StringAlignment:Far.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:ItemFormatString = "<DATA_VALUE:00.##>".
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Y:TickmarkInterval = Progress.Util.CastUtil:ToDouble("40").
      THIS-OBJECT:ultraChart1:Axis:Y:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:Y:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:ItemFormatString = "<DATA_VALUE:00.##>".
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Y2:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Y2:TickmarkInterval = Progress.Util.CastUtil:ToDouble("40").
      THIS-OBJECT:ultraChart1:Axis:Y2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:Y2:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:ItemFormatString = "".
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Z:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:Z:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:ItemFormatString = "".
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", Progress.Util.CastUtil:ToSingle(7)).
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
      THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Z2:LineThickness = 1.
      THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
      THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:Visible = TRUE.
      THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
      THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:Color = System.Drawing.Color:LightGray.
      THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
      THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:Visible = FALSE.
      THIS-OBJECT:ultraChart1:Axis:Z2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
      THIS-OBJECT:ultraChart1:Axis:Z2:Visible = FALSE.
      THIS-OBJECT:ultraChart1:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
      THIS-OBJECT:ultraChart1:ColorModel:AlphaLevel = System.Convert:ToByte(150).
      THIS-OBJECT:ultraChart1:ColorModel:ColorBegin = System.Drawing.Color:Pink.
      THIS-OBJECT:ultraChart1:ColorModel:ColorEnd = System.Drawing.Color:DarkRed.
      THIS-OBJECT:ultraChart1:ColorModel:ModelStyle = Infragistics.UltraChart.Shared.Styles.ColorModels:CustomLinear.
      THIS-OBJECT:ultraChart1:Effects:Effects:Add(gradientEffect1).
      THIS-OBJECT:ultraChart1:Location = NEW System.Drawing.Point(291, 63).
      THIS-OBJECT:ultraChart1:Name = "ultraChart1".
      THIS-OBJECT:ultraChart1:Size = NEW System.Drawing.Size(400, 300).
      THIS-OBJECT:ultraChart1:TabIndex = 0.
      THIS-OBJECT:ultraChart1:Tooltips:HighlightFillColor = System.Drawing.Color:DimGray.
      THIS-OBJECT:ultraChart1:Tooltips:HighlightOutlineColor = System.Drawing.Color:DarkGray.
      /*  */
      /* m_RibbonSandbox_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 189).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:Name = "m_RibbonSandbox_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 338).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:Ribbon.
      /*  */
      /* m_RibbonSandbox_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(979, 189).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:Name = "m_RibbonSandbox_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 338).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:Ribbon.
      /*  */
      /* m_RibbonSandbox_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:Name = "m_RibbonSandbox_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(987, 189).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:Ribbon.
      /*  */
      /* m_RibbonSandbox_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 527).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:Name = "m_RibbonSandbox_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(987, 8).
      THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:Ribbon.
      /*  */
      /* RibbonSandbox */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(987, 535).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:RibbonSandbox_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_RibbonSandbox_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Name = "RibbonSandbox".
      THIS-OBJECT:Text = "RibbonSandbox".
      CAST(THIS-OBJECT:Ribbon, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:RibbonSandbox_Fill_Panel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraChart1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID Ribbon_ToolClick( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):
		
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC RibbonSandbox ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.