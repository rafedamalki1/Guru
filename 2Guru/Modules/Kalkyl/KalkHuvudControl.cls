 
 /*------------------------------------------------------------------------
    File        : KalkHuvudControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed May 07 15:29:45 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.


CLASS Modules.Kalkyl.KalkHuvudControl INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE BestKund AS Controls.Combo NO-UNDO.
	DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE comboPriserUppf AS Controls.ComboSimpleLabel NO-UNDO.
	
   DEFINE PUBLIC VARIABLE LabelBest AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerHuvPerslista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerHuv AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE UtfardareBort AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE LabelUtf AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE LabelOmr AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE LabelKatalog AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE LabelAnm AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudUtfardare AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudTyp AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudOmrade AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudKatalog AS Controls.ComboSimple NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudKalknr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudBenamning AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudAnsvarig AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudAnmarkning AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE GridPersonal AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
		
	CONSTRUCTOR PUBLIC KalkHuvudControl ( INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      
      THIS-OBJECT:CreateForm().
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID BestKund_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:SparaComboV(BestKund).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridPersonal_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID HuvudKatalog_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkHuvudControl").
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
      THIS-OBJECT:splitContainerHuv = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LabelBest = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:BestKund = NEW Controls.Combo().
      THIS-OBJECT:UtfardareBort = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      
      THIS-OBJECT:comboPriserUppf = NEW Controls.ComboSimpleLabel().
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
      THIS-OBJECT:HuvudTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:LabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:splitContainerHuvPerslista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridPersonal = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerHuv, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHuv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerHuv:SuspendLayout().
      CAST(THIS-OBJECT:BestKund, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:UtfardareBort, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerHuvPerslista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerHuvPerslista:SuspendLayout().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerHuv */
      /*  */
      THIS-OBJECT:splitContainerHuv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerHuv:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerHuv:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerHuv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerHuv:Name = "splitContainerHuv".
      /*  */
      /* splitContainerHuv.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelBest).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:BestKund).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:UtfardareBort).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:comboPriserUppf).
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
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:HuvudTyp).
      THIS-OBJECT:splitContainerHuv:Panel1:Controls:Add(THIS-OBJECT:LabelKatalog).
      /*  */
      /* splitContainerHuv.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerHuv:Panel2:Controls:Add(THIS-OBJECT:splitContainerHuvPerslista).
      THIS-OBJECT:splitContainerHuv:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerHuv:SplitterDistance = 450.
      THIS-OBJECT:splitContainerHuv:TabIndex = 44.
      /*  */
      /* LabelBest */
      /*  */
      appearance1:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelBest:Appearance = appearance1.
      THIS-OBJECT:LabelBest:Location = NEW System.Drawing.Point(13, 63).
      THIS-OBJECT:LabelBest:Name = "LabelBest".
      THIS-OBJECT:LabelBest:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelBest:TabIndex = 46.
      /*  */
      /* BestKund */
      /*  */
      appearance2:BackColor = System.Drawing.SystemColors:Window.
      appearance2:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:BestKund:DisplayLayout:Appearance = appearance2.
      THIS-OBJECT:BestKund:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:BestKund:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance3:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance3:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance3:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance3:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:BestKund:DisplayLayout:GroupByBox:Appearance = appearance3.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:BestKund:DisplayLayout:GroupByBox:BandLabelAppearance = appearance4.
      THIS-OBJECT:BestKund:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance5:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance5:BackColor2 = System.Drawing.SystemColors:Control.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:BestKund:DisplayLayout:GroupByBox:PromptAppearance = appearance5.
      THIS-OBJECT:BestKund:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:BestKund:DisplayLayout:MaxRowScrollRegions = 1.
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:BestKund:DisplayLayout:Override:ActiveCellAppearance = appearance6.
      appearance7:BackColor = System.Drawing.SystemColors:Highlight.
      appearance7:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:BestKund:DisplayLayout:Override:ActiveRowAppearance = appearance7.
      THIS-OBJECT:BestKund:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:BestKund:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:BestKund:DisplayLayout:Override:CardAreaAppearance = appearance8.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:BestKund:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:BestKund:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:BestKund:DisplayLayout:Override:CellPadding = 0.
      appearance10:BackColor = System.Drawing.SystemColors:Control.
      appearance10:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance10:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance10:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance10:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:BestKund:DisplayLayout:Override:GroupByRowAppearance = appearance10.
      appearance11:TextHAlignAsString = "Left".
      THIS-OBJECT:BestKund:DisplayLayout:Override:HeaderAppearance = appearance11.
      THIS-OBJECT:BestKund:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:BestKund:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      appearance12:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:BestKund:DisplayLayout:Override:RowAppearance = appearance12.
      THIS-OBJECT:BestKund:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance13:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:BestKund:DisplayLayout:Override:TemplateAddRowAppearance = appearance13.
      THIS-OBJECT:BestKund:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:BestKund:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:BestKund:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:BestKund:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:BestKund:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:BestKund:Location = NEW System.Drawing.Point(97, 64).
      THIS-OBJECT:BestKund:Name = "BestKund".
      THIS-OBJECT:BestKund:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:BestKund:TabIndex = 45.
      THIS-OBJECT:BestKund:ValueChanged:Subscribe(THIS-OBJECT:BestKund_ValueChanged).
      /*  */
      /* UtfardareBort */
      /*  */
      THIS-OBJECT:UtfardareBort:Location = NEW System.Drawing.Point(97, 205).
      THIS-OBJECT:UtfardareBort:Name = "UtfardareBort".
      THIS-OBJECT:UtfardareBort:ReadOnly = TRUE.
      THIS-OBJECT:UtfardareBort:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:UtfardareBort:TabIndex = 44.
      THIS-OBJECT:UtfardareBort:ValueChanged:Subscribe(THIS-OBJECT:UtfardareBort_ValueChanged).
      /*  */
      /* comboPriserUppf */
      /*  */
      THIS-OBJECT:comboPriserUppf:Location = NEW System.Drawing.Point(8, 274).
      THIS-OBJECT:comboPriserUppf:Name = "comboPriserUppf".
      THIS-OBJECT:comboPriserUppf:Size = NEW System.Drawing.Size(366, 55).
      THIS-OBJECT:comboPriserUppf:TabIndex = 43.
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
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Appearance = appearance14.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance15:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance15:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance15:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance15:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:Appearance = appearance15.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BandLabelAppearance = appearance16.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance17:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance17:BackColor2 = System.Drawing.SystemColors:Control.
      appearance17:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:PromptAppearance = appearance17.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxRowScrollRegions = 1.
      appearance18:BackColor = System.Drawing.SystemColors:Window.
      appearance18:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveCellAppearance = appearance18.
      appearance19:BackColor = System.Drawing.SystemColors:Highlight.
      appearance19:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveRowAppearance = appearance19.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance20:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CardAreaAppearance = appearance20.
      appearance21:BorderColor = System.Drawing.Color:Silver.
      appearance21:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellAppearance = appearance21.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellPadding = 0.
      appearance22:BackColor = System.Drawing.SystemColors:Control.
      appearance22:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance22:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance22:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance22:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:GroupByRowAppearance = appearance22.
      appearance23:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderAppearance = appearance23.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance24:BackColor = System.Drawing.SystemColors:Window.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowAppearance = appearance24.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance25:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:TemplateAddRowAppearance = appearance25.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudUtfardare:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudUtfardare:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudUtfardare:Location = NEW System.Drawing.Point(97, 179).
      THIS-OBJECT:HuvudUtfardare:Name = "HuvudUtfardare".
      THIS-OBJECT:HuvudUtfardare:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudUtfardare:TabIndex = 41.
      THIS-OBJECT:HuvudUtfardare:ValueChanged:Subscribe(THIS-OBJECT:HuvudUtfardare_ValueChanged).
      /*  */
      /* HuvudAnmarkning */
      /*  */
      THIS-OBJECT:HuvudAnmarkning:AcceptsReturn = TRUE.
      THIS-OBJECT:HuvudAnmarkning:AcceptsTab = TRUE.
      THIS-OBJECT:HuvudAnmarkning:Location = NEW System.Drawing.Point(13, 365).
      THIS-OBJECT:HuvudAnmarkning:Multiline = TRUE.
      THIS-OBJECT:HuvudAnmarkning:Name = "HuvudAnmarkning".
      THIS-OBJECT:HuvudAnmarkning:Size = NEW System.Drawing.Size(361, 110).
      THIS-OBJECT:HuvudAnmarkning:TabIndex = 32.
      THIS-OBJECT:HuvudAnmarkning:Text = " ".
      /*  */
      /* LabelAnm */
      /*  */
      THIS-OBJECT:LabelAnm:Location = NEW System.Drawing.Point(13, 335).
      THIS-OBJECT:LabelAnm:Name = "LabelAnm".
      THIS-OBJECT:LabelAnm:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelAnm:TabIndex = 33.
      THIS-OBJECT:LabelAnm:Text = "Anmärkning".
      /*  */
      /* HuvudAnsvarig */
      /*  */
      THIS-OBJECT:HuvudAnsvarig:Location = NEW System.Drawing.Point(13, 230).
      THIS-OBJECT:HuvudAnsvarig:Name = "HuvudAnsvarig".
      THIS-OBJECT:HuvudAnsvarig:Rubrik = "GuruLabel".
      THIS-OBJECT:HuvudAnsvarig:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudAnsvarig:TabIndex = 40.
      /*  */
      /* HuvudKatalog */
      /*  */
      THIS-OBJECT:HuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:HuvudKatalog:Location = NEW System.Drawing.Point(97, 122).
      THIS-OBJECT:HuvudKatalog:Name = "HuvudKatalog".
      THIS-OBJECT:HuvudKatalog:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:HuvudKatalog:TabIndex = 42.
      THIS-OBJECT:HuvudKatalog:ValueChanged:Subscribe(THIS-OBJECT:HuvudKatalog_ValueChanged).
      /*  */
      /* LabelOmr */
      /*  */
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelOmr:Appearance = appearance26.
      THIS-OBJECT:LabelOmr:Location = NEW System.Drawing.Point(13, 40).
      THIS-OBJECT:LabelOmr:Name = "LabelOmr".
      THIS-OBJECT:LabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelOmr:TabIndex = 38.
      /*  */
      /* HuvudBenamning */
      /*  */
      
      THIS-OBJECT:HuvudBenamning:Location = NEW System.Drawing.Point(13, 147).
      THIS-OBJECT:HuvudBenamning:Name = "HuvudBenamning".
      THIS-OBJECT:HuvudBenamning:Rubrik = "GuruLabel".
      THIS-OBJECT:HuvudBenamning:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudBenamning:TabIndex = 35.
      THIS-OBJECT:HuvudBenamning:Tag = "".
      /*  */
      /* HuvudOmrade */
      /*  */
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudOmrade:Location = NEW System.Drawing.Point(97, 38).
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
      /* HuvudTyp */
      /*  */
      THIS-OBJECT:HuvudTyp:Location = NEW System.Drawing.Point(8, 90).
      THIS-OBJECT:HuvudTyp:Name = "HuvudTyp".
      THIS-OBJECT:HuvudTyp:Size = NEW System.Drawing.Size(366, 28).
      THIS-OBJECT:HuvudTyp:TabIndex = 36.
      THIS-OBJECT:HuvudTyp:GuruValueChanged:Subscribe(THIS-OBJECT:HuvudTyp_GuruValueChanged).
      /*  */
      /* LabelKatalog */
      /*  */
      appearance39:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelKatalog:Appearance = appearance39.
      THIS-OBJECT:LabelKatalog:Location = NEW System.Drawing.Point(13, 123).
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
      THIS-OBJECT:splitContainerHuvPerslista:Size = NEW System.Drawing.Size(691, 553).
      THIS-OBJECT:splitContainerHuvPerslista:SplitterDistance = 373.
      THIS-OBJECT:splitContainerHuvPerslista:TabIndex = 35.
      /*  */
      /* GridPersonal */
      /*  */
      appearance40:BackColor = System.Drawing.SystemColors:Window.
      appearance40:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPersonal:DisplayLayout:Appearance = appearance40.
      THIS-OBJECT:GridPersonal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPersonal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance41:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance41:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance41:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:Appearance = appearance41.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance42.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance43:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance43:BackColor2 = System.Drawing.SystemColors:Control.
      appearance43:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance43:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:PromptAppearance = appearance43.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance44:BackColor = System.Drawing.SystemColors:Window.
      appearance44:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveCellAppearance = appearance44.
      appearance45:BackColor = System.Drawing.SystemColors:Highlight.
      appearance45:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveRowAppearance = appearance45.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance46:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CardAreaAppearance = appearance46.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      appearance47:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellAppearance = appearance47.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellPadding = 0.
      appearance48:BackColor = System.Drawing.SystemColors:Control.
      appearance48:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance48:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance48:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance48:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:GroupByRowAppearance = appearance48.
      appearance49:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderAppearance = appearance49.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowAppearance = appearance50.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance51:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:TemplateAddRowAppearance = appearance51.
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
      THIS-OBJECT:GridPersonal:Size = NEW System.Drawing.Size(373, 553).
      THIS-OBJECT:GridPersonal:TabIndex = 34.
      THIS-OBJECT:GridPersonal:Text = "grid1".
      THIS-OBJECT:GridPersonal:DoubleClickRow:Subscribe(THIS-OBJECT:GridPersonal_DoubleClickRow).
      THIS-OBJECT:GridPersonal:Click:Subscribe(THIS-OBJECT:GridPersonal_Click).
      /*  */
      /* KalkHuvudControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerHuv).
      THIS-OBJECT:Name = "KalkHuvudControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerHuv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerHuv:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerHuv:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerHuv, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuv:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:BestKund, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:UtfardareBort, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuvPerslista:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerHuvPerslista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerHuvPerslista:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):EndInit().
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
   METHOD PRIVATE VOID GridPersonal_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
      THIS-OBJECT:HuvudAnsvarig:GuruText:Text = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudTyp_GuruValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:UpdateRibbonShow().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudOmrade_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:SparaComboV(HuvudOmrade).
      THIS-OBJECT:ControlShell:AddBestKund(STRING(THIS-OBJECT:HuvudOmrade:VALUE)).
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudUtfardare_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:SparaComboV(HuvudUtfardare).
      
      IF THIS-OBJECT:ControlShell:SokAnvUtfardare(STRING(THIS-OBJECT:HuvudUtfardare:VALUE)) = FALSE THEN DO:
         THIS-OBJECT:UtfardareBort:VISIBLE     = TRUE.
      END.   
      ELSE DO:
         THIS-OBJECT:UtfardareBort:VISIBLE     = FALSE.
      END.
      RETURN.

   END METHOD.
	
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID UtfardareBort_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      RETURN.

   END METHOD.
   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      

   END METHOD. 
	DESTRUCTOR PUBLIC KalkHuvudControl ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.