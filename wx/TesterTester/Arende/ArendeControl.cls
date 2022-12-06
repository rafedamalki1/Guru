 
 /*------------------------------------------------------------------------
    File        : KalkylControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 11:23:05 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Arende.ArendeControl INHERITS UserControl: 
   {ARENDEKATH.i}
	DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Arende.ArendeShell                                       NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopInAntal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkKodoFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkFri AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkArbkoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalk AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInLop AS Controls.FillinMask NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInKod AS Controls.FillinMask NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInAntal AS Controls.FillinDecimal NO-UNDO.
   DEFINE PUBLIC VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE Rubrikanm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylensKoderAnmarkning AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKalkylKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbetsKoder AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC ArendeControl (INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell.
      
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:GomSplit().
      {ARENDEKATDBH.i Root:DatabaseManager:Arende}
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   METHOD PUBLIC VOID GomSplit():
      
      THIS-OBJECT:splitContainerKalkFri:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerKalkBild:Panel2Collapsed = false.
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeControl").
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
      THIS-OBJECT:splitContainerKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkArbkoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbetsKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopInAntal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridLopKoder = NEW Controls.Grid().
      THIS-OBJECT:KalkylButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:fillinFriInKod = NEW Controls.FillinMask().
      THIS-OBJECT:fillinFriInAntal = NEW Controls.FillinDecimal().
      THIS-OBJECT:fillinFriInLop = NEW Controls.FillinMask().
      THIS-OBJECT:splitContainerKalkFri = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKodoFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKalkylKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:Rubrikanm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:KalkylensKoderAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:SuspendLayout().
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopInAntal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopInAntal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopInAntal:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopInAntal:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:SuspendLayout().
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:panel1:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkFri:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:SuspendLayout().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKalk */
      /*  */
      THIS-OBJECT:splitContainerKalk:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalk:Name = "splitContainerKalk".
      /*  */
      /* splitContainerKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkArbkoder).
      /*  */
      /* splitContainerKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkFri).
      THIS-OBJECT:splitContainerKalk:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerKalk:SplitterDistance = 291.
      THIS-OBJECT:splitContainerKalk:TabIndex = 12.
      /*  */
      /* splitContainerKalkArbkoder */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkArbkoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkArbkoder:Name = "splitContainerKalkArbkoder".
      THIS-OBJECT:splitContainerKalkArbkoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkArbkoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:Controls:Add(THIS-OBJECT:GridArbetsKoder).
      /*  */
      /* splitContainerKalkArbkoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopInAntal).
      THIS-OBJECT:splitContainerKalkArbkoder:Size = NEW System.Drawing.Size(289, 544).
      THIS-OBJECT:splitContainerKalkArbkoder:SplitterDistance = 248.
      THIS-OBJECT:splitContainerKalkArbkoder:TabIndex = 0.
      /*  */
      /* GridArbetsKoder */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbetsKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbetsKoder:GuruContext = ?.
      THIS-OBJECT:GridArbetsKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbetsKoder:Name = "GridArbetsKoder".
      THIS-OBJECT:GridArbetsKoder:Size = NEW System.Drawing.Size(289, 248).
      THIS-OBJECT:GridArbetsKoder:TabIndex = 4.
      THIS-OBJECT:GridArbetsKoder:Text = "grid1".
      /*  */
      /* splitContainerLopInAntal */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopInAntal:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerLopInAntal:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopInAntal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopInAntal:Name = "splitContainerLopInAntal".
      THIS-OBJECT:splitContainerLopInAntal:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopInAntal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkLopnr).
      /*  */
      /* splitContainerLopInAntal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Panel2:Controls:Add(THIS-OBJECT:panel1).
      THIS-OBJECT:splitContainerLopInAntal:Size = NEW System.Drawing.Size(289, 292).
      THIS-OBJECT:splitContainerLopInAntal:SplitterDistance = 217.
      THIS-OBJECT:splitContainerLopInAntal:TabIndex = 1.
      /*  */
      /* splitContainerKalkLopnr */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkLopnr:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerKalkLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKalkLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkLopnr:Name = "splitContainerKalkLopnr".
      /*  */
      /* splitContainerKalkLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:Controls:Add(THIS-OBJECT:GridLopKoder).
      /*  */
      /* splitContainerKalkLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonOverMark).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonBackMark).
      THIS-OBJECT:splitContainerKalkLopnr:Size = NEW System.Drawing.Size(289, 217).
      THIS-OBJECT:splitContainerKalkLopnr:SplitterDistance = 251.
      THIS-OBJECT:splitContainerKalkLopnr:TabIndex = 1.
      /*  */
      /* GridLopKoder */
      /*  */
      THIS-OBJECT:GridLopKoder:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridLopKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopKoder:GuruContext = ?.
      THIS-OBJECT:GridLopKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopKoder:Name = "GridLopKoder".
      THIS-OBJECT:GridLopKoder:Size = NEW System.Drawing.Size(251, 217).
      THIS-OBJECT:GridLopKoder:TabIndex = 5.
      THIS-OBJECT:GridLopKoder:Text = "grid2".
      /*  */
      /* KalkylButtonOverMark */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:KalkylButtonOverMark:Appearance = appearance25.
      THIS-OBJECT:KalkylButtonOverMark:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:KalkylButtonOverMark:Name = "KalkylButtonOverMark".
      THIS-OBJECT:KalkylButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:KalkylButtonOverMark:TabIndex = 7.
      THIS-OBJECT:KalkylButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:KalkylButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOvermark_ClickE).
      /*  */
      /* KalkylButtonBackMark */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:KalkylButtonBackMark:Appearance = appearance26.
      THIS-OBJECT:KalkylButtonBackMark:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:KalkylButtonBackMark:Name = "KalkylButtonBackMark".
      THIS-OBJECT:KalkylButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:KalkylButtonBackMark:TabIndex = 9.
      THIS-OBJECT:KalkylButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:KalkylButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMark_Click).
      /*  */
      /* panel1 */
      /*  */
      THIS-OBJECT:panel1:BackColor = System.Drawing.Color:AliceBlue.
      THIS-OBJECT:panel1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:fillinFriInKod).
      THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:fillinFriInAntal).
      THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:fillinFriInLop).
      THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:panel1:Name = "panel1".
      THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(289, 71).
      THIS-OBJECT:panel1:TabIndex = 3.
      /*  */
      /* fillinFriInKod */
      /*  */
      THIS-OBJECT:fillinFriInKod:Location = NEW System.Drawing.Point(20, 5).
      THIS-OBJECT:fillinFriInKod:Name = "fillinFriInKod".
      THIS-OBJECT:fillinFriInKod:Size = NEW System.Drawing.Size(120, 28).
      THIS-OBJECT:fillinFriInKod:TabIndex = 0.
      /*  */
      /* fillinFriInAntal */
      /*  */
      THIS-OBJECT:fillinFriInAntal:Location = NEW System.Drawing.Point(20, 34).
      THIS-OBJECT:fillinFriInAntal:Name = "fillinFriInAntal".
      THIS-OBJECT:fillinFriInAntal:Size = NEW System.Drawing.Size(261, 28).
      THIS-OBJECT:fillinFriInAntal:TabIndex = 2.
      /*  */
      /* fillinFriInLop */
      /*  */
      THIS-OBJECT:fillinFriInLop:Location = NEW System.Drawing.Point(146, 5).
      THIS-OBJECT:fillinFriInLop:Name = "fillinFriInLop".
      THIS-OBJECT:fillinFriInLop:Size = NEW System.Drawing.Size(136, 28).
      THIS-OBJECT:fillinFriInLop:TabIndex = 1.
      /*  */
      /* splitContainerKalkFri */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkFri:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkFri:Name = "splitContainerKalkFri".
      /*  */
      /* splitContainerKalkFri.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkKodoFrek).
      THIS-OBJECT:splitContainerKalkFri:Size = NEW System.Drawing.Size(814, 544).
      THIS-OBJECT:splitContainerKalkFri:SplitterDistance = 501.
      THIS-OBJECT:splitContainerKalkFri:TabIndex = 4.
      /*  */
      /* splitContainerKalkKodoFrek */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKodoFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKodoFrek:Name = "splitContainerKalkKodoFrek".
      /*  */
      /* splitContainerKalkKodoFrek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:Controls:Add(THIS-OBJECT:GridKalkylKoder).
      /*  */
      /* splitContainerKalkKodoFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkBild).
      THIS-OBJECT:splitContainerKalkKodoFrek:Size = NEW System.Drawing.Size(501, 544).
      THIS-OBJECT:splitContainerKalkKodoFrek:SplitterDistance = 375.
      THIS-OBJECT:splitContainerKalkKodoFrek:TabIndex = 36.
      /*  */
      /* GridKalkylKoder */
      /*  */
      THIS-OBJECT:GridKalkylKoder:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKalkylKoder:GuruContext = ?.
      THIS-OBJECT:GridKalkylKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridKalkylKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridKalkylKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKalkylKoder:Name = "GridKalkylKoder".
      THIS-OBJECT:GridKalkylKoder:Size = NEW System.Drawing.Size(375, 544).
      THIS-OBJECT:GridKalkylKoder:TabIndex = 3.
      THIS-OBJECT:GridKalkylKoder:Text = "grid3".
      THIS-OBJECT:GridKalkylKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKalkylKoder_GuruAfterDropped).
      THIS-OBJECT:GridKalkylKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterCellUpdate).
      THIS-OBJECT:GridKalkylKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterSelectChange).
      /*  */
      /* splitContainerKalkBild */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkBild:Name = "splitContainerKalkBild".
      /*  */
      /* splitContainerKalkBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      /*  */
      /* splitContainerKalkBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKalkBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerKalkBild:Size = NEW System.Drawing.Size(122, 544).
      THIS-OBJECT:splitContainerKalkBild:SplitterDistance = 40.
      THIS-OBJECT:splitContainerKalkBild:TabIndex = 0.
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
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:KalkylensKoderAnmarkning).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(40, 544).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 36.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* Rubrikanm */
      /*  */
      THIS-OBJECT:Rubrikanm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Rubrikanm:Enabled = FALSE.
      THIS-OBJECT:Rubrikanm:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:Rubrikanm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Rubrikanm:Name = "Rubrikanm".
      THIS-OBJECT:Rubrikanm:Size = NEW System.Drawing.Size(40, 33).
      THIS-OBJECT:Rubrikanm:TabIndex = 0.
      THIS-OBJECT:Rubrikanm:Text = "Anmärkning".
      /*  */
      /* KalkylensKoderAnmarkning */
      /*  */
      THIS-OBJECT:KalkylensKoderAnmarkning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KalkylensKoderAnmarkning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KalkylensKoderAnmarkning:Multiline = TRUE.
      THIS-OBJECT:KalkylensKoderAnmarkning:Name = "KalkylensKoderAnmarkning".
      THIS-OBJECT:KalkylensKoderAnmarkning:Size = NEW System.Drawing.Size(40, 504).
      THIS-OBJECT:KalkylensKoderAnmarkning:TabIndex = 33.
      THIS-OBJECT:KalkylensKoderAnmarkning:Text = " ".
      THIS-OBJECT:KalkylensKoderAnmarkning:AfterExitEditMode:Subscribe(THIS-OBJECT:KoderAnmarkning_AfterExitEditMode).
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:ReadOnly = TRUE.
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(40, 504).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 34.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
      /*  */
      /* ArendeControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerKalk).
      THIS-OBJECT:Name = "ArendeControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkArbkoder:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopInAntal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopInAntal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopInAntal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopInAntal:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkLopnr:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkFri:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
	
	METHOD PUBLIC VOID ButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
        
         THIS-OBJECT:Root:DatabaseManager:Arende:RemoveKoderSelected().
        
         THIS-OBJECT:KalkylButtonBackMark:GuruOver().
        
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
         THIS-OBJECT:ControlShell:DontCheck = FALSE.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.
	
	
	METHOD PRIVATE VOID ButtonOvermark_ClickE( INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:ButtonOvermark_Click(1).
      RETURN.
   END METHOD.
   

   METHOD PRIVATE VOID ButtonOvermark_Click(INPUT antalvar AS DECIMAL):
    
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:KalkylButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:KalkylButtonOverMark:GuruOver().
         
         THIS-OBJECT:Root:DatabaseManager:Arende:CreateKoderRows(antalvar).
         /* FIXAR OM NY VÄRDEN*/
         THIS-OBJECT:KalkylButtonOverMark:GuruOverefteruppdat().
         
         IF THIS-OBJECT:GridKalkylKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:KalkylButtonOverMark:GuruTogrid:GuruCellvaljare("antal").
         
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
   END METHOD.
	
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KoderAnmarkning_AfterExitEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF KoderTTh:AVAILABLE THEN DO:   
         THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkylensKoderAnmarkning:Text).
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
      END.
      RETURN.

   END METHOD.
	
	
	/*------------------------------------------------------------------------------
         Purpose:    skickar data till db efter cell uppdat                                                
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKalkylKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKod().
      THIS-OBJECT:ControlShell:BerKalkKod().
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
         Purpose:    gör då samma som buttenover                                                  
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopKoder THEN DO:
         THIS-OBJECT:ButtonOvermark_ClickE(?,?).
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_AfterSelectChange ( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN DO:
         
      END.
      RETURN.
    END METHOD.
	DESTRUCTOR PUBLIC ArendeControl ( ):

	END DESTRUCTOR.

END CLASS.