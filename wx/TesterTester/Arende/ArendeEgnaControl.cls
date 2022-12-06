 
 /*------------------------------------------------------------------------
    File        : KalkEgnaControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 14:29:41 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Arende.ArendeEgnaControl INHERITS UserControl: 
	{ARENDEKATH.i}
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Arende.ArendeShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaValj AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaVald AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaArbKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridValdaUnderKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridUnderKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridEgnaKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaKoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenTaBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenSkapa AS System.Windows.Forms.ToolStripButton NO-UNDO.

		
	CONSTRUCTOR PUBLIC ArendeEgnaControl ( INPUT r AS Guru.Root ):
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
     {ARENDEKATDBH.i Root:DatabaseManager:Arende}
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeEgnaControl").
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
      THIS-OBJECT:splitContainerEgnaBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaArbKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaKoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:EgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:EgenTaBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridEgnaKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaVald = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridValdaUnderKoder = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:SuspendLayout().
      THIS-OBJECT:EgnaKoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:SuspendLayout().
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerEgnaBild */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaBild:Name = "splitContainerEgnaBild".
      /*  */
      /* splitContainerEgnaBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaKoder).
      /*  */
      /* splitContainerEgnaBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerEgnaBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerEgnaBild:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerEgnaBild:SplitterDistance = 776.
      THIS-OBJECT:splitContainerEgnaBild:TabIndex = 20.
      /*  */
      /* splitContainerEgnaKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaKoder:Name = "splitContainerEgnaKoder".
      THIS-OBJECT:splitContainerEgnaKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaArbKoder).
      /*  */
      /* splitContainerEgnaKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaValj).
      THIS-OBJECT:splitContainerEgnaKoder:Size = NEW System.Drawing.Size(776, 546).
      THIS-OBJECT:splitContainerEgnaKoder:SplitterDistance = 197.
      THIS-OBJECT:splitContainerEgnaKoder:TabIndex = 18.
      /*  */
      /* splitContainerEgnaArbKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaArbKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaArbKoder:Name = "splitContainerEgnaArbKoder".
      THIS-OBJECT:splitContainerEgnaArbKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaArbKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:Controls:Add(THIS-OBJECT:EgnaKoderStrip).
      /*  */
      /* splitContainerEgnaArbKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:Controls:Add(THIS-OBJECT:GridEgnaKoder).
      THIS-OBJECT:splitContainerEgnaArbKoder:Size = NEW System.Drawing.Size(776, 197).
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterDistance = 25.
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerEgnaArbKoder:TabIndex = 14.
      /*  */
      /* EgnaKoderStrip */
      /*  */
      THIS-OBJECT:EgnaKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:EgenSkapa.
      arrayvar0[2] = THIS-OBJECT:EgenTaBort.
      THIS-OBJECT:EgnaKoderStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:EgnaKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:EgnaKoderStrip:Name = "EgnaKoderStrip".
      THIS-OBJECT:EgnaKoderStrip:Size = NEW System.Drawing.Size(776, 25).
      THIS-OBJECT:EgnaKoderStrip:TabIndex = 13.
      THIS-OBJECT:EgnaKoderStrip:Text = "toolStrip1".
      /*  */
      /* EgenSkapa */
      /*  */
      THIS-OBJECT:EgenSkapa:Image = CAST(resources:GetObject("EgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenSkapa:Name = "EgenSkapa".
      THIS-OBJECT:EgenSkapa:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:EgenSkapa:Text = "Lägg till egen".
      THIS-OBJECT:EgenSkapa:Click:Subscribe(THIS-OBJECT:SkapaNyEgen_Click).
      /*  */
      /* EgenTaBort */
      /*  */
      THIS-OBJECT:EgenTaBort:Image = CAST(resources:GetObject("EgenTaBort.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenTaBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenTaBort:Name = "EgenTaBort".
      THIS-OBJECT:EgenTaBort:Size = NEW System.Drawing.Size(94, 22).
      THIS-OBJECT:EgenTaBort:Text = "Ta bort egen".
      THIS-OBJECT:EgenTaBort:Click:Subscribe(THIS-OBJECT:TaBortEgen_Click).
      /*  */
      /* GridEgnaKoder */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgnaKoder:GuruContext = ?.
      THIS-OBJECT:GridEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgnaKoder:Name = "GridEgnaKoder".
      THIS-OBJECT:GridEgnaKoder:Size = NEW System.Drawing.Size(776, 171).
      THIS-OBJECT:GridEgnaKoder:TabIndex = 12.
      THIS-OBJECT:GridEgnaKoder:Text = "grid1".
      THIS-OBJECT:GridEgnaKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellUpdate).
      /*  */
      /* splitContainerEgnaValj */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerEgnaValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaValj:Name = "splitContainerEgnaValj".
      /*  */
      /* splitContainerEgnaValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel1:Controls:Add(THIS-OBJECT:GridUnderKoder).
      /*  */
      /* splitContainerEgnaValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaVald).
      THIS-OBJECT:splitContainerEgnaValj:Size = NEW System.Drawing.Size(776, 345).
      THIS-OBJECT:splitContainerEgnaValj:SplitterDistance = 252.
      THIS-OBJECT:splitContainerEgnaValj:TabIndex = 0.
      /*  */
      /* GridUnderKoder */
      /*  */
      THIS-OBJECT:GridUnderKoder:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUnderKoder:Name = "GridUnderKoder".
      THIS-OBJECT:GridUnderKoder:Size = NEW System.Drawing.Size(250, 343).
      THIS-OBJECT:GridUnderKoder:TabIndex = 10.
      THIS-OBJECT:GridUnderKoder:Text = "grid1".
      THIS-OBJECT:GridUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUnderKoder_GuruAfterDropped).
      /*  */
      /* splitContainerEgnaVald */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaVald:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerEgnaVald:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaVald:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaVald:Name = "splitContainerEgnaVald".
      /*  */
      /* splitContainerEgnaVald.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonOverMark).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonBackMark).
      /*  */
      /* splitContainerEgnaVald.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel2:Controls:Add(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:splitContainerEgnaVald:Size = NEW System.Drawing.Size(518, 343).
      THIS-OBJECT:splitContainerEgnaVald:SplitterDistance = 40.
      THIS-OBJECT:splitContainerEgnaVald:TabIndex = 0.
      /*  */
      /* EgnaButtonOverMark */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:EgnaButtonOverMark:Appearance = appearance25.
      THIS-OBJECT:EgnaButtonOverMark:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:EgnaButtonOverMark:Name = "EgnaButtonOverMark".
      THIS-OBJECT:EgnaButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:EgnaButtonOverMark:TabIndex = 13.
      THIS-OBJECT:EgnaButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:EgnaButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOverMarkPriser_Click).
      /*  */
      /* EgnaButtonBackMark */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:EgnaButtonBackMark:Appearance = appearance26.
      THIS-OBJECT:EgnaButtonBackMark:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:EgnaButtonBackMark:Name = "EgnaButtonBackMark".
      THIS-OBJECT:EgnaButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:EgnaButtonBackMark:TabIndex = 15.
      THIS-OBJECT:EgnaButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:EgnaButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMarkPriser_Click).
      /*  */
      /* GridValdaUnderKoder */
      /*  */
      THIS-OBJECT:GridValdaUnderKoder:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridValdaUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaUnderKoder:Name = "GridValdaUnderKoder".
      THIS-OBJECT:GridValdaUnderKoder:Size = NEW System.Drawing.Size(474, 343).
      THIS-OBJECT:GridValdaUnderKoder:TabIndex = 11.
      THIS-OBJECT:GridValdaUnderKoder:Text = "grid1".
      THIS-OBJECT:GridValdaUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaUnderKoder_GuruAfterDropped).
      THIS-OBJECT:GridValdaUnderKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdaUnderKoder_AfterCellUpdate).
      /*  */
      /* KalkEgnaControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerEgnaBild).
      THIS-OBJECT:Name = "KalkEgnaControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerEgnaBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
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
   METHOD PUBLIC VOID SkapaNyEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      THIS-OBJECT:ControlShell:DontCheck = TRUE.

      THIS-OBJECT:GridEgnaKoder:SyncMan().
      rid = THIS-OBJECT:Root:DatabaseManager:Arende:CreateEgenKod().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).
      THIS-OBJECT:GridEgnaKoder:GuruCellValjare("BENAMNING").       
      THIS-OBJECT:ControlShell:DontCheck= FALSE.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID TaBortEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.

      THIS-OBJECT:GridEgnaKoder:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN DO:
         THIS-OBJECT:GridEgnaKoder:SyncMan().
         IF THIS-OBJECT:GridEgnaKoder:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:Root:DatabaseManager:Arende:RemoveEgenKod().
            GridEgnaKoder:GuruBortrader(FALSE).
            THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
            THIS-OBJECT:ControlShell:FilterUnderKoder().
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
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
   METHOD PUBLIC VOID GridEgnaKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridEgnaKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      THIS-OBJECT:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKod().
      THIS-OBJECT:ControlShell:BerEgenKod().        
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaUnderKoder THEN DO:
         THIS-OBJECT:ButtonBackmarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridUnderKoder THEN DO:
         THIS-OBJECT:ButtonOvermarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridEgnaKoder:ActiveRow.
      IF THIS-OBJECT:EgnaButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:EgnaButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Arende:CreateValdaRows().      
         THIS-OBJECT:EgnaButtonOverMark:GuruOverefteruppdat().
         IF THIS-OBJECT:GridUnderKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:EgnaButtonOverMark:GuruTogrid:GuruCellvaljare("TIMMAR").       
         THIS-OBJECT:ControlShell:BerEgenKod().
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
   METHOD PRIVATE VOID ButtonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridValdaUnderKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:Arende:RemoveValdaSelectedEgna().
         THIS-OBJECT:EgnaButtonBackMark:GuruOver().      
         THIS-OBJECT:ControlShell:BerEgenKod().
         THIS-OBJECT:Root:DatabaseManager:Arende:ArendeSpara().
         THIS-OBJECT:ControlShell:DontCheck = FALSE.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.

      kolnamn = e:Cell:Column:ToString().     
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridValdaUnderKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.

      THIS-OBJECT:GridValdaUnderKoder:SyncMan().
      THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      IF kolnamn = "TIMMAR" OR kolnamn = "PRIS" OR kolnamn = "KOSTNAD" THEN DO:
         kolnamn = "FRI" + kolnamn.
         THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      END.    
      THIS-OBJECT:Root:DatabaseManager:Arende:UpdateKodSub().
      THIS-OBJECT:ControlShell:BerEgenKod().
      
      RETURN.
      
   END METHOD.
   
	DESTRUCTOR PUBLIC ArendeEgnaControl ( ):

	END DESTRUCTOR.

END CLASS.