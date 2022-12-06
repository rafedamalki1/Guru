 
 /*------------------------------------------------------------------------
    File        : ArendeStatusControl.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri May 09 09:29:16 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Arende.ArendeStatusControl INHERITS UserControl: 
	{ARENDEKATH.i}
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Arende.ArendeShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerStatus AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridStatus AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC ArendeStatusControl (INPUT r AS Guru.Root  ):
		
		
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

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridStatus_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeStatusControl").
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
      THIS-OBJECT:splitContainerStatus = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridStatus = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerStatus, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStatus:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStatus:SuspendLayout().
      CAST(THIS-OBJECT:GridStatus, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerStatus */
      /*  */
      THIS-OBJECT:splitContainerStatus:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStatus:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStatus:Name = "splitContainerStatus".
      /*  */
      /* splitContainerStatus.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStatus:Panel1:Controls:Add(THIS-OBJECT:GridStatus).
      /*  */
      /* splitContainerStatus.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerStatus.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerStatus:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerStatus:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerStatus:SplitterDistance = 565.
      THIS-OBJECT:splitContainerStatus:TabIndex = 1.
      /*  */
      /* GridStatus */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridStatus:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridStatus:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridStatus:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStatus:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridStatus:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridStatus:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridStatus:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridStatus:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridStatus:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridStatus:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridStatus:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridStatus:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridStatus:GuruContext = ?.
      THIS-OBJECT:GridStatus:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridStatus:Name = "GridStatus".
      THIS-OBJECT:GridStatus:Size = NEW System.Drawing.Size(565, 546).
      THIS-OBJECT:GridStatus:TabIndex = 0.
      THIS-OBJECT:GridStatus:Text = "grid1".
      THIS-OBJECT:GridStatus:AfterCellUpdate:Subscribe(THIS-OBJECT:GridStatus_AfterCellUpdate).
      /*  */
      /* ArendeStatusControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerStatus).
      THIS-OBJECT:Name = "ArendeStatusControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerStatus:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStatus, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStatus:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridStatus, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC ArendeStatusControl ( ):

	END DESTRUCTOR.

END CLASS.