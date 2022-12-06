 
 /*------------------------------------------------------------------------
    File        : EkgExport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Oct 08 10:57:18 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.EkgData.EkgExport INHERITS UserControl: 
 
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   
   DEFINE PUBLIC VARIABLE fillinDate1 AS Controls.FillinDate NO-UNDO.
   DEFINE PUBLIC VARIABLE gridExport AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE winlgbutton AS System.Windows.Forms.Button NO-UNDO.
   DEFINE PUBLIC VARIABLE shellbutton AS System.Windows.Forms.Button NO-UNDO.
   
   
  CONSTRUCTOR PUBLIC EkgExport (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.

   METHOD PRIVATE VOID InitializeComponent ( ):
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
      THIS-OBJECT:fillinDate1 = NEW Controls.FillinDate().
      THIS-OBJECT:gridExport = NEW Controls.Grid().
      THIS-OBJECT:shellbutton = NEW System.Windows.Forms.Button().
      THIS-OBJECT:winlgbutton = NEW System.Windows.Forms.Button().
      CAST(THIS-OBJECT:gridExport, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* fillinDate1 */
      /*  */
      THIS-OBJECT:fillinDate1:Location = NEW System.Drawing.Point(43, 17).
      THIS-OBJECT:fillinDate1:Name = "fillinDate1".
      THIS-OBJECT:fillinDate1:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinDate1:TabIndex = 0.
      /*  */
      /* gridExport */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridExport:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridExport:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridExport:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridExport:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridExport:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridExport:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridExport:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridExport:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridExport:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridExport:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridExport:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridExport:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridExport:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridExport:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridExport:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridExport:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridExport:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridExport:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridExport:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridExport:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridExport:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridExport:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridExport:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridExport:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridExport:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridExport:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridExport:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridExport:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridExport:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridExport:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridExport:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridExport:GuruContext = ?.
      THIS-OBJECT:gridExport:Location = NEW System.Drawing.Point(43, 80).
      THIS-OBJECT:gridExport:Name = "gridExport".
      THIS-OBJECT:gridExport:Size = NEW System.Drawing.Size(635, 460).
      THIS-OBJECT:gridExport:TabIndex = 1.
      THIS-OBJECT:gridExport:Text = "gridExport".
      /*  */
      /* shellbutton */
      /*  */
      THIS-OBJECT:shellbutton:Location = NEW System.Drawing.Point(868, 158).
      THIS-OBJECT:shellbutton:Name = "shellbutton".
      THIS-OBJECT:shellbutton:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:shellbutton:TabIndex = 2.
      THIS-OBJECT:shellbutton:Text = "shellbutton".
      THIS-OBJECT:shellbutton:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:shellbutton:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:shellbutton:Click:Subscribe(THIS-OBJECT:shellbutton_Click).
      /*  */
      /* winlgbutton */
      /*  */
      THIS-OBJECT:winlgbutton:Location = NEW System.Drawing.Point(889, 323).
      THIS-OBJECT:winlgbutton:Name = "winlgbutton".
      THIS-OBJECT:winlgbutton:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:winlgbutton:TabIndex = 3.
      THIS-OBJECT:winlgbutton:Text = "winlgbutton".
      THIS-OBJECT:winlgbutton:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:winlgbutton:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:winlgbutton:Click:Subscribe(THIS-OBJECT:winlgbutton_Click).
      /*  */
      /* EkgExport */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:winlgbutton).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:shellbutton).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:gridExport).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:fillinDate1).
      THIS-OBJECT:Name = "EkgExport".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1065, 584).
      CAST(THIS-OBJECT:gridExport, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID shellbutton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:Testar().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID winlgbutton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Hide().
      THIS-OBJECT:ControlWinLg:GomAllt(FALSE).
		RETURN.

	END METHOD.

END CLASS.