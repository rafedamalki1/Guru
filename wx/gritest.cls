 
 /*------------------------------------------------------------------------
    File        : gritest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Dec 15 16:13:44 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS gritest INHERITS UserControl: 
	
   DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE grid1 AS Controls.Grid NO-UNDO.
   DEFINE VARIABLE PH AS HANDLE NO-UNDO.
   DEFINE VARIABLE PQ AS HANDLE NO-UNDO.
   DEFINE VARIABLE TH AS HANDLE NO-UNDO.
   DEFINE VARIABLE TQ AS HANDLE NO-UNDO.		
	CONSTRUCTOR PUBLIC gritest (  ):
		
		
      SUPER().
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
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
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("personaltemp", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EFTERNAMN", -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FORNAMN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PERSONALKOD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn33 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn33 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("tidregitemp").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("gritest").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("tidregitemp", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn35 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn35 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AONR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn44 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn44 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DELNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn43 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn43 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DATUM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn76 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn76 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("START").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn75 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn75 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SLUT").
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
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("personaltemp").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("tidregitemp").
      THIS-OBJECT:grid1 = NEW Controls.Grid().
      THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* grid1 */
      /*  */
      THIS-OBJECT:grid1:DataSource = THIS-OBJECT:bindingSource1.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:grid1:DisplayLayout:Appearance = appearance1.
      ultraGridColumn10:Header:VisiblePosition = 0.
      ultraGridColumn11:Header:VisiblePosition = 1.
      ultraGridColumn22:Header:VisiblePosition = 2.
      ultraGridColumn33:Header:VisiblePosition = 3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 4 NO-UNDO.
      arrayvar0[1] = ultraGridColumn10.
      arrayvar0[2] = ultraGridColumn11.
      arrayvar0[3] = ultraGridColumn22.
      arrayvar0[4] = ultraGridColumn33.
      ultraGridBand1:Columns:AddRange(arrayvar0).
      appearance2:BackColor = System.Drawing.Color:Yellow.
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      ultraGridBand1:Header:Appearance = appearance2.
      ultraGridColumn35:Header:VisiblePosition = 0.
      ultraGridColumn44:Header:VisiblePosition = 1.
      ultraGridColumn43:Header:VisiblePosition = 2.
      ultraGridColumn76:Header:VisiblePosition = 4.
      ultraGridColumn75:Header:VisiblePosition = 3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 5 NO-UNDO.
      arrayvar1[1] = ultraGridColumn35.
      arrayvar1[2] = ultraGridColumn44.
      arrayvar1[3] = ultraGridColumn43.
      arrayvar1[4] = ultraGridColumn76.
      arrayvar1[5] = ultraGridColumn75.
      ultraGridBand2:Columns:AddRange(arrayvar1).
      THIS-OBJECT:grid1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:grid1:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
      THIS-OBJECT:grid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:grid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance3:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance3:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance3:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance3:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:Appearance = appearance3.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance4.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance5:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance5:BackColor2 = System.Drawing.SystemColors:Control.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:grid1:DisplayLayout:GroupByBox:PromptAppearance = appearance5.
      THIS-OBJECT:grid1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:grid1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveCellAppearance = appearance6.
      appearance7:BackColor = System.Drawing.SystemColors:Highlight.
      appearance7:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveRowAppearance = appearance7.
      THIS-OBJECT:grid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:grid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:Override:CardAreaAppearance = appearance8.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
      THIS-OBJECT:grid1:DisplayLayout:Override:CellPadding = 0.
      appearance10:BackColor = System.Drawing.SystemColors:Control.
      appearance10:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance10:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance10:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance10:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:grid1:DisplayLayout:Override:GroupByRowAppearance = appearance10.
      appearance11:TextHAlignAsString = "Left".
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderAppearance = appearance11.
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:grid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      appearance12:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:grid1:DisplayLayout:Override:RowAppearance = appearance12.
      THIS-OBJECT:grid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance13:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:grid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance13.
      THIS-OBJECT:grid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:grid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:grid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:grid1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:grid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:grid1:GuruContext = ?.
      THIS-OBJECT:grid1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:grid1:Name = "grid1".
      THIS-OBJECT:grid1:Size = NEW System.Drawing.Size(1052, 459).
      THIS-OBJECT:grid1:TabIndex = 0.
      THIS-OBJECT:grid1:Text = "grid1".
      /*  */
      /* bindingSource1 */
      /*  */
      THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
      THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
      THIS-OBJECT:bindingSource1:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
      arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("AONR", "AONR", Progress.Data.DataType:CHARACTER).
      arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("DELNR", "delnr", Progress.Data.DataType:INTEGER).
      arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("DATUM", "DATUM", Progress.Data.DataType:DATE).
      arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("START", "START TID", Progress.Data.DataType:DECIMAL).
      arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("SLUT", "SLUT TID", Progress.Data.DataType:DECIMAL).
      tableDesc2:Columns = arrayvar3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
      arrayvar4[1] = tableDesc2.
      tableDesc1:ChildTables = arrayvar4.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("EFTERNAMN", "EFTERNAMN", Progress.Data.DataType:CHARACTER).
      arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("FORNAMN", "FÖRNAMN", Progress.Data.DataType:CHARACTER).
      arrayvar5[3] = NEW Progress.Data.ColumnPropDesc("PERSONALKOD", "ENHET/SIGN", Progress.Data.DataType:CHARACTER).
      tableDesc1:Columns = arrayvar5.
      THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
      /*  */
      /* gritest */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:grid1).
      THIS-OBJECT:Name = "gritest".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1052, 459).
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC gritest ( ):

	END DESTRUCTOR.

END CLASS.