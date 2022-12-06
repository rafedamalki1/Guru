 
 /*------------------------------------------------------------------------
    File        : TESTARGRID
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu Dec 18 14:56:31 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.
USING Progress.Lang.*.
USING OpenEdge.Core.*.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.
USING OpenEdge.Net.URI.


CLASS TESTARGRID INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.

		
	CONSTRUCTOR PUBLIC TESTARGRID (  ):
		
		
      SUPER().
      InitializeComponent().
      AhlsellSaldo().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   METHOD PUBLIC VOID AhlsellSaldo():
      RUN AHLSELLAGERSTATUSp.P.
         
   END METHOD.
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
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("perstemp", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANVANDARE").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EFTERNAMN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FORNAMN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KUNDNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OMRADE").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OMRADENUM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PANDRA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PERSONALKOD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PREC").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("tidtemp").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("tidtemp", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AONR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BEFATTNING").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DATUM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DELNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EQDAG").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KOD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LUNCH").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("NYA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OANT1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OANT2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OBER").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OPRIS").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OSLUT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn26 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn26 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OSTART").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn27 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn27 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OTOTALT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn28 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn28 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PERSONALKOD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn29 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn29 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PRIS").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn30 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn30 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PRISTYP").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn31 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn31 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RESKOSTDEC").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn32 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn32 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SLUT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn33 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn33 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("START").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn34 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn34 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("tidlog").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn35 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn35 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TOTALT").
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
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("perstemp").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("tidtemp").
      THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
      THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* ultraGrid1 */
      /*  */
      THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance1.
      ultraGridColumn1:Header:VisiblePosition = 0.
      ultraGridColumn2:Header:VisiblePosition = 1.
      ultraGridColumn3:Header:VisiblePosition = 2.
      ultraGridColumn4:Header:VisiblePosition = 3.
      ultraGridColumn5:Header:VisiblePosition = 4.
      ultraGridColumn6:Header:VisiblePosition = 5.
      ultraGridColumn7:Header:VisiblePosition = 6.
      ultraGridColumn8:Header:VisiblePosition = 7.
      ultraGridColumn9:Header:VisiblePosition = 8.
      ultraGridColumn10:Header:VisiblePosition = 9.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 10 NO-UNDO.
      arrayvar0[1] = ultraGridColumn1.
      arrayvar0[2] = ultraGridColumn2.
      arrayvar0[3] = ultraGridColumn3.
      arrayvar0[4] = ultraGridColumn4.
      arrayvar0[5] = ultraGridColumn5.
      arrayvar0[6] = ultraGridColumn6.
      arrayvar0[7] = ultraGridColumn7.
      arrayvar0[8] = ultraGridColumn8.
      arrayvar0[9] = ultraGridColumn9.
      arrayvar0[10] = ultraGridColumn10.
      ultraGridBand1:Columns:AddRange(arrayvar0).
      ultraGridColumn11:Header:VisiblePosition = 0.
      ultraGridColumn12:Header:VisiblePosition = 1.
      ultraGridColumn13:Header:VisiblePosition = 2.
      ultraGridColumn14:Header:VisiblePosition = 3.
      ultraGridColumn15:Header:VisiblePosition = 4.
      ultraGridColumn16:Header:VisiblePosition = 5.
      ultraGridColumn17:Header:VisiblePosition = 6.
      ultraGridColumn18:Header:VisiblePosition = 7.
      ultraGridColumn19:Header:VisiblePosition = 8.
      ultraGridColumn20:Header:VisiblePosition = 9.
      ultraGridColumn21:Header:VisiblePosition = 10.
      ultraGridColumn22:Header:VisiblePosition = 11.
      ultraGridColumn23:Header:VisiblePosition = 12.
      ultraGridColumn24:Header:VisiblePosition = 13.
      ultraGridColumn25:Header:VisiblePosition = 14.
      ultraGridColumn26:Header:VisiblePosition = 15.
      ultraGridColumn27:Header:VisiblePosition = 16.
      ultraGridColumn28:Header:VisiblePosition = 17.
      ultraGridColumn29:Header:VisiblePosition = 18.
      ultraGridColumn30:Header:VisiblePosition = 19.
      ultraGridColumn31:Header:VisiblePosition = 20.
      ultraGridColumn32:Header:VisiblePosition = 21.
      ultraGridColumn33:Header:VisiblePosition = 22.
      ultraGridColumn34:Header:VisiblePosition = 23.
      ultraGridColumn35:Header:VisiblePosition = 24.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 25 NO-UNDO.
      arrayvar1[1] = ultraGridColumn11.
      arrayvar1[2] = ultraGridColumn12.
      arrayvar1[3] = ultraGridColumn13.
      arrayvar1[4] = ultraGridColumn14.
      arrayvar1[5] = ultraGridColumn15.
      arrayvar1[6] = ultraGridColumn16.
      arrayvar1[7] = ultraGridColumn17.
      arrayvar1[8] = ultraGridColumn18.
      arrayvar1[9] = ultraGridColumn19.
      arrayvar1[10] = ultraGridColumn20.
      arrayvar1[11] = ultraGridColumn21.
      arrayvar1[12] = ultraGridColumn22.
      arrayvar1[13] = ultraGridColumn23.
      arrayvar1[14] = ultraGridColumn24.
      arrayvar1[15] = ultraGridColumn25.
      arrayvar1[16] = ultraGridColumn26.
      arrayvar1[17] = ultraGridColumn27.
      arrayvar1[18] = ultraGridColumn28.
      arrayvar1[19] = ultraGridColumn29.
      arrayvar1[20] = ultraGridColumn30.
      arrayvar1[21] = ultraGridColumn31.
      arrayvar1[22] = ultraGridColumn32.
      arrayvar1[23] = ultraGridColumn33.
      arrayvar1[24] = ultraGridColumn34.
      arrayvar1[25] = ultraGridColumn35.
      ultraGridBand2:Columns:AddRange(arrayvar1).
      THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
      THIS-OBJECT:ultraGrid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:ultraGrid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:ultraGrid1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:ultraGrid1:DisplayLayout:MaxRowScrollRegions = 1.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:ultraGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraGrid1:Name = "ultraGrid1".
      THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(919, 359).
      THIS-OBJECT:ultraGrid1:TabIndex = 0.
      THIS-OBJECT:ultraGrid1:Text = "ultraGrid1".
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
      DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 25 NO-UNDO.
      arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("AONR", "?", Progress.Data.DataType:CHARACTER).
      arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("BEFATTNING", "?", Progress.Data.DataType:CHARACTER).
      arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("DATUM", "?", Progress.Data.DataType:DATE).
      arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("DELNR", "?", Progress.Data.DataType:INTEGER).
      arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("EQDAG", "?", Progress.Data.DataType:INTEGER).
      arrayvar3[6] = NEW Progress.Data.ColumnPropDesc("KOD", "?", Progress.Data.DataType:CHARACTER).
      arrayvar3[7] = NEW Progress.Data.ColumnPropDesc("KOST", "PRIS", Progress.Data.DataType:DECIMAL).
      arrayvar3[8] = NEW Progress.Data.ColumnPropDesc("LUNCH", "?", Progress.Data.DataType:DECIMAL).
      arrayvar3[9] = NEW Progress.Data.ColumnPropDesc("NYA", "?", Progress.Data.DataType:LOGICAL).
      arrayvar3[10] = NEW Progress.Data.ColumnPropDesc("OANT1", "?", Progress.Data.DataType:DECIMAL).
      arrayvar3[11] = NEW Progress.Data.ColumnPropDesc("OANT2", "?", Progress.Data.DataType:DECIMAL).
      arrayvar3[12] = NEW Progress.Data.ColumnPropDesc("OBER", "?", Progress.Data.DataType:LOGICAL).
      arrayvar3[13] = NEW Progress.Data.ColumnPropDesc("OKOST", "PRIS", Progress.Data.DataType:DECIMAL).
      arrayvar3[14] = NEW Progress.Data.ColumnPropDesc("OPRIS", "PRIS", Progress.Data.DataType:DECIMAL).
      arrayvar3[15] = NEW Progress.Data.ColumnPropDesc("OSLUT", "SLUT TID", Progress.Data.DataType:DECIMAL).
      arrayvar3[16] = NEW Progress.Data.ColumnPropDesc("OSTART", "START TID", Progress.Data.DataType:DECIMAL).
      arrayvar3[17] = NEW Progress.Data.ColumnPropDesc("OTOTALT", "?", Progress.Data.DataType:DECIMAL).
      arrayvar3[18] = NEW Progress.Data.ColumnPropDesc("PERSONALKOD", "ENHET/SIGN", Progress.Data.DataType:CHARACTER).
      arrayvar3[19] = NEW Progress.Data.ColumnPropDesc("PRIS", "PRIS", Progress.Data.DataType:DECIMAL).
      arrayvar3[20] = NEW Progress.Data.ColumnPropDesc("PRISTYP", "PRISTYP", Progress.Data.DataType:CHARACTER).
      arrayvar3[21] = NEW Progress.Data.ColumnPropDesc("RESKOSTDEC", "?", Progress.Data.DataType:DECIMAL).
      arrayvar3[22] = NEW Progress.Data.ColumnPropDesc("SLUT", "SLUT TID", Progress.Data.DataType:DECIMAL).
      arrayvar3[23] = NEW Progress.Data.ColumnPropDesc("START", "START TID", Progress.Data.DataType:DECIMAL).
      arrayvar3[24] = NEW Progress.Data.ColumnPropDesc("tidlog", "?", Progress.Data.DataType:LOGICAL).
      arrayvar3[25] = NEW Progress.Data.ColumnPropDesc("TOTALT", "?", Progress.Data.DataType:DECIMAL).
      tableDesc2:Columns = arrayvar3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
      arrayvar4[1] = tableDesc2.
      tableDesc1:ChildTables = arrayvar4.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
      arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("ANVANDARE", "?", Progress.Data.DataType:CHARACTER).
      arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("EFTERNAMN", "?", Progress.Data.DataType:CHARACTER).
      arrayvar5[3] = NEW Progress.Data.ColumnPropDesc("FORNAMN", "?", Progress.Data.DataType:CHARACTER).
      arrayvar5[4] = NEW Progress.Data.ColumnPropDesc("KUNDNR", "?", Progress.Data.DataType:INTEGER).
      arrayvar5[5] = NEW Progress.Data.ColumnPropDesc("OMRADE", "Område", Progress.Data.DataType:CHARACTER).
      arrayvar5[6] = NEW Progress.Data.ColumnPropDesc("OMRADENUM", "?", Progress.Data.DataType:CHARACTER).
      arrayvar5[7] = NEW Progress.Data.ColumnPropDesc("PANDRA", "?", Progress.Data.DataType:LOGICAL).
      arrayvar5[8] = NEW Progress.Data.ColumnPropDesc("PERSONALKOD", "?", Progress.Data.DataType:CHARACTER).
      arrayvar5[9] = NEW Progress.Data.ColumnPropDesc("PREC", "?", Progress.Data.DataType:RECID).
      tableDesc1:Columns = arrayvar5.
      THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
      /*  */
      /* TESTARGRID */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(919, 359).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGrid1).
      THIS-OBJECT:Name = "TESTARGRID".
      THIS-OBJECT:Text = "TESTARGRID".
      CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC TESTARGRID ( ):

	END DESTRUCTOR.

END CLASS.
