 
 /*------------------------------------------------------------------------
    File        : GuruFragorMaps
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Oct 19 13:11:35 CEST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Modules.Global.GuruFragorMaps INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE bSourceFragor AS Progress.Data.BindingSource NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFragorMaps AS Controls.GridJoin NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   
   
   DEFINE VARIABLE antalDStabeller AS INTEGER NO-UNDO.
   DEFINE VARIABLE topbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE funktionstexterbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE antalProtTabeller AS INTEGER NO-UNDO.
   
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   DEFINE VARIABLE fragorguruDsh AS HANDLE NO-UNDO.





		
	CONSTRUCTOR PUBLIC GuruFragorMaps (  ):
		
		
      SUPER().
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:Close().
      RETURN.

   END METHOD.


   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFragorMaps_VisibleChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GridFragorMaps:GuruExpand(TRUE).
		RETURN.

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
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("funktionsfragor", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FRAGA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR3").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR4").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR5").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR6").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR7").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR8").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR9").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SVAR10").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("funktionstexter").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("funktionstexter", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UNDERTEXT1").
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
      tableDesc1 = NEW Progress.Data.TableDesc("funktionsfragor").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("funktionstexter").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Global.GuruFragorMaps").
      THIS-OBJECT:splitContainerMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFragorMaps = NEW Controls.GridJoin().
      THIS-OBJECT:bSourceFragor = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMain:SuspendLayout().
      CAST(THIS-OBJECT:GridFragorMaps, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bSourceFragor, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerMain */
      /*  */
      THIS-OBJECT:splitContainerMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMain:Name = "splitContainerMain".
      THIS-OBJECT:splitContainerMain:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel1:Controls:Add(THIS-OBJECT:GridFragorMaps).
      /*  */
      /* splitContainerMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel2:Controls:Add(THIS-OBJECT:ButtonOK).
      THIS-OBJECT:splitContainerMain:Size = NEW System.Drawing.Size(1031, 577).
      THIS-OBJECT:splitContainerMain:SplitterDistance = 547.
      THIS-OBJECT:splitContainerMain:TabIndex = 1.
      /*  */
      /* GridFragorMaps */
      /*  */
      THIS-OBJECT:GridFragorMaps:DataSource = THIS-OBJECT:bSourceFragor.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Appearance = appearance1.
      ultraGridColumn7:Header:VisiblePosition = 0.
      ultraGridColumn8:Header:VisiblePosition = 1.
      ultraGridColumn9:Header:VisiblePosition = 2.
      ultraGridColumn10:Header:VisiblePosition = 3.
      ultraGridColumn11:Header:VisiblePosition = 4.
      ultraGridColumn12:Header:VisiblePosition = 5.
      ultraGridColumn13:Header:VisiblePosition = 6.
      ultraGridColumn14:Header:VisiblePosition = 7.
      ultraGridColumn15:Header:VisiblePosition = 8.
      ultraGridColumn16:Header:VisiblePosition = 9.
      ultraGridColumn17:Header:VisiblePosition = 10.
      ultraGridColumn18:Header:VisiblePosition = 11.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 12 NO-UNDO.
      arrayvar0[1] = ultraGridColumn7.
      arrayvar0[2] = ultraGridColumn8.
      arrayvar0[3] = ultraGridColumn9.
      arrayvar0[4] = ultraGridColumn10.
      arrayvar0[5] = ultraGridColumn11.
      arrayvar0[6] = ultraGridColumn12.
      arrayvar0[7] = ultraGridColumn13.
      arrayvar0[8] = ultraGridColumn14.
      arrayvar0[9] = ultraGridColumn15.
      arrayvar0[10] = ultraGridColumn16.
      arrayvar0[11] = ultraGridColumn17.
      arrayvar0[12] = ultraGridColumn18.
      ultraGridBand1:Columns:AddRange(arrayvar0).
      ultraGridColumn19:Header:VisiblePosition = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 1 NO-UNDO.
      arrayvar1[1] = ultraGridColumn19.
      ultraGridBand2:Columns:AddRange(arrayvar1).
      THIS-OBJECT:GridFragorMaps:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:GridFragorMaps:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
      THIS-OBJECT:GridFragorMaps:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFragorMaps:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFragorMaps:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFragorMaps:GuruContext = ?.
      THIS-OBJECT:GridFragorMaps:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFragorMaps:Name = "GridFragorMaps".
      THIS-OBJECT:GridFragorMaps:Size = NEW System.Drawing.Size(1031, 547).
      THIS-OBJECT:GridFragorMaps:TabIndex = 1.
      THIS-OBJECT:GridFragorMaps:Text = "gridJoin1".
      THIS-OBJECT:GridFragorMaps:VisibleChanged:Subscribe(THIS-OBJECT:GridFragorMaps_VisibleChanged).
      /*  */
      /* bSourceFragor */
      /*  */
      THIS-OBJECT:bSourceFragor:MaxDataGuess = 0.
      THIS-OBJECT:bSourceFragor:NoLOBs = FALSE.
      THIS-OBJECT:bSourceFragor:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 1 NO-UNDO.
      arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("UNDERTEXT1", "", Progress.Data.DataType:CHARACTER).
      tableDesc2:Columns = arrayvar3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
      arrayvar4[1] = tableDesc2.
      tableDesc1:ChildTables = arrayvar4.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 11 NO-UNDO.
      arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("FRAGA", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("SVAR1", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[3] = NEW Progress.Data.ColumnPropDesc("SVAR2", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[4] = NEW Progress.Data.ColumnPropDesc("SVAR3", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[5] = NEW Progress.Data.ColumnPropDesc("SVAR4", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[6] = NEW Progress.Data.ColumnPropDesc("SVAR5", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[7] = NEW Progress.Data.ColumnPropDesc("SVAR6", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[8] = NEW Progress.Data.ColumnPropDesc("SVAR7", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[9] = NEW Progress.Data.ColumnPropDesc("SVAR8", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[10] = NEW Progress.Data.ColumnPropDesc("SVAR9", "", Progress.Data.DataType:LOGICAL).
      arrayvar5[11] = NEW Progress.Data.ColumnPropDesc("SVAR10", "", Progress.Data.DataType:LOGICAL).
      tableDesc1:Columns = arrayvar5.
      THIS-OBJECT:bSourceFragor:TableSchema = tableDesc1.
      /*  */
      /* ButtonOK */
      /*  */
      THIS-OBJECT:ButtonOK:Anchor = System.Windows.Forms.AnchorStyles:Right.
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance13.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(909, -2).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 3.
      THIS-OBJECT:ButtonOK:TabStop = FALSE.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* GuruFragorMaps */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1031, 577).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerMain).
      THIS-OBJECT:Name = "GuruFragorMaps".
      THIS-OBJECT:Text = "Frågor angående Guru och karta, markvärdering, mobil mm?".
      THIS-OBJECT:splitContainerMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMain:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFragorMaps, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bSourceFragor, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   
   METHOD PUBLIC VOID ConnectDataSet(DATASET-HANDLE fragorguruDsh BIND, inantalDStabeller AS INTEGER):
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 1.
      
      
      THIS-OBJECT:fragorguruDsh:EMPTY-DATASET() NO-ERROR.
      THIS-OBJECT:antalDStabeller = inantalDStabeller.
      
      IF VALID-HANDLE(fragorguruDsh) THEN DO:
         topbuffh = fragorguruDsh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         funktionstexterbuffh = fragorguruDsh:GET-BUFFER-HANDLE(iCount).
      END. 
      
      THIS-OBJECT:fragorguruDsh = fragorguruDsh.
   END METHOD.
   METHOD PUBLIC VOID InitiateGuruMaps():
      THIS-OBJECT:CreateGridFragorMaps().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFragorMaps():
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE decCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE hideBands AS INTEGER EXTENT 10 NO-UNDO.
      DEFINE VARIABLE sortBands AS INTEGER EXTENT 50 NO-UNDO.
      DEFINE VARIABLE sortCount AS INTEGER NO-UNDO INITIAL 1.
      
      
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      
      
      
      
       
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
      iCount = 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      
      THIS-OBJECT:GridFragorMaps:GuruGridRadOption(TRUE).
      CellFont = NEW System.Drawing.Font("Arial Black", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).   
                                             /*styrfält , värde ,   färg ,                          font, alla col, lista col  0  bgcolor 1 = fg color 2 font*/
                                                                                                                          /*  ska avslutas med,*/ 
      THIS-OBJECT:GridFragorMaps:GuruAddRadOption("TYPSNITT", "1", Guru.StaticMethods:DotNetColor(12),CellFont,TRUE,"",2,0).
      
      
      /*start band*/        
      iband = 0.
      /*topbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("FRAGA","_",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("SVAR1","1",FALSE,10).
      rubrikergrid[3] = NEW Controls.GridRubrik("SVAR2","2",FALSE,10).
      rubrikergrid[4] = NEW Controls.GridRubrik("SVAR3","3",FALSE,10).
      rubrikergrid[5] = NEW Controls.GridRubrik("SVAR4","4",FALSE,10).
      rubrikergrid[6] = NEW Controls.GridRubrik("SVAR5","5",FALSE,10).
      rubrikergrid[7] = NEW Controls.GridRubrik("SVAR6","1",FALSE,10).
      rubrikergrid[8] = NEW Controls.GridRubrik("SVAR7","2",FALSE,10).
      rubrikergrid[9] = NEW Controls.GridRubrik("SVAR8","3",FALSE,10).
      rubrikergrid[10] = NEW Controls.GridRubrik("SVAR9","4",FALSE,10).
      rubrikergrid[11] = NEW Controls.GridRubrik("SVAR10","5",FALSE,10).
      MESSAGE "ss"
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT FALSE,INPUT 11,INPUT THIS-OBJECT:topbuffh).
     
      iband = iband + 1.
      rubrikergrid[1] = NEW Controls.GridRubrik("UNDERTEXT1","_",TRUE,500).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT FALSE,INPUT 1,INPUT THIS-OBJECT:funktionstexterbuffh).
      
           
      THIS-OBJECT:GridFragorMaps:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband + 1,DATASET-HANDLE fragorguruDsh).
      THIS-OBJECT:GridFragorMaps:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridFragorMaps:Text = "". 
      
      THIS-OBJECT:SkapaGrupper(2).
      THIS-OBJECT:SkapaGrupper(4).
      
   END METHOD.
   METHOD PUBLIC VOID SkapaGrupper(gruppband AS INTEGER ):
      /*
      THIS-OBJECT:GridJmfBerInk:SkapaGrupp(gruppband, "grupp1", "Beredning").
      
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "ANTALBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "LEVNAMNBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "BENAMNINGBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "ENRBER").
      
      THIS-OBJECT:GridJmfBerInk:SkapaGrupp(gruppband, "grupp2", "Inköp").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "ANTALINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "LEVNAMNINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "BENAMNINGINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "ENRINK").
      */
   END METHOD. 
	DESTRUCTOR PUBLIC GuruFragorMaps ( ):

	END DESTRUCTOR.

END CLASS.