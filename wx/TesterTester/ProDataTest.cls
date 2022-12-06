 
 /*------------------------------------------------------------------------
    File        : ProDataTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue May 29 15:15:02 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS Tester.ProDataTest INHERITS Form: 
	{KALKYLKAT.I}
   DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridNumsub AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridNum AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGridBagLayoutManager1 AS Infragistics.Win.Misc.UltraGridBagLayoutManager NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGridBagLayoutPanel2 AS Infragistics.Win.Misc.UltraGridBagLayoutPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGridBagLayoutPanel1 AS Infragistics.Win.Misc.UltraGridBagLayoutPanel NO-UNDO.

   DEFINE PUBLIC VARIABLE DatabaseHandler AS Tester.ProDataDb2 NO-UNDO.
		
	CONSTRUCTOR PUBLIC ProDataTest (  ):
      SUPER().
      InitializeComponent().

      THIS-OBJECT:DatabaseHandler = NEW Tester.ProDataDb2().

      THIS-OBJECT:DatabaseHandler:LaddaKalkyl(100284).

      THIS-OBJECT:CreateGrids().
      /*
      THIS-OBJECT:DatabaseHandler:NumTT:BUFFER-CREATE().
      THIS-OBJECT:DatabaseHandler:NumTT:BUFFER-FIELD("MARKNING"):BUFFER-VALUE ="Jag Är Ny Här!".
      THIS-OBJECT:DatabaseHandler:NumTT:BUFFER-FIELD("kalknr"):BUFFER-VALUE ="100284".
      */
      /*THIS-OBJECT:DatabaseHandler:NumTT:FIND-FIRST().*/
/*      IF THIS-OBJECT:DatabaseHandler:NumTT:AVAILABLE EQ TRUE THEN DO:
         
      END.*/
      
      THIS-OBJECT:DatabaseHandler:SparaKalkyl().

      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridNum_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
		THIS-OBJECT:GridNumsub:Gurufiltrera("NUM = " + STRING( THIS-OBJECT:DatabaseHandler:NumTT:BUFFER-FIELD("NUM"):BUFFER-VALUE )).
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
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE gridBagConstraint4 AS Infragistics.Win.Layout.GridBagConstraint NO-UNDO.
      gridBagConstraint4 = NEW Infragistics.Win.Layout.GridBagConstraint().
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
      DEFINE VARIABLE gridBagConstraint1 AS Infragistics.Win.Layout.GridBagConstraint NO-UNDO.
      gridBagConstraint1 = NEW Infragistics.Win.Layout.GridBagConstraint().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE gridBagConstraint2 AS Infragistics.Win.Layout.GridBagConstraint NO-UNDO.
      gridBagConstraint2 = NEW Infragistics.Win.Layout.GridBagConstraint().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE gridBagConstraint3 AS Infragistics.Win.Layout.GridBagConstraint NO-UNDO.
      gridBagConstraint3 = NEW Infragistics.Win.Layout.GridBagConstraint().
      THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:GridNum = NEW Controls.Grid().
      THIS-OBJECT:GridNumsub = NEW Controls.Grid().
      THIS-OBJECT:ultraGridBagLayoutManager1 = NEW Infragistics.Win.Misc.UltraGridBagLayoutManager(THIS-OBJECT:components).
      THIS-OBJECT:ultraGridBagLayoutPanel1 = NEW Infragistics.Win.Misc.UltraGridBagLayoutPanel().
      THIS-OBJECT:ultraGridBagLayoutPanel2 = NEW Infragistics.Win.Misc.UltraGridBagLayoutPanel().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridNum, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridNumsub, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraGridBagLayoutManager1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraGridBagLayoutPanel1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraGridBagLayoutPanel1:SuspendLayout().
      CAST(THIS-OBJECT:ultraGridBagLayoutPanel2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraGridBagLayoutPanel2:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* bindingSource1 */
      /*  */
      THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
      THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
      THIS-OBJECT:bindingSource1:TableSchema = ?.
      /*  */
      /* GridNum */
      /*  */
      appearance1:BackColor = System.Drawing.Color:White.
      THIS-OBJECT:GridNum:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridNum:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridNum:DisplayLayout:MaxRowScrollRegions = 1.
      appearance2:BackColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:GridNum:DisplayLayout:Override:CardAreaAppearance = appearance2.
      THIS-OBJECT:GridNum:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      appearance27:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(214))).
      appearance27:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(7)), System.Convert:ToInt32(System.Convert:ToByte(59)), System.Convert:ToInt32(System.Convert:ToByte(150))).
      appearance27:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance27:FontData:BoldAsString = "True".
      appearance27:FontData:Name = "Arial".
      appearance27:FontData:SizeInPoints = 10.
      appearance27:ForeColor = System.Drawing.Color:White.
      appearance27:ThemedElementAlpha = Infragistics.Win.Alpha:Transparent.
      THIS-OBJECT:GridNum:DisplayLayout:Override:HeaderAppearance = appearance27.
      THIS-OBJECT:GridNum:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      appearance28:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(214))).
      appearance28:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(7)), System.Convert:ToInt32(System.Convert:ToByte(59)), System.Convert:ToInt32(System.Convert:ToByte(150))).
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      THIS-OBJECT:GridNum:DisplayLayout:Override:RowSelectorAppearance = appearance28.
      appearance29:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(251)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(148))).
      appearance29:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(238)), System.Convert:ToInt32(System.Convert:ToByte(149)), System.Convert:ToInt32(System.Convert:ToByte(21))).
      appearance29:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      THIS-OBJECT:GridNum:DisplayLayout:Override:SelectedRowAppearance = appearance29.
      THIS-OBJECT:GridNum:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridNum:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridNum:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridNum:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridNum:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridNum:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      gridBagConstraint4:Fill = Infragistics.Win.Layout.FillType:Both.
      gridBagConstraint4:OriginX = 0.
      gridBagConstraint4:OriginY = 0.
      THIS-OBJECT:ultraGridBagLayoutPanel2:SetGridBagConstraint(THIS-OBJECT:GridNum, gridBagConstraint4).
      THIS-OBJECT:GridNum:GuruContext = ?.
      THIS-OBJECT:GridNum:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridNum:Name = "GridNum".
      THIS-OBJECT:ultraGridBagLayoutPanel2:SetPreferredSize(THIS-OBJECT:GridNum, NEW System.Drawing.Size(298, 240)).
      THIS-OBJECT:GridNum:Size = NEW System.Drawing.Size(361, 314).
      THIS-OBJECT:GridNum:TabIndex = 0.
      THIS-OBJECT:GridNum:AfterSelectChange:Subscribe(THIS-OBJECT:GridNum_AfterSelectChange).
      /*  */
      /* GridNumsub */
      /*  */
      appearance30:BackColor = System.Drawing.Color:White.
      THIS-OBJECT:GridNumsub:DisplayLayout:Appearance = appearance30.
      THIS-OBJECT:GridNumsub:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridNumsub:DisplayLayout:MaxRowScrollRegions = 1.
      appearance31:BackColor = System.Drawing.Color:Transparent.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:CardAreaAppearance = appearance31.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      appearance32:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(214))).
      appearance32:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(7)), System.Convert:ToInt32(System.Convert:ToByte(59)), System.Convert:ToInt32(System.Convert:ToByte(150))).
      appearance32:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance32:FontData:BoldAsString = "True".
      appearance32:FontData:Name = "Arial".
      appearance32:FontData:SizeInPoints = 10.
      appearance32:ForeColor = System.Drawing.Color:White.
      appearance32:ThemedElementAlpha = Infragistics.Win.Alpha:Transparent.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:HeaderAppearance = appearance32.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      appearance33:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(214))).
      appearance33:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(7)), System.Convert:ToInt32(System.Convert:ToByte(59)), System.Convert:ToInt32(System.Convert:ToByte(150))).
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:RowSelectorAppearance = appearance33.
      appearance34:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(251)), System.Convert:ToInt32(System.Convert:ToByte(230)), System.Convert:ToInt32(System.Convert:ToByte(148))).
      appearance34:BackColor2 = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(238)), System.Convert:ToInt32(System.Convert:ToByte(149)), System.Convert:ToInt32(System.Convert:ToByte(21))).
      appearance34:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      THIS-OBJECT:GridNumsub:DisplayLayout:Override:SelectedRowAppearance = appearance34.
      THIS-OBJECT:GridNumsub:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridNumsub:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridNumsub:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridNumsub:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridNumsub:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridNumsub:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      gridBagConstraint1:Fill = Infragistics.Win.Layout.FillType:Both.
      gridBagConstraint1:OriginX = 0.
      gridBagConstraint1:OriginY = 0.
      THIS-OBJECT:ultraGridBagLayoutPanel1:SetGridBagConstraint(THIS-OBJECT:GridNumsub, gridBagConstraint1).
      THIS-OBJECT:GridNumsub:GuruContext = ?.
      THIS-OBJECT:GridNumsub:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridNumsub:Name = "GridNumsub".
      THIS-OBJECT:ultraGridBagLayoutPanel1:SetPreferredSize(THIS-OBJECT:GridNumsub, NEW System.Drawing.Size(283, 240)).
      THIS-OBJECT:GridNumsub:Size = NEW System.Drawing.Size(372, 314).
      THIS-OBJECT:GridNumsub:TabIndex = 1.
      /*  */
      /* ultraGridBagLayoutManager1 */
      /*  */
      THIS-OBJECT:ultraGridBagLayoutManager1:ContainerControl = THIS-OBJECT.
      THIS-OBJECT:ultraGridBagLayoutManager1:ExpandToFitHeight = TRUE.
      THIS-OBJECT:ultraGridBagLayoutManager1:ExpandToFitWidth = TRUE.
      /*  */
      /* ultraGridBagLayoutPanel1 */
      /*  */
      THIS-OBJECT:ultraGridBagLayoutPanel1:Controls:Add(THIS-OBJECT:GridNumsub).
      THIS-OBJECT:ultraGridBagLayoutPanel1:ExpandToFitHeight = TRUE.
      THIS-OBJECT:ultraGridBagLayoutPanel1:ExpandToFitWidth = TRUE.
      gridBagConstraint2:Fill = Infragistics.Win.Layout.FillType:Both.
      THIS-OBJECT:ultraGridBagLayoutManager1:SetGridBagConstraint(THIS-OBJECT:ultraGridBagLayoutPanel1, gridBagConstraint2).
      THIS-OBJECT:ultraGridBagLayoutPanel1:Location = NEW System.Drawing.Point(361, 0).
      THIS-OBJECT:ultraGridBagLayoutPanel1:Name = "ultraGridBagLayoutPanel1".
      THIS-OBJECT:ultraGridBagLayoutManager1:SetPreferredSize(THIS-OBJECT:ultraGridBagLayoutPanel1, NEW System.Drawing.Size(283, 240)).
      THIS-OBJECT:ultraGridBagLayoutPanel1:Size = NEW System.Drawing.Size(372, 314).
      THIS-OBJECT:ultraGridBagLayoutPanel1:TabIndex = 2.
      /*  */
      /* ultraGridBagLayoutPanel2 */
      /*  */
      THIS-OBJECT:ultraGridBagLayoutPanel2:Controls:Add(THIS-OBJECT:GridNum).
      THIS-OBJECT:ultraGridBagLayoutPanel2:ExpandToFitHeight = TRUE.
      THIS-OBJECT:ultraGridBagLayoutPanel2:ExpandToFitWidth = TRUE.
      gridBagConstraint3:Fill = Infragistics.Win.Layout.FillType:Both.
      THIS-OBJECT:ultraGridBagLayoutManager1:SetGridBagConstraint(THIS-OBJECT:ultraGridBagLayoutPanel2, gridBagConstraint3).
      THIS-OBJECT:ultraGridBagLayoutPanel2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraGridBagLayoutPanel2:Name = "ultraGridBagLayoutPanel2".
      THIS-OBJECT:ultraGridBagLayoutManager1:SetPreferredSize(THIS-OBJECT:ultraGridBagLayoutPanel2, NEW System.Drawing.Size(275, 264)).
      THIS-OBJECT:ultraGridBagLayoutPanel2:Size = NEW System.Drawing.Size(361, 314).
      THIS-OBJECT:ultraGridBagLayoutPanel2:TabIndex = 3.
      /*  */
      /* ProDataTest */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(733, 314).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGridBagLayoutPanel2).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGridBagLayoutPanel1).
      THIS-OBJECT:Name = "ProDataTest".
      THIS-OBJECT:Text = "ProDataTest".
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridNum, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridNumsub, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraGridBagLayoutManager1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraGridBagLayoutPanel1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraGridBagLayoutPanel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraGridBagLayoutPanel2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraGridBagLayoutPanel2:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC ProDataTest ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridNum().
      THIS-OBJECT:CreateGridNumsub().
   END METHOD.

   METHOD PUBLIC VOID CreateGridNumsub():
      DEFINE VARIABLE kalkpriserlist     AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkpriserKPID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserKVID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserSokB     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserBenm     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserPris     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserEgenPris AS Controls.GridRubrik      NO-UNDO.
      
      kalkpriserlist = NEW Controls.GridRubrikLista().
      kalkpriserlist:ttBufferHandle = THIS-OBJECT:DatabaseHandler:NumSubTT.
      kalkpriserlist:multiselect = TRUE.
      
      kalkpriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 150).
      kalkpriserlist:addRubrik(kalkpriserBenm).
      
      kalkpriserPris = NEW Controls.GridRubrik("PRIS", "Pris", TRUE, 35).
      kalkpriserlist:addRubrik(kalkpriserPris).
      
      kalkpriserPris = NEW Controls.GridRubrik("NUM", "#", TRUE, 35).
      kalkpriserlist:addRubrik(kalkpriserPris).
      
      THIS-OBJECT:GridNumsub:guruInitGrid(kalkpriserlist).
      THIS-OBJECT:GridNumsub:TEXT = "Underkoder".
      THIS-OBJECT:GridNumsub:Gurufiltrera("NUM = " + STRING( THIS-OBJECT:DatabaseHandler:NumTT:BUFFER-FIELD("NUM"):BUFFER-VALUE )).
   END METHOD.

   METHOD PUBLIC VOID CreateGridNum():
      DEFINE VARIABLE kalkylkoderlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb     AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderMat     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSum     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderMark    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSubmark AS Controls.GridRubrik      NO-UNDO.

      
      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = THIS-OBJECT:DatabaseHandler:NumTT.
      kalkylkoderlist:multiselect = TRUE.
      
      kalkylkoderMat = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35).
      kalkylkoderlist:addRubrik(kalkylkoderMat).
       
      kalkylkoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,55).      
      kalkylkoderlist:addRubrik(kalkylkoderArb).
      
      kalkylkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      kalkylkoderlist:addRubrik(kalkylkoderLop).
      
      kalkylkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      kalkylkoderlist:addRubrik(kalkylkoderEnh).
      
      kalkylkoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").
      kalkylkoderlist:addRubrik(kalkylkoderAnt).
      
      kalkylkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      kalkylkoderlist:addRubrik(kalkylkoderBen).
      
      kalkylkoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      kalkylkoderlist:addRubrik(kalkylkoderSum).
      
      kalkylkoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      kalkylkoderlist:addRubrik(kalkylkoderMark).
      
      kalkylkoderSubmark = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",false,75).
      kalkylkoderlist:addRubrik(kalkylkoderSubmark).
      
      /*init av grid*/
      THIS-OBJECT:GridNum:guruInitGrid(kalkylkoderlist). 
      THIS-OBJECT:GridNum:TEXT = "Kalkylkoder".  
      
   END METHOD.

END CLASS.