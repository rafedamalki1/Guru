 
 /*------------------------------------------------------------------------
    File        : CaseForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Thu Nov 13 11:44:21 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS TesterTester.CaseForm INHERITS Form: 
	
    DEFINE TEMP-TABLE MyTableTT
        FIELD ID AS INTEGER
        FIELD USERNAME AS CHARACTER
        FIELD PASSWORD AS CHARACTER.

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE MyGrid AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE MySource AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE MyQuery AS HANDLE NO-UNDO.
    DEFINE PRIVATE VARIABLE MyTable AS HANDLE NO-UNDO.
		
	CONSTRUCTOR PUBLIC CaseForm (  ):
		
		
        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        
        THIS-OBJECT:FillTable().
        
        THIS-OBJECT:MyTable = TEMP-TABLE MyTableTT:DEFAULT-BUFFER-HANDLE.
        
        CREATE QUERY THIS-OBJECT:MyQuery.
        THIS-OBJECT:MyQuery:SET-BUFFERS(MyTable).
        THIS-OBJECT:MyQuery:QUERY-PREPARE("FOR EACH MyTableTT").
        THIS-OBJECT:MyQuery:QUERY-OPEN().
        
        
        THIS-OBJECT:MySource = NEW Progress.Data.BindingSource(THIS-OBJECT:MyQuery, "*", "").
        THIS-OBJECT:MyGrid:DataSource = THIS-OBJECT:MySource.
        
        THIS-OBJECT:SetSelectionStuff().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.
	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID MyGrid_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
	   
		IF e:KeyCode:ToString() EQ "delete" THEN DO:
		   System.Windows.Forms.MessageBox:Show(STRING(MyGrid:Selected:Rows:Count)).
		    MyGrid:DeleteSelectedRows().
	   END.
		RETURN.

	END METHOD.

	METHOD PUBLIC VOID SetSelectionStuff():
	   THIS-OBJECT:MyGrid:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
	   THIS-OBJECT:MyGrid:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
	   
	   /* What we currently have */
      THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[0]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
      THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[0]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
      THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[1]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
      THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[1]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
      
      /* What "fixes" it, although we dont want to be able to select individual cells (as stated above in SelectTypeCell).
      Without RowSelect as a CellClickAction you are able to select more than 1 row even if SelectTypeRow is set to single.
      You can select multiple rows by for example dragging in the grid. Even with RowSelect not set you are able to do this 
      if you try hard enough (happens sometimes). */
      THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[2]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
      /*THIS-OBJECT:MyGrid:DisplayLayout:Bands[0]:Columns[2]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.*/      
   END METHOD.
	
	METHOD PUBLIC VOID FillTable():
	   DEFINE VARIABLE counter AS INTEGER INITIAL 0 NO-UNDO.
	   DO WHILE counter LT 32:
   	   CREATE MyTableTT.
   	   ASSIGN
   	     MyTableTT.ID = counter
   	     MyTableTT.Username = "Foo"
   	     MyTableTT.Password = "Bar".
   	     
   	  counter = counter + 1.
      END.
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
      THIS-OBJECT:MyGrid = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
      CAST(THIS-OBJECT:MyGrid, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* MyGrid */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:MyGrid:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:MyGrid:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:MyGrid:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:MyGrid:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:MyGrid:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:MyGrid:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:MyGrid:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:MyGrid:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:MyGrid:DisplayLayout:MaxRowScrollRegions = 1.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:MyGrid:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:MyGrid:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:MyGrid:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:MyGrid:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:MyGrid:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:MyGrid:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:MyGrid:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:MyGrid:Name = "MyGrid".
      THIS-OBJECT:MyGrid:Size = NEW System.Drawing.Size(292, 266).
      THIS-OBJECT:MyGrid:TabIndex = 0.
      THIS-OBJECT:MyGrid:Text = "ultraGrid1".
      THIS-OBJECT:MyGrid:KeyDown:Subscribe(THIS-OBJECT:MyGrid_KeyDown).
      /*  */
      /* CaseForm */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:MyGrid).
      THIS-OBJECT:Name = "CaseForm".
      THIS-OBJECT:Text = "CaseForm".
      CAST(THIS-OBJECT:MyGrid, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC CaseForm ( ):

	END DESTRUCTOR.

END CLASS.