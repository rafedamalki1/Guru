 /*------------------------------------------------------------------------
    File        : Register
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue May 15 14:07:11 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
/*
USING Microsoft.Office.Interop.Excel.*.
*/

CLASS Modules.Register.Register INHERITS UserControl: 
	
	/* Includes */
	{SPRAKTEMP.i}
	DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridStrings AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSprak AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   /* Konstruktor & Destruktor*/
   CONSTRUCTOR PUBLIC Register ( INPUT r AS Guru.Root ):
      SUPER().
      THIS-OBJECT:Root = r.
      InitializeComponent().
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:Root:DatabaseManager:sprakdb().
      THIS-OBJECT:Root:DatabaseManager:Sprak:FetchTableHandles().
      THIS-OBJECT:SprakStrangTT = THIS-OBJECT:Root:DatabaseManager:Sprak:SprakStrangTT.
      THIS-OBJECT:SprakTT = THIS-OBJECT:Root:DatabaseManager:Sprak:SprakTT.
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:FetchSprak().
      THIS-OBJECT:RefreshSprakUI().
      

      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END CONSTRUCTOR.
	
   DESTRUCTOR PUBLIC Register ( ):
      IF VALID-OBJECT(THIS-OBJECT:components) THEN DO:
         CAST(THIS-OBJECT:components, System.IDisposable):Dispose().
      END.
   END DESTRUCTOR.
	
   DEFINE PRIVATE VARIABLE ComboToolTip AS System.Windows.Forms.ToolTip NO-UNDO.
	/* Variablar */
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.	
	DEFINE PUBLIC VARIABLE SprakTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE SprakStrangTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE SelectedSprak AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE SelectedSprakChar AS Character NO-UNDO.
	
   DEFINE PUBLIC VARIABLE ModuleHandle AS Guru.Module NO-UNDO.


	/* Metoder */
	
	METHOD PUBLIC LOGICAL Initialize():
	   THIS-OBJECT:CreateRibbon().
	  RETURN TRUE.
   END METHOD.
	
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridSprak().
      THIS-OBJECT:CreateGridStrings().
   END METHOD.
	
	METHOD PUBLIC VOID CreateGridSprak():
	   {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ID", "Id", TRUE, 50).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 150).
      GridSprak:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:SprakTT)).
      THIS-OBJECT:GridSprak:Text = "Språk".
      GridSprak:tBS:PositionChanged:Subscribe(valuechSprak).
   END METHOD.
	
	METHOD PUBLIC VOID CreateGridStrings():
	   {GridRubrikListaVarExtent.i}
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ID", "Id", TRUE, 100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", FALSE, 200).
      rubrikergrid[3] = NEW Controls.GridRubrik("SOKCHAR", "Styrfält", FALSE, 200).
      rubrikergrid[4] = NEW Controls.GridRubrik("SOKID", "Sök-Id", TRUE, 100).
      rubrikergrid[5] = NEW Controls.GridRubrik("SPRAKID", "Språk-Id", TRUE, 100).
      
      
      GridStrings:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:SprakStrangTT)).
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.
      ELSE THIS-OBJECT:GridStrings:DisplayLayout:Bands[0]:Columns["SOKCHAR"]:Hidden = TRUE.
      THIS-OBJECT:GridStrings:Text = "Strängar".
   END METHOD.
	
   METHOD PUBLIC VOID FetchSprak():
      THIS-OBJECT:Root:DatabaseManager:Sprak:FetchEverything().
      THIS-OBJECT:SprakTT = THIS-OBJECT:Root:DatabaseManager:Sprak:SprakTT.
      THIS-OBJECT:SprakStrangTT = THIS-OBJECT:Root:DatabaseManager:Sprak:SprakStrangTT.
   END METHOD.
	
	METHOD PUBLIC VOID SaveAllChanges():
	   THIS-OBJECT:Root:DatabaseManager:Sprak:SaveAllChanges().
   END METHOD.
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	
	METHOD PUBLIC VOID RefreshSprakUI():
	   
      /* Uppdatera Grids osv.*/
      THIS-OBJECT:GridSprak:GuruReopen().
      
      THIS-OBJECT:GridStrings:GuruReopen().
   END METHOD.
/*KÖRS INTE
   METHOD PUBLIC VOID ToExcel():
   
      DEFINE VARIABLE ExcelAppHandle AS Application NO-UNDO.
      DEFINE VARIABLE ExcelWbHandle AS Workbook NO-UNDO.
      DEFINE VARIABLE ExcelSheetHandle AS Worksheet NO-UNDO.
      
      ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      ExcelAppHandle:DisplayAlerts = FALSE.
      ExcelAppHandle:ScreenUpdating = FALSE.
      ExcelAppHandle:VISIBLE = FALSE.
      
      ExcelWbHandle = ExcelAppHandle:ActiveWorkbook.
      ExcelSheetHandle = CAST(ExcelWbHandle:ActiveSheet, Worksheet).
   END METHOD.
*/
	/* Ribbon grejjer! */
	METHOD PUBLIC VOID CreateRibbon():
      /* Ribbontabbar*/
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:NAME).
      THIS-OBJECT:RibbonTabSprak = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sprak").
      THIS-OBJECT:RibbonTabSprak:Caption = "Språk".
      
      THIS-OBJECT:RibbonTabSprakStrang = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("SprakStrang").
      THIS-OBJECT:RibbonTabSprakStrang:Caption = "Strängar".
      
      THIS-OBJECT:RibbonTabUrval = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Urval").
      THIS-OBJECT:RibbonTabUrval:Caption = "Urval".
      
      /* Ribbon tools */
      
      THIS-OBJECT:RibbonToolAddSprak = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AddSprak").
      THIS-OBJECT:RibbonToolAddSprak:SharedPropsInternal:Caption = "Skapa nytt".

      THIS-OBJECT:RibbonToolDelSprak = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("DelSprak").
      THIS-OBJECT:RibbonToolDelSprak:SharedPropsInternal:Caption = "Ta bort".
      
      THIS-OBJECT:RibbonToolAddString = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AddString").
      THIS-OBJECT:RibbonToolAddString:SharedPropsInternal:Caption = "Skapa ny".

      THIS-OBJECT:RibbonToolDelString = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("DelString").
      THIS-OBJECT:RibbonToolDelString:SharedPropsInternal:Caption = "Ta bort".
           
      THIS-OBJECT:RibbonToolUrvalTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("UrvalTyp").
      THIS-OBJECT:RibbonToolUrvalTyp:SharedPropsInternal:Caption = "Visa med".
      
      THIS-OBJECT:RibbonTest =  NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TEST").
      
      /* Preppa och adda*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAddSprak).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelSprak).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAddString).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDelString).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolUrvalTyp).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonTest).
      
      
      
      THIS-OBJECT:RibbonTabSprak:Tools:Add(THIS-OBJECT:RibbonToolAddSprak).
      THIS-OBJECT:RibbonTabSprak:Tools:Add(THIS-OBJECT:RibbonToolDelSprak).
      
      THIS-OBJECT:RibbonTabSprakStrang:Tools:Add(THIS-OBJECT:RibbonToolAddString).
      THIS-OBJECT:RibbonTabSprakStrang:Tools:Add(THIS-OBJECT:RibbonToolDelString).
      THIS-OBJECT:RibbonTabUrval:Tools:Add(THIS-OBJECT:RibbonToolUrvalTyp).
      THIS-OBJECT:RibbonTabSprakStrang:Tools:Add(THIS-OBJECT:RibbonTest).
      
      /* Events */
      THIS-OBJECT:RibbonToolAddSprak:ToolClick:Subscribe(THIS-OBJECT:BtnNewSprak_Click).
      THIS-OBJECT:RibbonToolDelSprak:ToolClick:Subscribe(THIS-OBJECT:BtnDeleteSprak_Click).
      
      THIS-OBJECT:RibbonToolAddString:ToolClick:Subscribe(THIS-OBJECT:BtnNewString_Click).
      THIS-OBJECT:RibbonToolDelString:ToolClick:Subscribe(THIS-OBJECT:BtnDeleteString_Click).
      
      THIS-OBJECT:RibbonTest:ToolClick:Subscribe(THIS-OBJECT:Test_Click).
      
      THIS-OBJECT:RibbonToolUrvalTyp:ToolValueChanged:Subscribe(RibbonToolUrvalTypValue).
      
      THIS-OBJECT:RibbonToolUrvalTyp:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolUrvalTyp:ValueList:ValueListItems:Add("Märkta").
      THIS-OBJECT:RibbonToolUrvalTyp:ValueList:ValueListItems:Add("Ej märkta").
      THIS-OBJECT:RibbonToolUrvalTyp:VALUE = "Alla".
      
      
   END METHOD.
	
	DEFINE PUBLIC VARIABLE RibbonTabSprak AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonTabSprakStrang AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonTabUrval AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
   
   DEFINE PUBLIC VARIABLE RibbonToolAddSprak AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonToolDelSprak AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
	
   DEFINE PUBLIC VARIABLE RibbonToolAddString AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonToolDelString AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonToolUrvalTyp AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonTest AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
   
   
   /* Events */

   METHOD PRIVATE VOID Test_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:SprakStrangTT:FIND-FIRST("WHERE ID = 0 AND SPRAKID = 0", NO-LOCK) NO-ERROR.
      THIS-OBJECT:SprakStrangTT:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "z".
      THIS-OBJECT:SaveAllChanges().
   END METHOD.

   METHOD PRIVATE VOID BtnDeleteSprak_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE r AS System.Windows.Forms.DialogResult NO-UNDO. 
      r = System.Windows.Forms.MessageBox:Show("Vill du verkligen ta bort detta språk? " , "Säker?", System.Windows.Forms.MessageBoxButtons:YesNo).
      IF r:ToString() EQ "Yes" THEN DO:
         idvar = THIS-OBJECT:SprakTT:BUFFER-FIELD("ID"):BUFFER-VALUE.         
         GridSprak:GuruBortrader(FALSE).
         THIS-OBJECT:Root:DatabaseManager:Sprak:DeleteSprak(idvar).        
      END.
      
      RETURN.
      

   END METHOD.

   METHOD PRIVATE VOID BtnDeleteString_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      DEFINE VARIABLE r AS System.Windows.Forms.DialogResult NO-UNDO. 
      r = System.Windows.Forms.MessageBox:Show("Vill du verkligen ta bort denna rad?", "Säker?", System.Windows.Forms.MessageBoxButtons:YesNo).
      IF r:ToString() EQ "Yes" THEN DO:
         
         GridStrings:GuruBortrader(FALSE). 
         THIS-OBJECT:Root:DatabaseManager:Sprak:DeleteStrang(). 
      END.
      
      RETURN.

   END METHOD.
   METHOD PUBLIC VOID RibbonToolUrvalTypValue(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE qGridUtb AS CHARACTER NO-UNDO.
     
      IF STRING(THIS-OBJECT:RibbonToolUrvalTyp:VALUE) = "Alla" THEN DO:
         qGridUtb = " ".
      END.
      IF STRING(THIS-OBJECT:RibbonToolUrvalTyp:VALUE) = "Märkta" THEN DO:
         qGridUtb = " AND SOKCHAR  NE ''" .
      END.
      IF STRING(THIS-OBJECT:RibbonToolUrvalTyp:VALUE) = "Ej märkta" THEN DO:
         qGridUtb = " AND SOKCHAR  = ''" .
      END.   
      THIS-OBJECT:GridStrings:GuruFiltrera(SprakStrangTT:TABLE + ".SPRAKID = " + STRING(THIS-OBJECT:SprakTT:BUFFER-FIELD("ID"):BUFFER-VALUE) + qGridUtb).
      THIS-OBJECT:GridStrings:GuruReopen().
   END METHOD.
   METHOD PRIVATE VOID BtnNewSprak_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO. 
      DEFINE VARIABLE NewDialog AS Modules.Register.NewSprakDialog NO-UNDO.
      NewDialog = NEW Modules.Register.NewSprakDialog().
      
      DEFINE VARIABLE Results AS System.Windows.Forms.DialogResult NO-UNDO.
      
      WAIT-FOR  NewDialog:ShowDialog() SET Results.
      
      
      IF Results:ToString() = "Ok" THEN DO:
         rid = THIS-OBJECT:Root:DatabaseManager:Sprak:NewSprak(NewDialog:Benamning, NewDialog:DoConstant).  
         THIS-OBJECT:GridSprak:GuruReopen().
         THIS-OBJECT:GridSprak:GuruRepositionto(rid).       
      END.

      RETURN.

   END METHOD.
	
   
   METHOD PRIVATE VOID BtnNewString_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.          
      rid = THIS-OBJECT:Root:DatabaseManager:Sprak:NewString( THIS-OBJECT:SelectedSprak, "< Ny sträng .. >", "").
       
      THIS-OBJECT:GridStrings:GuruReopen().
      THIS-OBJECT:GridStrings:GuruRepositionto(rid).

      RETURN.

   END METHOD.
   

	
	

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridStrings_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolvarde = e:Cell:Value:ToString().

      SprakStrangTT:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde. 
      IF SprakStrangTT:BUFFER-FIELD("SPRAKID"):BUFFER-VALUE = 0 THEN DO:
         THIS-OBJECT:Root:DatabaseManager:Sprak:GuruvarSet().         
      END.      
      THIS-OBJECT:Root:DatabaseManager:Sprak:SaveAllChanges().
      
   END METHOD.
   
	METHOD PRIVATE VOID valuechSprak (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:SprakTT:AVAILABLE  THEN  DO:
              
         THIS-OBJECT:GridStrings:GuruFiltrera(SprakStrangTT:TABLE + ".SPRAKID = " + STRING(THIS-OBJECT:SprakTT:BUFFER-FIELD("ID"):BUFFER-VALUE)).
        
      END.
         
   END METHOD.
		


	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridSprak = NEW Controls.Grid().
      THIS-OBJECT:GridStrings = NEW Controls.Grid().
      THIS-OBJECT:ComboToolTip = NEW System.Windows.Forms.ToolTip(THIS-OBJECT:components).
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:GridSprak, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridStrings, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(949, 584).
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:GridSprak).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:GridStrings).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(949, 584).
      THIS-OBJECT:splitContainer1:SplitterDistance = 272.
      THIS-OBJECT:splitContainer1:TabIndex = 3.
      /*  */
      /* GridSprak */
      /*  */
      appearance3:BackColor = System.Drawing.SystemColors:Window.
      appearance3:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSprak:DisplayLayout:Appearance = appearance3.
      THIS-OBJECT:GridSprak:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSprak:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance4:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSprak:DisplayLayout:GroupByBox:Appearance = appearance4.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSprak:DisplayLayout:GroupByBox:BandLabelAppearance = appearance5.
      THIS-OBJECT:GridSprak:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance6:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance6:BackColor2 = System.Drawing.SystemColors:Control.
      appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSprak:DisplayLayout:GroupByBox:PromptAppearance = appearance6.
      THIS-OBJECT:GridSprak:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSprak:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      appearance7:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:ActiveCellAppearance = appearance7.
      appearance8:BackColor = System.Drawing.SystemColors:Highlight.
      appearance8:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:ActiveRowAppearance = appearance8.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance9:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:CardAreaAppearance = appearance9.
      appearance10:BorderColor = System.Drawing.Color:Silver.
      appearance10:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:CellAppearance = appearance10.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:CellPadding = 0.
      appearance11:BackColor = System.Drawing.SystemColors:Control.
      appearance11:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance11:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance11:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance11:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:GroupByRowAppearance = appearance11.
      appearance12:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSprak:DisplayLayout:Override:HeaderAppearance = appearance12.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:RowAppearance = appearance13.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSprak:DisplayLayout:Override:TemplateAddRowAppearance = appearance14.
      THIS-OBJECT:GridSprak:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSprak:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSprak:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSprak:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSprak:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSprak:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSprak:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSprak:GuruContext = ?.
      THIS-OBJECT:GridSprak:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSprak:Name = "GridSprak".
      THIS-OBJECT:GridSprak:Size = NEW System.Drawing.Size(272, 584).
      THIS-OBJECT:GridSprak:TabIndex = 0.
      /*  */
      /* GridStrings */
      /*  */
      appearance40:BackColor = System.Drawing.SystemColors:Window.
      appearance40:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridStrings:DisplayLayout:Appearance = appearance40.
      THIS-OBJECT:GridStrings:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridStrings:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance41:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance41:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance41:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStrings:DisplayLayout:GroupByBox:Appearance = appearance41.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStrings:DisplayLayout:GroupByBox:BandLabelAppearance = appearance42.
      THIS-OBJECT:GridStrings:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance43:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance43:BackColor2 = System.Drawing.SystemColors:Control.
      appearance43:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance43:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridStrings:DisplayLayout:GroupByBox:PromptAppearance = appearance43.
      THIS-OBJECT:GridStrings:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridStrings:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance44:BackColor = System.Drawing.SystemColors:Window.
      appearance44:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:ActiveCellAppearance = appearance44.
      appearance45:BackColor = System.Drawing.SystemColors:Highlight.
      appearance45:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:ActiveRowAppearance = appearance45.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance46:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:CardAreaAppearance = appearance46.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      appearance47:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:CellAppearance = appearance47.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:CellPadding = 0.
      appearance48:BackColor = System.Drawing.SystemColors:Control.
      appearance48:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance48:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance48:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance48:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:GroupByRowAppearance = appearance48.
      appearance49:TextHAlignAsString = "Left".
      THIS-OBJECT:GridStrings:DisplayLayout:Override:HeaderAppearance = appearance49.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:RowAppearance = appearance50.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance51:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridStrings:DisplayLayout:Override:TemplateAddRowAppearance = appearance51.
      THIS-OBJECT:GridStrings:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridStrings:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridStrings:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridStrings:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridStrings:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridStrings:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridStrings:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridStrings:GuruContext = ?.
      THIS-OBJECT:GridStrings:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridStrings:Name = "GridStrings".
      THIS-OBJECT:GridStrings:Size = NEW System.Drawing.Size(673, 584).
      THIS-OBJECT:GridStrings:TabIndex = 0.
      THIS-OBJECT:GridStrings:AfterCellUpdate:Subscribe(THIS-OBJECT:GridStrings_AfterCellUpdate).
      /*  */
      /* TabManager */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance1.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(953, 610).
      THIS-OBJECT:TabManager:TabIndex = 1.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
      ultraTab1:Text = "Språkhantering".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
      arrayvar0[1] = ultraTab1.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar0).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(949, 584).
      /*  */
      /* Register */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "Register".
      THIS-OBJECT:Size = NEW System.Drawing.Size(953, 610).
      THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSprak, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridStrings, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.



END CLASS.
