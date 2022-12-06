 
 /*------------------------------------------------------------------------
    File        : KalkylAdmPris
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 30 15:25:30 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.KalkylAdm.KalkylAdmPris INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkAdmdbControl  AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   
   
   DEFINE PUBLIC VARIABLE ButtonNyPrisStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortPrisStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVisningBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVisningBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPrisStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatalogSubKatStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatalog AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE VisningBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE PrisStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridVisning AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSubKatalog AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKatPris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE DelKatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkylAdmPris (INPUT r AS Guru.Root ):
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkAdmdbControl = THIS-OBJECT:Root:DatabaseManager:KalkylAdm.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortPrisStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridKatPris:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortPris(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,INPUT INTEGER(STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KPID"]:VALUE))). 
      THIS-OBJECT:GridKatPris:GuruBortrader(FALSE).
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
         
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyPrisStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nykpid AS INTEGER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyPris(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,OUTPUT nykpid).
      THIS-OBJECT:KalkAdmdbControl:PriserTTh:FIND-FIRST("WHERE KPID = " + STRING(nykpid) + " AND KLOGSUBID = " + STRING(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE))  NO-ERROR.
      THIS-OBJECT:GridKatPris:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:PriserTTh:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatPris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKatPris:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN DO:
         queryvar = "KLOGSUBID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KLOGSUBID"]:VALUE) + " AND " + 
                    "KPID = " + STRING(THIS-OBJECT:GridKatPris:ActiveRow:Cells["KPID"]:VALUE).
         THIS-OBJECT:ControlShell:KatalogPrisUppdate(INPUT queryvar, INPUT kolnamn, INPUT kolvarde).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatPris_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatalog_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridSubKatalog:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatalog:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridVisning_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridVisning:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkAdmdbControl:VisningTTh:FIND-FIRST("WHERE KVID = '" + STRING( THIS-OBJECT:GridVisning:ActiveRow:Cells["KVID"]:VALUE )) NO-ERROR.
      THIS-OBJECT:KalkAdmdbControl:VisningTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
		RETURN.

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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmPris").
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
      THIS-OBJECT:splitContainerKatalog = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatalogSubKatStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:DelKatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridSubKatalog = NEW Controls.Grid().
      THIS-OBJECT:splitContainerPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPrisStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:PrisStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyPrisStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortPrisStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridKatPris = NEW Controls.Grid().
      THIS-OBJECT:splitContainerVisningBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerVisningBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:VisningBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridVisning = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatalog:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatalogSubKatStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPrisStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPrisStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPrisStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPrisStrip:SuspendLayout().
      THIS-OBJECT:PrisStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatPris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerVisningBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisningBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerVisningBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridVisning, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKatalog */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalog:Name = "splitContainerKatalog".
      /*  */
      /* splitContainerKatalog.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatalogSubKatStrip).
      /*  */
      /* splitContainerKatalog.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel2:Controls:Add(THIS-OBJECT:splitContainerPris).
      THIS-OBJECT:splitContainerKatalog:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:splitContainerKatalog:SplitterDistance = 379.
      THIS-OBJECT:splitContainerKatalog:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatalog:TabIndex = 1.
      /*  */
      /* splitContainerKatalogSubKatStrip */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Name = "splitContainerKatalogSubKatStrip".
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatalogSubKatStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:Controls:Add(THIS-OBJECT:DelKatStrip).
      /*  */
      /* splitContainerKatalogSubKatStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubKatalog).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Size = NEW System.Drawing.Size(379, 556).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatalogSubKatStrip:TabIndex = 1.
      /*  */
      /* DelKatStrip */
      /*  */
      THIS-OBJECT:DelKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:DelKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:DelKatStrip:Name = "DelKatStrip".
      THIS-OBJECT:DelKatStrip:Size = NEW System.Drawing.Size(379, 27).
      THIS-OBJECT:DelKatStrip:TabIndex = 2.
      THIS-OBJECT:DelKatStrip:Text = "toolStrip1".
      /*  */
      /* GridSubKatalog */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatalog:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatalog:GuruContext = ?.
      THIS-OBJECT:GridSubKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatalog:Name = "GridSubKatalog".
      THIS-OBJECT:GridSubKatalog:Size = NEW System.Drawing.Size(379, 528).
      THIS-OBJECT:GridSubKatalog:TabIndex = 0.
      THIS-OBJECT:GridSubKatalog:Text = "Grid1".
      THIS-OBJECT:GridSubKatalog:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatalog_AfterCellUpdate).
      /*  */
      /* splitContainerPris */
      /*  */
      THIS-OBJECT:splitContainerPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPris:Name = "splitContainerPris".
      /*  */
      /* splitContainerPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerPrisStrip).
      /*  */
      /* splitContainerPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPris:Panel2:Controls:Add(THIS-OBJECT:splitContainerVisningBild).
      THIS-OBJECT:splitContainerPris:Size = NEW System.Drawing.Size(673, 556).
      THIS-OBJECT:splitContainerPris:SplitterDistance = 328.
      THIS-OBJECT:splitContainerPris:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPris:TabIndex = 0.
      /*  */
      /* splitContainerPrisStrip */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPrisStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerPrisStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPrisStrip:Name = "splitContainerPrisStrip".
      THIS-OBJECT:splitContainerPrisStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerPrisStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Panel1:Controls:Add(THIS-OBJECT:PrisStrip).
      THIS-OBJECT:splitContainerPrisStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerPrisStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPrisStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatPris).
      THIS-OBJECT:splitContainerPrisStrip:Size = NEW System.Drawing.Size(328, 556).
      THIS-OBJECT:splitContainerPrisStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerPrisStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPrisStrip:TabIndex = 1.
      /*  */
      /* PrisStrip */
      /*  */
      THIS-OBJECT:PrisStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonNyPrisStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonBortPrisStrip.
      THIS-OBJECT:PrisStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:PrisStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:PrisStrip:Name = "PrisStrip".
      THIS-OBJECT:PrisStrip:Size = NEW System.Drawing.Size(328, 27).
      THIS-OBJECT:PrisStrip:TabIndex = 3.
      THIS-OBJECT:PrisStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyPrisStrip */
      /*  */
      THIS-OBJECT:ButtonNyPrisStrip:Image = CAST(resources:GetObject("ButtonNyPrisStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyPrisStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyPrisStrip:Name = "ButtonNyPrisStrip".
      THIS-OBJECT:ButtonNyPrisStrip:Size = NEW System.Drawing.Size(91, 24).
      THIS-OBJECT:ButtonNyPrisStrip:Text = "Lägg till pris".
      THIS-OBJECT:ButtonNyPrisStrip:Click:Subscribe(THIS-OBJECT:ButtonNyPrisStrip_Click).
      /*  */
      /* ButtonBortPrisStrip */
      /*  */
      THIS-OBJECT:ButtonBortPrisStrip:Image = CAST(resources:GetObject("ButtonBortPrisStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortPrisStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortPrisStrip:Name = "ButtonBortPrisStrip".
      THIS-OBJECT:ButtonBortPrisStrip:Size = NEW System.Drawing.Size(87, 24).
      THIS-OBJECT:ButtonBortPrisStrip:Text = "Ta bort pris".
      THIS-OBJECT:ButtonBortPrisStrip:Click:Subscribe(THIS-OBJECT:ButtonBortPrisStrip_Click).
      /*  */
      /* GridKatPris */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatPris:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridKatPris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatPris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatPris:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridKatPris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatPris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatPris:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridKatPris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatPris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatPris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatPris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatPris:GuruContext = ?.
      THIS-OBJECT:GridKatPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatPris:Name = "GridKatPris".
      THIS-OBJECT:GridKatPris:Size = NEW System.Drawing.Size(328, 528).
      THIS-OBJECT:GridKatPris:TabIndex = 0.
      THIS-OBJECT:GridKatPris:Text = "Grid1".
      THIS-OBJECT:GridKatPris:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatPris_AfterCellUpdate).
      THIS-OBJECT:GridKatPris:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridKatPris_BeforeCellUpdate).
      /*  */
      /* splitContainerVisningBild */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisningBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisningBild:Name = "splitContainerVisningBild".
      /*  */
      /* splitContainerVisningBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerVisningBildStrip).
      /*  */
      /* splitContainerVisningBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerVisningBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerVisningBild:Size = NEW System.Drawing.Size(344, 556).
      THIS-OBJECT:splitContainerVisningBild:SplitterDistance = 296.
      THIS-OBJECT:splitContainerVisningBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerVisningBild:TabIndex = 0.
      /*  */
      /* splitContainerVisningBildStrip */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisningBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerVisningBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisningBildStrip:Name = "splitContainerVisningBildStrip".
      THIS-OBJECT:splitContainerVisningBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerVisningBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:Controls:Add(THIS-OBJECT:VisningBildStrip).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerVisningBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:Controls:Add(THIS-OBJECT:GridVisning).
      THIS-OBJECT:splitContainerVisningBildStrip:Size = NEW System.Drawing.Size(296, 556).
      THIS-OBJECT:splitContainerVisningBildStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerVisningBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerVisningBildStrip:TabIndex = 1.
      /*  */
      /* VisningBildStrip */
      /*  */
      THIS-OBJECT:VisningBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisningBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisningBildStrip:Name = "VisningBildStrip".
      THIS-OBJECT:VisningBildStrip:Size = NEW System.Drawing.Size(296, 27).
      THIS-OBJECT:VisningBildStrip:TabIndex = 4.
      THIS-OBJECT:VisningBildStrip:Text = "toolStrip1".
      /*  */
      /* GridVisning */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridVisning:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridVisning:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridVisning:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVisning:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridVisning:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridVisning:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridVisning:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridVisning:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridVisning:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridVisning:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridVisning:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridVisning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridVisning:GuruContext = ?.
      THIS-OBJECT:GridVisning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridVisning:Name = "GridVisning".
      THIS-OBJECT:GridVisning:Size = NEW System.Drawing.Size(296, 528).
      THIS-OBJECT:GridVisning:TabIndex = 0.
      THIS-OBJECT:GridVisning:Text = "Grid1".
      THIS-OBJECT:GridVisning:AfterCellUpdate:Subscribe(THIS-OBJECT:GridVisning_AfterCellUpdate).
      /*  */
      /* KalkylAdmPris */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerKatalog).
      THIS-OBJECT:Name = "KalkylAdmPris".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:splitContainerKatalog:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatalog:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatalogSubKatStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatalogSubKatStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSubKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerPrisStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPrisStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisStrip:ResumeLayout(FALSE).
      THIS-OBJECT:PrisStrip:ResumeLayout(FALSE).
      THIS-OBJECT:PrisStrip:PerformLayout().
      CAST(THIS-OBJECT:GridKatPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisningBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisningBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerVisningBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisningBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisningBildStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridVisning, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC KalkylAdmPris ( ):

	END DESTRUCTOR.

END CLASS.