 
 /*------------------------------------------------------------------------
    File        : KalkylAdmSek
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 30 15:35:43 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.KalkylAdm.KalkylAdmSek INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkAdmdbControl  AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonOverAnvMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBackAnvMark AS Controls.ButtonOver NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKnappAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerBildSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridValdaAnvSek AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKatSek AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAnvSek AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkylAdmSek ( INPUT r AS Guru.Root ):
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
	METHOD PRIVATE VOID ButtonBackAnvMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridValdaAnvSek:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      
      THIS-OBJECT:ButtonBackAnvMark:GuruOver().
      /*Robin Sjöberg Elpool i Umeå AB  23 apr 2015 13:23:08 
      Alternativ lösning för sekretess bockning
      
      IF THIS-OBJECT:GridValdaAnvSek:Posn() = -1 THEN DO:
         THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE), INPUT "SEKRETESS", INPUT "FALSE").
         
      END.
      */

      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveAnvSelected().
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      
      THIS-OBJECT:ControlShell:DontCheck = FALSE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverAnvMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:GridAnvSek:Posn() = -1 THEN RETURN.
       IF THIS-OBJECT:ButtonOverAnvMark:RadFromGrid() = TRUE THEN DO:
         /*Robin Sjöberg Elpool i Umeå AB  23 apr 2015 12:51:34 
         Alternativ lösning för sekretess bockning
         IF THIS-OBJECT:GridValdaAnvSek:Posn() = -1 THEN DO:
            THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE), INPUT "SEKRETESS", INPUT "TRUE").
         END.
         */

         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:AddAnvSelected().
         THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
         
         THIS-OBJECT:ButtonOverAnvMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateAnvRows().
         THIS-OBJECT:ButtonOverAnvMark:GuruOverefteruppdat().
      END.
      ELSE DO:
         DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnvSek_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaAnvSek THEN DO:
         THIS-OBJECT:ButtonBackAnvMark_Click(?,?).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatSek_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKatSek:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatSek:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdaAnvSek_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridAnvSek THEN DO:
         THIS-OBJECT:ButtonOverAnvMark_Click(?,?).
      END.
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmSek").
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
      THIS-OBJECT:splitContainerKatSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKatSek = NEW Controls.Grid().
      THIS-OBJECT:splitContainerAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAnvSek = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKnappAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonOverAnvMark = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackAnvMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerBildSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridValdaAnvSek = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerKatSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:SuspendLayout().
      CAST(THIS-OBJECT:GridKatSek, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:SuspendLayout().
      CAST(THIS-OBJECT:GridAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKnappAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBildSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBildSek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBildSek:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaAnvSek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKatSek */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatSek:Name = "splitContainerKatSek".
      /*  */
      /* splitContainerKatSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Panel1:Controls:Add(THIS-OBJECT:GridKatSek).
      /*  */
      /* splitContainerKatSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvSek).
      THIS-OBJECT:splitContainerKatSek:Size = NEW System.Drawing.Size(1056, 556).
      THIS-OBJECT:splitContainerKatSek:SplitterDistance = 378.
      THIS-OBJECT:splitContainerKatSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatSek:TabIndex = 1.
      /*  */
      /* GridKatSek */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatSek:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridKatSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatSek:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridKatSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridKatSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatSek:GuruContext = ?.
      THIS-OBJECT:GridKatSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatSek:Name = "GridKatSek".
      THIS-OBJECT:GridKatSek:Size = NEW System.Drawing.Size(378, 556).
      THIS-OBJECT:GridKatSek:TabIndex = 0.
      THIS-OBJECT:GridKatSek:Text = "Grid1".
      THIS-OBJECT:GridKatSek:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatSek_AfterCellUpdate).
      /*  */
      /* splitContainerAnvSek */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvSek:Name = "splitContainerAnvSek".
      /*  */
      /* splitContainerAnvSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Panel1:Controls:Add(THIS-OBJECT:GridAnvSek).
      /*  */
      /* splitContainerAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKnappAnvSek).
      THIS-OBJECT:splitContainerAnvSek:Size = NEW System.Drawing.Size(677, 556).
      THIS-OBJECT:splitContainerAnvSek:SplitterDistance = 220.
      THIS-OBJECT:splitContainerAnvSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerAnvSek:TabIndex = 0.
      /*  */
      /* GridAnvSek */
      /*  */
      THIS-OBJECT:GridAnvSek:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridAnvSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnvSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridAnvSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnvSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnvSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnvSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnvSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnvSek:GuruContext = ?.
      THIS-OBJECT:GridAnvSek:GuruDraggable = TRUE.
      THIS-OBJECT:GridAnvSek:GuruDroppable = TRUE.
      THIS-OBJECT:GridAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnvSek:Name = "GridAnvSek".
      THIS-OBJECT:GridAnvSek:Size = NEW System.Drawing.Size(220, 556).
      THIS-OBJECT:GridAnvSek:TabIndex = 1.
      THIS-OBJECT:GridAnvSek:Text = "Grid1".
      THIS-OBJECT:GridAnvSek:GuruAfterDropped:Subscribe(THIS-OBJECT:GridAnvSek_GuruAfterDropped).
      /*  */
      /* splitContainerKnappAnvSek */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKnappAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKnappAnvSek:Name = "splitContainerKnappAnvSek".
      /*  */
      /* splitContainerKnappAnvSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:Controls:Add(THIS-OBJECT:ButtonOverAnvMark).
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:Controls:Add(THIS-OBJECT:ButtonBackAnvMark).
      /*  */
      /* splitContainerKnappAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerBildSek).
      THIS-OBJECT:splitContainerKnappAnvSek:Size = NEW System.Drawing.Size(456, 556).
      THIS-OBJECT:splitContainerKnappAnvSek:SplitterDistance = 31.
      THIS-OBJECT:splitContainerKnappAnvSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKnappAnvSek:TabIndex = 0.
      /*  */
      /* ButtonOverAnvMark */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:ButtonOverAnvMark:Appearance = appearance25.
      THIS-OBJECT:ButtonOverAnvMark:Location = NEW System.Drawing.Point(0, 137).
      THIS-OBJECT:ButtonOverAnvMark:Name = "ButtonOverAnvMark".
      THIS-OBJECT:ButtonOverAnvMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverAnvMark:TabIndex = 10.
      THIS-OBJECT:ButtonOverAnvMark:Click:Subscribe(THIS-OBJECT:ButtonOverAnvMark_Click).
      /*  */
      /* ButtonBackAnvMark */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:ButtonBackAnvMark:Appearance = appearance26.
      THIS-OBJECT:ButtonBackAnvMark:Location = NEW System.Drawing.Point(0, 168).
      THIS-OBJECT:ButtonBackAnvMark:Name = "ButtonBackAnvMark".
      THIS-OBJECT:ButtonBackAnvMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackAnvMark:TabIndex = 11.
      THIS-OBJECT:ButtonBackAnvMark:Click:Subscribe(THIS-OBJECT:ButtonBackAnvMark_Click).
      /*  */
      /* splitContainerBildSek */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBildSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBildSek:Name = "splitContainerBildSek".
      /*  */
      /* splitContainerBildSek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Panel1:Controls:Add(THIS-OBJECT:GridValdaAnvSek).
      /*  */
      /* splitContainerBildSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerBildSek.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerBildSek:Size = NEW System.Drawing.Size(424, 556).
      THIS-OBJECT:splitContainerBildSek:SplitterDistance = 224.
      THIS-OBJECT:splitContainerBildSek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerBildSek:TabIndex = 0.
      /*  */
      /* GridValdaAnvSek */
      /*  */
      THIS-OBJECT:GridValdaAnvSek:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaAnvSek:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaAnvSek:GuruContext = ?.
      THIS-OBJECT:GridValdaAnvSek:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaAnvSek:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaAnvSek:Name = "GridValdaAnvSek".
      THIS-OBJECT:GridValdaAnvSek:Size = NEW System.Drawing.Size(224, 556).
      THIS-OBJECT:GridValdaAnvSek:TabIndex = 2.
      THIS-OBJECT:GridValdaAnvSek:Text = "Grid1".
      THIS-OBJECT:GridValdaAnvSek:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaAnvSek_GuruAfterDropped).
      /*  */
      /* KalkylAdmSek */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerKatSek).
      THIS-OBJECT:Name = "KalkylAdmSek".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1056, 556).
      THIS-OBJECT:splitContainerKatSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKatSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKnappAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKnappAnvSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBildSek:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBildSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBildSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaAnvSek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC KalkylAdmSek ( ):

	END DESTRUCTOR.

END CLASS.