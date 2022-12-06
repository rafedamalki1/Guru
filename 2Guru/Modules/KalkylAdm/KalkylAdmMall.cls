 
 /*------------------------------------------------------------------------
    File        : KalkylAdmMall
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 30 15:31:35 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.KalkylAdm.KalkylAdmMall INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkAdmdbControl  AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonOverMall AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonNyMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKopMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortMallStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBackMall AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE ArbkodMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerValdMallarBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerValdMallarBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyKopMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerMallarBas AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopnrMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopnrMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerBtnOverMall AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerArbkodMallarStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerArbkodMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE ValdMallarBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE SubKatMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE NyKopMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE LopnrMallarStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridValdMallar AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSubMall AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridMallar AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopMall AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbKodMall AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkylAdmMall ( INPUT r AS Guru.Root ):
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
	METHOD PRIVATE VOID ButtonBackMall_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		/*OM INGA KVAR*/
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridValdMallar:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveKodMallarSelected().
      THIS-OBJECT:ButtonBackMall:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
      THIS-OBJECT:ControlShell:DontCheck = FALSE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridMallar:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortMallar(THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE). 
      THIS-OBJECT:GridMallar:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:MallarSpara().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nymallar AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridMallar:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyMallar(INPUT INTEGER(STRING(THIS-OBJECT:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE)),OUTPUT nymallar).
      THIS-OBJECT:GridMallar:GuruReopen().
      THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(nymallar))  NO-ERROR.
      THIS-OBJECT:GridMallar:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyMallStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nymallnr AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyMall(OUTPUT nymallnr).
      THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:FIND-FIRST("WHERE MALLNR = " + STRING(nymallnr))  NO-ERROR.
      THIS-OBJECT:GridMallar:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:KalkmallHuvudtth:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverMall_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:GridMallar:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridLopMall:Posn() = -1 THEN RETURN.
         THIS-OBJECT:ButtonOverMall:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateKoderMallarRows().
         THIS-OBJECT:ButtonOverMall:GuruOverefteruppdat().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopMall_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdMallar THEN DO:
         THIS-OBJECT:ButtonBackMall_Click(?,?).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridMallar_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridMallar:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:ControlShell:MallarUppdate(INPUT STRING(THIS-OBJECT:GridMallar:ActiveRow:Cells["MALLNR"]:VALUE), INPUT kolnamn, INPUT kolvarde).
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdMallar_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridValdMallar:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:ControlShell:MallarValdaUppdate(INPUT STRING(THIS-OBJECT:GridValdMallar:ActiveRow:Cells["ARBKOD"]:VALUE),INPUT STRING(THIS-OBJECT:GridValdMallar:ActiveRow:Cells["LOPNR"]:VALUE), INPUT kolnamn, INPUT kolvarde).
      RETURN.
		

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopMall THEN DO:
         THIS-OBJECT:ButtonOverMall_Click(?,?).
      END.
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmMall").
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
      appearance39 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
      appearance55 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
      appearance56 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
      appearance57 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
      appearance58 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance59 AS Infragistics.Win.Appearance NO-UNDO.
      appearance59 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance60 AS Infragistics.Win.Appearance NO-UNDO.
      appearance60 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance61 AS Infragistics.Win.Appearance NO-UNDO.
      appearance61 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
      appearance62 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerMallarBas = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:NyKopMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortMallStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridMallar = NEW Controls.Grid().
      THIS-OBJECT:splitContainerSubKatMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:SubKatMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridSubMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerArbkodMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerArbkodMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ArbkodMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridArbKodMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnrMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopnrMallarStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopnrMallarStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridLopMall = NEW Controls.Grid().
      THIS-OBJECT:splitContainerBtnOverMall = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonOverMall = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackMall = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerValdMallarBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerValdMallarBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ValdMallarBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridValdMallar = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerMallarBas, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMallarBas:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMallarBas:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMallarBas:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopMallar:SuspendLayout().
      THIS-OBJECT:NyKopMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerSubKatMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerArbkodMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerArbkodMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridArbKodMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopnrMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopnrMallarStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridLopMall, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerBtnOverMall, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBtnOverMall:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdMallarBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdMallarBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridValdMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerMallarBas */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMallarBas:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMallarBas:Name = "splitContainerMallarBas".
      /*  */
      /* splitContainerMallarBas.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopMallar).
      /*  */
      /* splitContainerMallarBas.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMallarBas:Panel2:Controls:Add(THIS-OBJECT:splitContainerSubKatMallar).
      THIS-OBJECT:splitContainerMallarBas:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:splitContainerMallarBas:SplitterDistance = 197.
      THIS-OBJECT:splitContainerMallarBas:SplitterWidth = 1.
      THIS-OBJECT:splitContainerMallarBas:TabIndex = 1.
      /*  */
      /* splitContainerNyKopMallar */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopMallar:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopMallar:Name = "splitContainerNyKopMallar".
      THIS-OBJECT:splitContainerNyKopMallar:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:Controls:Add(THIS-OBJECT:NyKopMallarStrip).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:Controls:Add(THIS-OBJECT:GridMallar).
      THIS-OBJECT:splitContainerNyKopMallar:Size = NEW System.Drawing.Size(197, 556).
      THIS-OBJECT:splitContainerNyKopMallar:SplitterDistance = 27.
      THIS-OBJECT:splitContainerNyKopMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopMallar:TabIndex = 0.
      /*  */
      /* NyKopMallarStrip */
      /*  */
      THIS-OBJECT:NyKopMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonNyMallStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonKopMallStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonBortMallStrip.
      THIS-OBJECT:NyKopMallarStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:NyKopMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:NyKopMallarStrip:Name = "NyKopMallarStrip".
      THIS-OBJECT:NyKopMallarStrip:Size = NEW System.Drawing.Size(197, 27).
      THIS-OBJECT:NyKopMallarStrip:TabIndex = 1.
      THIS-OBJECT:NyKopMallarStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyMallStrip */
      /*  */
      THIS-OBJECT:ButtonNyMallStrip:Image = CAST(resources:GetObject("ButtonNyMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyMallStrip:Name = "ButtonNyMallStrip".
      THIS-OBJECT:ButtonNyMallStrip:Size = NEW System.Drawing.Size(95, 24).
      THIS-OBJECT:ButtonNyMallStrip:Text = "Lägg till mall".
      THIS-OBJECT:ButtonNyMallStrip:Click:Subscribe(THIS-OBJECT:ButtonNyMallStrip_Click).
      /*  */
      /* ButtonKopMallStrip */
      /*  */
      THIS-OBJECT:ButtonKopMallStrip:Image = CAST(resources:GetObject("ButtonKopMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopMallStrip:Name = "ButtonKopMallStrip".
      THIS-OBJECT:ButtonKopMallStrip:Size = NEW System.Drawing.Size(93, 20).
      THIS-OBJECT:ButtonKopMallStrip:Text = "Kopiera mall".
      THIS-OBJECT:ButtonKopMallStrip:Click:Subscribe(THIS-OBJECT:ButtonKopMallStrip_Click).
      /*  */
      /* ButtonBortMallStrip */
      /*  */
      THIS-OBJECT:ButtonBortMallStrip:Image = CAST(resources:GetObject("ButtonBortMallStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortMallStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortMallStrip:Name = "ButtonBortMallStrip".
      THIS-OBJECT:ButtonBortMallStrip:Size = NEW System.Drawing.Size(91, 20).
      THIS-OBJECT:ButtonBortMallStrip:Text = "Ta bort mall".
      THIS-OBJECT:ButtonBortMallStrip:Click:Subscribe(THIS-OBJECT:ButtonBortMallStrip_Click).
      /*  */
      /* GridMallar */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallar:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallar:GuruContext = ?.
      THIS-OBJECT:GridMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallar:Name = "GridMallar".
      THIS-OBJECT:GridMallar:Size = NEW System.Drawing.Size(197, 528).
      THIS-OBJECT:GridMallar:TabIndex = 0.
      THIS-OBJECT:GridMallar:Text = "grid1".
      THIS-OBJECT:GridMallar:AfterCellUpdate:Subscribe(THIS-OBJECT:GridMallar_AfterCellUpdate).
      /*  */
      /* splitContainerSubKatMallar */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatMallar:Name = "splitContainerSubKatMallar".
      /*  */
      /* splitContainerSubKatMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatMallarStrip).
      /*  */
      /* splitContainerSubKatMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerArbkodMallar).
      THIS-OBJECT:splitContainerSubKatMallar:Size = NEW System.Drawing.Size(855, 556).
      THIS-OBJECT:splitContainerSubKatMallar:SplitterDistance = 165.
      THIS-OBJECT:splitContainerSubKatMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatMallar:TabIndex = 0.
      /*  */
      /* splitContainerSubKatMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerSubKatMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Name = "splitContainerSubKatMallarStrip".
      THIS-OBJECT:splitContainerSubKatMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSubKatMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:Controls:Add(THIS-OBJECT:SubKatMallarStrip).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerSubKatMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubMall).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Size = NEW System.Drawing.Size(165, 556).
      THIS-OBJECT:splitContainerSubKatMallarStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerSubKatMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatMallarStrip:TabIndex = 1.
      /*  */
      /* SubKatMallarStrip */
      /*  */
      THIS-OBJECT:SubKatMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:SubKatMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:SubKatMallarStrip:Name = "SubKatMallarStrip".
      THIS-OBJECT:SubKatMallarStrip:Size = NEW System.Drawing.Size(165, 27).
      THIS-OBJECT:SubKatMallarStrip:TabIndex = 3.
      THIS-OBJECT:SubKatMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridSubMall */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubMall:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridSubMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubMall:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridSubMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridSubMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubMall:GuruContext = ?.
      THIS-OBJECT:GridSubMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubMall:Name = "GridSubMall".
      THIS-OBJECT:GridSubMall:Size = NEW System.Drawing.Size(165, 528).
      THIS-OBJECT:GridSubMall:TabIndex = 0.
      THIS-OBJECT:GridSubMall:Text = "grid1".
      /*  */
      /* splitContainerArbkodMallar */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbkodMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbkodMallar:Name = "splitContainerArbkodMallar".
      /*  */
      /* splitContainerArbkodMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerArbkodMallarStrip).
      /*  */
      /* splitContainerArbkodMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnrMallar).
      THIS-OBJECT:splitContainerArbkodMallar:Size = NEW System.Drawing.Size(689, 556).
      THIS-OBJECT:splitContainerArbkodMallar:SplitterDistance = 165.
      THIS-OBJECT:splitContainerArbkodMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerArbkodMallar:TabIndex = 0.
      /*  */
      /* splitContainerArbkodMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbkodMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerArbkodMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Name = "splitContainerArbkodMallarStrip".
      THIS-OBJECT:splitContainerArbkodMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerArbkodMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:Controls:Add(THIS-OBJECT:ArbkodMallarStrip).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerArbkodMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridArbKodMall).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Size = NEW System.Drawing.Size(165, 556).
      THIS-OBJECT:splitContainerArbkodMallarStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerArbkodMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerArbkodMallarStrip:TabIndex = 1.
      /*  */
      /* ArbkodMallarStrip */
      /*  */
      THIS-OBJECT:ArbkodMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ArbkodMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ArbkodMallarStrip:Name = "ArbkodMallarStrip".
      THIS-OBJECT:ArbkodMallarStrip:Size = NEW System.Drawing.Size(165, 27).
      THIS-OBJECT:ArbkodMallarStrip:TabIndex = 4.
      THIS-OBJECT:ArbkodMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridArbKodMall */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbKodMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbKodMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbKodMall:GuruContext = ?.
      THIS-OBJECT:GridArbKodMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbKodMall:Name = "GridArbKodMall".
      THIS-OBJECT:GridArbKodMall:Size = NEW System.Drawing.Size(165, 528).
      THIS-OBJECT:GridArbKodMall:TabIndex = 0.
      THIS-OBJECT:GridArbKodMall:Text = "grid1".
      /*  */
      /* splitContainerLopnrMallar */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnrMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnrMallar:Name = "splitContainerLopnrMallar".
      /*  */
      /* splitContainerLopnrMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerLopnrMallarStrip).
      /*  */
      /* splitContainerLopnrMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:Controls:Add(THIS-OBJECT:splitContainerBtnOverMall).
      THIS-OBJECT:splitContainerLopnrMallar:Size = NEW System.Drawing.Size(523, 556).
      THIS-OBJECT:splitContainerLopnrMallar:SplitterDistance = 209.
      THIS-OBJECT:splitContainerLopnrMallar:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnrMallar:TabIndex = 0.
      /*  */
      /* splitContainerLopnrMallarStrip */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnrMallarStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopnrMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Name = "splitContainerLopnrMallarStrip".
      THIS-OBJECT:splitContainerLopnrMallarStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopnrMallarStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:Controls:Add(THIS-OBJECT:LopnrMallarStrip).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerLopnrMallarStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:Controls:Add(THIS-OBJECT:GridLopMall).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Size = NEW System.Drawing.Size(209, 556).
      THIS-OBJECT:splitContainerLopnrMallarStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerLopnrMallarStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnrMallarStrip:TabIndex = 1.
      /*  */
      /* LopnrMallarStrip */
      /*  */
      THIS-OBJECT:LopnrMallarStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:LopnrMallarStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopnrMallarStrip:Name = "LopnrMallarStrip".
      THIS-OBJECT:LopnrMallarStrip:Size = NEW System.Drawing.Size(209, 27).
      THIS-OBJECT:LopnrMallarStrip:TabIndex = 5.
      THIS-OBJECT:LopnrMallarStrip:Text = "toolStrip1".
      /*  */
      /* GridLopMall */
      /*  */
      THIS-OBJECT:GridLopMall:AllowDrop = TRUE.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopMall:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridLopMall:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopMall:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopMall:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridLopMall:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopMall:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopMall:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
      THIS-OBJECT:GridLopMall:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopMall:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopMall:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopMall:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopMall:GuruContext = ?.
      THIS-OBJECT:GridLopMall:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopMall:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopMall:Name = "GridLopMall".
      THIS-OBJECT:GridLopMall:Size = NEW System.Drawing.Size(209, 528).
      THIS-OBJECT:GridLopMall:TabIndex = 0.
      THIS-OBJECT:GridLopMall:Text = "grid1".
      THIS-OBJECT:GridLopMall:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopMall_GuruAfterDropped).
      /*  */
      /* splitContainerBtnOverMall */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBtnOverMall:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBtnOverMall:Name = "splitContainerBtnOverMall".
      /*  */
      /* splitContainerBtnOverMall.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:Controls:Add(THIS-OBJECT:ButtonOverMall).
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:Controls:Add(THIS-OBJECT:ButtonBackMall).
      /*  */
      /* splitContainerBtnOverMall.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:Controls:Add(THIS-OBJECT:splitContainerValdMallarBild).
      THIS-OBJECT:splitContainerBtnOverMall:Size = NEW System.Drawing.Size(313, 556).
      THIS-OBJECT:splitContainerBtnOverMall:SplitterDistance = 29.
      THIS-OBJECT:splitContainerBtnOverMall:SplitterWidth = 1.
      THIS-OBJECT:splitContainerBtnOverMall:TabIndex = 0.
      /*  */
      /* ButtonOverMall */
      /*  */
      appearance49:Image = CAST(resources:GetObject("appearance49.Image"), System.Object).
      THIS-OBJECT:ButtonOverMall:Appearance = appearance49.
      THIS-OBJECT:ButtonOverMall:Location = NEW System.Drawing.Point(5, 150).
      THIS-OBJECT:ButtonOverMall:Name = "ButtonOverMall".
      THIS-OBJECT:ButtonOverMall:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverMall:TabIndex = 14.
      THIS-OBJECT:ButtonOverMall:Click:Subscribe(THIS-OBJECT:ButtonOverMall_Click).
      /*  */
      /* ButtonBackMall */
      /*  */
      appearance50:Image = CAST(resources:GetObject("appearance50.Image"), System.Object).
      THIS-OBJECT:ButtonBackMall:Appearance = appearance50.
      THIS-OBJECT:ButtonBackMall:Location = NEW System.Drawing.Point(5, 180).
      THIS-OBJECT:ButtonBackMall:Name = "ButtonBackMall".
      THIS-OBJECT:ButtonBackMall:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackMall:TabIndex = 15.
      THIS-OBJECT:ButtonBackMall:Click:Subscribe(THIS-OBJECT:ButtonBackMall_Click).
      /*  */
      /* splitContainerValdMallarBild */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdMallarBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdMallarBild:Name = "splitContainerValdMallarBild".
      /*  */
      /* splitContainerValdMallarBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerValdMallarBildStrip).
      /*  */
      /* splitContainerValdMallarBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerValdMallarBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerValdMallarBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerValdMallarBild:Size = NEW System.Drawing.Size(283, 556).
      THIS-OBJECT:splitContainerValdMallarBild:SplitterDistance = 217.
      THIS-OBJECT:splitContainerValdMallarBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerValdMallarBild:TabIndex = 0.
      /*  */
      /* splitContainerValdMallarBildStrip */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdMallarBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerValdMallarBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Name = "splitContainerValdMallarBildStrip".
      THIS-OBJECT:splitContainerValdMallarBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerValdMallarBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:Controls:Add(THIS-OBJECT:ValdMallarBildStrip).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerValdMallarBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:Controls:Add(THIS-OBJECT:GridValdMallar).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Size = NEW System.Drawing.Size(217, 556).
      THIS-OBJECT:splitContainerValdMallarBildStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerValdMallarBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerValdMallarBildStrip:TabIndex = 1.
      /*  */
      /* ValdMallarBildStrip */
      /*  */
      THIS-OBJECT:ValdMallarBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdMallarBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdMallarBildStrip:Name = "ValdMallarBildStrip".
      THIS-OBJECT:ValdMallarBildStrip:Size = NEW System.Drawing.Size(217, 27).
      THIS-OBJECT:ValdMallarBildStrip:TabIndex = 6.
      THIS-OBJECT:ValdMallarBildStrip:Text = "toolStrip1".
      /*  */
      /* GridValdMallar */
      /*  */
      THIS-OBJECT:GridValdMallar:AllowDrop = TRUE.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridValdMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridValdMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdMallar:GuruContext = ?.
      THIS-OBJECT:GridValdMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdMallar:Name = "GridValdMallar".
      THIS-OBJECT:GridValdMallar:Size = NEW System.Drawing.Size(217, 528).
      THIS-OBJECT:GridValdMallar:TabIndex = 0.
      THIS-OBJECT:GridValdMallar:Text = "grid1".
      THIS-OBJECT:GridValdMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdMallar_GuruAfterDropped).
      THIS-OBJECT:GridValdMallar:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdMallar_AfterCellUpdate).
      /*  */
      /* KalkylAdmMall */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerMallarBas).
      THIS-OBJECT:Name = "KalkylAdmMall".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1053, 556).
      THIS-OBJECT:splitContainerMallarBas:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMallarBas:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMallarBas, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMallarBas:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopMallar:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopMallar:ResumeLayout(FALSE).
      THIS-OBJECT:NyKopMallarStrip:ResumeLayout(FALSE).
      THIS-OBJECT:NyKopMallarStrip:PerformLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerSubKatMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSubMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerArbkodMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerArbkodMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerArbkodMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbkodMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbKodMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnrMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerLopnrMallarStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnrMallarStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnrMallarStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBtnOverMall:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBtnOverMall:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBtnOverMall, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBtnOverMall:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValdMallarBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdMallarBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerValdMallarBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValdMallarBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdMallarBildStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC KalkylAdmMall ( ):

	END DESTRUCTOR.

END CLASS.