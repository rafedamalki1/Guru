 
 /*------------------------------------------------------------------------
    File        : KalkEgnaControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 14:29:41 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkEgnaControl INHERITS UserControl: 
   DEFINE PRIVATE VARIABLE AnvEgenCopy AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE AnvEgenStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE AnvEgenSkapa AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE AnvEgenBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAnvEgenSub AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAnvEgen AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvEgen AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvEgenStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvEgenLista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaValj AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaVald AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgnaArbKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridValdaUnderKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridUnderKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridEgnaKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaKoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenTaBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenSkapa AS System.Windows.Forms.ToolStripButton NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkEgnaControl ( INPUT r AS Guru.Root ):
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      
      
      IF NOT VALID-HANDLE(THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh) THEN DO:
         
         THIS-OBJECT:splitContainerAnvEgenStrip:Panel1Collapsed = TRUE.
         THIS-OBJECT:SplitContainerAnvEgen:Panel1Collapsed = TRUE.
         THIS-OBJECT:AnvEgenCopy:Visible = FALSE. 
      END.
      ELSE DO:
         THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
         THIS-OBJECT:CreateForm().
         THIS-OBJECT:SplitContainerAnvEgen:Panel2Collapsed = TRUE.
         
      END.   
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AnvEgenBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      THIS-OBJECT:GridAnvEgen:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridAnvEgen:ActiveRow NE ? THEN DO:
         THIS-OBJECT:GridAnvEgen:SyncMan().
         IF THIS-OBJECT:GridAnvEgen:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveAnvEgenKod().
            THIS-OBJECT:GridAnvEgen:GuruBortrader(FALSE).
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkAnvEgenSpar().
            THIS-OBJECT:FilterAnvUnderKoder().
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.


	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AnvEgenCopy_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID NO-UNDO.
		IF THIS-OBJECT:GridAnvEgen:tBS:Count > 0 THEN.
		ELSE RETURN.
		IF THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:AVAILABLE THEN.
      ELSE DO:
         THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      END.
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      THIS-OBJECT:GridEgnaKoder:SyncMan().
      
      
      rid = THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateEgenKodCopyAnvEgen().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).
      THIS-OBJECT:GridEgnaKoder:GuruCellValjare("BENAMNING").       
      THIS-OBJECT:ControlShell:DontCheck= FALSE.
		
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AnvEgenSkapa_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 THEN.
      ELSE RETURN.
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      THIS-OBJECT:GridAnvEgen:SyncMan().
      rid = THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateAnvEgenKod().
      THIS-OBJECT:GridAnvEgen:GuruReopen().
      THIS-OBJECT:GridAnvEgen:GuruRepositionto(rid).
      THIS-OBJECT:GridAnvEgen:GuruCellValjare("BENAMNING").       
      THIS-OBJECT:ControlShell:DontCheck= FALSE.
      RETURN.
		

	END METHOD.

	METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      
      
   END METHOD.
	METHOD PUBLIC VOID CreateGrids():
	   THIS-OBJECT:CreateGridAnvEgen().
   END METHOD.
   
	METHOD PUBLIC VOID CreateGridAnvEgen():
	   {GridRubrikListaVarExtent.i}
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      rubrikergrid[1] = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35,"INT","INT").
      rubrikergrid[2] = NEW Controls.GridRubrik("VIARBKOD","Kod",TRUE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,40).      
      rubrikergrid[4] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","Enh",FALSE,35).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,240).
      rubrikergrid[8] = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,80,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,100).
      rubrikergrid[10] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,150).
      rubrikergrid[11] = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      /*init av grid*/
      
      THIS-OBJECT:GridAnvEgen:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 11, INPUT THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh)). 
      THIS-OBJECT:GridAnvEgen:TEXT = "Sparade Egna koder".
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["ARBKOD"]:Hidden = TRUE.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = TRUE.  
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["MARKNING"]:Hidden = TRUE. 
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["ANTAL"]:Hidden = TRUE. 
      THIS-OBJECT:GridAnvEgen:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridAnvEgen:GuruOrderbyDefault("BY MATRIS BY ARBKOD BY MARKSUB BY LOPNR BY NUM").
      /*
      THIS-OBJECT:GridAnvEgen:GuruOrderby("BY MATRIS BY ARBKOD BY LOPNR BY NUM BY MARKSUB").
      */
      THIS-OBJECT:GridAnvEgen:tBS:PositionChanged:Subscribe(ValueAnvEgenKoderCh).
      
      THIS-OBJECT:GridAnvEgen:GuruReopen().
       
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("TIMMAR", "Antal",FALSE,45,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("PRIS", "Pris/enhet", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("NUM","#",true,35).
      
      THIS-OBJECT:GridAnvEgenSub:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkdbControl:Kalknumanvegensubbuffh)).
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = true.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Bands[0]:Columns["NUM"]:Hidden = true.
      THIS-OBJECT:GridAnvEgenSub:TEXT = "Sparade Priser".
      
      THIS-OBJECT:GridAnvEgenSub:GuruOrderby("BY BENAMNING").
      
      THIS-OBJECT:ControlShell:DontCheck = FALSE.
      THIS-OBJECT:GridAnvEgen:GuruLastrow().
      THIS-OBJECT:GridAnvEgen:GuruFirstrow().
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnvEgen_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridAnvEgen:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:KalkdbControl:LopposterTTh:FIND-FIRST("WHERE  ARBKOD =  'EGEN' AND LOPNR = " + STRING(THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)) NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkdbControl:LopposterTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE NO-ERROR.
      END.   
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RaknaAnvEgen().
      THIS-OBJECT:ControlShell:BerAnvEgenKod().        
      RETURN.
		 

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnvEgen_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:AnvEgenCopy_Click(?,?).
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnvEgenSub_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().     
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridAnvEgenSub:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:GridAnvEgenSub:SyncMan().
      THIS-OBJECT:KalkdbControl:Kalknumanvegensubbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      IF kolnamn = "TIMMAR" OR kolnamn = "PRIS" OR kolnamn = "KOSTNAD" THEN DO:
         kolnamn = "FRI" + kolnamn.
         THIS-OBJECT:KalkdbControl:Kalknumanvegensubbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      END.    
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RaknaAnvEgen().
      THIS-OBJECT:ControlShell:BerAnvEgenKod().
		
		
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridEgnaKoder_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:AnvEgenSkapa_Click(?,?).
		RETURN.

	END METHOD.

	
	METHOD PUBLIC VOID ValueAnvEgenKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
     
      THIS-OBJECT:FilterAnvUnderKoder().
      
   END METHOD.
   METHOD PUBLIC VOID FilterAnvUnderKoder():
      
      IF THIS-OBJECT:GridAnvEgen:tBS:Count > 0 AND THIS-OBJECT:GridAnvEgen:ActiveRow NE ? THEN DO:
         IF TRIM(STRING(THIS-OBJECT:GridAnvEgen:ActiveRow:Cells["NUM"]:VALUE)) = "" THEN RETURN.
         IF THIS-OBJECT:ControlShell:DontCheck = FALSE THEN DO:
            THIS-OBJECT:GridAnvEgenSub:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:GridAnvEgen:ActiveRow:Cells["NUM"]:VALUE)).
            THIS-OBJECT:GridAnvEgenSub:GuruFirstrow().
            THIS-OBJECT:GridAnvEgenSub:Enabled = TRUE.
         END.   
      END.
      ELSE IF THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:GridAnvEgenSub:GuruFiltrera( "NUM = " + STRING(THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE)).
         THIS-OBJECT:GridAnvEgenSub:GuruFirstrow().
         THIS-OBJECT:GridAnvEgenSub:Enabled = TRUE.
      END.      
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:GridAnvEgenSub:GuruFiltrera("NUM = -1").
         THIS-OBJECT:GridAnvEgenSub:Enabled = FALSE.
      END.
      
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridUnderKoder_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		THIS-OBJECT:ButtonOvermarkPriser_Click(?,?).
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkEgnaControl").
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
      THIS-OBJECT:splitContainerEgnaBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerEgnaArbKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaKoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:EgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:EgenTaBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:AnvEgenCopy = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridEgnaKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgnaVald = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridValdaUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerAnvEgenStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:AnvEgenStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:AnvEgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:AnvEgenBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerAnvEgen = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerAnvEgenLista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAnvEgen = NEW Controls.Grid().
      THIS-OBJECT:GridAnvEgenSub = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:SuspendLayout().
      THIS-OBJECT:EgnaKoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaValj:SuspendLayout().
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerEgnaVald:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAnvEgenStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvEgenStrip:SuspendLayout().
      THIS-OBJECT:AnvEgenStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAnvEgen, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvEgen:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvEgen:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAnvEgenLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvEgenLista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvEgenLista:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvEgenLista:SuspendLayout().
      CAST(THIS-OBJECT:GridAnvEgen, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridAnvEgenSub, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerEgnaBild */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaBild:Name = "splitContainerEgnaBild".
      /*  */
      /* splitContainerEgnaBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaKoder).
      /*  */
      /* splitContainerEgnaBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerEgnaBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerEgnaBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerEgnaBild:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvEgenStrip).
      THIS-OBJECT:splitContainerEgnaBild:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerEgnaBild:SplitterDistance = 675.
      THIS-OBJECT:splitContainerEgnaBild:TabIndex = 20.
      /*  */
      /* splitContainerEgnaKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaKoder:Name = "splitContainerEgnaKoder".
      THIS-OBJECT:splitContainerEgnaKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerEgnaArbKoder).
      /*  */
      /* splitContainerEgnaKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaValj).
      THIS-OBJECT:splitContainerEgnaKoder:Size = NEW System.Drawing.Size(675, 546).
      THIS-OBJECT:splitContainerEgnaKoder:SplitterDistance = 275.
      THIS-OBJECT:splitContainerEgnaKoder:TabIndex = 18.
      /*  */
      /* splitContainerEgnaArbKoder */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaArbKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaArbKoder:Name = "splitContainerEgnaArbKoder".
      THIS-OBJECT:splitContainerEgnaArbKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgnaArbKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:Controls:Add(THIS-OBJECT:EgnaKoderStrip).
      /*  */
      /* splitContainerEgnaArbKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:Controls:Add(THIS-OBJECT:GridEgnaKoder).
      THIS-OBJECT:splitContainerEgnaArbKoder:Size = NEW System.Drawing.Size(675, 275).
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterDistance = 34.
      THIS-OBJECT:splitContainerEgnaArbKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerEgnaArbKoder:TabIndex = 14.
      /*  */
      /* EgnaKoderStrip */
      /*  */
      THIS-OBJECT:EgnaKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:EgenSkapa.
      arrayvar0[2] = THIS-OBJECT:EgenTaBort.
      arrayvar0[3] = THIS-OBJECT:AnvEgenCopy.
      THIS-OBJECT:EgnaKoderStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:EgnaKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:EgnaKoderStrip:Name = "EgnaKoderStrip".
      THIS-OBJECT:EgnaKoderStrip:Size = NEW System.Drawing.Size(675, 34).
      THIS-OBJECT:EgnaKoderStrip:TabIndex = 13.
      THIS-OBJECT:EgnaKoderStrip:Text = "toolStrip1".
      /*  */
      /* EgenSkapa */
      /*  */
      THIS-OBJECT:EgenSkapa:Image = CAST(resources:GetObject("EgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenSkapa:Name = "EgenSkapa".
      THIS-OBJECT:EgenSkapa:Size = NEW System.Drawing.Size(98, 31).
      THIS-OBJECT:EgenSkapa:Text = "Lägg till egen".
      THIS-OBJECT:EgenSkapa:Click:Subscribe(THIS-OBJECT:SkapaNyEgen_Click).
      /*  */
      /* EgenTaBort */
      /*  */
      THIS-OBJECT:EgenTaBort:Image = CAST(resources:GetObject("EgenTaBort.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenTaBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenTaBort:Name = "EgenTaBort".
      THIS-OBJECT:EgenTaBort:Size = NEW System.Drawing.Size(94, 31).
      THIS-OBJECT:EgenTaBort:Text = "Ta bort egen".
      THIS-OBJECT:EgenTaBort:Click:Subscribe(THIS-OBJECT:TaBortEgen_Click).
      /*  */
      /* AnvEgenCopy */
      /*  */
      THIS-OBJECT:AnvEgenCopy:Image = CAST(resources:GetObject("AnvEgenCopy.Image"), System.Drawing.Image).
      THIS-OBJECT:AnvEgenCopy:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AnvEgenCopy:Name = "AnvEgenCopy".
      THIS-OBJECT:AnvEgenCopy:Size = NEW System.Drawing.Size(169, 31).
      THIS-OBJECT:AnvEgenCopy:Text = "Kopiera från Sparade koder".
      THIS-OBJECT:AnvEgenCopy:ToolTipText = "Markera Sparad Egen kod i Spara listan".
      THIS-OBJECT:AnvEgenCopy:Click:Subscribe(THIS-OBJECT:AnvEgenCopy_Click).
      /*  */
      /* GridEgnaKoder */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgnaKoder:GuruContext = ?.
      THIS-OBJECT:GridEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgnaKoder:Name = "GridEgnaKoder".
      THIS-OBJECT:GridEgnaKoder:Size = NEW System.Drawing.Size(675, 240).
      THIS-OBJECT:GridEgnaKoder:TabIndex = 12.
      THIS-OBJECT:GridEgnaKoder:Text = "grid1".
      THIS-OBJECT:GridEgnaKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellUpdate).
      THIS-OBJECT:GridEgnaKoder:DoubleClick:Subscribe(THIS-OBJECT:GridEgnaKoder_DoubleClick).
      /*  */
      /* splitContainerEgnaValj */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerEgnaValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaValj:Name = "splitContainerEgnaValj".
      /*  */
      /* splitContainerEgnaValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel1:Controls:Add(THIS-OBJECT:GridUnderKoder).
      /*  */
      /* splitContainerEgnaValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaValj:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgnaVald).
      THIS-OBJECT:splitContainerEgnaValj:Size = NEW System.Drawing.Size(675, 267).
      THIS-OBJECT:splitContainerEgnaValj:SplitterDistance = 249.
      THIS-OBJECT:splitContainerEgnaValj:TabIndex = 0.
      /*  */
      /* GridUnderKoder */
      /*  */
      THIS-OBJECT:GridUnderKoder:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUnderKoder:Name = "GridUnderKoder".
      THIS-OBJECT:GridUnderKoder:Size = NEW System.Drawing.Size(247, 265).
      THIS-OBJECT:GridUnderKoder:TabIndex = 10.
      THIS-OBJECT:GridUnderKoder:Text = "grid1".
      THIS-OBJECT:GridUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUnderKoder_GuruAfterDropped).
      THIS-OBJECT:GridUnderKoder:DoubleClickRow:Subscribe(THIS-OBJECT:GridUnderKoder_DoubleClickRow).
      /*  */
      /* splitContainerEgnaVald */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgnaVald:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerEgnaVald:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerEgnaVald:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgnaVald:Name = "splitContainerEgnaVald".
      /*  */
      /* splitContainerEgnaVald.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonOverMark).
      THIS-OBJECT:splitContainerEgnaVald:Panel1:Controls:Add(THIS-OBJECT:EgnaButtonBackMark).
      /*  */
      /* splitContainerEgnaVald.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgnaVald:Panel2:Controls:Add(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:splitContainerEgnaVald:Size = NEW System.Drawing.Size(420, 265).
      THIS-OBJECT:splitContainerEgnaVald:SplitterDistance = 40.
      THIS-OBJECT:splitContainerEgnaVald:TabIndex = 0.
      /*  */
      /* EgnaButtonOverMark */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:EgnaButtonOverMark:Appearance = appearance25.
      THIS-OBJECT:EgnaButtonOverMark:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:EgnaButtonOverMark:Name = "EgnaButtonOverMark".
      THIS-OBJECT:EgnaButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:EgnaButtonOverMark:TabIndex = 13.
      THIS-OBJECT:EgnaButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOverMarkPriser_Click).
      /*  */
      /* EgnaButtonBackMark */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:EgnaButtonBackMark:Appearance = appearance26.
      THIS-OBJECT:EgnaButtonBackMark:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:EgnaButtonBackMark:Name = "EgnaButtonBackMark".
      THIS-OBJECT:EgnaButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:EgnaButtonBackMark:TabIndex = 15.
      THIS-OBJECT:EgnaButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMarkPriser_Click).
      /*  */
      /* GridValdaUnderKoder */
      /*  */
      THIS-OBJECT:GridValdaUnderKoder:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridValdaUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaUnderKoder:Name = "GridValdaUnderKoder".
      THIS-OBJECT:GridValdaUnderKoder:Size = NEW System.Drawing.Size(376, 265).
      THIS-OBJECT:GridValdaUnderKoder:TabIndex = 11.
      THIS-OBJECT:GridValdaUnderKoder:Text = "grid1".
      THIS-OBJECT:GridValdaUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaUnderKoder_GuruAfterDropped).
      THIS-OBJECT:GridValdaUnderKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridValdaUnderKoder_AfterCellUpdate).
      /*  */
      /* splitContainerAnvEgenStrip */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvEgenStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvEgenStrip:Name = "splitContainerAnvEgenStrip".
      THIS-OBJECT:splitContainerAnvEgenStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerAnvEgenStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel1:Controls:Add(THIS-OBJECT:AnvEgenStrip).
      /*  */
      /* splitContainerAnvEgenStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvEgen).
      THIS-OBJECT:splitContainerAnvEgenStrip:Size = NEW System.Drawing.Size(432, 546).
      THIS-OBJECT:splitContainerAnvEgenStrip:SplitterDistance = 34.
      THIS-OBJECT:splitContainerAnvEgenStrip:TabIndex = 0.
      /*  */
      /* AnvEgenStrip */
      /*  */
      THIS-OBJECT:AnvEgenStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:AnvEgenSkapa.
      arrayvar1[2] = THIS-OBJECT:AnvEgenBort.
      THIS-OBJECT:AnvEgenStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:AnvEgenStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:AnvEgenStrip:Name = "AnvEgenStrip".
      THIS-OBJECT:AnvEgenStrip:Size = NEW System.Drawing.Size(432, 34).
      THIS-OBJECT:AnvEgenStrip:TabIndex = 0.
      THIS-OBJECT:AnvEgenStrip:Text = "toolStrip1".
      /*  */
      /* AnvEgenSkapa */
      /*  */
      THIS-OBJECT:AnvEgenSkapa:Image = CAST(resources:GetObject("AnvEgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:AnvEgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AnvEgenSkapa:Name = "AnvEgenSkapa".
      THIS-OBJECT:AnvEgenSkapa:Size = NEW System.Drawing.Size(149, 31).
      THIS-OBJECT:AnvEgenSkapa:Text = "Lägg till Egen i Sparade".
      THIS-OBJECT:AnvEgenSkapa:ToolTipText = "Markera Egen kod i kalkylen".
      THIS-OBJECT:AnvEgenSkapa:Click:Subscribe(THIS-OBJECT:AnvEgenSkapa_Click).
      /*  */
      /* AnvEgenBort */
      /*  */
      THIS-OBJECT:AnvEgenBort:Image = CAST(resources:GetObject("AnvEgenBort.Image"), System.Drawing.Image).
      THIS-OBJECT:AnvEgenBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AnvEgenBort:Name = "AnvEgenBort".
      THIS-OBJECT:AnvEgenBort:Size = NEW System.Drawing.Size(155, 31).
      THIS-OBJECT:AnvEgenBort:Text = "Ta bort sparad egen kod".
      THIS-OBJECT:AnvEgenBort:ToolTipText = "Den sparade koden tas bort".
      THIS-OBJECT:AnvEgenBort:Click:Subscribe(THIS-OBJECT:AnvEgenBort_Click).
      /*  */
      /* splitContainerAnvEgen */
      /*  */
      THIS-OBJECT:splitContainerAnvEgen:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvEgen:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvEgen:Name = "splitContainerAnvEgen".
      /*  */
      /* splitContainerAnvEgen.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgen:Panel1:Controls:Add(THIS-OBJECT:splitContainerAnvEgenLista).
      /*  */
      /* splitContainerAnvEgen.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgen:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerAnvEgen.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerAnvEgen:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerAnvEgen:Size = NEW System.Drawing.Size(432, 508).
      THIS-OBJECT:splitContainerAnvEgen:SplitterDistance = 257.
      THIS-OBJECT:splitContainerAnvEgen:TabIndex = 0.
      /*  */
      /* splitContainerAnvEgenLista */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenLista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvEgenLista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvEgenLista:Name = "splitContainerAnvEgenLista".
      THIS-OBJECT:splitContainerAnvEgenLista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerAnvEgenLista.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenLista:Panel1:Controls:Add(THIS-OBJECT:GridAnvEgen).
      /*  */
      /* splitContainerAnvEgenLista.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvEgenLista:Panel2:Controls:Add(THIS-OBJECT:GridAnvEgenSub).
      THIS-OBJECT:splitContainerAnvEgenLista:Size = NEW System.Drawing.Size(257, 508).
      THIS-OBJECT:splitContainerAnvEgenLista:SplitterDistance = 238.
      THIS-OBJECT:splitContainerAnvEgenLista:TabIndex = 0.
      /*  */
      /* GridAnvEgen */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnvEgen:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnvEgen:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnvEgen:GuruContext = ?.
      THIS-OBJECT:GridAnvEgen:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnvEgen:Name = "GridAnvEgen".
      THIS-OBJECT:GridAnvEgen:Size = NEW System.Drawing.Size(257, 238).
      THIS-OBJECT:GridAnvEgen:TabIndex = 0.
      THIS-OBJECT:GridAnvEgen:Text = "grid1".
      THIS-OBJECT:GridAnvEgen:AfterCellUpdate:Subscribe(THIS-OBJECT:GridAnvEgen_AfterCellUpdate).
      THIS-OBJECT:GridAnvEgen:DoubleClick:Subscribe(THIS-OBJECT:GridAnvEgen_DoubleClick).
      /*  */
      /* GridAnvEgenSub */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnvEgenSub:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnvEgenSub:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnvEgenSub:GuruContext = ?.
      THIS-OBJECT:GridAnvEgenSub:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnvEgenSub:Name = "GridAnvEgenSub".
      THIS-OBJECT:GridAnvEgenSub:Size = NEW System.Drawing.Size(257, 266).
      THIS-OBJECT:GridAnvEgenSub:TabIndex = 0.
      THIS-OBJECT:GridAnvEgenSub:Text = "grid1".
      THIS-OBJECT:GridAnvEgenSub:AfterCellUpdate:Subscribe(THIS-OBJECT:GridAnvEgenSub_AfterCellUpdate).
      /*  */
      /* KalkEgnaControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerEgnaBild).
      THIS-OBJECT:Name = "KalkEgnaControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerEgnaBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaBild:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerEgnaArbKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaArbKoder:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:EgnaKoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaValj:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaValj, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgnaVald:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerEgnaVald, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgnaVald:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerAnvEgenStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvEgenStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvEgenStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AnvEgenStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AnvEgenStrip:PerformLayout().
      THIS-OBJECT:splitContainerAnvEgen:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvEgen, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvEgen:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvEgenLista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvEgenLista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvEgenLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvEgenLista:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAnvEgen, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridAnvEgenSub, System.ComponentModel.ISupportInitialize):EndInit().
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
   METHOD PUBLIC VOID SkapaNyEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      THIS-OBJECT:GridEgnaKoder:SyncMan().
      rid = THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateEgenKod().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).
      THIS-OBJECT:GridEgnaKoder:GuruCellValjare("BENAMNING").       
      THIS-OBJECT:ControlShell:DontCheck= FALSE.
      /*
      MESSAGE "NU PROVAR VI"
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:KalkdbControl:AnvEgenTest().
      */
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID TaBortEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      THIS-OBJECT:GridEgnaKoder:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN DO:
         THIS-OBJECT:GridEgnaKoder:SyncMan().
         IF THIS-OBJECT:GridEgnaKoder:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveEgenKod().
            THIS-OBJECT:GridEgnaKoder:GuruBortrader(FALSE).
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
            THIS-OBJECT:ControlShell:FilterUnderKoder().
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
   
   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID GridEgnaKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridEgnaKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      THIS-OBJECT:KalkdbControl:Kalknumanvegenbuffh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridAnvEgen:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      IF kolnamn = "MATRIS" AND kolvarde=  "0" THEN DO:
         kolvarde = "1".
         THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:GridEgnaKoder:GuruReopen().
      END. 
      ELSE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().
      THIS-OBJECT:ControlShell:BerEgenKod().        
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaUnderKoder THEN DO:
         THIS-OBJECT:ButtonBackmarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridUnderKoder THEN DO:
         THIS-OBJECT:ButtonOvermarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridEgnaKoder:ActiveRow.
      
      IF THIS-OBJECT:EgnaButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
         THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-RELEASE () NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("SOKBENAMNING"):BUFFER-VALUE = "BEREDARE" THEN 
         THIS-OBJECT:KalkdbControl:ValdaPriserTTh:FIND-FIRST(" WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND BENAMNING = 'BEREDARE REGION'") NO-ERROR.
         
         IF THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("SOKBENAMNING"):BUFFER-VALUE = "BEREDARE REGION" THEN 
         THIS-OBJECT:KalkdbControl:ValdaPriserTTh:FIND-FIRST(" WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND BENAMNING = 'BEREDARE'") NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:AVAILABLE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(199), "").
            RETURN.
         END.
         THIS-OBJECT:EgnaButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateValdaRows().      
         THIS-OBJECT:EgnaButtonOverMark:GuruOverefteruppdat().
         IF THIS-OBJECT:GridUnderKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:EgnaButtonOverMark:GuruTogrid:GuruCellvaljare("TIMMAR").       
         THIS-OBJECT:ControlShell:BerEgenKod().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridEgnaKoder:Posn() = -1 THEN RETURN.
      IF THIS-OBJECT:GridValdaUnderKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveValdaSelectedEgna().
         THIS-OBJECT:EgnaButtonBackMark:GuruOver().      
         THIS-OBJECT:ControlShell:BerEgenKod().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
         THIS-OBJECT:ControlShell:DontCheck = FALSE.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.

      kolnamn = e:Cell:Column:ToString().     
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridValdaUnderKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:GridValdaUnderKoder:SyncMan().
      THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      IF kolnamn = "TIMMAR" OR kolnamn = "PRIS" OR kolnamn = "KOSTNAD" THEN DO:
         kolnamn = "FRI" + kolnamn.
         THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      END.    
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKodSub().
      THIS-OBJECT:ControlShell:BerEgenKod().
      
      RETURN.
      
   END METHOD.
   
	DESTRUCTOR PUBLIC KalkEgnaControl ( ):
     /*{KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.