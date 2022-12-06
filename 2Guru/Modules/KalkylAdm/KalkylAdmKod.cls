 
 /*------------------------------------------------------------------------
    File        : KalkylAdmKod
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 30 15:28:53 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.KalkylAdm.KalkylAdmKod INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkAdmdbControl  AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
   DEFINE PUBLIC VARIABLE AddPrisButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE AddPrisButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonImpKatXmlStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonExpKatXmlStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAddPris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmKatControl                 AS Modules.KalkylAdm.KalkylAdmKatalog                                   NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylAdmPrisControl                 AS Modules.KalkylAdm.KalkylAdmPris                                  NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   
   DEFINE PUBLIC VARIABLE ButtonNyLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonNyKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKopLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKopKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortLopnrStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortKoderStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFrekKom AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyPrisAdd AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatKoderStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyKopLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyKopKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopsubBildStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopsubBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopSubFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerGrundKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE SubKatKodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE Rubrikanm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE P2kommtoolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE LopSubBildStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE LopNrStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE LasintoolStrip AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC VARIABLE KoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE KatalogtoolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSubKatKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopnumsub AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopnr AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFrekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE FrekvenstoolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE DelkatalogToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkylAdmKod ( INPUT r AS Guru.Root ):
      SUPER().
      THIS-OBJECT:Root = r.
      
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ControlShell.
      THIS-OBJECT:KalkylAdmKatControl = THIS-OBJECT:ControlShell:KalkylAdmKatControl.
      THIS-OBJECT:KalkylAdmPrisControl = THIS-OBJECT:ControlShell:KalkylAdmPrisControl.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:splitContainerLopsubBild:Panel2Collapsed = TRUE.
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
	METHOD PRIVATE VOID AddPrisButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      IF THIS-OBJECT:GridLopnumsub:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot - THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE.
         THIS-OBJECT:AddPrisButtonBackMark:GuruOver().      
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
         INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT TRUE).
         THIS-OBJECT:ControlShell:GridLopnumsubTitle().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
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
	METHOD PRIVATE VOID AddPrisButtonOverMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      IF THIS-OBJECT:AddPrisButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:AddPrisButtonOverMark:GuruOver().
         IF THIS-OBJECT:AddPrisButtonOverMark:GuruFanns = TRUE THEN  rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(211), "").
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:AddPrisLopnrSub(). 
            THIS-OBJECT:AddPrisButtonOverMark:GuruOverefteruppdat().
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
            INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT FALSE).
            THIS-OBJECT:ControlShell:GridLopnumsubTitle().    
            THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
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
	METHOD PRIVATE VOID ButtonBortKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridArbKoder:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
 
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortArbKod(STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE)). 
      THIS-OBJECT:GridArbKoder:GuruBortrader(FALSE).
     /*
      THIS-OBJECT:RefreshArbKoder(?,?).
      THIS-OBJECT:GridArbKoder:GuruReopen().
      */
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridLopnr:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortLopnum(INPUT INTEGER(STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE))). 
      THIS-OBJECT:GridLopnr:GuruBortrader(FALSE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		IF THIS-OBJECT:GridArbKoder:Posn() = -1 THEN RETURN.				
      IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "< NY KOD >" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).            
         RETURN.
      END.
		THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.		
		IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:         
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).                     
         RETURN.
      END.   		
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyKoder(INPUT STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE)).
      THIS-OBJECT:ControlShell:RefreshArbKoder(?,?).
      THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.
      THIS-OBJECT:GridArbKoder:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nylopnr AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridLopnr:Posn() = -1 THEN RETURN.
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyLopnum(INPUT INTEGER(STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE)),OUTPUT nylopnr).
      THIS-OBJECT:GridLopnr:GuruReopen().
      THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE LOPNR = " + STRING(nylopnr))  NO-ERROR.
      THIS-OBJECT:GridLopnr:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyKoderStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
	   THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.	   
	   IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:         
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).                     
         RETURN.
      END. 	   
		THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyKoder(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, INPUT THIS-OBJECT:ControlShell:GetVisaTyp()).
      THIS-OBJECT:ControlShell:RefreshArbKoder(?,?).
      THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.
      THIS-OBJECT:GridArbKoder:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID).
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonImpKatxmlStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:XmlkatImport() = TRUE THEN DO:
      
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKatalog(?).
         THIS-OBJECT:InReopen().
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(164), "").      
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonExpkatXmlStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:XmlkatExport(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
      /*THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyKoder(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, INPUT THIS-OBJECT:ControlShell:GetVisaTyp()).
      THIS-OBJECT:ControlShell:RefreshArbKoder(?,?).
      THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + "< Ny kod >" + "'") NO-ERROR.
      THIS-OBJECT:GridArbKoder:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID).*/
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyLopnrStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nylopnr AS INTEGER NO-UNDO.
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "< NY KOD >" THEN DO:
		   rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).            
         RETURN.
		END.   
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyLopnum(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,OUTPUT nylopnr).
      THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE LOPNR = " + STRING(nylopnr))  NO-ERROR.
      THIS-OBJECT:GridLopnr:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:LopsubTTh:ROWID).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID DelkatalogToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE inlasstart AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:indelkatexcel_UI(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart).
      IF inlasstart = TRUE THEN DO:         
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKatalog(?).
         THIS-OBJECT:InReopen().       
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(164), "").
                  
      END.
      ELSE  THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID FrekvenstoolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE inlasstart AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:inxmlfrek_UI(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart).
      /*RUN inxmlfrek_UI IN katfrekinH (INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart).*/
      IF inlasstart = TRUE THEN DO:
         THIS-OBJECT:GridArbKoder:GuruReopen().
         THIS-OBJECT:GridArbKoder:GuruFirstrow().
         THIS-OBJECT:ControlShell:ValueGridArbKoderCh(?,?).
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(164), "").
      END.
      ELSE THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAddPris_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		THIS-OBJECT:AddPrisButtonOverMark_Click(?,?).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAddPris_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  
	   DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopnumsub THEN DO:
         THIS-OBJECT:AddPrisButtonBackMark_Click(?,?).
      END.
      RETURN.
		

	END METHOD.
  
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE inklogsubid AS INTEGER NO-UNDO.
      DEFINE VARIABLE inrecid AS RECID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF kolnamn = "ARBKOD" THEN THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
      ELSE THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE) + "'") NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:AVAILABLE THEN DO:
         inklogsubid = THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
         inrecid = THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
      END.
      ELSE RETURN.   
      IF kolnamn = "ARBKOD" THEN DO:
         IF kolvarde = "< Ny kod >" OR kolvarde = "" THEN DO:
            /*borde inte kunna hända*/
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE = beforekolvarde.
            
            RETURN.
         END.  
         ELSE DO:
            IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
            IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbKoderKoll(INPUT inklogsubid,INPUT kolvarde,INPUT inrecid) = FALSE THEN DO:
               THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID).
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
               RETURN.
            END.
            ELSE DO:
               THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbLopnrFix(INPUT beforekolvarde,INPUT kolvarde).  
               THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
               THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().
               THIS-OBJECT:GridArbKoder:GuruTom().
               THIS-OBJECT:ControlShell:RefreshArbKoder(?,?).
               THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + kolvarde + "'") NO-ERROR.
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:ROWID).             
              
               
            END.                    
         END.    
      END.
      ELSE DO:   
         IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
         THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KoderSpara().        
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridArbKoder:Guruegenskap:getRubrik(beforekolnamn):Readonly = TRUE THEN DO:
         e:Cancel = TRUE.
         RETURN.  
      END.
      IF beforekolnamn = "ARBKOD" THEN DO:
         IF beforekolvarde = "< Ny kod >" THEN.
         ELSE DO:
            /*du får inte ändra på en befintlig kod*/
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            e:Cancel = TRUE.
            RETURN.
         END.
      END.      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFrekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridFrekvens:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkAdmdbControl:FrekvensTTh:FIND-FIRST("WHERE KLOGSUBID = " + STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE) +
      " AND ARBKOD = '" + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["ARBKOD"]:VALUE ) + 
      "' AND LOPNR = " + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["LOPNR"]:VALUE ) +
      " AND FREKOD = '" + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["FREKOD"]:VALUE ) + 
      "' AND FREKNR = " + STRING( THIS-OBJECT:GridFrekvens:ActiveRow:Cells["FREKNR"]:VALUE )) NO-ERROR.
      THIS-OBJECT:KalkAdmdbControl:FrekvensTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FrekSpara().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnr_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridLopnr:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF kolnamn = "LOPNR" THEN DO:
         IF beforekolvarde NE kolvarde THEN DO:
            
            THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING( THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE ) + "' AND LOPNR = " + 
            STRING(kolvarde) + " AND KLOGSUBID = " + STRING( THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE )) NO-ERROR.
            IF THIS-OBJECT:KalkAdmdbControl:LopposterTTh:AVAILABLE THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(166)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
               THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE TTRECID = " + STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["TTRECID"]:VALUE)) NO-ERROR.
                
               THIS-OBJECT:GridLopnr:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:ROWID).
            /*
               THIS-OBJECT:GridArbKoder:ActiveRow:Cells["LOPNR"]:VALUE = beforekolvarde.
               */
               RETURN.
            END.
            
            
         END.
         THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE TTRECID = " + STRING(THIS-OBJECT:GridLopnr:ActiveRow:Cells["TTRECID"]:VALUE)) NO-ERROR.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:AndLopnum(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, THIS-OBJECT:KalkAdmdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,INPUT INTEGER(beforekolvarde),INPUT INTEGER(kolvarde)).
        
         THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
      END.
      ELSE DO:   
         THIS-OBJECT:KalkAdmdbControl:LopposterTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING( THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE ) + "' AND LOPNR = " + 
         STRING( THIS-OBJECT:GridLopnr:ActiveRow:Cells["LOPNR"]:VALUE ) + " AND KLOGSUBID = " + STRING( THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE )) NO-ERROR.         
         THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
      END.   
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnr_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnumsub_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridLopnumsub:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:KalkAdmdbControl:LopposterTTh:AVAILABLE THEN DO:
         queryvar = "WHERE ARBKOD = '" + STRING(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) +           
                                           "' AND LOPNR = " + STRING(THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) +
                                           " AND KPID = " + STRING(THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["KPID"]:VALUE ) +
                                           " AND KLOGSUBID = " + STRING( THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["KLOGSUBID"]:VALUE ).
         
         THIS-OBJECT:KalkAdmdbControl:LopsubTTh:FIND-FIRST(queryvar) NO-ERROR.
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot - THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE.
         
         THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         
         THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE = THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE +     
         THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE * THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.  
         STRING(THIS-OBJECT:GridLopnumsub:ActiveRow:Cells["POSTKOST"]:VALUE ) = STRING(THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE).     
         
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KalkylKodTot + THIS-OBJECT:KalkAdmdbControl:LopsubTTh:BUFFER-FIELD("POSTKOST"):BUFFER-VALUE. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KodKostTotSetGet(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
         INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE, INPUT TRUE).
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
         THIS-OBJECT:ControlShell:GridLopnumSubTitle().
      END.      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopnumsub_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridAddPris THEN DO:
         THIS-OBJECT:AddPrisButtonOverMark_Click(?,?).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridSubKatKoder:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatKoder:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmKod").
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
      appearance63 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
      appearance64 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
      appearance66 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
      appearance67 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
      appearance68 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
      appearance69 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
      appearance70 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
      appearance71 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
      appearance72 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
      appearance73 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
      appearance74 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerGrundKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatKoderStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:SubKatKodStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:LasintoolStrip = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:KatalogtoolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:FrekvenstoolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:P2kommtoolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:DelkatalogToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:ButtonImpKatXmlStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonExpKatXmlStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridSubKatKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerNyKopKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortKoderStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridArbKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopNrStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortLopnrStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridLopnr = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopsubBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopsubBildStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:LopSubBildStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:splitContainerLopSubFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyPrisAdd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAddPris = NEW Controls.Grid().
      THIS-OBJECT:AddPrisButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:AddPrisButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridLopnumsub = NEW Controls.Grid().
      THIS-OBJECT:splitContainerFrekKom = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFrekvens = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:Rubrikanm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      CAST(THIS-OBJECT:splitContainerGrundKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerGrundKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatKoderStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:SuspendLayout().
      THIS-OBJECT:SubKatKodStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerNyKopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:SuspendLayout().
      THIS-OBJECT:KoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:SuspendLayout().
      THIS-OBJECT:LopNrStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopsubBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopsubBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopsubBildStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLopSubFrek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyPrisAdd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyPrisAdd:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyPrisAdd:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyPrisAdd:SuspendLayout().
      CAST(THIS-OBJECT:GridAddPris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridLopnumsub, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerFrekKom, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFrekKom:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFrekKom:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFrekKom:SuspendLayout().
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerGrundKoder */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundKoder:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerGrundKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundKoder:Name = "splitContainerGrundKoder".
      /*  */
      /* splitContainerGrundKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatKoder).
      /*  */
      /* splitContainerGrundKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnr).
      THIS-OBJECT:splitContainerGrundKoder:Size = NEW System.Drawing.Size(2500, 556).
      THIS-OBJECT:splitContainerGrundKoder:SplitterDistance = 655.
      THIS-OBJECT:splitContainerGrundKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerGrundKoder:TabIndex = 1.
      /*  */
      /* splitContainerSubKatKoder */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatKoder:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerSubKatKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatKoder:Name = "splitContainerSubKatKoder".
      /*  */
      /* splitContainerSubKatKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatKoderStrip).
      /*  */
      /* splitContainerSubKatKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerNyKopKoder).
      THIS-OBJECT:splitContainerSubKatKoder:Size = NEW System.Drawing.Size(655, 556).
      THIS-OBJECT:splitContainerSubKatKoder:SplitterDistance = 380.
      THIS-OBJECT:splitContainerSubKatKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatKoder:TabIndex = 0.
      /*  */
      /* splitContainerSubKatKoderStrip */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatKoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Name = "splitContainerSubKatKoderStrip".
      THIS-OBJECT:splitContainerSubKatKoderStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSubKatKoderStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:Controls:Add(THIS-OBJECT:SubKatKodStrip).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerSubKatKoderStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:Controls:Add(THIS-OBJECT:GridSubKatKoder).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Size = NEW System.Drawing.Size(380, 556).
      THIS-OBJECT:splitContainerSubKatKoderStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerSubKatKoderStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatKoderStrip:TabIndex = 2.
      /*  */
      /* SubKatKodStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:LasintoolStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonImpKatXmlStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonExpKatXmlStrip.
      THIS-OBJECT:SubKatKodStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:SubKatKodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:SubKatKodStrip:Name = "SubKatKodStrip".
      THIS-OBJECT:SubKatKodStrip:Size = NEW System.Drawing.Size(380, 25).
      THIS-OBJECT:SubKatKodStrip:TabIndex = 1.
      THIS-OBJECT:SubKatKodStrip:Text = "toolStrip1".
      /*  */
      /* LasintoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:KatalogtoolStripMenuItem.
      arrayvar1[2] = THIS-OBJECT:FrekvenstoolStripMenuItem.
      arrayvar1[3] = THIS-OBJECT:P2kommtoolStripMenuItem.
      arrayvar1[4] = THIS-OBJECT:DelkatalogToolStripMenuItem.
      THIS-OBJECT:LasintoolStrip:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:LasintoolStrip:Image = CAST(resources:GetObject("LasintoolStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:LasintoolStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:LasintoolStrip:Name = "LasintoolStrip".
      THIS-OBJECT:LasintoolStrip:Size = NEW System.Drawing.Size(118, 22).
      THIS-OBJECT:LasintoolStrip:Text = "Läs in kataloger".
      /*  */
      /* KatalogtoolStripMenuItem */
      /*  */
      THIS-OBJECT:KatalogtoolStripMenuItem:Name = "KatalogtoolStripMenuItem".
      THIS-OBJECT:KatalogtoolStripMenuItem:Size = NEW System.Drawing.Size(218, 22).
      THIS-OBJECT:KatalogtoolStripMenuItem:Text = "Läs in katalog EBR".
      THIS-OBJECT:KatalogtoolStripMenuItem:Click:Subscribe(THIS-OBJECT:KatalogtoolStripMenuItem_Click).
      /*  */
      /* FrekvenstoolStripMenuItem */
      /*  */
      THIS-OBJECT:FrekvenstoolStripMenuItem:Name = "FrekvenstoolStripMenuItem".
      THIS-OBJECT:FrekvenstoolStripMenuItem:Size = NEW System.Drawing.Size(218, 22).
      THIS-OBJECT:FrekvenstoolStripMenuItem:Text = "Läs in frekvenser EBR".
      THIS-OBJECT:FrekvenstoolStripMenuItem:Click:Subscribe(THIS-OBJECT:FrekvenstoolStripMenuItem_Click).
      /*  */
      /* P2kommtoolStripMenuItem */
      /*  */
      THIS-OBJECT:P2kommtoolStripMenuItem:Name = "P2kommtoolStripMenuItem".
      THIS-OBJECT:P2kommtoolStripMenuItem:Size = NEW System.Drawing.Size(218, 22).
      THIS-OBJECT:P2kommtoolStripMenuItem:Text = "Läs in P2 kommentarer EBR".
      THIS-OBJECT:P2kommtoolStripMenuItem:Click:Subscribe(THIS-OBJECT:P2kommtoolStripMenuItem_Click).
      /*  */
      /* DelkatalogToolStripMenuItem */
      /*  */
      THIS-OBJECT:DelkatalogToolStripMenuItem:Name = "DelkatalogToolStripMenuItem".
      THIS-OBJECT:DelkatalogToolStripMenuItem:Size = NEW System.Drawing.Size(218, 22).
      THIS-OBJECT:DelkatalogToolStripMenuItem:Text = "Läs in delkatalog från Excel".
      THIS-OBJECT:DelkatalogToolStripMenuItem:Click:Subscribe(THIS-OBJECT:DelkatalogToolStripMenuItem_Click).
      /*  */
      /* ButtonImpKatXmlStrip */
      /*  */
      THIS-OBJECT:ButtonImpKatXmlStrip:Image = CAST(resources:GetObject("ButtonImpKatXmlStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonImpKatXmlStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonImpKatXmlStrip:Name = "ButtonImpKatXmlStrip".
      THIS-OBJECT:ButtonImpKatXmlStrip:Size = NEW System.Drawing.Size(132, 22).
      THIS-OBJECT:ButtonImpKatXmlStrip:Text = "Import katalog XML".
      THIS-OBJECT:ButtonImpKatXmlStrip:Click:Subscribe(THIS-OBJECT:ButtonImpKatXmlStrip_Click).
      /*  */
      /* ButtonExpKatXmlStrip */
      /*  */
      THIS-OBJECT:ButtonExpKatXmlStrip:Image = CAST(resources:GetObject("ButtonExpKatXmlStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonExpKatXmlStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonExpKatXmlStrip:Name = "ButtonExpKatXmlStrip".
      THIS-OBJECT:ButtonExpKatXmlStrip:Size = NEW System.Drawing.Size(130, 20).
      THIS-OBJECT:ButtonExpKatXmlStrip:Text = "Export Katalog XML".
      THIS-OBJECT:ButtonExpKatXmlStrip:Click:Subscribe(THIS-OBJECT:ButtonExpKatXmlStrip_Click).
      /*  */
      /* GridSubKatKoder */
      /*  */
      THIS-OBJECT:GridSubKatKoder:AlphaBlendMode = Infragistics.Win.AlphaBlendMode:Disabled.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatKoder:GuruContext = ?.
      THIS-OBJECT:GridSubKatKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatKoder:Name = "GridSubKatKoder".
      THIS-OBJECT:GridSubKatKoder:Size = NEW System.Drawing.Size(380, 528).
      THIS-OBJECT:GridSubKatKoder:TabIndex = 1.
      THIS-OBJECT:GridSubKatKoder:Text = "Grid1".
      THIS-OBJECT:GridSubKatKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatKoder_AfterCellUpdate).
      /*  */
      /* splitContainerNyKopKoder */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopKoder:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopKoder:Name = "splitContainerNyKopKoder".
      THIS-OBJECT:splitContainerNyKopKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:Controls:Add(THIS-OBJECT:KoderStrip).
      THIS-OBJECT:splitContainerNyKopKoder:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:Controls:Add(THIS-OBJECT:GridArbKoder).
      THIS-OBJECT:splitContainerNyKopKoder:Size = NEW System.Drawing.Size(274, 556).
      THIS-OBJECT:splitContainerNyKopKoder:SplitterDistance = 27.
      THIS-OBJECT:splitContainerNyKopKoder:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopKoder:TabIndex = 2.
      /*  */
      /* KoderStrip */
      /*  */
      THIS-OBJECT:KoderStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:ButtonNyKoderStrip.
      arrayvar2[2] = THIS-OBJECT:ButtonKopKoderStrip.
      arrayvar2[3] = THIS-OBJECT:ButtonBortKoderStrip.
      THIS-OBJECT:KoderStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:KoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KoderStrip:Name = "KoderStrip".
      THIS-OBJECT:KoderStrip:Size = NEW System.Drawing.Size(274, 27).
      THIS-OBJECT:KoderStrip:TabIndex = 0.
      THIS-OBJECT:KoderStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyKoderStrip */
      /*  */
      THIS-OBJECT:ButtonNyKoderStrip:Image = CAST(resources:GetObject("ButtonNyKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyKoderStrip:Name = "ButtonNyKoderStrip".
      THIS-OBJECT:ButtonNyKoderStrip:Size = NEW System.Drawing.Size(92, 24).
      THIS-OBJECT:ButtonNyKoderStrip:Text = "Lägg till kod".
      THIS-OBJECT:ButtonNyKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonNyKoderStrip_Click).
      /*  */
      /* ButtonKopKoderStrip */
      /*  */
      THIS-OBJECT:ButtonKopKoderStrip:Image = CAST(resources:GetObject("ButtonKopKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopKoderStrip:Name = "ButtonKopKoderStrip".
      THIS-OBJECT:ButtonKopKoderStrip:Size = NEW System.Drawing.Size(90, 24).
      THIS-OBJECT:ButtonKopKoderStrip:Text = "Kopiera kod".
      THIS-OBJECT:ButtonKopKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonKopKoderStrip_Click).
      /*  */
      /* ButtonBortKoderStrip */
      /*  */
      THIS-OBJECT:ButtonBortKoderStrip:Image = CAST(resources:GetObject("ButtonBortKoderStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortKoderStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortKoderStrip:Name = "ButtonBortKoderStrip".
      THIS-OBJECT:ButtonBortKoderStrip:Size = NEW System.Drawing.Size(88, 20).
      THIS-OBJECT:ButtonBortKoderStrip:Text = "Ta bort kod".
      THIS-OBJECT:ButtonBortKoderStrip:Click:Subscribe(THIS-OBJECT:ButtonBortKoderStrip_Click).
      /*  */
      /* GridArbKoder */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridArbKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbKoder:GuruContext = ?.
      THIS-OBJECT:GridArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbKoder:Name = "GridArbKoder".
      THIS-OBJECT:GridArbKoder:Size = NEW System.Drawing.Size(274, 528).
      THIS-OBJECT:GridArbKoder:TabIndex = 1.
      THIS-OBJECT:GridArbKoder:Text = "Grid1".
      THIS-OBJECT:GridArbKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_AfterCellUpdate).
      THIS-OBJECT:GridArbKoder:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_BeforeCellUpdate).
      /*  */
      /* splitContainerLopnr */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnr:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnr:Name = "splitContainerLopnr".
      /*  */
      /* splitContainerLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopLopnr).
      /*  */
      /* splitContainerLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopsubBild).
      THIS-OBJECT:splitContainerLopnr:Size = NEW System.Drawing.Size(1844, 556).
      THIS-OBJECT:splitContainerLopnr:SplitterDistance = 425.
      THIS-OBJECT:splitContainerLopnr:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopnr:TabIndex = 0.
      /*  */
      /* splitContainerNyKopLopnr */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopLopnr:Name = "splitContainerNyKopLopnr".
      THIS-OBJECT:splitContainerNyKopLopnr:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:Controls:Add(THIS-OBJECT:LopNrStrip).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:Controls:Add(THIS-OBJECT:GridLopnr).
      THIS-OBJECT:splitContainerNyKopLopnr:Size = NEW System.Drawing.Size(425, 556).
      THIS-OBJECT:splitContainerNyKopLopnr:SplitterDistance = 27.
      THIS-OBJECT:splitContainerNyKopLopnr:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopLopnr:TabIndex = 2.
      /*  */
      /* LopNrStrip */
      /*  */
      THIS-OBJECT:LopNrStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:ButtonNyLopnrStrip.
      arrayvar3[2] = THIS-OBJECT:ButtonKopLopnrStrip.
      arrayvar3[3] = THIS-OBJECT:ButtonBortLopnrStrip.
      THIS-OBJECT:LopNrStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:LopNrStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopNrStrip:Name = "LopNrStrip".
      THIS-OBJECT:LopNrStrip:Size = NEW System.Drawing.Size(425, 27).
      THIS-OBJECT:LopNrStrip:TabIndex = 2.
      THIS-OBJECT:LopNrStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonNyLopnrStrip:Image = CAST(resources:GetObject("ButtonNyLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyLopnrStrip:Name = "ButtonNyLopnrStrip".
      THIS-OBJECT:ButtonNyLopnrStrip:Size = NEW System.Drawing.Size(100, 24).
      THIS-OBJECT:ButtonNyLopnrStrip:Text = "Lägg till löpnr".
      THIS-OBJECT:ButtonNyLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonNyLopnrStrip_Click).
      /*  */
      /* ButtonKopLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonKopLopnrStrip:Image = CAST(resources:GetObject("ButtonKopLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopLopnrStrip:Name = "ButtonKopLopnrStrip".
      THIS-OBJECT:ButtonKopLopnrStrip:Size = NEW System.Drawing.Size(98, 24).
      THIS-OBJECT:ButtonKopLopnrStrip:Text = "Kopiera löpnr".
      THIS-OBJECT:ButtonKopLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonKopLopnrStrip_Click).
      /*  */
      /* ButtonBortLopnrStrip */
      /*  */
      THIS-OBJECT:ButtonBortLopnrStrip:Image = CAST(resources:GetObject("ButtonBortLopnrStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortLopnrStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortLopnrStrip:Name = "ButtonBortLopnrStrip".
      THIS-OBJECT:ButtonBortLopnrStrip:Size = NEW System.Drawing.Size(96, 24).
      THIS-OBJECT:ButtonBortLopnrStrip:Text = "Ta bort löpnr".
      THIS-OBJECT:ButtonBortLopnrStrip:Click:Subscribe(THIS-OBJECT:ButtonBortLopnrStrip_Click).
      /*  */
      /* GridLopnr */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopnr:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridLopnr:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopnr:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnr:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridLopnr:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopnr:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopnr:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridLopnr:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopnr:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopnr:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopnr:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopnr:GuruContext = ?.
      THIS-OBJECT:GridLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopnr:Name = "GridLopnr".
      THIS-OBJECT:GridLopnr:Size = NEW System.Drawing.Size(425, 528).
      THIS-OBJECT:GridLopnr:TabIndex = 1.
      THIS-OBJECT:GridLopnr:Text = "Grid1".
      THIS-OBJECT:GridLopnr:AfterCellUpdate:Subscribe(THIS-OBJECT:GridLopnr_AfterCellUpdate).
      THIS-OBJECT:GridLopnr:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridLopnr_BeforeCellUpdate).
      /*  */
      /* splitContainerLopsubBild */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopsubBild:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerLopsubBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopsubBild:Name = "splitContainerLopsubBild".
      /*  */
      /* splitContainerLopsubBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerLopsubBildStrip).
      /*  */
      /* splitContainerLopsubBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerLopsubBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerLopsubBild:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerLopsubBild:Size = NEW System.Drawing.Size(1418, 556).
      THIS-OBJECT:splitContainerLopsubBild:SplitterDistance = 590.
      THIS-OBJECT:splitContainerLopsubBild:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopsubBild:TabIndex = 0.
      /*  */
      /* splitContainerLopsubBildStrip */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopsubBildStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopsubBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopsubBildStrip:Name = "splitContainerLopsubBildStrip".
      THIS-OBJECT:splitContainerLopsubBildStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopsubBildStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:Controls:Add(THIS-OBJECT:LopSubBildStrip).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerLopsubBildStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopSubFrek).
      THIS-OBJECT:splitContainerLopsubBildStrip:Size = NEW System.Drawing.Size(1418, 556).
      THIS-OBJECT:splitContainerLopsubBildStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerLopsubBildStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopsubBildStrip:TabIndex = 4.
      /*  */
      /* LopSubBildStrip */
      /*  */
      THIS-OBJECT:LopSubBildStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:LopSubBildStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:LopSubBildStrip:Name = "LopSubBildStrip".
      THIS-OBJECT:LopSubBildStrip:Size = NEW System.Drawing.Size(1418, 27).
      THIS-OBJECT:LopSubBildStrip:TabIndex = 2.
      THIS-OBJECT:LopSubBildStrip:Text = "toolStrip1".
      /*  */
      /* splitContainerLopSubFrek */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopSubFrek:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerLopSubFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopSubFrek:Name = "splitContainerLopSubFrek".
      /*  */
      /* splitContainerLopSubFrek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyPris).
      /*  */
      /* splitContainerLopSubFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerFrekKom).
      THIS-OBJECT:splitContainerLopSubFrek:Size = NEW System.Drawing.Size(1418, 528).
      THIS-OBJECT:splitContainerLopSubFrek:SplitterDistance = 700.
      THIS-OBJECT:splitContainerLopSubFrek:SplitterWidth = 1.
      THIS-OBJECT:splitContainerLopSubFrek:TabIndex = 3.
      /*  */
      /* splitContainerNyPris */
      /*  */
      THIS-OBJECT:splitContainerNyPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyPris:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerNyPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyPris:Name = "splitContainerNyPris".
      /*  */
      /* splitContainerNyPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyPrisAdd).
      /*  */
      /* splitContainerNyPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyPris:Panel2:Controls:Add(THIS-OBJECT:GridLopnumsub).
      THIS-OBJECT:splitContainerNyPris:Size = NEW System.Drawing.Size(700, 528).
      THIS-OBJECT:splitContainerNyPris:SplitterDistance = 330.
      THIS-OBJECT:splitContainerNyPris:TabIndex = 0.
      /*  */
      /* splitContainerNyPrisAdd */
      /*  */
      THIS-OBJECT:splitContainerNyPrisAdd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyPrisAdd:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerNyPrisAdd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyPrisAdd:Name = "splitContainerNyPrisAdd".
      /*  */
      /* splitContainerNyPrisAdd.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyPrisAdd:Panel1:Controls:Add(THIS-OBJECT:GridAddPris).
      /*  */
      /* splitContainerNyPrisAdd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyPrisAdd:Panel2:Controls:Add(THIS-OBJECT:AddPrisButtonOverMark).
      THIS-OBJECT:splitContainerNyPrisAdd:Panel2:Controls:Add(THIS-OBJECT:AddPrisButtonBackMark).
      THIS-OBJECT:splitContainerNyPrisAdd:Panel2MinSize = 10.
      THIS-OBJECT:splitContainerNyPrisAdd:Size = NEW System.Drawing.Size(330, 528).
      THIS-OBJECT:splitContainerNyPrisAdd:SplitterDistance = 301.
      THIS-OBJECT:splitContainerNyPrisAdd:TabIndex = 0.
      /*  */
      /* GridAddPris */
      /*  */
      THIS-OBJECT:GridAddPris:AllowDrop = TRUE.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAddPris:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridAddPris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAddPris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAddPris:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAddPris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridAddPris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAddPris:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridAddPris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAddPris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAddPris:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
      THIS-OBJECT:GridAddPris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAddPris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAddPris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAddPris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAddPris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAddPris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAddPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAddPris:GuruContext = ?.
      THIS-OBJECT:GridAddPris:GuruDraggable = TRUE.
      THIS-OBJECT:GridAddPris:GuruDroppable = TRUE.
      THIS-OBJECT:GridAddPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAddPris:Name = "GridAddPris".
      THIS-OBJECT:GridAddPris:Size = NEW System.Drawing.Size(301, 528).
      THIS-OBJECT:GridAddPris:TabIndex = 0.
      THIS-OBJECT:GridAddPris:Text = "grid1".
      THIS-OBJECT:GridAddPris:GuruAfterDropped:Subscribe(THIS-OBJECT:GridAddPris_GuruAfterDropped).
      THIS-OBJECT:GridAddPris:DoubleClickRow:Subscribe(THIS-OBJECT:GridAddPris_DoubleClickRow).
      /*  */
      /* AddPrisButtonOverMark */
      /*  */
      appearance49:Image = CAST(resources:GetObject("appearance49.Image"), System.Object).
      THIS-OBJECT:AddPrisButtonOverMark:Appearance = appearance49.
      THIS-OBJECT:AddPrisButtonOverMark:Location = NEW System.Drawing.Point(0, 230).
      THIS-OBJECT:AddPrisButtonOverMark:Name = "AddPrisButtonOverMark".
      THIS-OBJECT:AddPrisButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:AddPrisButtonOverMark:TabIndex = 16.
      THIS-OBJECT:AddPrisButtonOverMark:Text = "Markerade väljs".
      THIS-OBJECT:AddPrisButtonOverMark:Click:Subscribe(THIS-OBJECT:AddPrisButtonOverMark_Click).
      /*  */
      /* AddPrisButtonBackMark */
      /*  */
      appearance50:Image = CAST(resources:GetObject("appearance50.Image"), System.Object).
      THIS-OBJECT:AddPrisButtonBackMark:Appearance = appearance50.
      THIS-OBJECT:AddPrisButtonBackMark:Location = NEW System.Drawing.Point(0, 275).
      THIS-OBJECT:AddPrisButtonBackMark:Name = "AddPrisButtonBackMark".
      THIS-OBJECT:AddPrisButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:AddPrisButtonBackMark:TabIndex = 17.
      THIS-OBJECT:AddPrisButtonBackMark:Text = "Markerade bort".
      THIS-OBJECT:AddPrisButtonBackMark:Click:Subscribe(THIS-OBJECT:AddPrisButtonBackMark_Click).
      /*  */
      /* GridLopnumsub */
      /*  */
      THIS-OBJECT:GridLopnumsub:AllowDrop = TRUE.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopnumsub:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopnumsub:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopnumsub:GuruContext = ?.
      THIS-OBJECT:GridLopnumsub:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopnumsub:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopnumsub:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopnumsub:Name = "GridLopnumsub".
      THIS-OBJECT:GridLopnumsub:Size = NEW System.Drawing.Size(366, 528).
      THIS-OBJECT:GridLopnumsub:TabIndex = 1.
      THIS-OBJECT:GridLopnumsub:Text = "Grid1".
      THIS-OBJECT:GridLopnumsub:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopnumsub_GuruAfterDropped).
      THIS-OBJECT:GridLopnumsub:AfterCellUpdate:Subscribe(THIS-OBJECT:GridLopnumsub_AfterCellUpdate).
      /*  */
      /* splitContainerFrekKom */
      /*  */
      THIS-OBJECT:splitContainerFrekKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFrekKom:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerFrekKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFrekKom:Name = "splitContainerFrekKom".
      /*  */
      /* splitContainerFrekKom.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFrekKom:Panel1:Controls:Add(THIS-OBJECT:GridFrekvens).
      /*  */
      /* splitContainerFrekKom.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFrekKom:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      THIS-OBJECT:splitContainerFrekKom:Size = NEW System.Drawing.Size(717, 528).
      THIS-OBJECT:splitContainerFrekKom:SplitterDistance = 600.
      THIS-OBJECT:splitContainerFrekKom:TabIndex = 18.
      /*  */
      /* GridFrekvens */
      /*  */
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:GridFrekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFrekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFrekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFrekvens:GuruContext = ?.
      THIS-OBJECT:GridFrekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFrekvens:Name = "GridFrekvens".
      THIS-OBJECT:GridFrekvens:Size = NEW System.Drawing.Size(600, 528).
      THIS-OBJECT:GridFrekvens:TabIndex = 2.
      THIS-OBJECT:GridFrekvens:Text = "grid1".
      THIS-OBJECT:GridFrekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFrekvens_AfterCellUpdate).
      /*  */
      /* splitContainerKalkKomAnm */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKomAnm:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerKalkKomAnm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKomAnm:Name = "splitContainerKalkKomAnm".
      THIS-OBJECT:splitContainerKalkKomAnm:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkKomAnm.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:Controls:Add(THIS-OBJECT:Rubrikanm).
      /*  */
      /* splitContainerKalkKomAnm.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(113, 528).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* Rubrikanm */
      /*  */
      THIS-OBJECT:Rubrikanm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Rubrikanm:Enabled = FALSE.
      THIS-OBJECT:Rubrikanm:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:Rubrikanm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Rubrikanm:Name = "Rubrikanm".
      THIS-OBJECT:Rubrikanm:Size = NEW System.Drawing.Size(113, 33).
      THIS-OBJECT:Rubrikanm:TabIndex = 1.
      THIS-OBJECT:Rubrikanm:Text = "Kommentar".
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:AcceptsReturn = TRUE.
      THIS-OBJECT:ValdKodKommentar:AcceptsTab = TRUE.
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(113, 477).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 34.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
      THIS-OBJECT:ValdKodKommentar:ValueChanged:Subscribe(THIS-OBJECT:ValdKodKommentar_ValueChanged).
      /*  */
      /* KalkylAdmKod */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerGrundKoder).
      THIS-OBJECT:Name = "KalkylAdmKod".
      THIS-OBJECT:Size = NEW System.Drawing.Size(2500, 556).
      THIS-OBJECT:splitContainerGrundKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerGrundKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerSubKatKoderStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatKoderStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatKoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:SubKatKodStrip:ResumeLayout(FALSE).
      THIS-OBJECT:SubKatKodStrip:PerformLayout().
      CAST(THIS-OBJECT:GridSubKatKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopKoder:ResumeLayout(FALSE).
      THIS-OBJECT:KoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KoderStrip:PerformLayout().
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopLopnr:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:LopNrStrip:ResumeLayout(FALSE).
      THIS-OBJECT:LopNrStrip:PerformLayout().
      CAST(THIS-OBJECT:GridLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopsubBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerLopsubBildStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopsubBildStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopsubBildStrip:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopSubFrek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopSubFrek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyPrisAdd:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyPrisAdd:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyPrisAdd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyPrisAdd:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAddPris, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridLopnumsub, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFrekKom:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFrekKom:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFrekKom, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFrekKom:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID InReopen():
      THIS-OBJECT:GridSubKatKoder:GuruReopen().
      THIS-OBJECT:GridSubKatKoder:GuruFirstrow().
      THIS-OBJECT:GridArbKoder:GuruReopen().
      THIS-OBJECT:GridArbKoder:GuruFirstrow().
      THIS-OBJECT:GridLopnr:GuruReopen().
      THIS-OBJECT:GridLopnr:GuruFirstrow().
      THIS-OBJECT:GridLopnumsub:GuruReopen(). 
      THIS-OBJECT:GridLopnr:GuruFirstrow().
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruReopen().
      THIS-OBJECT:KalkylAdmKatControl:GridKatalog:GuruFirstrow().
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruReopen().
      THIS-OBJECT:KalkylAdmPrisControl:GridSubKatalog:GuruFirstrow().
      THIS-OBJECT:ControlShell:ValueGridSubKatKoderCh(?,?).
   END METHOD. 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KatalogtoolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE inlasstart AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:inxmlkat(OUTPUT inlasstart).
      /*RUN inxmlkat_UI IN katfrekinH (OUTPUT inlasstart).*/
      IF inlasstart = TRUE THEN DO:
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchKatalog(?).
         THIS-OBJECT:InReopen().
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(164), "").
         
      END.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID P2kommtoolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE inlasstart1 AS LOGICAL NO-UNDO.
      DEFINE VARIABLE inlasstart2 AS LOGICAL NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE MATCHES "*KLG1*" THEN       
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:inP2komKLG1_UI(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart1).
      /*RUN inP2komKLG1_UI IN katfrekinH (INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart1).*/
      IF THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE MATCHES "*KLG2*" THEN    
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:inP2komKLG2_UI(INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart2).         
      /*RUN inP2komKLG2_UI IN katfrekinH (INPUT THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT inlasstart2).*/
      IF inlasstart1 OR inlasstart2 = TRUE THEN DO:
         THIS-OBJECT:ControlShell:ValueGridLopnrCh(?,?).     
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:FetchFrekvens(INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
         INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(164), "").
      END.
      ELSE THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ValdKodKommentar_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:KalkAdmdbControl:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:KalkAdmdbControl:LopposterTTh:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE = THIS-OBJECT:ValdKodKommentar:Text. 
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:LopnrSpara().
      END. 
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC KalkylAdmKod ( ):

	END DESTRUCTOR.

END CLASS.