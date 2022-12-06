 
 /*------------------------------------------------------------------------
    File        : KalkylAdmKatalog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 30 15:06:08 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.KalkylAdm.KalkylAdmKatalog INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkAdmdbControl           AS Modules.KalkylAdm.KalkylAdmDb NO-UNDO.
	DEFINE PUBLIC VARIABLE ControlShell               AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverHuvKat AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonNyKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonNyDelStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKopSubKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKopKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortSubKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBortKatStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBackHuvKat AS Controls.ButtonOver NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatPrisDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSubKatDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPrisKopDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyKopSubKat AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatHuvKopStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatHuvKop AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKatBildDelStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE NyDelKatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE KatStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE KatBildDelStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSubKatDel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKatalog AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKatHuvDel AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkylAdmKatalog ( INPUT r AS Guru.Root ):
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
	METHOD PRIVATE VOID ButtonBackHuvKat_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*OM INGA KVAR*/
      IF THIS-OBJECT:GridKatHuvDel:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:ControlShell:DontCheck = TRUE.
      THIS-OBJECT:KalkAdmdbControl:Katalogsubtth:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE) +
      " AND KLOGSUBID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
      /*Lena  Olsson Elpool i Umeå AB  6 okt 2014 15:31:56 
       
      */
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE) +
      " AND HKLOGSUBID = " + STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGSUBID"]:VALUE)) NO-ERROR.
      IF THIS-OBJECT:KalkAdmdbControl:KatalogTTh:AVAILABLE THEN DO:              
         THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("HKLOGSUBID"):BUFFER-VALUE = 0.  
         THIS-OBJECT:ControlShell:UppdateraGriddar("kat").       
      END.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KontrollKat(INTEGER(STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGSUBID"]:VALUE))).
      THIS-OBJECT:ButtonBackHuvKat:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:RemoveKopKat().
      THIS-OBJECT:ControlShell:DontCheck = FALSE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE okbort AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortkatOk(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE,?, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(180)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END. 
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortkatkollOk(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE,0, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(182)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END.     
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortKatalog(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE).
      THIS-OBJECT:ControlShell:KatalogAfterBort(). 
      THIS-OBJECT:GridKatalog:GuruBortrader(FALSE).
      THIS-OBJECT:ControlShell:AfterCopyKat().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      /*ROB*/
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      THIS-OBJECT:ControlShell:UppdateraGriddar("subkat").
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortSubKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE okbort AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      IF THIS-OBJECT:GridSubKatDel:Posn() = -1 THEN RETURN.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortkatOk(?,THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(180)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END. 
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortkatkollOk(?,THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT okbort). 
      IF okbort = FALSE THEN DO:
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(181)," ",System.Windows.Forms.MessageBoxButtons:Ok).
         RETURN.
      END.  
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:BortSubKatalog(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
      
      
      
      THIS-OBJECT:ControlShell:KatalogAfterBort().
      THIS-OBJECT:GridSubKatDel:GuruBortrader(FALSE).
      THIS-OBJECT:ControlShell:AfterCopyKat().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KatalogSpara().
      /*ROB*/
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      THIS-OBJECT:ControlShell:UppdateraGriddar("subkat").      
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nykatid AS INTEGER NO-UNDO.
      /*
      THIS-OBJECT:ButtonKopKat:Text = "Kopierar Vänta!".
      */
      IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopyKatalog(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE,OUTPUT nykatid). 
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(nykatid)) NO-ERROR.
      THIS-OBJECT:GridKatalog:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:KatalogTTh:ROWID).
      THIS-OBJECT:ControlShell:AfterCopyKat(). 
      /*ROB*/
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      THIS-OBJECT:ControlShell:UppdateraGriddar("subkat").
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      /*
      THIS-OBJECT:ButtonKopKat:Text = "Kopiera Katalog".
      */
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopSubKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nysubkatid AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridSubKatDel:Posn() = -1 THEN RETURN.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      
      IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CopySubKatalog(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE, OUTPUT nysubkatid) = TRUE THEN DO: 
         THIS-OBJECT:GridSubKatDel:GuruReopen().
         THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(nysubkatid)) NO-ERROR.
         THIS-OBJECT:GridSubKatDel:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
         THIS-OBJECT:ControlShell:AfterCopyKat().
         /*ROB*/
         THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
         THIS-OBJECT:ControlShell:UppdateraGriddar("subkat").
      END.   
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyDelStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE nysubkatid AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NySubKatalog(OUTPUT nysubkatid). 
      THIS-OBJECT:GridSubKatDel:GuruReopen().
      THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:FIND-FIRST("WHERE KLOGSUBID = " + STRING(nysubkatid)) NO-ERROR.
      THIS-OBJECT:GridSubKatDel:GuruRepositionTo( THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:ROWID).
      THIS-OBJECT:ControlShell:AfterCopyKat().
      /*ROB*/
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      THIS-OBJECT:ControlShell:UppdateraGriddar("subkat").
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyKatStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		DEFINE VARIABLE nykatid AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:NyKatalog(OUTPUT nykatid). 
      THIS-OBJECT:GridKatalog:GuruReopen().
      THIS-OBJECT:KalkAdmdbControl:KatalogTTh:FIND-FIRST("WHERE KLOGID = " + STRING(nykatid)) NO-ERROR.
      THIS-OBJECT:GridKatalog:GuruRepositionTo(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:ROWID).
      THIS-OBJECT:ControlShell:AfterCopyKat().
      /*ROB*/
      THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
      THIS-OBJECT:ControlShell:ValueGridKatalogCh(?,?).
      THIS-OBJECT:ControlShell:UppdateraGriddar("subkat"). 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      RETURN.
		
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOverHuvKat_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridKatalog:Posn() = -1 THEN RETURN. 
      /*
      IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KollaKoppKatRows(INTEGER(STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE)),INTEGER(STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE))) = TRUE THEN DO:
         THIS-OBJECT:ButtonOverHuvKat:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateKoppKatRows(INTEGER(STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE))).
         THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
         THIS-OBJECT:ButtonOverHuvKat:GuruOverefteruppdat().
      END.
      */
      IF THIS-OBJECT:Root:DatabaseManager:KalkylAdm:KollaKoppKatRows(INTEGER(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE),INTEGER(THIS-OBJECT:KalkAdmdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE)) = TRUE THEN DO:
         THIS-OBJECT:ButtonOverHuvKat:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CreateKoppKatRows(INTEGER(THIS-OBJECT:KalkAdmdbControl:KatalogTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:UppdateraGriddar("kat").
         THIS-OBJECT:ButtonOverHuvKat:GuruOverefteruppdat().
      END.
      
      ELSE DO:  
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(61)," ",System.Windows.Forms.MessageBoxButtons:Ok).
      END.   
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatalog_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      
      IF THIS-OBJECT:GridKatalog:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      /*Anders Olsson Elpool i Umeå AB  29 jan 2015 10:13:52 
       THIS-OBJECT:ControlShell:CellChvar är till för logiska fält så att dom uppdateras vid förändring
       GridKatalog_CellChange om CellChvar är true slipper man köra KatalogUppdate två gånger.
      */
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridKatHuvDel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKatalog THEN DO:
         THIS-OBJECT:ButtonOverHuvKat_Click(?,?).
      END.
      RETURN.
 
   END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatalog_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
     
      IF THIS-OBJECT:GridKatalog:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      THIS-OBJECT:ControlShell:CellChvar = FALSE.
      
      IF kolnamn = "AVSLUTAD" OR kolnamn = "SEKRETESS" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatalog:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         THIS-OBJECT:ControlShell:CellChvar = TRUE.
      END. 
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatHuvDel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKatHuvDel:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKatHuvDel_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      THIS-OBJECT:ControlShell:CellChvar = FALSE.
       MESSAGE THIS-OBJECT:ControlShell:CellChvar = FALSE "huvkat"
      VIEW-AS ALERT-BOX.
      IF THIS-OBJECT:GridKatHuvDel:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      
      IF kolnamn = "AVSLUTAD" OR kolnamn = "SEKRETESS" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:ControlShell:KatalogUppdate(INPUT STRING(THIS-OBJECT:GridKatHuvDel:ActiveRow:Cells["KLOGID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
         THIS-OBJECT:ControlShell:CellChvar = TRUE.
      END. 
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatDel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridSubKatDel:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:ControlShell:CellChvar = FALSE THEN THIS-OBJECT:ControlShell:KatalogsubUppdate(INPUT STRING(THIS-OBJECT:GridSubKatDel:ActiveRow:Cells["KLOGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridSubKatDel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKatHuvDel THEN DO:
         THIS-OBJECT:ButtonBackHuvKat_Click(?,?).
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
      resources = NEW Progress.Util.ResourceManager("Modules.KalkylAdm.KalkylAdmKatalog").
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
      THIS-OBJECT:splitContainerSubKatDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatPrisDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatHuvKopStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridKatalog = NEW Controls.Grid().
      THIS-OBJECT:splitContainerPrisKopDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatHuvKop = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopSubKat = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:NyDelKatStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyDelStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopSubKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortSubKatStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridSubKatDel = NEW Controls.Grid().
      THIS-OBJECT:buttonOverHuvKat = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBackHuvKat = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerKatBildDelStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatBildDelStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:GridKatHuvDel = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerSubKatDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSubKatPrisDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatHuvKopStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:SuspendLayout().
      THIS-OBJECT:KatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerPrisKopDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKatHuvKop, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKop:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyKopSubKat, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:SuspendLayout().
      THIS-OBJECT:NyDelKatStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridSubKatDel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKatBildDelStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridKatHuvDel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerSubKatDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatDel:Name = "splitContainerSubKatDel".
      /*  */
      /* splitContainerSubKatDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatPrisDel).
      /*  */
      /* splitContainerSubKatDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerSubKatDel.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerSubKatDel:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerSubKatDel:Size = NEW System.Drawing.Size(1056, 553).
      THIS-OBJECT:splitContainerSubKatDel:SplitterDistance = 972.
      THIS-OBJECT:splitContainerSubKatDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatDel:TabIndex = 1.
      /*  */
      /* splitContainerSubKatPrisDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatPrisDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatPrisDel:Name = "splitContainerSubKatPrisDel".
      /*  */
      /* splitContainerSubKatPrisDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatHuvKopStrip).
      /*  */
      /* splitContainerSubKatPrisDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerPrisKopDel).
      THIS-OBJECT:splitContainerSubKatPrisDel:Size = NEW System.Drawing.Size(972, 553).
      THIS-OBJECT:splitContainerSubKatPrisDel:SplitterDistance = 354.
      THIS-OBJECT:splitContainerSubKatPrisDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerSubKatPrisDel:TabIndex = 0.
      /*  */
      /* splitContainerKatHuvKopStrip */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatHuvKopStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatHuvKopStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Name = "splitContainerKatHuvKopStrip".
      THIS-OBJECT:splitContainerKatHuvKopStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatHuvKopStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:Controls:Add(THIS-OBJECT:KatStrip).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerKatHuvKopStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatalog).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Size = NEW System.Drawing.Size(354, 553).
      THIS-OBJECT:splitContainerKatHuvKopStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerKatHuvKopStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatHuvKopStrip:TabIndex = 4.
      /*  */
      /* KatStrip */
      /*  */
      THIS-OBJECT:KatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonNyKatStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonKopKatStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonBortKatStrip.
      THIS-OBJECT:KatStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:KatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatStrip:Name = "KatStrip".
      THIS-OBJECT:KatStrip:Size = NEW System.Drawing.Size(354, 27).
      THIS-OBJECT:KatStrip:TabIndex = 4.
      THIS-OBJECT:KatStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyKatStrip */
      /*  */
      THIS-OBJECT:ButtonNyKatStrip:Image = CAST(resources:GetObject("ButtonNyKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyKatStrip:Name = "ButtonNyKatStrip".
      THIS-OBJECT:ButtonNyKatStrip:Size = NEW System.Drawing.Size(111, 24).
      THIS-OBJECT:ButtonNyKatStrip:Text = "Lägg till katalog".
      THIS-OBJECT:ButtonNyKatStrip:Click:Subscribe(THIS-OBJECT:ButtonNyKatStrip_Click).
      /*  */
      /* ButtonKopKatStrip */
      /*  */
      THIS-OBJECT:ButtonKopKatStrip:Image = CAST(resources:GetObject("ButtonKopKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopKatStrip:Name = "ButtonKopKatStrip".
      THIS-OBJECT:ButtonKopKatStrip:Size = NEW System.Drawing.Size(109, 24).
      THIS-OBJECT:ButtonKopKatStrip:Text = "Kopiera katalog".
      THIS-OBJECT:ButtonKopKatStrip:Visible = FALSE.
      THIS-OBJECT:ButtonKopKatStrip:Click:Subscribe(THIS-OBJECT:ButtonKopKatStrip_Click).
      /*  */
      /* ButtonBortKatStrip */
      /*  */
      THIS-OBJECT:ButtonBortKatStrip:Image = CAST(resources:GetObject("ButtonBortKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortKatStrip:Name = "ButtonBortKatStrip".
      THIS-OBJECT:ButtonBortKatStrip:Size = NEW System.Drawing.Size(107, 24).
      THIS-OBJECT:ButtonBortKatStrip:Text = "Ta bort katalog".
      THIS-OBJECT:ButtonBortKatStrip:Click:Subscribe(THIS-OBJECT:ButtonBortKatStrip_Click).
      /*  */
      /* GridKatalog */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatalog:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridKatalog:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatalog:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatalog:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridKatalog:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatalog:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatalog:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridKatalog:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatalog:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatalog:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatalog:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatalog:GuruContext = ?.
      THIS-OBJECT:GridKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatalog:Name = "GridKatalog".
      THIS-OBJECT:GridKatalog:Size = NEW System.Drawing.Size(354, 525).
      THIS-OBJECT:GridKatalog:TabIndex = 3.
      THIS-OBJECT:GridKatalog:Text = "Grid1".
      THIS-OBJECT:GridKatalog:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatalog_AfterCellUpdate).
      THIS-OBJECT:GridKatalog:CellChange:Subscribe(THIS-OBJECT:GridKatalog_CellChange).
      /*  */
      /* splitContainerPrisKopDel */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPrisKopDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPrisKopDel:Name = "splitContainerPrisKopDel".
      /*  */
      /* splitContainerPrisKopDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatHuvKop).
      /*  */
      /* splitContainerPrisKopDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerKatBildDelStrip).
      THIS-OBJECT:splitContainerPrisKopDel:Size = NEW System.Drawing.Size(617, 553).
      THIS-OBJECT:splitContainerPrisKopDel:SplitterDistance = 388.
      THIS-OBJECT:splitContainerPrisKopDel:SplitterWidth = 1.
      THIS-OBJECT:splitContainerPrisKopDel:TabIndex = 2.
      /*  */
      /* splitContainerKatHuvKop */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatHuvKop:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatHuvKop:Name = "splitContainerKatHuvKop".
      /*  */
      /* splitContainerKatHuvKop.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopSubKat).
      /*  */
      /* splitContainerKatHuvKop.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:Controls:Add(THIS-OBJECT:buttonOverHuvKat).
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:Controls:Add(THIS-OBJECT:ButtonBackHuvKat).
      THIS-OBJECT:splitContainerKatHuvKop:Size = NEW System.Drawing.Size(388, 553).
      THIS-OBJECT:splitContainerKatHuvKop:SplitterDistance = 344.
      THIS-OBJECT:splitContainerKatHuvKop:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatHuvKop:TabIndex = 0.
      /*  */
      /* splitContainerNyKopSubKat */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopSubKat:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerNyKopSubKat:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopSubKat:Name = "splitContainerNyKopSubKat".
      THIS-OBJECT:splitContainerNyKopSubKat:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopSubKat.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:Controls:Add(THIS-OBJECT:NyDelKatStrip).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1MinSize = 10.
      /*  */
      /* splitContainerNyKopSubKat.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:Controls:Add(THIS-OBJECT:GridSubKatDel).
      THIS-OBJECT:splitContainerNyKopSubKat:Size = NEW System.Drawing.Size(344, 553).
      THIS-OBJECT:splitContainerNyKopSubKat:SplitterDistance = 27.
      THIS-OBJECT:splitContainerNyKopSubKat:SplitterWidth = 1.
      THIS-OBJECT:splitContainerNyKopSubKat:TabIndex = 1.
      /*  */
      /* NyDelKatStrip */
      /*  */
      THIS-OBJECT:NyDelKatStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:ButtonNyDelStrip.
      arrayvar1[2] = THIS-OBJECT:ButtonKopSubKatStrip.
      arrayvar1[3] = THIS-OBJECT:ButtonBortSubKatStrip.
      THIS-OBJECT:NyDelKatStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:NyDelKatStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:NyDelKatStrip:Name = "NyDelKatStrip".
      THIS-OBJECT:NyDelKatStrip:Size = NEW System.Drawing.Size(344, 27).
      THIS-OBJECT:NyDelKatStrip:TabIndex = 2.
      THIS-OBJECT:NyDelKatStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyDelStrip */
      /*  */
      THIS-OBJECT:ButtonNyDelStrip:Image = CAST(resources:GetObject("ButtonNyDelStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyDelStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyDelStrip:Name = "ButtonNyDelStrip".
      THIS-OBJECT:ButtonNyDelStrip:Size = NEW System.Drawing.Size(127, 24).
      THIS-OBJECT:ButtonNyDelStrip:Text = "Lägg till delkatalog".
      THIS-OBJECT:ButtonNyDelStrip:Click:Subscribe(THIS-OBJECT:ButtonNyDelStrip_Click).
      /*  */
      /* ButtonKopSubKatStrip */
      /*  */
      THIS-OBJECT:ButtonKopSubKatStrip:Image = CAST(resources:GetObject("ButtonKopSubKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopSubKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopSubKatStrip:Name = "ButtonKopSubKatStrip".
      THIS-OBJECT:ButtonKopSubKatStrip:Size = NEW System.Drawing.Size(125, 24).
      THIS-OBJECT:ButtonKopSubKatStrip:Text = "Kopiera delkatalog".
      THIS-OBJECT:ButtonKopSubKatStrip:Click:Subscribe(THIS-OBJECT:ButtonKopSubKatStrip_Click).
      /*  */
      /* ButtonBortSubKatStrip */
      /*  */
      THIS-OBJECT:ButtonBortSubKatStrip:Image = CAST(resources:GetObject("ButtonBortSubKatStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortSubKatStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortSubKatStrip:Name = "ButtonBortSubKatStrip".
      THIS-OBJECT:ButtonBortSubKatStrip:Size = NEW System.Drawing.Size(123, 20).
      THIS-OBJECT:ButtonBortSubKatStrip:Text = "Ta bort delkatalog".
      THIS-OBJECT:ButtonBortSubKatStrip:Click:Subscribe(THIS-OBJECT:ButtonBortSubKatStrip_Click).
      /*  */
      /* GridSubKatDel */
      /*  */
      THIS-OBJECT:GridSubKatDel:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSubKatDel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSubKatDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSubKatDel:GuruContext = ?.
      THIS-OBJECT:GridSubKatDel:GuruDraggable = TRUE.
      THIS-OBJECT:GridSubKatDel:GuruDroppable = TRUE.
      THIS-OBJECT:GridSubKatDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSubKatDel:Name = "GridSubKatDel".
      THIS-OBJECT:GridSubKatDel:Size = NEW System.Drawing.Size(344, 525).
      THIS-OBJECT:GridSubKatDel:TabIndex = 0.
      THIS-OBJECT:GridSubKatDel:Text = "grid1".
      THIS-OBJECT:GridSubKatDel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridSubKatDel_GuruAfterDropped).
      THIS-OBJECT:GridSubKatDel:AfterCellUpdate:Subscribe(THIS-OBJECT:GridSubKatDel_AfterCellUpdate).
      /*  */
      /* buttonOverHuvKat */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:buttonOverHuvKat:Appearance = appearance25.
      THIS-OBJECT:buttonOverHuvKat:Location = NEW System.Drawing.Point(5, 150).
      THIS-OBJECT:buttonOverHuvKat:Name = "buttonOverHuvKat".
      THIS-OBJECT:buttonOverHuvKat:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverHuvKat:TabIndex = 12.
      THIS-OBJECT:buttonOverHuvKat:Click:Subscribe(THIS-OBJECT:ButtonOverHuvKat_Click).
      /*  */
      /* ButtonBackHuvKat */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:ButtonBackHuvKat:Appearance = appearance26.
      THIS-OBJECT:ButtonBackHuvKat:Location = NEW System.Drawing.Point(5, 180).
      THIS-OBJECT:ButtonBackHuvKat:Name = "ButtonBackHuvKat".
      THIS-OBJECT:ButtonBackHuvKat:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackHuvKat:TabIndex = 13.
      THIS-OBJECT:ButtonBackHuvKat:Click:Subscribe(THIS-OBJECT:ButtonBackHuvKat_Click).
      /*  */
      /* splitContainerKatBildDelStrip */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatBildDelStrip:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKatBildDelStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatBildDelStrip:Name = "splitContainerKatBildDelStrip".
      THIS-OBJECT:splitContainerKatBildDelStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKatBildDelStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:Controls:Add(THIS-OBJECT:KatBildDelStrip).
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerKatBildDelStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:Controls:Add(THIS-OBJECT:GridKatHuvDel).
      THIS-OBJECT:splitContainerKatBildDelStrip:Size = NEW System.Drawing.Size(228, 553).
      THIS-OBJECT:splitContainerKatBildDelStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerKatBildDelStrip:SplitterWidth = 1.
      THIS-OBJECT:splitContainerKatBildDelStrip:TabIndex = 3.
      /*  */
      /* KatBildDelStrip */
      /*  */
      THIS-OBJECT:KatBildDelStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatBildDelStrip:Name = "KatBildDelStrip".
      THIS-OBJECT:KatBildDelStrip:Size = NEW System.Drawing.Size(228, 25).
      THIS-OBJECT:KatBildDelStrip:TabIndex = 4.
      THIS-OBJECT:KatBildDelStrip:Text = "toolStrip2".
      /*  */
      /* GridKatHuvDel */
      /*  */
      THIS-OBJECT:GridKatHuvDel:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKatHuvDel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKatHuvDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKatHuvDel:GuruContext = ?.
      THIS-OBJECT:GridKatHuvDel:GuruDraggable = TRUE.
      THIS-OBJECT:GridKatHuvDel:GuruDroppable = TRUE.
      THIS-OBJECT:GridKatHuvDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKatHuvDel:Name = "GridKatHuvDel".
      THIS-OBJECT:GridKatHuvDel:Size = NEW System.Drawing.Size(228, 525).
      THIS-OBJECT:GridKatHuvDel:TabIndex = 2.
      THIS-OBJECT:GridKatHuvDel:Text = "Grid1".
      THIS-OBJECT:GridKatHuvDel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKatHuvDel_GuruAfterDropped).
      THIS-OBJECT:GridKatHuvDel:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKatHuvDel_AfterCellUpdate).
      THIS-OBJECT:GridKatHuvDel:CellChange:Subscribe(THIS-OBJECT:GridKatHuvDel_CellChange).
      /*  */
      /* KalkylAdmKatalog */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerSubKatDel).
      THIS-OBJECT:Name = "KalkylAdmKatalog".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1056, 553).
      THIS-OBJECT:splitContainerSubKatDel:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSubKatPrisDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSubKatPrisDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatHuvKopStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatHuvKopStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatHuvKopStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatStrip:PerformLayout().
      CAST(THIS-OBJECT:GridKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisKopDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPrisKopDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPrisKopDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKop:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKop:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatHuvKop, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatHuvKop:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopSubKat:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyKopSubKat, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyKopSubKat:ResumeLayout(FALSE).
      THIS-OBJECT:NyDelKatStrip:ResumeLayout(FALSE).
      THIS-OBJECT:NyDelKatStrip:PerformLayout().
      CAST(THIS-OBJECT:GridSubKatDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKatBildDelStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKatBildDelStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKatBildDelStrip:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKatHuvDel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC KalkylAdmKatalog ( ):

	END DESTRUCTOR.

END CLASS.