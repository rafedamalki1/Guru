 
 /*------------------------------------------------------------------------
    File        : EkgMaterielControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Oct 11 15:48:30 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgMaterielControl INHERITS UserControl: 
  {EKGTH.I}
   DEFINE PUBLIC VARIABLE GridEjTraff AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportMtrl AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridListaEjAnvMtrl AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE importMtrlStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE ListMtrlStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlEjTraff AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE mtrlImportMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlKollSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlListaMenySC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlListEjAnvButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlStartImportListaSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE mtrlMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE mtrlImportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlExportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE laggTillMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridMateriel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonMtrlIn AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonMtrlBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonValjFil AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE uppdateraMtrlKostButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   

 CONSTRUCTOR PUBLIC EkgMaterielControl (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      {EKGDBH.i ControlDataDb}
      
      
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridMateriel().
      THIS-OBJECT:CreateGridImportMtrl().
      THIS-OBJECT:CreateGridListaEjAnvMtrl().  
      THIS-OBJECT:CreateGridEjTraff().      
     
   END METHOD. 
   METHOD PUBLIC VOID CreateGridMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","",FALSE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS","",FALSE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("BASPRIS","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("KALKYLPRIS","",FALSE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("UPPDATDATUM","",TRUE,150,"DATETIME").
      rubrikergrid[10] = NEW Controls.GridRubrik("LASTA","",FALSE,100, "LOG").
      rubrikergrid[11] = NEW Controls.GridRubrik("LEVKOD","",FALSE,50).
      rubrikergrid[12] = NEW Controls.GridRubrik("AKTIV","",FALSE,50, "LOG").
      
      THIS-OBJECT:GridMateriel:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 12, INPUT THIS-OBJECT:ekgmtrlTTh)).
      THIS-OBJECT:GridMateriel:Text = "Materiel".
      THIS-OBJECT:GridMateriel:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeMateriel).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridMateriel:DisplayLayout:Bands[0]:Columns["LEVKOD"]:Hidden = TRUE.
      THIS-OBJECT:GridMateriel:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridMateriel:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
  METHOD PUBLIC VOID CreateGridImportMtrl():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARTNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,500).
      
      THIS-OBJECT:GridImportMtrl:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:importMtrlTTh)).
      THIS-OBJECT:GridImportMtrl:Text = "Inläst materiel".
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportMtrl).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridListaEjAnvMtrl():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70,"DEC").
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70,"DEC").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ENHET","",TRUE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("BASPRIS","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("UPPDATDATUM","",TRUE,150,"DATETIME").
      rubrikergrid[10] = NEW Controls.GridRubrik("LASTA","",TRUE,100, "LOG").      
      THIS-OBJECT:GridListaEjAnvMtrl:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 10, INPUT THIS-OBJECT:listekgmtrlTTh)).
      THIS-OBJECT:GridListaEjAnvMtrl:Text = "Lista ej använt materiel".
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE.
      
      THIS-OBJECT:GridListaEjAnvMtrl:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueListaEjAnvMtrlCH).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEjTraff():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("ARTNR","",TRUE,90).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("LEVKOD","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("MTRLSUMMA","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("MTRLBENAMNING","",TRUE,250).
            
      THIS-OBJECT:GridEjTraff:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 11, INPUT THIS-OBJECT:EjekgP2mtrlTTh)).
      THIS-OBJECT:GridEjTraff:Text = "Använt materiel som ej finns i Katalogen! Måste åtgärdas!".
    
   
   END METHOD.
   
   
   
  
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridmateriel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:MaterielUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.  
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillMtrlButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyMtrlPop:Show().
      THIS-OBJECT:ControlShell:fillinArtNr:GuruText:Select().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID mtrlEjTraff_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE. 
      ELSE DO: 
         IF THIS-OBJECT:mtrlStartImportListaSC:Panel1Collapsed = TRUE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE.
      END.
      IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE THEN DO:
         THIS-OBJECT:mtrlStartImportListaSC:Panel1Collapsed = TRUE.
         THIS-OBJECT:mtrlStartImportListaSC:Panel2Collapsed = FALSE.
         THIS-OBJECT:mtrlKollSC:Panel1Collapsed = TRUE.
         THIS-OBJECT:mtrlKollSC:Panel2Collapsed = FALSE.
         THIS-OBJECT:ControlWinLg:EjMtrllista().
               
      END.   
      /*
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:Controls:Add(THIS-OBJECT:mtrlImportMenuSC).
      */
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID mtrlExportButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(142),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN THIS-OBJECT:ExportMtrl("Pris").
		ELSE THIS-OBJECT:ExportMtrl("").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID mtrlImportButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE. 
      ELSE DO: 
         IF THIS-OBJECT:mtrlStartImportListaSC:Panel2Collapsed = TRUE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE.
      END.   
	   IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE THEN DO:
	      THIS-OBJECT:mtrlStartImportListaSC:Panel1Collapsed = FALSE.
	      THIS-OBJECT:mtrlStartImportListaSC:Panel2Collapsed = TRUE.	      
	   END.   
	   /*Anders Olsson Elpool i Umeå AB  13 feb 2015 09:14:17 
	   varför finns denna rad
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:Controls:Add(THIS-OBJECT:mtrlImportMenuSC).
       
      */
	   
	   
      /*
	   
		IF THIS-OBJECT:mtrlImportMenuSC:VISIBLE = FALSE THEN THIS-OBJECT:mtrlImportMenuSC:VISIBLE = TRUE. 
		ELSE THIS-OBJECT:mtrlImportMenuSC:VISIBLE = FALSE.
		*/ 		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID mtrlListEjAnvButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE. 
      ELSE DO: 
         IF THIS-OBJECT:mtrlStartImportListaSC:Panel1Collapsed = TRUE THEN THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = FALSE.
      END.
      IF THIS-OBJECT:mtrlStartImportListaSC:VISIBLE = TRUE THEN DO:
         THIS-OBJECT:mtrlStartImportListaSC:Panel1Collapsed = TRUE.
         THIS-OBJECT:mtrlStartImportListaSC:Panel2Collapsed = FALSE.
         
         THIS-OBJECT:mtrlKollSC:Panel1Collapsed = FALSE.
         THIS-OBJECT:mtrlKollSC:Panel2Collapsed = TRUE.
         THIS-OBJECT:ControlWinLg:HamtMtrllista().
      END.
      /*Anders Olsson Elpool i Umeå AB  13 feb 2015 09:14:17 
      varför finns denna rad
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:Controls:Add(THIS-OBJECT:mtrlImportMenuSC).
      */
		RETURN.
	END METHOD.

    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortMtrlButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortMtrl().
      RETURN.

   END METHOD.   
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonMtrlBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("Mtrl").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonMtrlIn_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonRin_Click("Mtrl").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonValjFil_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonImport_Click("Mtrl").
		RETURN.

	END METHOD.

  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID uppdateraMtrlKostButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):   
      /*bör inte behöva användas -allt uppdateras direkt*/   
      THIS-OBJECT:ControlWinLg:UppdateraMtrlKost().      
      RETURN.
   END METHOD. 
   
  METHOD PUBLIC VOID ExportMtrl(listaP AS CHARACTER):
     DEFINE VARIABLE path AS CHARACTER NO-UNDO.
     DEFINE VARIABLE blad AS INTEGER NO-UNDO.
     THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
     THIS-OBJECT:ControlWinLg:VisaExcel:direkticell = TRUE.
     THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE.
     THIS-OBJECT:ControlWinLg:VisaExcel:OutExterntWin = FALSE.
      
     
     IF THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE THEN THIS-OBJECT:ControlWinLg:VisaExcel:StartExcelVal(INPUT FALSE).
     THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(FALSE).
     Guru.Konstanter:AmericanSet().
     THIS-OBJECT:ControlWinLg:VisaExcel:NyttBladExcel().
     THIS-OBJECT:ExportMtrlData(listaP).
     THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(TRUE).  
     THIS-OBJECT:ControlWinLg:VisaExcel:SlutExcel(1).
     Guru.Konstanter:EuropeanAmericanReset().
     THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
  END METHOD.  
  
  METHOD PUBLIC VOID ExportMtrlData(listaP AS CHARACTER): 
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
     DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
     DEFINE VARIABLE path AS CHARACTER NO-UNDO.

     THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
     THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).     
     THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("Materiel").
     THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
     THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
     THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
     THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
     THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),12). 
     THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),50).
     THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
     IF listap = "PRIS" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
     END.
     /*Rubriker*/
     THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","ArtNr").
     THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Benämning").
     THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Enhet").
     IF listap = "PRIS" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Kostnad").
     END.
     THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
     queryvar =  "FOR EACH " + THIS-OBJECT:ekgmtrlTTh:TABLE.
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgmtrlTTh,queryvar).
     qH:GET-FIRST().
     DO WHILE qH:QUERY-OFF-END = FALSE:
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight(). 
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight(). 
        IF listap = "PRIS" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ekgmtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        qH:GET-NEXT().         
     END.    
     THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad)).  
  END METHOD.   

   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgMaterielControl").
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
      THIS-OBJECT:mtrlMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrlMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:uppdateraMtrlKostButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlImportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlExportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlListEjAnvButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlEjTraff = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridMateriel = NEW Controls.Grid().
      THIS-OBJECT:mtrlStartImportListaSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrlImportMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:importMtrlStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonValjFil = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonMtrlIn = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonMtrlBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridImportMtrl = NEW Controls.Grid().
      THIS-OBJECT:mtrlListaMenySC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ListMtrlStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:mtrlKollSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridListaEjAnvMtrl = NEW Controls.Grid().
      THIS-OBJECT:GridEjTraff = NEW Controls.Grid().
      CAST(THIS-OBJECT:mtrlMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMainSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlMainSC:SuspendLayout().
      CAST(THIS-OBJECT:mtrlMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:SuspendLayout().
      THIS-OBJECT:mtrltoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:mtrlStartImportListaSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlStartImportListaSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlStartImportListaSC:SuspendLayout().
      CAST(THIS-OBJECT:mtrlImportMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlImportMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlImportMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlImportMenuSC:SuspendLayout().
      THIS-OBJECT:importMtrlStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridImportMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:mtrlListaMenySC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlListaMenySC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlListaMenySC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlListaMenySC:SuspendLayout().
      CAST(THIS-OBJECT:mtrlKollSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlKollSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlKollSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlKollSC:SuspendLayout().
      CAST(THIS-OBJECT:GridListaEjAnvMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridEjTraff, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* mtrlMainSC */
      /*  */
      THIS-OBJECT:mtrlMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlMainSC:Name = "mtrlMainSC".
      /*  */
      /* mtrlMainSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlMainSC:Panel1:Controls:Add(THIS-OBJECT:mtrlMenuSC).
      /*  */
      /* mtrlMainSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlMainSC:Panel2:Controls:Add(THIS-OBJECT:mtrlStartImportListaSC).
      THIS-OBJECT:mtrlMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:mtrlMainSC:SplitterDistance = 580.
      THIS-OBJECT:mtrlMainSC:TabIndex = 3.
      /*  */
      /* mtrlMenuSC */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:mtrlMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlMenuSC:Name = "mtrlMenuSC".
      THIS-OBJECT:mtrlMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* mtrlMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Panel1:Controls:Add(THIS-OBJECT:mtrltoolStrip).
      /*  */
      /* mtrlMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridMateriel).
      THIS-OBJECT:mtrlMenuSC:Size = NEW System.Drawing.Size(580, 553).
      THIS-OBJECT:mtrlMenuSC:SplitterDistance = 25.
      THIS-OBJECT:mtrlMenuSC:TabIndex = 0.
      /*  */
      /* mtrltoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 7 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillMtrlButton.
      arrayvar0[2] = THIS-OBJECT:taBortMtrlButton.
      arrayvar0[3] = THIS-OBJECT:uppdateraMtrlKostButton.
      arrayvar0[4] = THIS-OBJECT:mtrlImportButton.
      arrayvar0[5] = THIS-OBJECT:mtrlExportButton.
      arrayvar0[6] = THIS-OBJECT:mtrlListEjAnvButton.
      arrayvar0[7] = THIS-OBJECT:mtrlEjTraff.
      THIS-OBJECT:mtrltoolStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:mtrltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrltoolStrip:Name = "mtrltoolStrip".
      THIS-OBJECT:mtrltoolStrip:Size = NEW System.Drawing.Size(580, 25).
      THIS-OBJECT:mtrltoolStrip:TabIndex = 1.
      THIS-OBJECT:mtrltoolStrip:Text = "toolStrip1".
      /*  */
      /* laggTillMtrlButton */
      /*  */
      THIS-OBJECT:laggTillMtrlButton:Image = CAST(resources:GetObject("laggTillMtrlButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillMtrlButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillMtrlButton:Name = "laggTillMtrlButton".
      THIS-OBJECT:laggTillMtrlButton:Size = NEW System.Drawing.Size(115, 22).
      THIS-OBJECT:laggTillMtrlButton:Text = "Lägg till materiel".
      THIS-OBJECT:laggTillMtrlButton:Click:Subscribe(THIS-OBJECT:laggTillMtrlButton_Click).
      /*  */
      /* taBortMtrlButton */
      /*  */
      THIS-OBJECT:taBortMtrlButton:Image = CAST(resources:GetObject("taBortMtrlButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortMtrlButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortMtrlButton:Name = "taBortMtrlButton".
      THIS-OBJECT:taBortMtrlButton:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:taBortMtrlButton:Text = "Ta bort materiel".
      THIS-OBJECT:taBortMtrlButton:Click:Subscribe(THIS-OBJECT:taBortMtrlButton_Click).
      /*  */
      /* uppdateraMtrlKostButton */
      /*  */
      THIS-OBJECT:uppdateraMtrlKostButton:Image = CAST(resources:GetObject("uppdateraMtrlKostButton.Image"), System.Drawing.Image).
      THIS-OBJECT:uppdateraMtrlKostButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:uppdateraMtrlKostButton:Name = "uppdateraMtrlKostButton".
      THIS-OBJECT:uppdateraMtrlKostButton:Size = NEW System.Drawing.Size(180, 22).
      THIS-OBJECT:uppdateraMtrlKostButton:Text = "Uppdatera materielkostnader".
      THIS-OBJECT:uppdateraMtrlKostButton:Visible = FALSE.
      THIS-OBJECT:uppdateraMtrlKostButton:Click:Subscribe(THIS-OBJECT:uppdateraMtrlKostButton_Click).
      /*  */
      /* mtrlImportButton */
      /*  */
      THIS-OBJECT:mtrlImportButton:Image = CAST(resources:GetObject("mtrlImportButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlImportButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlImportButton:Name = "mtrlImportButton".
      THIS-OBJECT:mtrlImportButton:Size = NEW System.Drawing.Size(116, 22).
      THIS-OBJECT:mtrlImportButton:Text = "Import från Excel".
      THIS-OBJECT:mtrlImportButton:Click:Subscribe(THIS-OBJECT:mtrlImportButton_Click).
      /*  */
      /* mtrlExportButton */
      /*  */
      THIS-OBJECT:mtrlExportButton:Image = CAST(resources:GetObject("mtrlExportButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlExportButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlExportButton:Name = "mtrlExportButton".
      THIS-OBJECT:mtrlExportButton:Size = NEW System.Drawing.Size(122, 22).
      THIS-OBJECT:mtrlExportButton:Text = "Exportera materiel".
      THIS-OBJECT:mtrlExportButton:Click:Subscribe(THIS-OBJECT:mtrlExportButton_Click).
      /*  */
      /* mtrlListEjAnvButton */
      /*  */
      THIS-OBJECT:mtrlListEjAnvButton:Image = CAST(resources:GetObject("mtrlListEjAnvButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlListEjAnvButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlListEjAnvButton:Name = "mtrlListEjAnvButton".
      THIS-OBJECT:mtrlListEjAnvButton:Size = NEW System.Drawing.Size(148, 20).
      THIS-OBJECT:mtrlListEjAnvButton:Text = "Lista ej använt materiel".
      THIS-OBJECT:mtrlListEjAnvButton:Click:Subscribe(THIS-OBJECT:mtrlListEjAnvButton_Click).
      /*  */
      /* mtrlEjTraff */
      /*  */
      THIS-OBJECT:mtrlEjTraff:Image = CAST(resources:GetObject("mtrlEjTraff.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlEjTraff:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlEjTraff:Name = "mtrlEjTraff".
      THIS-OBJECT:mtrlEjTraff:Size = NEW System.Drawing.Size(189, 20).
      THIS-OBJECT:mtrlEjTraff:Text = "Använt materiel som inte finns".
      THIS-OBJECT:mtrlEjTraff:ToolTipText = "Använt materiel som inte finns materiellistan".
      THIS-OBJECT:mtrlEjTraff:Click:Subscribe(THIS-OBJECT:mtrlEjTraff_Click).
      /*  */
      /* gridMateriel */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridMateriel:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridMateriel:GuruContext = ?.
      THIS-OBJECT:gridMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridMateriel:Name = "gridMateriel".
      THIS-OBJECT:gridMateriel:Size = NEW System.Drawing.Size(580, 524).
      THIS-OBJECT:gridMateriel:TabIndex = 0.
      THIS-OBJECT:gridMateriel:Text = "grid1".
      THIS-OBJECT:gridMateriel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridmateriel_AfterCellUpdate).
      /*  */
      /* mtrlStartImportListaSC */
      /*  */
      THIS-OBJECT:mtrlStartImportListaSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlStartImportListaSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlStartImportListaSC:Name = "mtrlStartImportListaSC".
      THIS-OBJECT:mtrlStartImportListaSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* mtrlStartImportListaSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:Controls:Add(THIS-OBJECT:mtrlImportMenuSC).
      /*  */
      /* mtrlStartImportListaSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlStartImportListaSC:Panel2:Controls:Add(THIS-OBJECT:mtrlListaMenySC).
      THIS-OBJECT:mtrlStartImportListaSC:Size = NEW System.Drawing.Size(561, 553).
      THIS-OBJECT:mtrlStartImportListaSC:SplitterDistance = 301.
      THIS-OBJECT:mtrlStartImportListaSC:TabIndex = 1.
      THIS-OBJECT:mtrlStartImportListaSC:Visible = FALSE.
      /*  */
      /* mtrlImportMenuSC */
      /*  */
      THIS-OBJECT:mtrlImportMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlImportMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:mtrlImportMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlImportMenuSC:Name = "mtrlImportMenuSC".
      THIS-OBJECT:mtrlImportMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* mtrlImportMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlImportMenuSC:Panel1:Controls:Add(THIS-OBJECT:importMtrlStrip).
      /*  */
      /* mtrlImportMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlImportMenuSC:Panel2:Controls:Add(THIS-OBJECT:GridImportMtrl).
      THIS-OBJECT:mtrlImportMenuSC:Size = NEW System.Drawing.Size(561, 301).
      THIS-OBJECT:mtrlImportMenuSC:SplitterDistance = 25.
      THIS-OBJECT:mtrlImportMenuSC:TabIndex = 1.
      /*  */
      /* importMtrlStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:toolStripButtonValjFil.
      arrayvar1[2] = THIS-OBJECT:toolStripButtonMtrlIn.
      arrayvar1[3] = THIS-OBJECT:toolStripButtonMtrlBort.
      THIS-OBJECT:importMtrlStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:importMtrlStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:importMtrlStrip:Name = "importMtrlStrip".
      THIS-OBJECT:importMtrlStrip:Size = NEW System.Drawing.Size(561, 25).
      THIS-OBJECT:importMtrlStrip:TabIndex = 1.
      THIS-OBJECT:importMtrlStrip:Text = "toolStrip1".
      /*  */
      /* toolStripButtonValjFil */
      /*  */
      THIS-OBJECT:toolStripButtonValjFil:Image = CAST(resources:GetObject("toolStripButtonValjFil.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonValjFil:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonValjFil:Name = "toolStripButtonValjFil".
      THIS-OBJECT:toolStripButtonValjFil:Size = NEW System.Drawing.Size(85, 22).
      THIS-OBJECT:toolStripButtonValjFil:Text = "Välj Excelfil".
      THIS-OBJECT:toolStripButtonValjFil:Click:Subscribe(THIS-OBJECT:toolStripButtonValjFil_Click).
      /*  */
      /* toolStripButtonMtrlIn */
      /*  */
      THIS-OBJECT:toolStripButtonMtrlIn:Image = CAST(resources:GetObject("toolStripButtonMtrlIn.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonMtrlIn:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonMtrlIn:Name = "toolStripButtonMtrlIn".
      THIS-OBJECT:toolStripButtonMtrlIn:Size = NEW System.Drawing.Size(88, 22).
      THIS-OBJECT:toolStripButtonMtrlIn:Text = "Läs in listan".
      THIS-OBJECT:toolStripButtonMtrlIn:Click:Subscribe(THIS-OBJECT:toolStripButtonMtrlIn_Click).
      /*  */
      /* toolStripButtonMtrlBort */
      /*  */
      THIS-OBJECT:toolStripButtonMtrlBort:Image = CAST(resources:GetObject("toolStripButtonMtrlBort.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonMtrlBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonMtrlBort:Name = "toolStripButtonMtrlBort".
      THIS-OBJECT:toolStripButtonMtrlBort:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:toolStripButtonMtrlBort:Text = "Ta bort materiel".
      THIS-OBJECT:toolStripButtonMtrlBort:Click:Subscribe(THIS-OBJECT:toolStripButtonMtrlBort_Click).
      /*  */
      /* GridImportMtrl */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportMtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportMtrl:GuruContext = ?.
      THIS-OBJECT:GridImportMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportMtrl:Name = "GridImportMtrl".
      THIS-OBJECT:GridImportMtrl:Size = NEW System.Drawing.Size(561, 272).
      THIS-OBJECT:GridImportMtrl:TabIndex = 0.
      THIS-OBJECT:GridImportMtrl:Text = "grid1".
      /*  */
      /* mtrlListaMenySC */
      /*  */
      THIS-OBJECT:mtrlListaMenySC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlListaMenySC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:mtrlListaMenySC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlListaMenySC:Name = "mtrlListaMenySC".
      THIS-OBJECT:mtrlListaMenySC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* mtrlListaMenySC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlListaMenySC:Panel1:Controls:Add(THIS-OBJECT:ListMtrlStrip).
      /*  */
      /* mtrlListaMenySC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlListaMenySC:Panel2:Controls:Add(THIS-OBJECT:mtrlKollSC).
      THIS-OBJECT:mtrlListaMenySC:Size = NEW System.Drawing.Size(561, 248).
      THIS-OBJECT:mtrlListaMenySC:SplitterDistance = 25.
      THIS-OBJECT:mtrlListaMenySC:TabIndex = 1.
      /*  */
      /* ListMtrlStrip */
      /*  */
      THIS-OBJECT:ListMtrlStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ListMtrlStrip:Name = "ListMtrlStrip".
      THIS-OBJECT:ListMtrlStrip:Size = NEW System.Drawing.Size(561, 25).
      THIS-OBJECT:ListMtrlStrip:TabIndex = 2.
      THIS-OBJECT:ListMtrlStrip:Text = "toolStrip1".
      /*  */
      /* mtrlKollSC */
      /*  */
      THIS-OBJECT:mtrlKollSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:mtrlKollSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrlKollSC:Name = "mtrlKollSC".
      /*  */
      /* mtrlKollSC.Panel1 */
      /*  */
      THIS-OBJECT:mtrlKollSC:Panel1:Controls:Add(THIS-OBJECT:GridListaEjAnvMtrl).
      /*  */
      /* mtrlKollSC.Panel2 */
      /*  */
      THIS-OBJECT:mtrlKollSC:Panel2:Controls:Add(THIS-OBJECT:GridEjTraff).
      THIS-OBJECT:mtrlKollSC:Size = NEW System.Drawing.Size(561, 219).
      THIS-OBJECT:mtrlKollSC:SplitterDistance = 187.
      THIS-OBJECT:mtrlKollSC:TabIndex = 2.
      /*  */
      /* GridListaEjAnvMtrl */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridListaEjAnvMtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridListaEjAnvMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridListaEjAnvMtrl:GuruContext = ?.
      THIS-OBJECT:GridListaEjAnvMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridListaEjAnvMtrl:Name = "GridListaEjAnvMtrl".
      THIS-OBJECT:GridListaEjAnvMtrl:Size = NEW System.Drawing.Size(187, 219).
      THIS-OBJECT:GridListaEjAnvMtrl:TabIndex = 1.
      THIS-OBJECT:GridListaEjAnvMtrl:Text = "grid1".
      /*  */
      /* GridEjTraff */
      /*  */
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridEjTraff:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEjTraff:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEjTraff:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEjTraff:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridEjTraff:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEjTraff:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridEjTraff:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEjTraff:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEjTraff:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
      THIS-OBJECT:GridEjTraff:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEjTraff:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEjTraff:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEjTraff:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEjTraff:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEjTraff:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEjTraff:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEjTraff:GuruContext = ?.
      THIS-OBJECT:GridEjTraff:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEjTraff:Name = "GridEjTraff".
      THIS-OBJECT:GridEjTraff:Size = NEW System.Drawing.Size(370, 219).
      THIS-OBJECT:GridEjTraff:TabIndex = 0.
      THIS-OBJECT:GridEjTraff:Text = "grid1".
      /*  */
      /* EkgMaterielControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:mtrlMainSC).
      THIS-OBJECT:Name = "EkgMaterielControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:mtrlMainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:mtrlMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrltoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:mtrltoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlStartImportListaSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlStartImportListaSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlStartImportListaSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlStartImportListaSC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlImportMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlImportMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:mtrlImportMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlImportMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlImportMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:importMtrlStrip:ResumeLayout(FALSE).
      THIS-OBJECT:importMtrlStrip:PerformLayout().
      CAST(THIS-OBJECT:GridImportMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlListaMenySC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlListaMenySC:Panel1:PerformLayout().
      THIS-OBJECT:mtrlListaMenySC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlListaMenySC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlListaMenySC:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlKollSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlKollSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mtrlKollSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:mtrlKollSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridListaEjAnvMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridEjTraff, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.  
   
END CLASS.