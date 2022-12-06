 
 /*------------------------------------------------------------------------
    File        : UtbytListaAdm
    Purpose     : INNAN 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri Aug 21 10:24:49 CEST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Modules.Beredning.UtbytListaAdm INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE AddRemoveStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE AddButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE BenLabelBf AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE BenLabelBt AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBt AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBf AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyLi AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonMt AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonMf AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNy AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyLi1 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonSt AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonSf AS Controls.Button NO-UNDO.
   DEFINE PUBLIC VARIABLE comboFranLev AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboLista AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboTillLev AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboUtbTill AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboUtbFran AS Controls.Combo NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE EnrLabelEf AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE EnrLabelEt AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE EnrLabelUt AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE EnrLabelUf AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE ErsLabel AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinListName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridEnrTill AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridEnrFran AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFelImp AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridUtb AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE nyLiPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE nyLiPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE RemoveButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE SokBenBf AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE SokBenBt AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE SokEnrEt AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE SokEnrEf AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE SokEnrUt AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE SokEnrUf AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerListaFel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyBort AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSokEnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitListor AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitNyLista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitPopUp AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitValUtBytlista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitUrvalEnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitSokEnrLev AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitSokEnrHuvL AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitListaEnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitEnrVal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonAter AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonExcel AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtoNy AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripListaNy AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripListRubrik AS System.Windows.Forms.ToolStripLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Beredning.UtbytListaAdmDb                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.	
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC VARIABLE EfBfEnter AS CHARACTER NO-UNDO.	
   DEFINE PUBLIC VARIABLE EtBtEnter AS CHARACTER NO-UNDO.
   
   DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.
   
   DEFINE PRIVATE VARIABLE ViTillLev AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE ViFranLev AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE VARIABLE toolButtoval AS CHARACTER NO-UNDO.
   
   
   
	CONSTRUCTOR PUBLIC UtbytListaAdm (  INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:ROOT = r.
      InitializeComponent().
      
      THIS-OBJECT:SIZE = NEW System.Drawing.Size(Guru.Konstanter:globstorb,Guru.Konstanter:globstorh).
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:UtbyLAdm.
      THIS-OBJECT:ControlDataDb:ControlShell = THIS-OBJECT.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyLi_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		toolButtoval = "Avb".
		THIS-OBJECT:nyLiPop:Close().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNy_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		/*fRÅN GJÖMD PIL UPP FÖR ATT KOPPLA IHOP  GRIDS TA EJ BORT */
		DEFINE VARIABLE rid AS ROWID NO-UNDO.  
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ButtonNy:RadFromGrid() = TRUE THEN DO:
         IF THIS-OBJECT:GridEnrFran:ActiveRow =  ? THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
            RETURN.
         END.
         THIS-OBJECT:GridEnrFran:ActiveRow:Selected = TRUE.
         IF THIS-OBJECT:GridEnrTill:ActiveRow NE ? THEN THIS-OBJECT:GridEnrTill:ActiveRow:Selected = TRUE.
         THIS-OBJECT:ButtonNy:GuruOver().
         
         rid = THIS-OBJECT:ControlDataDb:NyMtrl(INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")))).
         THIS-OBJECT:GridUtb:GuruRepositionto(rid). 
         THIS-OBJECT:GridUtb:GuruReOpen().
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
	METHOD PRIVATE VOID buttonNyLi1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE uidvar AS INTEGER NO-UNDO.
		THIS-OBJECT:nyLiPop:Close().
      IF toolButtoval = "Änd" THEN DO:
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE = THIS-OBJECT:fillinListName:Text.
         uidvar = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE.
         THIS-OBJECT:comboLista:GuruFiltrera().
         THIS-OBJECT:comboLista:VALUE = uidvar.
         THIS-OBJECT:ControlDataDb:UListorspara().
      END.
      IF toolButtoval = "Ny" THEN DO:
         uidvar = 0.
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:FIND-LAST("WHERE ",NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:AVAILABLE THEN DO:
            uidvar = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE + 1.
         END.
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-CREATE().
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE = uidvar.
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE = THIS-OBJECT:fillinListName:Text.
         THIS-OBJECT:comboLista:GuruFiltrera().
         THIS-OBJECT:comboLista:VALUE = uidvar.
         THIS-OBJECT:ControlDataDb:UListorspara().
       END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID comboLista_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		/*
		Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = 'ALLA' ",NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO: 
         THIS-OBJECT:comboUtbFran:VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
         THIS-OBJECT:comboUtbTill:VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
      END.
      */ 
      THIS-OBJECT:ControlDataDb:UtbMtrlracking(FALSE).
      THIS-OBJECT:ControlDataDb:UtbyMtrl(INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")))). 
      
      THIS-OBJECT:OpenqGridUtb().
      RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID comboUtbFran_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:OpenqGridUtb().
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID comboUtbTill_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:OpenqGridUtb().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID fillinListName_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
      keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:buttonNyLi1_Click(?,?).
         e:Handled = TRUE.         
      END.
		RETURN.

	END METHOD.

			/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridUtb_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 
   
      IF THIS-OBJECT:GridUtb:DropedFile NE "" THEN DO:
         THIS-OBJECT:ExcelImporters(THIS-OBJECT:GridUtb:DropedFile).
      END. 
   
		RETURN.

	END METHOD.

/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   
   METHOD PUBLIC VOID SokEnrUtb():
      DEFINE VARIABLE qGridUtb AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD") = ? THEN RETURN.
      IF THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD") = ? THEN RETURN.
      IF STRING(THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD")) = "ALLA" THEN qGridUtb = qGridUtb.
      ELSE DO:
         qGridUtb = " ORGLEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD"))).
      END.   
      IF STRING(THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD")) = "ALLA" THEN qGridUtb = qGridUtb.
      ELSE DO:
         IF qGridUtb = "" THEN.
         ELSE qGridUtb = qGridUtb + " AND ".
         qGridUtb = qGridUtb + " BYTTILLLEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD"))).
      END.  
      IF SokEnrUf:TEXT = "" THEN. 
      ELSE DO:
         IF qGridUtb = "" THEN.
         ELSE qGridUtb = qGridUtb + " AND ". 
         qGridUtb = qGridUtb + "ORGENR BEGINS " + QUOTER(SokEnrUf:TEXT). 
      END.
      IF SokEnrUt:TEXT = "" THEN. 
      ELSE DO:
         IF qGridUtb = "" THEN.
         ELSE qGridUtb = qGridUtb + " AND ".
         qGridUtb = qGridUtb + "BYTTILLENR BEGINS " + QUOTER(SokEnrUt:TEXT).
      END.                      
   
      THIS-OBJECT:GridUtb:Guruegenskap:filter = qGridUtb.
       
      THIS-OBJECT:GridUtb:GuruReopen().
   END METHOD.



	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
   METHOD PUBLIC VOID OpenqGridUtb():
      THIS-OBJECT:SokEnrUtb().
      /*
      DEFINE VARIABLE qGridUtb AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD") = ? THEN RETURN.
      IF THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD") = ? THEN RETURN.
      IF STRING(THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD")) = "ALLA" THEN qGridUtb = qGridUtb.
      ELSE DO:
         qGridUtb = " ORGLEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboUtbFran:SelectedRow:GetCellVALUE("LEVKOD"))).
      END.   
      IF STRING(THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD")) = "ALLA" THEN qGridUtb = qGridUtb.
      ELSE DO:
         IF qGridUtb = "" THEN.
         ELSE qGridUtb = qGridUtb + " AND ".
         qGridUtb = qGridUtb + " BYTTILLLEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboUtbTill:SelectedRow:GetCellVALUE("LEVKOD"))).
      END.   
      THIS-OBJECT:GridUtb:Guruegenskap:filter = qGridUtb.
      THIS-OBJECT:GridUtb:GuruReopen().
      */
   END METHOD.
	METHOD PUBLIC VOID CreateForm():
	   THIS-OBJECT:Text = "Ersättningslistor".
	   THIS-OBJECT:EnrLabelEf:Text = "Sök från " + Guru.Konstanter:genk.
	   THIS-OBJECT:EnrLabelEt:Text = "Sök till " + Guru.Konstanter:genk.
	   THIS-OBJECT:EnrLabelUf:Text = "Sök från " + Guru.Konstanter:genk.
      THIS-OBJECT:EnrLabelUt:Text = "Sök till " + Guru.Konstanter:genk.
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateComboLev().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:splitContainerNyBort:Panel2Collapsed = TRUE.
      
      THIS-OBJECT:splitContainerListaFel:Panel2Collapsed = TRUE.
     
      THIS-OBJECT:comboLista_ValueChanged(?,?).
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridUtb().
      THIS-OBJECT:CreateGridEnrFran().
      THIS-OBJECT:CreateGridEnrTill().
      THIS-OBJECT:CreateGridFelImp().
      
      
      THIS-OBJECT:GridEnrTill:Text = "Till " + Guru.Konstanter:genk.
      THIS-OBJECT:GridEnrFran:Text = "Från " + Guru.Konstanter:genk.
      THIS-OBJECT:GridUtb:Text = "Ersättnings " +  Guru.Konstanter:genk.
     
   END METHOD.
   METHOD PUBLIC VOID CreateGridFunctions():
      THIS-OBJECT:ButtonNy:GuruKoppla(THIS-OBJECT:GridEnrTill,THIS-OBJECT:GridUtb).
      THIS-OBJECT:ButtonNy:GuruDublett = FALSE.
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridUtb():
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ORGLEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("OLEVNAMN","Leverantör",TRUE,75).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ORGENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("OBENAMNING","Benämning",TRUE,150).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("OPRIS","Pris",TRUE,65,"DEC","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BYTTILLLEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BLEVNAMN","Leverantör",TRUE,75).  
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BYTTILLENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BBENAMNING","Benämning",TRUE,150).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BPRIS","Pris",TRUE,65,"DEC","").
      THIS-OBJECT:GridUtb:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE, INPUT antallev, INPUT THIS-OBJECT:ControlDataDb:Utbyttabbuffh)).
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridUtb:DisplayLayout:Bands[0]:Columns["OPRIS"]:Hidden = TRUE.
         THIS-OBJECT:GridUtb:DisplayLayout:Bands[0]:Columns["BPRIS"]:Hidden = TRUE.         
      END.
      
      THIS-OBJECT:GridUtb:GuruReOpen().
      THIS-OBJECT:SkapaGrupper().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridEnrFran():
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
      
      
     
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("NPRIS","Pris",TRUE,65,"DEC","").
       
      THIS-OBJECT:GridEnrFran:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT antallev, INPUT THIS-OBJECT:ControlDataDb:Enrfranbuffh)).
         
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridEnrFran:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
     
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:GridEnrFran:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridEnrFran:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
         /* /*OBSE CASESENSITIVE*/ 
         THIS-OBJECT:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         */
      END.
      THIS-OBJECT:GridEnrFran:VisaViaInternetMenuItemAddAdd().
     
      
  
     
      
       
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridEnrTill():
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LEVKOD","Lev-id",TRUE,55).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("NPRIS","Pris",TRUE,65,"DEC","").
     
      THIS-OBJECT:GridEnrTill:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT antallev, INPUT THIS-OBJECT:ControlDataDb:Enrtillbuffh)). 
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridEnrTill:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:GridEnrTill:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridEnrTill:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
         /* /*OBSE CASESENSITIVE*/ 
         THIS-OBJECT:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         */
      END. 
      THIS-OBJECT:GridEnrTill:GuruReOpen().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFelImp():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("UT","Meddelande",TRUE,600).
      THIS-OBJECT:GridFelImp:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 1, INPUT THIS-OBJECT:ControlDataDb:tiduth)).
      THIS-OBJECT:GridFelImp:Text = "Fel vid inläsning!".
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboLev(): 
      DEFINE VARIABLE cmbLevlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE cmbLevUtbFran AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE cmbLevUtbTill AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE levkod        AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE levnamn      AS Controls.GridRubrik      NO-UNDO.
      cmbLevlist = NEW Controls.GridRubrikLista().
      cmbLevlist:ttBufferHandle = Guru.Konstanter:LeverantorTTh.
      levkod = NEW Controls.GridRubrik().
      levkod:Falt = "LEVKOD".
      levkod:Rubrik = "Lev-ID".
      cmbLevlist:addRubrik(levkod).
      levnamn = NEW Controls.GridRubrik().
      levnamn:Falt = "LEVNAMN".
      levnamn:Rubrik = "Leverantör".
      cmbLevlist:addRubrik(levnamn).
      levnamn:Bredd = 300.
      
      cmbLevUtbFran = NEW Controls.GridRubrikLista().
      cmbLevUtbFran:ttBufferHandle = Guru.Konstanter:LeverantorTTh.
      cmbLevUtbFran:addRubrik(levkod).
      cmbLevUtbFran:addRubrik(levnamn).
      
      cmbLevUtbTill = NEW Controls.GridRubrikLista().
      cmbLevUtbTill:ttBufferHandle = Guru.Konstanter:LeverantorTTh.
      cmbLevUtbTill:addRubrik(levkod).
      cmbLevUtbTill:addRubrik(levnamn).
      
      
      THIS-OBJECT:comboFranLev:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      THIS-OBJECT:comboTillLev:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      
      
      THIS-OBJECT:comboUtbFran:GuruInitCombo(cmbLevUtbFran,"LEVKOD","LEVNAMN").
      THIS-OBJECT:comboUtbTill:GuruInitCombo(cmbLevUtbTill,"LEVKOD","LEVNAMN").
     
      Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = 'ALLA' ",NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO:
         THIS-OBJECT:comboUtbTill:VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
         THIS-OBJECT:comboTillLev:VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
      END.
      
     
      Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = " + QUOTER(Guru.Konstanter:HuvudLeverantor)  ,NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN.
      ELSE Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = 'ALLA' ",NO-LOCK) NO-ERROR.
      IF Guru.Konstanter:LeverantorTTh:AVAILABLE THEN DO: 
         THIS-OBJECT:comboUtbFran:VALUE = Guru.Konstanter:LeverantorTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE. 
         THIS-OBJECT:comboFranLev:VALUE = THIS-OBJECT:comboUtbFran:VALUE.
      END.  
       
        
      cmbLevlist = NEW Controls.GridRubrikLista().
      cmbLevlist:ttBufferHandle = ControlDataDb:UtbytNamnbuffh.
      levkod = NEW Controls.GridRubrik().
      levkod:Falt = "UID".
      levkod:Rubrik = "Nr".
      cmbLevlist:addRubrik(levkod).
      levnamn = NEW Controls.GridRubrik().
      levnamn:Falt = "UNAMN".
      levnamn:Rubrik = "Ersättningslista".
      cmbLevlist:addRubrik(levnamn).
      levnamn:Bredd = 300.
      THIS-OBJECT:comboLista:GuruInitCombo(cmbLevlist,"UID","UNAMN").
      THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:comboLista:VALUE = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE.
      END.
      THIS-OBJECT:comboUtbFran:ValueChanged:Subscribe(THIS-OBJECT:comboUtbFran_ValueChanged).
      THIS-OBJECT:comboUtbTill:ValueChanged:Subscribe(THIS-OBJECT:comboUtbTill_ValueChanged).
      THIS-OBJECT:comboLista:ValueChanged:Subscribe(THIS-OBJECT:comboLista_ValueChanged).
     
   END METHOD.

   METHOD PUBLIC VOID SkapaGrupper():
     THIS-OBJECT:GridUtb:SkapaGrupp(0,"FRAN","Från").
     THIS-OBJECT:GridUtb:SetGroup(0,"FRAN","ORGLEVKOD").
     THIS-OBJECT:GridUtb:SetGroup(0,"FRAN","OLEVNAMN").
     THIS-OBJECT:GridUtb:SetGroup(0,"FRAN","ORGENR").
     THIS-OBJECT:GridUtb:SetGroup(0,"FRAN","OBENAMNING").
     THIS-OBJECT:GridUtb:SetGroup(0,"FRAN","OPRIS").
     THIS-OBJECT:GridUtb:SkapaGrupp(0,"TILL","Till").
     THIS-OBJECT:GridUtb:SetGroup(0,"TILL","BYTTILLLEVKOD").
     THIS-OBJECT:GridUtb:SetGroup(0,"TILL","BLEVNAMN").
     THIS-OBJECT:GridUtb:SetGroup(0,"TILL","BYTTILLENR").
     THIS-OBJECT:GridUtb:SetGroup(0,"TILL","BBENAMNING").
     THIS-OBJECT:GridUtb:SetGroup(0,"TILL","BPRIS").
     
   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokBenBf_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:EfBfEnter = "Bf".
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokBenBf_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
		keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:buttonBf_Click(?,?).
         e:Handled = TRUE.         
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokBenBt_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:EtBtEnter = "Bt".
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokBenBt_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
		keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:buttonBt_Click(?,?).
         e:Handled = TRUE.         
      END.
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrEf_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:EfBfEnter = "Ef".
		RETURN.

	END METHOD.

	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrEt_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:EtBtEnter = "Et".
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrEt_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
		keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:buttonBt_Click(?,?).
         e:Handled = TRUE.         
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrEf_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
     
      keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:buttonBf_Click(?,?).
         e:Handled = TRUE.         
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUf_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:SokEnrUtb().
		RETURN.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUf_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
     /* THIS-OBJECT:SokEnrUf_KeyDown(?,?).*/
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUf_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
      keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:SokEnrUtb().
         e:Handled = TRUE.         
      END.
      RETURN.

	END METHOD.

	
	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUt_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:SokEnrUtb().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUt_Enter( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SokEnrUt_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.
      keycodestr = e:KEYCODE:ToString().
      IF keycodestr  = "RETURN" THEN DO:
         THIS-OBJECT:SokEnrUtb().
         e:Handled = TRUE.         
      END.
      RETURN.
		
	END METHOD.

   METHOD PUBLIC VOID StartUtBytLisAdm():
      
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AddButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   /*FRÅN PIL UPP*/
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      IF THIS-OBJECT:GridEnrFran:ActiveRow =  ? OR THIS-OBJECT:GridEnrTill:ActiveRow =  ? THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
         RETURN.
      END.
      
      THIS-OBJECT:GridEnrFran:SyncMan().
      THIS-OBJECT:GridEnrTill:SyncMan().
      rid = THIS-OBJECT:ControlDataDb:NyMtrl(INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")))).
      THIS-OBJECT:GridUtb:GuruReopen().
      THIS-OBJECT:GridUtb:GuruRepositionto(rid).
      THIS-OBJECT:GridUtb:GuruReOpen().
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonBf_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:ControlDataDb:SokFranMtrl(INPUT SokEnrEf:TEXT,INPUT SokBenBf:TEXT,INPUT FALSE,INPUT STRING(THIS-OBJECT:comboFranLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonBt_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlDataDb:SokTillMtrl(INPUT SokEnrEt:TEXT,INPUT SokBenBt:TEXT,INPUT FALSE,INPUT STRING(THIS-OBJECT:comboTillLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonMf_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:ControlDataDb:SokFranMtrl(INPUT "*" + SokEnrEf:TEXT + "*" ,INPUT "*" + SokBenBf:TEXT + "*",INPUT FALSE,INPUT STRING(THIS-OBJECT:comboFranLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonMt_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlDataDb:SokTillMtrl(INPUT "*" + SokEnrEt:TEXT + "*",INPUT "*" + SokBenBt:TEXT + "*",INPUT FALSE,INPUT STRING(THIS-OBJECT:comboFranLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonSf_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlDataDb:SokFranMtrl(INPUT "*" + SokEnrEf:TEXT,INPUT "*" + SokBenBf:TEXT,INPUT FALSE,INPUT STRING(THIS-OBJECT:comboFranLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonSt_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlDataDb:SokTillMtrl(INPUT "*" + SokEnrEt:TEXT,INPUT "*" + SokBenBt:TEXT,INPUT FALSE,INPUT STRING(THIS-OBJECT:comboFranLev:SelectedRow:GetCellVALUE("LEVKOD"))).
		RETURN.

	END METHOD.

	
	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Beredning.UtbytListaAdm").
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
      THIS-OBJECT:splitListaEnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitValUtBytlista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitListor = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerListaFel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUtb = NEW Controls.Grid().
      THIS-OBJECT:GridFelImp = NEW Controls.Grid().
      THIS-OBJECT:splitNyLista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSokEnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EnrLabelUt = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokEnrUt = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:EnrLabelUf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokEnrUf = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:comboUtbTill = NEW Controls.Combo().
      THIS-OBJECT:ViTillLev = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:ViFranLev = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:comboUtbFran = NEW Controls.Combo().
      THIS-OBJECT:splitPopUp = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripListaNy = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtoNy = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripListRubrik = NEW System.Windows.Forms.ToolStripLabel().
      THIS-OBJECT:comboLista = NEW Controls.Combo().
      THIS-OBJECT:nyLiPanel = NEW Controls.PopPanel().
      THIS-OBJECT:ErsLabel = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:fillinListName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:buttonCancelNyLi = NEW Controls.Button().
      THIS-OBJECT:buttonNyLi1 = NEW Controls.Button().
      THIS-OBJECT:splitEnrVal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyBort = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:AddRemoveStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:RemoveButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:AddButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonExcel = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonAter = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonNy = NEW Controls.ButtonOver().
      THIS-OBJECT:splitUrvalEnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitSokEnrHuvL = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridEnrFran = NEW Controls.Grid().
      THIS-OBJECT:BenLabelBf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokBenBf = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:EnrLabelEf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokEnrEf = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:buttonSf = NEW Controls.Button().
      THIS-OBJECT:buttonMf = NEW Controls.Button().
      THIS-OBJECT:buttonBf = NEW Controls.Button().
      THIS-OBJECT:comboFranLev = NEW Controls.Combo().
      THIS-OBJECT:splitSokEnrLev = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridEnrTill = NEW Controls.Grid().
      THIS-OBJECT:BenLabelBt = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokBenBt = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:EnrLabelEt = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:SokEnrEt = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:buttonSt = NEW Controls.Button().
      THIS-OBJECT:buttonMt = NEW Controls.Button().
      THIS-OBJECT:buttonBt = NEW Controls.Button().
      THIS-OBJECT:comboTillLev = NEW Controls.Combo().
      THIS-OBJECT:nyLiPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      CAST(THIS-OBJECT:splitListaEnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitListaEnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitListaEnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitListaEnr:SuspendLayout().
      CAST(THIS-OBJECT:splitValUtBytlista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitValUtBytlista:Panel2:SuspendLayout().
      THIS-OBJECT:splitValUtBytlista:SuspendLayout().
      CAST(THIS-OBJECT:splitListor, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitListor:Panel1:SuspendLayout().
      THIS-OBJECT:splitListor:Panel2:SuspendLayout().
      THIS-OBJECT:splitListor:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerListaFel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerListaFel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerListaFel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerListaFel:SuspendLayout().
      CAST(THIS-OBJECT:GridUtb, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridFelImp, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitNyLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitNyLista:Panel1:SuspendLayout().
      THIS-OBJECT:splitNyLista:Panel2:SuspendLayout().
      THIS-OBJECT:splitNyLista:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSokEnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSokEnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSokEnr:SuspendLayout().
      CAST(THIS-OBJECT:SokEnrUt, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:SokEnrUf, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboUtbTill, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboUtbFran, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitPopUp, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitPopUp:Panel1:SuspendLayout().
      THIS-OBJECT:splitPopUp:Panel2:SuspendLayout().
      THIS-OBJECT:splitPopUp:SuspendLayout().
      THIS-OBJECT:toolStripListaNy:SuspendLayout().
      CAST(THIS-OBJECT:comboLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:nyLiPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyLiPanel:SuspendLayout().
      CAST(THIS-OBJECT:fillinListName, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitEnrVal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitEnrVal:Panel1:SuspendLayout().
      THIS-OBJECT:splitEnrVal:Panel2:SuspendLayout().
      THIS-OBJECT:splitEnrVal:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyBort:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:SuspendLayout().
      THIS-OBJECT:AddRemoveStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitUrvalEnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitUrvalEnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitUrvalEnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitUrvalEnr:SuspendLayout().
      CAST(THIS-OBJECT:splitSokEnrHuvL, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitSokEnrHuvL:Panel1:SuspendLayout().
      THIS-OBJECT:splitSokEnrHuvL:Panel2:SuspendLayout().
      THIS-OBJECT:splitSokEnrHuvL:SuspendLayout().
      CAST(THIS-OBJECT:GridEnrFran, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:SokBenBf, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:SokEnrEf, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboFranLev, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitSokEnrLev, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitSokEnrLev:Panel1:SuspendLayout().
      THIS-OBJECT:splitSokEnrLev:Panel2:SuspendLayout().
      THIS-OBJECT:splitSokEnrLev:SuspendLayout().
      CAST(THIS-OBJECT:GridEnrTill, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:SokBenBt, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:SokEnrEt, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboTillLev, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitListaEnr */
      /*  */
      THIS-OBJECT:splitListaEnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitListaEnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitListaEnr:Name = "splitListaEnr".
      THIS-OBJECT:splitListaEnr:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitListaEnr.Panel1 */
      /*  */
      THIS-OBJECT:splitListaEnr:Panel1:Controls:Add(THIS-OBJECT:splitValUtBytlista).
      /*  */
      /* splitListaEnr.Panel2 */
      /*  */
      THIS-OBJECT:splitListaEnr:Panel2:Controls:Add(THIS-OBJECT:splitEnrVal).
      THIS-OBJECT:splitListaEnr:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitListaEnr:SplitterDistance = 300.
      THIS-OBJECT:splitListaEnr:TabIndex = 0.
      /*  */
      /* splitValUtBytlista */
      /*  */
      THIS-OBJECT:splitValUtBytlista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitValUtBytlista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitValUtBytlista:Name = "splitValUtBytlista".
      THIS-OBJECT:splitValUtBytlista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitValUtBytlista.Panel2 */
      /*  */
      THIS-OBJECT:splitValUtBytlista:Panel2:Controls:Add(THIS-OBJECT:splitListor).
      THIS-OBJECT:splitValUtBytlista:Size = NEW System.Drawing.Size(1184, 300).
      THIS-OBJECT:splitValUtBytlista:SplitterDistance = 25.
      THIS-OBJECT:splitValUtBytlista:TabIndex = 0.
      /*  */
      /* splitListor */
      /*  */
      THIS-OBJECT:splitListor:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitListor:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitListor:Name = "splitListor".
      /*  */
      /* splitListor.Panel1 */
      /*  */
      THIS-OBJECT:splitListor:Panel1:Controls:Add(THIS-OBJECT:splitContainerListaFel).
      /*  */
      /* splitListor.Panel2 */
      /*  */
      THIS-OBJECT:splitListor:Panel2:Controls:Add(THIS-OBJECT:splitNyLista).
      THIS-OBJECT:splitListor:Size = NEW System.Drawing.Size(1184, 271).
      THIS-OBJECT:splitListor:SplitterDistance = 600.
      THIS-OBJECT:splitListor:TabIndex = 1.
      /*  */
      /* splitContainerListaFel */
      /*  */
      THIS-OBJECT:splitContainerListaFel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerListaFel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerListaFel:Name = "splitContainerListaFel".
      /*  */
      /* splitContainerListaFel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerListaFel:Panel1:Controls:Add(THIS-OBJECT:GridUtb).
      /*  */
      /* splitContainerListaFel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerListaFel:Panel2:Controls:Add(THIS-OBJECT:GridFelImp).
      THIS-OBJECT:splitContainerListaFel:Size = NEW System.Drawing.Size(600, 271).
      THIS-OBJECT:splitContainerListaFel:SplitterDistance = 300.
      THIS-OBJECT:splitContainerListaFel:TabIndex = 1.
      /*  */
      /* GridUtb */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUtb:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridUtb:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUtb:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUtb:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUtb:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridUtb:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUtb:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridUtb:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUtb:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUtb:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUtb:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridUtb:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUtb:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUtb:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUtb:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUtb:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUtb:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUtb:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUtb:GuruContext = ?.
      THIS-OBJECT:GridUtb:GuruDraggable = TRUE.
      THIS-OBJECT:GridUtb:GuruDroppable = TRUE.
      THIS-OBJECT:GridUtb:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUtb:Name = "GridUtb".
      THIS-OBJECT:GridUtb:Size = NEW System.Drawing.Size(300, 271).
      THIS-OBJECT:GridUtb:TabIndex = 16.
      THIS-OBJECT:GridUtb:Text = "Artiklar".
      THIS-OBJECT:GridUtb:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUtb_GuruAfterDropped).
      /*  */
      /* GridFelImp */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFelImp:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridFelImp:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFelImp:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelImp:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelImp:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridFelImp:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelImp:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridFelImp:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFelImp:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFelImp:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridFelImp:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFelImp:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFelImp:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFelImp:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFelImp:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFelImp:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFelImp:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFelImp:GuruContext = ?.
      THIS-OBJECT:GridFelImp:GuruDroppable = TRUE.
      THIS-OBJECT:GridFelImp:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFelImp:Name = "GridFelImp".
      THIS-OBJECT:GridFelImp:Size = NEW System.Drawing.Size(296, 271).
      THIS-OBJECT:GridFelImp:TabIndex = 17.
      THIS-OBJECT:GridFelImp:Text = "Från".
      /*  */
      /* splitNyLista */
      /*  */
      THIS-OBJECT:splitNyLista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitNyLista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitNyLista:Name = "splitNyLista".
      THIS-OBJECT:splitNyLista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitNyLista.Panel1 */
      /*  */
      THIS-OBJECT:splitNyLista:Panel1:Controls:Add(THIS-OBJECT:splitContainerSokEnr).
      /*  */
      /* splitNyLista.Panel2 */
      /*  */
      THIS-OBJECT:splitNyLista:Panel2:Controls:Add(THIS-OBJECT:splitPopUp).
      THIS-OBJECT:splitNyLista:Size = NEW System.Drawing.Size(580, 271).
      THIS-OBJECT:splitNyLista:SplitterDistance = 200.
      THIS-OBJECT:splitNyLista:TabIndex = 6.
      /*  */
      /* splitContainerSokEnr */
      /*  */
      THIS-OBJECT:splitContainerSokEnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSokEnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSokEnr:Name = "splitContainerSokEnr".
      THIS-OBJECT:splitContainerSokEnr:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSokEnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:EnrLabelUt).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:SokEnrUt).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:EnrLabelUf).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:SokEnrUf).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:comboUtbTill).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:ViTillLev).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:ViFranLev).
      THIS-OBJECT:splitContainerSokEnr:Panel1:Controls:Add(THIS-OBJECT:comboUtbFran).
      THIS-OBJECT:splitContainerSokEnr:Size = NEW System.Drawing.Size(580, 200).
      THIS-OBJECT:splitContainerSokEnr:SplitterDistance = 100.
      THIS-OBJECT:splitContainerSokEnr:TabIndex = 15.
      /*  */
      /* EnrLabelUt */
      /*  */
      THIS-OBJECT:EnrLabelUt:Location = NEW System.Drawing.Point(353, 61).
      THIS-OBJECT:EnrLabelUt:Name = "EnrLabelUt".
      THIS-OBJECT:EnrLabelUt:Size = NEW System.Drawing.Size(98, 23).
      THIS-OBJECT:EnrLabelUt:TabIndex = 19.
      THIS-OBJECT:EnrLabelUt:Text = "ENR".
      /*  */
      /* SokEnrUt */
      /*  */
      THIS-OBJECT:SokEnrUt:Location = NEW System.Drawing.Point(457, 61).
      THIS-OBJECT:SokEnrUt:Name = "SokEnrUt".
      THIS-OBJECT:SokEnrUt:Size = NEW System.Drawing.Size(96, 21).
      THIS-OBJECT:SokEnrUt:TabIndex = 18.
      THIS-OBJECT:SokEnrUt:DoubleClick:Subscribe(THIS-OBJECT:SokEnrUt_DoubleClick).
      THIS-OBJECT:SokEnrUt:Enter:Subscribe(THIS-OBJECT:SokEnrUt_Enter).
      THIS-OBJECT:SokEnrUt:KeyDown:Subscribe(THIS-OBJECT:SokEnrUt_KeyDown).
      /*  */
      /* EnrLabelUf */
      /*  */
      THIS-OBJECT:EnrLabelUf:Location = NEW System.Drawing.Point(40, 63).
      THIS-OBJECT:EnrLabelUf:Name = "EnrLabelUf".
      THIS-OBJECT:EnrLabelUf:Size = NEW System.Drawing.Size(98, 23).
      THIS-OBJECT:EnrLabelUf:TabIndex = 17.
      THIS-OBJECT:EnrLabelUf:Text = "ENR".
      /*  */
      /* SokEnrUf */
      /*  */
      THIS-OBJECT:SokEnrUf:Location = NEW System.Drawing.Point(144, 63).
      THIS-OBJECT:SokEnrUf:Name = "SokEnrUf".
      THIS-OBJECT:SokEnrUf:Size = NEW System.Drawing.Size(96, 21).
      THIS-OBJECT:SokEnrUf:TabIndex = 16.
      THIS-OBJECT:SokEnrUf:DoubleClick:Subscribe(THIS-OBJECT:SokEnrUf_DoubleClick).
      THIS-OBJECT:SokEnrUf:Enter:Subscribe(THIS-OBJECT:SokEnrUf_Enter).
      THIS-OBJECT:SokEnrUf:KeyDown:Subscribe(THIS-OBJECT:SokEnrUf_KeyDown).
      /*  */
      /* comboUtbTill */
      /*  */
      THIS-OBJECT:comboUtbTill:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboUtbTill:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboUtbTill:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboUtbTill:Location = NEW System.Drawing.Point(353, 28).
      THIS-OBJECT:comboUtbTill:Name = "comboUtbTill".
      THIS-OBJECT:comboUtbTill:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboUtbTill:TabIndex = 15.
      /*  */
      /* ViTillLev */
      /*  */
      THIS-OBJECT:ViTillLev:Location = NEW System.Drawing.Point(353, 3).
      THIS-OBJECT:ViTillLev:Name = "ViTillLev".
      THIS-OBJECT:ViTillLev:Size = NEW System.Drawing.Size(100, 19).
      THIS-OBJECT:ViTillLev:TabIndex = 14.
      THIS-OBJECT:ViTillLev:Text = "Visa till leverantör".
      /*  */
      /* ViFranLev */
      /*  */
      THIS-OBJECT:ViFranLev:Location = NEW System.Drawing.Point(40, 0).
      THIS-OBJECT:ViFranLev:Name = "ViFranLev".
      THIS-OBJECT:ViFranLev:Size = NEW System.Drawing.Size(159, 22).
      THIS-OBJECT:ViFranLev:TabIndex = 13.
      THIS-OBJECT:ViFranLev:Text = "Visa från leverantör".
      /*  */
      /* comboUtbFran */
      /*  */
      THIS-OBJECT:comboUtbFran:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboUtbFran:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboUtbFran:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboUtbFran:Location = NEW System.Drawing.Point(40, 28).
      THIS-OBJECT:comboUtbFran:Name = "comboUtbFran".
      THIS-OBJECT:comboUtbFran:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboUtbFran:TabIndex = 7.
      /*  */
      /* splitPopUp */
      /*  */
      THIS-OBJECT:splitPopUp:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitPopUp:Name = "splitPopUp".
      /*  */
      /* splitPopUp.Panel1 */
      /*  */
      THIS-OBJECT:splitPopUp:Panel1:Controls:Add(THIS-OBJECT:toolStripListaNy).
      THIS-OBJECT:splitPopUp:Panel1:Controls:Add(THIS-OBJECT:comboLista).
      /*  */
      /* splitPopUp.Panel2 */
      /*  */
      THIS-OBJECT:splitPopUp:Panel2:Controls:Add(THIS-OBJECT:nyLiPanel).
      THIS-OBJECT:splitPopUp:Size = NEW System.Drawing.Size(599, 165).
      THIS-OBJECT:splitPopUp:SplitterDistance = 263.
      THIS-OBJECT:splitPopUp:TabIndex = 6.
      /*  */
      /* toolStripListaNy */
      /*  */
      THIS-OBJECT:toolStripListaNy:Dock = System.Windows.Forms.DockStyle:None.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripButtoNy.
      arrayvar0[2] = THIS-OBJECT:toolStripButtonBort.
      arrayvar0[3] = THIS-OBJECT:toolStripListRubrik.
      THIS-OBJECT:toolStripListaNy:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripListaNy:Location = NEW System.Drawing.Point(9, 13).
      THIS-OBJECT:toolStripListaNy:Name = "toolStripListaNy".
      THIS-OBJECT:toolStripListaNy:Size = NEW System.Drawing.Size(134, 25).
      THIS-OBJECT:toolStripListaNy:TabIndex = 5.
      THIS-OBJECT:toolStripListaNy:Text = "toolStrip1".
      /*  */
      /* toolStripButtoNy */
      /*  */
      THIS-OBJECT:toolStripButtoNy:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
      THIS-OBJECT:toolStripButtoNy:Image = CAST(resources:GetObject("toolStripButtoNy.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtoNy:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtoNy:Name = "toolStripButtoNy".
      THIS-OBJECT:toolStripButtoNy:Size = NEW System.Drawing.Size(23, 22).
      THIS-OBJECT:toolStripButtoNy:Text = "Lägg till en lista".
      THIS-OBJECT:toolStripButtoNy:Click:Subscribe(THIS-OBJECT:toolStripButtoNy_Click).
      /*  */
      /* toolStripButtonBort */
      /*  */
      THIS-OBJECT:toolStripButtonBort:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
      THIS-OBJECT:toolStripButtonBort:Image = CAST(resources:GetObject("toolStripButtonBort.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonBort:Name = "toolStripButtonBort".
      THIS-OBJECT:toolStripButtonBort:Size = NEW System.Drawing.Size(23, 22).
      THIS-OBJECT:toolStripButtonBort:Text = "Ta bort listan".
      THIS-OBJECT:toolStripButtonBort:Click:Subscribe(THIS-OBJECT:toolStripButtonBort_Click).
      /*  */
      /* toolStripListRubrik */
      /*  */
      THIS-OBJECT:toolStripListRubrik:Name = "toolStripListRubrik".
      THIS-OBJECT:toolStripListRubrik:Size = NEW System.Drawing.Size(76, 22).
      THIS-OBJECT:toolStripListRubrik:Text = "Ändra Rubrik".
      THIS-OBJECT:toolStripListRubrik:Click:Subscribe(THIS-OBJECT:toolStripListRubrik_Click).
      /*  */
      /* comboLista */
      /*  */
      THIS-OBJECT:comboLista:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboLista:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboLista:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboLista:Location = NEW System.Drawing.Point(40, 52).
      THIS-OBJECT:comboLista:Name = "comboLista".
      THIS-OBJECT:comboLista:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboLista:TabIndex = 4.
      /*  */
      /* nyLiPanel */
      /*  */
      THIS-OBJECT:nyLiPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyLiPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyLiPanel:ClientArea:Controls:Add(THIS-OBJECT:ErsLabel).
      THIS-OBJECT:nyLiPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinListName).
      THIS-OBJECT:nyLiPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyLi).
      THIS-OBJECT:nyLiPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyLi1).
      THIS-OBJECT:nyLiPanel:Location = NEW System.Drawing.Point(1, 73).
      THIS-OBJECT:nyLiPanel:Name = "nyLiPanel".
      THIS-OBJECT:nyLiPanel:Size = NEW System.Drawing.Size(285, 94).
      THIS-OBJECT:nyLiPanel:TabIndex = 8.
      THIS-OBJECT:nyLiPanel:Visible = FALSE.
      /*  */
      /* ErsLabel */
      /*  */
      THIS-OBJECT:ErsLabel:Location = NEW System.Drawing.Point(1, 19).
      THIS-OBJECT:ErsLabel:Name = "ErsLabel".
      THIS-OBJECT:ErsLabel:Size = NEW System.Drawing.Size(100, 23).
      THIS-OBJECT:ErsLabel:TabIndex = 13.
      THIS-OBJECT:ErsLabel:Text = "Ersättningslista".
      /*  */
      /* fillinListName */
      /*  */
      THIS-OBJECT:fillinListName:Location = NEW System.Drawing.Point(107, 19).
      THIS-OBJECT:fillinListName:Name = "fillinListName".
      THIS-OBJECT:fillinListName:Size = NEW System.Drawing.Size(154, 21).
      THIS-OBJECT:fillinListName:TabIndex = 12.
      THIS-OBJECT:fillinListName:KeyDown:Subscribe(THIS-OBJECT:fillinListName_KeyDown).
      /*  */
      /* buttonCancelNyLi */
      /*  */
      THIS-OBJECT:buttonCancelNyLi:Location = NEW System.Drawing.Point(186, 57).
      THIS-OBJECT:buttonCancelNyLi:Name = "buttonCancelNyLi".
      THIS-OBJECT:buttonCancelNyLi:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyLi:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyLi:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyLi:Click:Subscribe(THIS-OBJECT:buttonCancelNyLi_Click).
      /*  */
      /* buttonNyLi1 */
      /*  */
      THIS-OBJECT:buttonNyLi1:Location = NEW System.Drawing.Point(109, 57).
      THIS-OBJECT:buttonNyLi1:Name = "buttonNyLi1".
      THIS-OBJECT:buttonNyLi1:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyLi1:TabIndex = 3.
      THIS-OBJECT:buttonNyLi1:Text = "Spara".
      THIS-OBJECT:buttonNyLi1:Click:Subscribe(THIS-OBJECT:buttonNyLi1_Click).
      /*  */
      /* splitEnrVal */
      /*  */
      THIS-OBJECT:splitEnrVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitEnrVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitEnrVal:Name = "splitEnrVal".
      THIS-OBJECT:splitEnrVal:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitEnrVal.Panel1 */
      /*  */
      THIS-OBJECT:splitEnrVal:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyBort).
      THIS-OBJECT:splitEnrVal:Panel1MinSize = 10.
      /*  */
      /* splitEnrVal.Panel2 */
      /*  */
      THIS-OBJECT:splitEnrVal:Panel2:Controls:Add(THIS-OBJECT:splitUrvalEnr).
      THIS-OBJECT:splitEnrVal:Size = NEW System.Drawing.Size(1184, 258).
      THIS-OBJECT:splitEnrVal:SplitterDistance = 51.
      THIS-OBJECT:splitEnrVal:TabIndex = 0.
      /*  */
      /* splitContainerNyBort */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyBort:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyBort:Name = "splitContainerNyBort".
      /*  */
      /* splitContainerNyBort.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel1:Controls:Add(THIS-OBJECT:AddRemoveStrip).
      /*  */
      /* splitContainerNyBort.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel2:Controls:Add(THIS-OBJECT:ButtonNy).
      THIS-OBJECT:splitContainerNyBort:Size = NEW System.Drawing.Size(1184, 51).
      THIS-OBJECT:splitContainerNyBort:SplitterDistance = 649.
      THIS-OBJECT:splitContainerNyBort:TabIndex = 6.
      /*  */
      /* AddRemoveStrip */
      /*  */
      THIS-OBJECT:AddRemoveStrip:Dock = System.Windows.Forms.DockStyle:None.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:RemoveButton.
      arrayvar1[2] = THIS-OBJECT:AddButton.
      arrayvar1[3] = THIS-OBJECT:toolStripButtonExcel.
      arrayvar1[4] = THIS-OBJECT:toolStripButtonAter.
      THIS-OBJECT:AddRemoveStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:AddRemoveStrip:Location = NEW System.Drawing.Point(135, 13).
      THIS-OBJECT:AddRemoveStrip:Name = "AddRemoveStrip".
      THIS-OBJECT:AddRemoveStrip:Size = NEW System.Drawing.Size(346, 25).
      THIS-OBJECT:AddRemoveStrip:TabIndex = 5.
      THIS-OBJECT:AddRemoveStrip:Text = "toolStrip1".
      /*  */
      /* RemoveButton */
      /*  */
      THIS-OBJECT:RemoveButton:Image = CAST(resources:GetObject("RemoveButton.Image"), System.Drawing.Image).
      THIS-OBJECT:RemoveButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:RemoveButton:Name = "RemoveButton".
      THIS-OBJECT:RemoveButton:Size = NEW System.Drawing.Size(133, 22).
      THIS-OBJECT:RemoveButton:Text = "Ta bort valda poster!".
      THIS-OBJECT:RemoveButton:Click:Subscribe(THIS-OBJECT:RemoveButton_Click).
      /*  */
      /* AddButton */
      /*  */
      THIS-OBJECT:AddButton:Image = CAST(resources:GetObject("AddButton.Image"), System.Drawing.Image).
      THIS-OBJECT:AddButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AddButton:Name = "AddButton".
      THIS-OBJECT:AddButton:Size = NEW System.Drawing.Size(108, 22).
      THIS-OBJECT:AddButton:Text = "Lägg till poster!".
      THIS-OBJECT:AddButton:Click:Subscribe(THIS-OBJECT:AddButton_Click).
      /*  */
      /* toolStripButtonExcel */
      /*  */
      THIS-OBJECT:toolStripButtonExcel:Image = CAST(resources:GetObject("toolStripButtonExcel.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonExcel:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonExcel:Name = "toolStripButtonExcel".
      THIS-OBJECT:toolStripButtonExcel:Size = NEW System.Drawing.Size(93, 22).
      THIS-OBJECT:toolStripButtonExcel:Text = "Import Excel".
      THIS-OBJECT:toolStripButtonExcel:ToolTipText = "Från lev i första kolumnen, Från Enr i andra, Till lev i tredje, Till Enr i fjärde".
      THIS-OBJECT:toolStripButtonExcel:Click:Subscribe(THIS-OBJECT:toolStripButtonExcel_Click).
      /*  */
      /* toolStripButtonAter */
      /*  */
      THIS-OBJECT:toolStripButtonAter:Image = CAST(resources:GetObject("toolStripButtonAter.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonAter:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonAter:Name = "toolStripButtonAter".
      THIS-OBJECT:toolStripButtonAter:Size = NEW System.Drawing.Size(62, 22).
      THIS-OBJECT:toolStripButtonAter:Text = "Återgå".
      THIS-OBJECT:toolStripButtonAter:Visible = FALSE.
      THIS-OBJECT:toolStripButtonAter:Click:Subscribe(THIS-OBJECT:toolStripAter_Click).
      /*  */
      /* ButtonNy */
      /*  */
      THIS-OBJECT:ButtonNy:GuruTooltip = "Lägg till post".
      THIS-OBJECT:ButtonNy:Location = NEW System.Drawing.Point(274, 13).
      THIS-OBJECT:ButtonNy:Name = "ButtonNy".
      THIS-OBJECT:ButtonNy:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonNy:TabIndex = 5.
      THIS-OBJECT:ButtonNy:Text = " ".
      THIS-OBJECT:ButtonNy:Click:Subscribe(THIS-OBJECT:ButtonNy_Click).
      /*  */
      /* splitUrvalEnr */
      /*  */
      THIS-OBJECT:splitUrvalEnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitUrvalEnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitUrvalEnr:Name = "splitUrvalEnr".
      /*  */
      /* splitUrvalEnr.Panel1 */
      /*  */
      THIS-OBJECT:splitUrvalEnr:Panel1:Controls:Add(THIS-OBJECT:splitSokEnrHuvL).
      /*  */
      /* splitUrvalEnr.Panel2 */
      /*  */
      THIS-OBJECT:splitUrvalEnr:Panel2:Controls:Add(THIS-OBJECT:splitSokEnrLev).
      THIS-OBJECT:splitUrvalEnr:Size = NEW System.Drawing.Size(1184, 203).
      THIS-OBJECT:splitUrvalEnr:SplitterDistance = 592.
      THIS-OBJECT:splitUrvalEnr:TabIndex = 0.
      /*  */
      /* splitSokEnrHuvL */
      /*  */
      THIS-OBJECT:splitSokEnrHuvL:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitSokEnrHuvL:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitSokEnrHuvL:Name = "splitSokEnrHuvL".
      THIS-OBJECT:splitSokEnrHuvL:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitSokEnrHuvL.Panel1 */
      /*  */
      THIS-OBJECT:splitSokEnrHuvL:Panel1:Controls:Add(THIS-OBJECT:GridEnrFran).
      /*  */
      /* splitSokEnrHuvL.Panel2 */
      /*  */
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:BenLabelBf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:SokBenBf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:EnrLabelEf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:SokEnrEf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:buttonSf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:buttonMf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:buttonBf).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:Controls:Add(THIS-OBJECT:comboFranLev).
      THIS-OBJECT:splitSokEnrHuvL:Size = NEW System.Drawing.Size(592, 203).
      THIS-OBJECT:splitSokEnrHuvL:SplitterDistance = 99.
      THIS-OBJECT:splitSokEnrHuvL:TabIndex = 0.
      /*  */
      /* GridEnrFran */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridEnrFran:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEnrFran:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridEnrFran:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEnrFran:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEnrFran:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEnrFran:GuruContext = ?.
      THIS-OBJECT:GridEnrFran:GuruDroppable = TRUE.
      THIS-OBJECT:GridEnrFran:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEnrFran:Name = "GridEnrFran".
      THIS-OBJECT:GridEnrFran:Size = NEW System.Drawing.Size(592, 99).
      THIS-OBJECT:GridEnrFran:TabIndex = 15.
      THIS-OBJECT:GridEnrFran:Text = "Från".
      /*  */
      /* BenLabelBf */
      /*  */
      THIS-OBJECT:BenLabelBf:Location = NEW System.Drawing.Point(12, 65).
      THIS-OBJECT:BenLabelBf:Name = "BenLabelBf".
      THIS-OBJECT:BenLabelBf:Size = NEW System.Drawing.Size(100, 23).
      THIS-OBJECT:BenLabelBf:TabIndex = 14.
      THIS-OBJECT:BenLabelBf:Text = "Benämning".
      /*  */
      /* SokBenBf */
      /*  */
      THIS-OBJECT:SokBenBf:Location = NEW System.Drawing.Point(135, 65).
      THIS-OBJECT:SokBenBf:Name = "SokBenBf".
      THIS-OBJECT:SokBenBf:Size = NEW System.Drawing.Size(154, 21).
      THIS-OBJECT:SokBenBf:TabIndex = 13.
      THIS-OBJECT:SokBenBf:Enter:Subscribe(THIS-OBJECT:SokBenBf_Enter).
      THIS-OBJECT:SokBenBf:KeyDown:Subscribe(THIS-OBJECT:SokBenBf_KeyDown).
      /*  */
      /* EnrLabelEf */
      /*  */
      THIS-OBJECT:EnrLabelEf:Location = NEW System.Drawing.Point(12, 31).
      THIS-OBJECT:EnrLabelEf:Name = "EnrLabelEf".
      THIS-OBJECT:EnrLabelEf:Size = NEW System.Drawing.Size(100, 23).
      THIS-OBJECT:EnrLabelEf:TabIndex = 12.
      THIS-OBJECT:EnrLabelEf:Text = "ENR".
      /*  */
      /* SokEnrEf */
      /*  */
      THIS-OBJECT:SokEnrEf:Location = NEW System.Drawing.Point(135, 31).
      THIS-OBJECT:SokEnrEf:Name = "SokEnrEf".
      THIS-OBJECT:SokEnrEf:Size = NEW System.Drawing.Size(154, 21).
      THIS-OBJECT:SokEnrEf:TabIndex = 11.
      THIS-OBJECT:SokEnrEf:Enter:Subscribe(THIS-OBJECT:SokEnrEf_Enter).
      THIS-OBJECT:SokEnrEf:KeyDown:Subscribe(THIS-OBJECT:SokEnrEf_KeyDown).
      /*  */
      /* buttonSf */
      /*  */
      THIS-OBJECT:buttonSf:Location = NEW System.Drawing.Point(314, 65).
      THIS-OBJECT:buttonSf:Name = "buttonSf".
      THIS-OBJECT:buttonSf:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonSf:TabIndex = 10.
      THIS-OBJECT:buttonSf:Text = "Slutet".
      THIS-OBJECT:buttonSf:Click:Subscribe(THIS-OBJECT:buttonSf_Click).
      /*  */
      /* buttonMf */
      /*  */
      THIS-OBJECT:buttonMf:Location = NEW System.Drawing.Point(314, 43).
      THIS-OBJECT:buttonMf:Name = "buttonMf".
      THIS-OBJECT:buttonMf:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonMf:TabIndex = 9.
      THIS-OBJECT:buttonMf:Text = "Någonstans".
      THIS-OBJECT:buttonMf:Click:Subscribe(THIS-OBJECT:buttonMf_Click).
      /*  */
      /* buttonBf */
      /*  */
      THIS-OBJECT:buttonBf:Location = NEW System.Drawing.Point(314, 21).
      THIS-OBJECT:buttonBf:Name = "buttonBf".
      THIS-OBJECT:buttonBf:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonBf:TabIndex = 8.
      THIS-OBJECT:buttonBf:Text = "Början".
      THIS-OBJECT:buttonBf:Click:Subscribe(THIS-OBJECT:buttonBf_Click).
      /*  */
      /* comboFranLev */
      /*  */
      THIS-OBJECT:comboFranLev:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboFranLev:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboFranLev:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboFranLev:Location = NEW System.Drawing.Point(12, 3).
      THIS-OBJECT:comboFranLev:Name = "comboFranLev".
      THIS-OBJECT:comboFranLev:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboFranLev:TabIndex = 5.
      /*  */
      /* splitSokEnrLev */
      /*  */
      THIS-OBJECT:splitSokEnrLev:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitSokEnrLev:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitSokEnrLev:Name = "splitSokEnrLev".
      THIS-OBJECT:splitSokEnrLev:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitSokEnrLev.Panel1 */
      /*  */
      THIS-OBJECT:splitSokEnrLev:Panel1:Controls:Add(THIS-OBJECT:GridEnrTill).
      /*  */
      /* splitSokEnrLev.Panel2 */
      /*  */
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:BenLabelBt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:SokBenBt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:EnrLabelEt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:SokEnrEt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:buttonSt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:buttonMt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:buttonBt).
      THIS-OBJECT:splitSokEnrLev:Panel2:Controls:Add(THIS-OBJECT:comboTillLev).
      THIS-OBJECT:splitSokEnrLev:Size = NEW System.Drawing.Size(588, 203).
      THIS-OBJECT:splitSokEnrLev:SplitterDistance = 99.
      THIS-OBJECT:splitSokEnrLev:TabIndex = 0.
      /*  */
      /* GridEnrTill */
      /*  */
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridEnrTill:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEnrTill:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrTill:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrTill:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridEnrTill:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrTill:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridEnrTill:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEnrTill:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEnrTill:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
      THIS-OBJECT:GridEnrTill:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEnrTill:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEnrTill:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEnrTill:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEnrTill:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEnrTill:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEnrTill:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEnrTill:GuruContext = ?.
      THIS-OBJECT:GridEnrTill:GuruDroppable = TRUE.
      THIS-OBJECT:GridEnrTill:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEnrTill:Name = "GridEnrTill".
      THIS-OBJECT:GridEnrTill:Size = NEW System.Drawing.Size(588, 99).
      THIS-OBJECT:GridEnrTill:TabIndex = 16.
      THIS-OBJECT:GridEnrTill:Text = "Från".
      /*  */
      /* BenLabelBt */
      /*  */
      THIS-OBJECT:BenLabelBt:Location = NEW System.Drawing.Point(12, 64).
      THIS-OBJECT:BenLabelBt:Name = "BenLabelBt".
      THIS-OBJECT:BenLabelBt:Size = NEW System.Drawing.Size(100, 23).
      THIS-OBJECT:BenLabelBt:TabIndex = 20.
      THIS-OBJECT:BenLabelBt:Text = "Benämning".
      /*  */
      /* SokBenBt */
      /*  */
      THIS-OBJECT:SokBenBt:Location = NEW System.Drawing.Point(135, 64).
      THIS-OBJECT:SokBenBt:Name = "SokBenBt".
      THIS-OBJECT:SokBenBt:Size = NEW System.Drawing.Size(154, 21).
      THIS-OBJECT:SokBenBt:TabIndex = 19.
      THIS-OBJECT:SokBenBt:Enter:Subscribe(THIS-OBJECT:SokBenBt_Enter).
      THIS-OBJECT:SokBenBt:KeyDown:Subscribe(THIS-OBJECT:SokBenBt_KeyDown).
      /*  */
      /* EnrLabelEt */
      /*  */
      THIS-OBJECT:EnrLabelEt:Location = NEW System.Drawing.Point(12, 35).
      THIS-OBJECT:EnrLabelEt:Name = "EnrLabelEt".
      THIS-OBJECT:EnrLabelEt:Size = NEW System.Drawing.Size(100, 23).
      THIS-OBJECT:EnrLabelEt:TabIndex = 18.
      THIS-OBJECT:EnrLabelEt:Text = "ENR".
      /*  */
      /* SokEnrEt */
      /*  */
      THIS-OBJECT:SokEnrEt:Location = NEW System.Drawing.Point(135, 35).
      THIS-OBJECT:SokEnrEt:Name = "SokEnrEt".
      THIS-OBJECT:SokEnrEt:Size = NEW System.Drawing.Size(154, 21).
      THIS-OBJECT:SokEnrEt:TabIndex = 17.
      THIS-OBJECT:SokEnrEt:Enter:Subscribe(THIS-OBJECT:SokEnrEt_Enter).
      THIS-OBJECT:SokEnrEt:KeyDown:Subscribe(THIS-OBJECT:SokEnrEt_KeyDown).
      /*  */
      /* buttonSt */
      /*  */
      THIS-OBJECT:buttonSt:Location = NEW System.Drawing.Point(314, 65).
      THIS-OBJECT:buttonSt:Name = "buttonSt".
      THIS-OBJECT:buttonSt:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonSt:TabIndex = 16.
      THIS-OBJECT:buttonSt:Text = "Slutet".
      THIS-OBJECT:buttonSt:Click:Subscribe(THIS-OBJECT:buttonSt_Click).
      /*  */
      /* buttonMt */
      /*  */
      THIS-OBJECT:buttonMt:Location = NEW System.Drawing.Point(314, 43).
      THIS-OBJECT:buttonMt:Name = "buttonMt".
      THIS-OBJECT:buttonMt:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonMt:TabIndex = 15.
      THIS-OBJECT:buttonMt:Text = "Någonstans".
      THIS-OBJECT:buttonMt:Click:Subscribe(THIS-OBJECT:buttonMt_Click).
      /*  */
      /* buttonBt */
      /*  */
      THIS-OBJECT:buttonBt:Location = NEW System.Drawing.Point(314, 21).
      THIS-OBJECT:buttonBt:Name = "buttonBt".
      THIS-OBJECT:buttonBt:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonBt:TabIndex = 14.
      THIS-OBJECT:buttonBt:Text = "Början".
      THIS-OBJECT:buttonBt:Click:Subscribe(THIS-OBJECT:buttonBt_Click).
      /*  */
      /* comboTillLev */
      /*  */
      THIS-OBJECT:comboTillLev:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboTillLev:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboTillLev:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboTillLev:Location = NEW System.Drawing.Point(12, 3).
      THIS-OBJECT:comboTillLev:Name = "comboTillLev".
      THIS-OBJECT:comboTillLev:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboTillLev:TabIndex = 11.
      /*  */
      /* nyLiPop */
      /*  */
      THIS-OBJECT:nyLiPop:PopupControl = THIS-OBJECT:nyLiPanel.
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:direkticell = FALSE.
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(300, 300).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      /*  */
      /* UtbytListaAdm */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitListaEnr).
      THIS-OBJECT:Name = "UtbytListaAdm".
      THIS-OBJECT:Text = "UtbytListaAdm".
      THIS-OBJECT:splitListaEnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitListaEnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitListaEnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitListaEnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitValUtBytlista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitValUtBytlista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitValUtBytlista:ResumeLayout(FALSE).
      THIS-OBJECT:splitListor:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitListor:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitListor, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitListor:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerListaFel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerListaFel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerListaFel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerListaFel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUtb, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridFelImp, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitNyLista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitNyLista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitNyLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitNyLista:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSokEnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSokEnr:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerSokEnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSokEnr:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:SokEnrUt, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:SokEnrUf, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboUtbTill, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboUtbFran, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitPopUp:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitPopUp:Panel1:PerformLayout().
      THIS-OBJECT:splitPopUp:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitPopUp, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitPopUp:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripListaNy:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripListaNy:PerformLayout().
      CAST(THIS-OBJECT:comboLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:nyLiPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyLiPanel:ClientArea:PerformLayout().
      THIS-OBJECT:nyLiPanel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:fillinListName, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitEnrVal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitEnrVal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitEnrVal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitEnrVal:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyBort:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyBort:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:PerformLayout().
      THIS-OBJECT:splitUrvalEnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitUrvalEnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitUrvalEnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitUrvalEnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitSokEnrHuvL:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitSokEnrHuvL:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitSokEnrHuvL, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitSokEnrHuvL:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridEnrFran, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:SokBenBf, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:SokEnrEf, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboFranLev, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitSokEnrLev:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitSokEnrLev:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitSokEnrLev:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitSokEnrLev, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitSokEnrLev:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridEnrTill, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:SokBenBt, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:SokEnrEt, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboTillLev, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID RemoveButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      IF THIS-OBJECT:GridUtb:ActiveRow NE ? THEN DO:
         IF THIS-OBJECT:GridUtb:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            THIS-OBJECT:GridUtb:GuruBortrader(FALSE).
            THIS-OBJECT:ControlDataDb:BortMtrl().
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
	METHOD PRIVATE VOID toolStripButton1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE uidvar AS INTEGER NO-UNDO.
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		toolButtoval = "Bo".
		IF INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID"))) = 0 THEN DO:
		   rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(237), "").  
         RETURN.
		END.
		rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(236), 
		STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UNAMN")), 
		System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() NE "Yes" THEN RETURN.
		
		uidvar = INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID"))). 
		THIS-OBJECT:ControlDataDb:BoLista(INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")))).
      
      IF THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:comboLista:VALUE = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE.
      END.
      ELSE DO:
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:FIND-FIRST("WHERE UID < " + STRING(uidvar),NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:AVAILABLE THEN DO:
            THIS-OBJECT:comboLista:VALUE = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE.
         END.
      END.
      THIS-OBJECT:comboLista:GuruFiltrera().
   	RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonExcel_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
   DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
   DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
   fildir = SESSION:TEMP-DIRECTORY.   
      
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(SESSION:TEMP-DIRECTORY) NO-ERROR.
   
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    fildir
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}
      IF OKvald = FALSE THEN RETURN.  
      THIS-OBJECT:ExcelImporters(fildir).
      
   END.               
   {musarrow.i}
		RETURN.

	END METHOD.

/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID toolStripAter_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
               
      THIS-OBJECT:AddButton:VISIBLE = TRUE.
      THIS-OBJECT:RemoveButton:VISIBLE = TRUE.
      THIS-OBJECT:toolStripButtonExcel:VISIBLE = TRUE.
      THIS-OBJECT:toolStripButtonAter:VISIBLE = FALSE.
      THIS-OBJECT:splitContainerListaFel:Panel1Collapsed = FALSE.
      THIS-OBJECT:splitContainerListaFel:Panel2Collapsed = TRUE.
      RETURN.
   END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtoNy_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   toolButtoval = "Ny".
	   THIS-OBJECT:fillinListName:Text = "Ersättningslista Mtrl".
		THIS-OBJECT:nyLiPop:Show().
		THIS-OBJECT:fillinListName:Select().
		  
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripListRubrik_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		toolButtoval = "Änd".
		THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:FIND-FIRST("WHERE  UID = " + STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")),NO-LOCK) NO-ERROR.
		IF THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE.
         THIS-OBJECT:fillinListName:Text = THIS-OBJECT:ControlDataDb:UtbytNamnbuffh:BUFFER-FIELD("UNAMN"):BUFFER-VALUE.
         THIS-OBJECT:nyLiPop:Show().
         THIS-OBJECT:fillinListName:Select().
        
		END.
		RETURN.

	END METHOD.
	METHOD PUBLIC VOID ExcelImporters(filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE extrakol AS LOGICAL NO-UNDO.

      THIS-OBJECT:ControlDataDb:utbimportbuffh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:VisaExcel:iRad = 1.    
      THIS-OBJECT:VisaExcel:iRad = 1.
      THIS-OBJECT:VisaExcel:cColname = "A".
      THIS-OBJECT:VisaExcel:ColumnRad().
      THIS-OBJECT:VisaExcel:ReadExcelStart(INPUT filnamn).
      extrakol = FALSE.
      DEBUGGER:SET-BREAK().            
      DO WHILE THIS-OBJECT:VisaExcel:iRad <= THIS-OBJECT:VisaExcel:iRadslut:
         invar = THIS-OBJECT:VisaExcel:GetData().
         IF invar = ? THEN.
         ELSE IF invar NE "" THEN DO:
            
            IF THIS-OBJECT:VisaExcel:cColname = "A" THEN DO:
               THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-CREATE().
               THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.   
            IF THIS-OBJECT:VisaExcel:cColname = "B" THEN DO:                                      
               THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
            END.
            IF THIS-OBJECT:VisaExcel:cColname = "C" THEN DO:
               THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD "):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.    
            IF THIS-OBJECT:VisaExcel:cColname = "D" THEN DO:                     
               THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
            END.
         END.
               
         THIS-OBJECT:VisaExcel:ColRight().                        
         IF THIS-OBJECT:VisaExcel:cColname = "E" THEN DO:
            THIS-OBJECT:VisaExcel:cColname = "A". 
            THIS-OBJECT:VisaExcel:Rowdown().
         END.                        
      END.  
      THIS-OBJECT:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().

      THIS-OBJECT:ImportLista(INTEGER(STRING(THIS-OBJECT:comboLista:SelectedRow:GetCellVALUE("UID")))).
      THIS-OBJECT:FelMeddCheck().
   END METHOD.
   
	METHOD PUBLIC VOID ImportLista(INPUT uidvar AS INTEGER):   
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kollstr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kollok AS LOGICAL NO-UNDO.
      DEFINE VARIABLE kollokenr AS LOGICAL NO-UNDO.
      DEFINE VARIABLE appdbh AS HANDLE NO-UNDO.
      THIS-OBJECT:ControlDataDb:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:utbimportbuffh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:utbimportbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         kollok = TRUE.
         Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = " + QUOTER(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         IF Guru.Konstanter:LeverantorTTh:AVAILABLE  = FALSE  THEN DO:
            kollok = FALSE.
            THIS-OBJECT:ControlDataDb:TidutHmt(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(239)).   
         END.         
         Guru.Konstanter:LeverantorTTh:FIND-FIRST("WHERE LEVKOD = " + QUOTER(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         IF Guru.Konstanter:LeverantorTTh:AVAILABLE = FALSE THEN DO:
            kollok = FALSE.
            THIS-OBJECT:ControlDataDb:TidutHmt(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(239)).
         END.
         
         appdbh = THIS-OBJECT:ControlDataDb:AppServerHandle.
         RUN KollMtrltill_UI IN appdbh (INPUT THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE, INPUT THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE, OUTPUT kollokenr).
         IF kollokenr = FALSE THEN DO:
            kollok = FALSE.
            THIS-OBJECT:ControlDataDb:TidutHmt(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(241)).
         END.   
         IF kollok = TRUE THEN DO:  
            kollstr = "WHERE  UID =   " + STRING(uidvar)  + " AND ORGLEVKOD  = " + QUOTER(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE)   + " AND ORGENR  = " + QUOTER(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE)
            + " AND BYTTILLLEVKOD  = " + QUOTER(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE) .       
            THIS-OBJECT:ControlDataDb:Utbyttabbuffh:FIND-FIRST( kollstr  ,NO-LOCK) NO-ERROR.      
            IF THIS-OBJECT:ControlDataDb:Utbyttabbuffh:AVAILABLE THEN DO:
               THIS-OBJECT:ControlDataDb:TidutHmt(THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE + " " + THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE + " " +  THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(238)).
            END.
            ELSE DO:
               /*allt ok-läs in*/
               
               THIS-OBJECT:ControlDataDb:Utbyttabbuffh:BUFFER-CREATE().      
               THIS-OBJECT:ControlDataDb:Utbyttabbuffh:BUFFER-COPY(THIS-OBJECT:ControlDataDb:utbimportbuffh, "UID").                              
               THIS-OBJECT:ControlDataDb:Utbyttabbuffh:BUFFER-FIELD("UID"):BUFFER-VALUE = uidvar.
                                            
               
            END.
         END.
         THIS-OBJECT:ControlDataDb:utbimportbuffh:BUFFER-DELETE() NO-ERROR.  
         qH:GET-NEXT().      
      END.      
            
          
      THIS-OBJECT:ControlDataDb:utbimportbuffh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      THIS-OBJECT:ControlDataDb:ImpUtbMtrlSpara().
      
      THIS-OBJECT:GridUtb:GuruReopen().
      THIS-OBJECT:GridUtb:GuruFirstrow().
      
       
   END METHOD.
   
   METHOD PUBLIC VOID FelMeddCheck():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:ControlDataDb:tiduth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:tiduth:AVAILABLE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(183),"", System.Windows.Forms.MessageBoxButtons:ok,System.Windows.Forms.MessageBoxIcon:Information).
         /*THIS-OBJECT:TabManager:Tabs["Felmedd"]:Selected = TRUE.*/
         THIS-OBJECT:GridFelImp:GuruReopen().
         THIS-OBJECT:GridFelImp:GuruFirstrow().
         THIS-OBJECT:splitContainerListaFel:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerListaFel:Panel2Collapsed = FALSE.
         Guru.GlobalaVariabler:DoNotOpenWin = FALSE.
         
         THIS-OBJECT:AddButton:VISIBLE = FALSE.
         THIS-OBJECT:RemoveButton:VISIBLE = FALSE.
         THIS-OBJECT:toolStripButtonExcel:VISIBLE = FALSE.
         THIS-OBJECT:toolStripButtonAter:VISIBLE = TRUE.
         
      END.
      
   END METHOD.
    

	
	DESTRUCTOR PUBLIC UtbytListaAdm ( ):

	END DESTRUCTOR.

END CLASS.