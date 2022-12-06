 
 /*------------------------------------------------------------------------
    File        : EkgExport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Oct 08 10:57:18 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.EkgData.EkgExportControl INHERITS UserControl: 
   
   {EKGEBRE.I} 
   {EKGEBREGURU.I}
   DEFINE PUBLIC VARIABLE EbrDLong AS LONGCHAR NO-UNDO.
   DEFINE PUBLIC VARIABLE EbrELong AS LONGCHAR NO-UNDO.
   DEFINE PUBLIC VARIABLE EbrTP1long AS LONGCHAR NO-UNDO.
   DEFINE PUBLIC VARIABLE EbrTP2long AS LONGCHAR NO-UNDO.
   DEFINE PUBLIC VARIABLE EbrTP3long AS LONGCHAR NO-UNDO.
   DEFINE PUBLIC VARIABLE comboTryckExp AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE ExportKatalogerSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridjmfKont AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KontrollDET AS Controls.Button NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   
   DEFINE PUBLIC VARIABLE p1blad AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE p2blad AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE p3blad AS INTEGER NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerjmfKoll AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TillEbrData AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE TillTryck AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE TillExcel AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE TillEbre AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE P2KomControl                 AS Modules.EkgData.EkgP2Kom                                       NO-UNDO.
   DEFINE PRIVATE VARIABLE Ebr-Data_P1P2Frekvensh       AS HANDLE NO-UNDO.  
   DEFINE PRIVATE VARIABLE Ebr-Data_P2P3Frekvensh       AS HANDLE NO-UNDO. 
   DEFINE PRIVATE VARIABLE Ebr-Data_P2Komh AS HANDLE NO-UNDO.
   DEFINE PRIVATE VARIABLE Ebr-Tryck_P2Komh AS HANDLE NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonAter AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStrip1 AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE Ebr-Data_loph AS HANDLE NO-UNDO.
   
   CONSTRUCTOR PUBLIC EkgExportControl (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Ebr-Data_P1P2Frekvensh = TEMP-TABLE Ebr-Data_P1P2Frekvens:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:Ebr-Data_P2P3Frekvensh = TEMP-TABLE Ebr-Data_P2P3Frekvens:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:Ebr-Data_P2Komh = TEMP-TABLE Ebr-Data_P2Kom:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:Ebr-Tryck_P2Komh = TEMP-TABLE  Ebr-Tryck_P2Kom:HANDLE:DEFAULT-BUFFER-HANDLE.
  
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      THIS-OBJECT:P2KomControl = THIS-OBJECT:ControlShell:P2KomControl.
      
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:ExportKatalogerSC:Panel2Collapsed = TRUE.
      THIS-OBJECT:ExportKatalogerSC:Panel1Collapsed = FALSE.
   END METHOD. 
   METHOD PUBLIC VOID CreateGrids():

      THIS-OBJECT:CreateGridjmfKont().
   END METHOD.
   METHOD PUBLIC VOID CreateGridjmfKont():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("ArbetsKod","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("Lopnummer","Löp",TRUE,30).
      rubrikergrid[3] = NEW Controls.GridRubrik("LISTA","Katalog",TRUE,60).
      rubrikergrid[4] = NEW Controls.GridRubrik("Level","Nivå",TRUE,60).
      rubrikergrid[5] = NEW Controls.GridRubrik("FELORSAK","Differens",TRUE,540).
      THIS-OBJECT:GridjmfKont:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:jmffelTTh)).
      THIS-OBJECT:GridjmfKont:Text = "Jämförelse Ebr-D mot Ebr-E och Ebr-Tryck".
      THIS-OBJECT:GridjmfKont:GuruUpdateTitle().
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KontrollDET_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ExportEBR-Tryck("Alla",TRUE).
      THIS-OBJECT:ExportEBR-EData("EbrE",TRUE).
      THIS-OBJECT:ExportEBR-EData("EbrD",TRUE).
      THIS-OBJECT:KontrollExport().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TillEbrE_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   Guru.Konstanter:AmericanSet().
	   THIS-OBJECT:ExportEBR-EData("EbrE",FALSE).
	   Guru.Konstanter:EuropeanAmericanReset().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TillExcel_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 THIS-OBJECT:ExportKLGData(ControlWinLg:ebrkatvar).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TillTryck_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 /*
      THIS-OBJECT:ExportKLGTryckStart("KLG1").
      */
      Guru.Konstanter:EuropeanSet(). 
      DEFINE VARIABLE listP AS CHARACTER NO-UNDO.
      /*Anders Olsson Elpool i Umeå AB  16 jan 2014 09:31:20 
      Alla P1 P2 P3 
      */
      /*listP = THIS-OBJECT:ControlDataDb:ekgexptryckvalTTh:BUFFER-FIELD("VALKOD"):BUFFER-VALUE. */ 
      listP = THIS-OBJECT:comboTryckExp:GuruCombo:VALUE.
      THIS-OBJECT:ControlDataDb:ekgexptryckvalTTh:FIND-FIRST("WHERE VAL = '" + STRING(THIS-OBJECT:comboTryckExp:GuruCombo:VALUE) + "'")  NO-ERROR. 
      listP = THIS-OBJECT:ControlDataDb:ekgexptryckvalTTh:BUFFER-FIELD("VALKOD"):BUFFER-VALUE.
      THIS-OBJECT:ExportEBR-Tryck(listP, FALSE).
		Guru.Konstanter:EuropeanAmericanReset().  
		RETURN. 

	END METHOD.

   METHOD PUBLIC VOID ExportEBR-EData(INPUT ExportVal AS CHARACTER, jmfKontroll AS LOGICAL):
      /*Anders Olsson Elpool i Umeå AB  3 dec 2013 12:40:09 
      Kod med temptabeller och inte handle
      metoden används både av EbrE och EbrD. 
      */
      
      DEFINE VARIABLE listaP AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:EbrXmlRensa().
      THIS-OBJECT:EbrXmlStart().
      
      /*p1*/
      listaP = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
      CREATE ebr-e_catalogue.
      ASSIGN
      ebr-e_catalogue.RID = RECID(ebr-e_year)
      ebr-e_catalogue.catalogue = "P1".
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = "G741" THEN. 
         ELSE DO:
            /*p1 arbkod*/
            CREATE ebr-e_workcode.
            ASSIGN
            ebr-e_workcode.RID = RECID(ebr-e_catalogue)
            ebr-e_workcode.code = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE.
            ebr-e_workcode.catalogue = ebr-e_catalogue.catalogue. 
            
            CREATE ebr-e_lop.
            ASSIGN
            ebr-e_lop.RID = RECID(ebr-e_workcode)
            ebr-e_lop.Level = ebr-e_catalogue.catalogue
            ebr-e_lop.code = ebr-e_workcode.code
            ebr-e_lop.number = "0".
            ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
            /*p1 lopnr*/
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1TTh:TABLE + " WHERE ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE ) NO-ERROR.
               IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                  IF ebr-e_lop.number = "0" THEN DO:
                     ebr-e_lop.code = ebr-e_workcode.code.
                     ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
                  END.
                  ELSE DO:
                     
                     CREATE ebr-e_lop.
                     ASSIGN
                     ebr-e_lop.RID = RECID(ebr-e_workcode)
                     ebr-e_lop.Level = ebr-e_catalogue.catalogue
                     ebr-e_lop.code = ebr-e_workcode.code
                     ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE,"99")
                     /*
                     ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                     */
                     ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
                  END.                
               END.
               CREATE ebr-e_lop.
               ASSIGN
               ebr-e_lop.RID = RECID(ebr-e_workcode)
               ebr-e_lop.Level = ebr-e_catalogue.catalogue
               ebr-e_lop.code = ebr-e_workcode.code
               ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE,"99")
               ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
               ebr-e_lop.unit = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
               ebr-e_lop.bered_hours = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE
               ebr-e_lop.mont_hours = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE
               ebr-e_lop.maskin_hours = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE
               ebr-e_lop.ea_hours = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE
               ebr-e_lop.arbete_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE
               ebr-e_lop.material_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE
               ebr-e_lop.maskin_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE
               ebr-e_lop.ovrigt_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE
               ebr-e_lop.summa_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE
               ebr-e_lop.utrust_hours = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE
               ebr-e_lop.utrust_cost = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE.
               
               IF THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
               ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               /*
               IF anmkop NE "" THEN DO:
                  IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-e_lop.comments = '-"-'.
                  ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               END.   
               ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).             
               */
               q2H:GET-NEXT().         
            END.
         END.   
         qH:GET-NEXT().         
      END. 
      /*p2*/
      CREATE ebr-e_catalogue.
      ASSIGN
      ebr-e_catalogue.RID = RECID(ebr-e_year).
      ebr-e_catalogue.catalogue = "P2".
 
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE BEGINS "7" THEN. 
         ELSE DO:
            /*p2 arbetskod*/
            CREATE ebr-e_workcode.
            ASSIGN
            ebr-e_workcode.RID = RECID(ebr-e_catalogue)
            ebr-e_workcode.code = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE.
            ebr-e_workcode.catalogue = ebr-e_catalogue.catalogue.
            CREATE ebr-e_lop.
            ASSIGN
            ebr-e_lop.RID = RECID(ebr-e_workcode)
            ebr-e_lop.Level = ebr-e_catalogue.catalogue
            ebr-e_lop.code = ebr-e_workcode.code
            ebr-e_lop.number = "0".
            ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
             /*p2 lopnr*/
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2TTh:TABLE + " WHERE ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE ) NO-ERROR.
               IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                  IF ebr-e_lop.number = "0" THEN DO:
                     ebr-e_lop.code = ebr-e_workcode.code.
                     ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
                  END.
                  ELSE DO:
                     CREATE ebr-e_lop.
                     ASSIGN
                     ebr-e_lop.RID = RECID(ebr-e_workcode)
                     ebr-e_lop.Level = ebr-e_catalogue.catalogue
                     ebr-e_lop.code = ebr-e_workcode.code
                     ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")
                     /*
                     ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                     */
                     ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
               
                  END.                
               END.
               CREATE ebr-e_lop.
               ASSIGN
               ebr-e_lop.RID = RECID(ebr-e_workcode)
               ebr-e_lop.Level = ebr-e_catalogue.catalogue
               ebr-e_lop.code = ebr-e_workcode.code
               ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")
               ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
               ebr-e_lop.unit = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
               ebr-e_lop.bered_hours = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE
               ebr-e_lop.mont_hours = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE
               ebr-e_lop.maskin_hours = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE
               ebr-e_lop.ea_hours = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("EA"):BUFFER-VALUE
               ebr-e_lop.arbete_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE
               ebr-e_lop.material_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE
               ebr-e_lop.maskin_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE
               ebr-e_lop.ovrigt_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE
               ebr-e_lop.summa_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE
               ebr-e_lop.utrust_hours = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE
               ebr-e_lop.utrust_cost = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE.
               IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
               /*
               ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               */
               /*
               IF anmkop NE "" THEN DO:
                  IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-e_lop.comments = '-"-'.
                  ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               END.   
               ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               */             
               q2H:GET-NEXT().         
            END.
         END.   
         qH:GET-NEXT().        
           
      END. 
      
      /*p3 */
      CREATE ebr-e_catalogue.
      ASSIGN
      ebr-e_catalogue.RID = RECID(ebr-e_year).
      ebr-e_catalogue.catalogue = "P3".
 
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         /*p3 arbetskod*/
         CREATE ebr-e_workcode.
         ASSIGN
         ebr-e_workcode.RID = RECID(ebr-e_catalogue)
         ebr-e_workcode.code = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE.
         ebr-e_workcode.catalogue = ebr-e_catalogue.catalogue.
        
         CREATE ebr-e_lop.
         ASSIGN
         ebr-e_lop.RID = RECID(ebr-e_workcode)
         ebr-e_lop.Level = ebr-e_catalogue.catalogue
         ebr-e_lop.code = ebr-e_workcode.code
         ebr-e_lop.number = "0"
         ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         /*p3 lopnr*/
         query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3TTh:TABLE + " WHERE ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'".
         q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3TTh,query2var).
         q2H:GET-FIRST().
         DO WHILE q2H:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE ) NO-ERROR.
            IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
               IF ebr-e_lop.number = "0" THEN DO:
                  ebr-e_lop.code = ebr-e_workcode.code.
                  ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
               END.
               ELSE DO:
                  CREATE ebr-e_lop.
                  ASSIGN
                  ebr-e_lop.RID = RECID(ebr-e_workcode)
                  ebr-e_lop.Level = ebr-e_catalogue.catalogue
                  ebr-e_lop.code = ebr-e_workcode.code
                  ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,"99")
                  /*
                  ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                  */
                  ebr-e_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
               END.                
            END.
            CREATE ebr-e_lop.
            ASSIGN
            ebr-e_lop.RID = RECID(ebr-e_workcode)
            ebr-e_lop.Level = ebr-e_catalogue.catalogue
            ebr-e_lop.code = ebr-e_workcode.code
            ebr-e_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,"99")
            ebr-e_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
            ebr-e_lop.unit = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
            ebr-e_lop.mont_hours = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE
            ebr-e_lop.maskin_hours = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE
            ebr-e_lop.ea_hours = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("EA"):BUFFER-VALUE
            ebr-e_lop.count_men = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE
            ebr-e_lop.timebase = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE
            ebr-e_lop.utrust_hours = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE.
            IF THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
            ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
            /*
            IF anmkop NE "" THEN DO:
               IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-e_lop.comments = '-"-'.
               ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
            END.   
            ELSE ebr-e_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
            anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).             
            */
            q2H:GET-NEXT().         
         END.
         qH:GET-NEXT().         
      END. 
      IF ExportVal = "EbrD"  THEN THIS-OBJECT:Ebr-Data-Export(ExportVal).
      
      THIS-OBJECT:EbrXmlutEData(listaP,"",ExportVal,jmfKontroll).


   END METHOD.
   METHOD PUBLIC VOID Ebr-Data-Export(INPUT ExportVal AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH2       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
      DEFINE VARIABLE loprid AS ROWID NO-UNDO.
      FIND FIRST ebr-e_costcatalogue WHERE NO-LOCK NO-ERROR.
      FIND FIRST ebr-e_type WHERE NO-LOCK NO-ERROR.
      FIND FIRST ebr-e_year WHERE NO-LOCK NO-ERROR.
     
      CREATE Ebr-Data_Katalog.
      ASSIGN 
      Ebr-Data_Katalog.KostnadsKatalog = ebr-e_costcatalogue.kostnadskatalog_header
      Ebr-Data_Katalog.Type = ebr-e_type.type 
      Ebr-Data_Katalog.Year = ebr-e_year.year.
      FOR EACH ebr-e_catalogue WHERE NO-LOCK:
         CREATE Ebr-Data_PLevel.
         ASSIGN 
         Ebr-Data_PLevel.Year = Ebr-Data_Katalog.Year
         Ebr-Data_PLevel.Level = ebr-e_catalogue.catalogue
         Ebr-Data_PLevel.RID = RECID(Ebr-Data_Katalog).
      END.
      FOR EACH Ebr-Data_PLevel NO-LOCK,
      EACH ebr-e_workcode WHERE ebr-e_workcode.catalogue = Ebr-Data_PLevel.Level NO-LOCK:
         CREATE Ebr-Data_Arbetskod.
         ASSIGN 
         Ebr-Data_Arbetskod.Level = ebr-e_workcode.catalogue
         Ebr-Data_Arbetskod.ArbetsKod = ebr-e_workcode.code.
         Ebr-Data_Arbetskod.RID = RECID(Ebr-Data_PLevel).
         FIND FIRST ebr-e_lop WHERE ebr-e_lop.Level = Ebr-Data_Arbetskod.Level AND ebr-e_lop.code = ebr-e_workcode.code AND  ebr-e_lop.number = "0" NO-LOCK NO-ERROR.
         IF AVAILABLE ebr-e_lop THEN DO:
            ASSIGN 
            Ebr-Data_Arbetskod.Arbete =  ebr-e_lop.work_construction.
            /*
            Ebr-Data_Arbetskod.Kommentar = ebr-e_lop.comments.
            */
         END.   
      END.
      
      FOR EACH Ebr-Data_PLevel NO-LOCK,
      EACH ebr-e_workcode WHERE ebr-e_workcode.catalogue = Ebr-Data_PLevel.Level NO-LOCK,
      EACH ebr-e_lop WHERE ebr-e_lop.Level = ebr-e_workcode.catalogue AND ebr-e_lop.code = ebr-e_workcode.code NO-LOCK:
         IF ebr-e_lop.work_construction = "" THEN. 
         ELSE IF ebr-e_lop.number = "0" THEN. 
         ELSE DO:
            CREATE Ebr-Data_lop.
            ASSIGN 
            Ebr-Data_lop.RID = RECID(Ebr-Data_PLevel)
            Ebr-Data_lop.Level         =    Ebr-Data_PLevel.Level                                    
            Ebr-Data_lop.ArbetsKod     =    ebr-e_lop.code           
            Ebr-Data_lop.Lopnummer     =    ebr-e_lop.number         
            Ebr-Data_lop.Arbete        =    ebr-e_lop.work_constructi
            Ebr-Data_lop.Enhet         =    ebr-e_lop.unit           
            Ebr-Data_lop.Bered_Tid     =    ebr-e_lop.bered_hours    
            Ebr-Data_lop.Mont_Tid      =    ebr-e_lop.mont_hours     
            Ebr-Data_lop.Maskin_Tid    =    ebr-e_lop.maskin_hours   
            Ebr-Data_lop.Utrust_Tid    =    ebr-e_lop.utrust_hours   
            Ebr-Data_lop.Ea_Tid        =    ebr-e_lop.ea_hours       
            Ebr-Data_lop.Arbete_Kost   =    ebr-e_lop.arbete_cost    
            Ebr-Data_lop.Material_Kost =    ebr-e_lop.material_cost  
            Ebr-Data_lop.Maskin_Kost   =    ebr-e_lop.maskin_cost    
            Ebr-Data_lop.Utrust_Kost   =    ebr-e_lop.utrust_cost    
            Ebr-Data_lop.Ovrigt_Kost   =    ebr-e_lop.ovrigt_cost    
            Ebr-Data_lop.Summa_Kost    =    ebr-e_lop.summa_cost     
            Ebr-Data_lop.Antal_Man     =    ebr-e_lop.count_men      
            Ebr-Data_lop.TidUnder      =    ebr-e_lop.timebase
            Ebr-Data_lop.Kommentar     =    ebr-e_lop.comments.
         END.
      END.
     
      FOR EACH ebr-e_lop WHERE ebr-e_lop.number = "0" OR ebr-e_lop.work_construction = "" NO-LOCK:
         CREATE Ebr-Data_lopanm.
         ASSIGN 
         Ebr-Data_lopanm.Level =  ebr-e_lop.Level
         Ebr-Data_lopanm.ArbetsKod = ebr-e_lop.code
         Ebr-Data_lopanm.Lopnummer = ebr-e_lop.number
         Ebr-Data_lopanm.Kommentar = ebr-e_lop.comments.
         DELETE ebr-e_lop.
      END.
      
      FOR EACH Ebr-Data_lop NO-LOCK:
         loprid = ?.
         FIND FIRST Ebr-Data_lopanm WHERE Ebr-Data_lopanm.Level = Ebr-Data_lop.Level AND Ebr-Data_lopanm.ArbetsKod = Ebr-Data_lop.ArbetsKod AND Ebr-Data_lopanm.Lopnummer <= Ebr-Data_lop.Lopnummer NO-LOCK NO-ERROR.
         REPEAT: 
            IF AVAILABLE Ebr-Data_lopanm THEN loprid = ROWID(Ebr-Data_lopanm).
            ELSE LEAVE.
            FIND NEXT Ebr-Data_lopanm WHERE Ebr-Data_lopanm.Level = Ebr-Data_lop.Level AND Ebr-Data_lopanm.ArbetsKod = Ebr-Data_lop.ArbetsKod AND Ebr-Data_lopanm.Lopnummer <= Ebr-Data_lop.Lopnummer NO-LOCK NO-ERROR.
         END.   
         IF loprid NE ? THEN DO:
            FIND Ebr-Data_lopanm WHERE ROWID(Ebr-Data_lopanm) = loprid NO-LOCK NO-ERROR.
            Ebr-Data_lop.Kommentar = TRIM(Ebr-Data_lopanm.Kommentar) + " " + TRIM(Ebr-Data_lop.Kommentar).
         END.
      END. 
      IF ExportVal = "EbrD"  THEN DO:
         THIS-OBJECT:ControlDataDb:laddaP2MtrlForExport().
         FIND FIRST Ebr-Data_PLevel WHERE Ebr-Data_PLevel.Level = "P2" NO-LOCK NO-ERROR.
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE BEGINS "7" THEN. 
            ELSE DO: 
               CREATE Ebr-Data_mtrl.
               ASSIGN  
               Ebr-Data_mtrl.Level = Ebr-Data_PLevel.Level               
               Ebr-Data_mtrl.RID = RECID(Ebr-Data_PLevel)
               Ebr-Data_mtrl.ArbetsKod = THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE  
               Ebr-Data_mtrl.Lopnummer = STRING(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")
               Ebr-Data_mtrl.Arbete = THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE
               Ebr-Data_mtrl.MtrlBenamning = THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("MTRLBENAMNING"):BUFFER-VALUE
               Ebr-Data_mtrl.Antal = THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
            END.   
            qH:GET-NEXT().         
         END.
         
         FOR EACH Ebr-Data_Katalog WHERE NO-LOCK:
            CREATE Ebr-Data_FrekP1P2Katalog.
            BUFFER-COPY Ebr-Data_Katalog TO Ebr-Data_FrekP1P2Katalog.
            CREATE Ebr-Data_FrekP2P3Katalog.
            BUFFER-COPY Ebr-Data_Katalog TO Ebr-Data_FrekP2P3Katalog.
         END.
         THIS-OBJECT:ControlDataDb:LaddaP1VFfrek().
         THIS-OBJECT:ControlDataDb:LaddaP2VFfrek().
         
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = "G741" THEN.
            ELSE DO:
               THIS-OBJECT:Ebr-Data_P1P2Frekvensh:BUFFER-CREATE().         
               THIS-OBJECT:Ebr-Data_P1P2Frekvensh:BUFFER-COPY(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh).
               THIS-OBJECT:Ebr-Data_P1P2Frekvensh:BUFFER-FIELD("Year"):BUFFER-VALUE = Ebr-Data_FrekP1P2Katalog.Year.
               THIS-OBJECT:Ebr-Data_P1P2Frekvensh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Data_FrekP1P2Katalog).
            END.   
            qH:GET-NEXT().         
         END.
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2frekvensTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2frekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp2frekvensTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE BEGINS "7" THEN.
            ELSE DO:
               THIS-OBJECT:Ebr-Data_P2P3Frekvensh:BUFFER-CREATE().         
               THIS-OBJECT:Ebr-Data_P2P3Frekvensh:BUFFER-COPY(THIS-OBJECT:ControlDataDb:ekgp2frekvensTTh).
               THIS-OBJECT:Ebr-Data_P2P3Frekvensh:BUFFER-FIELD("Year"):BUFFER-VALUE = Ebr-Data_FrekP2P3Katalog.Year.
               THIS-OBJECT:Ebr-Data_P2P3Frekvensh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Data_FrekP2P3Katalog).
            END.   
            qH:GET-NEXT().         
         END.
      END.
       /*Anders Olsson Elpool i Umeå AB  14 nov 2014 15:46:15 
      lägger till kommentarer 
      */
      /*p2 kom*/
      DEFINE VARIABLE varp2arb AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2lop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ordningvar AS INTEGER NO-UNDO.
      FOR EACH Ebr-Data_Katalog WHERE NO-LOCK:
         CREATE Ebr-Data_P2KomKatalog.
         BUFFER-COPY Ebr-Data_Katalog TO Ebr-Data_P2KomKatalog.         
      END.
      /*OUTPUT TO "c:\temp\p2.txt".
      
      FOR EACH ebr-e_lop WHERE NO-LOCK:
         PUT ebr-e_lop.code " " ebr-e_lop.number SKIP.
      END. 
     OUTPUT CLOSE.*/ 
      queryvar2 =  "FOR EACH " + THIS-OBJECT:ControlDataDb:sveptemph:TABLE. 
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF varp2arb = THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE AND 
         varp2lop = THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE THEN.
         ELSE DO:
            THIS-OBJECT:ControlDataDb:SvepLopNrStartData(THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE,THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
         END.   
         varp2arb = THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE.
         varp2lop = THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE.
         
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:sveptemph,queryvar2).
         qH2:GET-FIRST().
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            ordningvar = THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
            FIND FIRST Ebr-Data_lop WHERE Ebr-Data_lop.ArbetsKod =  THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE AND 
            Ebr-Data_lop.Lopnummer = STRING(THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") NO-LOCK NO-ERROR.
            IF AVAILABLE Ebr-Data_lop THEN DO:
               THIS-OBJECT:Ebr-Data_P2Komh:FIND-FIRST("WHERE P2ARBKOD= '" + THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND P2LOPNR = " +
               THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE + " AND ORDNING = " +
               THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE ) NO-ERROR.
               IF THIS-OBJECT:Ebr-Data_P2Komh:AVAILABLE THEN DO:
                  THIS-OBJECT:Ebr-Data_P2Komh:FIND-LAST("WHERE P2ARBKOD= '" + THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND P2LOPNR = " +
                  THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
                  ordningvar = THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE + 1.
               END.
                     
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-CREATE().
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-COPY(THIS-OBJECT:ControlDataDb:ekgKoppArbSvepTTh,"ORDNING").
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordningvar.
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE = STRING(THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"). 
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE = THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE.
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("Year"):BUFFER-VALUE = Ebr-Data_P2KomKatalog.Year.
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Data_P2KomKatalog).
            END.
            qH2:GET-NEXT().         
         END.
        
         qH:GET-NEXT().   
      END.
      THIS-OBJECT:ControlDataDb:SvepLopEmpty().
      varp2arb = "".
      varp2lop = "".
      FOR EACH Ebr-Data_P2Kom WHERE Ebr-Data_P2Kom.SEP2ARBKOD NE "" AND Ebr-Data_P2Kom.SESVEPLOPNR = "" NO-LOCK:
         FOR EACH Ebr-Data_P2Kombuff WHERE Ebr-Data_P2Kombuff.P2ARBKOD =  Ebr-Data_P2Kom.SEP2ARBKOD AND Ebr-Data_P2Kombuff.P2LOPNR = Ebr-Data_P2Kom.P2LOPNR NO-LOCK:
            CREATE Ebr-Data_P2Kombuff2.
            BUFFER-COPY Ebr-Data_P2Kombuff EXCEPT Ebr-Data_P2Kombuff.P2ARBKOD Ebr-Data_P2Kombuff.P2LOPNR TO Ebr-Data_P2Kombuff2.
            ASSIGN 
            Ebr-Data_P2Kombuff2.SEP2ARBKOD = "" 
            Ebr-Data_P2Kombuff2.SESVEPLOPNR = "" 
            Ebr-Data_P2Kombuff2.P2ARBKOD = Ebr-Data_P2Kom.P2ARBKOD
            Ebr-Data_P2Kombuff2.P2LOPNR = Ebr-Data_P2Kom.P2LOPNR
            Ebr-Data_P2Kombuff2.SVEPLOPNR = Ebr-Data_P2Kom.SVEPLOPNR.
            THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("Year"):BUFFER-VALUE = Ebr-Data_P2KomKatalog.Year.
            THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Data_P2KomKatalog).
         END.
         DELETE Ebr-Data_P2Kom.
      END. 
     
      FOR EACH Ebr-Data_P2Kom WHERE Ebr-Data_P2Kom.SEP2ARBKOD NE "" NO-LOCK:
         THIS-OBJECT:ControlDataDb:SvepLopNrStartData(Ebr-Data_P2Kom.SEP2ARBKOD,Ebr-Data_P2Kom.SESVEPLOPNR).
         THIS-OBJECT:ControlDataDb:sveptemph:FIND-FIRST("WHERE  ") NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:sveptemph:AVAILABLE THEN DO:
            FOR EACH Ebr-Data_P2Kombuff WHERE Ebr-Data_P2Kombuff.P2ARBKOD =  Ebr-Data_P2Kom.SEP2ARBKOD AND Ebr-Data_P2Kombuff.P2LOPNR = THIS-OBJECT:ControlDataDb:sveptemph:BUFFER-FIELD("LOPNR"):BUFFER-VALUE NO-LOCK:
               CREATE Ebr-Data_P2Kombuff2.
               BUFFER-COPY Ebr-Data_P2Kombuff EXCEPT Ebr-Data_P2Kombuff.P2ARBKOD Ebr-Data_P2Kombuff.P2LOPNR TO Ebr-Data_P2Kombuff2.
               ASSIGN 
               Ebr-Data_P2Kombuff2.SEP2ARBKOD = "" 
               Ebr-Data_P2Kombuff2.SESVEPLOPNR = "" 
               Ebr-Data_P2Kombuff2.P2ARBKOD = Ebr-Data_P2Kom.P2ARBKOD
               Ebr-Data_P2Kombuff2.P2LOPNR = Ebr-Data_P2Kom.P2LOPNR
               Ebr-Data_P2Kombuff2.SVEPLOPNR = Ebr-Data_P2Kom.SVEPLOPNR.
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("Year"):BUFFER-VALUE = Ebr-Data_P2KomKatalog.Year.
               THIS-OBJECT:Ebr-Data_P2Komh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Data_P2KomKatalog).
            END.
         END.   
         DELETE Ebr-Data_P2Kom.
      END.  
      THIS-OBJECT:ControlDataDb:SvepLopEmpty().
      
      
      
   END METHOD.
   METHOD PUBLIC VOID EbrXmlutEData(INPUT listaP AS CHARACTER, INPUT nollut AS CHARACTER, ExportVal AS CHARACTER,jmfKontroll AS LOGICAL):
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFileUt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE p1p2frekFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE p2p3frekFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE p2KomFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cEncoding  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE utfrekfillong AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE utp2fillong AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      cFile = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      p2KomFile = cFile. 
      p1p2frekFile = cFile.
      ASSIGN
      cEncoding = "iso8859-1"
      cFileUt = cFile + "costcatalogue" + listaP + ".xml".
      IF ExportVal = "EbrE"  THEN DO:
         DATASET  EBR-eDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
         IF jmfKontroll = TRUE THEN DO:
            EbrElong = utfillong.
            RETURN.
         END. 
         
         utfillong = REPLACE(utfillong,'<work_construction/>','').
         utfillong = REPLACE(utfillong,'<comments/>','').
         utfillong = REPLACE(utfillong,'<number>0</number>','').
         utfillong = REPLACE(utfillong,'<unit/>','').
         utfillong = REPLACE(utfillong,'<bered_hours>0</bered_hours>',nollut).
         utfillong = REPLACE(utfillong,'<bered_hours>0.0</bered_hours>',nollut).
         utfillong = REPLACE(utfillong,'<bered_hours>0.00</bered_hours>',nollut).
         utfillong = REPLACE(utfillong,'<mont_hours>0</mont_hours>',nollut).
         utfillong = REPLACE(utfillong,'<mont_hours>0.0</mont_hours>',nollut).
         utfillong = REPLACE(utfillong,'<mont_hours>0.00</mont_hours>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_hours>0</maskin_hours>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_hours>0.0</maskin_hours>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_hours>0.00</maskin_hours>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_hours>0</utrust_hours>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_hours>0.0</utrust_hours>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_hours>0.00</utrust_hours>',nollut).
         utfillong = REPLACE(utfillong,'<ea_hours>0</ea_hours>',nollut).
         utfillong = REPLACE(utfillong,'<ea_hours>0.0</ea_hours>',nollut).
         utfillong = REPLACE(utfillong,'<ea_hours>0.00</ea_hours>',nollut).
         utfillong = REPLACE(utfillong,'<arbete_cost>0</arbete_cost>',nollut).
         utfillong = REPLACE(utfillong,'<arbete_cost>0.0</arbete_cost>',nollut).
         utfillong = REPLACE(utfillong,'<arbete_cost>0.00</arbete_cost>',nollut).
         utfillong = REPLACE(utfillong,'<material_cost>0</material_cost>',nollut).
         utfillong = REPLACE(utfillong,'<material_cost>0.0</material_cost>',nollut).
         utfillong = REPLACE(utfillong,'<material_cost>0.00</material_cost>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_cost>0</maskin_cost>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_cost>0.0</maskin_cost>',nollut).
         utfillong = REPLACE(utfillong,'<maskin_cost>0.00</maskin_cost>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_cost>0</utrust_cost>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_cost>0.0</utrust_cost>',nollut).
         utfillong = REPLACE(utfillong,'<utrust_cost>0.00</utrust_cost>',nollut).
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0</ovrigt_cost>',nollut).
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0.0</ovrigt_cost>',nollut).
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0.00</ovrigt_cost>',nollut).
         utfillong = REPLACE(utfillong,'<summa_cost>0</summa_cost>',nollut).
         utfillong = REPLACE(utfillong,'<summa_cost>0.0</summa_cost>',nollut).
         utfillong = REPLACE(utfillong,'<summa_cost>0.00</summa_cost>',nollut).
         utfillong = REPLACE(utfillong,'<count_men>0</count_men>',nollut).
        
         utfillong = REPLACE(utfillong,'<timebase/>','').
         utfillong = REPLACE(utfillong,'<work_construction xsi:nil="true"/>','').
         utfillong = REPLACE(utfillong,'<timebase xsi:nil="true"/>','').
         utfillong = REPLACE(utfillong,'<unit xsi:nil="true"/>','').
         COPY-LOB FROM utfillong TO FILE cFileUt.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + cFileUt," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         IF ExportVal = "EbrE"  THEN THIS-OBJECT:Ebr-Data-Export(ExportVal).
         cFileUt = cFile + "costcatalogue" + listaP + "Kommentarer.xml".
         DATASET Ebr-DataP2KomDS:WRITE-XML("LONGCHAR", utp2fillong,FALSE,"iso8859-1").
         COPY-LOB FROM utp2fillong TO FILE cFileUt.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + cFileUt," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
      END.      
      ELSE IF ExportVal = "EbrD"  THEN DO:
         Ebr-Data_loph = TEMP-TABLE  Ebr-Data_lop:DEFAULT-BUFFER-HANDLE.
         cFileUt = p1p2frekFile + "EBR" + listaP + ".xml".    
         IF Guru.konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.
         ELSE DO:            
            Ebr-Data_loph:BUFFER-FIELD("Ea_Tid"):XML-NODE-TYPE = "HIDDEN".  
         END.   
         DATASET  Ebr-DataDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").  
         IF jmfKontroll = TRUE THEN DO:
            EbrDlong = utfillong.
            RETURN.
         END.
         
         COPY-LOB FROM utfillong TO FILE cFileUt.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + cFileUt," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         p2p3frekFile = p1p2frekFile.
         p1p2frekFile = p1p2frekFile + "EBR" + listaP + "P1P2frek.xml".
         p2p3frekFile = p2p3frekFile + "EBR" + listaP + "P2P3frek.xml".
         DATASET Ebr-DataP1P2DS:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         COPY-LOB FROM utfrekfillong TO FILE p1p2frekFile.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + p1p2frekFile," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         DATASET Ebr-DataP2P3DS:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         COPY-LOB FROM utfrekfillong TO FILE p2p3frekFile.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + p2p3frekFile," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).            
         p2KomFile = p2KomFile + "EBR" + listaP + "P2Kommentarer.xml".
         DATASET Ebr-DataP2KomDS:WRITE-XML("LONGCHAR", utp2fillong,FALSE,"iso8859-1").
         COPY-LOB FROM utp2fillong TO FILE p2KomFile.
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + p2KomFile," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         /*
         ekgp1frekvensTTh:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         
         utfrekfillong = REPLACE(utfrekfillong,'ekgp1frekvensTTRow','ekgp1frekvensTT').
         */
         
         /*
         ekgp2frekvensTTh:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         utfrekfillong = REPLACE(utfrekfillong,'ekgp2frekvensTTRow','ekgp2frekvensTT').
         COPY-LOB FROM utfrekfillong TO FILE p2p3frekFile.
         */
          
         
         /*Anders Olsson Elpool i Umeå AB  14 nov 2014 10:52:35 
         om nästlat. 
         */
         /*
         FOR EACH ebr-e_costcatalogue NO-LOCK:
            CREATE Ebr-Data_Katalog.
            BUFFER-COPY ebr-e_costcatalogue TO Ebr-Data_Katalog.
            FOR EACH ebr-e_type WHERE ebr-e_type.RID = RECID(ebr-e_costcatalogue) NO-LOCK:
               CREATE Ebr-Data_type.
               BUFFER-COPY ebr-e_type TO Ebr-Data_type.
               Ebr-Data_type.kostnadskatalog_header = Ebr-Data_Katalog.KostnadsKatalog.
               Ebr-Data_type.RRID = RECID(Ebr-Data_Katalog).
               FOR EACH ebr-e_year WHERE ebr-e_year.RID = RECID(ebr-e_type) NO-LOCK:
                  CREATE Ebr-Data_Year.
                  BUFFER-COPY ebr-e_year TO Ebr-Data_Year.
                  Ebr-Data_Year.RRID = RECID(Ebr-Data_type).
                  Ebr-Data_Year.Type = Ebr-Data_type.type.
                  FOR EACH ebr-e_catalogue WHERE ebr-e_catalogue.RID = RECID(ebr-e_year)  NO-LOCK:
                     CREATE Ebr-Data_catalogue.
                     BUFFER-COPY ebr-e_catalogue TO Ebr-Data_catalogue.
                     Ebr-Data_catalogue.RRID = RECID(Ebr-Data_Year).
                     Ebr-Data_catalogue.year = Ebr-Data_Year.year. 
                     FOR EACH ebr-e_workcode WHERE ebr-e_workcode.RID = RECID(ebr-e_catalogue) NO-LOCK:
                        CREATE Ebr-Data_workcode.
                        BUFFER-COPY ebr-e_workcode TO Ebr-Data_workcode.
                        Ebr-Data_workcode.RRID = RECID(Ebr-Data_catalogue).
                        Ebr-Data_workcode.catalogue = Ebr-Data_catalogue.catalogue.
                        FOR EACH ebr-e_lop WHERE ebr-e_lop.RID = RECID(ebr-e_workcode) NO-LOCK:
                           CREATE Ebr-Data_lop.
                           BUFFER-COPY ebr-e_lop TO Ebr-Data_lop.
                           Ebr-Data_lop.RRID = RECID(Ebr-Data_workcode).
                        END.
                     END.
                  END.
               END.
            END.
         END.
         DATASET  Ebr-DataDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
         p2p3frekFile = p1p2frekFile.
         p1p2frekFile = p1p2frekFile + "costcatalogue" + listaP + "P1P2frek.xml".
         p2p3frekFile = p2p3frekFile + "costcatalogue" + listaP + "P2P3frek.xml".
         
         ekgp1frekvensTTh:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         utfrekfillong = REPLACE(utfrekfillong,'ekgp1frekvensTTRow','ekgp1frekvensTT').
         COPY-LOB FROM utfrekfillong TO FILE p1p2frekFile.
         ekgp2frekvensTTh:WRITE-XML("LONGCHAR", utfrekfillong,FALSE,"iso8859-1").
         utfrekfillong = REPLACE(utfrekfillong,'ekgp2frekvensTTRow','ekgp2frekvensTT').
         COPY-LOB FROM utfrekfillong TO FILE p2p3frekFile. 
         /*
         ekgp1frekvensTTh:WRITE-XML("FILE", p1p2frekFile).
         ekgp2frekvensTTh:WRITE-XML("FILE", p2p3frekFile).
         */ 
            
             */
         
                
      END.      
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
     
   END METHOD.
  
   METHOD PUBLIC VOID ExportEBR-Tryck(INPUT valdlistaP AS CHARACTER,jmfKontroll AS LOGICAL):
      
      /*Anders Olsson Elpool i Umeå AB  15 jan 2014 15:15:48 
       Kod med temptabeller och inte handle för tryck alla eller vald katalog P1-P3 alltid olika filer
       anmärkning med -"-, belopp = 0 ger -, decimaler enligt katalog
      ControlWinLg:ebrkatvar
      */
      DEFINE VARIABLE antaldec AS INTEGER NO-UNDO.
      DEFINE VARIABLE listaP AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:EbrXmlRensa().
      IF valdlistaP = "Alla" OR valdlistaP = "P1" THEN DO:
         THIS-OBJECT:EbrXmlRensa().
         THIS-OBJECT:EbrXmlStartTryck().
         listaP = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.    
         CREATE ebr-tryck_catalogue.
         ASSIGN
         ebr-tryck_catalogue.RID = RECID(ebr-tryck_year).
         ebr-tryck_catalogue.catalogue = "P1".
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = "G741" THEN. 
            ELSE DO:
               CREATE ebr-tryck_workcode.
               ASSIGN
               ebr-tryck_workcode.RID = RECID(ebr-tryck_catalogue)
               ebr-tryck_workcode.code = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE.
               CREATE ebr-tryck_lop.
               ASSIGN
               ebr-tryck_lop.RID = RECID(ebr-tryck_workcode).
               ebr-tryck_lop.code = ebr-tryck_workcode.code.
               ebr-tryck_lop.number = "0".
               ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
               
               query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1TTh:TABLE + " WHERE ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'".
               q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1TTh,query2var).
               q2H:GET-FIRST().
               DO WHILE q2H:QUERY-OFF-END = FALSE:
                  THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE ) NO-ERROR.
                  IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                     IF ebr-tryck_lop.number = "0" THEN DO:
                        ebr-tryck_lop.code = ebr-tryck_workcode.code.
                        ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
                     END.
                     ELSE DO:
                        CREATE ebr-tryck_lop.
                        ASSIGN
                        ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
                        ebr-tryck_lop.code = ebr-tryck_workcode.code
                        ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE,"99")
                        /*
                        ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                        */
                        ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
                     END.                
                  END.
                  IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" THEN antaldec = 0.
                  ELSE antaldec = 0.
                  CREATE ebr-tryck_lop.
                  ASSIGN
                  ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
                  ebr-tryck_lop.code = ebr-tryck_workcode.code
                  ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE,"99")
                  ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                  ebr-tryck_lop.unit = THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
                  
                  ebr-tryck_lop.bered_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.mont_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.maskin_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.utrust_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE,antaldec).
                  IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" THEN ebr-tryck_lop.ea_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE,0).
                  ELSE ebr-tryck_lop.ea_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE,1).
                  antaldec = 0.
                  DEBUGGER:SET-BREAK().
                  
                  
                  
                  
                  
                  ASSIGN
                  ebr-tryck_lop.arbete_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.material_cost = REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.maskin_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.ovrigt_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.summa_cost =    REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.utrust_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ").
                  IF THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
                  ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  
                  IF anmkop NE "" THEN DO:
                     IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-tryck_lop.comments = '-"-'.
                     ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  END.   
                  ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).             
                  q2H:GET-NEXT().         
               END.
            END.   
            qH:GET-NEXT().         
         END.
         THIS-OBJECT:EbrXmlutTryck(listaP,"-",jmfKontroll,"P1").
      END.
         
      IF valdlistaP = "Alla" OR valdlistaP = "P2" THEN DO:
         THIS-OBJECT:EbrXmlRensa().
         THIS-OBJECT:EbrXmlStartTryck().
         listaP = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
         CREATE ebr-tryck_catalogue.
         ASSIGN
         ebr-tryck_catalogue.RID = RECID(ebr-tryck_year).
         ebr-tryck_catalogue.catalogue = "P2".
    
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" AND THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE BEGINS "7" THEN. 
            ELSE DO:
               CREATE ebr-tryck_workcode.
               ASSIGN
               ebr-tryck_workcode.RID = RECID(ebr-tryck_catalogue)
               ebr-tryck_workcode.code = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE.
               CREATE ebr-tryck_lop.
               ASSIGN
               ebr-tryck_lop.RID = RECID(ebr-tryck_workcode).
               ebr-tryck_lop.code = ebr-tryck_workcode.code.
               ebr-tryck_lop.number = "0".
               ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
               
               query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2TTh:TABLE + " WHERE ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
               q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2TTh,query2var).
               q2H:GET-FIRST().
               DO WHILE q2H:QUERY-OFF-END = FALSE:
                  THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE ) NO-ERROR.
                  IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                     IF ebr-tryck_lop.number = "0" THEN DO:
                        ebr-tryck_lop.code = ebr-tryck_workcode.code.
                        ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
                     END.
                     ELSE DO:
                        CREATE ebr-tryck_lop.
                        ASSIGN
                        ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
                        ebr-tryck_lop.code = ebr-tryck_workcode.code
                        ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")
                        /*
                        ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                        */
                        ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
                     END.                
                  END.
                  IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" THEN antaldec = 2.
                  ELSE antaldec = 2.
                  CREATE ebr-tryck_lop.
                  ASSIGN
                  ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
                  ebr-tryck_lop.code = ebr-tryck_workcode.code
                  ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")
                  ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                  ebr-tryck_lop.unit = THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
                  ebr-tryck_lop.bered_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.mont_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.maskin_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.utrust_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE,antaldec)
                  ebr-tryck_lop.ea_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("EA"):BUFFER-VALUE,antaldec).
                  antaldec = 0.
               
                  ASSIGN
                  ebr-tryck_lop.arbete_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.material_cost = REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.maskin_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.ovrigt_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.summa_cost =    REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ")
                  ebr-tryck_lop.utrust_cost =   REPLACE(TRIM(STRING(THIS-OBJECT:ControlWinLg:KlgTryckAvKost(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE,antaldec),"->,>>>,>>>,>>>,>>9")),"."," ").
                  IF THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
                  /*
                  ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  IF anmkop NE "" THEN DO:
                     IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-tryck_lop.comments = '-"-'.
                     ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  END.   
                  ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                  */             
                  q2H:GET-NEXT().         
               END.
            END.   
            qH:GET-NEXT().         
         END. 
         THIS-OBJECT:EbrXmlutTryck(listaP,"-",jmfKontroll,"P2").
      END.      
      IF valdlistaP = "Alla" OR valdlistaP = "P3" THEN DO:
         THIS-OBJECT:EbrXmlRensa().
         THIS-OBJECT:EbrXmlStartTryck().
         listaP = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
         CREATE ebr-tryck_catalogue.
         ASSIGN
         ebr-tryck_catalogue.RID = RECID(ebr-tryck_year).
         ebr-tryck_catalogue.catalogue = "P3".
    
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            CREATE ebr-tryck_workcode.
            ASSIGN
            ebr-tryck_workcode.RID = RECID(ebr-tryck_catalogue)
            ebr-tryck_workcode.code = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE.
            CREATE ebr-tryck_lop.
            ASSIGN
            ebr-tryck_lop.RID = RECID(ebr-tryck_workcode).
            ebr-tryck_lop.code = ebr-tryck_workcode.code.
            ebr-tryck_lop.number = "0".
            ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
            
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3TTh:TABLE + " WHERE ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE ) NO-ERROR.
               IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                  IF ebr-tryck_lop.number = "0" THEN DO:
                     ebr-tryck_lop.code = ebr-tryck_workcode.code.
                     ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
                  END.
                  ELSE DO:
                     CREATE ebr-tryck_lop.
                     ASSIGN
                     ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
                     ebr-tryck_lop.code = ebr-tryck_workcode.code
                     ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,"99")
                     /*
                     ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
                     */
                     ebr-tryck_lop.comments = THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE. 
                  END.                
               END.
               IF THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = "KLG1" THEN antaldec = 2.
               ELSE antaldec = 2.
               CREATE ebr-tryck_lop.
               ASSIGN
               ebr-tryck_lop.RID = RECID(ebr-tryck_workcode)
               ebr-tryck_lop.code = ebr-tryck_workcode.code
               ebr-tryck_lop.number = STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,"99")
               ebr-tryck_lop.work_construction = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
               ebr-tryck_lop.unit = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE
               ebr-tryck_lop.mont_hours =   THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE,antaldec)
               ebr-tryck_lop.maskin_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE,antaldec)
               ebr-tryck_lop.ea_hours =     THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("EA"):BUFFER-VALUE,antaldec)
               ebr-tryck_lop.utrust_hours = THIS-OBJECT:ControlWinLg:KlgTryckAvTid(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE,antaldec)
               ebr-tryck_lop.count_men = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE
               ebr-tryck_lop.timebase = THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE.
               IF THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
               ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               
               IF anmkop NE "" THEN DO:
                  IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN ebr-tryck_lop.comments = '-"-'.
                  ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               END.   
               ELSE ebr-tryck_lop.comments = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).             
               
               q2H:GET-NEXT().         
            END.
            qH:GET-NEXT().         
         END. 
         THIS-OBJECT:EbrXmlutTryck(listaP,"-",jmfKontroll,"P3").
      END.
      /*Anders Olsson Elpool i Umeå AB  9 dec 2014 11:53:30 
      P2 Kommentarer 
      */
      IF valdlistaP = "qP2" THEN DO:
         THIS-OBJECT:EbrXmlRensa().
         THIS-OBJECT:EbrXmlStartTryck().
        
         listaP = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE.
         CREATE Ebr-Tryck_P2KomKatalog.
         ASSIGN 
         Ebr-Tryck_P2KomKatalog.KostnadsKatalog = ebr-tryck_costcatalogue.kostnadskatalog_header
         Ebr-Tryck_P2KomKatalog.Type = ebr-tryck_type.type 
         Ebr-Tryck_P2KomKatalog.Year = ebr-tryck_year.year.
         THIS-OBJECT:P2KomControl:SkapaKomResultat().
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgP2KomResultTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgP2KomResultTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            Ebr-Tryck_P2Komh:BUFFER-CREATE().
            Ebr-Tryck_P2Komh:BUFFER-COPY(THIS-OBJECT:ControlDataDb:ekgP2KomResultTTh).
            THIS-OBJECT:Ebr-Tryck_P2Komh:BUFFER-FIELD("YEAR"):BUFFER-VALUE = Ebr-Tryck_P2KomKatalog.Year.
            THIS-OBJECT:Ebr-Tryck_P2Komh:BUFFER-FIELD("RID"):BUFFER-VALUE = RECID(Ebr-Tryck_P2KomKatalog).
            qH:GET-NEXT().   
         END.   
         THIS-OBJECT:EbrXmlutTryckP2Kom(listaP).
         
      END.
   END METHOD.

   METHOD PUBLIC VOID EbrXmlRensa():
      THIS-OBJECT:ControlDataDb:jmffelTTh:EMPTY-TEMP-TABLE()  NO-ERROR. 
      EMPTY TEMP-TABLE ebr-e_costcatalogue NO-ERROR.      
      EMPTY TEMP-TABLE ebr-e_type NO-ERROR.        
      EMPTY TEMP-TABLE ebr-e_year NO-ERROR.       
      EMPTY TEMP-TABLE ebr-e_catalogue NO-ERROR.     
      EMPTY TEMP-TABLE ebr-e_workcode NO-ERROR.     
      EMPTY TEMP-TABLE ebr-e_lop NO-ERROR.
      
      EMPTY TEMP-TABLE ebr-tryck_costcatalogue NO-ERROR.      
      EMPTY TEMP-TABLE ebr-tryck_type NO-ERROR.        
      EMPTY TEMP-TABLE ebr-tryck_year NO-ERROR.       
      EMPTY TEMP-TABLE ebr-tryck_catalogue NO-ERROR.     
      EMPTY TEMP-TABLE ebr-tryck_workcode NO-ERROR.     
      EMPTY TEMP-TABLE ebr-tryck_lop NO-ERROR.
      {Ebr-DataRensa.i} 
      EMPTY TEMP-TABLE Ebr-Data_FrekP1P2Katalog NO-ERROR.  
      EMPTY TEMP-TABLE Ebr-Data_FrekP2P3Katalog NO-ERROR.  
      EMPTY TEMP-TABLE Ebr-Data_P1P2Frekvens NO-ERROR.           
      EMPTY TEMP-TABLE Ebr-Data_P2P3Frekvens NO-ERROR.           

      EMPTY TEMP-TABLE Ebr-Data_P2KomKatalog NO-ERROR.  
      EMPTY TEMP-TABLE Ebr-Data_P2Kom NO-ERROR.  
      
      EMPTY TEMP-TABLE Ebr-Tryck_P2KomKatalog NO-ERROR.
      EMPTY TEMP-TABLE Ebr-Tryck_P2Kom NO-ERROR.  
   END METHOD.   
    
   METHOD PUBLIC VOID EbrXmlStart():
      THIS-OBJECT:ControlDataDb:ekgsubkatTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(THIS-OBJECT:ControlWinLg:ekgsubidvar)) NO-ERROR.
      CREATE ebr-e_costcatalogue.
      ebr-e_costcatalogue.kostnadskatalog_header = SUBSTRING(THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE,1,3).
      CREATE ebr-e_type.
      ASSIGN 
      ebr-e_type.RID = RECID(ebr-e_costcatalogue)
      ebr-e_type.type = INTEGER(SUBSTRING(THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE,4,1)).
      CREATE ebr-e_year.
      ASSIGN
      ebr-e_year.RID = RECID(ebr-e_type)
      ebr-e_year.year = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE. 
   END METHOD.
   METHOD PUBLIC VOID EbrXmlStartTryck():
      THIS-OBJECT:ControlDataDb:ekgsubkatTTh:FIND-FIRST("WHERE EKGSUBID = " + STRING(THIS-OBJECT:ControlWinLg:ekgsubidvar)) NO-ERROR.
      CREATE ebr-tryck_costcatalogue.
      ebr-tryck_costcatalogue.kostnadskatalog_header = SUBSTRING(THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE,1,3).
      CREATE ebr-tryck_type.
      ASSIGN 
      ebr-tryck_type.RID = RECID(ebr-tryck_costcatalogue).
      ebr-tryck_type.type = INTEGER(SUBSTRING(THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE,4,1)).
   
      CREATE ebr-tryck_year.
      ASSIGN
      ebr-tryck_year.RID = RECID(ebr-tryck_type).
      ebr-tryck_year.year = THIS-OBJECT:ControlDataDb:ekgsubkatTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE. 
      
   END METHOD. 
   METHOD PUBLIC VOID EbrXmlutTryckP2kom(INPUT listaP AS CHARACTER):
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cEncoding  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      cFile = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      ASSIGN
      cEncoding = "iso8859-1"
      cFile = cFile + "costcatalogue" + listaP + "P2Kommentarer" + ".xml".
      DATASET  Ebr-TryckP2KomDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
      COPY-LOB FROM utfillong TO FILE cFile.
      res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + cFile," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
   END METHOD.
   METHOD PUBLIC VOID EbrXmlutTryck(INPUT listaP AS CHARACTER, INPUT nollut AS CHARACTER, jmfKontroll AS LOGICAL,nivavar AS CHARACTER):
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cEncoding  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      cFile = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      ASSIGN
      cEncoding = "iso8859-1"
      cFile = cFile + "costcatalogue" + listaP + ebr-tryck_catalogue.catalogue + ".xml".
      
      /*
      omit-initial = TRUE.
      */   
           
      DATASET  EBR-TryckDS:WRITE-XML("LONGCHAR", utfillong,FALSE,"iso8859-1").
      
      IF jmfKontroll = TRUE THEN DO:
         IF nivavar = "P1" THEN  EbrTP1long = utfillong.
         IF nivavar = "P2" THEN  EbrTP2long = utfillong.
         IF nivavar = "P3" THEN  EbrTP3long = utfillong.
         RETURN.
      END.   
      FIND FIRST ebr-tryck_catalogue WHERE NO-LOCK NO-ERROR.
     
      utfillong = REPLACE(utfillong,'<work_construction/>','').
      IF ebr-tryck_catalogue.catalogue = "P3" THEN. 
      ELSE utfillong = REPLACE(utfillong,'<comments/>','').
      utfillong = REPLACE(utfillong,'<number>0</number>','').
      utfillong = REPLACE(utfillong,'<unit/>','').
      utfillong = REPLACE(utfillong,'<bered_hours/>','').
      utfillong = REPLACE(utfillong,'<mont_hours/>','').
      utfillong = REPLACE(utfillong,'<maskin_hours/>','').
      utfillong = REPLACE(utfillong,'<utrust_hours/>','').
      utfillong = REPLACE(utfillong,'<ea_hours/>','').
      utfillong = REPLACE(utfillong,'<arbete_cost/>','').
      utfillong = REPLACE(utfillong,'<material_cost/>','').
      utfillong = REPLACE(utfillong,'<maskin_cost/>','').
      utfillong = REPLACE(utfillong,'<utrust_cost/>','').
      IF listaP = "KLG1" THEN DO:
       /*
         DEBUGGER:SET-BREAK().
         IF INDEX(utfillong,'<utrust_cost> 0</utrust_cost>') = 0 THEN 
         MESSAGE "MELLAN 0"
         VIEW-AS ALERT-BOX.
         IF INDEX(utfillong,'<utrust_cost>_cost>') = 0 THEN 
         MESSAGE "UTAN MELLAN "
         VIEW-AS ALERT-BOX.
         utfillong = REPLACE(utfillong,'<utrust_cost>','').
         */                               
      END.   
      utfillong = REPLACE(utfillong,'<ovrigt_cost/>','').
      utfillong = REPLACE(utfillong,'<summa_cost/>','').
        
      /*utfillong = REPLACE(utfillong,'>.','>0.').*/
      utfillong = REPLACE(utfillong,'<bered_hours>0</bered_hours>','<bered_hours>' + nollut + '</bered_hours>').
      utfillong = REPLACE(utfillong,'<bered_hours>0,0</bered_hours>','<bered_hours>' + nollut + '</bered_hours>').
      utfillong = REPLACE(utfillong,'<bered_hours>0,00</bered_hours>','<bered_hours>' + nollut + '</bered_hours>').
      utfillong = REPLACE(utfillong,'<mont_hours>0</mont_hours>','<mont_hours>' + nollut + '</mont_hours>').
      utfillong = REPLACE(utfillong,'<mont_hours>0,0</mont_hours>','<mont_hours>' + nollut + '</mont_hours>').
      utfillong = REPLACE(utfillong,'<mont_hours>0,00</mont_hours>','<mont_hours>' + nollut + '</mont_hours>').
      utfillong = REPLACE(utfillong,'<maskin_hours>0</maskin_hours>','<maskin_hours>' + nollut + '</maskin_hours>').
      utfillong = REPLACE(utfillong,'<maskin_hours>0,0</maskin_hours>','<maskin_hours>' + nollut + '</maskin_hours>').
      utfillong = REPLACE(utfillong,'<maskin_hours>0,00</maskin_hours>','<maskin_hours>' + nollut + '</maskin_hours>').
      utfillong = REPLACE(utfillong,'<ea_hours>0</ea_hours>','<ea_hours>' + nollut + '</ea_hours>').
      utfillong = REPLACE(utfillong,'<ea_hours>0,0</ea_hours>','<ea_hours>' + nollut + '</ea_hours>').
      utfillong = REPLACE(utfillong,'<ea_hours>0,00</ea_hours>','<ea_hours>' + nollut + '</ea_hours>').
         
      IF listaP = "KLG1" THEN DO:
         utfillong = REPLACE(utfillong,'<utrust_hours>0</utrust_hours>','').
         utfillong = REPLACE(utfillong,'<utrust_hours>0,0</utrust_hours>','').
         utfillong = REPLACE(utfillong,'<utrust_hours>0,00</utrust_hours>','').
      END.
      ELSE DO:
         utfillong = REPLACE(utfillong,'<utrust_hours>0</utrust_hours>','<utrust_hours>' + nollut + '</utrust_hours>').
         utfillong = REPLACE(utfillong,'<utrust_hours>0,0</utrust_hours>','<utrust_hours>' + nollut + '</utrust_hours>').
         utfillong = REPLACE(utfillong,'<utrust_hours>0,00</utrust_hours>','<utrust_hours>' + nollut + '</utrust_hours>').
      END.   
      IF ebr-tryck_catalogue.catalogue = "P3" THEN DO:
         utfillong = REPLACE(utfillong,'<arbete_cost>0</arbete_cost>','').
         utfillong = REPLACE(utfillong,'<arbete_cost>0,0</arbete_cost>','').
         utfillong = REPLACE(utfillong,'<arbete_cost>0,00</arbete_cost>','').
         utfillong = REPLACE(utfillong,'<material_cost>0</material_cost>','').
         utfillong = REPLACE(utfillong,'<material_cost>0,0</material_cost>','').
         utfillong = REPLACE(utfillong,'<material_cost>0,00</material_cost>','').
         utfillong = REPLACE(utfillong,'<maskin_cost>0</maskin_cost>','').
         utfillong = REPLACE(utfillong,'<maskin_cost>0,0</maskin_cost>','').
         utfillong = REPLACE(utfillong,'<maskin_cost>0,00</maskin_cost>','').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0</ovrigt_cost>','').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0,0</ovrigt_cost>','').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0,00</ovrigt_cost>','').
         utfillong = REPLACE(utfillong,'<summa_cost>0</summa_cost>','').
         utfillong = REPLACE(utfillong,'<summa_cost>0,0</summa_cost>','').
         utfillong = REPLACE(utfillong,'<summa_cost>0,00</summa_cost>','').
      END.
      ELSE DO:
         utfillong = REPLACE(utfillong,'<arbete_cost>0</arbete_cost>','<arbete_cost>' + nollut + '</arbete_cost>').
         utfillong = REPLACE(utfillong,'<arbete_cost>0,0</arbete_cost>','<arbete_cost>' + nollut + '</arbete_cost>').
         utfillong = REPLACE(utfillong,'<arbete_cost>0,00</arbete_cost>','<arbete_cost>' + nollut + '</arbete_cost>').
         utfillong = REPLACE(utfillong,'<arbete_cost xsi:nil="true"/>','<arbete_cost>' + nollut + '</arbete_cost>').
         utfillong = REPLACE(utfillong,'<material_cost>0</material_cost>','<material_cost>' + nollut + '</material_cost>').
         utfillong = REPLACE(utfillong,'<material_cost>0,0</material_cost>','<material_cost>' + nollut + '</material_cost>').
         utfillong = REPLACE(utfillong,'<material_cost>0,00</material_cost>','<material_cost>' + nollut + '</material_cost>').
         utfillong = REPLACE(utfillong,'<material_cost xsi:nil="true"/>','<material_cost>' + nollut + '</material_cost>').
         utfillong = REPLACE(utfillong,'<maskin_cost>0</maskin_cost>','<maskin_cost>' + nollut + '</maskin_cost>').
         utfillong = REPLACE(utfillong,'<maskin_cost>0,0</maskin_cost>','<maskin_cost>' + nollut + '</maskin_cost>').
         utfillong = REPLACE(utfillong,'<maskin_cost>0,00</maskin_cost>','<maskin_cost>' + nollut + '</maskin_cost>').
         utfillong = REPLACE(utfillong,'<maskin_cost xsi:nil="true"/>','<maskin_cost>' + nollut + '</maskin_cost>').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0</ovrigt_cost>','<ovrigt_cost>' + nollut + '</ovrigt_cost>').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0,0</ovrigt_cost>','<ovrigt_cost>' + nollut + '</ovrigt_cost>').
         utfillong = REPLACE(utfillong,'<ovrigt_cost>0,00</ovrigt_cost>','<ovrigt_cost>' + nollut + '</ovrigt_cost>').
         utfillong = REPLACE(utfillong,'<ovrigt_cost xsi:nil="true"/>','<ovrigt_cost>' + nollut + '</ovrigt_cost>').
         utfillong = REPLACE(utfillong,'<summa_cost>0</summa_cost>','<summa_cost>' + nollut + '</summa_cost>').
         utfillong = REPLACE(utfillong,'<summa_cost>0,0</summa_cost>','<summa_cost>' + nollut + '</summa_cost>').
         utfillong = REPLACE(utfillong,'<summa_cost>0,00</summa_cost>','<summa_cost>' + nollut + '</summa_cost>').
         utfillong = REPLACE(utfillong,'<summa_cost xsi:nil="true"/>','<summa_cost>' + nollut + '</summa_cost>').
      END.    
      IF listaP = "KLG1" OR ebr-tryck_catalogue.catalogue = "P3" THEN DO:
         utfillong = REPLACE(utfillong,'<utrust_cost>0</utrust_cost>','').
         utfillong = REPLACE(utfillong,'<utrust_cost>0,0</utrust_cost>','').
         utfillong = REPLACE(utfillong,'<utrust_cost>0,00</utrust_cost>','').
      END.
      ELSE DO:
         utfillong = REPLACE(utfillong,'<utrust_cost>0</utrust_cost>','<utrust_cost>' + nollut + '</utrust_cost>').
         utfillong = REPLACE(utfillong,'<utrust_cost>0,0</utrust_cost>','<utrust_cost>' + nollut + '</utrust_cost>').
         utfillong = REPLACE(utfillong,'<utrust_cost>0,00</utrust_cost>','<utrust_cost>' + nollut + '</utrust_cost>').
         utfillong = REPLACE(utfillong,'<utrust_cost xsi:nil="true"/>','<utrust_cost>' + nollut + '</utrust_cost>').
      END.   
      
           
      IF ebr-tryck_catalogue.catalogue = "P3" THEN utfillong = REPLACE(utfillong,'<count_men>0</count_men>','<count_men>' + nollut + '</count_men>').
      ELSE utfillong = REPLACE(utfillong,'<count_men>0</count_men>','').
      
      utfillong = REPLACE(utfillong,'<count_men/>','').
      utfillong = REPLACE(utfillong,'<timebase/>','').
      utfillong = REPLACE(utfillong,'<work_construction xsi:nil="true"/>','').
      utfillong = REPLACE(utfillong,'<timebase xsi:nil="true"/>','').
      utfillong = REPLACE(utfillong,'<unit xsi:nil="true"/>','').
      
      
      
      COPY-LOB FROM utfillong TO FILE cFile.
      
      res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(157) + cFile," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).   
     
   END METHOD.
  
    
   METHOD PUBLIC VOID ExportKLGData(INPUT listaP AS CHARACTER):
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE blad AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:ControlWinLg:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:ControlWinLg:VisaExcel:OutExterntWin = FALSE.
      
      IF THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE THEN  THIS-OBJECT:ControlWinLg:VisaExcel:StartExcelVal(INPUT FALSE).
      THIS-OBJECT:ControlWinLg:VisaExcel:bladvarVersion().
      THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(FALSE).
      Guru.Konstanter:AmericanSet().
      IF listaP = "KLG1" THEN DO:
         REPEAT:
            blad = blad + 1.         
            THIS-OBJECT:ControlWinLg:VisaExcel:NyttBladExcel().
            IF blad = 6 THEN LEAVE.
         END.   
       
         THIS-OBJECT:ExportKLGDataKoder(INPUT listaP + "P1").
         THIS-OBJECT:ExportKLGDataFrek(listaP + "P1F").
         THIS-OBJECT:ExportKLGDataKoder(listaP + "P2").
         THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(4).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P2 Kommentarer").
         THIS-OBJECT:ExportKLGDataMtrl(listaP + "P2M").
       
         THIS-OBJECT:ExportKLGDataKoder(INPUT listaP + "P3").
      END.
      IF listaP = "KLG2" THEN DO:
         THIS-OBJECT:ExportKLGDataKoder(INPUT listaP + "P1").
         THIS-OBJECT:ExportKLGDataKoder(listaP + "P2").
         THIS-OBJECT:ExportKLGDataKoder(INPUT listaP + "P3").
      END.
      THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(TRUE).  
      THIS-OBJECT:ControlWinLg:VisaExcel:SlutExcel(1).
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   METHOD PUBLIC VOID ExportKLGDataFrek(INPUT listaP AS CHARACTER): 
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      IF listaP = "KLG1P1F" THEN DO:
         THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(2).
         THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P1P2frekv").
         THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),12.14). 
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),47).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),75.14).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
          /*ut rubriker*/
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","P1kod").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","P1.Arbete").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","P2kod").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","P1.Arbete"). 
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Antal").
         THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight(). 
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P1BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight(). 
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            qH:GET-NEXT().         
         END.
                  
         THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E2:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").    
      END. 
      THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad)).  
   END METHOD.   
   METHOD PUBLIC VOID ExportKLGDataMtrl(INPUT listaP AS CHARACTER): 
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      IF listaP = "KLG1P2M" THEN DO:
         THIS-OBJECT:ControlDataDb:laddaP2MtrlForExport().
         THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(5).
         THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P2 Materiel").
         THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43). 
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),40.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),64.71).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
        
         /*ut rubriker*/
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","P2löpnr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Arbete").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Benämning").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Antal"). 
        
         THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,"99")).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight(). 
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("P2BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("MTRLBENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            qH:GET-NEXT().         
         END.    
      END.  
      THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("D2:" + "D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
      THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad)).
   END METHOD.  
   METHOD PUBLIC VOID ExportKLGTryckStart(INPUT listaP AS CHARACTER):
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE blad AS INTEGER NO-UNDO.
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      p1blad = 0.
      p2blad = 0.
      p3blad = 0.
      THIS-OBJECT:ControlWinLg:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE.
      THIS-OBJECT:ControlWinLg:VisaExcel:OutExterntWin = FALSE.
        
     
      IF THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE THEN  THIS-OBJECT:ControlWinLg:VisaExcel:StartExcelVal(INPUT FALSE).
      THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(TRUE).
      Guru.Konstanter:AmericanSet().
      IF listaP = "KLG1" THEN DO:
         THIS-OBJECT:ExportKLGTryck(INPUT listaP + "P1").
         THIS-OBJECT:ExportKLGTryck(INPUT listaP + "P2").
         THIS-OBJECT:ExportKLGTryck(INPUT listaP + "P3").
      END.
      IF listaP = "KLG2" THEN DO:
         THIS-OBJECT:ExportKLGTryck(INPUT listaP + "P1").
      END.
      THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(TRUE).  
      THIS-OBJECT:ControlWinLg:VisaExcel:SlutExcel(1).
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   METHOD PUBLIC VOID ExportKLGDataKoder(INPUT listaP AS CHARACTER): 
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
     DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
     DEFINE VARIABLE path AS CHARACTER NO-UNDO.
     IF listaP = "KLG1P1" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P1").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),9).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),73.43).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Kostnad i kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M1","Summa" + Guru.Konstanter:globradbrytch + "kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N1","Kommentar").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","Arbete").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2","Materiel").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2","Övrigt").
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:H1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:E2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F2:F2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:G2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H2:H2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:L1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M1:M2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("N1:N2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","L1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","N2","4:7").        
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A1" ,"N1","",0,TRUE ,15,0 ).
        
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           
           IF THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE = "G741" THEN. 
           ELSE DO:              
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
              THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            
              query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1TTh:TABLE + " WHERE ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'".
              q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1TTh,query2var).
              q2H:GET-FIRST().
              DO WHILE q2H:QUERY-OFF-END = FALSE:
                 THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE ) NO-ERROR.
                 IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                    THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                    THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
                    THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().                
                 END.
                 
                 THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 IF THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
                 IF anmkop NE "" THEN DO:
                    IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN THIS-OBJECT:ControlWinLg:VisaExcel:DataOut('-"-').
                    ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
                 END.   
                 ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
                 anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
                 
                 THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
                 
                 q2H:GET-NEXT().         
              END.
           END.           
           qH:GET-NEXT().         
        END. 
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "O" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.

        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("H4:" + "H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("I4:" + "I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("J4:" + "J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("K4:" + "K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("L4:" + "L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("M4:" + "M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
     END.     
     IF listaP = "KLG1P2" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(3).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P2").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.57). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),59.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),4.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10.29).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A2","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B2","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C2","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D2","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","Kostnad i kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2","Summa" + Guru.Konstanter:globradbrytch + "kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E3","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F3","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G3","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H3","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I3","Arbete").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J3","Materiel").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K3","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L3","Övrigt").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A2:A3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B2:B3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C2:C3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D2:D3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:H2",3).        
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E3:E3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F3:F3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G3:G3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H3:H3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I3:I3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J3:J3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K3:K3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("L3:L3",3).                
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I2:L2",3).        
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M2:M3",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","M1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E2","L2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A3","M3","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A2" ,"M2","",0,TRUE ,15,0 ).
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           
           IF THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE BEGINS "7" THEN. 
           ELSE DO:
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
              THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2TTh:TABLE + " WHERE ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
              q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2TTh,query2var).
              q2H:GET-FIRST().
              DO WHILE q2H:QUERY-OFF-END = FALSE:
                 THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                 THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
                 THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
                 q2H:GET-NEXT().         
              END.
           END.   
           THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
           qH:GET-NEXT().         
        END. 
        
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "N" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "2",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("H4:" + "H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("I4:" + "I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("J4:" + "J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("K4:" + "K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("L4:" + "L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("M4:" + "M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
     END.
     IF listaP = "KLG1P3" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(6).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P3").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.57). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),46.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),55.14).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Enhet"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H1","Antal" + Guru.Konstanter:globradbrytch + "man").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Kommentar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J1","Tids-" + Guru.Konstanter:globradbrytch + "underlag").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:G1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:E2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F2:F2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:G2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H1:H2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:I2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:J2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","G1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","J2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A1" ,"J1","",0,TRUE ,15,0 ).
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3TTh:TABLE + " WHERE ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3TTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.  
           THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").      
           qH:GET-NEXT().         
        END. 
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        
     END.
     IF listaP = "KLG2P1" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P1").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.86). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),47.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),9.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("P" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),57.14).
         /*ut rubriker*/
        
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J1","Kostnad i kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("O1","Summa" + Guru.Konstanter:globradbrytch + "kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("P1","Kommentar").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2","Utrust").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2","Arbete").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2","Materiel").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2","Utrust").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N2","Övrigt").
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:I1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:E2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F2:F2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:G2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H2:H2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I2:I2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J2:J2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K2:K2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("L2:L2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M2:M2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("N2:N2",3).
                
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:N1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("O1:O2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("P1:P2",2).
      
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","N1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","P2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A1" ,"P1","",0,TRUE ,15,0 ).
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1TTh:TABLE + " WHERE ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1TTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE,">>99")).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              IF THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
              IF anmkop NE "" THEN DO:
                 IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN THIS-OBJECT:ControlWinLg:VisaExcel:DataOut('-"-').
                 ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
              END.   
              ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
              anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"P" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
           qH:GET-NEXT().         
        END. 
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "Q" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("H4:" + "H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("I4:" + "I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("J4:" + "J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("K4:" + "K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("L4:" + "L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("M4:" + "M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("N4:" + "N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("O4:" + "O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
     END.
     IF listaP = "KLG2P2" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(2).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P2").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.57). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),62.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),4.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.29).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),9.43).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J1","Kostnad i kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("O1","Summa" + Guru.Konstanter:globradbrytch + "kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2","Utrust").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2","Arbete").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2","Materiel").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2","Utrust").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N2","Övrigt").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:I1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:E2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F2:F2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:G2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H2:H2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I2:I2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J2:J2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K2:K2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("L2:L2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M2:M2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("N2:N2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:N1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("O1:O2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","N1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","O2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A1" ,"M1","",0,TRUE ,15,0 ).
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2TTh:TABLE + " WHERE ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2TTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE,">>99")).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("UTRUSTKOST"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
           qH:GET-NEXT().         
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "P" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("H4:" + "H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("I4:" + "I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("J4:" + "J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("K4:" + "K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("L4:" + "L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("M4:" + "M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("N4:" + "N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("O4:" + "O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0").
     END.
     IF listaP = "KLG2P3" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(3).
        THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P3").
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.57). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),63.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.0).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),40.86).
         /*ut rubriker*/
         
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Enhet"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Antal" + Guru.Konstanter:globradbrytch + "man").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J1","Kommentar").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Utrust").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:H1",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:E2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F2:F2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:G2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H2:H2",3).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:I2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:J2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","H1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","J2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A1" ,"J1","",0,TRUE ,15,0 ).
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:TABLE.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3TTh:TABLE + " WHERE ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3TTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE,">>99")).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("UTRUSTTIM"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.  
           THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").      
           qH:GET-NEXT().         
        END. 
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("E4:" + "E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("F4:" + "F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("G4:" + "G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").
        THIS-OBJECT:ControlWinLg:VisaExcel:KolumnFormat("H4:" + "H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"0,00").        
     END.   
     THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad)). 
  END METHOD.
   METHOD PUBLIC VOID ExportKLGTryck(INPUT listaP AS CHARACTER): 
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
      DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      IF listaP = "KLG1P1" THEN DO:
         THIS-OBJECT:NyTryckSidaExcel(listaP).
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp1TTh:TABLE + " WHERE ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp1TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE ) NO-ERROR.
               IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                  THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
               END.
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               IF THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
               IF anmkop NE "" THEN DO:
                  IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN THIS-OBJECT:ControlWinLg:VisaExcel:DataOut('-"-').
                  ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
               END.   
               ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
               anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgp1TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              
               IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 40 THEN DO:
                  THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                  REPEAT:
                     THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                     IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "O" THEN LEAVE.
                     THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
                  END.
                  THIS-OBJECT:NyTryckSidaExcel(listaP).
                  THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
                  IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                     THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                     THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
                     THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
                  END.
               END.
               q2H:GET-NEXT().        
            END.
            IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad > 35 THEN DO:
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
               REPEAT:
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "O" THEN LEAVE.
                  THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
               END.
               THIS-OBJECT:NyTryckSidaExcel(listaP).  
            END.
            qH:GET-NEXT().    
         END. 
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         REPEAT:
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "O" THEN LEAVE.
            THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
         END.  
      END.
        
      IF listaP = "KLG1P2" THEN DO:
         THIS-OBJECT:NyTryckSidaExcel(listaP).
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp2TTh:TABLE + " WHERE ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp2TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp2TTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
               IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 40 THEN DO:
                  REPEAT:
                     THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                     IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "N" THEN LEAVE.
                     THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "2",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
                  END. 
                  THIS-OBJECT:NyTryckSidaExcel(listaP).
                  THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
               END.
               q2H:GET-NEXT().         
            END.
            THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
            IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad > 35 THEN DO:
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
               REPEAT:
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "N" THEN LEAVE.
                  THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "2",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
               END.
               THIS-OBJECT:NyTryckSidaExcel(listaP).  
            END.
            qH:GET-NEXT().
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            REPEAT:
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "N" THEN LEAVE.
               THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "2",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
            END.         
         END.        
      END.
     
      IF listaP = "KLG1P3" THEN DO:
         THIS-OBJECT:NyTryckSidaExcel(listaP).
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
            THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
            query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgp3TTh:TABLE + " WHERE ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'".
            q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgp3TTh,query2var).
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().   
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
               THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgp3TTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE)).
               THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
               IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 55 THEN DO:
                  REPEAT:
                     THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                     IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
                     THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
                  END.
                  THIS-OBJECT:NyTryckSidaExcel(listaP).
                  THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgp3arbkodTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                  THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
               END.
               q2H:GET-NEXT().         
            END.  
            THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
            IF THIS-OBJECT:ControlWinLg:VisaExcel:iRad > 50 THEN DO:
               THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
               THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
               REPEAT:
                  THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
                  IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
                  THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
               END.
               THIS-OBJECT:NyTryckSidaExcel(listaP).  
            END.    
            qH:GET-NEXT().         
         END. 
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         REPEAT:
            THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
            IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
            THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
         END.
      END.   
   END METHOD.
   
    METHOD PUBLIC VOID KontrollExport():
      THIS-OBJECT:EbrXmlRensa().
      DATASET Ebr-DataDS:READ-XML ("LONGCHAR", EbrDlong, "MERGE", ?, FALSE).
      DATASET EBR-eDS:READ-XML ("LONGCHAR", EbrElong, "MERGE", ?, FALSE).
      DATASET EBR-TryckDS:READ-XML ("LONGCHAR", EbrTP1long, "MERGE", ?, FALSE).
      DATASET EBR-TryckDS:READ-XML ("LONGCHAR", EbrTP2long, "MERGE", ?, FALSE).
      DATASET EBR-TryckDS:READ-XML ("LONGCHAR", EbrTP3long, "MERGE", ?, FALSE).
     
      
      /*
       DATASET EBR-eDS:WRITE-XML("file", "c:\protemp11\Eds.xml",FALSE,"iso8859-1").
      DATASET  Ebr-DataDS:WRITE-XML("file", "c:\protemp11\Dds.xml",FALSE,"iso8859-1").
      DATASET  EBR-eDS:WRITE-XML("file", "c:\protemp11\Eds.xml",FALSE,"iso8859-1").
      DATASET  EBR-TryckDS:WRITE-XML("file", "c:\protemp11\TP1ds.xml",FALSE,"iso8859-1").
      DATASET  EBR-TryckDS:WRITE-XML("file", "c:\protemp11\TP2ds.xml",FALSE,"iso8859-1").
      DATASET  EBR-TryckDS:WRITE-XML("file", "c:\protemp11\TP3ds.xml",FALSE,"iso8859-1").
      */
      
      FOR EACH ebr-e_catalogue WHERE NO-LOCK:
         FOR EACH ebr-e_workcode  WHERE ebr-e_workcode.RID = RECID(ebr-e_catalogue)  NO-LOCK:
            FOR EACH ebr-e_lop WHERE ebr-e_lop.RID = RECID(ebr-e_workcode) NO-LOCK:
               ASSIGN 
               ebr-e_lop.Level = ebr-e_catalogue.catalogue
               ebr-e_lop.code = ebr-e_workcode.code.
            END.
         END.
      END.
      
      FOR EACH ebr-tryck_catalogue WHERE NO-LOCK:
         FOR EACH ebr-tryck_workcode  WHERE ebr-tryck_workcode.RID = RECID(ebr-tryck_catalogue)  NO-LOCK:
            FOR EACH ebr-tryck_lop WHERE ebr-tryck_lop.RID = RECID(ebr-tryck_workcode) NO-LOCK:
               ASSIGN 
               ebr-tryck_lop.Level = ebr-tryck_catalogue.catalogue
               ebr-tryck_lop.code = ebr-tryck_workcode.code.
            END.
         END.
      END.
      
      
      FOR EACH ebr-e_lop  WHERE ebr-e_lop.unit = "" NO-LOCK:
         DELETE ebr-e_lop.
      END. 
      FOR EACH ebr-tryck_lop  WHERE ebr-tryck_lop.unit = "" NO-LOCK:
         DELETE ebr-tryck_lop.
      END.
      FOR EACH Ebr-Data_lop WHERE NO-LOCK:
          IF INTEGER(Ebr-Data_lop.Lopnummer) = 0 THEN NEXT.
          FIND FIRST ebr-e_lop WHERE ebr-e_lop.Level = Ebr-Data_lop.Level AND ebr-e_lop.code = Ebr-Data_lop.ArbetsKod AND ebr-e_lop.number = Ebr-Data_lop.Lopnummer NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ebr-e_lop THEN DO:
             THIS-OBJECT:SkapjmffelTT("Ebr-E","Kod saknas").
          END.
          ELSE DO:
             IF Ebr-Data_lop.Bered_Tid      NE   ebr-e_lop.bered_hours      THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Beredartiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Bered_Tid     - ebr-e_lop.bered_hours)).
             IF Ebr-Data_lop.Mont_Tid       NE   ebr-e_lop.mont_hours       THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Montörstiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Mont_Tid      - ebr-e_lop.mont_hours   )).    
             IF Ebr-Data_lop.Maskin_Tid     NE   ebr-e_lop.maskin_hours     THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Maskintiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Maskin_Tid    - ebr-e_lop.maskin_hours )).  
             IF Ebr-Data_lop.Utrust_Tid     NE   ebr-e_lop.utrust_hours     THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Utrustningstiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Utrust_Tid    - ebr-e_lop.utrust_hours )).  
             IF Ebr-Data_lop.Ea_Tid         NE   ebr-e_lop.ea_hours         THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Ea-mängden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Ea_Tid        - ebr-e_lop.ea_hours     )).  
             IF Ebr-Data_lop.Arbete_Kost    NE   ebr-e_lop.arbete_cost      THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Arbetskostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Arbete_Kost   - ebr-e_lop.arbete_cost  )).  
             IF Ebr-Data_lop.Material_Kost  NE   ebr-e_lop.material_cost    THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Materielkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Material_Kost - ebr-e_lop.material_cost)).  
             IF Ebr-Data_lop.Maskin_Kost    NE   ebr-e_lop.maskin_cost      THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Maskinkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Maskin_Kost   - ebr-e_lop.maskin_cost  )).  
             IF Ebr-Data_lop.Utrust_Kost    NE   ebr-e_lop.utrust_cost      THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Utrustningskostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Utrust_Kost   - ebr-e_lop.utrust_cost  )).  
             IF Ebr-Data_lop.Ovrigt_Kost    NE   ebr-e_lop.ovrigt_cost      THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Övrigkostnad stämmer inte! Diff = " + STRING(Ebr-Data_lop.Ovrigt_Kost   - ebr-e_lop.ovrigt_cost  )).  
             IF Ebr-Data_lop.Summa_Kost     NE   ebr-e_lop.summa_cost       THEN THIS-OBJECT:SkapjmffelTT("Ebr-E","Totalkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Summa_Kost    - ebr-e_lop.summa_cost   )).  
          END.   
          FIND FIRST ebr-tryck_lop WHERE   ebr-tryck_lop.Level = Ebr-Data_lop.Level AND  ebr-tryck_lop.code = Ebr-Data_lop.ArbetsKod AND ebr-tryck_lop.number = Ebr-Data_lop.Lopnummer NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ebr-tryck_lop THEN DO:
             THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Kod saknas").
          END.
          ELSE DO:
             /*
             IF Ebr-Data_lop.Bered_Tid      NE   DECIMAL(ebr-tryck_lop.bered_hours)  AND ABSOLUTE(Ebr-Data_lop.Bered_Tid     - DECIMAL(ebr-tryck_lop.bered_hours))   > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Beredartiden stämmer inte! Diff = "         + STRING(Ebr-Data_lop.Bered_Tid     - DECIMAL(ebr-tryck_lop.bered_hours))).
             IF Ebr-Data_lop.Mont_Tid       NE   DECIMAL(ebr-tryck_lop.mont_hours)   AND ABSOLUTE(Ebr-Data_lop.Mont_Tid      - DECIMAL(ebr-tryck_lop.mont_hours))    > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Montörstiden stämmer inte! Diff = "         + STRING(Ebr-Data_lop.Mont_Tid       - DECIMAL(ebr-tryck_lop.mont_hours))).    
             IF Ebr-Data_lop.Maskin_Tid     NE   DECIMAL(ebr-tryck_lop.maskin_hours) AND ABSOLUTE(Ebr-Data_lop.Maskin_Tid    - DECIMAL(ebr-tryck_lop.maskin_hours))  > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Maskintiden stämmer inte! Diff = "          + STRING(Ebr-Data_lop.Maskin_Tid     - DECIMAL(ebr-tryck_lop.maskin_hours))).  
             IF Ebr-Data_lop.Utrust_Tid     NE   DECIMAL(ebr-tryck_lop.utrust_hours) AND ABSOLUTE(Ebr-Data_lop.Utrust_Tid    - DECIMAL(ebr-tryck_lop.utrust_hours))  > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Utrustningstiden stämmer inte! Diff = "     + STRING(Ebr-Data_lop.Utrust_Tid     - DECIMAL(ebr-tryck_lop.utrust_hours))).  
             IF Ebr-Data_lop.Ea_Tid         NE   DECIMAL(ebr-tryck_lop.ea_hours)     AND ABSOLUTE(Ebr-Data_lop.Ea_Tid        - DECIMAL(ebr-tryck_lop.ea_hours))      > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Ea-mängden stämmer inte! Diff = "           + STRING(Ebr-Data_lop.Ea_Tid         - DECIMAL(ebr-tryck_lop.ea_hours))).  
             */
             IF Ebr-Data_lop.Bered_Tid      NE   DECIMAL(ebr-tryck_lop.bered_hours)                                                                                        THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Beredartiden stämmer inte! Diff = "         + STRING(Ebr-Data_lop.Bered_Tid      - DECIMAL(ebr-tryck_lop.bered_hours))).
             IF Ebr-Data_lop.Mont_Tid       NE   DECIMAL(ebr-tryck_lop.mont_hours)                                                                                         THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Montörstiden stämmer inte! Diff = "         + STRING(Ebr-Data_lop.Mont_Tid       - DECIMAL(ebr-tryck_lop.mont_hours))).    
             IF Ebr-Data_lop.Maskin_Tid     NE   DECIMAL(ebr-tryck_lop.maskin_hours)                                                                                       THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Maskintiden stämmer inte! Diff = "          + STRING(Ebr-Data_lop.Maskin_Tid     - DECIMAL(ebr-tryck_lop.maskin_hours))).  
             IF Ebr-Data_lop.Utrust_Tid     NE   DECIMAL(ebr-tryck_lop.utrust_hours)                                                                                       THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Utrustningstiden stämmer inte! Diff = "     + STRING(Ebr-Data_lop.Utrust_Tid     - DECIMAL(ebr-tryck_lop.utrust_hours))).  
             IF Ebr-Data_lop.Ea_Tid         NE   DECIMAL(ebr-tryck_lop.ea_hours)                                                                                           THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Ea-mängden stämmer inte! Diff = "           + STRING(Ebr-Data_lop.Ea_Tid         - DECIMAL(ebr-tryck_lop.ea_hours))).
              
             IF Ebr-Data_lop.Arbete_Kost    NE   DECIMAL(ebr-tryck_lop.arbete_cost)  AND ABSOLUTE(Ebr-Data_lop.Arbete_Kost   - DECIMAL(ebr-tryck_lop.arbete_cost))   > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Arbetskostnaden stämmer inte! Diff = "      + STRING(Ebr-Data_lop.Arbete_Kost    - DECIMAL(ebr-tryck_lop.arbete_cost))).  
             IF Ebr-Data_lop.Material_Kost  NE   DECIMAL(ebr-tryck_lop.material_cost)AND ABSOLUTE(Ebr-Data_lop.Material_Kost - DECIMAL(ebr-tryck_lop.material_cost)) > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Materielkostnaden stämmer inte! Diff = "    + STRING(Ebr-Data_lop.Material_Kost  - DECIMAL(ebr-tryck_lop.material_cost))).  
             IF Ebr-Data_lop.Maskin_Kost    NE   DECIMAL(ebr-tryck_lop.maskin_cost)  AND ABSOLUTE(Ebr-Data_lop.Maskin_Kost   - DECIMAL(ebr-tryck_lop.maskin_cost))   > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Maskinkostnaden stämmer inte! Diff = "      + STRING(Ebr-Data_lop.Maskin_Kost    - DECIMAL(ebr-tryck_lop.maskin_cost))).  
             IF Ebr-Data_lop.Utrust_Kost    NE   DECIMAL(ebr-tryck_lop.utrust_cost)  AND ABSOLUTE(Ebr-Data_lop.Utrust_Kost   - DECIMAL(ebr-tryck_lop.utrust_cost))   > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Utrustningskostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Utrust_Kost    - DECIMAL(ebr-tryck_lop.utrust_cost))).  
             IF Ebr-Data_lop.Ovrigt_Kost    NE   DECIMAL(ebr-tryck_lop.ovrigt_cost)  AND ABSOLUTE(Ebr-Data_lop.Ovrigt_Kost   - DECIMAL(ebr-tryck_lop.ovrigt_cost))   > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Övrigkostnad stämmer inte! Diff = "         + STRING(Ebr-Data_lop.Ovrigt_Kost    - DECIMAL(ebr-tryck_lop.ovrigt_cost))).  
             IF Ebr-Data_lop.Summa_Kost     NE   DECIMAL(ebr-tryck_lop.summa_cost)   AND ABSOLUTE(Ebr-Data_lop.Summa_Kost    - DECIMAL(ebr-tryck_lop.summa_cost))    > 1   THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Totalkostnaden stämmer inte! Diff = "       + STRING(Ebr-Data_lop.Summa_Kost     - DECIMAL(ebr-tryck_lop.summa_cost))).
             
             /*
             IF Ebr-Data_lop.Bered_Tid      NE   DECIMAL(ebr-tryck_lop.bered_hours)      THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Beredartiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Bered_Tid             - DECIMAL(ebr-tryck_lop.bered_hours))).
             IF Ebr-Data_lop.Mont_Tid       NE   DECIMAL(ebr-tryck_lop.mont_hours)       THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Montörstiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Mont_Tid              - DECIMAL(ebr-tryck_lop.mont_hours))).    
             IF Ebr-Data_lop.Maskin_Tid     NE   DECIMAL(ebr-tryck_lop.maskin_hours)     THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Maskintiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Maskin_Tid             - DECIMAL(ebr-tryck_lop.maskin_hours))).  
             IF Ebr-Data_lop.Utrust_Tid     NE   DECIMAL(ebr-tryck_lop.utrust_hours)     THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Utrustningstiden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Utrust_Tid        - DECIMAL(ebr-tryck_lop.utrust_hours))).  
             IF Ebr-Data_lop.Ea_Tid         NE   DECIMAL(ebr-tryck_lop.ea_hours)         THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Ea-mängden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Ea_Tid                  - DECIMAL(ebr-tryck_lop.ea_hours))).  
             IF Ebr-Data_lop.Arbete_Kost    NE   DECIMAL(ebr-tryck_lop.arbete_cost)      THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Arbetskostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Arbete_Kost        - DECIMAL(ebr-tryck_lop.arbete_cost))).  
             IF Ebr-Data_lop.Material_Kost  NE   DECIMAL(ebr-tryck_lop.material_cost)    THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Materielkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Material_Kost    - DECIMAL(ebr-tryck_lop.material_cost))).  
             IF Ebr-Data_lop.Maskin_Kost    NE   DECIMAL(ebr-tryck_lop.maskin_cost)      THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Maskinkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Maskin_Kost        - DECIMAL(ebr-tryck_lop.maskin_cost))).  
             IF Ebr-Data_lop.Utrust_Kost    NE   DECIMAL(ebr-tryck_lop.utrust_cost)      THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Utrustningskostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Utrust_Kost   - DECIMAL(ebr-tryck_lop.utrust_cost))).  
             IF Ebr-Data_lop.Ovrigt_Kost    NE   DECIMAL(ebr-tryck_lop.ovrigt_cost)      THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Övrigkostnad stämmer inte! Diff = " + STRING(Ebr-Data_lop.Ovrigt_Kost           - DECIMAL(ebr-tryck_lop.ovrigt_cost))).  
             IF Ebr-Data_lop.Summa_Kost     NE   DECIMAL(ebr-tryck_lop.summa_cost)       THEN THIS-OBJECT:SkapjmffelTT("Ebr-Tryck","Totalkostnaden stämmer inte! Diff = " + STRING(Ebr-Data_lop.Summa_Kost          - DECIMAL(ebr-tryck_lop.summa_cost))).                                           
             */
          END.   
       END.
       THIS-OBJECT:ControlDataDb:jmffelTTh:FIND-FIRST("WHERE  LISTA = 'Ebr-E'" ) NO-ERROR.
       IF THIS-OBJECT:ControlDataDb:jmffelTTh:AVAILABLE THEN .
       ELSE DO:
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-CREATE().
          ASSIGN
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("LISTA"):BUFFER-VALUE = "Ebr-E"
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("FELORSAK"):BUFFER-VALUE= "Allt stämmer".
       END. 
       THIS-OBJECT:ControlDataDb:jmffelTTh:FIND-FIRST("WHERE  LISTA = 'Ebr-Tryck'" ) NO-ERROR.
       IF THIS-OBJECT:ControlDataDb:jmffelTTh:AVAILABLE THEN .
       ELSE DO:
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-CREATE().
          ASSIGN
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("LISTA"):BUFFER-VALUE = "Ebr-Tryck"
          THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("FELORSAK"):BUFFER-VALUE= "Allt stämmer".
       END.        

       THIS-OBJECT:GridjmfKont:GuruReopen().
       THIS-OBJECT:GridjmfKont:GuruFirstrow().
       THIS-OBJECT:ExportKatalogerSC:Panel1Collapsed = TRUE.
       THIS-OBJECT:ExportKatalogerSC:Panel2Collapsed = FALSE.
   END METHOD.
   METHOD PUBLIC VOID SkapjmffelTT(listain AS CHARACTER, fel AS CHARACTER):
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-CREATE().
      ASSIGN 
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("ArbetsKod"):BUFFER-VALUE =  Ebr-Data_lop.ArbetsKod
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("Lopnummer"):BUFFER-VALUE = Ebr-Data_lop.Lopnummer 
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("Level"):BUFFER-VALUE = Ebr-Data_lop.Level 
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("LISTA"):BUFFER-VALUE = listain
      THIS-OBJECT:ControlDataDb:jmffelTTh:BUFFER-FIELD("FELORSAK"):BUFFER-VALUE= fel.
   END METHOD.
   METHOD PUBLIC VOID NyTryckSidaExcel(INPUT listaP AS CHARACTER):
      IF listaP = "KLG1P1" THEN DO:
         p1blad = p1blad + 1.
         /*bladnamn + bredd*/
         THIS-OBJECT:ControlWinLg:VisaExcel:NyttBladExcel().
         IF p1blad EQ 1 THEN THIS-OBJECT:ControlWinLg:VisaExcel:SetBladvar(1).
         THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P1 Sida " + STRING(p1blad)).
         THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43.71).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),3.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.43).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),9).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),73.43).
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb kod").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp nr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete Konstruktion").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Enhet"). 
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").       
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Kostnad i kr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M1","Summa kr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N1","Kommentar").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().         
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Bered").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Mont").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Maskin").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2","EA").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","Arbete").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2","Materiel").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2","Maskin").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2","Övrigt").
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:H1",3).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:L1",3).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M1:M2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("N1:N2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","L1","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","N2","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().   
      END.
      IF listaP = "KLG1P2" THEN DO:
         p2blad = p2blad + 1.
         /*bladnamn + bredd*/
         IF THIS-OBJECT:ControlWinLg:VisaExcel:GetBladvar() NE 1 THEN THIS-OBJECT:ControlWinLg:VisaExcel:NyttBladExcel().
         THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P2 Sida " + STRING(p2blad)).
         THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),59.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.43).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10.29).
          /*ut rubriker*/
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A2","Arb kod").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B2","Löp nr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C2","Arbete Konstruktion").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D2","Enhet"). 
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Timmar").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","Kostnad i kr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2","Summa kr").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E3","Bered").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F3","Mont").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G3","Maskin").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H3","EA").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I3","Arbete").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J3","Materiel").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K3","Maskin").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L3","Övrigt").
        
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A2:A3",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B2:B3",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C2:C3",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D2:D3",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:H2",3).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I2:L2",3).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M2:M3",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","M1","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E2","L2","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A3","M3","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
      END.
      IF listaP = "KLG1P3" THEN DO:
         p3blad = p3blad + 1.
         /*bladnamn + bredd*/
         IF THIS-OBJECT:ControlWinLg:VisaExcel:GetBladvar() NE 1 THEN THIS-OBJECT:ControlWinLg:VisaExcel:NyttBladExcel().
         THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
         THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel("P3 Sida " + STRING(p3blad)).
         THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
         THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.71).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),46.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6.57).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5.86).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),5).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),17.29).
         THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),8.43).
          /*ut rubriker*/
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb kod").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp nr").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete Konstruktion").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Enhet"). 
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H1","Antal man").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Kommentar").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J1","Tids-underlag").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Mont").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2","Maskin").
         THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","EA").
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:G1",3).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H1:H2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:I2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:J2",2).
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","G1","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","J2","4:7").
         THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
      END.
   END METHOD.
   
  
   METHOD PUBLIC VOID ExportKLGTillWord(INPUT listaP AS CHARACTER):
     /*THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
     THIS-OBJECT:ControlWinLg:VisaExcel:OpenUnderlag("underlagfil").
     THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = TRUE.
     IF listap = "KLG1" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:OpenWord("C:\klg1mall.doc").
        THIS-OBJECT:skapaSidor(listaP).   
     END.
     ELSE IF listap = "KLG2" THEN DO:
     END.
     ELSE RETURN.
     THIS-OBJECT:ControlWinLg:VisaExcel:StangUnderlag().
     THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).*/
   END METHOD.
  
   METHOD PUBLIC VOID skapaSidor(INPUT listaP AS CHARACTER):
     /*DEFINE VARIABLE antalp1blad AS INTEGER INITIAL 9.
     DEFINE VARIABLE antalp2blad AS INTEGER INITIAL 26.
     DEFINE VARIABLE antalp3blad AS INTEGER INITIAL 22.
     p1blad = 1.
     p2blad = 1.
     p3blad = 1.
     
     IF listap = "KLG1" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:TillBokmarke("P1").
        DO WHILE p1blad <= antalp1blad:
           THIS-OBJECT:ControlWinLg:VisaExcel:ValjSheet("P1 Sida " + STRING(p1blad)).  
           THIS-OBJECT:ControlWinLg:VisaExcel:KopieraOmrade("A1:N40").
           THIS-OBJECT:ControlWinLg:VisaExcel:KlistraIn().
           /*THIS-OBJECT:ControlWinLg:VisaExcel:HanteraBild(515, 610, 50, -49).*/
           THIS-OBJECT:ControlWinLg:VisaExcel:VridBild(-90).
           IF p1blad NE antalp1blad THEN DO:
              THIS-OBJECT:ControlWinLg:VisaExcel:Sidbrytning().
           END.
           p1blad = p1blad + 1.
        END.  
        THIS-OBJECT:ControlWinLg:VisaExcel:TillBokmarke("P2").
        DO WHILE p2blad <= antalp2blad:
           THIS-OBJECT:ControlWinLg:VisaExcel:ValjSheet("P2 Sida " + STRING(p2blad)). 
           THIS-OBJECT:ControlWinLg:VisaExcel:KopieraOmrade("A1:N40").
           THIS-OBJECT:ControlWinLg:VisaExcel:KlistraIn().
           /*THIS-OBJECT:ControlWinLg:VisaExcel:HanteraBild(515, 610, 50, -49).*/
           THIS-OBJECT:ControlWinLg:VisaExcel:VridBild(-90).
           IF p2blad NE antalp2blad THEN DO:
              THIS-OBJECT:ControlWinLg:VisaExcel:Sidbrytning().
           END.
           p2blad = p2blad + 1.
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:TillBokmarke("P3").
        DO WHILE p3blad <= antalp3blad:
           THIS-OBJECT:ControlWinLg:VisaExcel:ValjSheet("P3 Sida " + STRING(p3blad)).  
           THIS-OBJECT:ControlWinLg:VisaExcel:KopieraOmrade("A1:J40").
           THIS-OBJECT:ControlWinLg:VisaExcel:KlistraIn().
           /*THIS-OBJECT:ControlWinLg:VisaExcel:HanteraBild(610, 515, 50, -49).*/
           IF p3blad NE antalp3blad THEN DO:
              THIS-OBJECT:ControlWinLg:VisaExcel:Sidbrytning().
           END.
           p3blad = p3blad + 1.
        END.  
     END.
     ELSE IF listap = "KLG2" THEN DO:
     END.
     ELSE RETURN.*/
   END METHOD.
  
  


   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgExportControl").
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
      THIS-OBJECT:ExportKatalogerSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KontrollDET = NEW Controls.Button().
      THIS-OBJECT:TillEbrData = NEW Controls.Button().
      THIS-OBJECT:comboTryckExp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:TillTryck = NEW Controls.Button().
      THIS-OBJECT:TillEbre = NEW Controls.Button().
      THIS-OBJECT:TillExcel = NEW Controls.Button().
      THIS-OBJECT:splitContainerjmfKoll = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStrip1 = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonAter = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridjmfKont = NEW Controls.Grid().
      CAST(THIS-OBJECT:ExportKatalogerSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ExportKatalogerSC:Panel1:SuspendLayout().
      THIS-OBJECT:ExportKatalogerSC:Panel2:SuspendLayout().
      THIS-OBJECT:ExportKatalogerSC:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerjmfKoll, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerjmfKoll:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerjmfKoll:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerjmfKoll:SuspendLayout().
      THIS-OBJECT:toolStrip1:SuspendLayout().
      CAST(THIS-OBJECT:GridjmfKont, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* ExportKatalogerSC */
      /*  */
      THIS-OBJECT:ExportKatalogerSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ExportKatalogerSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ExportKatalogerSC:Name = "ExportKatalogerSC".
      /*  */
      /* ExportKatalogerSC.Panel1 */
      /*  */
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:KontrollDET).
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:TillEbrData).
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:comboTryckExp).
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:TillTryck).
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:TillEbre).
      THIS-OBJECT:ExportKatalogerSC:Panel1:Controls:Add(THIS-OBJECT:TillExcel).
      /*  */
      /* ExportKatalogerSC.Panel2 */
      /*  */
      THIS-OBJECT:ExportKatalogerSC:Panel2:Controls:Add(THIS-OBJECT:splitContainerjmfKoll).
      THIS-OBJECT:ExportKatalogerSC:Size = NEW System.Drawing.Size(1065, 584).
      THIS-OBJECT:ExportKatalogerSC:SplitterDistance = 939.
      THIS-OBJECT:ExportKatalogerSC:TabIndex = 0.
      /*  */
      /* KontrollDET */
      /*  */
      appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
      THIS-OBJECT:KontrollDET:Appearance = appearance1.
      THIS-OBJECT:KontrollDET:GuruTooltip = "Jämför Ebr-D Ebr-E Ebr-T".
      THIS-OBJECT:KontrollDET:ImageSize = NEW System.Drawing.Size(260, 260).
      THIS-OBJECT:KontrollDET:Location = NEW System.Drawing.Point(1214, 53).
      THIS-OBJECT:KontrollDET:Name = "KontrollDET".
      THIS-OBJECT:KontrollDET:Size = NEW System.Drawing.Size(270, 315).
      THIS-OBJECT:KontrollDET:TabIndex = 12.
      THIS-OBJECT:KontrollDET:Click:Subscribe(THIS-OBJECT:KontrollDET_Click).
      /*  */
      /* TillEbrData */
      /*  */
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      THIS-OBJECT:TillEbrData:Appearance = appearance2.
      THIS-OBJECT:TillEbrData:GuruTooltip = "Export av Ebr-Data".
      THIS-OBJECT:TillEbrData:ImageSize = NEW System.Drawing.Size(260, 260).
      THIS-OBJECT:TillEbrData:Location = NEW System.Drawing.Point(914, 53).
      THIS-OBJECT:TillEbrData:Name = "TillEbrData".
      THIS-OBJECT:TillEbrData:Size = NEW System.Drawing.Size(270, 315).
      THIS-OBJECT:TillEbrData:TabIndex = 11.
      THIS-OBJECT:TillEbrData:Click:Subscribe(THIS-OBJECT:TillEbrData_Click).
      /*  */
      /* comboTryckExp */
      /*  */
      THIS-OBJECT:comboTryckExp:Location = NEW System.Drawing.Point(14, 19).
      THIS-OBJECT:comboTryckExp:Name = "comboTryckExp".
      THIS-OBJECT:comboTryckExp:Size = NEW System.Drawing.Size(260, 28).
      THIS-OBJECT:comboTryckExp:TabIndex = 9.
      /*  */
      /* TillTryck */
      /*  */
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      THIS-OBJECT:TillTryck:Appearance = appearance3.
      THIS-OBJECT:TillTryck:GuruTooltip = "Export av Ebr-Tryck".
      THIS-OBJECT:TillTryck:ImageSize = NEW System.Drawing.Size(260, 260).
      THIS-OBJECT:TillTryck:Location = NEW System.Drawing.Point(14, 53).
      THIS-OBJECT:TillTryck:Name = "TillTryck".
      THIS-OBJECT:TillTryck:Size = NEW System.Drawing.Size(270, 315).
      THIS-OBJECT:TillTryck:TabIndex = 8.
      THIS-OBJECT:TillTryck:Click:Subscribe(THIS-OBJECT:TillTryck_Click).
      /*  */
      /* TillEbre */
      /*  */
      appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
      THIS-OBJECT:TillEbre:Appearance = appearance4.
      THIS-OBJECT:TillEbre:GuruTooltip = "Export av Ebr-E".
      THIS-OBJECT:TillEbre:ImageSize = NEW System.Drawing.Size(260, 260).
      THIS-OBJECT:TillEbre:Location = NEW System.Drawing.Point(614, 53).
      THIS-OBJECT:TillEbre:Name = "TillEbre".
      THIS-OBJECT:TillEbre:Size = NEW System.Drawing.Size(270, 315).
      THIS-OBJECT:TillEbre:TabIndex = 7.
      THIS-OBJECT:TillEbre:Click:Subscribe(THIS-OBJECT:TillEbrE_Click).
      /*  */
      /* TillExcel */
      /*  */
      appearance5:Image = CAST(resources:GetObject("appearance5.Image"), System.Object).
      THIS-OBJECT:TillExcel:Appearance = appearance5.
      THIS-OBJECT:TillExcel:GuruTooltip = "Export av Ebr-Excel".
      THIS-OBJECT:TillExcel:ImageSize = NEW System.Drawing.Size(260, 260).
      THIS-OBJECT:TillExcel:Location = NEW System.Drawing.Point(314, 53).
      THIS-OBJECT:TillExcel:Name = "TillExcel".
      THIS-OBJECT:TillExcel:Size = NEW System.Drawing.Size(270, 315).
      THIS-OBJECT:TillExcel:TabIndex = 5.
      THIS-OBJECT:TillExcel:Click:Subscribe(THIS-OBJECT:TillExcel_Click).
      /*  */
      /* splitContainerjmfKoll */
      /*  */
      THIS-OBJECT:splitContainerjmfKoll:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerjmfKoll:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerjmfKoll:Name = "splitContainerjmfKoll".
      THIS-OBJECT:splitContainerjmfKoll:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerjmfKoll.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerjmfKoll:Panel1:Controls:Add(THIS-OBJECT:toolStrip1).
      /*  */
      /* splitContainerjmfKoll.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerjmfKoll:Panel2:Controls:Add(THIS-OBJECT:GridjmfKont).
      THIS-OBJECT:splitContainerjmfKoll:Size = NEW System.Drawing.Size(122, 584).
      THIS-OBJECT:splitContainerjmfKoll:SplitterDistance = 25.
      THIS-OBJECT:splitContainerjmfKoll:TabIndex = 1.
      /*  */
      /* toolStrip1 */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripButtonAter.
      THIS-OBJECT:toolStrip1:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStrip1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStrip1:Name = "toolStrip1".
      THIS-OBJECT:toolStrip1:Size = NEW System.Drawing.Size(122, 25).
      THIS-OBJECT:toolStrip1:TabIndex = 0.
      THIS-OBJECT:toolStrip1:Text = "toolStrip1".
      /*  */
      /* toolStripButtonAter */
      /*  */
      THIS-OBJECT:toolStripButtonAter:Image = CAST(resources:GetObject("toolStripButtonAter.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonAter:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonAter:Name = "toolStripButtonAter".
      THIS-OBJECT:toolStripButtonAter:Size = NEW System.Drawing.Size(101, 22).
      THIS-OBJECT:toolStripButtonAter:Text = "Åter till Export".
      THIS-OBJECT:toolStripButtonAter:Click:Subscribe(THIS-OBJECT:toolStripButtonAter_Click).
      /*  */
      /* GridjmfKont */
      /*  */
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Appearance = appearance6.
      THIS-OBJECT:GridjmfKont:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridjmfKont:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance7:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance7:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance7:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridjmfKont:DisplayLayout:GroupByBox:Appearance = appearance7.
      appearance8:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridjmfKont:DisplayLayout:GroupByBox:BandLabelAppearance = appearance8.
      THIS-OBJECT:GridjmfKont:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance9:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance9:BackColor2 = System.Drawing.SystemColors:Control.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridjmfKont:DisplayLayout:GroupByBox:PromptAppearance = appearance9.
      THIS-OBJECT:GridjmfKont:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridjmfKont:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      appearance10:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:ActiveCellAppearance = appearance10.
      appearance11:BackColor = System.Drawing.SystemColors:Highlight.
      appearance11:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:ActiveRowAppearance = appearance11.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:CardAreaAppearance = appearance12.
      appearance13:BorderColor = System.Drawing.Color:Silver.
      appearance13:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:CellAppearance = appearance13.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:CellPadding = 0.
      appearance14:BackColor = System.Drawing.SystemColors:Control.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:GroupByRowAppearance = appearance14.
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:HeaderAppearance = appearance15.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance16:BackColor = System.Drawing.SystemColors:Window.
      appearance16:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:RowAppearance = appearance16.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance17:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridjmfKont:DisplayLayout:Override:TemplateAddRowAppearance = appearance17.
      THIS-OBJECT:GridjmfKont:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridjmfKont:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridjmfKont:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridjmfKont:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridjmfKont:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridjmfKont:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridjmfKont:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridjmfKont:GuruContext = ?.
      THIS-OBJECT:GridjmfKont:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridjmfKont:Name = "GridjmfKont".
      THIS-OBJECT:GridjmfKont:Size = NEW System.Drawing.Size(122, 555).
      THIS-OBJECT:GridjmfKont:TabIndex = 0.
      THIS-OBJECT:GridjmfKont:Text = "Jämförelse Ebr-D mot Ebr-E och Ebr-Tryck".
      /*  */
      /* EkgExportControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:ExportKatalogerSC).
      THIS-OBJECT:Name = "EkgExportControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1065, 584).
      THIS-OBJECT:ExportKatalogerSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:ExportKatalogerSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ExportKatalogerSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ExportKatalogerSC:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerjmfKoll:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerjmfKoll:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerjmfKoll:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerjmfKoll, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerjmfKoll:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:PerformLayout().
      CAST(THIS-OBJECT:GridjmfKont, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID TillEbrData_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   Guru.Konstanter:AmericanSet().
      THIS-OBJECT:ExportEBR-EData("EbrD",FALSE).
      Guru.Konstanter:EuropeanAmericanReset().
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonAter_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:EbrXmlRensa().
		THIS-OBJECT:ExportKatalogerSC:Panel2Collapsed = TRUE.
		THIS-OBJECT:ExportKatalogerSC:Panel1Collapsed = FALSE.
      RETURN.

	END METHOD.


	

END CLASS.