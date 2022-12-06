
 /*------------------------------------------------------------------------
    File        : MtrlJmfPris
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed Oct 21 12:45:03 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  

CLASS Modules.Materiel.MtrlJmfPris INHERITS Form   : 
   DEFINE PRIVATE VARIABLE ButtonAHL AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonES AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonJMF AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonON AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonRE AS Controls.Button NO-UNDO.
   DEFINE PUBLIC VARIABLE comboLev4 AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboLev3 AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboLev2 AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE comboLev1 AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE GridJmfPris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE Levnamn AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE RexPostnr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Materiel.MtrlJmfPrisDb                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerJmfPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSaldoSok AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSok AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE poslev1kn AS INTEGER NO-UNDO.
   DEFINE VARIABLE poslev2kn AS INTEGER NO-UNDO.
   DEFINE VARIABLE poslev3kn AS INTEGER NO-UNDO.
   DEFINE VARIABLE poslev4kn AS INTEGER NO-UNDO.
   
   
   
	CONSTRUCTOR PUBLIC MtrlJmfPris( r AS Guru.Root ):
	
		SUPER().
		THIS-OBJECT:ROOT = r.
		InitializeComponent ( ).
		THIS-OBJECT:SIZE = NEW System.Drawing.Size(Guru.Konstanter:globstorb,Guru.Konstanter:globstorh).
		THIS-OBJECT:Root:DatabaseManager:MtrlJmfPris:ControllMtrlJmfPris = THIS-OBJECT.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:MtrlJmfPris.
      
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      THIS-OBJECT:CreateForm().
      
		CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC MtrlJmfPris( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonAHL_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
      THIS-OBJECT:ControlDataDb:SaldoAHL().
      THIS-OBJECT:GridJmfPris:GuruReopen().
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonES_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
		THIS-OBJECT:ControlDataDb:SaldoES().
		THIS-OBJECT:GridJmfPris:GuruReopen().
		THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonJMF_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
      
      THIS-OBJECT:ControlDataDb:BytLev().
      THIS-OBJECT:GridJmfPris:GuruReopen().
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
      RETURN.

	END METHOD.


	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		THIS-OBJECT:ControlDataDb:SparaValdLev().
		THIS-OBJECT:CLOSE().
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonON_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
		THIS-OBJECT:ControlDataDb:SaldoON().
      THIS-OBJECT:GridJmfPris:GuruReopen().
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonRE_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
		THIS-OBJECT:ControlDataDb:SaldoRE().
      THIS-OBJECT:GridJmfPris:GuruReopen().
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	

   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:Text = "Jämför materielpriser ".
      THIS-OBJECT:RexPostnr:GuruLabel:Text     = "Postnr".
      THIS-OBJECT:Levnamn:GuruLabel:Text = "Sökt saldo hos".
      THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateComboLev().
      THIS-OBJECT:CreateGridJmfPris().
      THIS-OBJECT:CreateGridSaldo().
       
      THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:EMPTY-TEMP-TABLE().
      THIS-OBJECT:GridJmfPris:GuruReopen().
      THIS-OBJECT:ControlDataDb:JmfMtrl().
      THIS-OBJECT:GridJmfPris:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID CreateComboLev():
      DEFINE VARIABLE cmbLevlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE levkod        AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE levnamn      AS Controls.GridRubrik      NO-UNDO.
      cmbLevlist = NEW Controls.GridRubrikLista().
      cmbLevlist:ttBufferHandle = Guru.GlobalaVariabler:jmfLevantorBuffh.
      levkod = NEW Controls.GridRubrik().
      levkod:Falt = "LEVKOD".
      levkod:Rubrik = "Lev-ID".
      cmbLevlist:addRubrik(levkod).
      levnamn = NEW Controls.GridRubrik().
      levnamn:Falt = "LEVNAMN".
      levnamn:Rubrik = "Leverantör".
      cmbLevlist:addRubrik(levnamn).
      levnamn:Bredd = 300.
      
      THIS-OBJECT:comboLev1:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      THIS-OBJECT:comboLev2:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      THIS-OBJECT:comboLev3:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      THIS-OBJECT:comboLev4:GuruInitCombo(cmbLevlist,"LEVKOD","LEVNAMN").
      /*
      queryvar = "FOR EACH " + Guru.GlobalaVariabler:jmfLevantorBuffh:TABLE. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:jmfLevantorBuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            
            MESSAGE 
            Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE
            Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE
            Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE
            VIEW-AS ALERT-BOX.
            qH:GET-NEXT().
         END.
      */
      
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridSaldo():
     
   END METHOD.
   METHOD PUBLIC VOID PlaceraSaldoKn(INPUT placera AS INTEGER ):
      
      IF placera = 1 THEN DO:
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN THIS-OBJECT:ButtonES:Location = NEW System.Drawing.Point(poslev1kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN THIS-OBJECT:ButtonON:Location = NEW System.Drawing.Point(poslev1kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "SELGA" OR
            THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN THIS-OBJECT:ButtonRE:Location = NEW System.Drawing.Point(poslev1kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN THIS-OBJECT:ButtonAHL:Location = NEW System.Drawing.Point(poslev1kn, 3).
      END. 
      IF placera = 2 THEN DO:
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN THIS-OBJECT:ButtonES:Location = NEW System.Drawing.Point(poslev2kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN THIS-OBJECT:ButtonON:Location = NEW System.Drawing.Point(poslev2kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "SELGA" OR
            THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN THIS-OBJECT:ButtonRE:Location = NEW System.Drawing.Point(poslev2kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN THIS-OBJECT:ButtonAHL:Location = NEW System.Drawing.Point(poslev2kn, 3).
      END.  
      IF placera = 3 THEN DO:
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN THIS-OBJECT:ButtonES:Location = NEW System.Drawing.Point(poslev3kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN THIS-OBJECT:ButtonON:Location = NEW System.Drawing.Point(poslev3kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "SELGA" OR
            THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN THIS-OBJECT:ButtonRE:Location = NEW System.Drawing.Point(poslev3kn, 3).
           
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN THIS-OBJECT:ButtonAHL:Location = NEW System.Drawing.Point(poslev3kn, 3).
      END.  
      IF placera = 4 THEN DO:
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN THIS-OBJECT:ButtonES:Location = NEW System.Drawing.Point(poslev4kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN THIS-OBJECT:ButtonON:Location = NEW System.Drawing.Point(poslev4kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "SELGA" OR
            THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN THIS-OBJECT:ButtonRE:Location = NEW System.Drawing.Point(poslev4kn, 3).
         IF THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN THIS-OBJECT:ButtonAHL:Location = NEW System.Drawing.Point(poslev4kn, 3).
      END.
      THIS-OBJECT:RexPostnr:LOCATION =  THIS-OBJECT:ButtonRE:Location.   
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridJmfPris():
     
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:AVAILABLE THEN.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(197), "").
        
      END.
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE antallevstart AS INTEGER NO-UNDO.
      DEFINE VARIABLE Prisvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      {GridRubrikListaVarExtent.i}
      
     
      
      poslev1kn = INTEGER(SUBSTRING(STRING(THIS-OBJECT:ButtonES:Location),INDEX(STRING(THIS-OBJECT:ButtonES:Location),"=") + 1, 
                  INDEX(STRING(THIS-OBJECT:ButtonES:Location),",") - 1  - INDEX(STRING(THIS-OBJECT:ButtonES:Location),"="))).
      poslev2kn = INTEGER(SUBSTRING(STRING(THIS-OBJECT:ButtonON:Location),INDEX(STRING(THIS-OBJECT:ButtonON:Location),"=") + 1, 
                  INDEX(STRING(THIS-OBJECT:ButtonON:Location),",") - 1  - INDEX(STRING(THIS-OBJECT:ButtonON:Location),"="))).
      poslev3kn = INTEGER(SUBSTRING(STRING(THIS-OBJECT:ButtonAHL:Location),INDEX(STRING(THIS-OBJECT:ButtonAHL:Location),"=") + 1, 
                  INDEX(STRING(THIS-OBJECT:ButtonAHL:Location),",") - 1  - INDEX(STRING(THIS-OBJECT:ButtonAHL:Location),"="))).
      poslev4kn = INTEGER(SUBSTRING(STRING(THIS-OBJECT:ButtonRE:Location),INDEX(STRING(THIS-OBJECT:ButtonRE:Location),"=") + 1, 
                  INDEX(STRING(THIS-OBJECT:ButtonRE:Location),",") - 1  - INDEX(STRING(THIS-OBJECT:ButtonRE:Location),"="))).
      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:rubrikTTh:TABLE  .
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:rubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         antallev = antallev + 1.
         IF antallev = 1 THEN THIS-OBJECT:comboLev1:VALUE = THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
         IF antallev = 2 THEN THIS-OBJECT:comboLev2:VALUE = THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
         IF antallev = 3 THEN THIS-OBJECT:comboLev3:VALUE = THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.
         IF antallev = 4 THEN THIS-OBJECT:comboLev4:VALUE = THIS-OBJECT:ControlDataDb:rubrikTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE.  
         THIS-OBJECT:PlaceraSaldoKn(antallev).
         qH:GET-NEXT().
      END.
      antallev = 0.
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,TRUE,65).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      /*
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHET","Enhet",TRUE,45).
      */
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("MINPRIS","MINPRIS",TRUE,50).
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHETJMF","ENHETJMF",TRUE,50).
      
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,50,"INT","").
      
      
      
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENRLEV",1,Guru.Konstanter:genk,TRUE,65,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHET",1,"Enh",TRUE,35,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("PRIS",1,"Pris/st",TRUE,60,"DEC","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LAGERSALDO",1,"Saldo",TRUE,50,"INT","").
      antallev = antallev + 1.  
      rubrikergrid[antallev] = NEW Controls.GridRubrik("VALD",1,"Välj",FALSE,30,"LOG","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ANMARK",1,"Amn",TRUE,100,"","").
      
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENRLEV",2,Guru.Konstanter:genk,TRUE,65,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHET",2,"Enh",TRUE,35,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("PRIS",2,"Pris/st",TRUE,50,"DEC","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LAGERSALDO",2,"Saldo",TRUE,50,"INT","").
      antallev = antallev + 1.  
      rubrikergrid[antallev] = NEW Controls.GridRubrik("VALD",2,"Välj",FALSE,30,"LOG","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ANMARK",2,"Amn",TRUE,100,"","").
      
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENRLEV",3,Guru.Konstanter:genk,TRUE,65,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHET",3,"Enh",TRUE,35,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("PRIS",3,"Pris/st",TRUE,60,"DEC","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LAGERSALDO",3,"Saldo",TRUE,50,"INT","").
      antallev = antallev + 1.  
      rubrikergrid[antallev] = NEW Controls.GridRubrik("VALD",3,"Välj",FALSE,30,"LOG","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ANMARK",3,"Amn",TRUE,100,"","").
      
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENRLEV",4,Guru.Konstanter:genk,TRUE,65,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ENHET",4,"Enh",TRUE,35,"","").
      antallev = antallev + 1.
      rubrikergrid[antallev] = NEW Controls.GridRubrik("PRIS",4,"Pris/st",TRUE,50,"DEC","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("LAGERSALDO",4,"Saldo",TRUE,50,"INT","").
      antallev = antallev + 1.  
      rubrikergrid[antallev] = NEW Controls.GridRubrik("VALD",4,"Välj",FALSE,30,"LOG","").
      antallev = antallev + 1.     
      rubrikergrid[antallev] = NEW Controls.GridRubrik("ANMARK",4,"Amn",TRUE,100,"","").
     
      HmtRubrikerlista:tillatSort = FALSE.
      THIS-OBJECT:GridJmfPris:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT antallev, INPUT THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh)). 
      THIS-OBJECT:GridJmfPris:ExtentAntal(antallev).
      
      THIS-OBJECT:GridJmfPris:GuruRadOptionStartEx(TRUE).
      CellFont = NEW System.Drawing.Font("", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
         
                                                 /*styrfält , värde ,   färg ,                          font, alla col, lista col  0 = bgcolor 1 = fg color 2 font*/
                                                                                                                             /*  ska avslutas med,*/ 
      
      THIS-OBJECT:GridJmfPris:GuruRadOptionAdd(1,"PRIS","MINPRIS","", Guru.StaticMethods:DotNetColor(10),CellFont,FALSE,"PRIS,",0,0).
      THIS-OBJECT:GridJmfPris:DisplayLayout:Bands[0]:Columns["MINPRIS"]:Hidden = TRUE.
      
      THIS-OBJECT:GridJmfPris:GuruRadOptionAdd(2,"ENHET","ENHETJMF","", Guru.StaticMethods:DotNetColor(12),CellFont,FALSE,"ENHET,",0,4).
      
      THIS-OBJECT:GridJmfPris:DisplayLayout:Bands[0]:Columns["ENHETJMF"]:Hidden = TRUE.
      
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ENRLEV",1).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ENHET",1).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("PRIS",1).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("LAGERSALDO",1).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("VALD",1).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ANMARK",1).
      
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ENRLEV",3).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ENHET",3).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("PRIS",3).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("LAGERSALDO",3).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("VALD",3).
      THIS-OBJECT:GridJmfPris:ColumnBoldSet("ANMARK",3).
      
      
      
      
      THIS-OBJECT:GridJmfPris:AfterCellUpdate:Subscribe(THIS-OBJECT:GridJmfPris_AfterCellUpdate).
      THIS-OBJECT:GridJmfPris:CellChange:Subscribe(THIS-OBJECT:GridJmfPris_CellChange).
      THIS-OBJECT:GridJmfPris:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridJmfPris_BeforeCellUpdate).
      
      THIS-OBJECT:GridJmfPris:GuruReOpen().
      THIS-OBJECT:GridJmfPris:SetTitle("Materiel priser").
      THIS-OBJECT:SkapaGrupper().
      
       
   END METHOD.  

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID GridJmfPris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
      
      RETURN.

   END METHOD.
	METHOD PRIVATE VOID GridJmfPris_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
     
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
	 METHOD PRIVATE VOID GridJmfPris_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
     
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE valdnr AS INTEGER NO-UNDO. 
      DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      THIS-OBJECT:ControlDataDb:SummaRadbuffh:FIND-FIRST("WHERE  ENR = ''",NO-LOCK) NO-ERROR.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF kolnamn BEGINS "VALD" THEN DO:
         valdnr = INTEGER(SUBSTRING(kolnamn,6,1)).
        
         IF beforekolvarde = "FALSE" THEN DO:
            THIS-OBJECT:ControlDataDb:SummaRadbuffh:BUFFER-FIELD("VALD"):BUFFER-VALUE(1) = FALSE.
            THIS-OBJECT:ControlDataDb:SummaRadbuffh:BUFFER-FIELD("VALD"):BUFFER-VALUE(2) = FALSE.
            THIS-OBJECT:ControlDataDb:SummaRadbuffh:BUFFER-FIELD("VALD"):BUFFER-VALUE(3) = FALSE.
            THIS-OBJECT:ControlDataDb:SummaRadbuffh:BUFFER-FIELD("VALD"):BUFFER-VALUE(4) = FALSE. 
            REPEAT:
               raknare = raknare + 1.
               IF raknare > 4 THEN LEAVE.
               IF raknare = valdnr THEN DO:
                  THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(valdnr) = TRUE.
                  /*THIS-OBJECT:GridJmfPris:ActiveRow:Cells[kolnamn]:VALUE = TRUE.*/
               END.
               ELSE DO:
                  THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(raknare) = FALSE.
                  /*THIS-OBJECT:GridJmfPris:ActiveRow:Cells[kolnamn]:VALUE = FALSE.*/
               END.   
            END.  
                       
         END.
         ELSE DO:
            THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(valdnr) = FALSE.
            /*THIS-OBJECT:GridJmfPris:ActiveRow:Cells[kolnamn]:VALUE = FALSE.*/
         END.    
        
         IF THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "" THEN DO:
           
            IF THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(1) = TRUE OR
            THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(2) = TRUE OR
            THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(3) = TRUE OR
            THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(4) = TRUE  THEN DO:
               queryvar = "FOR EACH " + THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:TABLE. 
               qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh,queryvar).
               qH:GET-FIRST().
               DO WHILE qH:QUERY-OFF-END = FALSE: 
                  IF THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "" THEN THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(valdnr) = FALSE.
                  ELSE DO:
                     THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(1) = FALSE.
                     THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(2) = FALSE.
                     THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(3) = FALSE.
                     THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(4) = FALSE.
                     THIS-OBJECT:ControlDataDb:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(valdnr) = TRUE.
                     
                  END.    
                  qH:GET-NEXT().
               END.
            END.      
         END.
            
      END.
      THIS-OBJECT:GridJmfPris:GuruReopen(). 
      RETURN.
   END METHOD.
	 
	 
	METHOD PUBLIC VOID SkapaGrupper():
	   THIS-OBJECT:GridJmfPris:SkapaGrupp(0,"Materiel","Materiel").
      THIS-OBJECT:GridJmfPris:SetGroup(0,"Materiel","ENR").
      THIS-OBJECT:GridJmfPris:SetGroup(0,"Materiel","BENAMNING").
      THIS-OBJECT:GridJmfPris:SetGroup(0,"Materiel","ANTAL").
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboLev1:VALUE)),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         THIS-OBJECT:GridJmfPris:SkapaGrupp(0,"Leverantor1",Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE).
        
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","ENRLEV[1]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","ENHET[1]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","PRIS[1]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","LAGERSALDO[1]").
         IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","VALD[1]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor1","ANMARK[1]").
      END.
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboLev2:VALUE)),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         THIS-OBJECT:GridJmfPris:SkapaGrupp(0,"Leverantor2",Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE).
       
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","ENRLEV[2]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","ENHET[2]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","PRIS[2]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","LAGERSALDO[2]").
         IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","VALD[2]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor2","ANMARK[2]").
      END.  
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboLev3:VALUE)),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         THIS-OBJECT:GridJmfPris:SkapaGrupp(0,"Leverantor3",Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE).
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","ENRLEV[3]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","ENHET[3]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","PRIS[3]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","LAGERSALDO[3]").
         IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","VALD[3]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor3","ANMARK[3]").
      END.
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(STRING(THIS-OBJECT:comboLev4:VALUE)),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO: 
         THIS-OBJECT:GridJmfPris:SkapaGrupp(0,"Leverantor4",Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE).
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","ENRLEV[4]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","ENHET[4]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","PRIS[4]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","LAGERSALDO[4]").
         IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? THEN THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","VALD[4]").
         THIS-OBJECT:GridJmfPris:SetGroup(0,"Leverantor4","ANMARK[4]").
      END.    
   END METHOD.   
	
	
	
	@VisualDesigner.
	METHOD PRIVATE VOID MtrlJmfPris_Shown( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 THIS-OBJECT:GridJmfPris:GuruAvmarkeraTot().  
		RETURN.

	END METHOD.
   
   
    METHOD PUBLIC LOGICAL Initialize():
      
      RETURN TRUE.
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	METHOD PRIVATE VOID InitializeComponent ( ):

		/* NOTE: The following method is automatically generated.
		   
		   We strongly suggest that the contents of this method only be modified using the 
		   Visual Designer to avoid any incompatible modifications.
		   
		   Modifying the contents of this method using a code editor will invalidate any support for this file. */
		
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Materiel.MtrlJmfPris").
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
      THIS-OBJECT:splitContainerJmfPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSok = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSaldoSok = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:ButtonAHL = NEW Controls.Button().
      THIS-OBJECT:Levnamn = NEW Controls.FillinChar().
      THIS-OBJECT:ButtonRE = NEW Controls.Button().
      THIS-OBJECT:ButtonON = NEW Controls.Button().
      THIS-OBJECT:ButtonES = NEW Controls.Button().
      THIS-OBJECT:ButtonJMF = NEW Controls.Button().
      THIS-OBJECT:comboLev4 = NEW Controls.Combo().
      THIS-OBJECT:comboLev3 = NEW Controls.Combo().
      THIS-OBJECT:comboLev2 = NEW Controls.Combo().
      THIS-OBJECT:comboLev1 = NEW Controls.Combo().
      THIS-OBJECT:GridJmfPris = NEW Controls.Grid().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:RexPostnr = NEW Controls.FillinInteger().
      CAST(THIS-OBJECT:splitContainerJmfPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerJmfPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerJmfPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerJmfPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSok, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSok:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSok:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSok:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSaldoSok, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSaldoSok:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSaldoSok:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSaldoSok:SuspendLayout().
      CAST(THIS-OBJECT:comboLev4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboLev3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboLev2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:comboLev1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridJmfPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerJmfPris */
      /*  */
      THIS-OBJECT:splitContainerJmfPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerJmfPris:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerJmfPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerJmfPris:Name = "splitContainerJmfPris".
      THIS-OBJECT:splitContainerJmfPris:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerJmfPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerJmfPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerSok).
      /*  */
      /* splitContainerJmfPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerJmfPris:Panel2:Controls:Add(THIS-OBJECT:GridJmfPris).
      THIS-OBJECT:splitContainerJmfPris:Panel2MinSize = 28.
      THIS-OBJECT:splitContainerJmfPris:Size = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:splitContainerJmfPris:SplitterDistance = 200.
      THIS-OBJECT:splitContainerJmfPris:TabIndex = 0.
      /*  */
      /* splitContainerSok */
      /*  */
      THIS-OBJECT:splitContainerSok:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSok:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSok:Name = "splitContainerSok".
      THIS-OBJECT:splitContainerSok:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSok.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSok:Panel1:Controls:Add(THIS-OBJECT:splitContainerSaldoSok).
      /*  */
      /* splitContainerSok.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSok:Panel2:Controls:Add(THIS-OBJECT:ButtonJMF).
      THIS-OBJECT:splitContainerSok:Panel2:Controls:Add(THIS-OBJECT:comboLev4).
      THIS-OBJECT:splitContainerSok:Panel2:Controls:Add(THIS-OBJECT:comboLev3).
      THIS-OBJECT:splitContainerSok:Panel2:Controls:Add(THIS-OBJECT:comboLev2).
      THIS-OBJECT:splitContainerSok:Panel2:Controls:Add(THIS-OBJECT:comboLev1).
      THIS-OBJECT:splitContainerSok:Size = NEW System.Drawing.Size(1194, 200).
      THIS-OBJECT:splitContainerSok:SplitterDistance = 138.
      THIS-OBJECT:splitContainerSok:TabIndex = 9.
      /*  */
      /* splitContainerSaldoSok */
      /*  */
      THIS-OBJECT:splitContainerSaldoSok:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSaldoSok:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerSaldoSok:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSaldoSok:Name = "splitContainerSaldoSok".
      THIS-OBJECT:splitContainerSaldoSok:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSaldoSok.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSaldoSok:Panel1:Controls:Add(THIS-OBJECT:RexPostnr).
      THIS-OBJECT:splitContainerSaldoSok:Panel1:Controls:Add(THIS-OBJECT:ButtonOK).
      /*  */
      /* splitContainerSaldoSok.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSaldoSok:Panel2:Controls:Add(THIS-OBJECT:ButtonAHL).
      THIS-OBJECT:splitContainerSaldoSok:Panel2:Controls:Add(THIS-OBJECT:Levnamn).
      THIS-OBJECT:splitContainerSaldoSok:Panel2:Controls:Add(THIS-OBJECT:ButtonRE).
      THIS-OBJECT:splitContainerSaldoSok:Panel2:Controls:Add(THIS-OBJECT:ButtonON).
      THIS-OBJECT:splitContainerSaldoSok:Panel2:Controls:Add(THIS-OBJECT:ButtonES).
      THIS-OBJECT:splitContainerSaldoSok:Size = NEW System.Drawing.Size(1194, 138).
      THIS-OBJECT:splitContainerSaldoSok:SplitterDistance = 35.
      THIS-OBJECT:splitContainerSaldoSok:TabIndex = 0.
      /*  */
      /* ButtonOK */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
      THIS-OBJECT:ButtonOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance18.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(1072, 3).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 8.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* ButtonAHL */
      /*  */
      appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
      THIS-OBJECT:ButtonAHL:Appearance = appearance19.
      THIS-OBJECT:ButtonAHL:ImageSize = NEW System.Drawing.Size(200, 51).
      THIS-OBJECT:ButtonAHL:Location = NEW System.Drawing.Point(541, 3).
      THIS-OBJECT:ButtonAHL:Name = "ButtonAHL".
      THIS-OBJECT:ButtonAHL:Size = NEW System.Drawing.Size(211, 60).
      THIS-OBJECT:ButtonAHL:TabIndex = 5.
      THIS-OBJECT:ButtonAHL:Click:Subscribe(THIS-OBJECT:ButtonAHL_Click).
      /*  */
      /* Levnamn */
      /*  */
      THIS-OBJECT:Levnamn:Location = NEW System.Drawing.Point(298, 79).
      THIS-OBJECT:Levnamn:Name = "Levnamn".
      THIS-OBJECT:Levnamn:Rubrik = "GuruLabel".
      THIS-OBJECT:Levnamn:Size = NEW System.Drawing.Size(387, 28).
      THIS-OBJECT:Levnamn:TabIndex = 4.
      /*  */
      /* ButtonRE */
      /*  */
      appearance20:Image = CAST(resources:GetObject("appearance20.Image"), System.Object).
      THIS-OBJECT:ButtonRE:Appearance = appearance20.
      THIS-OBJECT:ButtonRE:ImageSize = NEW System.Drawing.Size(203, 51).
      THIS-OBJECT:ButtonRE:Location = NEW System.Drawing.Point(788, 3).
      THIS-OBJECT:ButtonRE:Name = "ButtonRE".
      THIS-OBJECT:ButtonRE:Size = NEW System.Drawing.Size(211, 60).
      THIS-OBJECT:ButtonRE:TabIndex = 2.
      THIS-OBJECT:ButtonRE:Click:Subscribe(THIS-OBJECT:ButtonRE_Click).
      /*  */
      /* ButtonON */
      /*  */
      appearance21:Image = CAST(resources:GetObject("appearance21.Image"), System.Object).
      THIS-OBJECT:ButtonON:Appearance = appearance21.
      THIS-OBJECT:ButtonON:ImageSize = NEW System.Drawing.Size(200, 51).
      THIS-OBJECT:ButtonON:Location = NEW System.Drawing.Point(294, 3).
      THIS-OBJECT:ButtonON:Name = "ButtonON".
      THIS-OBJECT:ButtonON:Size = NEW System.Drawing.Size(211, 60).
      THIS-OBJECT:ButtonON:TabIndex = 1.
      THIS-OBJECT:ButtonON:Click:Subscribe(THIS-OBJECT:ButtonON_Click).
      /*  */
      /* ButtonES */
      /*  */
      appearance22:Image = CAST(resources:GetObject("appearance22.Image"), System.Object).
      THIS-OBJECT:ButtonES:Appearance = appearance22.
      THIS-OBJECT:ButtonES:ImageSize = NEW System.Drawing.Size(201, 51).
      THIS-OBJECT:ButtonES:Location = NEW System.Drawing.Point(47, 3).
      THIS-OBJECT:ButtonES:Name = "ButtonES".
      THIS-OBJECT:ButtonES:Size = NEW System.Drawing.Size(211, 60).
      THIS-OBJECT:ButtonES:TabIndex = 0.
      THIS-OBJECT:ButtonES:Click:Subscribe(THIS-OBJECT:ButtonES_Click).
      /*  */
      /* ButtonJMF */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
      THIS-OBJECT:ButtonJMF:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:ButtonJMF:Location = NEW System.Drawing.Point(1072, 10).
      THIS-OBJECT:ButtonJMF:Name = "ButtonJMF".
      THIS-OBJECT:ButtonJMF:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonJMF:TabIndex = 7.
      THIS-OBJECT:ButtonJMF:Text = "Jämför".
      THIS-OBJECT:ButtonJMF:Click:Subscribe(THIS-OBJECT:ButtonJMF_Click).
      /*  */
      /* comboLev4 */
      /*  */
      THIS-OBJECT:comboLev4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboLev4:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboLev4:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboLev4:Location = NEW System.Drawing.Point(832, 10).
      THIS-OBJECT:comboLev4:Name = "comboLev4".
      THIS-OBJECT:comboLev4:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboLev4:TabIndex = 6.
      /*  */
      /* comboLev3 */
      /*  */
      THIS-OBJECT:comboLev3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboLev3:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboLev3:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboLev3:Location = NEW System.Drawing.Point(574, 10).
      THIS-OBJECT:comboLev3:Name = "comboLev3".
      THIS-OBJECT:comboLev3:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboLev3:TabIndex = 5.
      /*  */
      /* comboLev2 */
      /*  */
      THIS-OBJECT:comboLev2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboLev2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboLev2:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboLev2:Location = NEW System.Drawing.Point(316, 10).
      THIS-OBJECT:comboLev2:Name = "comboLev2".
      THIS-OBJECT:comboLev2:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboLev2:TabIndex = 4.
      /*  */
      /* comboLev1 */
      /*  */
      THIS-OBJECT:comboLev1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:comboLev1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:comboLev1:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:comboLev1:Location = NEW System.Drawing.Point(58, 10).
      THIS-OBJECT:comboLev1:Name = "comboLev1".
      THIS-OBJECT:comboLev1:Size = NEW System.Drawing.Size(200, 22).
      THIS-OBJECT:comboLev1:TabIndex = 3.
      /*  */
      /* GridJmfPris */
      /*  */
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Appearance = appearance6.
      THIS-OBJECT:GridJmfPris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridJmfPris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance7:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance7:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance7:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfPris:DisplayLayout:GroupByBox:Appearance = appearance7.
      appearance8:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJmfPris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance8.
      THIS-OBJECT:GridJmfPris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance9:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance9:BackColor2 = System.Drawing.SystemColors:Control.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJmfPris:DisplayLayout:GroupByBox:PromptAppearance = appearance9.
      THIS-OBJECT:GridJmfPris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridJmfPris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      appearance10:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:ActiveCellAppearance = appearance10.
      appearance11:BackColor = System.Drawing.SystemColors:Highlight.
      appearance11:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:ActiveRowAppearance = appearance11.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:CardAreaAppearance = appearance12.
      appearance13:BorderColor = System.Drawing.Color:Silver.
      appearance13:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:CellAppearance = appearance13.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:CellPadding = 0.
      appearance14:BackColor = System.Drawing.SystemColors:Control.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:GroupByRowAppearance = appearance14.
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:HeaderAppearance = appearance15.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance16:BackColor = System.Drawing.SystemColors:Window.
      appearance16:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:RowAppearance = appearance16.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance17:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridJmfPris:DisplayLayout:Override:TemplateAddRowAppearance = appearance17.
      THIS-OBJECT:GridJmfPris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridJmfPris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridJmfPris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridJmfPris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridJmfPris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridJmfPris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridJmfPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridJmfPris:GuruContext = ?.
      THIS-OBJECT:GridJmfPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridJmfPris:Name = "GridJmfPris".
      THIS-OBJECT:GridJmfPris:Size = NEW System.Drawing.Size(1194, 368).
      THIS-OBJECT:GridJmfPris:TabIndex = 0.
      THIS-OBJECT:GridJmfPris:Text = "grid1".
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(1, 20).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(196, 77).
      /*  */
      /* RexPostnr */
      /*  */
      THIS-OBJECT:RexPostnr:Location = NEW System.Drawing.Point(790, 7).
      THIS-OBJECT:RexPostnr:Name = "RexPostnr".
      THIS-OBJECT:RexPostnr:Size = NEW System.Drawing.Size(209, 20).
      THIS-OBJECT:RexPostnr:TabIndex = 9.
      /*  */
      /* MtrlJmfPris */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerJmfPris).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "MtrlJmfPris".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "MtrlJmfPris".
      THIS-OBJECT:Shown:Subscribe(THIS-OBJECT:MtrlJmfPris_Shown).
      THIS-OBJECT:splitContainerJmfPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerJmfPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerJmfPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerJmfPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSok:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSok:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSok:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerSok, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSok:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSaldoSok:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSaldoSok:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSaldoSok, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSaldoSok:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:comboLev4, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboLev3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboLev2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:comboLev1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridJmfPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD. 
 
	METHOD PUBLIC VOID ShowModalDialog(  ):
		
		WAIT-FOR THIS-OBJECT:ShowDialog().

	END METHOD.


 
END CLASS.
