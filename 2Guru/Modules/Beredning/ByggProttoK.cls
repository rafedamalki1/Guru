 
 /*------------------------------------------------------------------------
    File        : ByggProttoK
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu Jan 23 13:33:27 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Modules.Beredning.ByggProttoK INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE grid1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE KodSokInput AS System.Windows.Forms.TextBox NO-UNDO.
   DEFINE PUBLIC VARIABLE lop AS System.Windows.Forms.TextBox NO-UNDO.
   DEFINE PRIVATE VARIABLE splitBygg AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridByggProt AS Controls.GridJoin NO-UNDO.
   DEFINE VARIABLE Byggprottempbuffh AS HANDLE EXTENT 30 NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin             AS Controls.GridRubrikListaJoin                                      NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   
    DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   
   DEFINE VARIABLE ByggProttDSh AS HANDLE NO-UNDO.
	DEFINE VARIABLE antalDStabeller AS INTEGER NO-UNDO.	
	DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   
	CONSTRUCTOR PUBLIC ByggProttoK (  ):
		
		
      SUPER().
      InitializeComponent().
      THIS-OBJECT:SIZE = NEW System.Drawing.Size(Guru.Konstanter:globstorb,Guru.Konstanter:globstorh).
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Close().
      RETURN.
		
	END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID fillarb_KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
		DEFINE VARIABLE ArbKod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE Lopnr AS CHARACTER  NO-UNDO.
      
       DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      keykodestring = e:KEYCODE:ToString().
      
      IF keykodestring = "Return" THEN DO:
         
         ArbKod = THIS-OBJECT:KodSokInput:Text.
         Lopnr = THIS-OBJECT:lop:Text.
         /*
         THIS-OBJECT:GridByggProt:GuruRepositionTo(ArbKod, Lopnr).
         */
      END.
		RETURN.

	END METHOD.

	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridByggProt_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
     
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
      THIS-OBJECT:Byggprottempbuffh[11]:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde. 
      IF kolnamn = "UTFORT" OR kolnamn = "EJT" THEN. 
      ELSE DO: 
         IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN.
         ELSE DO:
            
            /*
            THIS-OBJECT:GridByggProt:ActiveRow:Cells[kolnamn]:VALUE  = kolvarde.
            
            THIS-OBJECT:Grid1:ActiveRow:Cells[kolnamn]:VALUE  = kolvarde.
            */
         END.
               
      END. 
      /*
      MESSAGE "CA" THIS-OBJECT:Grid1:ActiveRow:Cells["UTFORT"]:VALUE THIS-OBJECT:Grid1:ActiveRow:Cells["BENAMNING"]:VALUE
      VIEW-AS ALERT-BOX.
      */
		RETURN.

	END METHOD.

	
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridByggProt_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridByggProt_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvardeRI AS CHARACTER NO-UNDO.
      DEFINE VARIABLE nyvarde AS LOGICAL NO-UNDO. 
      
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      kolvardeRI = THIS-OBJECT:GridByggProt:ActiveRow:Cells["TTRECID"]:VALUE.
      THIS-OBJECT:Byggprottempbuffh[11]:FIND-FIRST("WHERE  TTRECID = " + STRING(kolvardeRI),NO-LOCK) NO-ERROR.
      IF kolnamn = "UTFORT" THEN DO:
          IF kolvarde = "FALSE" THEN THIS-OBJECT:GridByggProt:ActiveRow:Cells["UTFORT"]:VALUE = TRUE.
          ELSE THIS-OBJECT:GridByggProt:ActiveRow:Cells["UTFORT"]:VALUE = FALSE.
          /*
          IF kolvarde = "FALSE" THEN THIS-OBJECT:Grid1:ActiveRow:Cells["UTFORT"]:VALUE = TRUE.
          ELSE THIS-OBJECT:Grid1:ActiveRow:Cells["UTFORT"]:VALUE = FALSE.
          */
      END.
      ELSE IF kolnamn = "EJT" THEN DO:
         IF kolvarde = "FALSE" THEN THIS-OBJECT:GridByggProt:ActiveRow:Cells["EJT"]:VALUE = TRUE.
          ELSE THIS-OBJECT:GridByggProt:ActiveRow:Cells["EJT"]:VALUE = FALSE.
        /*
          IF kolvarde = "FALSE" THEN THIS-OBJECT:Grid1:ActiveRow:Cells["EJT"]:VALUE = TRUE.
          ELSE THIS-OBJECT:Grid1:ActiveRow:Cells["EJT"]:VALUE = FALSE.
         */
      END.
      ELSE  THIS-OBJECT:GridByggProt:ActiveRow:Cells[kolnamn]:VALUE  = kolvarde.
      
      
      /*
       Grid:PerformAction(UltraGridAction:EnterEditMode).
         IF kolnamn = "UTFORT" OR  kolnamn = "EJT" THEN DO:
            
            
            IF kolvarde = "FALSE" THEN nyvarde = TRUE.
            ELSE nyvarde = FALSE.
            THIS-OBJECT:GridByggProt:ActiveRow:Cells[kolnamn]:VALUE = nyvarde.
            kolvardeRI = THIS-OBJECT:GridByggProt:ActiveRow:Cells["TTRECID"]:VALUE.
            THIS-OBJECT:Byggprottempbuffh[11]:FIND-FIRST("WHERE  TTRECID = " + STRING(kolvardeRI),NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:Byggprottempbuffh[11]:AVAILABLE THEN DO:
               THIS-OBJECT:Byggprottempbuffh[11]:BUFFER-FIELD(kolnamn):BUFFER-VALUE = nyvarde.
            END.   
         END.
         /*
         ELSE DO:
            
            THIS-OBJECT:GridByggProt:ActiveRow:Cells[kolnamn]:VALUE = kolvarde.
         END.
         */   
      */
      
      /*
      kolvardeRI = THIS-OBJECT:GridByggProt:ActiveRow:Cells["TTRECID"]:VALUE.
      THIS-OBJECT:Byggprottempbuffh[11]:FIND-FIRST("WHERE  TTRECID = " + STRING(kolvardeRI),NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:Byggprottempbuffh[11]:AVAILABLE THEN DO:
         THIS-OBJECT:Byggprottempbuffh[11]:BUFFER-FIELD(kolnamn):BUFFER-VALUE = nyvarde.
         THIS-OBJECT:Byggprottempbuffh[11]:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      END.   
      */           

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	METHOD PUBLIC VOID InitiateByggPro():
      
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID ConnectDataSet(DATASET-HANDLE ByggProttDSh BIND, antalDStabellerIN AS INTEGER):
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE ttquery AS CHARACTER NO-UNDO.
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:WaitCursor.
      antalDStabeller = antalDStabellerIN.
      IF VALID-HANDLE(ByggProttDSh) THEN DO:
          iCount = 1.
         DO WHILE iCount <= antalDStabeller:
            THIS-OBJECT:Byggprottempbuffh[iCount] = ByggProttDSh:GET-BUFFER-HANDLE(iCount).
            iCount = iCount + 1.
         END.   
      END. 
      THIS-OBJECT:ByggProttDSh = ByggProttDSh.
      THIS-OBJECT:CreateGridByggProt().
     
     ttquery = "ByggVisaTT" + 
         " ,EACH ByggHuvTT WHERE ByggHuvTT.RID =  ByggVisaTT.TTRECID" + 
         " ,EACH ByggMtrlSamTT WHERE ByggMtrlSamTT.RID = ByggVisaTT.TTRECID" + 
         " ,EACH ByggUppTT WHERE ByggUppTT.RID = ByggVisaTT.TTRECID" + 
         
         " ,EACH ByggUppAnmTT WHERE ByggUppAnmTT.RID = ByggUppTT.TTRECID" + 
         " ,EACH ByggKonsTT WHERE ByggKonsTT.RID = ByggUppTT.TTRECID" + 

         " ,EACH ByggKablarTT WHERE ByggKablarTT.RID = ByggKonsTT.TTRECID" + 
         " ,EACH ByggKonstAnmTT WHERE ByggKonstAnmTT.RID = ByggKonsTT.TTRECID" + 
         " ,EACH ByggMtrlTT WHERE ByggMtrlTT.RID = ByggKonsTT.TTRECID" + 
         " ,EACH ByggKalkTT WHERE ByggKalkTT.RID = ByggKonsTT.TTRECID" + 
         " ,EACH ByggEgenKontTT WHERE ByggEgenKontTT.RID = ByggKonsTT.TTRECID".


/* 
      ttquery = "ByggVisaTT" + 
         " ,EACH ByggHuvTT WHERE  ByggHuvTT.RID =  ByggVisaTT.TTRECID" + 
         " ,EACH ByggKalkTT WHERE ByggKalkTT.RID = ByggHuvTT.TTRECID". 
*/

      /*
      THIS-OBJECT:GridByggProt:GridData(DATASET-HANDLE ByggProttDSh).      

*/      
      THIS-OBJECT:KodSokInput:Text = "109".
      THIS-OBJECT:lop:Text = "14".

      THIS-OBJECT:GridByggProt:GuruMulteQ(ttquery).

   END METHOD.
   METHOD PUBLIC VOID CreateGridByggProt():
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE tempNr AS INTEGER NO-UNDO.
      DEFINE VARIABLE decCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE hideBands AS INTEGER EXTENT 10 NO-UNDO.
      DEFINE VARIABLE sortBands AS INTEGER EXTENT 50 NO-UNDO.
      DEFINE VARIABLE sortCount AS INTEGER NO-UNDO INITIAL 1.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
     
      DEFINE VARIABLE ReadOnly AS LOGICAL NO-UNDO.
      
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      iCount = 1.
      DO WHILE iCount <= antalDStabeller:
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
         iCount = iCount + 1.
      END.
      
      /*start*/     
      iband = 0.
      tempNr = 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Beredning",TRUE,100).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
         THIS-OBJECT:GridByggProt:GuruGridRadOption(TRUE).
         
      /*huvud*/

      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("UPPGIFT","_",TRUE,200). 
         rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","_",TRUE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("DATUM","_",TRUE,60).
         rubrikergrid[4] = NEW Controls.GridRubrik("TID","_",TRUE,60).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 4,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
       
      /*mtrllista sunnat*/     
      iband =  iband + 1.
      tempNr = tempNr + 1. 
         rubrikergrid[1] = NEW Controls.GridRubrik("ARTIKEL","_",TRUE,65).
         rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","_",TRUE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("ENH","_",TRUE,50).
         rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","_",TRUE,80,"INT","").
         rubrikergrid[5] = NEW Controls.GridRubrik("PRIS","_",TRUE,50,"DEC","").
         rubrikergrid[6] = NEW Controls.GridRubrik("SUMMA","_",TRUE,50,"DEC","").
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 6,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
       
      /*upplag*/
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","_",TRUE,250).
         rubrikergrid[2] = NEW Controls.GridRubrik("EXTRATEXT","_",TRUE,250).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*uppanm*/
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("ANMARK","_",TRUE,500).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*Konst*/
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("KONST","_",TRUE,250).
         rubrikergrid[2] = NEW Controls.GridRubrik("KONSTDATA","_",TRUE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("TYP","_",TRUE,250).
         rubrikergrid[4] = NEW Controls.GridRubrik("TYPnr","_",TRUE,250).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 4,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*ByggKablarTT*/   
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("KABEL","_",TRUE,100).
         rubrikergrid[2] = NEW Controls.GridRubrik("B2","_",TRUE,100).
         rubrikergrid[3] = NEW Controls.GridRubrik("B3","_",TRUE,100).
         rubrikergrid[4] = NEW Controls.GridRubrik("B4","_",TRUE,100).
         rubrikergrid[5] = NEW Controls.GridRubrik("B5","_",TRUE,100).
         /*
         rubrikergrid[6] = NEW Controls.GridRubrik("B6","_",TRUE,100).
         */
         rubrikergrid[6] = NEW Controls.GridRubrik("ADRESS","_",TRUE,100).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 6,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*konstanm*/
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("ANMARK","_",TRUE,500).
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*konstmtr */     
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("ENR","_",TRUE,65).
         rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","_",TRUE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("ENH","_",TRUE,50).
         rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","_",TRUE,50,"INT","INT").
         rubrikergrid[5] = NEW Controls.GridRubrik("PRIS","_",TRUE,50,"DEC","").
         rubrikergrid[6] = NEW Controls.GridRubrik("SUMMA","_",TRUE,50,"DEC","").
          
       THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 6,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*konstkalk*/     
      iband =  iband + 1.
      tempNr = tempNr + 1.
         rubrikergrid[1] = NEW Controls.GridRubrik("ARBKODLOPNR","_",TRUE,65).
         rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","_",TRUE,250).
         rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","_",TRUE,50).
         rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","_",TRUE,50,"DEC", "KALKYL").
         
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 4,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      /*egenkontt*/     
     
      iband =  iband + 1.
      tempNr = tempNr + 1.
      IF Guru.GlobalaVariabler:Egengkontroll = TRUE THEN ReadOnly = FALSE.
      ELSE ReadOnly = TRUE. 
         rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","_",ReadOnly,400).
         rubrikergrid[2] = NEW Controls.GridRubrik("UTFORT","Utfört",ReadOnly,50).
         rubrikergrid[3] = NEW Controls.GridRubrik("EJT","Ej tillämplig",ReadOnly,50).
         rubrikergrid[4] = NEW Controls.GridRubrik("DATUM","_",ReadOnly,60).
         rubrikergrid[5] = NEW Controls.GridRubrik("SIGN","_",ReadOnly,70).
         
      /*
      tHIS-OBJECT:Grid1:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:Byggprottempbuffh[tempNr])).
      */
      
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 5,INPUT THIS-OBJECT:Byggprottempbuffh[tempNr]).
      THIS-OBJECT:GridByggProt:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,tempNr,DATASET-HANDLE ByggProttDSh).
     
      /*
      THIS-OBJECT:GridByggProt:GuruHidden(1,"ordning", TRUE).
       THIS-OBJECT:GridByggProt:GuruHiddenBand(hideBands[1], TRUE).
      
      */
      THIS-OBJECT:GridByggProt:Text = "Byggprotokoll".
      THIS-OBJECT:SkapaGrupper().
      Guru.GlobalaVariabler:ShowLevelsRow = 5.
    
      THIS-OBJECT:Cursor = System.Windows.Forms.Cursors:Default.
      /*placering av subgrids*/
      THIS-OBJECT:GridByggProt:SetViewStyleBand(TRUE).
      /*
      ByggProttDSh:WRITE-XML("FILE", "c:\exe.xml").
     */
    
       
  END METHOD.
  METHOD PUBLIC VOID SkapaGrupper():
     THIS-OBJECT:GridByggProt:SkapaGrupp(1,"Beredningsinfo","Beredningsinfo").
     THIS-OBJECT:GridByggProt:SetGroupOrd(1,"Beredningsinfo","UPPGIFT").
     THIS-OBJECT:GridByggProt:SetGroupOrd(1,"Beredningsinfo","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(1,"Beredningsinfo","DATUM").
     THIS-OBJECT:GridByggProt:SetGroupOrd(1,"Beredningsinfo","TID").
     THIS-OBJECT:GridByggProt:SkapaGrupp(2,"Sumtrl","Materielsamanställning").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","ARTIKEL").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","ENH").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","ANTAL").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","PRIS").
     THIS-OBJECT:GridByggProt:SetGroupOrd(2,"Sumtrl","SUMMA").
     THIS-OBJECT:GridByggProt:SkapaGrupp(3,"Uppl","Upplag").
     THIS-OBJECT:GridByggProt:SetGroupOrd(3,"Uppl","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(3,"Uppl","EXTRATEXT").
     THIS-OBJECT:GridByggProt:SkapaGrupp(4,"UpplAnm","Anmärkning").
     THIS-OBJECT:GridByggProt:SetGroupOrd(4,"UpplAnm","ANMARK").
     THIS-OBJECT:GridByggProt:SkapaGrupp(5,"Konst","Konstruktion").
     THIS-OBJECT:GridByggProt:SetGroupOrd(5,"Konst","KONST").
     THIS-OBJECT:GridByggProt:SetGroupOrd(5,"Konst","KONSTDATA").
     THIS-OBJECT:GridByggProt:SetGroupOrd(5,"Konst","TYP").
     THIS-OBJECT:GridByggProt:SetGroupOrd(5,"Konst","TYPnr").
     THIS-OBJECT:GridByggProt:SkapaGrupp(6,"Kablar","Kablar").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","KABEL").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","B2").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","B3").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","B4").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","B5").
     THIS-OBJECT:GridByggProt:SetGroupOrd(6,"Kablar","ADRESS").
     THIS-OBJECT:GridByggProt:SkapaGrupp(7,"KonstAnm","Anmärkning").
     THIS-OBJECT:GridByggProt:SetGroupOrd(7,"KonstAnm","ANMARK").
     THIS-OBJECT:GridByggProt:SkapaGrupp(8,"Konstmtrl","Materiel").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","ENR").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","ENH").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","ANTAL").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","PRIS").
     THIS-OBJECT:GridByggProt:SetGroupOrd(8,"Konstmtrl","SUMMA").
     THIS-OBJECT:GridByggProt:SkapaGrupp(9,"Kalkyl","Kalkyl").
     THIS-OBJECT:GridByggProt:SetGroupOrd(9,"Kalkyl","ARBKODLOPNR").
     THIS-OBJECT:GridByggProt:SetGroupOrd(9,"Kalkyl","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(9,"Kalkyl","ENHET"). 
     THIS-OBJECT:GridByggProt:SetGroupOrd(9,"Kalkyl","ANTAL").
     THIS-OBJECT:GridByggProt:SkapaGrupp(10,"Egenk","Egenkontroll enligt ovan").
     THIS-OBJECT:GridByggProt:SetGroupOrd(10,"Egenk","BENAMNING").
     THIS-OBJECT:GridByggProt:SetGroupOrd(10,"Egenk","UTFORT").
     THIS-OBJECT:GridByggProt:SetGroupOrd(10,"Egenk","EJT").
     THIS-OBJECT:GridByggProt:SetGroupOrd(10,"Egenk","DATUM").
     THIS-OBJECT:GridByggProt:SetGroupOrd(10,"Egenk","SIGN").
    
  END METHOD. 
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Beredning.ByggProttoK").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("ByggVisaTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("ByggHuvTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc3 = NEW Progress.Data.TableDesc("ByggMtrlSamTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc4 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc4 = NEW Progress.Data.TableDesc("ByggUppTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc5 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc5 = NEW Progress.Data.TableDesc("ByggUppAnmTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc6 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc6 = NEW Progress.Data.TableDesc("ByggKonsTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc7 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc7 = NEW Progress.Data.TableDesc("ByggKablarTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc8 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc8 = NEW Progress.Data.TableDesc("ByggKonstAnmTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc9 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc9 = NEW Progress.Data.TableDesc("ByggMtrlTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc10 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc10 = NEW Progress.Data.TableDesc("ByggKalkTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc11 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc11 = NEW Progress.Data.TableDesc("ByggEgenKontTT").
      THIS-OBJECT:splitBygg = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:grid1 = NEW Controls.Grid().
      THIS-OBJECT:GridByggProt = NEW Controls.GridJoin().
      THIS-OBJECT:lop = NEW System.Windows.Forms.TextBox().
      THIS-OBJECT:KodSokInput = NEW System.Windows.Forms.TextBox().
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      CAST(THIS-OBJECT:splitBygg, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitBygg:Panel1:SuspendLayout().
      THIS-OBJECT:splitBygg:Panel2:SuspendLayout().
      THIS-OBJECT:splitBygg:SuspendLayout().
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridByggProt, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitBygg */
      /*  */
      THIS-OBJECT:splitBygg:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitBygg:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitBygg:Name = "splitBygg".
      THIS-OBJECT:splitBygg:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitBygg.Panel1 */
      /*  */
      THIS-OBJECT:splitBygg:Panel1:Controls:Add(THIS-OBJECT:grid1).
      THIS-OBJECT:splitBygg:Panel1:Controls:Add(THIS-OBJECT:GridByggProt).
      /*  */
      /* splitBygg.Panel2 */
      /*  */
      THIS-OBJECT:splitBygg:Panel2:Controls:Add(THIS-OBJECT:lop).
      THIS-OBJECT:splitBygg:Panel2:Controls:Add(THIS-OBJECT:KodSokInput).
      THIS-OBJECT:splitBygg:Panel2:Controls:Add(THIS-OBJECT:ButtonOK).
      THIS-OBJECT:splitBygg:Size = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:splitBygg:SplitterDistance = 818.
      THIS-OBJECT:splitBygg:TabIndex = 0.
      /*  */
      /* grid1 */
      /*  */
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance1:AlphaLevel = System.Convert:ToInt16(111).
      appearance1:BackColor = System.Drawing.SystemColors:Highlight.
      appearance1:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveCellAppearance = appearance1.
      appearance2:AlphaLevel = System.Convert:ToInt16(111).
      appearance2:BackColor = System.Drawing.SystemColors:Highlight.
      appearance2:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:grid1:DisplayLayout:Override:ActiveRowAppearance = appearance2.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:grid1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:grid1:GuruContext = ?.
      THIS-OBJECT:grid1:Location = NEW System.Drawing.Point(622, 108).
      THIS-OBJECT:grid1:Name = "grid1".
      THIS-OBJECT:grid1:Size = NEW System.Drawing.Size(550, 322).
      THIS-OBJECT:grid1:TabIndex = 3.
      THIS-OBJECT:grid1:Text = "grid1".
      THIS-OBJECT:grid1:AfterCellUpdate:Subscribe(THIS-OBJECT:GridByggProt_AfterCellUpdate).
      THIS-OBJECT:grid1:CellChange:Subscribe(THIS-OBJECT:GridByggProt_CellChange).
      THIS-OBJECT:grid1:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridByggProt_BeforeCellUpdate).
      /*  */
      /* GridByggProt */
      /*  */
      appearance3:BackColor = System.Drawing.SystemColors:Window.
      appearance3:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridByggProt:DisplayLayout:Appearance = appearance3.
      THIS-OBJECT:GridByggProt:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridByggProt:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance4:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridByggProt:DisplayLayout:GroupByBox:Appearance = appearance4.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridByggProt:DisplayLayout:GroupByBox:BandLabelAppearance = appearance5.
      THIS-OBJECT:GridByggProt:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance6:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance6:BackColor2 = System.Drawing.SystemColors:Control.
      appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridByggProt:DisplayLayout:GroupByBox:PromptAppearance = appearance6.
      THIS-OBJECT:GridByggProt:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridByggProt:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      appearance7:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:ActiveCellAppearance = appearance7.
      appearance8:BackColor = System.Drawing.SystemColors:Highlight.
      appearance8:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:ActiveRowAppearance = appearance8.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance9:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:CardAreaAppearance = appearance9.
      appearance10:BorderColor = System.Drawing.Color:Silver.
      appearance10:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:CellAppearance = appearance10.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:CellPadding = 0.
      appearance11:BackColor = System.Drawing.SystemColors:Control.
      appearance11:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance11:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance11:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance11:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:GroupByRowAppearance = appearance11.
      appearance12:TextHAlignAsString = "Left".
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:HeaderAppearance = appearance12.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:RowAppearance = appearance13.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridByggProt:DisplayLayout:Override:TemplateAddRowAppearance = appearance14.
      THIS-OBJECT:GridByggProt:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridByggProt:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridByggProt:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridByggProt:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridByggProt:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridByggProt:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridByggProt:Dock = System.Windows.Forms.DockStyle:Left.
      THIS-OBJECT:GridByggProt:GuruContext = ?.
      THIS-OBJECT:GridByggProt:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridByggProt:Name = "GridByggProt".
      THIS-OBJECT:GridByggProt:Size = NEW System.Drawing.Size(600, 818).
      THIS-OBJECT:GridByggProt:TabIndex = 2.
      THIS-OBJECT:GridByggProt:Text = "Byggprotokoll".
      THIS-OBJECT:GridByggProt:AfterCellUpdate:Subscribe(THIS-OBJECT:GridByggProt_AfterCellUpdate).
      THIS-OBJECT:GridByggProt:CellChange:Subscribe(THIS-OBJECT:GridByggProt_CellChange).
      THIS-OBJECT:GridByggProt:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridByggProt_BeforeCellUpdate).
      /*  */
      /* lop */
      /*  */
      THIS-OBJECT:lop:Location = NEW System.Drawing.Point(534, 10).
      THIS-OBJECT:lop:Name = "lop".
      THIS-OBJECT:lop:Size = NEW System.Drawing.Size(116, 20).
      THIS-OBJECT:lop:TabIndex = 8.
      THIS-OBJECT:lop:KeyDown:Subscribe(THIS-OBJECT:fillarb_KeyDown).
      /*  */
      /* KodSokInput */
      /*  */
      THIS-OBJECT:KodSokInput:Location = NEW System.Drawing.Point(20, 10).
      THIS-OBJECT:KodSokInput:Name = "KodSokInput".
      THIS-OBJECT:KodSokInput:Size = NEW System.Drawing.Size(116, 20).
      THIS-OBJECT:KodSokInput:TabIndex = 7.
      THIS-OBJECT:KodSokInput:KeyDown:Subscribe(THIS-OBJECT:fillarb_KeyDown).
      /*  */
      /* ButtonOK */
      /*  */
      THIS-OBJECT:ButtonOK:Anchor = System.Windows.Forms.AnchorStyles:Right.
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance15.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(1062, 5).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 4.
      THIS-OBJECT:ButtonOK:TabStop = FALSE.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* bindingSource1 */
      /*  */
      THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
      THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
      THIS-OBJECT:bindingSource1:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
      arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("UPPGIFT", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("DATUM", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("TID", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc2:Columns = arrayvar1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc3:ChildTables = arrayvar2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
      arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("ARTIKEL", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("ENH", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[4] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[5] = NEW Progress.Data.ColumnPropDesc("PRIS", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[6] = NEW Progress.Data.ColumnPropDesc("SUMMA", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[7] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar3[8] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar3[9] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc3:Columns = arrayvar3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar4 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc5:ChildTables = arrayvar4.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
      arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("ANMARK", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar5[3] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar5[4] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc5:Columns = arrayvar5.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar6 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc7:ChildTables = arrayvar6.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
      arrayvar7[1] = NEW Progress.Data.ColumnPropDesc("KABEL", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[2] = NEW Progress.Data.ColumnPropDesc("B2", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[3] = NEW Progress.Data.ColumnPropDesc("B3", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[4] = NEW Progress.Data.ColumnPropDesc("B4", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[5] = NEW Progress.Data.ColumnPropDesc("B5", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[6] = NEW Progress.Data.ColumnPropDesc("B6", "", Progress.Data.DataType:INTEGER).
      arrayvar7[7] = NEW Progress.Data.ColumnPropDesc("ADRESS", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[8] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar7[9] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar7[10] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc7:Columns = arrayvar7.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar8 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar8 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc8:ChildTables = arrayvar8.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar9 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
      arrayvar9[1] = NEW Progress.Data.ColumnPropDesc("ANMARK", "", Progress.Data.DataType:CHARACTER).
      arrayvar9[2] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar9[3] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar9[4] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc8:Columns = arrayvar9.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar10 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar10 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc9:ChildTables = arrayvar10.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar11 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
      arrayvar11[1] = NEW Progress.Data.ColumnPropDesc("ENR", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[3] = NEW Progress.Data.ColumnPropDesc("ENH", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[4] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[5] = NEW Progress.Data.ColumnPropDesc("PRIS", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[6] = NEW Progress.Data.ColumnPropDesc("SUMMA", "", Progress.Data.DataType:CHARACTER).
      arrayvar11[7] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar11[8] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar11[9] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc9:Columns = arrayvar11.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar12 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar12 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc10:ChildTables = arrayvar12.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar13 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
      arrayvar13[1] = NEW Progress.Data.ColumnPropDesc("ARBKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[2] = NEW Progress.Data.ColumnPropDesc("ARBKOD", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[3] = NEW Progress.Data.ColumnPropDesc("LOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[4] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[5] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[6] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar13[7] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar13[8] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar13[9] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc10:Columns = arrayvar13.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar14 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar14 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc11:ChildTables = arrayvar14.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar15 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
      arrayvar15[1] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar15[2] = NEW Progress.Data.ColumnPropDesc("UTFORT", "", Progress.Data.DataType:CHARACTER).
      arrayvar15[3] = NEW Progress.Data.ColumnPropDesc("EJT", "", Progress.Data.DataType:CHARACTER).
      arrayvar15[4] = NEW Progress.Data.ColumnPropDesc("DATUM", "", Progress.Data.DataType:CHARACTER).
      arrayvar15[5] = NEW Progress.Data.ColumnPropDesc("SIGN", "", Progress.Data.DataType:CHARACTER).
      arrayvar15[6] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar15[7] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar15[8] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc11:Columns = arrayvar15.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar16 AS Progress.Data.TableDesc EXTENT 5 NO-UNDO.
      arrayvar16[1] = tableDesc7.
      arrayvar16[2] = tableDesc8.
      arrayvar16[3] = tableDesc9.
      arrayvar16[4] = tableDesc10.
      arrayvar16[5] = tableDesc11.
      tableDesc6:ChildTables = arrayvar16.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar17 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
      arrayvar17[1] = NEW Progress.Data.ColumnPropDesc("KONST", "", Progress.Data.DataType:CHARACTER).
      arrayvar17[2] = NEW Progress.Data.ColumnPropDesc("KONSTDATA", "", Progress.Data.DataType:CHARACTER).
      arrayvar17[3] = NEW Progress.Data.ColumnPropDesc("TYP", "", Progress.Data.DataType:CHARACTER).
      arrayvar17[4] = NEW Progress.Data.ColumnPropDesc("TYPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar17[5] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar17[6] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar17[7] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc6:Columns = arrayvar17.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar18 AS Progress.Data.TableDesc EXTENT 2 NO-UNDO.
      arrayvar18[1] = tableDesc5.
      arrayvar18[2] = tableDesc6.
      tableDesc4:ChildTables = arrayvar18.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar19 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
      arrayvar19[1] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar19[2] = NEW Progress.Data.ColumnPropDesc("EXTRATEXT", "", Progress.Data.DataType:CHARACTER).
      arrayvar19[3] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar19[4] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar19[5] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc4:Columns = arrayvar19.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar20 AS Progress.Data.TableDesc EXTENT 3 NO-UNDO.
      arrayvar20[1] = tableDesc2.
      arrayvar20[2] = tableDesc3.
      arrayvar20[3] = tableDesc4.
      tableDesc1:ChildTables = arrayvar20.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar21 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar21[1] = NEW Progress.Data.ColumnPropDesc("ORDNING", "", Progress.Data.DataType:INTEGER).
      arrayvar21[2] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      arrayvar21[3] = NEW Progress.Data.ColumnPropDesc("RID", "", Progress.Data.DataType:RECID).
      tableDesc1:Columns = arrayvar21.
      THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
      /*  */
      /* ByggProttoK */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitBygg).
      THIS-OBJECT:Name = "ByggProttoK".
      THIS-OBJECT:Text = "Byggprotokoll".
      THIS-OBJECT:splitBygg:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitBygg:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitBygg:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitBygg, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitBygg:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:grid1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridByggProt, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.


	DESTRUCTOR PUBLIC ByggProttoK ( ):
      Guru.GlobalaVariabler:ShowLevelsRow = 0.
	END DESTRUCTOR.

END CLASS.