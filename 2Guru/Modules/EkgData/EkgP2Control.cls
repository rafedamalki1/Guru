 
 /*------------------------------------------------------------------------
    File        : EkgP2Control
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Wed Oct 09 09:57:47 CEST 2023
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.EkgData.EkgP2Control INHERITS UserControl: 
 
   DEFINE PRIVATE VARIABLE copyP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE flyttaP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP2M AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Lista AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE p2ListaToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P2ListaStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p2materielSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2materielSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p2arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2MaterielToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p2ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p2KoderToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p2KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p2FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP2arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3ForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Mtrl AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridMtrlForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverp2Mtrl AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP2resursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP2Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP2MtrlBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP2FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP2Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE P2ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP2F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP2ListaGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP2Kompin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP2Lista AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2Lista AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP2 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP2arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP2ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP2Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2materiel AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP2 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP2Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP2Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP2Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   
  CONSTRUCTOR PUBLIC EkgP2Control (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      /*flyttaP2Button:VISIBLE = FALSE.*/
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
	METHOD PRIVATE VOID copyP2Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:copyP2Pop:Show().
      THIS-OBJECT:ControlShell:fillinP2coplopnr:GuruText:Select().
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      /*P2*/
      THIS-OBJECT:CreateGridP3ForP2().
      THIS-OBJECT:CreateGridP2().
      THIS-OBJECT:CreateGridP2Resurs().
      THIS-OBJECT:CreateGridP2Frekvens().
      THIS-OBJECT:CreateGridP2Mtrl().
      THIS-OBJECT:CreateGridMtrlForP2().
      THIS-OBJECT:CreateGridResursForP2().
      THIS-OBJECT:CreateGridP2arbkod().
      THIS-OBJECT:CreateGridImportP2().
      THIS-OBJECT:CreateGridP2Lista().
   END METHOD.
   METHOD PUBLIC VOID CreateGridP3ForP2(): 
      {GridRubrikListaVarExtent.i} 

      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:GridP3ForP2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ControlDataDb:ekgp3TTh)).
      THIS-OBJECT:GridP3ForP2:Text = "P3-Koder".
      THIS-OBJECT:GridP3ForP2:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP3ForP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP3ForP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   METHOD PUBLIC VOID CreateGridP2():
      {GridRubrikListaVarExtent.i} 
  
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",FALSE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("BERTIM","",TRUE,70,"DEC").  
      rubrikergrid[9] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC"). 
      rubrikergrid[10] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,250).
      
      THIS-OBJECT:GridP2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 22, INPUT THIS-OBJECT:ControlDataDb:ekgp2TTh)).
      THIS-OBJECT:GridP2:Text = "P2-Koder".
      THIS-OBJECT:GridP2:GuruUpdateTitle().
      THIS-OBJECT:GridP2:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP2).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      THIS-OBJECT:GridP2:DisplayLayout:Bands[0]:Columns["ANMARKNING"]:Hidden = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("P2ENHET","",TRUE,100).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,70).
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,75,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("UTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
     
      THIS-OBJECT:GridP2Resurs:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 20, INPUT THIS-OBJECT:ControlDataDb:ekgp2resursTTh)).
      THIS-OBJECT:GridP2Resurs:Text = "P2-Resurser".
      THIS-OBJECT:GridP2Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Frekvens(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,50).
      rubrikergrid[7] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[8] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FBERTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FUTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FUTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("FEAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").
      
      THIS-OBJECT:GridP2Frekvens:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 22, INPUT THIS-OBJECT:ControlDataDb:ekgp2frekvensTTh)).
      THIS-OBJECT:GridP2Frekvens:Text = "P2-Frekvenser".
      THIS-OBJECT:GridP2Frekvens:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Mtrl():
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
      
      THIS-OBJECT:GridP2Mtrl:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 11, INPUT THIS-OBJECT:ControlDataDb:ekgp2mtrlTTh)).
      THIS-OBJECT:GridP2Mtrl:Text = "P2-Materiel".
      THIS-OBJECT:GridP2Mtrl:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["LEVKOD"]:Hidden = TRUE.
      THIS-OBJECT:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2Mtrl:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMtrlForP2():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",TRUE,230).
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("BASPRIS","",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,70,"DEC").

      THIS-OBJECT:GridMtrlForP2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:ControlDataDb:ekgmtrlTTh)).
      THIS-OBJECT:GridMtrlForP2:Text = "Materiel".
      THIS-OBJECT:GridMtrlForP2:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridMtrlForP2:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE.
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResursForP2():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      
      THIS-OBJECT:GridResursForP2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgresursForTTh)).
      THIS-OBJECT:GridResursForP2:Text = "Resurser".
      THIS-OBJECT:GridResursForP2:GuruUpdateTitle().
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2arbkod():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,150).
      
      THIS-OBJECT:GridP2arbkod:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgp2arbkodTTh)).
      THIS-OBJECT:GridP2arbkod:Text = "P2-Arbetskoder".
      THIS-OBJECT:GridP2arbkod:GuruUpdateTitle().
      THIS-OBJECT:GridP2arbkod:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP2ArbKod).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP2arbkod:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2arbkod:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP2(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,150).
      rubrikergrid[8] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").      
      rubrikergrid[9] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).
      THIS-OBJECT:GridImportP2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 9, INPUT THIS-OBJECT:ControlDataDb:importP2TTh)).
      THIS-OBJECT:GridImportP2:Text = "P2 Kod Import".
      THIS-OBJECT:GridImportP2:GuruUpdateTitle().
      
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP2).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[6] = NEW Controls.GridRubrik("P3BENAMNING","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC"). 
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).     
      THIS-OBJECT:GridImportP2F:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 8, INPUT THIS-OBJECT:ControlDataDb:importP2P3TTh)).
      THIS-OBJECT:GridImportP2F:Text = "P2 Frekvens Import".
      THIS-OBJECT:GridImportP2F:GuruUpdateTitle().
      THIS-OBJECT:GridImportP2F:VISIBLE = FALSE. 
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP2F).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ARTNR","",TRUE,90).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).
      
      THIS-OBJECT:GridImportP2M:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:importP2MTTh)).
      THIS-OBJECT:GridImportP2M:Text = "P2 Materiel Import".
      THIS-OBJECT:GridImportP2M:GuruUpdateTitle().
      THIS-OBJECT:GridImportP2M:VISIBLE = FALSE. 
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP2M).      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2Lista():
      {GridRubrikListaVarExtent.i}               
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").            
      rubrikergrid[3] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,50, "EKGLOP").           
      rubrikergrid[5] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).                 
      rubrikergrid[6] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).                          
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,150).
      rubrikergrid[9] = NEW Controls.GridRubrik("RANTAL","Resursantal",TRUE,75,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("TOTANTAL","",TRUE,90,"DEC").            
      rubrikergrid[11] = NEW Controls.GridRubrik("BERTIM","",TRUE,50,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("MONTTIM","",TRUE,50,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKTIM","",TRUE,50,"DEC").      
      rubrikergrid[14] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,50,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      /*rubrikergrid[17] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").*/
      rubrikergrid[16] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      /*rubrikergrid[20] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").    */
      rubrikergrid[19] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
           
      THIS-OBJECT:GridP2Lista:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 22, INPUT THIS-OBJECT:ControlDataDb:ekgp1detaljTTh)).
      THIS-OBJECT:GridP2Lista:Text = "P2-Kod Detaljerad Lista".
      THIS-OBJECT:GridP2Lista:GuruUpdateTitle().
      /*THIS-OBJECT:GridP1Lista:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP1).*/
      
      
      
   END METHOD.
   
   

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID flyttaP2Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):

		THIS-OBJECT:ControlShell:flyttaP2Pop:Show().
      THIS-OBJECT:ControlShell:fillinP2flyttalopnr:GuruText:Select().      
		RETURN.

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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP2Control").
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
      appearance75 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
      appearance76 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance77 AS Infragistics.Win.Appearance NO-UNDO.
      appearance77 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
      appearance78 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
      appearance79 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance80 AS Infragistics.Win.Appearance NO-UNDO.
      appearance80 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance81 AS Infragistics.Win.Appearance NO-UNDO.
      appearance81 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance82 AS Infragistics.Win.Appearance NO-UNDO.
      appearance82 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance83 AS Infragistics.Win.Appearance NO-UNDO.
      appearance83 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance84 AS Infragistics.Win.Appearance NO-UNDO.
      appearance84 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance85 AS Infragistics.Win.Appearance NO-UNDO.
      appearance85 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance86 AS Infragistics.Win.Appearance NO-UNDO.
      appearance86 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance87 AS Infragistics.Win.Appearance NO-UNDO.
      appearance87 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance88 AS Infragistics.Win.Appearance NO-UNDO.
      appearance88 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance89 AS Infragistics.Win.Appearance NO-UNDO.
      appearance89 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance90 AS Infragistics.Win.Appearance NO-UNDO.
      appearance90 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance91 AS Infragistics.Win.Appearance NO-UNDO.
      appearance91 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance92 AS Infragistics.Win.Appearance NO-UNDO.
      appearance92 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance93 AS Infragistics.Win.Appearance NO-UNDO.
      appearance93 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance94 AS Infragistics.Win.Appearance NO-UNDO.
      appearance94 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance95 AS Infragistics.Win.Appearance NO-UNDO.
      appearance95 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance96 AS Infragistics.Win.Appearance NO-UNDO.
      appearance96 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance97 AS Infragistics.Win.Appearance NO-UNDO.
      appearance97 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance98 AS Infragistics.Win.Appearance NO-UNDO.
      appearance98 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
      appearance99 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
      appearance100 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
      appearance105 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance106 AS Infragistics.Win.Appearance NO-UNDO.
      appearance106 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance107 AS Infragistics.Win.Appearance NO-UNDO.
      appearance107 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance108 AS Infragistics.Win.Appearance NO-UNDO.
      appearance108 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance109 AS Infragistics.Win.Appearance NO-UNDO.
      appearance109 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance110 AS Infragistics.Win.Appearance NO-UNDO.
      appearance110 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance111 AS Infragistics.Win.Appearance NO-UNDO.
      appearance111 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
      appearance112 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
      appearance113 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance114 AS Infragistics.Win.Appearance NO-UNDO.
      appearance114 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance115 AS Infragistics.Win.Appearance NO-UNDO.
      appearance115 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance116 AS Infragistics.Win.Appearance NO-UNDO.
      appearance116 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance117 AS Infragistics.Win.Appearance NO-UNDO.
      appearance117 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance118 AS Infragistics.Win.Appearance NO-UNDO.
      appearance118 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance119 AS Infragistics.Win.Appearance NO-UNDO.
      appearance119 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance120 AS Infragistics.Win.Appearance NO-UNDO.
      appearance120 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance121 AS Infragistics.Win.Appearance NO-UNDO.
      appearance121 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance122 AS Infragistics.Win.Appearance NO-UNDO.
      appearance122 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance123 AS Infragistics.Win.Appearance NO-UNDO.
      appearance123 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance124 AS Infragistics.Win.Appearance NO-UNDO.
      appearance124 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance125 AS Infragistics.Win.Appearance NO-UNDO.
      appearance125 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance126 AS Infragistics.Win.Appearance NO-UNDO.
      appearance126 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance127 AS Infragistics.Win.Appearance NO-UNDO.
      appearance127 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance128 AS Infragistics.Win.Appearance NO-UNDO.
      appearance128 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance129 AS Infragistics.Win.Appearance NO-UNDO.
      appearance129 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance130 AS Infragistics.Win.Appearance NO-UNDO.
      appearance130 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance131 AS Infragistics.Win.Appearance NO-UNDO.
      appearance131 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance132 AS Infragistics.Win.Appearance NO-UNDO.
      appearance132 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance133 AS Infragistics.Win.Appearance NO-UNDO.
      appearance133 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance134 AS Infragistics.Win.Appearance NO-UNDO.
      appearance134 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance135 AS Infragistics.Win.Appearance NO-UNDO.
      appearance135 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance136 AS Infragistics.Win.Appearance NO-UNDO.
      appearance136 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance137 AS Infragistics.Win.Appearance NO-UNDO.
      appearance137 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance138 AS Infragistics.Win.Appearance NO-UNDO.
      appearance138 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance139 AS Infragistics.Win.Appearance NO-UNDO.
      appearance139 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance140 AS Infragistics.Win.Appearance NO-UNDO.
      appearance140 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance141 AS Infragistics.Win.Appearance NO-UNDO.
      appearance141 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance142 AS Infragistics.Win.Appearance NO-UNDO.
      appearance142 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance143 AS Infragistics.Win.Appearance NO-UNDO.
      appearance143 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance144 AS Infragistics.Win.Appearance NO-UNDO.
      appearance144 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance145 AS Infragistics.Win.Appearance NO-UNDO.
      appearance145 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance146 AS Infragistics.Win.Appearance NO-UNDO.
      appearance146 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance147 AS Infragistics.Win.Appearance NO-UNDO.
      appearance147 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance148 AS Infragistics.Win.Appearance NO-UNDO.
      appearance148 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance149 AS Infragistics.Win.Appearance NO-UNDO.
      appearance149 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance150 AS Infragistics.Win.Appearance NO-UNDO.
      appearance150 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab7 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab7 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance151 AS Infragistics.Win.Appearance NO-UNDO.
      appearance151 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab8 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab8 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance152 AS Infragistics.Win.Appearance NO-UNDO.
      appearance152 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab9 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab9 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance153 AS Infragistics.Win.Appearance NO-UNDO.
      appearance153 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance154 AS Infragistics.Win.Appearance NO-UNDO.
      appearance154 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab28 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab28 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance155 AS Infragistics.Win.Appearance NO-UNDO.
      appearance155 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance156 AS Infragistics.Win.Appearance NO-UNDO.
      appearance156 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabP2resurs = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p2ResursSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2ResursSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridResursForP2 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP2resursBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP2Resurs = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP2Resurs = NEW Controls.Grid().
      THIS-OBJECT:TabP2frekvens = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p2FrekvensSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2FrekvensSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridP3ForP2 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP2FrekvensBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP2Frekvens = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP2Frekvens = NEW Controls.Grid().
      THIS-OBJECT:TabP2materiel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p2materielSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2materielSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridMtrlForP2 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP2MtrlBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverp2Mtrl = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP2Mtrl = NEW Controls.Grid().
      THIS-OBJECT:TabP2Lista = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP2ListaGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P2ListaStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP2Lista = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p2ListaToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:gridP2Lista = NEW Controls.Grid().
      THIS-OBJECT:TabP2Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP2ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P2ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP2Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p2KoderToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p2MaterielToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p2FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP2Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP2Kompin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP2Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP2Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP2M = NEW Controls.Grid().
      THIS-OBJECT:GridImportP2F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP2 = NEW Controls.Grid().
      THIS-OBJECT:p2MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2kodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2arbkodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2arbkodStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP2arbkod = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP2arbkodButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP2arbkod = NEW Controls.Grid().
      THIS-OBJECT:p2KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p2KodertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP2Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:copyP2Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:flyttaP2Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP2Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP2 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP2resurs:SuspendLayout().
      CAST(THIS-OBJECT:p2ResursSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2ResursSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2ResursSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2ResursSC:SuspendLayout().
      CAST(THIS-OBJECT:p2ResursSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2ResursSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p2ResursSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p2ResursSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForP2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP2Resurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP2frekvens:SuspendLayout().
      CAST(THIS-OBJECT:p2FrekvensSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2FrekvensSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2FrekvensSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2FrekvensSC:SuspendLayout().
      CAST(THIS-OBJECT:p2FrekvensSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2FrekvensSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p2FrekvensSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p2FrekvensSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridP3ForP2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP2Frekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP2materiel:SuspendLayout().
      CAST(THIS-OBJECT:p2materielSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2materielSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2materielSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2materielSC:SuspendLayout().
      CAST(THIS-OBJECT:p2materielSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2materielSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p2materielSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p2materielSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridMtrlForP2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP2Mtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP2Lista:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP2ListaGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP2ListaGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP2ListaGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP2ListaGrund:SuspendLayout().
      THIS-OBJECT:P2ListaStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP2Lista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP2Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP2ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP2ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP2ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP2ImportGrund:SuspendLayout().
      THIS-OBJECT:P2ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP2Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP2Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP2Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP2M, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP2F, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p2MainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2MainSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2MainSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2MainSC:SuspendLayout().
      CAST(THIS-OBJECT:p2kodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2kodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2kodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2kodSC:SuspendLayout().
      CAST(THIS-OBJECT:p2arbkodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2arbkodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2arbkodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2arbkodSC:SuspendLayout().
      THIS-OBJECT:p2arbkodStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP2arbkod, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p2KoderMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2KoderMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:p2KoderMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:p2KoderMenuSC:SuspendLayout().
      THIS-OBJECT:p2KodertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerP2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerP2:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabP2resurs */
      /*  */
      THIS-OBJECT:TabP2resurs:Controls:Add(THIS-OBJECT:p2ResursSC).
      THIS-OBJECT:TabP2resurs:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabP2resurs:Name = "TabP2resurs".
      THIS-OBJECT:TabP2resurs:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p2ResursSC */
      /*  */
      THIS-OBJECT:p2ResursSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2ResursSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2ResursSC:Name = "p2ResursSC".
      /*  */
      /* p2ResursSC.Panel1 */
      /*  */
      THIS-OBJECT:p2ResursSC:Panel1:Controls:Add(THIS-OBJECT:p2ResursSC2).
      /*  */
      /* p2ResursSC.Panel2 */
      /*  */
      THIS-OBJECT:p2ResursSC:Panel2:Controls:Add(THIS-OBJECT:gridP2Resurs).
      THIS-OBJECT:p2ResursSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p2ResursSC:SplitterDistance = 349.
      THIS-OBJECT:p2ResursSC:TabIndex = 1.
      /*  */
      /* p2ResursSC2 */
      /*  */
      THIS-OBJECT:p2ResursSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2ResursSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p2ResursSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2ResursSC2:Name = "p2ResursSC2".
      /*  */
      /* p2ResursSC2.Panel1 */
      /*  */
      THIS-OBJECT:p2ResursSC2:Panel1:Controls:Add(THIS-OBJECT:gridResursForP2).
      /*  */
      /* p2ResursSC2.Panel2 */
      /*  */
      THIS-OBJECT:p2ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP2resursBort).
      THIS-OBJECT:p2ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP2Resurs).
      THIS-OBJECT:p2ResursSC2:Size = NEW System.Drawing.Size(349, 284).
      THIS-OBJECT:p2ResursSC2:SplitterDistance = 309.
      THIS-OBJECT:p2ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP2 */
      /*  */
      THIS-OBJECT:gridResursForP2:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResursForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResursForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridResursForP2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForP2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForP2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForP2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForP2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForP2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForP2:GuruContext = ?.
      THIS-OBJECT:gridResursForP2:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursForP2:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursForP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForP2:Name = "gridResursForP2".
      THIS-OBJECT:gridResursForP2:Size = NEW System.Drawing.Size(309, 284).
      THIS-OBJECT:gridResursForP2:TabIndex = 1.
      THIS-OBJECT:gridResursForP2:Text = "grid1".
      THIS-OBJECT:gridResursForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2resursBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverP2resursBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverP2resursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2resursBort:Name = "buttonOverP2resursBort".
      THIS-OBJECT:buttonOverP2resursBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2resursBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2resursBort:Click:Subscribe(THIS-OBJECT:buttonOverP2ResursBort_Click).
      /*  */
      /* buttonOverP2Resurs */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverP2Resurs:Appearance = appearance14.
      THIS-OBJECT:buttonOverP2Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP2Resurs:Name = "buttonOverP2Resurs".
      THIS-OBJECT:buttonOverP2Resurs:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2Resurs:TabIndex = 2.
      THIS-OBJECT:buttonOverP2Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP2Resurs_Click).
      /*  */
      /* gridP2Resurs */
      /*  */
      THIS-OBJECT:gridP2Resurs:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2Resurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2Resurs:GuruContext = ?.
      THIS-OBJECT:gridP2Resurs:GuruDraggable = TRUE.
      THIS-OBJECT:gridP2Resurs:GuruDroppable = TRUE.
      THIS-OBJECT:gridP2Resurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2Resurs:Name = "gridP2Resurs".
      THIS-OBJECT:gridP2Resurs:Size = NEW System.Drawing.Size(788, 284).
      THIS-OBJECT:gridP2Resurs:TabIndex = 0.
      THIS-OBJECT:gridP2Resurs:Text = "grid1".
      THIS-OBJECT:gridP2Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP2Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp2resurs_AfterCellUpdate).
      /*  */
      /* TabP2frekvens */
      /*  */
      THIS-OBJECT:TabP2frekvens:Controls:Add(THIS-OBJECT:p2FrekvensSC).
      THIS-OBJECT:TabP2frekvens:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2frekvens:Name = "TabP2frekvens".
      THIS-OBJECT:TabP2frekvens:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p2FrekvensSC */
      /*  */
      THIS-OBJECT:p2FrekvensSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2FrekvensSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2FrekvensSC:Name = "p2FrekvensSC".
      /*  */
      /* p2FrekvensSC.Panel1 */
      /*  */
      THIS-OBJECT:p2FrekvensSC:Panel1:Controls:Add(THIS-OBJECT:p2FrekvensSC2).
      /*  */
      /* p2FrekvensSC.Panel2 */
      /*  */
      THIS-OBJECT:p2FrekvensSC:Panel2:Controls:Add(THIS-OBJECT:gridP2Frekvens).
      THIS-OBJECT:p2FrekvensSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p2FrekvensSC:SplitterDistance = 349.
      THIS-OBJECT:p2FrekvensSC:TabIndex = 2.
      /*  */
      /* p2FrekvensSC2 */
      /*  */
      THIS-OBJECT:p2FrekvensSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2FrekvensSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p2FrekvensSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2FrekvensSC2:Name = "p2FrekvensSC2".
      /*  */
      /* p2FrekvensSC2.Panel1 */
      /*  */
      THIS-OBJECT:p2FrekvensSC2:Panel1:Controls:Add(THIS-OBJECT:gridP3ForP2).
      /*  */
      /* p2FrekvensSC2.Panel2 */
      /*  */
      THIS-OBJECT:p2FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP2FrekvensBort).
      THIS-OBJECT:p2FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP2Frekvens).
      THIS-OBJECT:p2FrekvensSC2:Size = NEW System.Drawing.Size(349, 284).
      THIS-OBJECT:p2FrekvensSC2:SplitterDistance = 309.
      THIS-OBJECT:p2FrekvensSC2:TabIndex = 0.
      /*  */
      /* gridP3ForP2 */
      /*  */
      THIS-OBJECT:gridP3ForP2:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP3ForP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP3ForP2:GuruContext = ?.
      THIS-OBJECT:gridP3ForP2:GuruDraggable = TRUE.
      THIS-OBJECT:gridP3ForP2:GuruDroppable = TRUE.
      THIS-OBJECT:gridP3ForP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP3ForP2:Name = "gridP3ForP2".
      THIS-OBJECT:gridP3ForP2:Size = NEW System.Drawing.Size(309, 284).
      THIS-OBJECT:gridP3ForP2:TabIndex = 1.
      THIS-OBJECT:gridP3ForP2:Text = "grid1".
      THIS-OBJECT:gridP3ForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP3ForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2FrekvensBort */
      /*  */
      appearance39:Image = CAST(resources:GetObject("appearance39.Image"), System.Object).
      THIS-OBJECT:buttonOverP2FrekvensBort:Appearance = appearance39.
      THIS-OBJECT:buttonOverP2FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2FrekvensBort:Name = "buttonOverP2FrekvensBort".
      THIS-OBJECT:buttonOverP2FrekvensBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2FrekvensBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP2FrekvensBort_Click).
      /*  */
      /* buttonOverP2Frekvens */
      /*  */
      appearance40:Image = CAST(resources:GetObject("appearance40.Image"), System.Object).
      THIS-OBJECT:buttonOverP2Frekvens:Appearance = appearance40.
      THIS-OBJECT:buttonOverP2Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP2Frekvens:Name = "buttonOverP2Frekvens".
      THIS-OBJECT:buttonOverP2Frekvens:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2Frekvens:TabIndex = 2.
      THIS-OBJECT:buttonOverP2Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP2Frekvens_Click).
      /*  */
      /* gridP2Frekvens */
      /*  */
      THIS-OBJECT:gridP2Frekvens:AllowDrop = TRUE.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Appearance = appearance41.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance42:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance42:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance42:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:Appearance = appearance42.
      appearance43:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance43.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance44:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance44:BackColor2 = System.Drawing.SystemColors:Control.
      appearance44:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance44:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance44.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      appearance45:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance45.
      appearance46:BackColor = System.Drawing.SystemColors:Highlight.
      appearance46:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance46.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance47.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      appearance48:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellAppearance = appearance48.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance49:BackColor = System.Drawing.SystemColors:Control.
      appearance49:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance49:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance49:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance49:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance49.
      appearance50:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderAppearance = appearance50.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:RowAppearance = appearance51.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance52.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2Frekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2Frekvens:GuruContext = ?.
      THIS-OBJECT:gridP2Frekvens:GuruDraggable = TRUE.
      THIS-OBJECT:gridP2Frekvens:GuruDroppable = TRUE.
      THIS-OBJECT:gridP2Frekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2Frekvens:Name = "gridP2Frekvens".
      THIS-OBJECT:gridP2Frekvens:Size = NEW System.Drawing.Size(788, 284).
      THIS-OBJECT:gridP2Frekvens:TabIndex = 1.
      THIS-OBJECT:gridP2Frekvens:Text = "grid1".
      THIS-OBJECT:gridP2Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP2Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp2frekvens_AfterCellUpdate).
      /*  */
      /* TabP2materiel */
      /*  */
      THIS-OBJECT:TabP2materiel:Controls:Add(THIS-OBJECT:p2materielSC).
      THIS-OBJECT:TabP2materiel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2materiel:Name = "TabP2materiel".
      THIS-OBJECT:TabP2materiel:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p2materielSC */
      /*  */
      THIS-OBJECT:p2materielSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2materielSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2materielSC:Name = "p2materielSC".
      /*  */
      /* p2materielSC.Panel1 */
      /*  */
      THIS-OBJECT:p2materielSC:Panel1:Controls:Add(THIS-OBJECT:p2materielSC2).
      /*  */
      /* p2materielSC.Panel2 */
      /*  */
      THIS-OBJECT:p2materielSC:Panel2:Controls:Add(THIS-OBJECT:gridP2Mtrl).
      THIS-OBJECT:p2materielSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p2materielSC:SplitterDistance = 349.
      THIS-OBJECT:p2materielSC:TabIndex = 3.
      /*  */
      /* p2materielSC2 */
      /*  */
      THIS-OBJECT:p2materielSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2materielSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p2materielSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2materielSC2:Name = "p2materielSC2".
      /*  */
      /* p2materielSC2.Panel1 */
      /*  */
      THIS-OBJECT:p2materielSC2:Panel1:Controls:Add(THIS-OBJECT:gridMtrlForP2).
      /*  */
      /* p2materielSC2.Panel2 */
      /*  */
      THIS-OBJECT:p2materielSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP2MtrlBort).
      THIS-OBJECT:p2materielSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverp2Mtrl).
      THIS-OBJECT:p2materielSC2:Size = NEW System.Drawing.Size(349, 284).
      THIS-OBJECT:p2materielSC2:SplitterDistance = 308.
      THIS-OBJECT:p2materielSC2:TabIndex = 0.
      /*  */
      /* gridMtrlForP2 */
      /*  */
      THIS-OBJECT:gridMtrlForP2:AllowDrop = TRUE.
      appearance53:BackColor = System.Drawing.SystemColors:Window.
      appearance53:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Appearance = appearance53.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance54:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance54:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance54:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:Appearance = appearance54.
      appearance55:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance55.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance56:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance56:BackColor2 = System.Drawing.SystemColors:Control.
      appearance56:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance56.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      appearance57:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveCellAppearance = appearance57.
      appearance58:BackColor = System.Drawing.SystemColors:Highlight.
      appearance58:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveRowAppearance = appearance58.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance59:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CardAreaAppearance = appearance59.
      appearance60:BorderColor = System.Drawing.Color:Silver.
      appearance60:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellAppearance = appearance60.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellPadding = 0.
      appearance61:BackColor = System.Drawing.SystemColors:Control.
      appearance61:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance61:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance61:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance61:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:GroupByRowAppearance = appearance61.
      appearance62:TextHAlignAsString = "Left".
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderAppearance = appearance62.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:RowAppearance = appearance63.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance64.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridMtrlForP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridMtrlForP2:GuruContext = ?.
      THIS-OBJECT:gridMtrlForP2:GuruDraggable = TRUE.
      THIS-OBJECT:gridMtrlForP2:GuruDroppable = TRUE.
      THIS-OBJECT:gridMtrlForP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridMtrlForP2:Name = "gridMtrlForP2".
      THIS-OBJECT:gridMtrlForP2:Size = NEW System.Drawing.Size(308, 284).
      THIS-OBJECT:gridMtrlForP2:TabIndex = 1.
      THIS-OBJECT:gridMtrlForP2:Text = "grid1".
      THIS-OBJECT:gridMtrlForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridMtrlForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2MtrlBort */
      /*  */
      appearance65:Image = CAST(resources:GetObject("appearance65.Image"), System.Object).
      THIS-OBJECT:buttonOverP2MtrlBort:Appearance = appearance65.
      THIS-OBJECT:buttonOverP2MtrlBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2MtrlBort:Name = "buttonOverP2MtrlBort".
      THIS-OBJECT:buttonOverP2MtrlBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2MtrlBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2MtrlBort:Click:Subscribe(THIS-OBJECT:buttonOverP2MtrlBort_Click).
      /*  */
      /* buttonOverp2Mtrl */
      /*  */
      appearance66:Image = CAST(resources:GetObject("appearance66.Image"), System.Object).
      THIS-OBJECT:buttonOverp2Mtrl:Appearance = appearance66.
      THIS-OBJECT:buttonOverp2Mtrl:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverp2Mtrl:Name = "buttonOverp2Mtrl".
      THIS-OBJECT:buttonOverp2Mtrl:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverp2Mtrl:TabIndex = 2.
      THIS-OBJECT:buttonOverp2Mtrl:Click:Subscribe(THIS-OBJECT:buttonOverp2Mtrl_Click).
      /*  */
      /* gridP2Mtrl */
      /*  */
      THIS-OBJECT:gridP2Mtrl:AllowDrop = TRUE.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Appearance = appearance67.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance68:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance68:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance68:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:Appearance = appearance68.
      appearance69:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance69.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance70:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance70:BackColor2 = System.Drawing.SystemColors:Control.
      appearance70:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance70:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance70.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      appearance71:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveCellAppearance = appearance71.
      appearance72:BackColor = System.Drawing.SystemColors:Highlight.
      appearance72:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveRowAppearance = appearance72.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CardAreaAppearance = appearance73.
      appearance74:BorderColor = System.Drawing.Color:Silver.
      appearance74:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellAppearance = appearance74.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellPadding = 0.
      appearance75:BackColor = System.Drawing.SystemColors:Control.
      appearance75:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance75:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance75:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance75:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:GroupByRowAppearance = appearance75.
      appearance76:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderAppearance = appearance76.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance77:BackColor = System.Drawing.SystemColors:Window.
      appearance77:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:RowAppearance = appearance77.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance78:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance78.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2Mtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2Mtrl:GuruContext = ?.
      THIS-OBJECT:gridP2Mtrl:GuruDraggable = TRUE.
      THIS-OBJECT:gridP2Mtrl:GuruDroppable = TRUE.
      THIS-OBJECT:gridP2Mtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2Mtrl:Name = "gridP2Mtrl".
      THIS-OBJECT:gridP2Mtrl:Size = NEW System.Drawing.Size(788, 284).
      THIS-OBJECT:gridP2Mtrl:TabIndex = 1.
      THIS-OBJECT:gridP2Mtrl:Text = "grid1".
      THIS-OBJECT:gridP2Mtrl:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Mtrl_GuruAfterDropped).
      THIS-OBJECT:gridP2Mtrl:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp2mtrl_AfterCellUpdate).
      /*  */
      /* TabP2Lista */
      /*  */
      THIS-OBJECT:TabP2Lista:Controls:Add(THIS-OBJECT:splitContainerP2ListaGrund).
      THIS-OBJECT:TabP2Lista:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2Lista:Name = "TabP2Lista".
      THIS-OBJECT:TabP2Lista:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP2ListaGrund */
      /*  */
      THIS-OBJECT:splitContainerP2ListaGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP2ListaGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP2ListaGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP2ListaGrund:Name = "splitContainerP2ListaGrund".
      THIS-OBJECT:splitContainerP2ListaGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP2ListaGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP2ListaGrund:Panel1:Controls:Add(THIS-OBJECT:P2ListaStrip).
      /*  */
      /* splitContainerP2ListaGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP2ListaGrund:Panel2:Controls:Add(THIS-OBJECT:gridP2Lista).
      THIS-OBJECT:splitContainerP2ListaGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP2ListaGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP2ListaGrund:TabIndex = 7.
      /*  */
      /* P2ListaStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP2Lista.
      THIS-OBJECT:P2ListaStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:P2ListaStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P2ListaStrip:Name = "P2ListaStrip".
      THIS-OBJECT:P2ListaStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P2ListaStrip:TabIndex = 0.
      THIS-OBJECT:P2ListaStrip:Text = "P1ListaToolStrip".
      /*  */
      /* StripButtonP2Lista */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:p2ListaToolStripMenuItem.
      THIS-OBJECT:StripButtonP2Lista:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonP2Lista:Image = CAST(resources:GetObject("StripButtonP2Lista.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Lista:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Lista:Name = "StripButtonP2Lista".
      THIS-OBJECT:StripButtonP2Lista:Size = NEW System.Drawing.Size(78, 22).
      THIS-OBJECT:StripButtonP2Lista:Text = "Välj lista".
      THIS-OBJECT:StripButtonP2Lista:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p2ListaToolStripMenuItem */
      /*  */
      THIS-OBJECT:p2ListaToolStripMenuItem:Image = CAST(resources:GetObject("p2ListaToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p2ListaToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p2ListaToolStripMenuItem:Name = "p2ListaToolStripMenuItem".
      THIS-OBJECT:p2ListaToolStripMenuItem:Size = NEW System.Drawing.Size(227, 56).
      THIS-OBJECT:p2ListaToolStripMenuItem:Text = "Detaljerad information".
      THIS-OBJECT:p2ListaToolStripMenuItem:Click:Subscribe(THIS-OBJECT:P2ListaToolStripMenuItem_Click).
      /*  */
      /* gridP2Lista */
      /*  */
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Appearance = appearance79.
      THIS-OBJECT:gridP2Lista:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Lista:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance80:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance80:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance80:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Lista:DisplayLayout:GroupByBox:Appearance = appearance80.
      appearance81:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Lista:DisplayLayout:GroupByBox:BandLabelAppearance = appearance81.
      THIS-OBJECT:gridP2Lista:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance82:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance82:BackColor2 = System.Drawing.SystemColors:Control.
      appearance82:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance82:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Lista:DisplayLayout:GroupByBox:PromptAppearance = appearance82.
      THIS-OBJECT:gridP2Lista:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Lista:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      appearance83:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:ActiveCellAppearance = appearance83.
      appearance84:BackColor = System.Drawing.SystemColors:Highlight.
      appearance84:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:ActiveRowAppearance = appearance84.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:CardAreaAppearance = appearance85.
      appearance86:BorderColor = System.Drawing.Color:Silver.
      appearance86:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:CellAppearance = appearance86.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:CellPadding = 0.
      appearance87:BackColor = System.Drawing.SystemColors:Control.
      appearance87:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance87:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance87:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance87:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:GroupByRowAppearance = appearance87.
      appearance88:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:HeaderAppearance = appearance88.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:RowAppearance = appearance89.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Lista:DisplayLayout:Override:TemplateAddRowAppearance = appearance90.
      THIS-OBJECT:gridP2Lista:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2Lista:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2Lista:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2Lista:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2Lista:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2Lista:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2Lista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2Lista:GuruContext = ?.
      THIS-OBJECT:gridP2Lista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2Lista:Name = "gridP2Lista".
      THIS-OBJECT:gridP2Lista:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:gridP2Lista:TabIndex = 2.
      THIS-OBJECT:gridP2Lista:Text = "grid1".
      /*  */
      /* TabP2Import */
      /*  */
      THIS-OBJECT:TabP2Import:Controls:Add(THIS-OBJECT:splitContainerP2ImportGrund).
      THIS-OBJECT:TabP2Import:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2Import:Name = "TabP2Import".
      THIS-OBJECT:TabP2Import:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP2ImportGrund */
      /*  */
      THIS-OBJECT:splitContainerP2ImportGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP2ImportGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP2ImportGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP2ImportGrund:Name = "splitContainerP2ImportGrund".
      THIS-OBJECT:splitContainerP2ImportGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP2ImportGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP2ImportGrund:Panel1:Controls:Add(THIS-OBJECT:P2ImportStrip).
      /*  */
      /* splitContainerP2ImportGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP2ImportGrund:Panel2:Controls:Add(THIS-OBJECT:splitContainerP2Import).
      THIS-OBJECT:splitContainerP2ImportGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP2ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP2ImportGrund:TabIndex = 4.
      /*  */
      /* P2ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:StripButtonP2Import.
      arrayvar2[2] = THIS-OBJECT:StripButtonP2Rin.
      arrayvar2[3] = THIS-OBJECT:StripButtonP2Kompin.
      arrayvar2[4] = THIS-OBJECT:StripButtonP2Rbort.
      THIS-OBJECT:P2ImportStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:P2ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P2ImportStrip:Name = "P2ImportStrip".
      THIS-OBJECT:P2ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P2ImportStrip:TabIndex = 0.
      THIS-OBJECT:P2ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP2Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:p2KoderToolStripMenuItem.
      arrayvar3[2] = THIS-OBJECT:p2MaterielToolStripMenuItem.
      arrayvar3[3] = THIS-OBJECT:p2FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP2Import:DropDownItems:AddRange(arrayvar3).
      THIS-OBJECT:StripButtonP2Import:Image = CAST(resources:GetObject("StripButtonP2Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Import:Name = "StripButtonP2Import".
      THIS-OBJECT:StripButtonP2Import:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:StripButtonP2Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP2Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p2KoderToolStripMenuItem */
      /*  */
      THIS-OBJECT:p2KoderToolStripMenuItem:Image = CAST(resources:GetObject("p2KoderToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p2KoderToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p2KoderToolStripMenuItem:Name = "p2KoderToolStripMenuItem".
      THIS-OBJECT:p2KoderToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p2KoderToolStripMenuItem:Text = "P2 Kod".
      THIS-OBJECT:p2KoderToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p2KoderToolStripMenuItem_Click).
      /*  */
      /* p2MaterielToolStripMenuItem */
      /*  */
      THIS-OBJECT:p2MaterielToolStripMenuItem:Image = CAST(resources:GetObject("p2MaterielToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p2MaterielToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p2MaterielToolStripMenuItem:Name = "p2MaterielToolStripMenuItem".
      THIS-OBJECT:p2MaterielToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p2MaterielToolStripMenuItem:Text = "P2 Materiel".
      THIS-OBJECT:p2MaterielToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p2MaterielToolStripMenuItem_Click).
      /*  */
      /* p2FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p2FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p2FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Name = "p2FrekvensToolStripMenuItem".
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Text = "P2 Frekvens".
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p2FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP2Rin */
      /*  */
      THIS-OBJECT:StripButtonP2Rin:Image = CAST(resources:GetObject("StripButtonP2Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Rin:Name = "StripButtonP2Rin".
      THIS-OBJECT:StripButtonP2Rin:Size = NEW System.Drawing.Size(118, 22).
      THIS-OBJECT:StripButtonP2Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP2Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP2Rin:Click:Subscribe(THIS-OBJECT:StripButtonP2Rin_Click).
      /*  */
      /* StripButtonP2Kompin */
      /*  */
      THIS-OBJECT:StripButtonP2Kompin:Image = CAST(resources:GetObject("StripButtonP2Kompin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Kompin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Kompin:Name = "StripButtonP2Kompin".
      THIS-OBJECT:StripButtonP2Kompin:Size = NEW System.Drawing.Size(180, 22).
      THIS-OBJECT:StripButtonP2Kompin:Text = "Komplettera med Excel listan".
      THIS-OBJECT:StripButtonP2Kompin:Visible = FALSE.
      THIS-OBJECT:StripButtonP2Kompin:Click:Subscribe(THIS-OBJECT:StripButtonP2Kompin_Click).
      /*  */
      /* StripButtonP2Rbort */
      /*  */
      THIS-OBJECT:StripButtonP2Rbort:Image = CAST(resources:GetObject("StripButtonP2Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Rbort:Name = "StripButtonP2Rbort".
      THIS-OBJECT:StripButtonP2Rbort:Size = NEW System.Drawing.Size(161, 22).
      THIS-OBJECT:StripButtonP2Rbort:Text = "Ta bort markerade poster ".
      THIS-OBJECT:StripButtonP2Rbort:Visible = FALSE.
      THIS-OBJECT:StripButtonP2Rbort:Click:Subscribe(THIS-OBJECT:StripButtonP2Rbort_Click).
      /*  */
      /* splitContainerP2Import */
      /*  */
      THIS-OBJECT:splitContainerP2Import:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP2Import:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP2Import:Name = "splitContainerP2Import".
      /*  */
      /* splitContainerP2Import.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP2Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP2M).
      THIS-OBJECT:splitContainerP2Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP2F).
      THIS-OBJECT:splitContainerP2Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP2).
      THIS-OBJECT:splitContainerP2Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP2Import:SplitterDistance = 379.
      THIS-OBJECT:splitContainerP2Import:TabIndex = 0.
      /*  */
      /* GridImportP2M */
      /*  */
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Appearance = appearance91.
      THIS-OBJECT:GridImportP2M:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP2M:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance92:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance92:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance92:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2M:DisplayLayout:GroupByBox:Appearance = appearance92.
      appearance93:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2M:DisplayLayout:GroupByBox:BandLabelAppearance = appearance93.
      THIS-OBJECT:GridImportP2M:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance94:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance94:BackColor2 = System.Drawing.SystemColors:Control.
      appearance94:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance94:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2M:DisplayLayout:GroupByBox:PromptAppearance = appearance94.
      THIS-OBJECT:GridImportP2M:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP2M:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      appearance95:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:ActiveCellAppearance = appearance95.
      appearance96:BackColor = System.Drawing.SystemColors:Highlight.
      appearance96:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:ActiveRowAppearance = appearance96.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:CardAreaAppearance = appearance97.
      appearance98:BorderColor = System.Drawing.Color:Silver.
      appearance98:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:CellAppearance = appearance98.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:CellPadding = 0.
      appearance99:BackColor = System.Drawing.SystemColors:Control.
      appearance99:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance99:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance99:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance99:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:GroupByRowAppearance = appearance99.
      appearance100:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:HeaderAppearance = appearance100.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance101:BackColor = System.Drawing.SystemColors:Window.
      appearance101:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:RowAppearance = appearance101.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance102:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP2M:DisplayLayout:Override:TemplateAddRowAppearance = appearance102.
      THIS-OBJECT:GridImportP2M:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP2M:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP2M:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP2M:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP2M:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP2M:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP2M:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP2M:GuruContext = ?.
      THIS-OBJECT:GridImportP2M:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP2M:Name = "GridImportP2M".
      THIS-OBJECT:GridImportP2M:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP2M:TabIndex = 2.
      THIS-OBJECT:GridImportP2M:Text = "grid1".
      /*  */
      /* GridImportP2F */
      /*  */
      appearance103:BackColor = System.Drawing.SystemColors:Window.
      appearance103:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Appearance = appearance103.
      THIS-OBJECT:GridImportP2F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP2F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance104:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance104:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance104:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance104:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:Appearance = appearance104.
      appearance105:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance105.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance106:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance106:BackColor2 = System.Drawing.SystemColors:Control.
      appearance106:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance106:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:PromptAppearance = appearance106.
      THIS-OBJECT:GridImportP2F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP2F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance107:BackColor = System.Drawing.SystemColors:Window.
      appearance107:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveCellAppearance = appearance107.
      appearance108:BackColor = System.Drawing.SystemColors:Highlight.
      appearance108:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveRowAppearance = appearance108.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance109:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CardAreaAppearance = appearance109.
      appearance110:BorderColor = System.Drawing.Color:Silver.
      appearance110:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellAppearance = appearance110.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellPadding = 0.
      appearance111:BackColor = System.Drawing.SystemColors:Control.
      appearance111:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance111:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance111:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance111:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:GroupByRowAppearance = appearance111.
      appearance112:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderAppearance = appearance112.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance113:BackColor = System.Drawing.SystemColors:Window.
      appearance113:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:RowAppearance = appearance113.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance114:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:TemplateAddRowAppearance = appearance114.
      THIS-OBJECT:GridImportP2F:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP2F:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP2F:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP2F:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP2F:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP2F:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP2F:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP2F:GuruContext = ?.
      THIS-OBJECT:GridImportP2F:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP2F:Name = "GridImportP2F".
      THIS-OBJECT:GridImportP2F:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP2F:TabIndex = 1.
      THIS-OBJECT:GridImportP2F:Text = "grid1".
      /*  */
      /* GridImportP2 */
      /*  */
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP2:DisplayLayout:Appearance = appearance115.
      THIS-OBJECT:GridImportP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance116:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance116:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance116:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance116:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:Appearance = appearance116.
      appearance117:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance117.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance118:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance118:BackColor2 = System.Drawing.SystemColors:Control.
      appearance118:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance118:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:PromptAppearance = appearance118.
      THIS-OBJECT:GridImportP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance119:BackColor = System.Drawing.SystemColors:Window.
      appearance119:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveCellAppearance = appearance119.
      appearance120:BackColor = System.Drawing.SystemColors:Highlight.
      appearance120:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveRowAppearance = appearance120.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CardAreaAppearance = appearance121.
      appearance122:BorderColor = System.Drawing.Color:Silver.
      appearance122:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellAppearance = appearance122.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellPadding = 0.
      appearance123:BackColor = System.Drawing.SystemColors:Control.
      appearance123:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance123:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance123:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance123:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:GroupByRowAppearance = appearance123.
      appearance124:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderAppearance = appearance124.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance125:BackColor = System.Drawing.SystemColors:Window.
      appearance125:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:RowAppearance = appearance125.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance126.
      THIS-OBJECT:GridImportP2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP2:GuruContext = ?.
      THIS-OBJECT:GridImportP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP2:Name = "GridImportP2".
      THIS-OBJECT:GridImportP2:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP2:TabIndex = 0.
      THIS-OBJECT:GridImportP2:Text = "grid1".
      /*  */
      /* p2MainSC */
      /*  */
      THIS-OBJECT:p2MainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2MainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p2MainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2MainSC:Name = "p2MainSC".
      THIS-OBJECT:p2MainSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p2MainSC.Panel1 */
      /*  */
      THIS-OBJECT:p2MainSC:Panel1:Controls:Add(THIS-OBJECT:p2kodSC).
      /*  */
      /* p2MainSC.Panel2 */
      /*  */
      THIS-OBJECT:p2MainSC:Panel2:Controls:Add(THIS-OBJECT:TabManagerP2).
      THIS-OBJECT:p2MainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:p2MainSC:SplitterDistance = 230.
      THIS-OBJECT:p2MainSC:TabIndex = 1.
      /*  */
      /* p2kodSC */
      /*  */
      THIS-OBJECT:p2kodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2kodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p2kodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2kodSC:Name = "p2kodSC".
      /*  */
      /* p2kodSC.Panel1 */
      /*  */
      THIS-OBJECT:p2kodSC:Panel1:Controls:Add(THIS-OBJECT:p2arbkodSC).
      /*  */
      /* p2kodSC.Panel2 */
      /*  */
      THIS-OBJECT:p2kodSC:Panel2:Controls:Add(THIS-OBJECT:p2KoderMenuSC).
      THIS-OBJECT:p2kodSC:Size = NEW System.Drawing.Size(1145, 230).
      THIS-OBJECT:p2kodSC:SplitterDistance = 400.
      THIS-OBJECT:p2kodSC:TabIndex = 1.
      /*  */
      /* p2arbkodSC */
      /*  */
      THIS-OBJECT:p2arbkodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2arbkodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p2arbkodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2arbkodSC:Name = "p2arbkodSC".
      THIS-OBJECT:p2arbkodSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p2arbkodSC.Panel1 */
      /*  */
      THIS-OBJECT:p2arbkodSC:Panel1:Controls:Add(THIS-OBJECT:p2arbkodStrip).
      /*  */
      /* p2arbkodSC.Panel2 */
      /*  */
      THIS-OBJECT:p2arbkodSC:Panel2:Controls:Add(THIS-OBJECT:gridP2arbkod).
      THIS-OBJECT:p2arbkodSC:Size = NEW System.Drawing.Size(400, 230).
      THIS-OBJECT:p2arbkodSC:SplitterDistance = 25.
      THIS-OBJECT:p2arbkodSC:TabIndex = 0.
      /*  */
      /* p2arbkodStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:laggTillP2arbkod.
      arrayvar4[2] = THIS-OBJECT:taBortP2arbkodButton.
      THIS-OBJECT:p2arbkodStrip:Items:AddRange(arrayvar4).
      THIS-OBJECT:p2arbkodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2arbkodStrip:Name = "p2arbkodStrip".
      THIS-OBJECT:p2arbkodStrip:Size = NEW System.Drawing.Size(400, 25).
      THIS-OBJECT:p2arbkodStrip:TabIndex = 0.
      THIS-OBJECT:p2arbkodStrip:Text = "toolStrip1".
      /*  */
      /* laggTillP2arbkod */
      /*  */
      THIS-OBJECT:laggTillP2arbkod:Image = CAST(resources:GetObject("laggTillP2arbkod.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP2arbkod:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP2arbkod:Name = "laggTillP2arbkod".
      THIS-OBJECT:laggTillP2arbkod:Size = NEW System.Drawing.Size(144, 22).
      THIS-OBJECT:laggTillP2arbkod:Text = "Lägg till P2-Arbetskod".
      THIS-OBJECT:laggTillP2arbkod:Click:Subscribe(THIS-OBJECT:laggTillP2arbkod_Click).
      /*  */
      /* taBortP2arbkodButton */
      /*  */
      THIS-OBJECT:taBortP2arbkodButton:Image = CAST(resources:GetObject("taBortP2arbkodButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP2arbkodButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP2arbkodButton:Name = "taBortP2arbkodButton".
      THIS-OBJECT:taBortP2arbkodButton:Size = NEW System.Drawing.Size(138, 22).
      THIS-OBJECT:taBortP2arbkodButton:Text = "Ta bort P2-Arbetskod".
      THIS-OBJECT:taBortP2arbkodButton:ToolTipText = "Tar bort markerad P2-Arbetskod".
      THIS-OBJECT:taBortP2arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP2arbkodButton_Click).
      /*  */
      /* gridP2arbkod */
      /*  */
      appearance127:BackColor = System.Drawing.SystemColors:Window.
      appearance127:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Appearance = appearance127.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance128:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance128:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance128:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance128:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:Appearance = appearance128.
      appearance129:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance129.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance130:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance130:BackColor2 = System.Drawing.SystemColors:Control.
      appearance130:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance130:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance130.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance131:BackColor = System.Drawing.SystemColors:Window.
      appearance131:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance131.
      appearance132:BackColor = System.Drawing.SystemColors:Highlight.
      appearance132:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance132.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance133:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CardAreaAppearance = appearance133.
      appearance134:BorderColor = System.Drawing.Color:Silver.
      appearance134:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellAppearance = appearance134.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance135:BackColor = System.Drawing.SystemColors:Control.
      appearance135:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance135:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance135:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance135:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance135.
      appearance136:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderAppearance = appearance136.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance137:BackColor = System.Drawing.SystemColors:Window.
      appearance137:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:RowAppearance = appearance137.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance138:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance138.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2arbkod:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2arbkod:GuruContext = ?.
      THIS-OBJECT:gridP2arbkod:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2arbkod:Name = "gridP2arbkod".
      THIS-OBJECT:gridP2arbkod:Size = NEW System.Drawing.Size(400, 201).
      THIS-OBJECT:gridP2arbkod:TabIndex = 0.
      THIS-OBJECT:gridP2arbkod:Text = "grid1".
      THIS-OBJECT:gridP2arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp2arbkod_AfterCellUpdate).
      THIS-OBJECT:gridP2arbkod:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp2_BeforeCellUpdate).
      /*  */
      /* p2KoderMenuSC */
      /*  */
      THIS-OBJECT:p2KoderMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p2KoderMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p2KoderMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2KoderMenuSC:Name = "p2KoderMenuSC".
      THIS-OBJECT:p2KoderMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p2KoderMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:p2KoderMenuSC:Panel1:Controls:Add(THIS-OBJECT:p2KodertoolStrip).
      /*  */
      /* p2KoderMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:p2KoderMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridP2).
      THIS-OBJECT:p2KoderMenuSC:Size = NEW System.Drawing.Size(741, 230).
      THIS-OBJECT:p2KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p2KoderMenuSC:TabIndex = 1.
      /*  */
      /* p2KodertoolStrip */
      /*  */
      THIS-OBJECT:p2KodertoolStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar5[1] = THIS-OBJECT:laggTillP2Button.
      arrayvar5[2] = THIS-OBJECT:copyP2Button.
      arrayvar5[3] = THIS-OBJECT:flyttaP2Button.
      arrayvar5[4] = THIS-OBJECT:taBortP2Button.
      THIS-OBJECT:p2KodertoolStrip:Items:AddRange(arrayvar5).
      THIS-OBJECT:p2KodertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2KodertoolStrip:Name = "p2KodertoolStrip".
      THIS-OBJECT:p2KodertoolStrip:Size = NEW System.Drawing.Size(741, 25).
      THIS-OBJECT:p2KodertoolStrip:TabIndex = 0.
      THIS-OBJECT:p2KodertoolStrip:Text = "toolStrip2".
      /*  */
      /* laggTillP2Button */
      /*  */
      THIS-OBJECT:laggTillP2Button:Image = CAST(resources:GetObject("laggTillP2Button.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP2Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP2Button:Name = "laggTillP2Button".
      THIS-OBJECT:laggTillP2Button:Size = NEW System.Drawing.Size(127, 22).
      THIS-OBJECT:laggTillP2Button:Text = "Lägg till ny P2-Kod".
      THIS-OBJECT:laggTillP2Button:Click:Subscribe(THIS-OBJECT:laggTillP2Button_Click).
      /*  */
      /* copyP2Button */
      /*  */
      THIS-OBJECT:copyP2Button:Image = CAST(resources:GetObject("copyP2Button.Image"), System.Drawing.Image).
      THIS-OBJECT:copyP2Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:copyP2Button:Name = "copyP2Button".
      THIS-OBJECT:copyP2Button:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:copyP2Button:Text = "Kopiera P2-Kod".
      THIS-OBJECT:copyP2Button:ToolTipText = "Markera den P2 kod som ska kopieras och fyll i till vilken arbetskod löpnr den ska kopieras.".
      THIS-OBJECT:copyP2Button:Click:Subscribe(THIS-OBJECT:copyP2Button_Click).
      /*  */
      /* flyttaP2Button */
      /*  */
      THIS-OBJECT:flyttaP2Button:Image = CAST(resources:GetObject("flyttaP2Button.Image"), System.Drawing.Image).
      THIS-OBJECT:flyttaP2Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:flyttaP2Button:Name = "flyttaP2Button".
      THIS-OBJECT:flyttaP2Button:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:flyttaP2Button:Text = "Flytta P2-Kod".
      THIS-OBJECT:flyttaP2Button:ToolTipText = "Markera den P2 kod som ska flyttas och fyll i till vilken arbetskod löpnr den ska flyttas till.".
      THIS-OBJECT:flyttaP2Button:Click:Subscribe(THIS-OBJECT:flyttaP2Button_Click).
      /*  */
      /* taBortP2Button */
      /*  */
      THIS-OBJECT:taBortP2Button:Image = CAST(resources:GetObject("taBortP2Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP2Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP2Button:Name = "taBortP2Button".
      THIS-OBJECT:taBortP2Button:Size = NEW System.Drawing.Size(105, 22).
      THIS-OBJECT:taBortP2Button:Text = "Ta bort P2-Kod".
      THIS-OBJECT:taBortP2Button:ToolTipText = "Tar bort markerad P2-Kod".
      THIS-OBJECT:taBortP2Button:Click:Subscribe(THIS-OBJECT:taBortP2Button_Click).
      /*  */
      /* gridP2 */
      /*  */
      appearance139:BackColor = System.Drawing.SystemColors:Window.
      appearance139:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2:DisplayLayout:Appearance = appearance139.
      THIS-OBJECT:gridP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance140:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance140:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance140:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance140:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:Appearance = appearance140.
      appearance141:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance141.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance142:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance142:BackColor2 = System.Drawing.SystemColors:Control.
      appearance142:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance142:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:PromptAppearance = appearance142.
      THIS-OBJECT:gridP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance143:BackColor = System.Drawing.SystemColors:Window.
      appearance143:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveCellAppearance = appearance143.
      appearance144:BackColor = System.Drawing.SystemColors:Highlight.
      appearance144:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveRowAppearance = appearance144.
      THIS-OBJECT:gridP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance145:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CardAreaAppearance = appearance145.
      appearance146:BorderColor = System.Drawing.Color:Silver.
      appearance146:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellAppearance = appearance146.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellPadding = 0.
      appearance147:BackColor = System.Drawing.SystemColors:Control.
      appearance147:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance147:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance147:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance147:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:Override:GroupByRowAppearance = appearance147.
      appearance148:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderAppearance = appearance148.
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance149:BackColor = System.Drawing.SystemColors:Window.
      appearance149:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2:DisplayLayout:Override:RowAppearance = appearance149.
      THIS-OBJECT:gridP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance150:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance150.
      THIS-OBJECT:gridP2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2:GuruContext = ?.
      THIS-OBJECT:gridP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2:Name = "gridP2".
      THIS-OBJECT:gridP2:Size = NEW System.Drawing.Size(741, 201).
      THIS-OBJECT:gridP2:TabIndex = 0.
      THIS-OBJECT:gridP2:Text = "grid1".
      THIS-OBJECT:gridP2:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp2_AfterCellUpdate).
      THIS-OBJECT:gridP2:CellChange:Subscribe(THIS-OBJECT:gridP2_CellChange).
      THIS-OBJECT:gridP2:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp2_BeforeCellUpdate).
      /*  */
      /* TabManagerP2 */
      /*  */
      THIS-OBJECT:TabManagerP2:AllowDrop = TRUE.
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP2).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2resurs).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2frekvens).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2materiel).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2Import).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2Lista).
      THIS-OBJECT:TabManagerP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP2:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP2:Name = "TabManagerP2".
      THIS-OBJECT:TabManagerP2:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP2.
      THIS-OBJECT:TabManagerP2:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP2:TabIndex = 1.
      appearance151:Image = CAST(resources:GetObject("appearance151.Image"), System.Object).
      ultraTab7:Appearance = appearance151.
      ultraTab7:Key = "P2Resurs".
      ultraTab7:TabPage = THIS-OBJECT:TabP2resurs.
      ultraTab7:Text = "P2-Resurser".
      appearance152:Image = CAST(resources:GetObject("appearance152.Image"), System.Object).
      ultraTab8:Appearance = appearance152.
      ultraTab8:Key = "P2Frekvens".
      ultraTab8:TabPage = THIS-OBJECT:TabP2frekvens.
      ultraTab8:Text = "P2-Frekvenser".
      appearance153:Image = CAST(resources:GetObject("appearance153.Image"), System.Object).
      ultraTab9:Appearance = appearance153.
      ultraTab9:Key = "P2Materiel".
      ultraTab9:TabPage = THIS-OBJECT:TabP2materiel.
      ultraTab9:Text = "P2-Materiel".
      appearance154:Image = CAST(resources:GetObject("appearance154.Image"), System.Object).
      ultraTab2:Appearance = appearance154.
      ultraTab2:Key = "P2listor".
      ultraTab2:TabPage = THIS-OBJECT:TabP2Lista.
      ultraTab2:Text = "P2-Listor".
      appearance155:Image = CAST(resources:GetObject("appearance155.Image"), System.Object).
      ultraTab28:ActiveAppearance = appearance155.
      appearance156:Image = CAST(resources:GetObject("appearance156.Image"), System.Object).
      ultraTab28:Appearance = appearance156.
      ultraTab28:Key = "Import".
      ultraTab28:TabPage = THIS-OBJECT:TabP2Import.
      ultraTab28:Text = "Import Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 5 NO-UNDO.
      arrayvar6[1] = ultraTab7.
      arrayvar6[2] = ultraTab8.
      arrayvar6[3] = ultraTab9.
      arrayvar6[4] = ultraTab2.
      arrayvar6[5] = ultraTab28.
      THIS-OBJECT:TabManagerP2:Tabs:AddRange(arrayvar6).
      THIS-OBJECT:TabManagerP2:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP2_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP2 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP2:Name = "ultraTabSharedControlsPageP2".
      THIS-OBJECT:ultraTabSharedControlsPageP2:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* EkgP2Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:AutoSize = TRUE.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p2MainSC).
      THIS-OBJECT:Margin = NEW System.Windows.Forms.Padding(3, 2, 3, 2).
      THIS-OBJECT:Name = "EkgP2Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabP2resurs:ResumeLayout(FALSE).
      THIS-OBJECT:p2ResursSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2ResursSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2ResursSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2ResursSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2ResursSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2ResursSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2ResursSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2ResursSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursForP2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP2Resurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP2frekvens:ResumeLayout(FALSE).
      THIS-OBJECT:p2FrekvensSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2FrekvensSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2FrekvensSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2FrekvensSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2FrekvensSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2FrekvensSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2FrekvensSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2FrekvensSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridP3ForP2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP2Frekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP2materiel:ResumeLayout(FALSE).
      THIS-OBJECT:p2materielSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2materielSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2materielSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2materielSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2materielSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2materielSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2materielSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2materielSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridMtrlForP2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP2Mtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP2Lista:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP2ListaGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP2ListaGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP2ListaGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP2ListaGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP2ListaGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P2ListaStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P2ListaStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP2Lista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP2Import:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP2ImportGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP2ImportGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP2ImportGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP2ImportGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP2ImportGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P2ImportStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P2ImportStrip:PerformLayout().
      THIS-OBJECT:splitContainerP2Import:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP2Import, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP2Import:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridImportP2M, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP2F, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2MainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2MainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2MainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2MainSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2kodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2kodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2kodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2kodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2arbkodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2arbkodSC:Panel1:PerformLayout().
      THIS-OBJECT:p2arbkodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2arbkodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2arbkodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2arbkodStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p2arbkodStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP2arbkod, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2KoderMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p2KoderMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:p2KoderMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p2KoderMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2KoderMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:p2KodertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p2KodertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerP2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerP2:ResumeLayout(FALSE).
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
   METHOD PRIVATE VOID buttonOverp2Mtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2Mtrl().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP2MtrlBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2MtrlBort().
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP2Frekvens_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2Frekvens().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP2FrekvensBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2FrekvensBort().
      RETURN.

   END METHOD.
   

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP2Resurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2Resurs().
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP2ResursBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP2ResursBort().
      RETURN.

   END METHOD.
   
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2arbkod_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P2ArbkodUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2arbkod_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().  
      IF THIS-OBJECT:gridP2:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      IF THIS-OBJECT:ControlWinLg:P2Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO:             
         THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P2").            
      END. 
      ELSE THIS-OBJECT:gridP2:GuruReopen().
               
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP2_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridP2:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "FREKVENS" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE". 
         THIS-OBJECT:gridP2:GuruCurrentRowid(THIS-OBJECT:ControlDataDb:ekgp2tth:ROWID). /*Fix*/
         IF THIS-OBJECT:ControlWinLg:P2Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO: 
            THIS-OBJECT:gridP2:GuruGetCurrentRowid(). /*Fix*/            
            THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P2").
         END. 
         ELSE THIS-OBJECT:gridP2:GuruReopen(). 
      END. 
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ControlWinLg:P2ResursUppdate(INPUT kolnamn, INPUT kolvarde).                    
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp2mtrl_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P2mtrlUppdate(INPUT kolnamn, INPUT kolvarde).    
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.   
   METHOD PRIVATE VOID gridp2frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ControlWinLg:P2FrekvensUppdate(INPUT kolnamn, INPUT kolvarde).                
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP2Mtrl_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridMtrlForP2 THEN DO:
         THIS-OBJECT:buttonOverP2Mtrl_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridMtrlForP2_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP2Mtrl THEN DO:
         THIS-OBJECT:buttonOverP2MtrlBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP2Frekvens_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP3ForP2 THEN DO:
         THIS-OBJECT:buttonOverP2Frekvens_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP3ForP2_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP2Frekvens THEN DO:
         THIS-OBJECT:buttonOverP2FrekvensBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP2Resurs_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursForP2 THEN DO:
         THIS-OBJECT:buttonOverP2Resurs_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridResursForP2_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP2Resurs THEN DO:
         THIS-OBJECT:buttonOverP2ResursBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID P2ListaToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:gridP2Lista:VISIBLE = TRUE.   
      THIS-OBJECT:ControlDataDb:ekgp1detaljTTh:FIND-FIRST() NO-ERROR.
      IF NOT THIS-OBJECT:ControlDataDb:ekgp1detaljTTh:AVAILABLE THEN DO:    
         THIS-OBJECT:ControlWinLg:HamtP1detaljlista("2").
      END.     
          
      RETURN.

   END METHOD.
   
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP2Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP2Pop:Show().
      THIS-OBJECT:ControlShell:fillinP2LopNr:GuruText:Select().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP2arbkodPop:Show().
      THIS-OBJECT:ControlShell:fillinP2ArbKod:GuruText:Select().
      RETURN.

   END METHOD.


   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p2FrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP2:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2M:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2F:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P2F").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p2KoderToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP2F:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2M:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P2").
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p2MaterielToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP2F:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP2M:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P2M").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP2Kompin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P2K").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP2Rbort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("P2").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP2Rin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P2").
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP2_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(THIS-OBJECT:TabManagerP2:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP2:SelectedTab:KEY).    
      END. 
      RETURN.
   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP2arbkodButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP2ArbKod().
      RETURN.

   END METHOD.
   
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP2Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP2().
      RETURN.

   END METHOD.
   
END CLASS.
