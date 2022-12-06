 
 /*------------------------------------------------------------------------
    File        : EkgP1Control
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Wed Oct 09 09:57:47 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgP1Control INHERITS UserControl: 
 
   
   DEFINE PRIVATE VARIABLE copyP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1Lista AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1Artikelfrekvens AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE P1ArtikelFrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p1kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1frekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p1arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE P1ListaToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P1ListaStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p1MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p1KoderToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p1KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p1FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTIllP1arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2forP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP1frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP1FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE P1ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP1F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP1ListaGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   
   DEFINE PRIVATE VARIABLE StripButtonP1Lista AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1Lista AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP1ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP1Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Kompin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP1Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   
   CONSTRUCTOR PUBLIC EkgP1Control (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      
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
	METHOD PRIVATE VOID copyP1Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:copyP1Pop:Show().
      THIS-OBJECT:ControlShell:fillinP1coplopnr:GuruText:Select().
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      /*P1*/
      THIS-OBJECT:CreateGridP2ForP1().
      THIS-OBJECT:CreateGridP1().
      THIS-OBJECT:CreateGridP1Frekvens().
      THIS-OBJECT:CreateGridP1arbkod().
      THIS-OBJECT:CreateGridImportP1().
      THIS-OBJECT:CreateGridP1Lista().
      THIS-OBJECT:CreategridP1ArtikelFrekvens().

   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP2ForP1():
      {GridRubrikListaVarExtent.i} 

      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:GridP2ForP1:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ControlDataDb:ekgp2TTh)).
      THIS-OBJECT:GridP2ForP1:Text = "P2-Koder".
      THIS-OBJECT:GridP2ForP1:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP2ForP1:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2ForP1:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP1():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50,"EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70, "LOG").
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
      rubrikergrid[19] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:GridP1:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 22, INPUT THIS-OBJECT:ControlDataDb:ekgp1TTh)).
      THIS-OBJECT:GridP1:Text = "P1-Koder".
      THIS-OBJECT:GridP1:GuruUpdateTitle().
      THIS-OBJECT:GridP1:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP1).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP1:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP1:DisplayLayout:Bands[0]:Columns["FREKVENS"]:Hidden = TRUE.
      THIS-OBJECT:GridP1:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridP1Frekvens(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50,"EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P1BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[7] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[8] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FBERTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FUTRUSTTIM","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FMATERIELKOST","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FUTRUSTKOST","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[21] = NEW Controls.GridRubrik("FEAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[23] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").
      
      THIS-OBJECT:GridP1Frekvens:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 23, INPUT THIS-OBJECT:ControlDataDb:ekgp1frekvensTTh)).
      THIS-OBJECT:GridP1Frekvens:Text = "P1-Frekvenser".
      THIS-OBJECT:GridP1Frekvens:GuruUpdateTitle().
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP1Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE.     
   END METHOD.
   METHOD PUBLIC VOID CreateGridP1arbkod():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,150).
      
      THIS-OBJECT:GridP1arbkod:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgp1arbkodTTh)).
      THIS-OBJECT:GridP1arbkod:Text = "P1-Arbetskoder".
      THIS-OBJECT:GridP1arbkod:GuruUpdateTitle().
      THIS-OBJECT:GridP1arbkod:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP1ArbKod).
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP1arbkod:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP1arbkod:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP1(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).
      THIS-OBJECT:GridImportP1:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ControlDataDb:importP1TTh)).
      THIS-OBJECT:GridImportP1:Text = "P1 Kod Import".
      THIS-OBJECT:GridImportP1:GuruUpdateTitle().
      
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP1).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("P1BENAMNING","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[6] = NEW Controls.GridRubrik("P2BENAMNING","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").    
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).  
      THIS-OBJECT:GridImportP1F:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 8, INPUT THIS-OBJECT:ControlDataDb:importP1P2TTh)).
      THIS-OBJECT:GridImportP1F:Text = "P1 Frekvens Import".
      THIS-OBJECT:GridImportP1F:GuruUpdateTitle().
      THIS-OBJECT:GridImportP1F:VISIBLE = FALSE.
      
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP1F).
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP1Lista():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50, "EKGLOP").      
      rubrikergrid[3] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("P2LOPNR","",TRUE,50, "EKGLOP").            
      rubrikergrid[5] = NEW Controls.GridRubrik("P3ARBKOD","",TRUE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("P3LOPNR","",TRUE,50, "EKGLOP").           
      rubrikergrid[7] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).                 
      rubrikergrid[8] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).                          
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[10] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,150).
      rubrikergrid[11] = NEW Controls.GridRubrik("RANTAL","Resursantal",TRUE,75,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("TOTANTAL","",TRUE,90,"DEC").            
      rubrikergrid[13] = NEW Controls.GridRubrik("BERTIM","",TRUE,50,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("MONTTIM","",TRUE,50,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("MASKTIM","",TRUE,50,"DEC").      
      rubrikergrid[16] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,50,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      /*rubrikergrid[17] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").*/
      rubrikergrid[18] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[19] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[20] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      /*rubrikergrid[20] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").    */
      rubrikergrid[21] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[22] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[23] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[24] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
           
      THIS-OBJECT:GridP1Lista:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 24, INPUT THIS-OBJECT:ControlDataDb:ekgp1detaljTTh)).
      THIS-OBJECT:GridP1Lista:Text = "P1-Kod Detaljerad Lista".
      THIS-OBJECT:GridP1Lista:GuruUpdateTitle().
      /*
      THIS-OBJECT:GridP1Lista:SetViewStyleBand(TRUE).
      */
      /*THIS-OBJECT:GridP1Lista:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP1).*/
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridP1Lista:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP1Lista:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE.
     
      
   END METHOD.
   
   METHOD PUBLIC VOID CreategridP1ArtikelFrekvens():
      {GridRubrikListaVarExtent.i}    
      rubrikergrid[1] = NEW Controls.GridRubrik("P1ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("P1LOPNR","",TRUE,50, "EKGLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("P1BENAMNING","",TRUE,200).            
      rubrikergrid[4] = NEW Controls.GridRubrik("ARTNR","",TRUE,90).
      rubrikergrid[5] = NEW Controls.GridRubrik("MTRLBENAMNING","",TRUE,300).
      rubrikergrid[6] = NEW Controls.GridRubrik("ANTAL","",TRUE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("KALKYLPRIS","",TRUE,80,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("MTRLSUMMA","",TRUE,80,"DEC").
                  
          
      THIS-OBJECT:gridP1ArtikelFrekvens:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 8, INPUT THIS-OBJECT:ControlDataDb:ekgp1mtrlTTh)).
      THIS-OBJECT:gridP1ArtikelFrekvens:Text = "P1-Artikelfrekvens".
      THIS-OBJECT:gridP1ArtikelFrekvens:GuruUpdateTitle().
      /*THIS-OBJECT:GridP1Lista:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP1).*/
      
      
      
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
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP1Control").
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
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
      appearance99 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
      appearance100 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab29 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab29 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabP1frekvens = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p1FrekvensSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1frekvensSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridP2forP1 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP1FrekvensBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP1frekvens = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP1Frekvens = NEW Controls.Grid().
      THIS-OBJECT:TabP1Lista = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP1ListaGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P1ListaStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP1Lista = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:P1ListaToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:gridP1Artikelfrekvens = NEW Controls.Grid().
      THIS-OBJECT:gridP1Lista = NEW Controls.Grid().
      THIS-OBJECT:TabP1Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP1ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P1ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP1Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p1KoderToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p1FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP1Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP1Kompin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP1Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP1Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP1F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP1 = NEW Controls.Grid().
      THIS-OBJECT:p1MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1kodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1arbkodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1arbkodStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTIllP1arbkod = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP1arbkodButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP1arbkod = NEW Controls.Grid().
      THIS-OBJECT:p1KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1KodertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP1Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP1Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:copyP1Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP1 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP1frekvens:SuspendLayout().
      CAST(THIS-OBJECT:p1FrekvensSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1FrekvensSC:Panel1:SuspendLayout().
      THIS-OBJECT:p1FrekvensSC:Panel2:SuspendLayout().
      THIS-OBJECT:p1FrekvensSC:SuspendLayout().
      CAST(THIS-OBJECT:p1frekvensSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1frekvensSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p1frekvensSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p1frekvensSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridP2forP1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP1Frekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP1Lista:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP1ListaGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP1ListaGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP1ListaGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP1ListaGrund:SuspendLayout().
      THIS-OBJECT:P1ListaStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP1Artikelfrekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP1Lista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP1Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP1ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP1ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP1ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP1ImportGrund:SuspendLayout().
      THIS-OBJECT:P1ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP1Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP1Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP1Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP1F, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p1MainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1MainSC:Panel1:SuspendLayout().
      THIS-OBJECT:p1MainSC:Panel2:SuspendLayout().
      THIS-OBJECT:p1MainSC:SuspendLayout().
      CAST(THIS-OBJECT:p1kodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1kodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p1kodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p1kodSC:SuspendLayout().
      CAST(THIS-OBJECT:p1arbkodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1arbkodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p1arbkodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p1arbkodSC:SuspendLayout().
      THIS-OBJECT:p1arbkodStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP1arbkod, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p1KoderMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p1KoderMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:p1KoderMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:p1KoderMenuSC:SuspendLayout().
      THIS-OBJECT:p1KodertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerP1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerP1:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabP1frekvens */
      /*  */
      THIS-OBJECT:TabP1frekvens:Controls:Add(THIS-OBJECT:p1FrekvensSC).
      THIS-OBJECT:TabP1frekvens:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabP1frekvens:Name = "TabP1frekvens".
      THIS-OBJECT:TabP1frekvens:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p1FrekvensSC */
      /*  */
      THIS-OBJECT:p1FrekvensSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1FrekvensSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1FrekvensSC:Name = "p1FrekvensSC".
      /*  */
      /* p1FrekvensSC.Panel1 */
      /*  */
      THIS-OBJECT:p1FrekvensSC:Panel1:Controls:Add(THIS-OBJECT:p1frekvensSC2).
      /*  */
      /* p1FrekvensSC.Panel2 */
      /*  */
      THIS-OBJECT:p1FrekvensSC:Panel2:Controls:Add(THIS-OBJECT:gridP1Frekvens).
      THIS-OBJECT:p1FrekvensSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p1FrekvensSC:SplitterDistance = 349.
      THIS-OBJECT:p1FrekvensSC:TabIndex = 1.
      /*  */
      /* p1frekvensSC2 */
      /*  */
      THIS-OBJECT:p1frekvensSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1frekvensSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p1frekvensSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1frekvensSC2:Name = "p1frekvensSC2".
      /*  */
      /* p1frekvensSC2.Panel1 */
      /*  */
      THIS-OBJECT:p1frekvensSC2:Panel1:Controls:Add(THIS-OBJECT:gridP2forP1).
      /*  */
      /* p1frekvensSC2.Panel2 */
      /*  */
      THIS-OBJECT:p1frekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP1FrekvensBort).
      THIS-OBJECT:p1frekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP1frekvens).
      THIS-OBJECT:p1frekvensSC2:Size = NEW System.Drawing.Size(349, 284).
      THIS-OBJECT:p1frekvensSC2:SplitterDistance = 310.
      THIS-OBJECT:p1frekvensSC2:TabIndex = 0.
      /*  */
      /* gridP2forP1 */
      /*  */
      THIS-OBJECT:gridP2forP1:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridP2forP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2forP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridP2forP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2forP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridP2forP1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP2forP1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP2forP1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP2forP1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP2forP1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP2forP1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP2forP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP2forP1:GuruContext = ?.
      THIS-OBJECT:gridP2forP1:GuruDraggable = TRUE.
      THIS-OBJECT:gridP2forP1:GuruDroppable = TRUE.
      THIS-OBJECT:gridP2forP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP2forP1:Name = "gridP2forP1".
      THIS-OBJECT:gridP2forP1:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridP2forP1:TabIndex = 1.
      THIS-OBJECT:gridP2forP1:Text = "grid1".
      THIS-OBJECT:gridP2forP1:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2ForP1_GuruAfterDropped).
      /*  */
      /* buttonOverP1FrekvensBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverP1FrekvensBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverP1FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP1FrekvensBort:Name = "buttonOverP1FrekvensBort".
      THIS-OBJECT:buttonOverP1FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1FrekvensBort:TabIndex = 1.
      THIS-OBJECT:buttonOverP1FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP1FrekvensBort_Click).
      /*  */
      /* buttonOverP1frekvens */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverP1frekvens:Appearance = appearance14.
      THIS-OBJECT:buttonOverP1frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP1frekvens:Name = "buttonOverP1frekvens".
      THIS-OBJECT:buttonOverP1frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1frekvens:TabIndex = 0.
      THIS-OBJECT:buttonOverP1frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP1Frekvens_Click).
      /*  */
      /* gridP1Frekvens */
      /*  */
      THIS-OBJECT:gridP1Frekvens:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP1Frekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP1Frekvens:GuruContext = ?.
      THIS-OBJECT:gridP1Frekvens:GuruDraggable = TRUE.
      THIS-OBJECT:gridP1Frekvens:GuruDroppable = TRUE.
      THIS-OBJECT:gridP1Frekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP1Frekvens:Name = "gridP1Frekvens".
      THIS-OBJECT:gridP1Frekvens:Size = NEW System.Drawing.Size(788, 284).
      THIS-OBJECT:gridP1Frekvens:TabIndex = 0.
      THIS-OBJECT:gridP1Frekvens:Text = "grid1".
      THIS-OBJECT:gridP1Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP1Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP1Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp1frekvens_AfterCellUpdate).
      /*  */
      /* TabP1Lista */
      /*  */
      THIS-OBJECT:TabP1Lista:Controls:Add(THIS-OBJECT:splitContainerP1ListaGrund).
      THIS-OBJECT:TabP1Lista:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP1Lista:Name = "TabP1Lista".
      THIS-OBJECT:TabP1Lista:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP1ListaGrund */
      /*  */
      THIS-OBJECT:splitContainerP1ListaGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP1ListaGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP1ListaGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP1ListaGrund:Name = "splitContainerP1ListaGrund".
      THIS-OBJECT:splitContainerP1ListaGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP1ListaGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP1ListaGrund:Panel1:Controls:Add(THIS-OBJECT:P1ListaStrip).
      /*  */
      /* splitContainerP1ListaGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP1ListaGrund:Panel2:Controls:Add(THIS-OBJECT:gridP1Artikelfrekvens).
      THIS-OBJECT:splitContainerP1ListaGrund:Panel2:Controls:Add(THIS-OBJECT:gridP1Lista).
      THIS-OBJECT:splitContainerP1ListaGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP1ListaGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP1ListaGrund:TabIndex = 6.
      /*  */
      /* P1ListaStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP1Lista.
      THIS-OBJECT:P1ListaStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:P1ListaStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P1ListaStrip:Name = "P1ListaStrip".
      THIS-OBJECT:P1ListaStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P1ListaStrip:TabIndex = 0.
      THIS-OBJECT:P1ListaStrip:Text = "P1ListaToolStrip".
      /*  */
      /* StripButtonP1Lista */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:P1ListaToolStripMenuItem.
      arrayvar1[2] = THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP1Lista:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonP1Lista:Image = CAST(resources:GetObject("StripButtonP1Lista.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Lista:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Lista:Name = "StripButtonP1Lista".
      THIS-OBJECT:StripButtonP1Lista:Size = NEW System.Drawing.Size(78, 22).
      THIS-OBJECT:StripButtonP1Lista:Text = "Välj lista".
      /*  */
      /* P1ListaToolStripMenuItem */
      /*  */
      THIS-OBJECT:P1ListaToolStripMenuItem:Image = CAST(resources:GetObject("P1ListaToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:P1ListaToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:P1ListaToolStripMenuItem:Name = "P1ListaToolStripMenuItem".
      THIS-OBJECT:P1ListaToolStripMenuItem:Size = NEW System.Drawing.Size(227, 56).
      THIS-OBJECT:P1ListaToolStripMenuItem:Text = "Detaljerad information".
      THIS-OBJECT:P1ListaToolStripMenuItem:Click:Subscribe(THIS-OBJECT:P1ListaToolStripMenuItem_Click).
      /*  */
      /* P1ArtikelFrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:Image = CAST(resources:GetObject("P1ArtikelFrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:Name = "P1ArtikelFrekvensToolStripMenuItem".
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(227, 56).
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:Text = "Artikelfrekvens P1".
      THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:P1ArtikelFrekvensToolStripMenuItem_Click).
      /*  */
      /* gridP1Artikelfrekvens */
      /*  */
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP1Artikelfrekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP1Artikelfrekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP1Artikelfrekvens:GuruContext = ?.
      THIS-OBJECT:gridP1Artikelfrekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP1Artikelfrekvens:Name = "gridP1Artikelfrekvens".
      THIS-OBJECT:gridP1Artikelfrekvens:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:gridP1Artikelfrekvens:TabIndex = 3.
      THIS-OBJECT:gridP1Artikelfrekvens:Text = "grid1".
      THIS-OBJECT:gridP1Artikelfrekvens:Visible = FALSE.
      /*  */
      /* gridP1Lista */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:gridP1Lista:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1Lista:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Lista:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Lista:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:gridP1Lista:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Lista:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:gridP1Lista:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1Lista:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1Lista:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:gridP1Lista:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP1Lista:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP1Lista:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP1Lista:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP1Lista:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP1Lista:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP1Lista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP1Lista:GuruContext = ?.
      THIS-OBJECT:gridP1Lista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP1Lista:Name = "gridP1Lista".
      THIS-OBJECT:gridP1Lista:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:gridP1Lista:TabIndex = 2.
      THIS-OBJECT:gridP1Lista:Text = "grid1".
      /*  */
      /* TabP1Import */
      /*  */
      THIS-OBJECT:TabP1Import:Controls:Add(THIS-OBJECT:splitContainerP1ImportGrund).
      THIS-OBJECT:TabP1Import:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP1Import:Name = "TabP1Import".
      THIS-OBJECT:TabP1Import:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP1ImportGrund */
      /*  */
      THIS-OBJECT:splitContainerP1ImportGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP1ImportGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP1ImportGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP1ImportGrund:Name = "splitContainerP1ImportGrund".
      THIS-OBJECT:splitContainerP1ImportGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP1ImportGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP1ImportGrund:Panel1:Controls:Add(THIS-OBJECT:P1ImportStrip).
      /*  */
      /* splitContainerP1ImportGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP1ImportGrund:Panel2:Controls:Add(THIS-OBJECT:splitContainerP1Import).
      THIS-OBJECT:splitContainerP1ImportGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP1ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP1ImportGrund:TabIndex = 5.
      /*  */
      /* P1ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:StripButtonP1Import.
      arrayvar2[2] = THIS-OBJECT:StripButtonP1Rin.
      arrayvar2[3] = THIS-OBJECT:StripButtonP1Kompin.
      arrayvar2[4] = THIS-OBJECT:StripButtonP1Rbort.
      THIS-OBJECT:P1ImportStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:P1ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P1ImportStrip:Name = "P1ImportStrip".
      THIS-OBJECT:P1ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P1ImportStrip:TabIndex = 0.
      THIS-OBJECT:P1ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP1Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:p1KoderToolStripMenuItem.
      arrayvar3[2] = THIS-OBJECT:p1FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP1Import:DropDownItems:AddRange(arrayvar3).
      THIS-OBJECT:StripButtonP1Import:Image = CAST(resources:GetObject("StripButtonP1Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Import:Name = "StripButtonP1Import".
      THIS-OBJECT:StripButtonP1Import:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:StripButtonP1Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP1Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p1KoderToolStripMenuItem */
      /*  */
      THIS-OBJECT:p1KoderToolStripMenuItem:Image = CAST(resources:GetObject("p1KoderToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p1KoderToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p1KoderToolStripMenuItem:Name = "p1KoderToolStripMenuItem".
      THIS-OBJECT:p1KoderToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p1KoderToolStripMenuItem:Text = "P1 Kod".
      THIS-OBJECT:p1KoderToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1KoderToolStripMenuItem_Click).
      /*  */
      /* p1FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p1FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p1FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Name = "p1FrekvensToolStripMenuItem".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Text = "P1 Frekvens".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP1Rin */
      /*  */
      THIS-OBJECT:StripButtonP1Rin:Image = CAST(resources:GetObject("StripButtonP1Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rin:Name = "StripButtonP1Rin".
      THIS-OBJECT:StripButtonP1Rin:Size = NEW System.Drawing.Size(118, 22).
      THIS-OBJECT:StripButtonP1Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP1Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP1Rin:Click:Subscribe(THIS-OBJECT:StripButtonP1Rin_Click).
      /*  */
      /* StripButtonP1Kompin */
      /*  */
      THIS-OBJECT:StripButtonP1Kompin:Image = CAST(resources:GetObject("StripButtonP1Kompin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Kompin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Kompin:Name = "StripButtonP1Kompin".
      THIS-OBJECT:StripButtonP1Kompin:Size = NEW System.Drawing.Size(180, 22).
      THIS-OBJECT:StripButtonP1Kompin:Text = "Komplettera med Excel listan".
      THIS-OBJECT:StripButtonP1Kompin:Visible = FALSE.
      THIS-OBJECT:StripButtonP1Kompin:Click:Subscribe(THIS-OBJECT:StripButtonP1Kompin_Click).
      /*  */
      /* StripButtonP1Rbort */
      /*  */
      THIS-OBJECT:StripButtonP1Rbort:Image = CAST(resources:GetObject("StripButtonP1Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rbort:Name = "StripButtonP1Rbort".
      THIS-OBJECT:StripButtonP1Rbort:Size = NEW System.Drawing.Size(161, 22).
      THIS-OBJECT:StripButtonP1Rbort:Text = "Ta bort markerade poster ".
      THIS-OBJECT:StripButtonP1Rbort:Visible = FALSE.
      THIS-OBJECT:StripButtonP1Rbort:Click:Subscribe(THIS-OBJECT:StripButtonP1Rbort_Click).
      /*  */
      /* splitContainerP1Import */
      /*  */
      THIS-OBJECT:splitContainerP1Import:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP1Import:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP1Import:Name = "splitContainerP1Import".
      /*  */
      /* splitContainerP1Import.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP1Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP1F).
      THIS-OBJECT:splitContainerP1Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP1).
      THIS-OBJECT:splitContainerP1Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP1Import:SplitterDistance = 379.
      THIS-OBJECT:splitContainerP1Import:TabIndex = 0.
      /*  */
      /* GridImportP1F */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridImportP1F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridImportP1F:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP1F:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP1F:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP1F:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP1F:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP1F:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP1F:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP1F:GuruContext = ?.
      THIS-OBJECT:GridImportP1F:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP1F:Name = "GridImportP1F".
      THIS-OBJECT:GridImportP1F:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP1F:TabIndex = 1.
      THIS-OBJECT:GridImportP1F:Text = "grid1".
      /*  */
      /* GridImportP1 */
      /*  */
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:GridImportP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:GridImportP1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP1:GuruContext = ?.
      THIS-OBJECT:GridImportP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP1:Name = "GridImportP1".
      THIS-OBJECT:GridImportP1:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP1:TabIndex = 0.
      THIS-OBJECT:GridImportP1:Text = "grid1".
      /*  */
      /* p1MainSC */
      /*  */
      THIS-OBJECT:p1MainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1MainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p1MainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1MainSC:Name = "p1MainSC".
      THIS-OBJECT:p1MainSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p1MainSC.Panel1 */
      /*  */
      THIS-OBJECT:p1MainSC:Panel1:Controls:Add(THIS-OBJECT:p1kodSC).
      /*  */
      /* p1MainSC.Panel2 */
      /*  */
      THIS-OBJECT:p1MainSC:Panel2:Controls:Add(THIS-OBJECT:TabManagerP1).
      THIS-OBJECT:p1MainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:p1MainSC:SplitterDistance = 230.
      THIS-OBJECT:p1MainSC:TabIndex = 1.
      /*  */
      /* p1kodSC */
      /*  */
      THIS-OBJECT:p1kodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1kodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p1kodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1kodSC:Name = "p1kodSC".
      /*  */
      /* p1kodSC.Panel1 */
      /*  */
      THIS-OBJECT:p1kodSC:Panel1:Controls:Add(THIS-OBJECT:p1arbkodSC).
      /*  */
      /* p1kodSC.Panel2 */
      /*  */
      THIS-OBJECT:p1kodSC:Panel2:Controls:Add(THIS-OBJECT:p1KoderMenuSC).
      THIS-OBJECT:p1kodSC:Size = NEW System.Drawing.Size(1145, 230).
      THIS-OBJECT:p1kodSC:SplitterDistance = 400.
      THIS-OBJECT:p1kodSC:TabIndex = 1.
      /*  */
      /* p1arbkodSC */
      /*  */
      THIS-OBJECT:p1arbkodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1arbkodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p1arbkodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1arbkodSC:Name = "p1arbkodSC".
      THIS-OBJECT:p1arbkodSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p1arbkodSC.Panel1 */
      /*  */
      THIS-OBJECT:p1arbkodSC:Panel1:Controls:Add(THIS-OBJECT:p1arbkodStrip).
      /*  */
      /* p1arbkodSC.Panel2 */
      /*  */
      THIS-OBJECT:p1arbkodSC:Panel2:Controls:Add(THIS-OBJECT:gridP1arbkod).
      THIS-OBJECT:p1arbkodSC:Size = NEW System.Drawing.Size(400, 230).
      THIS-OBJECT:p1arbkodSC:SplitterDistance = 25.
      THIS-OBJECT:p1arbkodSC:TabIndex = 0.
      /*  */
      /* p1arbkodStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:laggTIllP1arbkod.
      arrayvar4[2] = THIS-OBJECT:taBortP1arbkodButton.
      THIS-OBJECT:p1arbkodStrip:Items:AddRange(arrayvar4).
      THIS-OBJECT:p1arbkodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1arbkodStrip:Name = "p1arbkodStrip".
      THIS-OBJECT:p1arbkodStrip:Size = NEW System.Drawing.Size(400, 25).
      THIS-OBJECT:p1arbkodStrip:TabIndex = 0.
      THIS-OBJECT:p1arbkodStrip:Text = "toolStrip1".
      /*  */
      /* laggTIllP1arbkod */
      /*  */
      THIS-OBJECT:laggTIllP1arbkod:Image = CAST(resources:GetObject("laggTIllP1arbkod.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTIllP1arbkod:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTIllP1arbkod:Name = "laggTIllP1arbkod".
      THIS-OBJECT:laggTIllP1arbkod:Size = NEW System.Drawing.Size(144, 22).
      THIS-OBJECT:laggTIllP1arbkod:Text = "Lägg till P1-Arbetskod".
      THIS-OBJECT:laggTIllP1arbkod:Click:Subscribe(THIS-OBJECT:laggTIllP1arbkod_Click).
      /*  */
      /* taBortP1arbkodButton */
      /*  */
      THIS-OBJECT:taBortP1arbkodButton:Image = CAST(resources:GetObject("taBortP1arbkodButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP1arbkodButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP1arbkodButton:Name = "taBortP1arbkodButton".
      THIS-OBJECT:taBortP1arbkodButton:Size = NEW System.Drawing.Size(138, 22).
      THIS-OBJECT:taBortP1arbkodButton:Text = "Ta bort P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:ToolTipText = "Tar bort markerad P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP1arbkodButton_Click).
      /*  */
      /* gridP1arbkod */
      /*  */
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Appearance = appearance75.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance76:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance76:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance76:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:Appearance = appearance76.
      appearance77:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance77.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance78:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance78:BackColor2 = System.Drawing.SystemColors:Control.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance78:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance78.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance79.
      appearance80:BackColor = System.Drawing.SystemColors:Highlight.
      appearance80:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance80.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CardAreaAppearance = appearance81.
      appearance82:BorderColor = System.Drawing.Color:Silver.
      appearance82:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellAppearance = appearance82.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance83:BackColor = System.Drawing.SystemColors:Control.
      appearance83:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance83:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance83:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance83:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance83.
      appearance84:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderAppearance = appearance84.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      appearance85:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowAppearance = appearance85.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance86:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance86.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP1arbkod:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP1arbkod:GuruContext = ?.
      THIS-OBJECT:gridP1arbkod:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP1arbkod:Name = "gridP1arbkod".
      THIS-OBJECT:gridP1arbkod:Size = NEW System.Drawing.Size(400, 201).
      THIS-OBJECT:gridP1arbkod:TabIndex = 0.
      THIS-OBJECT:gridP1arbkod:Text = "grid1".
      THIS-OBJECT:gridP1arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp1arbkod_AfterCellUpdate).
      THIS-OBJECT:gridP1arbkod:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp1arbkod_BeforeCellUpdate).
      /*  */
      /* p1KoderMenuSC */
      /*  */
      THIS-OBJECT:p1KoderMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p1KoderMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p1KoderMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1KoderMenuSC:Name = "p1KoderMenuSC".
      THIS-OBJECT:p1KoderMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p1KoderMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:p1KoderMenuSC:Panel1:Controls:Add(THIS-OBJECT:p1KodertoolStrip).
      /*  */
      /* p1KoderMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:p1KoderMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridP1).
      THIS-OBJECT:p1KoderMenuSC:Size = NEW System.Drawing.Size(741, 230).
      THIS-OBJECT:p1KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p1KoderMenuSC:TabIndex = 0.
      /*  */
      /* p1KodertoolStrip */
      /*  */
      THIS-OBJECT:p1KodertoolStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar5[1] = THIS-OBJECT:laggTillP1Button.
      arrayvar5[2] = THIS-OBJECT:copyP1Button.
      arrayvar5[3] = THIS-OBJECT:taBortP1Button.
      THIS-OBJECT:p1KodertoolStrip:Items:AddRange(arrayvar5).
      THIS-OBJECT:p1KodertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1KodertoolStrip:Name = "p1KodertoolStrip".
      THIS-OBJECT:p1KodertoolStrip:Size = NEW System.Drawing.Size(741, 25).
      THIS-OBJECT:p1KodertoolStrip:TabIndex = 0.
      THIS-OBJECT:p1KodertoolStrip:Text = "toolStrip1".
      /*  */
      /* laggTillP1Button */
      /*  */
      THIS-OBJECT:laggTillP1Button:Image = CAST(resources:GetObject("laggTillP1Button.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP1Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP1Button:Name = "laggTillP1Button".
      THIS-OBJECT:laggTillP1Button:Size = NEW System.Drawing.Size(127, 22).
      THIS-OBJECT:laggTillP1Button:Text = "Lägg till ny P1-Kod".
      THIS-OBJECT:laggTillP1Button:Click:Subscribe(THIS-OBJECT:laggTillP1Button_Click).
      /*  */
      /* taBortP1Button */
      /*  */
      THIS-OBJECT:taBortP1Button:Image = CAST(resources:GetObject("taBortP1Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP1Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP1Button:Name = "taBortP1Button".
      THIS-OBJECT:taBortP1Button:Size = NEW System.Drawing.Size(105, 22).
      THIS-OBJECT:taBortP1Button:Text = "Ta bort P1-Kod".
      THIS-OBJECT:taBortP1Button:ToolTipText = "Tar bort markerad P1-Kod".
      THIS-OBJECT:taBortP1Button:Click:Subscribe(THIS-OBJECT:taBortP1Button_Click).
      /*  */
      /* copyP1Button */
      /*  */
      THIS-OBJECT:copyP1Button:Image = CAST(resources:GetObject("copyP1Button.Image"), System.Drawing.Image).
      THIS-OBJECT:copyP1Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:copyP1Button:Name = "copyP1Button".
      THIS-OBJECT:copyP1Button:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:copyP1Button:Text = "Kopiera P1-Kod".
      THIS-OBJECT:copyP1Button:ToolTipText = "Markera den P1 kod som ska kopieras, och fyll i till vilken arbetskod löpnr den ska kopieras.".
      THIS-OBJECT:copyP1Button:Click:Subscribe(THIS-OBJECT:copyP1Button_Click).
      /*  */
      /* gridP1 */
      /*  */
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1:DisplayLayout:Appearance = appearance87.
      THIS-OBJECT:gridP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance88:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance88:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance88:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:Appearance = appearance88.
      appearance89:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance89.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance90:BackColor2 = System.Drawing.SystemColors:Control.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance90:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:PromptAppearance = appearance90.
      THIS-OBJECT:gridP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveCellAppearance = appearance91.
      appearance92:BackColor = System.Drawing.SystemColors:Highlight.
      appearance92:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveRowAppearance = appearance92.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CardAreaAppearance = appearance93.
      appearance94:BorderColor = System.Drawing.Color:Silver.
      appearance94:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellAppearance = appearance94.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellPadding = 0.
      appearance95:BackColor = System.Drawing.SystemColors:Control.
      appearance95:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance95:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance95:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance95:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:GroupByRowAppearance = appearance95.
      appearance96:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderAppearance = appearance96.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      appearance97:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowAppearance = appearance97.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance98:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance98.
      THIS-OBJECT:gridP1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP1:GuruContext = ?.
      THIS-OBJECT:gridP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP1:Name = "gridP1".
      THIS-OBJECT:gridP1:Size = NEW System.Drawing.Size(741, 201).
      THIS-OBJECT:gridP1:TabIndex = 0.
      THIS-OBJECT:gridP1:Text = "grid1".
      THIS-OBJECT:gridP1:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp1_AfterCellUpdate).
      THIS-OBJECT:gridP1:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp1_BeforeCellUpdate).
      /*  */
      /* TabManagerP1 */
      /*  */
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP1).
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:TabP1frekvens).
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:TabP1Import).
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:TabP1Lista).
      THIS-OBJECT:TabManagerP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP1:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP1:Name = "TabManagerP1".
      THIS-OBJECT:TabManagerP1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP1.
      THIS-OBJECT:TabManagerP1:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP1:TabIndex = 2.
      appearance99:Image = CAST(resources:GetObject("appearance99.Image"), System.Object).
      ultraTab6:Appearance = appearance99.
      ultraTab6:Key = "P1Frekvens".
      ultraTab6:TabPage = THIS-OBJECT:TabP1frekvens.
      ultraTab6:Text = "P1-Frekvenser".
      appearance100:Image = CAST(resources:GetObject("appearance100.Image"), System.Object).
      ultraTab1:Appearance = appearance100.
      ultraTab1:Key = "P1Listor".
      ultraTab1:TabPage = THIS-OBJECT:TabP1Lista.
      ultraTab1:Text = "P1-Listor".
      appearance101:Image = CAST(resources:GetObject("appearance101.Image"), System.Object).
      ultraTab29:ActiveAppearance = appearance101.
      appearance102:Image = CAST(resources:GetObject("appearance102.Image"), System.Object).
      ultraTab29:Appearance = appearance102.
      ultraTab29:Key = "Import".
      ultraTab29:TabPage = THIS-OBJECT:TabP1Import.
      ultraTab29:Text = "Import Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar6[1] = ultraTab6.
      arrayvar6[2] = ultraTab1.
      arrayvar6[3] = ultraTab29.
      THIS-OBJECT:TabManagerP1:Tabs:AddRange(arrayvar6).
      THIS-OBJECT:TabManagerP1:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP1_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP1:Name = "ultraTabSharedControlsPageP1".
      THIS-OBJECT:ultraTabSharedControlsPageP1:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* EkgP1Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p1MainSC).
      THIS-OBJECT:Name = "EkgP1Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabP1frekvens:ResumeLayout(FALSE).
      THIS-OBJECT:p1FrekvensSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1FrekvensSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1FrekvensSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1FrekvensSC:ResumeLayout(FALSE).
      THIS-OBJECT:p1frekvensSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1frekvensSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1frekvensSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1frekvensSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridP2forP1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP1Frekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP1Lista:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP1ListaGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP1ListaGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP1ListaGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP1ListaGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP1ListaGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P1ListaStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P1ListaStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP1Artikelfrekvens, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP1Lista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP1Import:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP1ImportGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP1ImportGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP1ImportGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP1ImportGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP1ImportGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P1ImportStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P1ImportStrip:PerformLayout().
      THIS-OBJECT:splitContainerP1Import:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP1Import, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP1Import:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridImportP1F, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1MainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1MainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1MainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1MainSC:ResumeLayout(FALSE).
      THIS-OBJECT:p1kodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1kodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1kodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1kodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p1arbkodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1arbkodSC:Panel1:PerformLayout().
      THIS-OBJECT:p1arbkodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1arbkodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1arbkodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p1arbkodStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p1arbkodStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP1arbkod, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1KoderMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p1KoderMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:p1KoderMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p1KoderMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p1KoderMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:p1KodertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p1KodertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerP1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerP1:ResumeLayout(FALSE).
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
   METHOD PRIVATE VOID buttonOverP1Frekvens_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP1Frekvens().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP1FrekvensBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP1FrekvensBort().
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1arbkod_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P1ArbkodUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
            
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1arbkod_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P1Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
   
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.   
   METHOD PRIVATE VOID gridp1frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P1FrekvensUppdate(INPUT kolnamn, INPUT kolvarde).           
      RETURN.
   END METHOD.
   
     /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP1Frekvens_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP2ForP1 THEN DO:
         THIS-OBJECT:buttonOverP1Frekvens_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP2ForP1_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP1Frekvens THEN DO:
         THIS-OBJECT:buttonOverP1FrekvensBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTIllP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP1arbkodPop:Show().
      THIS-OBJECT:ControlShell:fillinP1ArbKod:GuruText:Select().
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP1Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP1Pop:Show().
      THIS-OBJECT:ControlShell:fillinP1LopNr:GuruText:Select().
      RETURN.

   END METHOD.
   
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID P1ArtikelFrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:gridP1Lista:VISIBLE = FALSE.
      THIS-OBJECT:gridP1ArtikelFrekvens:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:HamtP1detaljmtrl().  
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p1FrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP1F:VISIBLE = TRUE.
      THIS-OBJECT:GridImportP1:VISIBLE = FALSE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P1F").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p1KoderToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP1F:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP1:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P1").
      RETURN.

   END METHOD.
   
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID P1ListaToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:gridP1Lista:VISIBLE = TRUE.
		THIS-OBJECT:gridP1ArtikelFrekvens:VISIBLE = FALSE.		
			
		THIS-OBJECT:ControlDataDb:ekgp1detaljTTh:FIND-FIRST() NO-ERROR.
		IF NOT THIS-OBJECT:ControlDataDb:ekgp1detaljTTh:AVAILABLE THEN DO: 
		   THIS-OBJECT:ControlWinLg:HamtP1detaljlista("1").
		END.     
		    
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID StripButtonP1Kompin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P1K").
		RETURN.

	END METHOD.

  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP1Rbort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("P1").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP1Rin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P1").
      RETURN.

   END METHOD.
   
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP1_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(THIS-OBJECT:TabManagerP1:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP1:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP1arbkodButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP1ArbKod().
      RETURN.

   END METHOD.
   
   
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP1Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP1().
      RETURN.

   END METHOD.


END CLASS.
