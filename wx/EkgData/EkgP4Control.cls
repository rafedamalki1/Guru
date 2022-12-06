 
 /*------------------------------------------------------------------------
    File        : EkgP4Control
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Oct 10 14:57:10 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgP4Control INHERITS UserControl: 
   {EKGTH.I}
   DEFINE PRIVATE VARIABLE p4ListaToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4Lista AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE P4ListaStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p4ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p4KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p4KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p4KodToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE p4FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p4FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP4Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5ForP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP4ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP4Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP4FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP4Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE P4ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP4F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP4ListaGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Lista AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP4Lista AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP4 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP4Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP4ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP4Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP4 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP4Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP4Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP4Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.


   CONSTRUCTOR PUBLIC EkgP4Control (INPUT r AS Guru.Root):
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
      /*P4*/
      THIS-OBJECT:CreateGridP5ForP4().
      THIS-OBJECT:CreateGridP4().
      THIS-OBJECT:CreateGridP4Resurs().
      THIS-OBJECT:CreateGridResursForP4().
      THIS-OBJECT:CreateGridP4Frekvens().
      THIS-OBJECT:CreateGridImportP4().
      THIS-OBJECT:CreateGridP4Lista().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP5ForP4():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      
      THIS-OBJECT:GridP5ForP4:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ekgp5TTh)).
      THIS-OBJECT:GridP5ForP4:Text = "P5-Koder".
      THIS-OBJECT:GridP5ForP4:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP5ForP4:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:GridP5ForP4:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP5ForP4:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.  
   
   METHOD PUBLIC VOID CreateGridP4(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[6] = NEW Controls.GridRubrik("ENHET","",FALSE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("FREKVENS","",FALSE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("MONTTIM","",TRUE,70,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MASKTIM","",TRUE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:GridP4:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 17, INPUT THIS-OBJECT:ekgp4TTh)).
      THIS-OBJECT:GridP4:Text = "P4-Koder".
      THIS-OBJECT:GridP4:GuruUpdateTitle().
      THIS-OBJECT:GridP4:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeP4).
      
      /*DÖLJ*/
      THIS-OBJECT:GridP4:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:GridP4:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP4:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP4Resurs(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("P4ENHET","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[8] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,100,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("RESURSPRIS","",TRUE,75,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EAMONT","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
     
      THIS-OBJECT:GridP4Resurs:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp4resursTTh)).
      THIS-OBJECT:GridP4Resurs:Text = "P4-Resurser".
      THIS-OBJECT:GridP4Resurs:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP4Resurs:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP4Resurs:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridResursForP4():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("PRIS","",TRUE,70,"DEC").
      rubrikergrid[5] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").   
      /*LENA*/
      rubrikergrid[6] = NEW Controls.GridRubrik("REGELID","",TRUE,100,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("UTOVERFREK","",TRUE,70,"LOG").
      
      THIS-OBJECT:GridResursForP4:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:ekgresursForTTh)).
      THIS-OBJECT:GridResursForP4:Text = "Resurser".
   END METHOD.  
   
   METHOD PUBLIC VOID CreateGridP4Frekvens(): 
      {GridRubrikListaVarExtent.i}
  
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P4LOPNR","",TRUE,70, "EKGLOP").
      rubrikergrid[5] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,150).
      rubrikergrid[6] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("P5LOPNR","",TRUE,70, "EKGLOP").
      rubrikergrid[8] = NEW Controls.GridRubrik("P5BENAMNING","",TRUE,250).
      rubrikergrid[9] = NEW Controls.GridRubrik("ANTAL","",FALSE,70,"DEC").
      rubrikergrid[10] = NEW Controls.GridRubrik("FMONTTIM","",TRUE,70,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("FMASKTIM","",TRUE,70,"DEC").
      rubrikergrid[12] = NEW Controls.GridRubrik("FARBETEKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("FMASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("FOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[15] = NEW Controls.GridRubrik("FSUMMA","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("FEAMASK","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("FEAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("FEA","",TRUE,70,"DEC").  
      
      THIS-OBJECT:GridP4Frekvens:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp4frekvensTTh)).
      THIS-OBJECT:GridP4Frekvens:Text = "P4-Frekvenser".
      THIS-OBJECT:GridP4Frekvens:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P4LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["P5LOPNR"]:Hidden = TRUE.
      THIS-OBJECT:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridP4Frekvens:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridImportP4(): 
      {GridRubrikListaVarExtent.i}     
      rubrikergrid[1] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("FREKVENS","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,150).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).
      THIS-OBJECT:GridImportP4:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:importP4TTh)).
      THIS-OBJECT:GridImportP4:Text = "P4 Kod Import".
      THIS-OBJECT:GridImportP4:GuruUpdateTitle().
      
      THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP4).   
      
      rubrikergrid[1] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("P4BENAMNING","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("P5BENAMNING","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,70,"DEC"). 
      rubrikergrid[6] = NEW Controls.GridRubrik("MEDDELANDE","",TRUE,1300).     
      THIS-OBJECT:GridImportP4F:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:importP4P5TTh)).
      THIS-OBJECT:GridImportP4F:Text = "P4 Frekvens Import".
      THIS-OBJECT:GridImportP4F:GuruUpdateTitle().
      THIS-OBJECT:GridImportP4F:VISIBLE = FALSE.
      
     THIS-OBJECT:ControlWinLg:FelMeddCheckImport(THIS-OBJECT:GridImportP4F).
      
       
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridP4Lista():
      {GridRubrikListaVarExtent.i}                                                  
      rubrikergrid[1] = NEW Controls.GridRubrik("P4ARBKOD","",TRUE,70).                 
      rubrikergrid[2] = NEW Controls.GridRubrik("P5ARBKOD","",TRUE,70).                          
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,150).
      rubrikergrid[5] = NEW Controls.GridRubrik("RANTAL","Resursantal",TRUE,75,"DEC").
      rubrikergrid[6] = NEW Controls.GridRubrik("TOTANTAL","",TRUE,90,"DEC").            
      rubrikergrid[7] = NEW Controls.GridRubrik("BERTIM","",TRUE,50,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("MONTTIM","",TRUE,50,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("MASKTIM","",TRUE,50,"DEC").      
      rubrikergrid[10] = NEW Controls.GridRubrik("UTRUSTTIM","",TRUE,50,"DEC").
      rubrikergrid[11] = NEW Controls.GridRubrik("ARBETEKOST","",TRUE,70,"DEC").
      /*rubrikergrid[17] = NEW Controls.GridRubrik("MATERIELKOST","",TRUE,70,"DEC").*/
      rubrikergrid[12] = NEW Controls.GridRubrik("MASKINKOST","",TRUE,70,"DEC").
      rubrikergrid[13] = NEW Controls.GridRubrik("UTRUSTKOST"," ",TRUE,70,"DEC").
      rubrikergrid[14] = NEW Controls.GridRubrik("OVRIGT","",TRUE,70,"DEC").
      /*rubrikergrid[20] = NEW Controls.GridRubrik("SUMMA","",TRUE,70,"DEC").    */
      rubrikergrid[15] = NEW Controls.GridRubrik("EAMASK","",TRUE,70,"DEC").
      rubrikergrid[16] = NEW Controls.GridRubrik("EAUTRUST","",TRUE,70,"DEC").
      rubrikergrid[17] = NEW Controls.GridRubrik("EAOVRIGT","",TRUE,70,"DEC").
      rubrikergrid[18] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
           
      THIS-OBJECT:GridP4Lista:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 18, INPUT THIS-OBJECT:ekgp1detaljTTh)).
      THIS-OBJECT:GridP4Lista:Text = "P4-Kod Detaljerad Lista".
      THIS-OBJECT:GridP4Lista:GuruUpdateTitle().
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
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP4Control").
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
      DEFINE VARIABLE ultraTab12 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab12 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
      appearance105 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabP4Resurs = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p4ResursSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4ResursSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridResursForP4 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP4ResursBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP4Resurs = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP4Resurs = NEW Controls.Grid().
      THIS-OBJECT:TabP4Frekvens = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p4FrekvensSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4FrekvensSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridP5ForP4 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP4FrekvensBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP4Frekvens = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP4Frekvens = NEW Controls.Grid().
      THIS-OBJECT:TabP4Lista = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP4ListaGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P4ListaStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP4Lista = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p4ListaToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:gridP4Lista = NEW Controls.Grid().
      THIS-OBJECT:TabP4Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP4ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P4ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP4Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p4KodToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p4FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP4Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP4Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP4Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP4F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP4 = NEW Controls.Grid().
      THIS-OBJECT:p4MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4KodertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP4Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP4Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP4 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP4Resurs:SuspendLayout().
      CAST(THIS-OBJECT:p4ResursSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4ResursSC:Panel1:SuspendLayout().
      THIS-OBJECT:p4ResursSC:Panel2:SuspendLayout().
      THIS-OBJECT:p4ResursSC:SuspendLayout().
      CAST(THIS-OBJECT:p4ResursSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4ResursSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p4ResursSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p4ResursSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForP4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP4Resurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP4Frekvens:SuspendLayout().
      CAST(THIS-OBJECT:p4FrekvensSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4FrekvensSC:Panel1:SuspendLayout().
      THIS-OBJECT:p4FrekvensSC:Panel2:SuspendLayout().
      THIS-OBJECT:p4FrekvensSC:SuspendLayout().
      CAST(THIS-OBJECT:p4FrekvensSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4FrekvensSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p4FrekvensSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p4FrekvensSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridP5ForP4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP4Frekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP4Lista:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP4ListaGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP4ListaGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP4ListaGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP4ListaGrund:SuspendLayout().
      THIS-OBJECT:P4ListaStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP4Lista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP4Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP4ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP4ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP4ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP4ImportGrund:SuspendLayout().
      THIS-OBJECT:P4ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP4Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP4Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP4Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP4F, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p4MainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4MainSC:Panel1:SuspendLayout().
      THIS-OBJECT:p4MainSC:Panel2:SuspendLayout().
      THIS-OBJECT:p4MainSC:SuspendLayout().
      CAST(THIS-OBJECT:p4KoderMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p4KoderMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:p4KoderMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:p4KoderMenuSC:SuspendLayout().
      THIS-OBJECT:p4KodertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerP4, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerP4:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabP4Resurs */
      /*  */
      THIS-OBJECT:TabP4Resurs:Controls:Add(THIS-OBJECT:p4ResursSC).
      THIS-OBJECT:TabP4Resurs:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabP4Resurs:Name = "TabP4Resurs".
      THIS-OBJECT:TabP4Resurs:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p4ResursSC */
      /*  */
      THIS-OBJECT:p4ResursSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4ResursSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4ResursSC:Name = "p4ResursSC".
      /*  */
      /* p4ResursSC.Panel1 */
      /*  */
      THIS-OBJECT:p4ResursSC:Panel1:Controls:Add(THIS-OBJECT:p4ResursSC2).
      /*  */
      /* p4ResursSC.Panel2 */
      /*  */
      THIS-OBJECT:p4ResursSC:Panel2:Controls:Add(THIS-OBJECT:gridP4Resurs).
      THIS-OBJECT:p4ResursSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p4ResursSC:SplitterDistance = 350.
      THIS-OBJECT:p4ResursSC:TabIndex = 1.
      /*  */
      /* p4ResursSC2 */
      /*  */
      THIS-OBJECT:p4ResursSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4ResursSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p4ResursSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4ResursSC2:Name = "p4ResursSC2".
      /*  */
      /* p4ResursSC2.Panel1 */
      /*  */
      THIS-OBJECT:p4ResursSC2:Panel1:Controls:Add(THIS-OBJECT:gridResursForP4).
      /*  */
      /* p4ResursSC2.Panel2 */
      /*  */
      THIS-OBJECT:p4ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP4ResursBort).
      THIS-OBJECT:p4ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP4Resurs).
      THIS-OBJECT:p4ResursSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p4ResursSC2:SplitterDistance = 310.
      THIS-OBJECT:p4ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP4 */
      /*  */
      THIS-OBJECT:gridResursForP4:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResursForP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResursForP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridResursForP4:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForP4:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForP4:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForP4:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForP4:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForP4:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForP4:GuruContext = ?.
      THIS-OBJECT:gridResursForP4:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursForP4:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursForP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForP4:Name = "gridResursForP4".
      THIS-OBJECT:gridResursForP4:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridResursForP4:TabIndex = 1.
      THIS-OBJECT:gridResursForP4:Text = "grid1".
      THIS-OBJECT:gridResursForP4:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP4_GuruAfterDropped).
      /*  */
      /* buttonOverP4ResursBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverP4ResursBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverP4ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP4ResursBort:Name = "buttonOverP4ResursBort".
      THIS-OBJECT:buttonOverP4ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4ResursBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP4ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP4ResursBort_Click).
      /*  */
      /* buttonOverP4Resurs */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverP4Resurs:Appearance = appearance14.
      THIS-OBJECT:buttonOverP4Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP4Resurs:Name = "buttonOverP4Resurs".
      THIS-OBJECT:buttonOverP4Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4Resurs:TabIndex = 4.
      THIS-OBJECT:buttonOverP4Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP4Resurs_Click).
      /*  */
      /* gridP4Resurs */
      /*  */
      THIS-OBJECT:gridP4Resurs:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP4Resurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP4Resurs:GuruContext = ?.
      THIS-OBJECT:gridP4Resurs:GuruDraggable = TRUE.
      THIS-OBJECT:gridP4Resurs:GuruDroppable = TRUE.
      THIS-OBJECT:gridP4Resurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP4Resurs:Name = "gridP4Resurs".
      THIS-OBJECT:gridP4Resurs:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP4Resurs:TabIndex = 0.
      THIS-OBJECT:gridP4Resurs:Text = "grid1".
      THIS-OBJECT:gridP4Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP4Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP4Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp4resurs_AfterCellUpdate).
      /*  */
      /* TabP4Frekvens */
      /*  */
      THIS-OBJECT:TabP4Frekvens:Controls:Add(THIS-OBJECT:p4FrekvensSC).
      THIS-OBJECT:TabP4Frekvens:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4Frekvens:Name = "TabP4Frekvens".
      THIS-OBJECT:TabP4Frekvens:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p4FrekvensSC */
      /*  */
      THIS-OBJECT:p4FrekvensSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4FrekvensSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4FrekvensSC:Name = "p4FrekvensSC".
      /*  */
      /* p4FrekvensSC.Panel1 */
      /*  */
      THIS-OBJECT:p4FrekvensSC:Panel1:Controls:Add(THIS-OBJECT:p4FrekvensSC2).
      /*  */
      /* p4FrekvensSC.Panel2 */
      /*  */
      THIS-OBJECT:p4FrekvensSC:Panel2:Controls:Add(THIS-OBJECT:gridP4Frekvens).
      THIS-OBJECT:p4FrekvensSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p4FrekvensSC:SplitterDistance = 346.
      THIS-OBJECT:p4FrekvensSC:TabIndex = 2.
      /*  */
      /* p4FrekvensSC2 */
      /*  */
      THIS-OBJECT:p4FrekvensSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4FrekvensSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p4FrekvensSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4FrekvensSC2:Name = "p4FrekvensSC2".
      /*  */
      /* p4FrekvensSC2.Panel1 */
      /*  */
      THIS-OBJECT:p4FrekvensSC2:Panel1:Controls:Add(THIS-OBJECT:gridP5ForP4).
      /*  */
      /* p4FrekvensSC2.Panel2 */
      /*  */
      THIS-OBJECT:p4FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP4FrekvensBort).
      THIS-OBJECT:p4FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP4Frekvens).
      THIS-OBJECT:p4FrekvensSC2:Size = NEW System.Drawing.Size(346, 284).
      THIS-OBJECT:p4FrekvensSC2:SplitterDistance = 310.
      THIS-OBJECT:p4FrekvensSC2:TabIndex = 0.
      /*  */
      /* gridP5ForP4 */
      /*  */
      THIS-OBJECT:gridP5ForP4:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP5ForP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP5ForP4:GuruContext = ?.
      THIS-OBJECT:gridP5ForP4:GuruDraggable = TRUE.
      THIS-OBJECT:gridP5ForP4:GuruDroppable = TRUE.
      THIS-OBJECT:gridP5ForP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP5ForP4:Name = "gridP5ForP4".
      THIS-OBJECT:gridP5ForP4:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridP5ForP4:TabIndex = 2.
      THIS-OBJECT:gridP5ForP4:Text = "grid1".
      THIS-OBJECT:gridP5ForP4:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP5ForP4_GuruAfterDropped).
      /*  */
      /* buttonOverP4FrekvensBort */
      /*  */
      appearance39:Image = CAST(resources:GetObject("appearance39.Image"), System.Object).
      THIS-OBJECT:buttonOverP4FrekvensBort:Appearance = appearance39.
      THIS-OBJECT:buttonOverP4FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP4FrekvensBort:Name = "buttonOverP4FrekvensBort".
      THIS-OBJECT:buttonOverP4FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4FrekvensBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP4FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP4FrekvensBort_Click).
      /*  */
      /* buttonOverP4Frekvens */
      /*  */
      appearance40:Image = CAST(resources:GetObject("appearance40.Image"), System.Object).
      THIS-OBJECT:buttonOverP4Frekvens:Appearance = appearance40.
      THIS-OBJECT:buttonOverP4Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP4Frekvens:Name = "buttonOverP4Frekvens".
      THIS-OBJECT:buttonOverP4Frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4Frekvens:TabIndex = 4.
      THIS-OBJECT:buttonOverP4Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP4Frekvens_Click).
      /*  */
      /* gridP4Frekvens */
      /*  */
      THIS-OBJECT:gridP4Frekvens:AllowDrop = TRUE.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Appearance = appearance41.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance42:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance42:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance42:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:Appearance = appearance42.
      appearance43:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance43.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance44:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance44:BackColor2 = System.Drawing.SystemColors:Control.
      appearance44:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance44:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance44.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      appearance45:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance45.
      appearance46:BackColor = System.Drawing.SystemColors:Highlight.
      appearance46:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance46.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance47.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      appearance48:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellAppearance = appearance48.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance49:BackColor = System.Drawing.SystemColors:Control.
      appearance49:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance49:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance49:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance49:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance49.
      appearance50:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderAppearance = appearance50.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:RowAppearance = appearance51.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance52.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP4Frekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP4Frekvens:GuruContext = ?.
      THIS-OBJECT:gridP4Frekvens:GuruDraggable = TRUE.
      THIS-OBJECT:gridP4Frekvens:GuruDroppable = TRUE.
      THIS-OBJECT:gridP4Frekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP4Frekvens:Name = "gridP4Frekvens".
      THIS-OBJECT:gridP4Frekvens:Size = NEW System.Drawing.Size(791, 284).
      THIS-OBJECT:gridP4Frekvens:TabIndex = 1.
      THIS-OBJECT:gridP4Frekvens:Text = "grid1".
      THIS-OBJECT:gridP4Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP4Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP4Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp4frekvens_AfterCellUpdate).
      /*  */
      /* TabP4Lista */
      /*  */
      THIS-OBJECT:TabP4Lista:Controls:Add(THIS-OBJECT:splitContainerP4ListaGrund).
      THIS-OBJECT:TabP4Lista:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4Lista:Name = "TabP4Lista".
      THIS-OBJECT:TabP4Lista:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP4ListaGrund */
      /*  */
      THIS-OBJECT:splitContainerP4ListaGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP4ListaGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP4ListaGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP4ListaGrund:Name = "splitContainerP4ListaGrund".
      THIS-OBJECT:splitContainerP4ListaGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP4ListaGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP4ListaGrund:Panel1:Controls:Add(THIS-OBJECT:P4ListaStrip).
      /*  */
      /* splitContainerP4ListaGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP4ListaGrund:Panel2:Controls:Add(THIS-OBJECT:gridP4Lista).
      THIS-OBJECT:splitContainerP4ListaGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP4ListaGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP4ListaGrund:TabIndex = 9.
      /*  */
      /* P4ListaStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP4Lista.
      THIS-OBJECT:P4ListaStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:P4ListaStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P4ListaStrip:Name = "P4ListaStrip".
      THIS-OBJECT:P4ListaStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P4ListaStrip:TabIndex = 0.
      THIS-OBJECT:P4ListaStrip:Text = "P1ListaToolStrip".
      /*  */
      /* StripButtonP4Lista */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:p4ListaToolStripMenuItem.
      THIS-OBJECT:StripButtonP4Lista:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonP4Lista:Image = CAST(resources:GetObject("StripButtonP4Lista.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP4Lista:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP4Lista:Name = "StripButtonP4Lista".
      THIS-OBJECT:StripButtonP4Lista:Size = NEW System.Drawing.Size(79, 22).
      THIS-OBJECT:StripButtonP4Lista:Text = "Välj lista".
      THIS-OBJECT:StripButtonP4Lista:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p4ListaToolStripMenuItem */
      /*  */
      THIS-OBJECT:p4ListaToolStripMenuItem:Image = CAST(resources:GetObject("p4ListaToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p4ListaToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p4ListaToolStripMenuItem:Name = "p4ListaToolStripMenuItem".
      THIS-OBJECT:p4ListaToolStripMenuItem:Size = NEW System.Drawing.Size(227, 56).
      THIS-OBJECT:p4ListaToolStripMenuItem:Text = "Detaljerad information".
      THIS-OBJECT:P4ListaToolStripMenuItem:Click:Subscribe(THIS-OBJECT:P4ListaToolStripMenuItem_Click).
      /*  */
      /* gridP4Lista */
      /*  */
      appearance53:BackColor = System.Drawing.SystemColors:Window.
      appearance53:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Appearance = appearance53.
      THIS-OBJECT:gridP4Lista:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4Lista:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance54:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance54:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance54:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Lista:DisplayLayout:GroupByBox:Appearance = appearance54.
      appearance55:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Lista:DisplayLayout:GroupByBox:BandLabelAppearance = appearance55.
      THIS-OBJECT:gridP4Lista:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance56:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance56:BackColor2 = System.Drawing.SystemColors:Control.
      appearance56:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Lista:DisplayLayout:GroupByBox:PromptAppearance = appearance56.
      THIS-OBJECT:gridP4Lista:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4Lista:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      appearance57:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:ActiveCellAppearance = appearance57.
      appearance58:BackColor = System.Drawing.SystemColors:Highlight.
      appearance58:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:ActiveRowAppearance = appearance58.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance59:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:CardAreaAppearance = appearance59.
      appearance60:BorderColor = System.Drawing.Color:Silver.
      appearance60:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:CellAppearance = appearance60.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:CellPadding = 0.
      appearance61:BackColor = System.Drawing.SystemColors:Control.
      appearance61:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance61:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance61:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance61:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:GroupByRowAppearance = appearance61.
      appearance62:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:HeaderAppearance = appearance62.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:RowAppearance = appearance63.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4Lista:DisplayLayout:Override:TemplateAddRowAppearance = appearance64.
      THIS-OBJECT:gridP4Lista:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP4Lista:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP4Lista:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP4Lista:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP4Lista:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP4Lista:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP4Lista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP4Lista:GuruContext = ?.
      THIS-OBJECT:gridP4Lista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP4Lista:Name = "gridP4Lista".
      THIS-OBJECT:gridP4Lista:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:gridP4Lista:TabIndex = 2.
      THIS-OBJECT:gridP4Lista:Text = "grid1".
      /*  */
      /* TabP4Import */
      /*  */
      THIS-OBJECT:TabP4Import:Controls:Add(THIS-OBJECT:splitContainerP4ImportGrund).
      THIS-OBJECT:TabP4Import:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4Import:Name = "TabP4Import".
      THIS-OBJECT:TabP4Import:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP4ImportGrund */
      /*  */
      THIS-OBJECT:splitContainerP4ImportGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP4ImportGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP4ImportGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP4ImportGrund:Name = "splitContainerP4ImportGrund".
      THIS-OBJECT:splitContainerP4ImportGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP4ImportGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP4ImportGrund:Panel1:Controls:Add(THIS-OBJECT:P4ImportStrip).
      /*  */
      /* splitContainerP4ImportGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP4ImportGrund:Panel2:Controls:Add(THIS-OBJECT:splitContainerP4Import).
      THIS-OBJECT:splitContainerP4ImportGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP4ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP4ImportGrund:TabIndex = 2.
      /*  */
      /* P4ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:StripButtonP4Import.
      arrayvar2[2] = THIS-OBJECT:StripButtonP4Rin.
      arrayvar2[3] = THIS-OBJECT:StripButtonP4Rbort.
      THIS-OBJECT:P4ImportStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:P4ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P4ImportStrip:Name = "P4ImportStrip".
      THIS-OBJECT:P4ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P4ImportStrip:TabIndex = 0.
      THIS-OBJECT:P4ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP4Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:p4KodToolStripMenuItem.
      arrayvar3[2] = THIS-OBJECT:p4FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP4Import:DropDownItems:AddRange(arrayvar3).
      THIS-OBJECT:StripButtonP4Import:Image = CAST(resources:GetObject("StripButtonP4Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP4Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP4Import:Name = "StripButtonP4Import".
      THIS-OBJECT:StripButtonP4Import:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:StripButtonP4Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP4Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p4KodToolStripMenuItem */
      /*  */
      THIS-OBJECT:p4KodToolStripMenuItem:Image = CAST(resources:GetObject("p4KodToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p4KodToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p4KodToolStripMenuItem:Name = "p4KodToolStripMenuItem".
      THIS-OBJECT:p4KodToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p4KodToolStripMenuItem:Text = "P4 Kod".
      THIS-OBJECT:p4KodToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p4KoderToolStripMenuItem_Click).
      /*  */
      /* p4FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p4FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p4FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Name = "p4FrekvensToolStripMenuItem".
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(170, 56).
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Text = "P4 Frekvens".
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p4FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP4Rin */
      /*  */
      THIS-OBJECT:StripButtonP4Rin:Image = CAST(resources:GetObject("StripButtonP4Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP4Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP4Rin:Name = "StripButtonP4Rin".
      THIS-OBJECT:StripButtonP4Rin:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:StripButtonP4Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP4Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP4Rin:Click:Subscribe(THIS-OBJECT:StripButtonP4Rin_Click).
      /*  */
      /* StripButtonP4Rbort */
      /*  */
      THIS-OBJECT:StripButtonP4Rbort:Image = CAST(resources:GetObject("StripButtonP4Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP4Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP4Rbort:Name = "StripButtonP4Rbort".
      THIS-OBJECT:StripButtonP4Rbort:Size = NEW System.Drawing.Size(163, 22).
      THIS-OBJECT:StripButtonP4Rbort:Text = "Ta bort markerade poster ".
      THIS-OBJECT:StripButtonP4Rbort:Visible = FALSE.
      THIS-OBJECT:StripButtonP4Rbort:Click:Subscribe(THIS-OBJECT:StripButtonP4Rbort_Click).
      /*  */
      /* splitContainerP4Import */
      /*  */
      THIS-OBJECT:splitContainerP4Import:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP4Import:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP4Import:Name = "splitContainerP4Import".
      /*  */
      /* splitContainerP4Import.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP4Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP4F).
      THIS-OBJECT:splitContainerP4Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP4).
      THIS-OBJECT:splitContainerP4Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP4Import:SplitterDistance = 380.
      THIS-OBJECT:splitContainerP4Import:TabIndex = 0.
      /*  */
      /* GridImportP4F */
      /*  */
      appearance65:BackColor = System.Drawing.SystemColors:Window.
      appearance65:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Appearance = appearance65.
      THIS-OBJECT:GridImportP4F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP4F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:GridImportP4F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP4F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
      THIS-OBJECT:GridImportP4F:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP4F:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP4F:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP4F:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP4F:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP4F:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP4F:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP4F:GuruContext = ?.
      THIS-OBJECT:GridImportP4F:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP4F:Name = "GridImportP4F".
      THIS-OBJECT:GridImportP4F:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP4F:TabIndex = 1.
      THIS-OBJECT:GridImportP4F:Text = "grid1".
      /*  */
      /* GridImportP4 */
      /*  */
      appearance77:BackColor = System.Drawing.SystemColors:Window.
      appearance77:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP4:DisplayLayout:Appearance = appearance77.
      THIS-OBJECT:GridImportP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance78:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance78:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance78:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:Appearance = appearance78.
      appearance79:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance79.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance80:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance80:BackColor2 = System.Drawing.SystemColors:Control.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance80:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:PromptAppearance = appearance80.
      THIS-OBJECT:GridImportP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      appearance81:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveCellAppearance = appearance81.
      appearance82:BackColor = System.Drawing.SystemColors:Highlight.
      appearance82:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveRowAppearance = appearance82.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CardAreaAppearance = appearance83.
      appearance84:BorderColor = System.Drawing.Color:Silver.
      appearance84:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellAppearance = appearance84.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellPadding = 0.
      appearance85:BackColor = System.Drawing.SystemColors:Control.
      appearance85:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance85:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance85:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance85:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:GroupByRowAppearance = appearance85.
      appearance86:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderAppearance = appearance86.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:RowAppearance = appearance87.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance88.
      THIS-OBJECT:GridImportP4:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP4:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP4:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP4:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP4:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP4:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP4:GuruContext = ?.
      THIS-OBJECT:GridImportP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP4:Name = "GridImportP4".
      THIS-OBJECT:GridImportP4:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP4:TabIndex = 0.
      THIS-OBJECT:GridImportP4:Text = "grid1".
      /*  */
      /* p4MainSC */
      /*  */
      THIS-OBJECT:p4MainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4MainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p4MainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4MainSC:Name = "p4MainSC".
      THIS-OBJECT:p4MainSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p4MainSC.Panel1 */
      /*  */
      THIS-OBJECT:p4MainSC:Panel1:Controls:Add(THIS-OBJECT:p4KoderMenuSC).
      /*  */
      /* p4MainSC.Panel2 */
      /*  */
      THIS-OBJECT:p4MainSC:Panel2:Controls:Add(THIS-OBJECT:TabManagerP4).
      THIS-OBJECT:p4MainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:p4MainSC:SplitterDistance = 230.
      THIS-OBJECT:p4MainSC:TabIndex = 1.
      /*  */
      /* p4KoderMenuSC */
      /*  */
      THIS-OBJECT:p4KoderMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p4KoderMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p4KoderMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4KoderMenuSC:Name = "p4KoderMenuSC".
      THIS-OBJECT:p4KoderMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p4KoderMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:p4KoderMenuSC:Panel1:Controls:Add(THIS-OBJECT:p4KodertoolStrip).
      /*  */
      /* p4KoderMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:p4KoderMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridP4).
      THIS-OBJECT:p4KoderMenuSC:Size = NEW System.Drawing.Size(1145, 230).
      THIS-OBJECT:p4KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p4KoderMenuSC:TabIndex = 1.
      /*  */
      /* p4KodertoolStrip */
      /*  */
      THIS-OBJECT:p4KodertoolStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:laggTillP4Button.
      arrayvar4[2] = THIS-OBJECT:taBortP4Button.
      THIS-OBJECT:p4KodertoolStrip:Items:AddRange(arrayvar4).
      THIS-OBJECT:p4KodertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p4KodertoolStrip:Name = "p4KodertoolStrip".
      THIS-OBJECT:p4KodertoolStrip:Size = NEW System.Drawing.Size(1145, 25).
      THIS-OBJECT:p4KodertoolStrip:TabIndex = 0.
      THIS-OBJECT:p4KodertoolStrip:Text = "toolStrip2".
      /*  */
      /* laggTillP4Button */
      /*  */
      THIS-OBJECT:laggTillP4Button:Image = CAST(resources:GetObject("laggTillP4Button.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP4Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP4Button:Name = "laggTillP4Button".
      THIS-OBJECT:laggTillP4Button:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:laggTillP4Button:Text = "Lägg till P4-Kod".
      THIS-OBJECT:laggTillP4Button:Click:Subscribe(THIS-OBJECT:laggTillP4Button_Click).
      /*  */
      /* taBortP4Button */
      /*  */
      THIS-OBJECT:taBortP4Button:Image = CAST(resources:GetObject("taBortP4Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP4Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP4Button:Name = "taBortP4Button".
      THIS-OBJECT:taBortP4Button:Size = NEW System.Drawing.Size(106, 22).
      THIS-OBJECT:taBortP4Button:Text = "Ta bort P4-kod".
      THIS-OBJECT:taBortP4Button:ToolTipText = "Tar bort markerad P4-Kod".
      THIS-OBJECT:taBortP4Button:Click:Subscribe(THIS-OBJECT:taBortP4Button_Click).
      /*  */
      /* gridP4 */
      /*  */
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4:DisplayLayout:Appearance = appearance89.
      THIS-OBJECT:gridP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance90:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance90:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:Appearance = appearance90.
      appearance91:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance91.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance92:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance92:BackColor2 = System.Drawing.SystemColors:Control.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance92:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:PromptAppearance = appearance92.
      THIS-OBJECT:gridP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      appearance93:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveCellAppearance = appearance93.
      appearance94:BackColor = System.Drawing.SystemColors:Highlight.
      appearance94:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveRowAppearance = appearance94.
      THIS-OBJECT:gridP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CardAreaAppearance = appearance95.
      appearance96:BorderColor = System.Drawing.Color:Silver.
      appearance96:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellAppearance = appearance96.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellPadding = 0.
      appearance97:BackColor = System.Drawing.SystemColors:Control.
      appearance97:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance97:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance97:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance97:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:Override:GroupByRowAppearance = appearance97.
      appearance98:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderAppearance = appearance98.
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4:DisplayLayout:Override:RowAppearance = appearance99.
      THIS-OBJECT:gridP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance100.
      THIS-OBJECT:gridP4:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP4:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP4:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP4:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP4:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP4:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP4:GuruContext = ?.
      THIS-OBJECT:gridP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP4:Name = "gridP4".
      THIS-OBJECT:gridP4:Size = NEW System.Drawing.Size(1145, 201).
      THIS-OBJECT:gridP4:TabIndex = 0.
      THIS-OBJECT:gridP4:Text = "grid1".
      THIS-OBJECT:gridP4:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp4_AfterCellUpdate).
      THIS-OBJECT:gridP4:CellChange:Subscribe(THIS-OBJECT:gridP4_CellChange).
      THIS-OBJECT:gridP4:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp4_BeforeCellUpdate).
      /*  */
      /* TabManagerP4 */
      /*  */
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP4).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Resurs).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Frekvens).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Import).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Lista).
      THIS-OBJECT:TabManagerP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP4:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP4:Name = "TabManagerP4".
      THIS-OBJECT:TabManagerP4:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP4.
      THIS-OBJECT:TabManagerP4:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP4:TabIndex = 3.
      appearance101:Image = CAST(resources:GetObject("appearance101.Image"), System.Object).
      ultraTab12:Appearance = appearance101.
      ultraTab12:Key = "P4Resurs".
      ultraTab12:TabPage = THIS-OBJECT:TabP4Resurs.
      ultraTab12:Text = "P4-Resurser".
      appearance102:Image = CAST(resources:GetObject("appearance102.Image"), System.Object).
      ultraTab15:Appearance = appearance102.
      ultraTab15:Key = "P4Frekvens".
      ultraTab15:TabPage = THIS-OBJECT:TabP4Frekvens.
      ultraTab15:Text = "P4-Frekvenser".
      appearance103:Image = CAST(resources:GetObject("appearance103.Image"), System.Object).
      ultraTab2:Appearance = appearance103.
      ultraTab2:Key = "P4Listor".
      ultraTab2:TabPage = THIS-OBJECT:TabP4Lista.
      ultraTab2:Text = "P4-Listor".
      appearance104:Image = CAST(resources:GetObject("appearance104.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance104.
      appearance105:Image = CAST(resources:GetObject("appearance105.Image"), System.Object).
      ultraTab1:Appearance = appearance105.
      ultraTab1:Key = "Import".
      ultraTab1:TabPage = THIS-OBJECT:TabP4Import.
      ultraTab1:Text = "Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
      arrayvar5[1] = ultraTab12.
      arrayvar5[2] = ultraTab15.
      arrayvar5[3] = ultraTab2.
      arrayvar5[4] = ultraTab1.
      THIS-OBJECT:TabManagerP4:Tabs:AddRange(arrayvar5).
      THIS-OBJECT:TabManagerP4:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP4_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP4 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP4:Name = "ultraTabSharedControlsPageP4".
      THIS-OBJECT:ultraTabSharedControlsPageP4:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* EkgP4Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p4MainSC).
      THIS-OBJECT:Name = "EkgP4Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabP4Resurs:ResumeLayout(FALSE).
      THIS-OBJECT:p4ResursSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4ResursSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4ResursSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4ResursSC:ResumeLayout(FALSE).
      THIS-OBJECT:p4ResursSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4ResursSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4ResursSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4ResursSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursForP4, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP4Resurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP4Frekvens:ResumeLayout(FALSE).
      THIS-OBJECT:p4FrekvensSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4FrekvensSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4FrekvensSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4FrekvensSC:ResumeLayout(FALSE).
      THIS-OBJECT:p4FrekvensSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4FrekvensSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4FrekvensSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4FrekvensSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridP5ForP4, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP4Frekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP4Lista:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP4ListaGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP4ListaGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP4ListaGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP4ListaGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP4ListaGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P4ListaStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P4ListaStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP4Lista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP4Import:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP4ImportGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP4ImportGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP4ImportGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP4ImportGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP4ImportGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P4ImportStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P4ImportStrip:PerformLayout().
      THIS-OBJECT:splitContainerP4Import:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP4Import, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP4Import:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridImportP4F, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP4, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4MainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4MainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4MainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4MainSC:ResumeLayout(FALSE).
      THIS-OBJECT:p4KoderMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p4KoderMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:p4KoderMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p4KoderMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p4KoderMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:p4KodertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p4KodertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP4, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerP4, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerP4:ResumeLayout(FALSE).
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
   METHOD PRIVATE VOID P4ListaToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:gridP4Lista:VISIBLE = TRUE.      
      THIS-OBJECT:ekgp1detaljTTh:FIND-FIRST() NO-ERROR.
      IF NOT THIS-OBJECT:ekgp1detaljTTh:AVAILABLE THEN DO: 
         THIS-OBJECT:ControlWinLg:HamtP1detaljlista("4").
      END.              
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP4Frekvens_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP4Frekvens().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP4FrekvensBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP4FrekvensBort().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP4Resurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP4Resurs().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP4ResursBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP4ResursBort().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes: 
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp4_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:gridP4:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      IF THIS-OBJECT:ControlWinLg:P4Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO:                
         THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P4"). 
      END.   
      ELSE THIS-OBJECT:gridP4:GuruReopen().         
         
      RETURN.
   END METHOD.


   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp4_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
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
   METHOD PRIVATE VOID gridP4_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridP4:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "FREKVENS" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE". 
         THIS-OBJECT:gridP4:GuruCurrentRowid(ekgp4tth:ROWID). /*Fix*/
         IF THIS-OBJECT:ControlWinLg:P4Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO: 
            THIS-OBJECT:gridP4:GuruGetCurrentRowid(). /*Fix*/
            THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P4").
         END.   
         ELSE THIS-OBJECT:gridP4:GuruReopen().
      END. 
      RETURN.
   END METHOD.
   

    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp4resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ControlWinLg:P4ResursUppdate(INPUT kolnamn, INPUT kolvarde).                    
      RETURN.
   END METHOD.

 

  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.   
   METHOD PRIVATE VOID gridp4frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P4FrekvensUppdate(INPUT kolnamn, INPUT kolvarde).               
      RETURN.
   END METHOD.

   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP4Frekvens_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP5ForP4 THEN DO:
         THIS-OBJECT:buttonOverP4Frekvens_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP5ForP4_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP4Frekvens THEN DO:
         THIS-OBJECT:buttonOverP4FrekvensBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP4Resurs_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursForP4 THEN DO:
         THIS-OBJECT:buttonOverP4Resurs_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridResursForP4_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP4Resurs THEN DO:
         THIS-OBJECT:buttonOverP4ResursBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP4Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP4Pop:Show().
      THIS-OBJECT:ControlShell:fillinP4arbkod:GuruText:Select().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p4FrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP4F:VISIBLE = TRUE.
      THIS-OBJECT:GridImportP4:VISIBLE = FALSE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P4F").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID p4KoderToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GridImportP4F:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP4:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P4").
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP4Rbort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("P4").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP4Rin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P4").
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP4_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(THIS-OBJECT:TabManagerP4:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP4:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
   
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP4Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP4().
      RETURN.

   END METHOD.
   
   
END CLASS.