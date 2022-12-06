 
 /*------------------------------------------------------------------------
    File        : EkgShell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jul 01 11:11:11 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgShell INHERITS UserControl: 
	
	DEFINE PUBLIC VARIABLE EkgExport AS Modules.EkgData.EkgExport NO-UNDO.
    
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyNiva AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyMtrl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyPaslag AS Controls.Button NO-UNDO.
   
   DEFINE PRIVATE VARIABLE buttonNyPaslag AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyResurspris AS System.Windows.Forms.ToolStripButton NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE buttonCancelNyP1arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP2 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP1 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP3arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP2arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP4 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP3 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyResurs AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelNyP5 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP1arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP2 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP1 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP3arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP2arbkod AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP4 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP3 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyP5 AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonNyResurs AS Controls.Button NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP1frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP1FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverp2Mtrl AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP2MtrlBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP2resursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP2Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP2FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP2Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP3FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP3Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP3ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP3Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP5ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP5Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP4ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP4Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP4FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverP4Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverResurspaslagBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC  VARIABLE buttonOverResurspaslag AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverResurspris AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonNyNiva AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonTaBortResurs AS Controls.Button NO-UNDO.
   DEFINE PUBLIC VARIABLE comboNiva AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC  VARIABLE comboP1ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE comboP2ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC  VARIABLE comboP3ArbKod AS Controls.ComboSimpleLabel NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   
   DEFINE PUBLIC VARIABLE fillinArtNr AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP2arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP1arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP2LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP1LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP3arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP3LopNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP4arbkod AS Controls.FillinChar NO-UNDO.   
   DEFINE PUBLIC VARIABLE fillinPaslagBenamning AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinResursBenamning AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinResursNr AS Controls.FillinInteger NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinP5arbkod AS Controls.FillinChar NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP1F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP2F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP3F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP4F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridKataloger AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridMateriel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridMtrlForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2forP1 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Mtrl AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3ForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4ForP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5ForP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridRegel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForResurspris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursniva AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurspris AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogerValjButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogKatalogSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE KopieraKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillNivaButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP2arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTIllP1arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP3arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP3Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP5Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP4Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE miniToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlImportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlExportButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE mtrlMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE mtrlMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyMtrlPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyMtrlPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyNivaPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyNivaPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP1arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP1arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP1Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP2arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP2arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP2Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP1Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP3arbkodPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP3arbkodPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP3Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP2Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP4Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP3Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP4Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP5Pop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyP5Panel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyPaslagPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyPaslagPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE nyResursPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE nyResursPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p1arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1frekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P1ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p1KoderToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p1kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p2arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P2ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p2KoderToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p2kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2materielSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2materielSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p3FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P3ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p3kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3KodToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p3ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p4FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE P4ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p4KodToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p4ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p5KoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE P5ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE p1KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p5MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p5KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p4MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p4KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p4KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p3KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p2MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p2KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p2KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p5ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p5ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE paslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE paslagToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE regelMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE regeltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE resurserMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resurserMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ResursertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE resurspaslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resursPaslagtoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE resurspristoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
	/*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlWinLg                 AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlDataDb                AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   /*Drag Dropp*/
   
   DEFINE PUBLIC VARIABLE splitContainerBasresurser AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP1ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP1Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP2ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP2Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP3ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP3Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP4ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP4Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNiva AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP5ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP5Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResursPrisLista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResursPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPaslagOver AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPaslag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerTotalExport AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP1Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP2Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP2Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP2Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP3Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP3Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP3Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP4Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP4Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP4Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP5Import AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP5Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP5Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE TabDelad AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE TabExport AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerEkg AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP2 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP4 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP3 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP5 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManagerResurspriser AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMtrl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortMtrlButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortNiva AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP3arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP2arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP2Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP3Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP4Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP5Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortPaslagButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortResursPrisButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2materiel AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP2frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP4Frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP5Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP5resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabPopups AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResurser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabResursPriser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPPaslag AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPNiva AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPBasresurser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPRegel AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabP5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripBasresurser AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripNiva AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP5 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP4 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPagePaslagNiv AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP3 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP2 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE uppdateraMtrlKostButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
	CONSTRUCTOR PUBLIC EkgShell ( INPUT r AS Guru.Root ):
		
		
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      
           
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   
     /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyMtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinArtNr:GuruText:Text = "".
      THIS-OBJECT:nyMtrlPop:Close().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyNivaPop:Close().
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyPaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinPaslagBenamning:GuruText:Text = "".
      THIS-OBJECT:nyPaslagPop:Close().
      RETURN.

   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP3arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP3arbkodPop:Close().
      RETURN.

   END METHOD.
  
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP2arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP2arbkodPop:Close().
      RETURN.

   END METHOD.
   
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP1arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP1arbkodPop:Close().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP1LopNr:GuruText:Text = "".
      THIS-OBJECT:nyP1Pop:Close().
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP2LopNr:GuruText:Text = "".
      THIS-OBJECT:nyP2Pop:Close().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyP3_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinP3LopNr:GuruText:Text = "".
		THIS-OBJECT:nyP3Pop:Close().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP4_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:fillinP4arbkod:GuruText:Text = "".
      THIS-OBJECT:nyP4Pop:Close().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyP5_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinP5arbkod:GuruText:Text = "".
		THIS-OBJECT:nyP5Pop:Close().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonCancelNyResurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:fillinResursBenamning:GuruText:Text = "".
	   THIS-OBJECT:fillinResursNr:GuruText:Text = "".
		THIS-OBJECT:nyResursPop:Close().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyMtrl_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyMtrl().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:Nyniva().
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
   Purpose:
   Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyPaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyPaslag().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP1().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP1ArbKod().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP2_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP2().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyP2ArbKod().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyP3_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyP3().
		RETURN.

	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP3ArbKod().
      RETURN.

   END METHOD.
   /*-----------
   -------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyP4_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyP4().
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyP5_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyP5().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonNyResurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:NyResurs().
		RETURN.

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
   METHOD PRIVATE VOID buttonOverP3Frekvens_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP3Frekvens().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP3FrekvensBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP3FrekvensBort().
      RETURN.

   END METHOD.
    	

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverP3Resurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:buttonOverP3Resurs().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverP3ResursBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:buttonOverP3ResursBort().
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
   METHOD PRIVATE VOID buttonOverP5Resurs_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP5Resurs().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverP5ResursBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverP5ResursBort().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResurspaslagBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverResurspaslagBort(). 
      RETURN.

   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverResurspaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:buttonOverResurspaslag().
		RETURN.
	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridKataloger_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde). 
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridKataloger_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
		RETURN.
	END METHOD.
	
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "LASTA" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE". 
         THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde).
      END. 
      IF kolnamn = "ANVANDS" THEN DO:
         IF beforekolvarde = "TRUE" THEN DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOff(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
         ELSE DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOn(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
      END. 
      RETURN.
   END METHOD.
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.           
      RETURN.
   END METHOD.


/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridregel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:RegelUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridpaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:PaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN. 
         
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresursniva_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursNivaUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.         
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursPaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursprisUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridmateriel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridp5_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:P5Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN. 
      ELSE THIS-OBJECT:gridP5:GuruReopen().    
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp5_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridp3arbkod_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P3ArbkodUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
      ELSE STRING(e:Cell:VALUE) =  beforekolvarde.      
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp3arbkod_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
      
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp3_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:gridP3:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      IF THIS-OBJECT:ControlWinLg:P3Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO:      
         THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P3").            
      END.
      ELSE THIS-OBJECT:gridP3:GuruReopen().         
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp3_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP3_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridP3:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "FREKVENS" THEN DO:
         IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:gridP3:GuruCurrentRowid(ekgp3tth:ROWID). /*Fix*/
         IF THIS-OBJECT:ControlWinLg:P3Uppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN DO: 
            THIS-OBJECT:gridP3:GuruGetCurrentRowid(). /*Fix*/     
            THIS-OBJECT:ControlWinLg:FrekvensResursKoll("P3").
         END.
         ELSE THIS-OBJECT:gridP3:GuruReopen().
      END. 
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
      ELSE STRING(e:Cell:VALUE) =  beforekolvarde.      
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
         THIS-OBJECT:gridP2:GuruCurrentRowid(ekgp2tth:ROWID). /*Fix*/
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
   METHOD PRIVATE VOID gridp1arbkod_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P1ArbkodUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
      ELSE STRING(e:Cell:VALUE) =  beforekolvarde.      
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
      ELSE STRING(e:Cell:VALUE) =  beforekolvarde.      
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
   METHOD PRIVATE VOID gridp5resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().    
      THIS-OBJECT:ControlWinLg:P5ResursUppdate(INPUT kolnamn, INPUT kolvarde).      
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
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridp3resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P3ResursUppdate(INPUT kolnamn, INPUT kolvarde).               
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
      DEBUGGER:SET-BREAK().  
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
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridp4frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P4FrekvensUppdate(INPUT kolnamn, INPUT kolvarde).               
      RETURN.
   END METHOD.

/*  ------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.   
   METHOD PRIVATE VOID gridp3frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P3FrekvensUppdate(INPUT kolnamn, INPUT kolvarde).           
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
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridp1frekvens_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
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
   METHOD PRIVATE VOID gridKataloger_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).
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
   METHOD PRIVATE VOID gridP3Frekvens_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP4ForP3 THEN DO:
         THIS-OBJECT:buttonOverP3Frekvens_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP4ForP3_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP3Frekvens THEN DO:
         THIS-OBJECT:buttonOverP3FrekvensBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridP3Resurs_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursForP3 THEN DO:
         THIS-OBJECT:buttonOverP3Resurs_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridResursForP3_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP3Resurs THEN DO:
         THIS-OBJECT:buttonOverP4ResursBort_Click(?,?).
      END.
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
   METHOD PRIVATE VOID gridP5Resurs_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursForP5 THEN DO:
         THIS-OBJECT:buttonOverP5Resurs_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridResursForP5_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridP5Resurs THEN DO:
         THIS-OBJECT:buttonOverP5ResursBort_Click(?,?).
      END.
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		{AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslagBort_Click(?,?).
      END.
		RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridResursPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		{AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslag_Click(?,?).
      END.
		RETURN.

	END METHOD.

   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell = THIS-OBJECT. 
      THIS-OBJECT:ControlWinLg = NEW Modules.EkgData.EkgWinLg(THIS-OBJECT:Root).
      
      THIS-OBJECT:ControlWinLg:Initialize().
      
      RETURN TRUE.
   END METHOD.
     
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KatalogerValjButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:gridKataloger:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).   
      RETURN.

   END METHOD.

/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KopieraKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:ControlWinLg:KopieraKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      RETURN.
 

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillNivaButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   
		THIS-OBJECT:nyNivaPop:Show().		

		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyPaslagPop:Show().
      THIS-OBJECT:fillinPaslagBenamning:GuruText:Select().
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillMtrlButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyMtrlPop:Show().
		THIS-OBJECT:fillinArtNr:GuruText:Select().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTIllP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyP1arbkodPop:Show().
		THIS-OBJECT:fillinP1ArbKod:GuruText:Select().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillP2arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyP2arbkodPop:Show().
		THIS-OBJECT:fillinP2ArbKod:GuruText:Select().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyP3arbkodPop:Show().
		THIS-OBJECT:fillinP3ArbKod:GuruText:Select().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:nyResursPop:Show().		
		THIS-OBJECT:fillinResursNr:GuruText:Select().
		/*THIS-OBJECT:fillinResursBenamning:GuruText:Select().*/
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillP5Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   THIS-OBJECT:nyP5Pop:Show().
	   THIS-OBJECT:fillinP5arbkod:GuruText:Select().
		RETURN.

	END METHOD.
	
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP4Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyP4Pop:Show().
      THIS-OBJECT:fillinP4arbkod:GuruText:Select().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP3Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyP3Pop:Show().
      THIS-OBJECT:fillinP3LopNr:GuruText:Select().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP2Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyP2Pop:Show().
      THIS-OBJECT:fillinP2LopNr:GuruText:Select().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP1Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:nyP1Pop:Show().
      THIS-OBJECT:fillinP1LopNr:GuruText:Select().
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
	METHOD PRIVATE VOID p2FrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GridImportP2F:VISIBLE = TRUE.
      THIS-OBJECT:GridImportP2:VISIBLE = FALSE.
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
      THIS-OBJECT:GridImportP2:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P2").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID p3FrekvensToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GridImportP3F:VISIBLE = TRUE.
      THIS-OBJECT:GridImportP3:VISIBLE = FALSE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P3F").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID p3KoderToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GridImportP3F:VISIBLE = FALSE.
      THIS-OBJECT:GridImportP3:VISIBLE = TRUE.
      THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P3").
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

   METHOD PUBLIC VOID StartEkgShell():
      
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgShell").
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
      DEFINE VARIABLE appearance151 AS Infragistics.Win.Appearance NO-UNDO.
      appearance151 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance152 AS Infragistics.Win.Appearance NO-UNDO.
      appearance152 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance153 AS Infragistics.Win.Appearance NO-UNDO.
      appearance153 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance154 AS Infragistics.Win.Appearance NO-UNDO.
      appearance154 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance155 AS Infragistics.Win.Appearance NO-UNDO.
      appearance155 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance156 AS Infragistics.Win.Appearance NO-UNDO.
      appearance156 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance157 AS Infragistics.Win.Appearance NO-UNDO.
      appearance157 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance158 AS Infragistics.Win.Appearance NO-UNDO.
      appearance158 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance159 AS Infragistics.Win.Appearance NO-UNDO.
      appearance159 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance160 AS Infragistics.Win.Appearance NO-UNDO.
      appearance160 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance161 AS Infragistics.Win.Appearance NO-UNDO.
      appearance161 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance162 AS Infragistics.Win.Appearance NO-UNDO.
      appearance162 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance163 AS Infragistics.Win.Appearance NO-UNDO.
      appearance163 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance164 AS Infragistics.Win.Appearance NO-UNDO.
      appearance164 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance165 AS Infragistics.Win.Appearance NO-UNDO.
      appearance165 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance166 AS Infragistics.Win.Appearance NO-UNDO.
      appearance166 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance167 AS Infragistics.Win.Appearance NO-UNDO.
      appearance167 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance168 AS Infragistics.Win.Appearance NO-UNDO.
      appearance168 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance169 AS Infragistics.Win.Appearance NO-UNDO.
      appearance169 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance170 AS Infragistics.Win.Appearance NO-UNDO.
      appearance170 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance171 AS Infragistics.Win.Appearance NO-UNDO.
      appearance171 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance172 AS Infragistics.Win.Appearance NO-UNDO.
      appearance172 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance173 AS Infragistics.Win.Appearance NO-UNDO.
      appearance173 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance174 AS Infragistics.Win.Appearance NO-UNDO.
      appearance174 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance175 AS Infragistics.Win.Appearance NO-UNDO.
      appearance175 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance176 AS Infragistics.Win.Appearance NO-UNDO.
      appearance176 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance177 AS Infragistics.Win.Appearance NO-UNDO.
      appearance177 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance178 AS Infragistics.Win.Appearance NO-UNDO.
      appearance178 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance179 AS Infragistics.Win.Appearance NO-UNDO.
      appearance179 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance180 AS Infragistics.Win.Appearance NO-UNDO.
      appearance180 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance181 AS Infragistics.Win.Appearance NO-UNDO.
      appearance181 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance182 AS Infragistics.Win.Appearance NO-UNDO.
      appearance182 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance183 AS Infragistics.Win.Appearance NO-UNDO.
      appearance183 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance184 AS Infragistics.Win.Appearance NO-UNDO.
      appearance184 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance185 AS Infragistics.Win.Appearance NO-UNDO.
      appearance185 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance186 AS Infragistics.Win.Appearance NO-UNDO.
      appearance186 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance187 AS Infragistics.Win.Appearance NO-UNDO.
      appearance187 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance188 AS Infragistics.Win.Appearance NO-UNDO.
      appearance188 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance189 AS Infragistics.Win.Appearance NO-UNDO.
      appearance189 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance190 AS Infragistics.Win.Appearance NO-UNDO.
      appearance190 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance191 AS Infragistics.Win.Appearance NO-UNDO.
      appearance191 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance192 AS Infragistics.Win.Appearance NO-UNDO.
      appearance192 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance193 AS Infragistics.Win.Appearance NO-UNDO.
      appearance193 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance194 AS Infragistics.Win.Appearance NO-UNDO.
      appearance194 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance195 AS Infragistics.Win.Appearance NO-UNDO.
      appearance195 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance196 AS Infragistics.Win.Appearance NO-UNDO.
      appearance196 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance197 AS Infragistics.Win.Appearance NO-UNDO.
      appearance197 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance198 AS Infragistics.Win.Appearance NO-UNDO.
      appearance198 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance199 AS Infragistics.Win.Appearance NO-UNDO.
      appearance199 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance200 AS Infragistics.Win.Appearance NO-UNDO.
      appearance200 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance201 AS Infragistics.Win.Appearance NO-UNDO.
      appearance201 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance202 AS Infragistics.Win.Appearance NO-UNDO.
      appearance202 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance203 AS Infragistics.Win.Appearance NO-UNDO.
      appearance203 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance204 AS Infragistics.Win.Appearance NO-UNDO.
      appearance204 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance205 AS Infragistics.Win.Appearance NO-UNDO.
      appearance205 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance206 AS Infragistics.Win.Appearance NO-UNDO.
      appearance206 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance207 AS Infragistics.Win.Appearance NO-UNDO.
      appearance207 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance208 AS Infragistics.Win.Appearance NO-UNDO.
      appearance208 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance209 AS Infragistics.Win.Appearance NO-UNDO.
      appearance209 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance210 AS Infragistics.Win.Appearance NO-UNDO.
      appearance210 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance211 AS Infragistics.Win.Appearance NO-UNDO.
      appearance211 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance212 AS Infragistics.Win.Appearance NO-UNDO.
      appearance212 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance213 AS Infragistics.Win.Appearance NO-UNDO.
      appearance213 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance214 AS Infragistics.Win.Appearance NO-UNDO.
      appearance214 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance215 AS Infragistics.Win.Appearance NO-UNDO.
      appearance215 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance216 AS Infragistics.Win.Appearance NO-UNDO.
      appearance216 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance217 AS Infragistics.Win.Appearance NO-UNDO.
      appearance217 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance218 AS Infragistics.Win.Appearance NO-UNDO.
      appearance218 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance219 AS Infragistics.Win.Appearance NO-UNDO.
      appearance219 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance220 AS Infragistics.Win.Appearance NO-UNDO.
      appearance220 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance221 AS Infragistics.Win.Appearance NO-UNDO.
      appearance221 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance222 AS Infragistics.Win.Appearance NO-UNDO.
      appearance222 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance223 AS Infragistics.Win.Appearance NO-UNDO.
      appearance223 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance224 AS Infragistics.Win.Appearance NO-UNDO.
      appearance224 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance225 AS Infragistics.Win.Appearance NO-UNDO.
      appearance225 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance226 AS Infragistics.Win.Appearance NO-UNDO.
      appearance226 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance227 AS Infragistics.Win.Appearance NO-UNDO.
      appearance227 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance228 AS Infragistics.Win.Appearance NO-UNDO.
      appearance228 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance229 AS Infragistics.Win.Appearance NO-UNDO.
      appearance229 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance230 AS Infragistics.Win.Appearance NO-UNDO.
      appearance230 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance231 AS Infragistics.Win.Appearance NO-UNDO.
      appearance231 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance232 AS Infragistics.Win.Appearance NO-UNDO.
      appearance232 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance233 AS Infragistics.Win.Appearance NO-UNDO.
      appearance233 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance234 AS Infragistics.Win.Appearance NO-UNDO.
      appearance234 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance235 AS Infragistics.Win.Appearance NO-UNDO.
      appearance235 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance236 AS Infragistics.Win.Appearance NO-UNDO.
      appearance236 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance237 AS Infragistics.Win.Appearance NO-UNDO.
      appearance237 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance238 AS Infragistics.Win.Appearance NO-UNDO.
      appearance238 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance239 AS Infragistics.Win.Appearance NO-UNDO.
      appearance239 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance240 AS Infragistics.Win.Appearance NO-UNDO.
      appearance240 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance241 AS Infragistics.Win.Appearance NO-UNDO.
      appearance241 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance242 AS Infragistics.Win.Appearance NO-UNDO.
      appearance242 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance243 AS Infragistics.Win.Appearance NO-UNDO.
      appearance243 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance244 AS Infragistics.Win.Appearance NO-UNDO.
      appearance244 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance245 AS Infragistics.Win.Appearance NO-UNDO.
      appearance245 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance246 AS Infragistics.Win.Appearance NO-UNDO.
      appearance246 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance247 AS Infragistics.Win.Appearance NO-UNDO.
      appearance247 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance248 AS Infragistics.Win.Appearance NO-UNDO.
      appearance248 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance249 AS Infragistics.Win.Appearance NO-UNDO.
      appearance249 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance250 AS Infragistics.Win.Appearance NO-UNDO.
      appearance250 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance251 AS Infragistics.Win.Appearance NO-UNDO.
      appearance251 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance252 AS Infragistics.Win.Appearance NO-UNDO.
      appearance252 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance253 AS Infragistics.Win.Appearance NO-UNDO.
      appearance253 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance254 AS Infragistics.Win.Appearance NO-UNDO.
      appearance254 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance255 AS Infragistics.Win.Appearance NO-UNDO.
      appearance255 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance256 AS Infragistics.Win.Appearance NO-UNDO.
      appearance256 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance257 AS Infragistics.Win.Appearance NO-UNDO.
      appearance257 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance258 AS Infragistics.Win.Appearance NO-UNDO.
      appearance258 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance259 AS Infragistics.Win.Appearance NO-UNDO.
      appearance259 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance260 AS Infragistics.Win.Appearance NO-UNDO.
      appearance260 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance261 AS Infragistics.Win.Appearance NO-UNDO.
      appearance261 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance262 AS Infragistics.Win.Appearance NO-UNDO.
      appearance262 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance263 AS Infragistics.Win.Appearance NO-UNDO.
      appearance263 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance264 AS Infragistics.Win.Appearance NO-UNDO.
      appearance264 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance265 AS Infragistics.Win.Appearance NO-UNDO.
      appearance265 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance266 AS Infragistics.Win.Appearance NO-UNDO.
      appearance266 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance267 AS Infragistics.Win.Appearance NO-UNDO.
      appearance267 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance268 AS Infragistics.Win.Appearance NO-UNDO.
      appearance268 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance269 AS Infragistics.Win.Appearance NO-UNDO.
      appearance269 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance270 AS Infragistics.Win.Appearance NO-UNDO.
      appearance270 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance271 AS Infragistics.Win.Appearance NO-UNDO.
      appearance271 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance272 AS Infragistics.Win.Appearance NO-UNDO.
      appearance272 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance273 AS Infragistics.Win.Appearance NO-UNDO.
      appearance273 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance274 AS Infragistics.Win.Appearance NO-UNDO.
      appearance274 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance275 AS Infragistics.Win.Appearance NO-UNDO.
      appearance275 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance276 AS Infragistics.Win.Appearance NO-UNDO.
      appearance276 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance277 AS Infragistics.Win.Appearance NO-UNDO.
      appearance277 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance278 AS Infragistics.Win.Appearance NO-UNDO.
      appearance278 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance279 AS Infragistics.Win.Appearance NO-UNDO.
      appearance279 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance280 AS Infragistics.Win.Appearance NO-UNDO.
      appearance280 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance281 AS Infragistics.Win.Appearance NO-UNDO.
      appearance281 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance282 AS Infragistics.Win.Appearance NO-UNDO.
      appearance282 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance283 AS Infragistics.Win.Appearance NO-UNDO.
      appearance283 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance284 AS Infragistics.Win.Appearance NO-UNDO.
      appearance284 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance285 AS Infragistics.Win.Appearance NO-UNDO.
      appearance285 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance286 AS Infragistics.Win.Appearance NO-UNDO.
      appearance286 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance287 AS Infragistics.Win.Appearance NO-UNDO.
      appearance287 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance288 AS Infragistics.Win.Appearance NO-UNDO.
      appearance288 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance289 AS Infragistics.Win.Appearance NO-UNDO.
      appearance289 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance290 AS Infragistics.Win.Appearance NO-UNDO.
      appearance290 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance291 AS Infragistics.Win.Appearance NO-UNDO.
      appearance291 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance292 AS Infragistics.Win.Appearance NO-UNDO.
      appearance292 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance293 AS Infragistics.Win.Appearance NO-UNDO.
      appearance293 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance294 AS Infragistics.Win.Appearance NO-UNDO.
      appearance294 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance295 AS Infragistics.Win.Appearance NO-UNDO.
      appearance295 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance296 AS Infragistics.Win.Appearance NO-UNDO.
      appearance296 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance297 AS Infragistics.Win.Appearance NO-UNDO.
      appearance297 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance298 AS Infragistics.Win.Appearance NO-UNDO.
      appearance298 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance299 AS Infragistics.Win.Appearance NO-UNDO.
      appearance299 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance300 AS Infragistics.Win.Appearance NO-UNDO.
      appearance300 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance301 AS Infragistics.Win.Appearance NO-UNDO.
      appearance301 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance302 AS Infragistics.Win.Appearance NO-UNDO.
      appearance302 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance303 AS Infragistics.Win.Appearance NO-UNDO.
      appearance303 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance304 AS Infragistics.Win.Appearance NO-UNDO.
      appearance304 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance305 AS Infragistics.Win.Appearance NO-UNDO.
      appearance305 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance306 AS Infragistics.Win.Appearance NO-UNDO.
      appearance306 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance307 AS Infragistics.Win.Appearance NO-UNDO.
      appearance307 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance308 AS Infragistics.Win.Appearance NO-UNDO.
      appearance308 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance309 AS Infragistics.Win.Appearance NO-UNDO.
      appearance309 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance310 AS Infragistics.Win.Appearance NO-UNDO.
      appearance310 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance311 AS Infragistics.Win.Appearance NO-UNDO.
      appearance311 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance312 AS Infragistics.Win.Appearance NO-UNDO.
      appearance312 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance313 AS Infragistics.Win.Appearance NO-UNDO.
      appearance313 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance314 AS Infragistics.Win.Appearance NO-UNDO.
      appearance314 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance315 AS Infragistics.Win.Appearance NO-UNDO.
      appearance315 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance316 AS Infragistics.Win.Appearance NO-UNDO.
      appearance316 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance317 AS Infragistics.Win.Appearance NO-UNDO.
      appearance317 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance318 AS Infragistics.Win.Appearance NO-UNDO.
      appearance318 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance319 AS Infragistics.Win.Appearance NO-UNDO.
      appearance319 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance320 AS Infragistics.Win.Appearance NO-UNDO.
      appearance320 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance321 AS Infragistics.Win.Appearance NO-UNDO.
      appearance321 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance322 AS Infragistics.Win.Appearance NO-UNDO.
      appearance322 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance323 AS Infragistics.Win.Appearance NO-UNDO.
      appearance323 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance324 AS Infragistics.Win.Appearance NO-UNDO.
      appearance324 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance325 AS Infragistics.Win.Appearance NO-UNDO.
      appearance325 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance326 AS Infragistics.Win.Appearance NO-UNDO.
      appearance326 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance327 AS Infragistics.Win.Appearance NO-UNDO.
      appearance327 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance328 AS Infragistics.Win.Appearance NO-UNDO.
      appearance328 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance329 AS Infragistics.Win.Appearance NO-UNDO.
      appearance329 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance330 AS Infragistics.Win.Appearance NO-UNDO.
      appearance330 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance331 AS Infragistics.Win.Appearance NO-UNDO.
      appearance331 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance332 AS Infragistics.Win.Appearance NO-UNDO.
      appearance332 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance333 AS Infragistics.Win.Appearance NO-UNDO.
      appearance333 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance334 AS Infragistics.Win.Appearance NO-UNDO.
      appearance334 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance335 AS Infragistics.Win.Appearance NO-UNDO.
      appearance335 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance336 AS Infragistics.Win.Appearance NO-UNDO.
      appearance336 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance337 AS Infragistics.Win.Appearance NO-UNDO.
      appearance337 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance338 AS Infragistics.Win.Appearance NO-UNDO.
      appearance338 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance339 AS Infragistics.Win.Appearance NO-UNDO.
      appearance339 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance340 AS Infragistics.Win.Appearance NO-UNDO.
      appearance340 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance341 AS Infragistics.Win.Appearance NO-UNDO.
      appearance341 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance342 AS Infragistics.Win.Appearance NO-UNDO.
      appearance342 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance343 AS Infragistics.Win.Appearance NO-UNDO.
      appearance343 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance344 AS Infragistics.Win.Appearance NO-UNDO.
      appearance344 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance345 AS Infragistics.Win.Appearance NO-UNDO.
      appearance345 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance346 AS Infragistics.Win.Appearance NO-UNDO.
      appearance346 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance347 AS Infragistics.Win.Appearance NO-UNDO.
      appearance347 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance348 AS Infragistics.Win.Appearance NO-UNDO.
      appearance348 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance349 AS Infragistics.Win.Appearance NO-UNDO.
      appearance349 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance350 AS Infragistics.Win.Appearance NO-UNDO.
      appearance350 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance351 AS Infragistics.Win.Appearance NO-UNDO.
      appearance351 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance352 AS Infragistics.Win.Appearance NO-UNDO.
      appearance352 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance353 AS Infragistics.Win.Appearance NO-UNDO.
      appearance353 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance354 AS Infragistics.Win.Appearance NO-UNDO.
      appearance354 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance355 AS Infragistics.Win.Appearance NO-UNDO.
      appearance355 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance356 AS Infragistics.Win.Appearance NO-UNDO.
      appearance356 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance357 AS Infragistics.Win.Appearance NO-UNDO.
      appearance357 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance358 AS Infragistics.Win.Appearance NO-UNDO.
      appearance358 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance359 AS Infragistics.Win.Appearance NO-UNDO.
      appearance359 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance360 AS Infragistics.Win.Appearance NO-UNDO.
      appearance360 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance361 AS Infragistics.Win.Appearance NO-UNDO.
      appearance361 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance362 AS Infragistics.Win.Appearance NO-UNDO.
      appearance362 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance363 AS Infragistics.Win.Appearance NO-UNDO.
      appearance363 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance364 AS Infragistics.Win.Appearance NO-UNDO.
      appearance364 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance365 AS Infragistics.Win.Appearance NO-UNDO.
      appearance365 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance366 AS Infragistics.Win.Appearance NO-UNDO.
      appearance366 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance367 AS Infragistics.Win.Appearance NO-UNDO.
      appearance367 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance368 AS Infragistics.Win.Appearance NO-UNDO.
      appearance368 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance369 AS Infragistics.Win.Appearance NO-UNDO.
      appearance369 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance370 AS Infragistics.Win.Appearance NO-UNDO.
      appearance370 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance371 AS Infragistics.Win.Appearance NO-UNDO.
      appearance371 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance372 AS Infragistics.Win.Appearance NO-UNDO.
      appearance372 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance373 AS Infragistics.Win.Appearance NO-UNDO.
      appearance373 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance374 AS Infragistics.Win.Appearance NO-UNDO.
      appearance374 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance375 AS Infragistics.Win.Appearance NO-UNDO.
      appearance375 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance376 AS Infragistics.Win.Appearance NO-UNDO.
      appearance376 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance377 AS Infragistics.Win.Appearance NO-UNDO.
      appearance377 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance378 AS Infragistics.Win.Appearance NO-UNDO.
      appearance378 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance379 AS Infragistics.Win.Appearance NO-UNDO.
      appearance379 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance380 AS Infragistics.Win.Appearance NO-UNDO.
      appearance380 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance381 AS Infragistics.Win.Appearance NO-UNDO.
      appearance381 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance382 AS Infragistics.Win.Appearance NO-UNDO.
      appearance382 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance383 AS Infragistics.Win.Appearance NO-UNDO.
      appearance383 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance384 AS Infragistics.Win.Appearance NO-UNDO.
      appearance384 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance385 AS Infragistics.Win.Appearance NO-UNDO.
      appearance385 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance386 AS Infragistics.Win.Appearance NO-UNDO.
      appearance386 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance387 AS Infragistics.Win.Appearance NO-UNDO.
      appearance387 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance388 AS Infragistics.Win.Appearance NO-UNDO.
      appearance388 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance389 AS Infragistics.Win.Appearance NO-UNDO.
      appearance389 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance390 AS Infragistics.Win.Appearance NO-UNDO.
      appearance390 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance391 AS Infragistics.Win.Appearance NO-UNDO.
      appearance391 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance392 AS Infragistics.Win.Appearance NO-UNDO.
      appearance392 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance393 AS Infragistics.Win.Appearance NO-UNDO.
      appearance393 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance394 AS Infragistics.Win.Appearance NO-UNDO.
      appearance394 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance395 AS Infragistics.Win.Appearance NO-UNDO.
      appearance395 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance396 AS Infragistics.Win.Appearance NO-UNDO.
      appearance396 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance397 AS Infragistics.Win.Appearance NO-UNDO.
      appearance397 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance398 AS Infragistics.Win.Appearance NO-UNDO.
      appearance398 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance399 AS Infragistics.Win.Appearance NO-UNDO.
      appearance399 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance400 AS Infragistics.Win.Appearance NO-UNDO.
      appearance400 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance401 AS Infragistics.Win.Appearance NO-UNDO.
      appearance401 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance402 AS Infragistics.Win.Appearance NO-UNDO.
      appearance402 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance403 AS Infragistics.Win.Appearance NO-UNDO.
      appearance403 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance404 AS Infragistics.Win.Appearance NO-UNDO.
      appearance404 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance405 AS Infragistics.Win.Appearance NO-UNDO.
      appearance405 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance406 AS Infragistics.Win.Appearance NO-UNDO.
      appearance406 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance407 AS Infragistics.Win.Appearance NO-UNDO.
      appearance407 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance408 AS Infragistics.Win.Appearance NO-UNDO.
      appearance408 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance409 AS Infragistics.Win.Appearance NO-UNDO.
      appearance409 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance410 AS Infragistics.Win.Appearance NO-UNDO.
      appearance410 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance411 AS Infragistics.Win.Appearance NO-UNDO.
      appearance411 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance412 AS Infragistics.Win.Appearance NO-UNDO.
      appearance412 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance413 AS Infragistics.Win.Appearance NO-UNDO.
      appearance413 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance414 AS Infragistics.Win.Appearance NO-UNDO.
      appearance414 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance415 AS Infragistics.Win.Appearance NO-UNDO.
      appearance415 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance416 AS Infragistics.Win.Appearance NO-UNDO.
      appearance416 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance417 AS Infragistics.Win.Appearance NO-UNDO.
      appearance417 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance418 AS Infragistics.Win.Appearance NO-UNDO.
      appearance418 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance419 AS Infragistics.Win.Appearance NO-UNDO.
      appearance419 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance420 AS Infragistics.Win.Appearance NO-UNDO.
      appearance420 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance421 AS Infragistics.Win.Appearance NO-UNDO.
      appearance421 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance422 AS Infragistics.Win.Appearance NO-UNDO.
      appearance422 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance423 AS Infragistics.Win.Appearance NO-UNDO.
      appearance423 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance424 AS Infragistics.Win.Appearance NO-UNDO.
      appearance424 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance425 AS Infragistics.Win.Appearance NO-UNDO.
      appearance425 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance426 AS Infragistics.Win.Appearance NO-UNDO.
      appearance426 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance427 AS Infragistics.Win.Appearance NO-UNDO.
      appearance427 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance428 AS Infragistics.Win.Appearance NO-UNDO.
      appearance428 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance429 AS Infragistics.Win.Appearance NO-UNDO.
      appearance429 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance430 AS Infragistics.Win.Appearance NO-UNDO.
      appearance430 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance431 AS Infragistics.Win.Appearance NO-UNDO.
      appearance431 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance432 AS Infragistics.Win.Appearance NO-UNDO.
      appearance432 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance433 AS Infragistics.Win.Appearance NO-UNDO.
      appearance433 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance434 AS Infragistics.Win.Appearance NO-UNDO.
      appearance434 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance435 AS Infragistics.Win.Appearance NO-UNDO.
      appearance435 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance436 AS Infragistics.Win.Appearance NO-UNDO.
      appearance436 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance437 AS Infragistics.Win.Appearance NO-UNDO.
      appearance437 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance438 AS Infragistics.Win.Appearance NO-UNDO.
      appearance438 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance439 AS Infragistics.Win.Appearance NO-UNDO.
      appearance439 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance440 AS Infragistics.Win.Appearance NO-UNDO.
      appearance440 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab21 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab21 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab23 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab23 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab18 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab18 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance441 AS Infragistics.Win.Appearance NO-UNDO.
      appearance441 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance442 AS Infragistics.Win.Appearance NO-UNDO.
      appearance442 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance443 AS Infragistics.Win.Appearance NO-UNDO.
      appearance443 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance444 AS Infragistics.Win.Appearance NO-UNDO.
      appearance444 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance445 AS Infragistics.Win.Appearance NO-UNDO.
      appearance445 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance446 AS Infragistics.Win.Appearance NO-UNDO.
      appearance446 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance447 AS Infragistics.Win.Appearance NO-UNDO.
      appearance447 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance448 AS Infragistics.Win.Appearance NO-UNDO.
      appearance448 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance449 AS Infragistics.Win.Appearance NO-UNDO.
      appearance449 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance450 AS Infragistics.Win.Appearance NO-UNDO.
      appearance450 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance451 AS Infragistics.Win.Appearance NO-UNDO.
      appearance451 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance452 AS Infragistics.Win.Appearance NO-UNDO.
      appearance452 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance453 AS Infragistics.Win.Appearance NO-UNDO.
      appearance453 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance454 AS Infragistics.Win.Appearance NO-UNDO.
      appearance454 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance455 AS Infragistics.Win.Appearance NO-UNDO.
      appearance455 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance456 AS Infragistics.Win.Appearance NO-UNDO.
      appearance456 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance457 AS Infragistics.Win.Appearance NO-UNDO.
      appearance457 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance458 AS Infragistics.Win.Appearance NO-UNDO.
      appearance458 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance459 AS Infragistics.Win.Appearance NO-UNDO.
      appearance459 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance460 AS Infragistics.Win.Appearance NO-UNDO.
      appearance460 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance461 AS Infragistics.Win.Appearance NO-UNDO.
      appearance461 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance462 AS Infragistics.Win.Appearance NO-UNDO.
      appearance462 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance463 AS Infragistics.Win.Appearance NO-UNDO.
      appearance463 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance464 AS Infragistics.Win.Appearance NO-UNDO.
      appearance464 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab26 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab26 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance465 AS Infragistics.Win.Appearance NO-UNDO.
      appearance465 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab27 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab27 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance466 AS Infragistics.Win.Appearance NO-UNDO.
      appearance466 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance467 AS Infragistics.Win.Appearance NO-UNDO.
      appearance467 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance468 AS Infragistics.Win.Appearance NO-UNDO.
      appearance468 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance469 AS Infragistics.Win.Appearance NO-UNDO.
      appearance469 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance470 AS Infragistics.Win.Appearance NO-UNDO.
      appearance470 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance471 AS Infragistics.Win.Appearance NO-UNDO.
      appearance471 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance472 AS Infragistics.Win.Appearance NO-UNDO.
      appearance472 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance473 AS Infragistics.Win.Appearance NO-UNDO.
      appearance473 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance474 AS Infragistics.Win.Appearance NO-UNDO.
      appearance474 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance475 AS Infragistics.Win.Appearance NO-UNDO.
      appearance475 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance476 AS Infragistics.Win.Appearance NO-UNDO.
      appearance476 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance477 AS Infragistics.Win.Appearance NO-UNDO.
      appearance477 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance478 AS Infragistics.Win.Appearance NO-UNDO.
      appearance478 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance479 AS Infragistics.Win.Appearance NO-UNDO.
      appearance479 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab12 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab12 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance480 AS Infragistics.Win.Appearance NO-UNDO.
      appearance480 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance481 AS Infragistics.Win.Appearance NO-UNDO.
      appearance481 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance482 AS Infragistics.Win.Appearance NO-UNDO.
      appearance482 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance483 AS Infragistics.Win.Appearance NO-UNDO.
      appearance483 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance484 AS Infragistics.Win.Appearance NO-UNDO.
      appearance484 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance485 AS Infragistics.Win.Appearance NO-UNDO.
      appearance485 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance486 AS Infragistics.Win.Appearance NO-UNDO.
      appearance486 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance487 AS Infragistics.Win.Appearance NO-UNDO.
      appearance487 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance488 AS Infragistics.Win.Appearance NO-UNDO.
      appearance488 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance489 AS Infragistics.Win.Appearance NO-UNDO.
      appearance489 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance490 AS Infragistics.Win.Appearance NO-UNDO.
      appearance490 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance491 AS Infragistics.Win.Appearance NO-UNDO.
      appearance491 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance492 AS Infragistics.Win.Appearance NO-UNDO.
      appearance492 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance493 AS Infragistics.Win.Appearance NO-UNDO.
      appearance493 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance494 AS Infragistics.Win.Appearance NO-UNDO.
      appearance494 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance495 AS Infragistics.Win.Appearance NO-UNDO.
      appearance495 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance496 AS Infragistics.Win.Appearance NO-UNDO.
      appearance496 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance497 AS Infragistics.Win.Appearance NO-UNDO.
      appearance497 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance498 AS Infragistics.Win.Appearance NO-UNDO.
      appearance498 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance499 AS Infragistics.Win.Appearance NO-UNDO.
      appearance499 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance500 AS Infragistics.Win.Appearance NO-UNDO.
      appearance500 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance501 AS Infragistics.Win.Appearance NO-UNDO.
      appearance501 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance502 AS Infragistics.Win.Appearance NO-UNDO.
      appearance502 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance503 AS Infragistics.Win.Appearance NO-UNDO.
      appearance503 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance504 AS Infragistics.Win.Appearance NO-UNDO.
      appearance504 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance505 AS Infragistics.Win.Appearance NO-UNDO.
      appearance505 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance506 AS Infragistics.Win.Appearance NO-UNDO.
      appearance506 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance507 AS Infragistics.Win.Appearance NO-UNDO.
      appearance507 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab10 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab10 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance508 AS Infragistics.Win.Appearance NO-UNDO.
      appearance508 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab11 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab11 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance509 AS Infragistics.Win.Appearance NO-UNDO.
      appearance509 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance510 AS Infragistics.Win.Appearance NO-UNDO.
      appearance510 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance511 AS Infragistics.Win.Appearance NO-UNDO.
      appearance511 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance512 AS Infragistics.Win.Appearance NO-UNDO.
      appearance512 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance513 AS Infragistics.Win.Appearance NO-UNDO.
      appearance513 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance514 AS Infragistics.Win.Appearance NO-UNDO.
      appearance514 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance515 AS Infragistics.Win.Appearance NO-UNDO.
      appearance515 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance516 AS Infragistics.Win.Appearance NO-UNDO.
      appearance516 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance517 AS Infragistics.Win.Appearance NO-UNDO.
      appearance517 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance518 AS Infragistics.Win.Appearance NO-UNDO.
      appearance518 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance519 AS Infragistics.Win.Appearance NO-UNDO.
      appearance519 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance520 AS Infragistics.Win.Appearance NO-UNDO.
      appearance520 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance521 AS Infragistics.Win.Appearance NO-UNDO.
      appearance521 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance522 AS Infragistics.Win.Appearance NO-UNDO.
      appearance522 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance523 AS Infragistics.Win.Appearance NO-UNDO.
      appearance523 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance524 AS Infragistics.Win.Appearance NO-UNDO.
      appearance524 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance525 AS Infragistics.Win.Appearance NO-UNDO.
      appearance525 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance526 AS Infragistics.Win.Appearance NO-UNDO.
      appearance526 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance527 AS Infragistics.Win.Appearance NO-UNDO.
      appearance527 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance528 AS Infragistics.Win.Appearance NO-UNDO.
      appearance528 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance529 AS Infragistics.Win.Appearance NO-UNDO.
      appearance529 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance530 AS Infragistics.Win.Appearance NO-UNDO.
      appearance530 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance531 AS Infragistics.Win.Appearance NO-UNDO.
      appearance531 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance532 AS Infragistics.Win.Appearance NO-UNDO.
      appearance532 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance533 AS Infragistics.Win.Appearance NO-UNDO.
      appearance533 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance534 AS Infragistics.Win.Appearance NO-UNDO.
      appearance534 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance535 AS Infragistics.Win.Appearance NO-UNDO.
      appearance535 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab7 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab7 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance536 AS Infragistics.Win.Appearance NO-UNDO.
      appearance536 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab8 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab8 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance537 AS Infragistics.Win.Appearance NO-UNDO.
      appearance537 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab9 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab9 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance538 AS Infragistics.Win.Appearance NO-UNDO.
      appearance538 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab28 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab28 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance539 AS Infragistics.Win.Appearance NO-UNDO.
      appearance539 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance540 AS Infragistics.Win.Appearance NO-UNDO.
      appearance540 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance541 AS Infragistics.Win.Appearance NO-UNDO.
      appearance541 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance542 AS Infragistics.Win.Appearance NO-UNDO.
      appearance542 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance543 AS Infragistics.Win.Appearance NO-UNDO.
      appearance543 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance544 AS Infragistics.Win.Appearance NO-UNDO.
      appearance544 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance545 AS Infragistics.Win.Appearance NO-UNDO.
      appearance545 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance546 AS Infragistics.Win.Appearance NO-UNDO.
      appearance546 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance547 AS Infragistics.Win.Appearance NO-UNDO.
      appearance547 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance548 AS Infragistics.Win.Appearance NO-UNDO.
      appearance548 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance549 AS Infragistics.Win.Appearance NO-UNDO.
      appearance549 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance550 AS Infragistics.Win.Appearance NO-UNDO.
      appearance550 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance551 AS Infragistics.Win.Appearance NO-UNDO.
      appearance551 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance552 AS Infragistics.Win.Appearance NO-UNDO.
      appearance552 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance553 AS Infragistics.Win.Appearance NO-UNDO.
      appearance553 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance554 AS Infragistics.Win.Appearance NO-UNDO.
      appearance554 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance555 AS Infragistics.Win.Appearance NO-UNDO.
      appearance555 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance556 AS Infragistics.Win.Appearance NO-UNDO.
      appearance556 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance557 AS Infragistics.Win.Appearance NO-UNDO.
      appearance557 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance558 AS Infragistics.Win.Appearance NO-UNDO.
      appearance558 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance559 AS Infragistics.Win.Appearance NO-UNDO.
      appearance559 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance560 AS Infragistics.Win.Appearance NO-UNDO.
      appearance560 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance561 AS Infragistics.Win.Appearance NO-UNDO.
      appearance561 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance562 AS Infragistics.Win.Appearance NO-UNDO.
      appearance562 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance563 AS Infragistics.Win.Appearance NO-UNDO.
      appearance563 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance564 AS Infragistics.Win.Appearance NO-UNDO.
      appearance564 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance565 AS Infragistics.Win.Appearance NO-UNDO.
      appearance565 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab29 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab29 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance566 AS Infragistics.Win.Appearance NO-UNDO.
      appearance566 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance567 AS Infragistics.Win.Appearance NO-UNDO.
      appearance567 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance568 AS Infragistics.Win.Appearance NO-UNDO.
      appearance568 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance569 AS Infragistics.Win.Appearance NO-UNDO.
      appearance569 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance570 AS Infragistics.Win.Appearance NO-UNDO.
      appearance570 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance571 AS Infragistics.Win.Appearance NO-UNDO.
      appearance571 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance572 AS Infragistics.Win.Appearance NO-UNDO.
      appearance572 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance573 AS Infragistics.Win.Appearance NO-UNDO.
      appearance573 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance574 AS Infragistics.Win.Appearance NO-UNDO.
      appearance574 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance575 AS Infragistics.Win.Appearance NO-UNDO.
      appearance575 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance576 AS Infragistics.Win.Appearance NO-UNDO.
      appearance576 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance577 AS Infragistics.Win.Appearance NO-UNDO.
      appearance577 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance578 AS Infragistics.Win.Appearance NO-UNDO.
      appearance578 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance579 AS Infragistics.Win.Appearance NO-UNDO.
      appearance579 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance580 AS Infragistics.Win.Appearance NO-UNDO.
      appearance580 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance581 AS Infragistics.Win.Appearance NO-UNDO.
      appearance581 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance582 AS Infragistics.Win.Appearance NO-UNDO.
      appearance582 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance583 AS Infragistics.Win.Appearance NO-UNDO.
      appearance583 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance584 AS Infragistics.Win.Appearance NO-UNDO.
      appearance584 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab25 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab25 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      THIS-OBJECT:TabRPPaslag = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerPaslag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPaslagOver = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagToolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridPaslag = NEW Controls.Grid().
      THIS-OBJECT:buttonOverResurspaslagBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverResurspaslag = NEW Controls.ButtonOver().
      THIS-OBJECT:resurspaslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resursPaslagtoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridResursPaslag = NEW Controls.Grid().
      THIS-OBJECT:TabRPNiva = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerNiva = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripNiva = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillNivaButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortNiva = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResursniva = NEW Controls.Grid().
      THIS-OBJECT:TabRPRegel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:regelMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:regeltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridRegel = NEW Controls.Grid().
      THIS-OBJECT:TabRPBasresurser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerBasresurser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripBasresurser = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:buttonNyResurspris = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResursForResurspris = NEW Controls.Grid().
      THIS-OBJECT:TabP5resurs = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p5ResursSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p5ResursSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridResursForP5 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP5ResursBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP5Resurs = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP5Resurs = NEW Controls.Grid().
      THIS-OBJECT:TabP5Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP5ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P5ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP5Import = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP5Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP5Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP5Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP5 = NEW Controls.Grid().
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
      THIS-OBJECT:TabP3Resurs = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p3ResursSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3ResursSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridResursForP3 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP3ResursBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP3Resurs = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP3Resurs = NEW Controls.Grid().
      THIS-OBJECT:TabP3Frekvens = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p3FrekvensSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3FrekvensSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridP4ForP3 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP3FrekvensBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP3Frekvens = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP3Frekvens = NEW Controls.Grid().
      THIS-OBJECT:TabP3Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP3ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P3ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP3Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p3KodToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p3FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP3Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP3Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP3Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP3F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP3 = NEW Controls.Grid().
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
      THIS-OBJECT:TabP2Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP2ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P2ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP2Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p2KoderToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p2FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP2Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP2Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP2Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP2F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP2 = NEW Controls.Grid().
      THIS-OBJECT:TabP1frekvens = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p1FrekvensSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p1frekvensSC2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridP2forP1 = NEW Controls.Grid().
      THIS-OBJECT:buttonOverP1FrekvensBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverP1frekvens = NEW Controls.ButtonOver().
      THIS-OBJECT:gridP1Frekvens = NEW Controls.Grid().
      THIS-OBJECT:TabP1Import = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerP1ImportGrund = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:P1ImportStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonP1Import = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:p1KoderToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:p1FrekvensToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:StripButtonP1Rin = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:StripButtonP1Rbort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerP1Import = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridImportP1F = NEW Controls.Grid().
      THIS-OBJECT:GridImportP1 = NEW Controls.Grid().
      THIS-OBJECT:TabResurser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:resurserMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurserMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ResursertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurs = NEW Controls.Grid().
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:KatalogMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogKatalogSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:KatalogerValjButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:KopieraKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridKataloger = NEW Controls.Grid().
      THIS-OBJECT:TabResursPriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerResursPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerResursPrisLista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurspristoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:taBortResursPrisButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurspris = NEW Controls.Grid().
      THIS-OBJECT:TabManagerResurspriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabMtrl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:mtrlMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrlMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:mtrltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortMtrlButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:uppdateraMtrlKostButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlImportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:mtrlExportButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridMateriel = NEW Controls.Grid().
      THIS-OBJECT:TabP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p5MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p5KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p5KoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP5Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP5Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP5 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p4MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p4KodertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP4Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP4Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP4 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:p3MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3kodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3arbkodSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3arbkodStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP3arbkod = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP3arbkodButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP3arbkod = NEW Controls.Grid().
      THIS-OBJECT:p3KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p3KodertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP3Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP3Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP3 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
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
      THIS-OBJECT:taBortP2Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP2 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
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
      THIS-OBJECT:gridP1 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabExport = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerTotalExport = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabPopups = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:buttonOverResurspris = NEW Controls.ButtonOver().
      THIS-OBJECT:nyPaslagPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyPaslag = NEW Controls.Button().
      THIS-OBJECT:fillinPaslagBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyPaslag = NEW Controls.Button().
      THIS-OBJECT:nyMtrlPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyMtrl = NEW Controls.Button().
      THIS-OBJECT:fillinArtNr = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyMtrl = NEW Controls.Button().
      THIS-OBJECT:nyP1arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP1arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP1arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP1arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP2arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP2arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP2arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP2arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP3arbkodPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP3arbkod = NEW Controls.Button().
      THIS-OBJECT:fillinP3arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP3arbkod = NEW Controls.Button().
      THIS-OBJECT:nyP1Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP1ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP1LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP1 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP1 = NEW Controls.Button().
      THIS-OBJECT:nyP2Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP2ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP2LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP2 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP2 = NEW Controls.Button().
      THIS-OBJECT:nyP3Panel = NEW Controls.PopPanel().
      THIS-OBJECT:comboP3ArbKod = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinP3LopNr = NEW Controls.FillinInteger().
      THIS-OBJECT:buttonCancelNyP3 = NEW Controls.Button().
      THIS-OBJECT:buttonNyP3 = NEW Controls.Button().
      THIS-OBJECT:nyP4Panel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP4 = NEW Controls.Button().
      THIS-OBJECT:fillinP4arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP4 = NEW Controls.Button().
      THIS-OBJECT:nyP5Panel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyP5 = NEW Controls.Button().
      THIS-OBJECT:fillinP5arbkod = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyP5 = NEW Controls.Button().
      THIS-OBJECT:nyResursPanel = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelNyResurs = NEW Controls.Button().
      THIS-OBJECT:fillinResursNr = NEW Controls.FillinInteger().
      THIS-OBJECT:fillinResursBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:buttonNyResurs = NEW Controls.Button().
      THIS-OBJECT:nyNivaPanel = NEW Controls.PopPanel().
      THIS-OBJECT:comboNiva = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:buttonCancelNyNiva = NEW Controls.Button().
      THIS-OBJECT:buttonNyNiva = NEW Controls.Button().
      THIS-OBJECT:laggTillPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortPaslagButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:miniToolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:TabDelad = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabManagerEkg = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:nyResursPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP5Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP4Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP3Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP2Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP1Pop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP3arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP2arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyP1arbkodPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyPaslagPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyMtrlPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:nyNivaPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:TabRPPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslagOver:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:SuspendLayout().
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:paslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPNiva:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNiva:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:SuspendLayout().
      THIS-OBJECT:toolStripNiva:SuspendLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPRegel:SuspendLayout().
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:regelMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:regelMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:regelMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPBasresurser:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBasresurser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:SuspendLayout().
      THIS-OBJECT:toolStripBasresurser:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP5resurs:SuspendLayout().
      CAST(THIS-OBJECT:p5ResursSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p5ResursSC:Panel1:SuspendLayout().
      THIS-OBJECT:p5ResursSC:Panel2:SuspendLayout().
      THIS-OBJECT:p5ResursSC:SuspendLayout().
      CAST(THIS-OBJECT:p5ResursSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p5ResursSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p5ResursSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p5ResursSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForP5, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP5Resurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP5Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP5ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP5ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP5ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP5ImportGrund:SuspendLayout().
      THIS-OBJECT:P5ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP5Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP5Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP5Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP5, System.ComponentModel.ISupportInitialize):BeginInit().
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
      THIS-OBJECT:TabP3Resurs:SuspendLayout().
      CAST(THIS-OBJECT:p3ResursSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3ResursSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3ResursSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3ResursSC:SuspendLayout().
      CAST(THIS-OBJECT:p3ResursSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3ResursSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p3ResursSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p3ResursSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForP3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP3Resurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP3Frekvens:SuspendLayout().
      CAST(THIS-OBJECT:p3FrekvensSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3FrekvensSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3FrekvensSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3FrekvensSC:SuspendLayout().
      CAST(THIS-OBJECT:p3FrekvensSC2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3FrekvensSC2:Panel1:SuspendLayout().
      THIS-OBJECT:p3FrekvensSC2:Panel2:SuspendLayout().
      THIS-OBJECT:p3FrekvensSC2:SuspendLayout().
      CAST(THIS-OBJECT:gridP4ForP3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridP3Frekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP3Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP3ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP3ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP3ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP3ImportGrund:SuspendLayout().
      THIS-OBJECT:P3ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP3Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP3Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP3Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP3F, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP3, System.ComponentModel.ISupportInitialize):BeginInit().
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
      THIS-OBJECT:TabP2Import:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP2ImportGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP2ImportGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP2ImportGrund:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerP2ImportGrund:SuspendLayout().
      THIS-OBJECT:P2ImportStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerP2Import, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerP2Import:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerP2Import:SuspendLayout().
      CAST(THIS-OBJECT:GridImportP2F, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridImportP2, System.ComponentModel.ISupportInitialize):BeginInit().
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
      THIS-OBJECT:TabResurser:SuspendLayout().
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMainSC:SuspendLayout().
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:SuspendLayout().
      THIS-OBJECT:ResursertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKataloger:SuspendLayout().
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogMainSC:SuspendLayout().
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogKatalogSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:SuspendLayout().
      THIS-OBJECT:KatalogertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabResursPriser:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:SuspendLayout().
      THIS-OBJECT:resurspristoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerResurspriser:SuspendLayout().
      THIS-OBJECT:TabMtrl:SuspendLayout().
      CAST(THIS-OBJECT:mtrlMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMainSC:SuspendLayout().
      CAST(THIS-OBJECT:mtrlMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:mtrlMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:mtrlMenuSC:SuspendLayout().
      THIS-OBJECT:mtrltoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabP5:SuspendLayout().
      CAST(THIS-OBJECT:p5MainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p5MainSC:Panel1:SuspendLayout().
      THIS-OBJECT:p5MainSC:Panel2:SuspendLayout().
      THIS-OBJECT:p5MainSC:SuspendLayout().
      CAST(THIS-OBJECT:p5KoderMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p5KoderMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:p5KoderMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:p5KoderMenuSC:SuspendLayout().
      THIS-OBJECT:p5KoderStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP5, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerP5, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerP5:SuspendLayout().
      THIS-OBJECT:TabP4:SuspendLayout().
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
      THIS-OBJECT:TabP3:SuspendLayout().
      CAST(THIS-OBJECT:p3MainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3MainSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3MainSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3MainSC:SuspendLayout().
      CAST(THIS-OBJECT:p3kodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3kodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3kodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3kodSC:SuspendLayout().
      CAST(THIS-OBJECT:p3arbkodSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3arbkodSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3arbkodSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3arbkodSC:SuspendLayout().
      THIS-OBJECT:p3arbkodStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP3arbkod, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:p3KoderMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p3KoderMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:p3KoderMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:p3KoderMenuSC:SuspendLayout().
      THIS-OBJECT:p3KodertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridP3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerP3, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerP3:SuspendLayout().
      THIS-OBJECT:TabP2:SuspendLayout().
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
      THIS-OBJECT:TabP1:SuspendLayout().
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
      THIS-OBJECT:TabExport:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerTotalExport, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerTotalExport:SuspendLayout().
      THIS-OBJECT:TabPopups:SuspendLayout().
      THIS-OBJECT:nyPaslagPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyPaslagPanel:SuspendLayout().
      THIS-OBJECT:nyMtrlPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyMtrlPanel:SuspendLayout().
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP1arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP2arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP3arbkodPanel:SuspendLayout().
      THIS-OBJECT:nyP1Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP1Panel:SuspendLayout().
      THIS-OBJECT:nyP2Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP2Panel:SuspendLayout().
      THIS-OBJECT:nyP3Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP3Panel:SuspendLayout().
      THIS-OBJECT:nyP4Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP4Panel:SuspendLayout().
      THIS-OBJECT:nyP5Panel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyP5Panel:SuspendLayout().
      THIS-OBJECT:nyResursPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyResursPanel:SuspendLayout().
      THIS-OBJECT:nyNivaPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:nyNivaPanel:SuspendLayout().
      CAST(THIS-OBJECT:TabManagerEkg, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerEkg:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabRPPaslag */
      /*  */
      THIS-OBJECT:TabRPPaslag:Controls:Add(THIS-OBJECT:splitContainerPaslag).
      THIS-OBJECT:TabRPPaslag:Location = NEW System.Drawing.Point(1, 23).
      THIS-OBJECT:TabRPPaslag:Name = "TabRPPaslag".
      THIS-OBJECT:TabRPPaslag:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerPaslag */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslag:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslag:Name = "splitContainerPaslag".
      /*  */
      /* splitContainerPaslag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel1:Controls:Add(THIS-OBJECT:splitContainerPaslagOver).
      /*  */
      /* splitContainerPaslag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel2:Controls:Add(THIS-OBJECT:resurspaslagMenuSC).
      THIS-OBJECT:splitContainerPaslag:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerPaslag:SplitterDistance = 362.
      THIS-OBJECT:splitContainerPaslag:TabIndex = 0.
      /*  */
      /* splitContainerPaslagOver */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslagOver:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslagOver:Name = "splitContainerPaslagOver".
      /*  */
      /* splitContainerPaslagOver.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel1:Controls:Add(THIS-OBJECT:paslagMenuSC).
      /*  */
      /* splitContainerPaslagOver.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslagBort).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslag).
      THIS-OBJECT:splitContainerPaslagOver:Size = NEW System.Drawing.Size(362, 527).
      THIS-OBJECT:splitContainerPaslagOver:SplitterDistance = 328.
      THIS-OBJECT:splitContainerPaslagOver:TabIndex = 0.
      /*  */
      /* paslagMenuSC */
      /*  */
      THIS-OBJECT:paslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:paslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:paslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagMenuSC:Name = "paslagMenuSC".
      THIS-OBJECT:paslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* paslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:paslagToolStrip).
      /*  */
      /* paslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridPaslag).
      THIS-OBJECT:paslagMenuSC:Size = NEW System.Drawing.Size(328, 527).
      THIS-OBJECT:paslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:paslagMenuSC:TabIndex = 1.
      /*  */
      /* paslagToolStrip */
      /*  */
      THIS-OBJECT:paslagToolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagToolStrip:Name = "paslagToolStrip".
      THIS-OBJECT:paslagToolStrip:Size = NEW System.Drawing.Size(328, 25).
      THIS-OBJECT:paslagToolStrip:TabIndex = 1.
      THIS-OBJECT:paslagToolStrip:Text = "toolStrip1".
      /*  */
      /* gridPaslag */
      /*  */
      THIS-OBJECT:gridPaslag:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridPaslag:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridPaslag:GuruContext = ?.
      THIS-OBJECT:gridPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridPaslag:Name = "gridPaslag".
      THIS-OBJECT:gridPaslag:Size = NEW System.Drawing.Size(328, 498).
      THIS-OBJECT:gridPaslag:TabIndex = 0.
      THIS-OBJECT:gridPaslag:Text = "grid1".
      THIS-OBJECT:gridPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridPaslag_GuruAfterDropped).
      THIS-OBJECT:gridPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridPaslag_AfterCellUpdate).
      /*  */
      /* buttonOverResurspaslagBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslagBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverResurspaslagBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverResurspaslagBort:Name = "buttonOverResurspaslagBort".
      THIS-OBJECT:buttonOverResurspaslagBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslagBort:TabIndex = 7.
      THIS-OBJECT:buttonOverResurspaslagBort:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslagBort_Click).
      /*  */
      /* buttonOverResurspaslag */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslag:Appearance = appearance14.
      THIS-OBJECT:buttonOverResurspaslag:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverResurspaslag:Name = "buttonOverResurspaslag".
      THIS-OBJECT:buttonOverResurspaslag:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslag:TabIndex = 6.
      THIS-OBJECT:buttonOverResurspaslag:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslag_Click).
      /*  */
      /* resurspaslagMenuSC */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurspaslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurspaslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspaslagMenuSC:Name = "resurspaslagMenuSC".
      THIS-OBJECT:resurspaslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurspaslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:resursPaslagtoolStrip).
      /*  */
      /* resurspaslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResursPaslag).
      THIS-OBJECT:resurspaslagMenuSC:Size = NEW System.Drawing.Size(296, 527).
      THIS-OBJECT:resurspaslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurspaslagMenuSC:TabIndex = 2.
      /*  */
      /* resursPaslagtoolStrip */
      /*  */
      THIS-OBJECT:resursPaslagtoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resursPaslagtoolStrip:Name = "resursPaslagtoolStrip".
      THIS-OBJECT:resursPaslagtoolStrip:Size = NEW System.Drawing.Size(296, 25).
      THIS-OBJECT:resursPaslagtoolStrip:TabIndex = 1.
      THIS-OBJECT:resursPaslagtoolStrip:Text = "toolStrip1".
      /*  */
      /* gridResursPaslag */
      /*  */
      THIS-OBJECT:gridResursPaslag:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursPaslag:GuruContext = ?.
      THIS-OBJECT:gridResursPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursPaslag:Name = "gridResursPaslag".
      THIS-OBJECT:gridResursPaslag:Size = NEW System.Drawing.Size(296, 498).
      THIS-OBJECT:gridResursPaslag:TabIndex = 0.
      THIS-OBJECT:gridResursPaslag:Text = "grid1".
      THIS-OBJECT:gridResursPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursPaslag_GuruAfterDropped).
      THIS-OBJECT:gridResursPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurspaslag_AfterCellUpdate).
      /*  */
      /* TabRPNiva */
      /*  */
      THIS-OBJECT:TabRPNiva:Controls:Add(THIS-OBJECT:splitContainerNiva).
      THIS-OBJECT:TabRPNiva:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPNiva:Name = "TabRPNiva".
      THIS-OBJECT:TabRPNiva:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerNiva */
      /*  */
      THIS-OBJECT:splitContainerNiva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNiva:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNiva:Name = "splitContainerNiva".
      THIS-OBJECT:splitContainerNiva:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNiva.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel1:Controls:Add(THIS-OBJECT:toolStripNiva).
      /*  */
      /* splitContainerNiva.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel2:Controls:Add(THIS-OBJECT:gridResursniva).
      THIS-OBJECT:splitContainerNiva:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerNiva:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNiva:TabIndex = 0.
      /*  */
      /* toolStripNiva */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillNivaButton.
      arrayvar0[2] = THIS-OBJECT:taBortNiva.
      THIS-OBJECT:toolStripNiva:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripNiva:Name = "toolStripNiva".
      THIS-OBJECT:toolStripNiva:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:toolStripNiva:TabIndex = 0.
      THIS-OBJECT:toolStripNiva:Text = "toolStrip1".
      /*  */
      /* laggTillNivaButton */
      /*  */
      THIS-OBJECT:laggTillNivaButton:Image = CAST(resources:GetObject("laggTillNivaButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillNivaButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillNivaButton:Name = "laggTillNivaButton".
      THIS-OBJECT:laggTillNivaButton:Size = NEW System.Drawing.Size(110, 22).
      THIS-OBJECT:laggTillNivaButton:Text = "Lägg till ny nivå".
      THIS-OBJECT:laggTillNivaButton:ToolTipText = "Lägg till resursen på ny nivå".
      THIS-OBJECT:laggTillNivaButton:Click:Subscribe(THIS-OBJECT:laggTillNivaButton_Click).
      /*  */
      /* taBortNiva */
      /*  */
      THIS-OBJECT:taBortNiva:Image = CAST(resources:GetObject("taBortNiva.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortNiva:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortNiva:Name = "taBortNiva".
      THIS-OBJECT:taBortNiva:Size = NEW System.Drawing.Size(90, 22).
      THIS-OBJECT:taBortNiva:Text = "Ta bort nivå".
      THIS-OBJECT:taBortNiva:ToolTipText = "Ta bort resursen för nivå".
      THIS-OBJECT:taBortNiva:Click:Subscribe(THIS-OBJECT:taBortNiva_Click).
      /*  */
      /* gridResursniva */
      /*  */
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursniva:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridResursniva:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursniva:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursniva:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursniva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursniva:GuruContext = ?.
      THIS-OBJECT:gridResursniva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursniva:Name = "gridResursniva".
      THIS-OBJECT:gridResursniva:Size = NEW System.Drawing.Size(662, 498).
      THIS-OBJECT:gridResursniva:TabIndex = 2.
      THIS-OBJECT:gridResursniva:Text = "grid1".
      THIS-OBJECT:gridResursniva:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResursniva_AfterCellUpdate).
      /*  */
      /* TabRPRegel */
      /*  */
      THIS-OBJECT:TabRPRegel:Controls:Add(THIS-OBJECT:regelMenuSC).
      THIS-OBJECT:TabRPRegel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPRegel:Name = "TabRPRegel".
      THIS-OBJECT:TabRPRegel:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* regelMenuSC */
      /*  */
      THIS-OBJECT:regelMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:regelMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:regelMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regelMenuSC:Name = "regelMenuSC".
      THIS-OBJECT:regelMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* regelMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel1:Controls:Add(THIS-OBJECT:regeltoolStrip).
      /*  */
      /* regelMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridRegel).
      THIS-OBJECT:regelMenuSC:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:regelMenuSC:SplitterDistance = 25.
      THIS-OBJECT:regelMenuSC:TabIndex = 1.
      /*  */
      /* regeltoolStrip */
      /*  */
      THIS-OBJECT:regeltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regeltoolStrip:Name = "regeltoolStrip".
      THIS-OBJECT:regeltoolStrip:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:regeltoolStrip:TabIndex = 0.
      THIS-OBJECT:regeltoolStrip:Text = "toolStrip2".
      /*  */
      /* gridRegel */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridRegel:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:gridRegel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridRegel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridRegel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridRegel:GuruContext = ?.
      THIS-OBJECT:gridRegel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridRegel:Name = "gridRegel".
      THIS-OBJECT:gridRegel:Size = NEW System.Drawing.Size(787, 498).
      THIS-OBJECT:gridRegel:TabIndex = 3.
      THIS-OBJECT:gridRegel:Text = "grid2".
      THIS-OBJECT:gridRegel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridRegel_AfterCellUpdate).
      /*  */
      /* TabRPBasresurser */
      /*  */
      THIS-OBJECT:TabRPBasresurser:Controls:Add(THIS-OBJECT:splitContainerBasresurser).
      THIS-OBJECT:TabRPBasresurser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPBasresurser:Name = "TabRPBasresurser".
      THIS-OBJECT:TabRPBasresurser:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* splitContainerBasresurser */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBasresurser:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBasresurser:Name = "splitContainerBasresurser".
      THIS-OBJECT:splitContainerBasresurser:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerBasresurser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel1:Controls:Add(THIS-OBJECT:toolStripBasresurser).
      /*  */
      /* splitContainerBasresurser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel2:Controls:Add(THIS-OBJECT:gridResursForResurspris).
      THIS-OBJECT:splitContainerBasresurser:Size = NEW System.Drawing.Size(662, 527).
      THIS-OBJECT:splitContainerBasresurser:SplitterDistance = 25.
      THIS-OBJECT:splitContainerBasresurser:TabIndex = 0.
      /*  */
      /* toolStripBasresurser */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:buttonNyResurspris.
      THIS-OBJECT:toolStripBasresurser:Items:AddRange(arrayvar1).
      THIS-OBJECT:toolStripBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripBasresurser:Name = "toolStripBasresurser".
      THIS-OBJECT:toolStripBasresurser:Size = NEW System.Drawing.Size(662, 25).
      THIS-OBJECT:toolStripBasresurser:TabIndex = 0.
      THIS-OBJECT:toolStripBasresurser:Text = "toolStrip2".
      /*  */
      /* buttonNyResurspris */
      /*  */
      THIS-OBJECT:buttonNyResurspris:Image = CAST(resources:GetObject("buttonNyResurspris.Image"), System.Drawing.Image).
      THIS-OBJECT:buttonNyResurspris:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:buttonNyResurspris:Name = "buttonNyResurspris".
      THIS-OBJECT:buttonNyResurspris:Size = NEW System.Drawing.Size(151, 22).
      THIS-OBJECT:buttonNyResurspris:Text = "Lägg till resurs i katalog".
      THIS-OBJECT:buttonNyResurspris:ToolTipText = "Lägg till den markerade resursern i katalogen".
      THIS-OBJECT:buttonNyResurspris:Click:Subscribe(THIS-OBJECT:buttonNyResurspris_Click).
      /*  */
      /* gridResursForResurspris */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForResurspris:GuruContext = ?.
      THIS-OBJECT:gridResursForResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForResurspris:Name = "gridResursForResurspris".
      THIS-OBJECT:gridResursForResurspris:Size = NEW System.Drawing.Size(662, 498).
      THIS-OBJECT:gridResursForResurspris:TabIndex = 1.
      THIS-OBJECT:gridResursForResurspris:Text = "grid1".
      /*  */
      /* TabP5resurs */
      /*  */
      THIS-OBJECT:TabP5resurs:Controls:Add(THIS-OBJECT:p5ResursSC).
      THIS-OBJECT:TabP5resurs:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP5resurs:Name = "TabP5resurs".
      THIS-OBJECT:TabP5resurs:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p5ResursSC */
      /*  */
      THIS-OBJECT:p5ResursSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p5ResursSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p5ResursSC:Name = "p5ResursSC".
      /*  */
      /* p5ResursSC.Panel1 */
      /*  */
      THIS-OBJECT:p5ResursSC:Panel1:Controls:Add(THIS-OBJECT:p5ResursSC2).
      /*  */
      /* p5ResursSC.Panel2 */
      /*  */
      THIS-OBJECT:p5ResursSC:Panel2:Controls:Add(THIS-OBJECT:gridP5Resurs).
      THIS-OBJECT:p5ResursSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p5ResursSC:SplitterDistance = 349.
      THIS-OBJECT:p5ResursSC:TabIndex = 1.
      /*  */
      /* p5ResursSC2 */
      /*  */
      THIS-OBJECT:p5ResursSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p5ResursSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p5ResursSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p5ResursSC2:Name = "p5ResursSC2".
      /*  */
      /* p5ResursSC2.Panel1 */
      /*  */
      THIS-OBJECT:p5ResursSC2:Panel1:Controls:Add(THIS-OBJECT:gridResursForP5).
      /*  */
      /* p5ResursSC2.Panel2 */
      /*  */
      THIS-OBJECT:p5ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP5ResursBort).
      THIS-OBJECT:p5ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP5Resurs).
      THIS-OBJECT:p5ResursSC2:Size = NEW System.Drawing.Size(349, 284).
      THIS-OBJECT:p5ResursSC2:SplitterDistance = 309.
      THIS-OBJECT:p5ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP5 */
      /*  */
      THIS-OBJECT:gridResursForP5:AllowDrop = TRUE.
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:gridResursForP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:gridResursForP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:gridResursForP5:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForP5:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForP5:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForP5:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForP5:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForP5:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForP5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForP5:GuruContext = ?.
      THIS-OBJECT:gridResursForP5:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursForP5:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursForP5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForP5:Name = "gridResursForP5".
      THIS-OBJECT:gridResursForP5:Size = NEW System.Drawing.Size(309, 284).
      THIS-OBJECT:gridResursForP5:TabIndex = 1.
      THIS-OBJECT:gridResursForP5:Text = "grid1".
      THIS-OBJECT:gridResursForP5:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP5_GuruAfterDropped).
      /*  */
      /* buttonOverP5ResursBort */
      /*  */
      appearance75:Image = CAST(resources:GetObject("appearance75.Image"), System.Object).
      THIS-OBJECT:buttonOverP5ResursBort:Appearance = appearance75.
      THIS-OBJECT:buttonOverP5ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP5ResursBort:Name = "buttonOverP5ResursBort".
      THIS-OBJECT:buttonOverP5ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP5ResursBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP5ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP5ResursBort_Click).
      /*  */
      /* buttonOverP5Resurs */
      /*  */
      appearance76:Image = CAST(resources:GetObject("appearance76.Image"), System.Object).
      THIS-OBJECT:buttonOverP5Resurs:Appearance = appearance76.
      THIS-OBJECT:buttonOverP5Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP5Resurs:Name = "buttonOverP5Resurs".
      THIS-OBJECT:buttonOverP5Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP5Resurs:TabIndex = 4.
      THIS-OBJECT:buttonOverP5Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP5Resurs_Click).
      /*  */
      /* gridP5Resurs */
      /*  */
      THIS-OBJECT:gridP5Resurs:AllowDrop = TRUE.
      appearance77:BackColor = System.Drawing.SystemColors:Window.
      appearance77:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Appearance = appearance77.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance78:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance78:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance78:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:Appearance = appearance78.
      appearance79:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance79.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance80:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance80:BackColor2 = System.Drawing.SystemColors:Control.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance80:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance80.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      appearance81:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance81.
      appearance82:BackColor = System.Drawing.SystemColors:Highlight.
      appearance82:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance82.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CardAreaAppearance = appearance83.
      appearance84:BorderColor = System.Drawing.Color:Silver.
      appearance84:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellAppearance = appearance84.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance85:BackColor = System.Drawing.SystemColors:Control.
      appearance85:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance85:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance85:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance85:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance85.
      appearance86:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderAppearance = appearance86.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:RowAppearance = appearance87.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance88.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP5Resurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP5Resurs:GuruContext = ?.
      THIS-OBJECT:gridP5Resurs:GuruDraggable = TRUE.
      THIS-OBJECT:gridP5Resurs:GuruDroppable = TRUE.
      THIS-OBJECT:gridP5Resurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP5Resurs:Name = "gridP5Resurs".
      THIS-OBJECT:gridP5Resurs:Size = NEW System.Drawing.Size(788, 284).
      THIS-OBJECT:gridP5Resurs:TabIndex = 0.
      THIS-OBJECT:gridP5Resurs:Text = "grid1".
      THIS-OBJECT:gridP5Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP5Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP5Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP5resurs_AfterCellUpdate).
      /*  */
      /* TabP5Import */
      /*  */
      THIS-OBJECT:TabP5Import:Controls:Add(THIS-OBJECT:splitContainerP5ImportGrund).
      THIS-OBJECT:TabP5Import:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabP5Import:Name = "TabP5Import".
      THIS-OBJECT:TabP5Import:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP5ImportGrund */
      /*  */
      THIS-OBJECT:splitContainerP5ImportGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP5ImportGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP5ImportGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP5ImportGrund:Name = "splitContainerP5ImportGrund".
      THIS-OBJECT:splitContainerP5ImportGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP5ImportGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP5ImportGrund:Panel1:Controls:Add(THIS-OBJECT:P5ImportStrip).
      /*  */
      /* splitContainerP5ImportGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP5ImportGrund:Panel2:Controls:Add(THIS-OBJECT:splitContainerP5Import).
      THIS-OBJECT:splitContainerP5ImportGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP5ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP5ImportGrund:TabIndex = 1.
      /*  */
      /* P5ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:StripButtonP5Import.
      arrayvar2[2] = THIS-OBJECT:StripButtonP5Rin.
      arrayvar2[3] = THIS-OBJECT:StripButtonP5Rbort.
      THIS-OBJECT:P5ImportStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:P5ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P5ImportStrip:Name = "P5ImportStrip".
      THIS-OBJECT:P5ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P5ImportStrip:TabIndex = 0.
      THIS-OBJECT:P5ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP5Import */
      /*  */
      THIS-OBJECT:StripButtonP5Import:Image = CAST(resources:GetObject("StripButtonP5Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP5Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP5Import:Name = "StripButtonP5Import".
      THIS-OBJECT:StripButtonP5Import:Size = NEW System.Drawing.Size(85, 22).
      THIS-OBJECT:StripButtonP5Import:Text = "Välj Excelfil".
      THIS-OBJECT:StripButtonP5Import:ToolTipText = "Filen ska vara enligt spec och data i blad1".
      THIS-OBJECT:StripButtonP5Import:Click:Subscribe(THIS-OBJECT:StripButtonP5Import_Click).
      /*  */
      /* StripButtonP5Rin */
      /*  */
      THIS-OBJECT:StripButtonP5Rin:Image = CAST(resources:GetObject("StripButtonP5Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP5Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP5Rin:Name = "StripButtonP5Rin".
      THIS-OBJECT:StripButtonP5Rin:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:StripButtonP5Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP5Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP5Rin:Click:Subscribe(THIS-OBJECT:StripButtonP5Rin_Click).
      /*  */
      /* StripButtonP5Rbort */
      /*  */
      THIS-OBJECT:StripButtonP5Rbort:Image = CAST(resources:GetObject("StripButtonP5Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP5Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP5Rbort:Name = "StripButtonP5Rbort".
      THIS-OBJECT:StripButtonP5Rbort:Size = NEW System.Drawing.Size(163, 22).
      THIS-OBJECT:StripButtonP5Rbort:Text = "Ta bort markerade poster ".
      THIS-OBJECT:StripButtonP5Rbort:Visible = FALSE.
      THIS-OBJECT:StripButtonP5Rbort:Click:Subscribe(THIS-OBJECT:StripButtonP5Rbort_Click).
      /*  */
      /* splitContainerP5Import */
      /*  */
      THIS-OBJECT:splitContainerP5Import:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP5Import:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP5Import:Name = "splitContainerP5Import".
      /*  */
      /* splitContainerP5Import.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP5Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP5).
      THIS-OBJECT:splitContainerP5Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP5Import:SplitterDistance = 379.
      THIS-OBJECT:splitContainerP5Import:TabIndex = 0.
      /*  */
      /* GridImportP5 */
      /*  */
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP5:DisplayLayout:Appearance = appearance89.
      THIS-OBJECT:GridImportP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance90:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance90:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:Appearance = appearance90.
      appearance91:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance91.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance92:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance92:BackColor2 = System.Drawing.SystemColors:Control.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance92:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:PromptAppearance = appearance92.
      THIS-OBJECT:GridImportP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      appearance93:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveCellAppearance = appearance93.
      appearance94:BackColor = System.Drawing.SystemColors:Highlight.
      appearance94:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveRowAppearance = appearance94.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CardAreaAppearance = appearance95.
      appearance96:BorderColor = System.Drawing.Color:Silver.
      appearance96:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellAppearance = appearance96.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellPadding = 0.
      appearance97:BackColor = System.Drawing.SystemColors:Control.
      appearance97:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance97:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance97:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance97:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:GroupByRowAppearance = appearance97.
      appearance98:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderAppearance = appearance98.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:RowAppearance = appearance99.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance100.
      THIS-OBJECT:GridImportP5:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP5:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP5:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP5:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP5:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP5:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP5:GuruContext = ?.
      THIS-OBJECT:GridImportP5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP5:Name = "GridImportP5".
      THIS-OBJECT:GridImportP5:Size = NEW System.Drawing.Size(379, 255).
      THIS-OBJECT:GridImportP5:TabIndex = 0.
      THIS-OBJECT:GridImportP5:Text = "grid1".
      /*  */
      /* TabP4Resurs */
      /*  */
      THIS-OBJECT:TabP4Resurs:Controls:Add(THIS-OBJECT:p4ResursSC).
      THIS-OBJECT:TabP4Resurs:Location = NEW System.Drawing.Point(-10000, -10000).
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
      appearance101:BackColor = System.Drawing.SystemColors:Window.
      appearance101:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Appearance = appearance101.
      THIS-OBJECT:gridResursForP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance102:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance102:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance102:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance102:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:Appearance = appearance102.
      appearance103:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance103.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance104:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance104:BackColor2 = System.Drawing.SystemColors:Control.
      appearance104:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance104:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:GroupByBox:PromptAppearance = appearance104.
      THIS-OBJECT:gridResursForP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance105:BackColor = System.Drawing.SystemColors:Window.
      appearance105:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveCellAppearance = appearance105.
      appearance106:BackColor = System.Drawing.SystemColors:Highlight.
      appearance106:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:ActiveRowAppearance = appearance106.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance107:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CardAreaAppearance = appearance107.
      appearance108:BorderColor = System.Drawing.Color:Silver.
      appearance108:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellAppearance = appearance108.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:CellPadding = 0.
      appearance109:BackColor = System.Drawing.SystemColors:Control.
      appearance109:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance109:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance109:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance109:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:GroupByRowAppearance = appearance109.
      appearance110:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderAppearance = appearance110.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance111:BackColor = System.Drawing.SystemColors:Window.
      appearance111:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:RowAppearance = appearance111.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance112:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance112.
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
      appearance113:Image = CAST(resources:GetObject("appearance113.Image"), System.Object).
      THIS-OBJECT:buttonOverP4ResursBort:Appearance = appearance113.
      THIS-OBJECT:buttonOverP4ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP4ResursBort:Name = "buttonOverP4ResursBort".
      THIS-OBJECT:buttonOverP4ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4ResursBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP4ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP4ResursBort_Click).
      /*  */
      /* buttonOverP4Resurs */
      /*  */
      appearance114:Image = CAST(resources:GetObject("appearance114.Image"), System.Object).
      THIS-OBJECT:buttonOverP4Resurs:Appearance = appearance114.
      THIS-OBJECT:buttonOverP4Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP4Resurs:Name = "buttonOverP4Resurs".
      THIS-OBJECT:buttonOverP4Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4Resurs:TabIndex = 4.
      THIS-OBJECT:buttonOverP4Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP4Resurs_Click).
      /*  */
      /* gridP4Resurs */
      /*  */
      THIS-OBJECT:gridP4Resurs:AllowDrop = TRUE.
      appearance115:BackColor = System.Drawing.SystemColors:Window.
      appearance115:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Appearance = appearance115.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance116:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance116:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance116:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance116:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:Appearance = appearance116.
      appearance117:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance117.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance118:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance118:BackColor2 = System.Drawing.SystemColors:Control.
      appearance118:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance118:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance118.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance119:BackColor = System.Drawing.SystemColors:Window.
      appearance119:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance119.
      appearance120:BackColor = System.Drawing.SystemColors:Highlight.
      appearance120:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance120.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance121:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CardAreaAppearance = appearance121.
      appearance122:BorderColor = System.Drawing.Color:Silver.
      appearance122:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellAppearance = appearance122.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance123:BackColor = System.Drawing.SystemColors:Control.
      appearance123:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance123:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance123:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance123:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance123.
      appearance124:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderAppearance = appearance124.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance125:BackColor = System.Drawing.SystemColors:Window.
      appearance125:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:RowAppearance = appearance125.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance126:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance126.
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
      THIS-OBJECT:gridP4Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP4resurs_AfterCellUpdate).
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
      THIS-OBJECT:p4FrekvensSC:SplitterDistance = 350.
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
      THIS-OBJECT:p4FrekvensSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p4FrekvensSC2:SplitterDistance = 310.
      THIS-OBJECT:p4FrekvensSC2:TabIndex = 0.
      /*  */
      /* gridP5ForP4 */
      /*  */
      THIS-OBJECT:gridP5ForP4:AllowDrop = TRUE.
      appearance127:BackColor = System.Drawing.SystemColors:Window.
      appearance127:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Appearance = appearance127.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance128:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance128:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance128:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance128:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:Appearance = appearance128.
      appearance129:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance129.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance130:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance130:BackColor2 = System.Drawing.SystemColors:Control.
      appearance130:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance130:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:GroupByBox:PromptAppearance = appearance130.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance131:BackColor = System.Drawing.SystemColors:Window.
      appearance131:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveCellAppearance = appearance131.
      appearance132:BackColor = System.Drawing.SystemColors:Highlight.
      appearance132:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:ActiveRowAppearance = appearance132.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance133:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CardAreaAppearance = appearance133.
      appearance134:BorderColor = System.Drawing.Color:Silver.
      appearance134:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellAppearance = appearance134.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:CellPadding = 0.
      appearance135:BackColor = System.Drawing.SystemColors:Control.
      appearance135:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance135:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance135:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance135:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:GroupByRowAppearance = appearance135.
      appearance136:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderAppearance = appearance136.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance137:BackColor = System.Drawing.SystemColors:Window.
      appearance137:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:RowAppearance = appearance137.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance138:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5ForP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance138.
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
      appearance139:Image = CAST(resources:GetObject("appearance139.Image"), System.Object).
      THIS-OBJECT:buttonOverP4FrekvensBort:Appearance = appearance139.
      THIS-OBJECT:buttonOverP4FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP4FrekvensBort:Name = "buttonOverP4FrekvensBort".
      THIS-OBJECT:buttonOverP4FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4FrekvensBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP4FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP4FrekvensBort_Click).
      /*  */
      /* buttonOverP4Frekvens */
      /*  */
      appearance140:Image = CAST(resources:GetObject("appearance140.Image"), System.Object).
      THIS-OBJECT:buttonOverP4Frekvens:Appearance = appearance140.
      THIS-OBJECT:buttonOverP4Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP4Frekvens:Name = "buttonOverP4Frekvens".
      THIS-OBJECT:buttonOverP4Frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP4Frekvens:TabIndex = 4.
      THIS-OBJECT:buttonOverP4Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP4Frekvens_Click).
      /*  */
      /* gridP4Frekvens */
      /*  */
      THIS-OBJECT:gridP4Frekvens:AllowDrop = TRUE.
      appearance141:BackColor = System.Drawing.SystemColors:Window.
      appearance141:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Appearance = appearance141.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance142:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance142:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance142:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance142:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:Appearance = appearance142.
      appearance143:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance143.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance144:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance144:BackColor2 = System.Drawing.SystemColors:Control.
      appearance144:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance144:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance144.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance145:BackColor = System.Drawing.SystemColors:Window.
      appearance145:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance145.
      appearance146:BackColor = System.Drawing.SystemColors:Highlight.
      appearance146:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance146.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance147:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance147.
      appearance148:BorderColor = System.Drawing.Color:Silver.
      appearance148:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellAppearance = appearance148.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance149:BackColor = System.Drawing.SystemColors:Control.
      appearance149:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance149:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance149:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance149:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance149.
      appearance150:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderAppearance = appearance150.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance151:BackColor = System.Drawing.SystemColors:Window.
      appearance151:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:RowAppearance = appearance151.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance152:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance152.
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
      THIS-OBJECT:gridP4Frekvens:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP4Frekvens:TabIndex = 1.
      THIS-OBJECT:gridP4Frekvens:Text = "grid1".
      THIS-OBJECT:gridP4Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP4Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP4Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP4frekvens_AfterCellUpdate).
      /*  */
      /* TabP4Import */
      /*  */
      THIS-OBJECT:TabP4Import:Controls:Add(THIS-OBJECT:splitContainerP4ImportGrund).
      THIS-OBJECT:TabP4Import:Location = NEW System.Drawing.Point(1, 32).
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
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:StripButtonP4Import.
      arrayvar3[2] = THIS-OBJECT:StripButtonP4Rin.
      arrayvar3[3] = THIS-OBJECT:StripButtonP4Rbort.
      THIS-OBJECT:P4ImportStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:P4ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P4ImportStrip:Name = "P4ImportStrip".
      THIS-OBJECT:P4ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P4ImportStrip:TabIndex = 0.
      THIS-OBJECT:P4ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP4Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar4[1] = THIS-OBJECT:p4KodToolStripMenuItem.
      arrayvar4[2] = THIS-OBJECT:p4FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP4Import:DropDownItems:AddRange(arrayvar4).
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
      THIS-OBJECT:p4KodToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p4KodToolStripMenuItem:Text = "P4 Kod".
      THIS-OBJECT:p4KodToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p4KoderToolStripMenuItem_Click).
      /*  */
      /* p4FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p4FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p4FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Name = "p4FrekvensToolStripMenuItem".
      THIS-OBJECT:p4FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
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
      appearance153:BackColor = System.Drawing.SystemColors:Window.
      appearance153:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Appearance = appearance153.
      THIS-OBJECT:GridImportP4F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP4F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance154:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance154:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance154:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance154:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:Appearance = appearance154.
      appearance155:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance155.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance156:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance156:BackColor2 = System.Drawing.SystemColors:Control.
      appearance156:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance156:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:GroupByBox:PromptAppearance = appearance156.
      THIS-OBJECT:GridImportP4F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP4F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance157:BackColor = System.Drawing.SystemColors:Window.
      appearance157:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveCellAppearance = appearance157.
      appearance158:BackColor = System.Drawing.SystemColors:Highlight.
      appearance158:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:ActiveRowAppearance = appearance158.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance159:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CardAreaAppearance = appearance159.
      appearance160:BorderColor = System.Drawing.Color:Silver.
      appearance160:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellAppearance = appearance160.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:CellPadding = 0.
      appearance161:BackColor = System.Drawing.SystemColors:Control.
      appearance161:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance161:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance161:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance161:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:GroupByRowAppearance = appearance161.
      appearance162:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderAppearance = appearance162.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance163:BackColor = System.Drawing.SystemColors:Window.
      appearance163:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:RowAppearance = appearance163.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance164:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP4F:DisplayLayout:Override:TemplateAddRowAppearance = appearance164.
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
      appearance165:BackColor = System.Drawing.SystemColors:Window.
      appearance165:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP4:DisplayLayout:Appearance = appearance165.
      THIS-OBJECT:GridImportP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance166:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance166:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance166:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance166:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:Appearance = appearance166.
      appearance167:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance167.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance168:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance168:BackColor2 = System.Drawing.SystemColors:Control.
      appearance168:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance168:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP4:DisplayLayout:GroupByBox:PromptAppearance = appearance168.
      THIS-OBJECT:GridImportP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance169:BackColor = System.Drawing.SystemColors:Window.
      appearance169:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveCellAppearance = appearance169.
      appearance170:BackColor = System.Drawing.SystemColors:Highlight.
      appearance170:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:ActiveRowAppearance = appearance170.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance171:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CardAreaAppearance = appearance171.
      appearance172:BorderColor = System.Drawing.Color:Silver.
      appearance172:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellAppearance = appearance172.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:CellPadding = 0.
      appearance173:BackColor = System.Drawing.SystemColors:Control.
      appearance173:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance173:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance173:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance173:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:GroupByRowAppearance = appearance173.
      appearance174:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderAppearance = appearance174.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance175:BackColor = System.Drawing.SystemColors:Window.
      appearance175:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:RowAppearance = appearance175.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance176:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance176.
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
      /* TabP3Resurs */
      /*  */
      THIS-OBJECT:TabP3Resurs:Controls:Add(THIS-OBJECT:p3ResursSC).
      THIS-OBJECT:TabP3Resurs:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP3Resurs:Name = "TabP3Resurs".
      THIS-OBJECT:TabP3Resurs:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p3ResursSC */
      /*  */
      THIS-OBJECT:p3ResursSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3ResursSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3ResursSC:Name = "p3ResursSC".
      /*  */
      /* p3ResursSC.Panel1 */
      /*  */
      THIS-OBJECT:p3ResursSC:Panel1:Controls:Add(THIS-OBJECT:p3ResursSC2).
      /*  */
      /* p3ResursSC.Panel2 */
      /*  */
      THIS-OBJECT:p3ResursSC:Panel2:Controls:Add(THIS-OBJECT:gridP3Resurs).
      THIS-OBJECT:p3ResursSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p3ResursSC:SplitterDistance = 350.
      THIS-OBJECT:p3ResursSC:TabIndex = 1.
      /*  */
      /* p3ResursSC2 */
      /*  */
      THIS-OBJECT:p3ResursSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3ResursSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p3ResursSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3ResursSC2:Name = "p3ResursSC2".
      /*  */
      /* p3ResursSC2.Panel1 */
      /*  */
      THIS-OBJECT:p3ResursSC2:Panel1:Controls:Add(THIS-OBJECT:gridResursForP3).
      /*  */
      /* p3ResursSC2.Panel2 */
      /*  */
      THIS-OBJECT:p3ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP3ResursBort).
      THIS-OBJECT:p3ResursSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP3Resurs).
      THIS-OBJECT:p3ResursSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p3ResursSC2:SplitterDistance = 310.
      THIS-OBJECT:p3ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP3 */
      /*  */
      THIS-OBJECT:gridResursForP3:AllowDrop = TRUE.
      appearance177:BackColor = System.Drawing.SystemColors:Window.
      appearance177:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Appearance = appearance177.
      THIS-OBJECT:gridResursForP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance178:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance178:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance178:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance178:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:Appearance = appearance178.
      appearance179:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance179.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance180:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance180:BackColor2 = System.Drawing.SystemColors:Control.
      appearance180:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance180:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:PromptAppearance = appearance180.
      THIS-OBJECT:gridResursForP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance181:BackColor = System.Drawing.SystemColors:Window.
      appearance181:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveCellAppearance = appearance181.
      appearance182:BackColor = System.Drawing.SystemColors:Highlight.
      appearance182:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveRowAppearance = appearance182.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance183:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CardAreaAppearance = appearance183.
      appearance184:BorderColor = System.Drawing.Color:Silver.
      appearance184:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellAppearance = appearance184.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellPadding = 0.
      appearance185:BackColor = System.Drawing.SystemColors:Control.
      appearance185:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance185:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance185:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance185:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:GroupByRowAppearance = appearance185.
      appearance186:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderAppearance = appearance186.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance187:BackColor = System.Drawing.SystemColors:Window.
      appearance187:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:RowAppearance = appearance187.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance188:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance188.
      THIS-OBJECT:gridResursForP3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForP3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForP3:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForP3:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForP3:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForP3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForP3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForP3:GuruContext = ?.
      THIS-OBJECT:gridResursForP3:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursForP3:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursForP3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForP3:Name = "gridResursForP3".
      THIS-OBJECT:gridResursForP3:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridResursForP3:TabIndex = 1.
      THIS-OBJECT:gridResursForP3:Text = "grid1".
      THIS-OBJECT:gridResursForP3:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP3_GuruAfterDropped).
      /*  */
      /* buttonOverP3ResursBort */
      /*  */
      appearance189:Image = CAST(resources:GetObject("appearance189.Image"), System.Object).
      THIS-OBJECT:buttonOverP3ResursBort:Appearance = appearance189.
      THIS-OBJECT:buttonOverP3ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP3ResursBort:Name = "buttonOverP3ResursBort".
      THIS-OBJECT:buttonOverP3ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3ResursBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP3ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP3ResursBort_Click).
      /*  */
      /* buttonOverP3Resurs */
      /*  */
      appearance190:Image = CAST(resources:GetObject("appearance190.Image"), System.Object).
      THIS-OBJECT:buttonOverP3Resurs:Appearance = appearance190.
      THIS-OBJECT:buttonOverP3Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP3Resurs:Name = "buttonOverP3Resurs".
      THIS-OBJECT:buttonOverP3Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3Resurs:TabIndex = 2.
      THIS-OBJECT:buttonOverP3Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP3Resurs_Click).
      /*  */
      /* gridP3Resurs */
      /*  */
      THIS-OBJECT:gridP3Resurs:AllowDrop = TRUE.
      appearance191:BackColor = System.Drawing.SystemColors:Window.
      appearance191:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Appearance = appearance191.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance192:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance192:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance192:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance192:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:Appearance = appearance192.
      appearance193:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance193.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance194:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance194:BackColor2 = System.Drawing.SystemColors:Control.
      appearance194:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance194:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance194.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance195:BackColor = System.Drawing.SystemColors:Window.
      appearance195:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance195.
      appearance196:BackColor = System.Drawing.SystemColors:Highlight.
      appearance196:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance196.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance197:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CardAreaAppearance = appearance197.
      appearance198:BorderColor = System.Drawing.Color:Silver.
      appearance198:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellAppearance = appearance198.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance199:BackColor = System.Drawing.SystemColors:Control.
      appearance199:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance199:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance199:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance199:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance199.
      appearance200:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderAppearance = appearance200.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance201:BackColor = System.Drawing.SystemColors:Window.
      appearance201:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:RowAppearance = appearance201.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance202:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance202.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP3Resurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP3Resurs:GuruContext = ?.
      THIS-OBJECT:gridP3Resurs:GuruDraggable = TRUE.
      THIS-OBJECT:gridP3Resurs:GuruDroppable = TRUE.
      THIS-OBJECT:gridP3Resurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP3Resurs:Name = "gridP3Resurs".
      THIS-OBJECT:gridP3Resurs:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP3Resurs:TabIndex = 0.
      THIS-OBJECT:gridP3Resurs:Text = "grid1".
      THIS-OBJECT:gridP3Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP3Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP3Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP3resurs_AfterCellUpdate).
      /*  */
      /* TabP3Frekvens */
      /*  */
      THIS-OBJECT:TabP3Frekvens:Controls:Add(THIS-OBJECT:p3FrekvensSC).
      THIS-OBJECT:TabP3Frekvens:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP3Frekvens:Name = "TabP3Frekvens".
      THIS-OBJECT:TabP3Frekvens:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* p3FrekvensSC */
      /*  */
      THIS-OBJECT:p3FrekvensSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3FrekvensSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3FrekvensSC:Name = "p3FrekvensSC".
      /*  */
      /* p3FrekvensSC.Panel1 */
      /*  */
      THIS-OBJECT:p3FrekvensSC:Panel1:Controls:Add(THIS-OBJECT:p3FrekvensSC2).
      /*  */
      /* p3FrekvensSC.Panel2 */
      /*  */
      THIS-OBJECT:p3FrekvensSC:Panel2:Controls:Add(THIS-OBJECT:gridP3Frekvens).
      THIS-OBJECT:p3FrekvensSC:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:p3FrekvensSC:SplitterDistance = 350.
      THIS-OBJECT:p3FrekvensSC:TabIndex = 2.
      /*  */
      /* p3FrekvensSC2 */
      /*  */
      THIS-OBJECT:p3FrekvensSC2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3FrekvensSC2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:p3FrekvensSC2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3FrekvensSC2:Name = "p3FrekvensSC2".
      /*  */
      /* p3FrekvensSC2.Panel1 */
      /*  */
      THIS-OBJECT:p3FrekvensSC2:Panel1:Controls:Add(THIS-OBJECT:gridP4ForP3).
      /*  */
      /* p3FrekvensSC2.Panel2 */
      /*  */
      THIS-OBJECT:p3FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP3FrekvensBort).
      THIS-OBJECT:p3FrekvensSC2:Panel2:Controls:Add(THIS-OBJECT:buttonOverP3Frekvens).
      THIS-OBJECT:p3FrekvensSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p3FrekvensSC2:SplitterDistance = 309.
      THIS-OBJECT:p3FrekvensSC2:TabIndex = 0.
      /*  */
      /* gridP4ForP3 */
      /*  */
      THIS-OBJECT:gridP4ForP3:AllowDrop = TRUE.
      appearance203:BackColor = System.Drawing.SystemColors:Window.
      appearance203:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Appearance = appearance203.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance204:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance204:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance204:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance204:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:Appearance = appearance204.
      appearance205:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance205.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance206:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance206:BackColor2 = System.Drawing.SystemColors:Control.
      appearance206:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance206:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:PromptAppearance = appearance206.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance207:BackColor = System.Drawing.SystemColors:Window.
      appearance207:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveCellAppearance = appearance207.
      appearance208:BackColor = System.Drawing.SystemColors:Highlight.
      appearance208:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveRowAppearance = appearance208.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance209:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CardAreaAppearance = appearance209.
      appearance210:BorderColor = System.Drawing.Color:Silver.
      appearance210:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellAppearance = appearance210.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellPadding = 0.
      appearance211:BackColor = System.Drawing.SystemColors:Control.
      appearance211:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance211:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance211:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance211:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:GroupByRowAppearance = appearance211.
      appearance212:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderAppearance = appearance212.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance213:BackColor = System.Drawing.SystemColors:Window.
      appearance213:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:RowAppearance = appearance213.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance214:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance214.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP4ForP3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP4ForP3:GuruContext = ?.
      THIS-OBJECT:gridP4ForP3:GuruDraggable = TRUE.
      THIS-OBJECT:gridP4ForP3:GuruDroppable = TRUE.
      THIS-OBJECT:gridP4ForP3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP4ForP3:Name = "gridP4ForP3".
      THIS-OBJECT:gridP4ForP3:Size = NEW System.Drawing.Size(309, 284).
      THIS-OBJECT:gridP4ForP3:TabIndex = 2.
      THIS-OBJECT:gridP4ForP3:Text = "grid1".
      THIS-OBJECT:gridP4ForP3:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP4ForP3_GuruAfterDropped).
      /*  */
      /* buttonOverP3FrekvensBort */
      /*  */
      appearance215:Image = CAST(resources:GetObject("appearance215.Image"), System.Object).
      THIS-OBJECT:buttonOverP3FrekvensBort:Appearance = appearance215.
      THIS-OBJECT:buttonOverP3FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP3FrekvensBort:Name = "buttonOverP3FrekvensBort".
      THIS-OBJECT:buttonOverP3FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3FrekvensBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP3FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP3FrekvensBort_Click).
      /*  */
      /* buttonOverP3Frekvens */
      /*  */
      appearance216:Image = CAST(resources:GetObject("appearance216.Image"), System.Object).
      THIS-OBJECT:buttonOverP3Frekvens:Appearance = appearance216.
      THIS-OBJECT:buttonOverP3Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP3Frekvens:Name = "buttonOverP3Frekvens".
      THIS-OBJECT:buttonOverP3Frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3Frekvens:TabIndex = 4.
      THIS-OBJECT:buttonOverP3Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP3Frekvens_Click).
      /*  */
      /* gridP3Frekvens */
      /*  */
      THIS-OBJECT:gridP3Frekvens:AllowDrop = TRUE.
      appearance217:BackColor = System.Drawing.SystemColors:Window.
      appearance217:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Appearance = appearance217.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance218:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance218:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance218:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance218:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:Appearance = appearance218.
      appearance219:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance219.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance220:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance220:BackColor2 = System.Drawing.SystemColors:Control.
      appearance220:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance220:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance220.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance221:BackColor = System.Drawing.SystemColors:Window.
      appearance221:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance221.
      appearance222:BackColor = System.Drawing.SystemColors:Highlight.
      appearance222:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance222.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance223:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance223.
      appearance224:BorderColor = System.Drawing.Color:Silver.
      appearance224:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellAppearance = appearance224.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance225:BackColor = System.Drawing.SystemColors:Control.
      appearance225:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance225:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance225:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance225:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance225.
      appearance226:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderAppearance = appearance226.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance227:BackColor = System.Drawing.SystemColors:Window.
      appearance227:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:RowAppearance = appearance227.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance228:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance228.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP3Frekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP3Frekvens:GuruContext = ?.
      THIS-OBJECT:gridP3Frekvens:GuruDraggable = TRUE.
      THIS-OBJECT:gridP3Frekvens:GuruDroppable = TRUE.
      THIS-OBJECT:gridP3Frekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP3Frekvens:Name = "gridP3Frekvens".
      THIS-OBJECT:gridP3Frekvens:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP3Frekvens:TabIndex = 1.
      THIS-OBJECT:gridP3Frekvens:Text = "grid1".
      THIS-OBJECT:gridP3Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP3Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP3Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP3frekvens_AfterCellUpdate).
      /*  */
      /* TabP3Import */
      /*  */
      THIS-OBJECT:TabP3Import:Controls:Add(THIS-OBJECT:splitContainerP3ImportGrund).
      THIS-OBJECT:TabP3Import:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabP3Import:Name = "TabP3Import".
      THIS-OBJECT:TabP3Import:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* splitContainerP3ImportGrund */
      /*  */
      THIS-OBJECT:splitContainerP3ImportGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP3ImportGrund:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerP3ImportGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP3ImportGrund:Name = "splitContainerP3ImportGrund".
      THIS-OBJECT:splitContainerP3ImportGrund:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerP3ImportGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP3ImportGrund:Panel1:Controls:Add(THIS-OBJECT:P3ImportStrip).
      /*  */
      /* splitContainerP3ImportGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerP3ImportGrund:Panel2:Controls:Add(THIS-OBJECT:splitContainerP3Import).
      THIS-OBJECT:splitContainerP3ImportGrund:Size = NEW System.Drawing.Size(1141, 284).
      THIS-OBJECT:splitContainerP3ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP3ImportGrund:TabIndex = 3.
      /*  */
      /* P3ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar5[1] = THIS-OBJECT:StripButtonP3Import.
      arrayvar5[2] = THIS-OBJECT:StripButtonP3Rin.
      arrayvar5[3] = THIS-OBJECT:StripButtonP3Rbort.
      THIS-OBJECT:P3ImportStrip:Items:AddRange(arrayvar5).
      THIS-OBJECT:P3ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P3ImportStrip:Name = "P3ImportStrip".
      THIS-OBJECT:P3ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P3ImportStrip:TabIndex = 0.
      THIS-OBJECT:P3ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP3Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar6[1] = THIS-OBJECT:p3KodToolStripMenuItem.
      arrayvar6[2] = THIS-OBJECT:p3FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP3Import:DropDownItems:AddRange(arrayvar6).
      THIS-OBJECT:StripButtonP3Import:Image = CAST(resources:GetObject("StripButtonP3Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP3Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP3Import:Name = "StripButtonP3Import".
      THIS-OBJECT:StripButtonP3Import:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:StripButtonP3Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP3Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p3KodToolStripMenuItem */
      /*  */
      THIS-OBJECT:p3KodToolStripMenuItem:Image = CAST(resources:GetObject("p3KodToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p3KodToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p3KodToolStripMenuItem:Name = "p3KodToolStripMenuItem".
      THIS-OBJECT:p3KodToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p3KodToolStripMenuItem:Text = "P3 Kod".
      THIS-OBJECT:p3KodToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p3KoderToolStripMenuItem_Click).
      /*  */
      /* p3FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p3FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p3FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p3FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p3FrekvensToolStripMenuItem:Name = "p3FrekvensToolStripMenuItem".
      THIS-OBJECT:p3FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p3FrekvensToolStripMenuItem:Text = "P3 Frekvens".
      THIS-OBJECT:p3FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p3FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP3Rin */
      /*  */
      THIS-OBJECT:StripButtonP3Rin:Image = CAST(resources:GetObject("StripButtonP3Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP3Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP3Rin:Name = "StripButtonP3Rin".
      THIS-OBJECT:StripButtonP3Rin:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:StripButtonP3Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP3Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP3Rin:Click:Subscribe(THIS-OBJECT:StripButtonP3Rin_Click).
      /*  */
      /* StripButtonP3Rbort */
      /*  */
      THIS-OBJECT:StripButtonP3Rbort:Image = CAST(resources:GetObject("StripButtonP3Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP3Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP3Rbort:Name = "StripButtonP3Rbort".
      THIS-OBJECT:StripButtonP3Rbort:Size = NEW System.Drawing.Size(163, 22).
      THIS-OBJECT:StripButtonP3Rbort:Text = "Ta bort markerade poster ".
      THIS-OBJECT:StripButtonP3Rbort:Visible = FALSE.
      THIS-OBJECT:StripButtonP3Rbort:Click:Subscribe(THIS-OBJECT:StripButtonP3Rbort_Click).
      /*  */
      /* splitContainerP3Import */
      /*  */
      THIS-OBJECT:splitContainerP3Import:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerP3Import:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerP3Import:Name = "splitContainerP3Import".
      /*  */
      /* splitContainerP3Import.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerP3Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP3F).
      THIS-OBJECT:splitContainerP3Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP3).
      THIS-OBJECT:splitContainerP3Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP3Import:SplitterDistance = 380.
      THIS-OBJECT:splitContainerP3Import:TabIndex = 0.
      /*  */
      /* GridImportP3F */
      /*  */
      appearance229:BackColor = System.Drawing.SystemColors:Window.
      appearance229:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Appearance = appearance229.
      THIS-OBJECT:GridImportP3F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP3F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance230:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance230:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance230:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance230:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:Appearance = appearance230.
      appearance231:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance231.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance232:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance232:BackColor2 = System.Drawing.SystemColors:Control.
      appearance232:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance232:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:PromptAppearance = appearance232.
      THIS-OBJECT:GridImportP3F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP3F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance233:BackColor = System.Drawing.SystemColors:Window.
      appearance233:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveCellAppearance = appearance233.
      appearance234:BackColor = System.Drawing.SystemColors:Highlight.
      appearance234:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveRowAppearance = appearance234.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance235:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CardAreaAppearance = appearance235.
      appearance236:BorderColor = System.Drawing.Color:Silver.
      appearance236:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellAppearance = appearance236.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellPadding = 0.
      appearance237:BackColor = System.Drawing.SystemColors:Control.
      appearance237:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance237:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance237:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance237:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:GroupByRowAppearance = appearance237.
      appearance238:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderAppearance = appearance238.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance239:BackColor = System.Drawing.SystemColors:Window.
      appearance239:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:RowAppearance = appearance239.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance240:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:TemplateAddRowAppearance = appearance240.
      THIS-OBJECT:GridImportP3F:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP3F:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP3F:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP3F:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP3F:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP3F:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP3F:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP3F:GuruContext = ?.
      THIS-OBJECT:GridImportP3F:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP3F:Name = "GridImportP3F".
      THIS-OBJECT:GridImportP3F:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP3F:TabIndex = 1.
      THIS-OBJECT:GridImportP3F:Text = "grid1".
      /*  */
      /* GridImportP3 */
      /*  */
      appearance241:BackColor = System.Drawing.SystemColors:Window.
      appearance241:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP3:DisplayLayout:Appearance = appearance241.
      THIS-OBJECT:GridImportP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance242:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance242:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance242:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance242:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:Appearance = appearance242.
      appearance243:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance243.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance244:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance244:BackColor2 = System.Drawing.SystemColors:Control.
      appearance244:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance244:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:PromptAppearance = appearance244.
      THIS-OBJECT:GridImportP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance245:BackColor = System.Drawing.SystemColors:Window.
      appearance245:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveCellAppearance = appearance245.
      appearance246:BackColor = System.Drawing.SystemColors:Highlight.
      appearance246:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveRowAppearance = appearance246.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance247:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CardAreaAppearance = appearance247.
      appearance248:BorderColor = System.Drawing.Color:Silver.
      appearance248:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellAppearance = appearance248.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellPadding = 0.
      appearance249:BackColor = System.Drawing.SystemColors:Control.
      appearance249:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance249:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance249:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance249:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:GroupByRowAppearance = appearance249.
      appearance250:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderAppearance = appearance250.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance251:BackColor = System.Drawing.SystemColors:Window.
      appearance251:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:RowAppearance = appearance251.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance252:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance252.
      THIS-OBJECT:GridImportP3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridImportP3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridImportP3:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridImportP3:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridImportP3:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridImportP3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridImportP3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridImportP3:GuruContext = ?.
      THIS-OBJECT:GridImportP3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridImportP3:Name = "GridImportP3".
      THIS-OBJECT:GridImportP3:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP3:TabIndex = 0.
      THIS-OBJECT:GridImportP3:Text = "grid1".
      /*  */
      /* TabP2resurs */
      /*  */
      THIS-OBJECT:TabP2resurs:Controls:Add(THIS-OBJECT:p2ResursSC).
      THIS-OBJECT:TabP2resurs:Location = NEW System.Drawing.Point(-10000, -10000).
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
      THIS-OBJECT:p2ResursSC:SplitterDistance = 350.
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
      THIS-OBJECT:p2ResursSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p2ResursSC2:SplitterDistance = 310.
      THIS-OBJECT:p2ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP2 */
      /*  */
      THIS-OBJECT:gridResursForP2:AllowDrop = TRUE.
      appearance253:BackColor = System.Drawing.SystemColors:Window.
      appearance253:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Appearance = appearance253.
      THIS-OBJECT:gridResursForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance254:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance254:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance254:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance254:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:Appearance = appearance254.
      appearance255:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance255.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance256:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance256:BackColor2 = System.Drawing.SystemColors:Control.
      appearance256:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance256:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance256.
      THIS-OBJECT:gridResursForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance257:BackColor = System.Drawing.SystemColors:Window.
      appearance257:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveCellAppearance = appearance257.
      appearance258:BackColor = System.Drawing.SystemColors:Highlight.
      appearance258:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:ActiveRowAppearance = appearance258.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance259:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CardAreaAppearance = appearance259.
      appearance260:BorderColor = System.Drawing.Color:Silver.
      appearance260:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellAppearance = appearance260.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:CellPadding = 0.
      appearance261:BackColor = System.Drawing.SystemColors:Control.
      appearance261:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance261:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance261:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance261:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:GroupByRowAppearance = appearance261.
      appearance262:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderAppearance = appearance262.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance263:BackColor = System.Drawing.SystemColors:Window.
      appearance263:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:RowAppearance = appearance263.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance264:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance264.
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
      THIS-OBJECT:gridResursForP2:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridResursForP2:TabIndex = 1.
      THIS-OBJECT:gridResursForP2:Text = "grid1".
      THIS-OBJECT:gridResursForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2resursBort */
      /*  */
      appearance265:Image = CAST(resources:GetObject("appearance265.Image"), System.Object).
      THIS-OBJECT:buttonOverP2resursBort:Appearance = appearance265.
      THIS-OBJECT:buttonOverP2resursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2resursBort:Name = "buttonOverP2resursBort".
      THIS-OBJECT:buttonOverP2resursBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2resursBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2resursBort:Click:Subscribe(THIS-OBJECT:buttonOverP2ResursBort_Click).
      /*  */
      /* buttonOverP2Resurs */
      /*  */
      appearance266:Image = CAST(resources:GetObject("appearance266.Image"), System.Object).
      THIS-OBJECT:buttonOverP2Resurs:Appearance = appearance266.
      THIS-OBJECT:buttonOverP2Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP2Resurs:Name = "buttonOverP2Resurs".
      THIS-OBJECT:buttonOverP2Resurs:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2Resurs:TabIndex = 2.
      THIS-OBJECT:buttonOverP2Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP2Resurs_Click).
      /*  */
      /* gridP2Resurs */
      /*  */
      THIS-OBJECT:gridP2Resurs:AllowDrop = TRUE.
      appearance267:BackColor = System.Drawing.SystemColors:Window.
      appearance267:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Appearance = appearance267.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance268:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance268:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance268:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance268:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:Appearance = appearance268.
      appearance269:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance269.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance270:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance270:BackColor2 = System.Drawing.SystemColors:Control.
      appearance270:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance270:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance270.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance271:BackColor = System.Drawing.SystemColors:Window.
      appearance271:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance271.
      appearance272:BackColor = System.Drawing.SystemColors:Highlight.
      appearance272:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance272.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance273:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CardAreaAppearance = appearance273.
      appearance274:BorderColor = System.Drawing.Color:Silver.
      appearance274:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellAppearance = appearance274.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance275:BackColor = System.Drawing.SystemColors:Control.
      appearance275:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance275:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance275:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance275:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance275.
      appearance276:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderAppearance = appearance276.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance277:BackColor = System.Drawing.SystemColors:Window.
      appearance277:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:RowAppearance = appearance277.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance278:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance278.
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
      THIS-OBJECT:gridP2Resurs:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP2Resurs:TabIndex = 0.
      THIS-OBJECT:gridP2Resurs:Text = "grid1".
      THIS-OBJECT:gridP2Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP2Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP2resurs_AfterCellUpdate).
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
      THIS-OBJECT:p2FrekvensSC:SplitterDistance = 350.
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
      THIS-OBJECT:p2FrekvensSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p2FrekvensSC2:SplitterDistance = 310.
      THIS-OBJECT:p2FrekvensSC2:TabIndex = 0.
      /*  */
      /* gridP3ForP2 */
      /*  */
      THIS-OBJECT:gridP3ForP2:AllowDrop = TRUE.
      appearance279:BackColor = System.Drawing.SystemColors:Window.
      appearance279:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Appearance = appearance279.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance280:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance280:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance280:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance280:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:Appearance = appearance280.
      appearance281:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance281.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance282:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance282:BackColor2 = System.Drawing.SystemColors:Control.
      appearance282:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance282:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance282.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance283:BackColor = System.Drawing.SystemColors:Window.
      appearance283:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveCellAppearance = appearance283.
      appearance284:BackColor = System.Drawing.SystemColors:Highlight.
      appearance284:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:ActiveRowAppearance = appearance284.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance285:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CardAreaAppearance = appearance285.
      appearance286:BorderColor = System.Drawing.Color:Silver.
      appearance286:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellAppearance = appearance286.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:CellPadding = 0.
      appearance287:BackColor = System.Drawing.SystemColors:Control.
      appearance287:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance287:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance287:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance287:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:GroupByRowAppearance = appearance287.
      appearance288:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderAppearance = appearance288.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance289:BackColor = System.Drawing.SystemColors:Window.
      appearance289:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:RowAppearance = appearance289.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance290:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3ForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance290.
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
      THIS-OBJECT:gridP3ForP2:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridP3ForP2:TabIndex = 1.
      THIS-OBJECT:gridP3ForP2:Text = "grid1".
      THIS-OBJECT:gridP3ForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP3ForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2FrekvensBort */
      /*  */
      appearance291:Image = CAST(resources:GetObject("appearance291.Image"), System.Object).
      THIS-OBJECT:buttonOverP2FrekvensBort:Appearance = appearance291.
      THIS-OBJECT:buttonOverP2FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2FrekvensBort:Name = "buttonOverP2FrekvensBort".
      THIS-OBJECT:buttonOverP2FrekvensBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2FrekvensBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP2FrekvensBort_Click).
      /*  */
      /* buttonOverP2Frekvens */
      /*  */
      appearance292:Image = CAST(resources:GetObject("appearance292.Image"), System.Object).
      THIS-OBJECT:buttonOverP2Frekvens:Appearance = appearance292.
      THIS-OBJECT:buttonOverP2Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP2Frekvens:Name = "buttonOverP2Frekvens".
      THIS-OBJECT:buttonOverP2Frekvens:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2Frekvens:TabIndex = 2.
      THIS-OBJECT:buttonOverP2Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP2Frekvens_Click).
      /*  */
      /* gridP2Frekvens */
      /*  */
      THIS-OBJECT:gridP2Frekvens:AllowDrop = TRUE.
      appearance293:BackColor = System.Drawing.SystemColors:Window.
      appearance293:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Appearance = appearance293.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance294:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance294:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance294:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance294:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:Appearance = appearance294.
      appearance295:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance295.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance296:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance296:BackColor2 = System.Drawing.SystemColors:Control.
      appearance296:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance296:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance296.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance297:BackColor = System.Drawing.SystemColors:Window.
      appearance297:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance297.
      appearance298:BackColor = System.Drawing.SystemColors:Highlight.
      appearance298:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance298.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance299:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance299.
      appearance300:BorderColor = System.Drawing.Color:Silver.
      appearance300:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellAppearance = appearance300.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance301:BackColor = System.Drawing.SystemColors:Control.
      appearance301:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance301:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance301:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance301:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance301.
      appearance302:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderAppearance = appearance302.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance303:BackColor = System.Drawing.SystemColors:Window.
      appearance303:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:RowAppearance = appearance303.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance304:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance304.
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
      THIS-OBJECT:gridP2Frekvens:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP2Frekvens:TabIndex = 1.
      THIS-OBJECT:gridP2Frekvens:Text = "grid1".
      THIS-OBJECT:gridP2Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP2Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP2frekvens_AfterCellUpdate).
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
      THIS-OBJECT:p2materielSC:SplitterDistance = 350.
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
      THIS-OBJECT:p2materielSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p2materielSC2:SplitterDistance = 309.
      THIS-OBJECT:p2materielSC2:TabIndex = 0.
      /*  */
      /* gridMtrlForP2 */
      /*  */
      THIS-OBJECT:gridMtrlForP2:AllowDrop = TRUE.
      appearance305:BackColor = System.Drawing.SystemColors:Window.
      appearance305:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Appearance = appearance305.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance306:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance306:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance306:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance306:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:Appearance = appearance306.
      appearance307:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance307.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance308:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance308:BackColor2 = System.Drawing.SystemColors:Control.
      appearance308:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance308:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:GroupByBox:PromptAppearance = appearance308.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance309:BackColor = System.Drawing.SystemColors:Window.
      appearance309:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveCellAppearance = appearance309.
      appearance310:BackColor = System.Drawing.SystemColors:Highlight.
      appearance310:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:ActiveRowAppearance = appearance310.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance311:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CardAreaAppearance = appearance311.
      appearance312:BorderColor = System.Drawing.Color:Silver.
      appearance312:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellAppearance = appearance312.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:CellPadding = 0.
      appearance313:BackColor = System.Drawing.SystemColors:Control.
      appearance313:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance313:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance313:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance313:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:GroupByRowAppearance = appearance313.
      appearance314:TextHAlignAsString = "Left".
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderAppearance = appearance314.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance315:BackColor = System.Drawing.SystemColors:Window.
      appearance315:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:RowAppearance = appearance315.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance316:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridMtrlForP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance316.
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
      THIS-OBJECT:gridMtrlForP2:Size = NEW System.Drawing.Size(309, 284).
      THIS-OBJECT:gridMtrlForP2:TabIndex = 1.
      THIS-OBJECT:gridMtrlForP2:Text = "grid1".
      THIS-OBJECT:gridMtrlForP2:GuruAfterDropped:Subscribe(THIS-OBJECT:gridMtrlForP2_GuruAfterDropped).
      /*  */
      /* buttonOverP2MtrlBort */
      /*  */
      appearance317:Image = CAST(resources:GetObject("appearance317.Image"), System.Object).
      THIS-OBJECT:buttonOverP2MtrlBort:Appearance = appearance317.
      THIS-OBJECT:buttonOverP2MtrlBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP2MtrlBort:Name = "buttonOverP2MtrlBort".
      THIS-OBJECT:buttonOverP2MtrlBort:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverP2MtrlBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP2MtrlBort:Click:Subscribe(THIS-OBJECT:buttonOverp2MtrlBort_Click).
      /*  */
      /* buttonOverp2Mtrl */
      /*  */
      appearance318:Image = CAST(resources:GetObject("appearance318.Image"), System.Object).
      THIS-OBJECT:buttonOverp2Mtrl:Appearance = appearance318.
      THIS-OBJECT:buttonOverp2Mtrl:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverp2Mtrl:Name = "buttonOverp2Mtrl".
      THIS-OBJECT:buttonOverp2Mtrl:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:buttonOverp2Mtrl:TabIndex = 2.
      THIS-OBJECT:buttonOverp2Mtrl:Click:Subscribe(THIS-OBJECT:buttonOverp2Mtrl_Click).
      /*  */
      /* gridP2Mtrl */
      /*  */
      THIS-OBJECT:gridP2Mtrl:AllowDrop = TRUE.
      appearance319:BackColor = System.Drawing.SystemColors:Window.
      appearance319:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Appearance = appearance319.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance320:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance320:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance320:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance320:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:Appearance = appearance320.
      appearance321:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance321.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance322:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance322:BackColor2 = System.Drawing.SystemColors:Control.
      appearance322:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance322:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance322.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance323:BackColor = System.Drawing.SystemColors:Window.
      appearance323:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveCellAppearance = appearance323.
      appearance324:BackColor = System.Drawing.SystemColors:Highlight.
      appearance324:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:ActiveRowAppearance = appearance324.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance325:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CardAreaAppearance = appearance325.
      appearance326:BorderColor = System.Drawing.Color:Silver.
      appearance326:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellAppearance = appearance326.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:CellPadding = 0.
      appearance327:BackColor = System.Drawing.SystemColors:Control.
      appearance327:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance327:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance327:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance327:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:GroupByRowAppearance = appearance327.
      appearance328:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderAppearance = appearance328.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance329:BackColor = System.Drawing.SystemColors:Window.
      appearance329:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:RowAppearance = appearance329.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance330:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2Mtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance330.
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
      THIS-OBJECT:gridP2Mtrl:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP2Mtrl:TabIndex = 1.
      THIS-OBJECT:gridP2Mtrl:Text = "grid1".
      THIS-OBJECT:gridP2Mtrl:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2Mtrl_GuruAfterDropped).
      THIS-OBJECT:gridP2Mtrl:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP2mtrl_AfterCellUpdate).
      /*  */
      /* TabP2Import */
      /*  */
      THIS-OBJECT:TabP2Import:Controls:Add(THIS-OBJECT:splitContainerP2ImportGrund).
      THIS-OBJECT:TabP2Import:Location = NEW System.Drawing.Point(1, 32).
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
      DEFINE VARIABLE arrayvar7 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar7[1] = THIS-OBJECT:StripButtonP2Import.
      arrayvar7[2] = THIS-OBJECT:StripButtonP2Rin.
      arrayvar7[3] = THIS-OBJECT:StripButtonP2Rbort.
      THIS-OBJECT:P2ImportStrip:Items:AddRange(arrayvar7).
      THIS-OBJECT:P2ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P2ImportStrip:Name = "P2ImportStrip".
      THIS-OBJECT:P2ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P2ImportStrip:TabIndex = 0.
      THIS-OBJECT:P2ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP2Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar8 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar8[1] = THIS-OBJECT:p2KoderToolStripMenuItem.
      arrayvar8[2] = THIS-OBJECT:p2FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP2Import:DropDownItems:AddRange(arrayvar8).
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
      THIS-OBJECT:p2KoderToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p2KoderToolStripMenuItem:Text = "P2 Kod".
      THIS-OBJECT:p2KoderToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p2KoderToolStripMenuItem_Click).
      /*  */
      /* p2FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p2FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p2FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Name = "p2FrekvensToolStripMenuItem".
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Text = "P2 Frekvens".
      THIS-OBJECT:p2FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p2FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP2Rin */
      /*  */
      THIS-OBJECT:StripButtonP2Rin:Image = CAST(resources:GetObject("StripButtonP2Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Rin:Name = "StripButtonP2Rin".
      THIS-OBJECT:StripButtonP2Rin:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:StripButtonP2Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP2Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP2Rin:Click:Subscribe(THIS-OBJECT:StripButtonP2Rin_Click).
      /*  */
      /* StripButtonP2Rbort */
      /*  */
      THIS-OBJECT:StripButtonP2Rbort:Image = CAST(resources:GetObject("StripButtonP2Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP2Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP2Rbort:Name = "StripButtonP2Rbort".
      THIS-OBJECT:StripButtonP2Rbort:Size = NEW System.Drawing.Size(163, 22).
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
      THIS-OBJECT:splitContainerP2Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP2F).
      THIS-OBJECT:splitContainerP2Import:Panel1:Controls:Add(THIS-OBJECT:GridImportP2).
      THIS-OBJECT:splitContainerP2Import:Size = NEW System.Drawing.Size(1141, 255).
      THIS-OBJECT:splitContainerP2Import:SplitterDistance = 380.
      THIS-OBJECT:splitContainerP2Import:TabIndex = 0.
      /*  */
      /* GridImportP2F */
      /*  */
      appearance331:BackColor = System.Drawing.SystemColors:Window.
      appearance331:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Appearance = appearance331.
      THIS-OBJECT:GridImportP2F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP2F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance332:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance332:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance332:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance332:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:Appearance = appearance332.
      appearance333:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance333.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance334:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance334:BackColor2 = System.Drawing.SystemColors:Control.
      appearance334:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance334:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:GroupByBox:PromptAppearance = appearance334.
      THIS-OBJECT:GridImportP2F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP2F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance335:BackColor = System.Drawing.SystemColors:Window.
      appearance335:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveCellAppearance = appearance335.
      appearance336:BackColor = System.Drawing.SystemColors:Highlight.
      appearance336:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:ActiveRowAppearance = appearance336.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance337:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CardAreaAppearance = appearance337.
      appearance338:BorderColor = System.Drawing.Color:Silver.
      appearance338:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellAppearance = appearance338.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:CellPadding = 0.
      appearance339:BackColor = System.Drawing.SystemColors:Control.
      appearance339:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance339:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance339:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance339:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:GroupByRowAppearance = appearance339.
      appearance340:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderAppearance = appearance340.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance341:BackColor = System.Drawing.SystemColors:Window.
      appearance341:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:RowAppearance = appearance341.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance342:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP2F:DisplayLayout:Override:TemplateAddRowAppearance = appearance342.
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
      THIS-OBJECT:GridImportP2F:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP2F:TabIndex = 1.
      THIS-OBJECT:GridImportP2F:Text = "grid1".
      /*  */
      /* GridImportP2 */
      /*  */
      appearance343:BackColor = System.Drawing.SystemColors:Window.
      appearance343:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP2:DisplayLayout:Appearance = appearance343.
      THIS-OBJECT:GridImportP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance344:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance344:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance344:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance344:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:Appearance = appearance344.
      appearance345:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance345.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance346:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance346:BackColor2 = System.Drawing.SystemColors:Control.
      appearance346:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance346:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP2:DisplayLayout:GroupByBox:PromptAppearance = appearance346.
      THIS-OBJECT:GridImportP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance347:BackColor = System.Drawing.SystemColors:Window.
      appearance347:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveCellAppearance = appearance347.
      appearance348:BackColor = System.Drawing.SystemColors:Highlight.
      appearance348:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:ActiveRowAppearance = appearance348.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance349:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CardAreaAppearance = appearance349.
      appearance350:BorderColor = System.Drawing.Color:Silver.
      appearance350:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellAppearance = appearance350.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:CellPadding = 0.
      appearance351:BackColor = System.Drawing.SystemColors:Control.
      appearance351:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance351:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance351:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance351:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:GroupByRowAppearance = appearance351.
      appearance352:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderAppearance = appearance352.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance353:BackColor = System.Drawing.SystemColors:Window.
      appearance353:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:RowAppearance = appearance353.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance354:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance354.
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
      THIS-OBJECT:GridImportP2:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP2:TabIndex = 0.
      THIS-OBJECT:GridImportP2:Text = "grid1".
      /*  */
      /* TabP1frekvens */
      /*  */
      THIS-OBJECT:TabP1frekvens:Controls:Add(THIS-OBJECT:p1FrekvensSC).
      THIS-OBJECT:TabP1frekvens:Location = NEW System.Drawing.Point(-10000, -10000).
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
      THIS-OBJECT:p1FrekvensSC:SplitterDistance = 350.
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
      THIS-OBJECT:p1frekvensSC2:Size = NEW System.Drawing.Size(350, 284).
      THIS-OBJECT:p1frekvensSC2:SplitterDistance = 310.
      THIS-OBJECT:p1frekvensSC2:TabIndex = 0.
      /*  */
      /* gridP2forP1 */
      /*  */
      appearance355:BackColor = System.Drawing.SystemColors:Window.
      appearance355:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Appearance = appearance355.
      THIS-OBJECT:gridP2forP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2forP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance356:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance356:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance356:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance356:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:Appearance = appearance356.
      appearance357:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance357.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance358:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance358:BackColor2 = System.Drawing.SystemColors:Control.
      appearance358:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance358:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:GroupByBox:PromptAppearance = appearance358.
      THIS-OBJECT:gridP2forP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2forP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance359:BackColor = System.Drawing.SystemColors:Window.
      appearance359:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveCellAppearance = appearance359.
      appearance360:BackColor = System.Drawing.SystemColors:Highlight.
      appearance360:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:ActiveRowAppearance = appearance360.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance361:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CardAreaAppearance = appearance361.
      appearance362:BorderColor = System.Drawing.Color:Silver.
      appearance362:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellAppearance = appearance362.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:CellPadding = 0.
      appearance363:BackColor = System.Drawing.SystemColors:Control.
      appearance363:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance363:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance363:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance363:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:GroupByRowAppearance = appearance363.
      appearance364:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderAppearance = appearance364.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance365:BackColor = System.Drawing.SystemColors:Window.
      appearance365:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:RowAppearance = appearance365.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance366:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2forP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance366.
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
      appearance367:Image = CAST(resources:GetObject("appearance367.Image"), System.Object).
      THIS-OBJECT:buttonOverP1FrekvensBort:Appearance = appearance367.
      THIS-OBJECT:buttonOverP1FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP1FrekvensBort:Name = "buttonOverP1FrekvensBort".
      THIS-OBJECT:buttonOverP1FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1FrekvensBort:TabIndex = 1.
      THIS-OBJECT:buttonOverP1FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP1FrekvensBort_Click).
      /*  */
      /* buttonOverP1frekvens */
      /*  */
      appearance368:Image = CAST(resources:GetObject("appearance368.Image"), System.Object).
      THIS-OBJECT:buttonOverP1frekvens:Appearance = appearance368.
      THIS-OBJECT:buttonOverP1frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP1frekvens:Name = "buttonOverP1frekvens".
      THIS-OBJECT:buttonOverP1frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1frekvens:TabIndex = 0.
      THIS-OBJECT:buttonOverP1frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP1frekvens_Click).
      /*  */
      /* gridP1Frekvens */
      /*  */
      appearance369:BackColor = System.Drawing.SystemColors:Window.
      appearance369:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Appearance = appearance369.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance370:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance370:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance370:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance370:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:Appearance = appearance370.
      appearance371:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance371.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance372:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance372:BackColor2 = System.Drawing.SystemColors:Control.
      appearance372:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance372:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance372.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance373:BackColor = System.Drawing.SystemColors:Window.
      appearance373:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance373.
      appearance374:BackColor = System.Drawing.SystemColors:Highlight.
      appearance374:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance374.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance375:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance375.
      appearance376:BorderColor = System.Drawing.Color:Silver.
      appearance376:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellAppearance = appearance376.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance377:BackColor = System.Drawing.SystemColors:Control.
      appearance377:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance377:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance377:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance377:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance377.
      appearance378:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderAppearance = appearance378.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance379:BackColor = System.Drawing.SystemColors:Window.
      appearance379:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowAppearance = appearance379.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance380:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance380.
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
      THIS-OBJECT:gridP1Frekvens:Size = NEW System.Drawing.Size(787, 284).
      THIS-OBJECT:gridP1Frekvens:TabIndex = 0.
      THIS-OBJECT:gridP1Frekvens:Text = "grid1".
      THIS-OBJECT:gridP1Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP1Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP1Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP1frekvens_AfterCellUpdate).
      /*  */
      /* TabP1Import */
      /*  */
      THIS-OBJECT:TabP1Import:Controls:Add(THIS-OBJECT:splitContainerP1ImportGrund).
      THIS-OBJECT:TabP1Import:Location = NEW System.Drawing.Point(1, 32).
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
      DEFINE VARIABLE arrayvar9 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar9[1] = THIS-OBJECT:StripButtonP1Import.
      arrayvar9[2] = THIS-OBJECT:StripButtonP1Rin.
      arrayvar9[3] = THIS-OBJECT:StripButtonP1Rbort.
      THIS-OBJECT:P1ImportStrip:Items:AddRange(arrayvar9).
      THIS-OBJECT:P1ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P1ImportStrip:Name = "P1ImportStrip".
      THIS-OBJECT:P1ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P1ImportStrip:TabIndex = 0.
      THIS-OBJECT:P1ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP1Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar10 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar10[1] = THIS-OBJECT:p1KoderToolStripMenuItem.
      arrayvar10[2] = THIS-OBJECT:p1FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP1Import:DropDownItems:AddRange(arrayvar10).
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
      THIS-OBJECT:p1KoderToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p1KoderToolStripMenuItem:Text = "P1 Kod".
      THIS-OBJECT:p1KoderToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1KoderToolStripMenuItem_Click).
      /*  */
      /* p1FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Image = CAST(resources:GetObject("p1FrekvensToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:p1FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Name = "p1FrekvensToolStripMenuItem".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(186, 56).
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Text = "P1 Frekvens".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP1Rin */
      /*  */
      THIS-OBJECT:StripButtonP1Rin:Image = CAST(resources:GetObject("StripButtonP1Rin.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rin:Name = "StripButtonP1Rin".
      THIS-OBJECT:StripButtonP1Rin:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:StripButtonP1Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP1Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP1Rin:Click:Subscribe(THIS-OBJECT:StripButtonP1Rin_Click).
      /*  */
      /* StripButtonP1Rbort */
      /*  */
      THIS-OBJECT:StripButtonP1Rbort:Image = CAST(resources:GetObject("StripButtonP1Rbort.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP1Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rbort:Name = "StripButtonP1Rbort".
      THIS-OBJECT:StripButtonP1Rbort:Size = NEW System.Drawing.Size(163, 22).
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
      THIS-OBJECT:splitContainerP1Import:SplitterDistance = 380.
      THIS-OBJECT:splitContainerP1Import:TabIndex = 0.
      /*  */
      /* GridImportP1F */
      /*  */
      appearance381:BackColor = System.Drawing.SystemColors:Window.
      appearance381:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Appearance = appearance381.
      THIS-OBJECT:GridImportP1F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance382:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance382:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance382:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance382:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:Appearance = appearance382.
      appearance383:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance383.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance384:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance384:BackColor2 = System.Drawing.SystemColors:Control.
      appearance384:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance384:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:PromptAppearance = appearance384.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance385:BackColor = System.Drawing.SystemColors:Window.
      appearance385:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveCellAppearance = appearance385.
      appearance386:BackColor = System.Drawing.SystemColors:Highlight.
      appearance386:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveRowAppearance = appearance386.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance387:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CardAreaAppearance = appearance387.
      appearance388:BorderColor = System.Drawing.Color:Silver.
      appearance388:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellAppearance = appearance388.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellPadding = 0.
      appearance389:BackColor = System.Drawing.SystemColors:Control.
      appearance389:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance389:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance389:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance389:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:GroupByRowAppearance = appearance389.
      appearance390:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderAppearance = appearance390.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance391:BackColor = System.Drawing.SystemColors:Window.
      appearance391:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowAppearance = appearance391.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance392:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:TemplateAddRowAppearance = appearance392.
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
      THIS-OBJECT:GridImportP1F:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP1F:TabIndex = 1.
      THIS-OBJECT:GridImportP1F:Text = "grid1".
      /*  */
      /* GridImportP1 */
      /*  */
      appearance393:BackColor = System.Drawing.SystemColors:Window.
      appearance393:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1:DisplayLayout:Appearance = appearance393.
      THIS-OBJECT:GridImportP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance394:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance394:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance394:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance394:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:Appearance = appearance394.
      appearance395:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance395.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance396:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance396:BackColor2 = System.Drawing.SystemColors:Control.
      appearance396:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance396:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:PromptAppearance = appearance396.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance397:BackColor = System.Drawing.SystemColors:Window.
      appearance397:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveCellAppearance = appearance397.
      appearance398:BackColor = System.Drawing.SystemColors:Highlight.
      appearance398:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveRowAppearance = appearance398.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance399:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CardAreaAppearance = appearance399.
      appearance400:BorderColor = System.Drawing.Color:Silver.
      appearance400:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellAppearance = appearance400.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellPadding = 0.
      appearance401:BackColor = System.Drawing.SystemColors:Control.
      appearance401:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance401:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance401:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance401:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:GroupByRowAppearance = appearance401.
      appearance402:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderAppearance = appearance402.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance403:BackColor = System.Drawing.SystemColors:Window.
      appearance403:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowAppearance = appearance403.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance404:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance404.
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
      THIS-OBJECT:GridImportP1:Size = NEW System.Drawing.Size(380, 255).
      THIS-OBJECT:GridImportP1:TabIndex = 0.
      THIS-OBJECT:GridImportP1:Text = "grid1".
      /*  */
      /* TabResurser */
      /*  */
      THIS-OBJECT:TabResurser:Controls:Add(THIS-OBJECT:resurserMainSC).
      THIS-OBJECT:TabResurser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResurser:Name = "TabResurser".
      THIS-OBJECT:TabResurser:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* resurserMainSC */
      /*  */
      THIS-OBJECT:resurserMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMainSC:Name = "resurserMainSC".
      /*  */
      /* resurserMainSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMainSC:Panel1:Controls:Add(THIS-OBJECT:resurserMenuSC).
      THIS-OBJECT:resurserMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:resurserMainSC:SplitterDistance = 897.
      THIS-OBJECT:resurserMainSC:TabIndex = 0.
      /*  */
      /* resurserMenuSC */
      /*  */
      THIS-OBJECT:resurserMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMenuSC:Name = "resurserMenuSC".
      THIS-OBJECT:resurserMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurserMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel1:Controls:Add(THIS-OBJECT:ResursertoolStrip).
      /*  */
      /* resurserMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResurs).
      THIS-OBJECT:resurserMenuSC:Size = NEW System.Drawing.Size(897, 553).
      THIS-OBJECT:resurserMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurserMenuSC:TabIndex = 3.
      /*  */
      /* ResursertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar11 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar11[1] = THIS-OBJECT:laggTillResursButton.
      arrayvar11[2] = THIS-OBJECT:TaBortResursButton.
      THIS-OBJECT:ResursertoolStrip:Items:AddRange(arrayvar11).
      THIS-OBJECT:ResursertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ResursertoolStrip:Name = "ResursertoolStrip".
      THIS-OBJECT:ResursertoolStrip:Size = NEW System.Drawing.Size(897, 25).
      THIS-OBJECT:ResursertoolStrip:TabIndex = 2.
      THIS-OBJECT:ResursertoolStrip:Text = "ResursertoolStrip".
      /*  */
      /* laggTillResursButton */
      /*  */
      THIS-OBJECT:laggTillResursButton:Image = CAST(resources:GetObject("laggTillResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillResursButton:Name = "laggTillResursButton".
      THIS-OBJECT:laggTillResursButton:Size = NEW System.Drawing.Size(103, 22).
      THIS-OBJECT:laggTillResursButton:Text = "Lägg till resurs".
      THIS-OBJECT:laggTillResursButton:Click:Subscribe(THIS-OBJECT:laggTillResursButton_Click).
      /*  */
      /* TaBortResursButton */
      /*  */
      THIS-OBJECT:TaBortResursButton:Image = CAST(resources:GetObject("TaBortResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortResursButton:Name = "TaBortResursButton".
      THIS-OBJECT:TaBortResursButton:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:TaBortResursButton:Text = "Ta bort resurs".
      THIS-OBJECT:TaBortResursButton:Click:Subscribe(THIS-OBJECT:TaBortResursButton_Click).
      /*  */
      /* gridResurs */
      /*  */
      appearance405:BackColor = System.Drawing.SystemColors:Window.
      appearance405:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurs:DisplayLayout:Appearance = appearance405.
      THIS-OBJECT:gridResurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance406:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance406:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance406:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance406:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:Appearance = appearance406.
      appearance407:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance407.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance408:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance408:BackColor2 = System.Drawing.SystemColors:Control.
      appearance408:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance408:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:PromptAppearance = appearance408.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance409:BackColor = System.Drawing.SystemColors:Window.
      appearance409:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveCellAppearance = appearance409.
      appearance410:BackColor = System.Drawing.SystemColors:Highlight.
      appearance410:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveRowAppearance = appearance410.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance411:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CardAreaAppearance = appearance411.
      appearance412:BorderColor = System.Drawing.Color:Silver.
      appearance412:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellAppearance = appearance412.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellPadding = 0.
      appearance413:BackColor = System.Drawing.SystemColors:Control.
      appearance413:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance413:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance413:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance413:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:GroupByRowAppearance = appearance413.
      appearance414:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderAppearance = appearance414.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance415:BackColor = System.Drawing.SystemColors:Window.
      appearance415:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowAppearance = appearance415.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance416:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance416.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurs:GuruContext = ?.
      THIS-OBJECT:gridResurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurs:Name = "gridResurs".
      THIS-OBJECT:gridResurs:Size = NEW System.Drawing.Size(897, 524).
      THIS-OBJECT:gridResurs:TabIndex = 0.
      THIS-OBJECT:gridResurs:Text = "grid1".
      THIS-OBJECT:gridResurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurs_AfterCellUpdate).
      /*  */
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:KatalogMainSC).
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* KatalogMainSC */
      /*  */
      THIS-OBJECT:KatalogMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogMainSC:Name = "KatalogMainSC".
      /*  */
      /* KatalogMainSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel1:Controls:Add(THIS-OBJECT:KatalogKatalogSC).
      /*  */
      /* KatalogMainSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImage = CAST(resources:GetObject("KatalogMainSC.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:KatalogMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:KatalogMainSC:SplitterDistance = 497.
      THIS-OBJECT:KatalogMainSC:TabIndex = 1.
      /*  */
      /* KatalogKatalogSC */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogKatalogSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:KatalogKatalogSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogKatalogSC:Name = "KatalogKatalogSC".
      THIS-OBJECT:KatalogKatalogSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* KatalogKatalogSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel1:Controls:Add(THIS-OBJECT:KatalogertoolStrip).
      /*  */
      /* KatalogKatalogSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel2:Controls:Add(THIS-OBJECT:gridKataloger).
      THIS-OBJECT:KatalogKatalogSC:Size = NEW System.Drawing.Size(497, 553).
      THIS-OBJECT:KatalogKatalogSC:SplitterDistance = 25.
      THIS-OBJECT:KatalogKatalogSC:TabIndex = 0.
      /*  */
      /* KatalogertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar12 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar12[1] = THIS-OBJECT:KatalogerValjButton.
      arrayvar12[2] = THIS-OBJECT:KopieraKatalogButton.
      arrayvar12[3] = THIS-OBJECT:TaBortKatalogButton.
      THIS-OBJECT:KatalogertoolStrip:Items:AddRange(arrayvar12).
      THIS-OBJECT:KatalogertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogertoolStrip:Name = "KatalogertoolStrip".
      THIS-OBJECT:KatalogertoolStrip:Size = NEW System.Drawing.Size(497, 25).
      THIS-OBJECT:KatalogertoolStrip:TabIndex = 1.
      THIS-OBJECT:KatalogertoolStrip:Text = "toolStrip1".
      /*  */
      /* KatalogerValjButton */
      /*  */
      THIS-OBJECT:KatalogerValjButton:Image = CAST(resources:GetObject("KatalogerValjButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KatalogerValjButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KatalogerValjButton:Name = "KatalogerValjButton".
      THIS-OBJECT:KatalogerValjButton:Size = NEW System.Drawing.Size(88, 22).
      THIS-OBJECT:KatalogerValjButton:Text = "Välj katalog".
      THIS-OBJECT:KatalogerValjButton:Click:Subscribe(THIS-OBJECT:KatalogerValjButton_Click).
      /*  */
      /* KopieraKatalogButton */
      /*  */
      THIS-OBJECT:KopieraKatalogButton:Image = CAST(resources:GetObject("KopieraKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KopieraKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KopieraKatalogButton:Name = "KopieraKatalogButton".
      THIS-OBJECT:KopieraKatalogButton:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:KopieraKatalogButton:Text = "Kopiera katalog".
      THIS-OBJECT:KopieraKatalogButton:Click:Subscribe(THIS-OBJECT:KopieraKatalogButton_Click).
      /*  */
      /* TaBortKatalogButton */
      /*  */
      THIS-OBJECT:TaBortKatalogButton:Image = CAST(resources:GetObject("TaBortKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortKatalogButton:Name = "TaBortKatalogButton".
      THIS-OBJECT:TaBortKatalogButton:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:TaBortKatalogButton:Text = "Ta bort katalog".
      THIS-OBJECT:TaBortKatalogButton:Click:Subscribe(THIS-OBJECT:TaBortKatalogButton_Click).
      /*  */
      /* gridKataloger */
      /*  */
      appearance417:BackColor = System.Drawing.SystemColors:Window.
      appearance417:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridKataloger:DisplayLayout:Appearance = appearance417.
      THIS-OBJECT:gridKataloger:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridKataloger:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance418:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance418:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance418:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance418:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:Appearance = appearance418.
      appearance419:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BandLabelAppearance = appearance419.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance420:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance420:BackColor2 = System.Drawing.SystemColors:Control.
      appearance420:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance420:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:PromptAppearance = appearance420.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance421:BackColor = System.Drawing.SystemColors:Window.
      appearance421:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveCellAppearance = appearance421.
      appearance422:BackColor = System.Drawing.SystemColors:Highlight.
      appearance422:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveRowAppearance = appearance422.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance423:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CardAreaAppearance = appearance423.
      appearance424:BorderColor = System.Drawing.Color:Silver.
      appearance424:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellAppearance = appearance424.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellPadding = 0.
      appearance425:BackColor = System.Drawing.SystemColors:Control.
      appearance425:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance425:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance425:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance425:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:GroupByRowAppearance = appearance425.
      appearance426:TextHAlignAsString = "Left".
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderAppearance = appearance426.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance427:BackColor = System.Drawing.SystemColors:Window.
      appearance427:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowAppearance = appearance427.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance428:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:TemplateAddRowAppearance = appearance428.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridKataloger:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridKataloger:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridKataloger:GuruContext = ?.
      THIS-OBJECT:gridKataloger:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridKataloger:Name = "gridKataloger".
      THIS-OBJECT:gridKataloger:Size = NEW System.Drawing.Size(497, 524).
      THIS-OBJECT:gridKataloger:TabIndex = 0.
      THIS-OBJECT:gridKataloger:Text = "grid1".
      THIS-OBJECT:gridKataloger:AfterCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_AfterCellUpdate).
      THIS-OBJECT:gridKataloger:CellChange:Subscribe(THIS-OBJECT:gridKataloger_CellChange).
      THIS-OBJECT:gridKataloger:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_BeforeCellUpdate).
      THIS-OBJECT:gridKataloger:DoubleClickRow:Subscribe(THIS-OBJECT:gridKataloger_DoubleClickRow).
      /*  */
      /* TabResursPriser */
      /*  */
      THIS-OBJECT:TabResursPriser:Controls:Add(THIS-OBJECT:splitContainerResursPris).
      THIS-OBJECT:TabResursPriser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabResursPriser:Name = "TabResursPriser".
      THIS-OBJECT:TabResursPriser:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerResursPris */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPris:Name = "splitContainerResursPris".
      /*  */
      /* splitContainerResursPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerResursPrisLista).
      /*  */
      /* splitContainerResursPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel2:Controls:Add(THIS-OBJECT:TabManagerResurspriser).
      THIS-OBJECT:splitContainerResursPris:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerResursPris:SplitterDistance = 475.
      THIS-OBJECT:splitContainerResursPris:TabIndex = 4.
      /*  */
      /* splitContainerResursPrisLista */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPrisLista:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerResursPrisLista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPrisLista:Name = "splitContainerResursPrisLista".
      THIS-OBJECT:splitContainerResursPrisLista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerResursPrisLista.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:Controls:Add(THIS-OBJECT:resurspristoolStrip).
      /*  */
      /* splitContainerResursPrisLista.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:Controls:Add(THIS-OBJECT:gridResurspris).
      THIS-OBJECT:splitContainerResursPrisLista:Size = NEW System.Drawing.Size(475, 553).
      THIS-OBJECT:splitContainerResursPrisLista:SplitterDistance = 25.
      THIS-OBJECT:splitContainerResursPrisLista:TabIndex = 0.
      /*  */
      /* resurspristoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar13 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar13[1] = THIS-OBJECT:taBortResursPrisButton.
      THIS-OBJECT:resurspristoolStrip:Items:AddRange(arrayvar13).
      THIS-OBJECT:resurspristoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspristoolStrip:Name = "resurspristoolStrip".
      THIS-OBJECT:resurspristoolStrip:Size = NEW System.Drawing.Size(475, 25).
      THIS-OBJECT:resurspristoolStrip:TabIndex = 0.
      THIS-OBJECT:resurspristoolStrip:Text = "toolStrip3".
      /*  */
      /* taBortResursPrisButton */
      /*  */
      THIS-OBJECT:taBortResursPrisButton:Image = CAST(resources:GetObject("taBortResursPrisButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortResursPrisButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortResursPrisButton:Name = "taBortResursPrisButton".
      THIS-OBJECT:taBortResursPrisButton:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:taBortResursPrisButton:Text = "Ta bort resurs".
      THIS-OBJECT:taBortResursPrisButton:ToolTipText = "Tar bort den markerade resursen från katalogen".
      THIS-OBJECT:taBortResursPrisButton:Click:Subscribe(THIS-OBJECT:taBortResursPrisButton_Click).
      /*  */
      /* gridResurspris */
      /*  */
      appearance429:BackColor = System.Drawing.SystemColors:Window.
      appearance429:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurspris:DisplayLayout:Appearance = appearance429.
      THIS-OBJECT:gridResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance430:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance430:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance430:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance430:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:Appearance = appearance430.
      appearance431:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance431.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance432:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance432:BackColor2 = System.Drawing.SystemColors:Control.
      appearance432:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance432:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance432.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance433:BackColor = System.Drawing.SystemColors:Window.
      appearance433:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance433.
      appearance434:BackColor = System.Drawing.SystemColors:Highlight.
      appearance434:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance434.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance435:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CardAreaAppearance = appearance435.
      appearance436:BorderColor = System.Drawing.Color:Silver.
      appearance436:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellAppearance = appearance436.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance437:BackColor = System.Drawing.SystemColors:Control.
      appearance437:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance437:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance437:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance437:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance437.
      appearance438:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderAppearance = appearance438.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance439:BackColor = System.Drawing.SystemColors:Window.
      appearance439:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowAppearance = appearance439.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance440:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance440.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurspris:GuruContext = ?.
      THIS-OBJECT:gridResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurspris:Name = "gridResurspris".
      THIS-OBJECT:gridResurspris:Size = NEW System.Drawing.Size(475, 524).
      THIS-OBJECT:gridResurspris:TabIndex = 1.
      THIS-OBJECT:gridResurspris:Text = "grid1".
      THIS-OBJECT:gridResurspris:AfterCellUpdate:Subscribe(THIS-OBJECT:gridResurspris_AfterCellUpdate).
      /*  */
      /* TabManagerResurspriser */
      /*  */
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPPaslag).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPNiva).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPRegel).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPBasresurser).
      THIS-OBJECT:TabManagerResurspriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerResurspriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerResurspriser:Name = "TabManagerResurspriser".
      THIS-OBJECT:TabManagerResurspriser:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv.
      THIS-OBJECT:TabManagerResurspriser:Size = NEW System.Drawing.Size(666, 553).
      THIS-OBJECT:TabManagerResurspriser:TabIndex = 0.
      ultraTab21:TabPage = THIS-OBJECT:TabRPPaslag.
      ultraTab21:Text = "Påslag".
      ultraTab23:TabPage = THIS-OBJECT:TabRPNiva.
      ultraTab23:Text = "Nivåer".
      ultraTab18:TabPage = THIS-OBJECT:TabRPRegel.
      ultraTab18:Text = "Regel".
      ultraTab24:Key = "Basresurser".
      ultraTab24:TabPage = THIS-OBJECT:TabRPBasresurser.
      ultraTab24:Text = "Basresurser".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar14 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
      arrayvar14[1] = ultraTab21.
      arrayvar14[2] = ultraTab23.
      arrayvar14[3] = ultraTab18.
      arrayvar14[4] = ultraTab24.
      THIS-OBJECT:TabManagerResurspriser:Tabs:AddRange(arrayvar14).
      /*  */
      /* ultraTabSharedControlsPagePaslagNiv */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Name = "ultraTabSharedControlsPagePaslagNiv".
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Size = NEW System.Drawing.Size(662, 527).
      /*  */
      /* TabMtrl */
      /*  */
      THIS-OBJECT:TabMtrl:Controls:Add(THIS-OBJECT:mtrlMainSC).
      THIS-OBJECT:TabMtrl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMtrl:Name = "TabMtrl".
      THIS-OBJECT:TabMtrl:Size = NEW System.Drawing.Size(1145, 553).
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
      THIS-OBJECT:mtrlMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:mtrlMainSC:SplitterDistance = 576.
      THIS-OBJECT:mtrlMainSC:TabIndex = 2.
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
      THIS-OBJECT:mtrlMenuSC:Size = NEW System.Drawing.Size(576, 553).
      THIS-OBJECT:mtrlMenuSC:SplitterDistance = 25.
      THIS-OBJECT:mtrlMenuSC:TabIndex = 0.
      /*  */
      /* mtrltoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar15 AS System.Windows.Forms.ToolStripItem EXTENT 5 NO-UNDO.
      arrayvar15[1] = THIS-OBJECT:laggTillMtrlButton.
      arrayvar15[2] = THIS-OBJECT:taBortMtrlButton.
      arrayvar15[3] = THIS-OBJECT:uppdateraMtrlKostButton.
      arrayvar15[4] = THIS-OBJECT:mtrlImportButton.
      arrayvar15[5] = THIS-OBJECT:mtrlExportButton.
      THIS-OBJECT:mtrltoolStrip:Items:AddRange(arrayvar15).
      THIS-OBJECT:mtrltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:mtrltoolStrip:Name = "mtrltoolStrip".
      THIS-OBJECT:mtrltoolStrip:Size = NEW System.Drawing.Size(576, 25).
      THIS-OBJECT:mtrltoolStrip:TabIndex = 1.
      THIS-OBJECT:mtrltoolStrip:Text = "toolStrip1".
      THIS-OBJECT:mtrltoolStrip:ItemClicked:Subscribe(THIS-OBJECT:toolStrip2_ItemClicked).
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
      THIS-OBJECT:mtrlImportButton:Size = NEW System.Drawing.Size(125, 22).
      THIS-OBJECT:mtrlImportButton:Text = "Importera materiel".
      /*  */
      /* mtrlExportButton */
      /*  */
      THIS-OBJECT:mtrlExportButton:Image = CAST(resources:GetObject("mtrlExportButton.Image"), System.Drawing.Image).
      THIS-OBJECT:mtrlExportButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:mtrlExportButton:Name = "mtrlExportButton".
      THIS-OBJECT:mtrlExportButton:Size = NEW System.Drawing.Size(122, 22).
      THIS-OBJECT:mtrlExportButton:Text = "Exportera materiel".
      /*  */
      /* gridMateriel */
      /*  */
      appearance441:BackColor = System.Drawing.SystemColors:Window.
      appearance441:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridMateriel:DisplayLayout:Appearance = appearance441.
      THIS-OBJECT:gridMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance442:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance442:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance442:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance442:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:Appearance = appearance442.
      appearance443:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance443.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance444:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance444:BackColor2 = System.Drawing.SystemColors:Control.
      appearance444:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance444:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance444.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance445:BackColor = System.Drawing.SystemColors:Window.
      appearance445:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance445.
      appearance446:BackColor = System.Drawing.SystemColors:Highlight.
      appearance446:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance446.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance447:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CardAreaAppearance = appearance447.
      appearance448:BorderColor = System.Drawing.Color:Silver.
      appearance448:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellAppearance = appearance448.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance449:BackColor = System.Drawing.SystemColors:Control.
      appearance449:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance449:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance449:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance449:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance449.
      appearance450:TextHAlignAsString = "Left".
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderAppearance = appearance450.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance451:BackColor = System.Drawing.SystemColors:Window.
      appearance451:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowAppearance = appearance451.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance452:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance452.
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
      THIS-OBJECT:gridMateriel:Size = NEW System.Drawing.Size(576, 524).
      THIS-OBJECT:gridMateriel:TabIndex = 0.
      THIS-OBJECT:gridMateriel:Text = "grid1".
      THIS-OBJECT:gridMateriel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridMateriel_AfterCellUpdate).
      /*  */
      /* TabP5 */
      /*  */
      THIS-OBJECT:TabP5:Controls:Add(THIS-OBJECT:p5MainSC).
      THIS-OBJECT:TabP5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP5:Name = "TabP5".
      THIS-OBJECT:TabP5:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* p5MainSC */
      /*  */
      THIS-OBJECT:p5MainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p5MainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p5MainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p5MainSC:Name = "p5MainSC".
      THIS-OBJECT:p5MainSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p5MainSC.Panel1 */
      /*  */
      THIS-OBJECT:p5MainSC:Panel1:Controls:Add(THIS-OBJECT:p5KoderMenuSC).
      /*  */
      /* p5MainSC.Panel2 */
      /*  */
      THIS-OBJECT:p5MainSC:Panel2:Controls:Add(THIS-OBJECT:TabManagerP5).
      THIS-OBJECT:p5MainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:p5MainSC:SplitterDistance = 230.
      THIS-OBJECT:p5MainSC:TabIndex = 0.
      /*  */
      /* p5KoderMenuSC */
      /*  */
      THIS-OBJECT:p5KoderMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p5KoderMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p5KoderMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p5KoderMenuSC:Name = "p5KoderMenuSC".
      THIS-OBJECT:p5KoderMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p5KoderMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:p5KoderMenuSC:Panel1:Controls:Add(THIS-OBJECT:p5KoderStrip).
      /*  */
      /* p5KoderMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:p5KoderMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridP5).
      THIS-OBJECT:p5KoderMenuSC:Size = NEW System.Drawing.Size(1145, 230).
      THIS-OBJECT:p5KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p5KoderMenuSC:TabIndex = 1.
      /*  */
      /* p5KoderStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar16 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar16[1] = THIS-OBJECT:laggTillP5Button.
      arrayvar16[2] = THIS-OBJECT:taBortP5Button.
      THIS-OBJECT:p5KoderStrip:Items:AddRange(arrayvar16).
      THIS-OBJECT:p5KoderStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p5KoderStrip:Name = "p5KoderStrip".
      THIS-OBJECT:p5KoderStrip:Size = NEW System.Drawing.Size(1145, 25).
      THIS-OBJECT:p5KoderStrip:TabIndex = 1.
      THIS-OBJECT:p5KoderStrip:Text = "toolStrip4".
      /*  */
      /* laggTillP5Button */
      /*  */
      THIS-OBJECT:laggTillP5Button:Image = CAST(resources:GetObject("laggTillP5Button.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP5Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP5Button:Name = "laggTillP5Button".
      THIS-OBJECT:laggTillP5Button:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:laggTillP5Button:Text = "Lägg till P5-Kod".
      THIS-OBJECT:laggTillP5Button:Click:Subscribe(THIS-OBJECT:laggTillP5Button_Click).
      /*  */
      /* taBortP5Button */
      /*  */
      THIS-OBJECT:taBortP5Button:Image = CAST(resources:GetObject("taBortP5Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP5Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP5Button:Name = "taBortP5Button".
      THIS-OBJECT:taBortP5Button:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:taBortP5Button:Text = "Ta bort P5-Kod".
      THIS-OBJECT:taBortP5Button:ToolTipText = "Tar bort markerad P5-Kod".
      THIS-OBJECT:taBortP5Button:Click:Subscribe(THIS-OBJECT:taBortP5Button_Click).
      /*  */
      /* gridP5 */
      /*  */
      appearance453:BackColor = System.Drawing.SystemColors:Window.
      appearance453:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5:DisplayLayout:Appearance = appearance453.
      THIS-OBJECT:gridP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance454:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance454:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance454:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance454:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:Appearance = appearance454.
      appearance455:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance455.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance456:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance456:BackColor2 = System.Drawing.SystemColors:Control.
      appearance456:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance456:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:PromptAppearance = appearance456.
      THIS-OBJECT:gridP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance457:BackColor = System.Drawing.SystemColors:Window.
      appearance457:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveCellAppearance = appearance457.
      appearance458:BackColor = System.Drawing.SystemColors:Highlight.
      appearance458:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveRowAppearance = appearance458.
      THIS-OBJECT:gridP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance459:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CardAreaAppearance = appearance459.
      appearance460:BorderColor = System.Drawing.Color:Silver.
      appearance460:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellAppearance = appearance460.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellPadding = 0.
      appearance461:BackColor = System.Drawing.SystemColors:Control.
      appearance461:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance461:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance461:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance461:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:Override:GroupByRowAppearance = appearance461.
      appearance462:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderAppearance = appearance462.
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance463:BackColor = System.Drawing.SystemColors:Window.
      appearance463:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5:DisplayLayout:Override:RowAppearance = appearance463.
      THIS-OBJECT:gridP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance464:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance464.
      THIS-OBJECT:gridP5:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP5:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP5:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP5:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP5:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP5:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP5:GuruContext = ?.
      THIS-OBJECT:gridP5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP5:Name = "gridP5".
      THIS-OBJECT:gridP5:Size = NEW System.Drawing.Size(1145, 201).
      THIS-OBJECT:gridP5:TabIndex = 0.
      THIS-OBJECT:gridP5:Text = "grid1".
      THIS-OBJECT:gridP5:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP5_AfterCellUpdate).
      THIS-OBJECT:gridP5:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP5_BeforeCellUpdate).
      /*  */
      /* TabManagerP5 */
      /*  */
      THIS-OBJECT:TabManagerP5:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP5).
      THIS-OBJECT:TabManagerP5:Controls:Add(THIS-OBJECT:TabP5resurs).
      THIS-OBJECT:TabManagerP5:Controls:Add(THIS-OBJECT:TabP5Import).
      THIS-OBJECT:TabManagerP5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP5:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP5:Name = "TabManagerP5".
      THIS-OBJECT:TabManagerP5:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP5.
      THIS-OBJECT:TabManagerP5:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP5:TabIndex = 1.
      appearance465:Image = CAST(resources:GetObject("appearance465.Image"), System.Object).
      ultraTab26:Appearance = appearance465.
      ultraTab26:Key = "P5Resurs".
      ultraTab26:TabPage = THIS-OBJECT:TabP5resurs.
      ultraTab26:Text = "P5-Resurser".
      appearance466:Image = CAST(resources:GetObject("appearance466.Image"), System.Object).
      ultraTab27:ActiveAppearance = appearance466.
      appearance467:Image = CAST(resources:GetObject("appearance467.Image"), System.Object).
      ultraTab27:Appearance = appearance467.
      ultraTab27:Key = "Import".
      ultraTab27:TabPage = THIS-OBJECT:TabP5Import.
      ultraTab27:Text = "Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar17[1] = ultraTab26.
      arrayvar17[2] = ultraTab27.
      THIS-OBJECT:TabManagerP5:Tabs:AddRange(arrayvar17).
      THIS-OBJECT:TabManagerP5:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP5_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP5 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP5:Name = "ultraTabSharedControlsPageP5".
      THIS-OBJECT:ultraTabSharedControlsPageP5:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* TabP4 */
      /*  */
      THIS-OBJECT:TabP4:Controls:Add(THIS-OBJECT:p4MainSC).
      THIS-OBJECT:TabP4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP4:Name = "TabP4".
      THIS-OBJECT:TabP4:Size = NEW System.Drawing.Size(1145, 553).
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
      THIS-OBJECT:p4MainSC:TabIndex = 0.
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
      DEFINE VARIABLE arrayvar18 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar18[1] = THIS-OBJECT:laggTillP4Button.
      arrayvar18[2] = THIS-OBJECT:taBortP4Button.
      THIS-OBJECT:p4KodertoolStrip:Items:AddRange(arrayvar18).
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
      appearance468:BackColor = System.Drawing.SystemColors:Window.
      appearance468:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4:DisplayLayout:Appearance = appearance468.
      THIS-OBJECT:gridP4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance469:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance469:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance469:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance469:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:Appearance = appearance469.
      appearance470:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance470.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance471:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance471:BackColor2 = System.Drawing.SystemColors:Control.
      appearance471:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance471:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4:DisplayLayout:GroupByBox:PromptAppearance = appearance471.
      THIS-OBJECT:gridP4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance472:BackColor = System.Drawing.SystemColors:Window.
      appearance472:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveCellAppearance = appearance472.
      appearance473:BackColor = System.Drawing.SystemColors:Highlight.
      appearance473:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:ActiveRowAppearance = appearance473.
      THIS-OBJECT:gridP4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance474:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CardAreaAppearance = appearance474.
      appearance475:BorderColor = System.Drawing.Color:Silver.
      appearance475:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellAppearance = appearance475.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4:DisplayLayout:Override:CellPadding = 0.
      appearance476:BackColor = System.Drawing.SystemColors:Control.
      appearance476:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance476:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance476:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance476:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4:DisplayLayout:Override:GroupByRowAppearance = appearance476.
      appearance477:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderAppearance = appearance477.
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance478:BackColor = System.Drawing.SystemColors:Window.
      appearance478:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4:DisplayLayout:Override:RowAppearance = appearance478.
      THIS-OBJECT:gridP4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance479:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4:DisplayLayout:Override:TemplateAddRowAppearance = appearance479.
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
      THIS-OBJECT:gridP4:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP4_AfterCellUpdate).
      THIS-OBJECT:gridP4:CellChange:Subscribe(THIS-OBJECT:gridP4_CellChange).
      THIS-OBJECT:gridP4:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP4_BeforeCellUpdate).
      /*  */
      /* TabManagerP4 */
      /*  */
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP4).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Resurs).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Frekvens).
      THIS-OBJECT:TabManagerP4:Controls:Add(THIS-OBJECT:TabP4Import).
      THIS-OBJECT:TabManagerP4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP4:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP4:Name = "TabManagerP4".
      THIS-OBJECT:TabManagerP4:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP4.
      THIS-OBJECT:TabManagerP4:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP4:TabIndex = 3.
      appearance480:Image = CAST(resources:GetObject("appearance480.Image"), System.Object).
      ultraTab12:Appearance = appearance480.
      ultraTab12:Key = "P4Resurs".
      ultraTab12:TabPage = THIS-OBJECT:TabP4Resurs.
      ultraTab12:Text = "P4-Resurser".
      appearance481:Image = CAST(resources:GetObject("appearance481.Image"), System.Object).
      ultraTab15:Appearance = appearance481.
      ultraTab15:Key = "P4Frekvens".
      ultraTab15:TabPage = THIS-OBJECT:TabP4Frekvens.
      ultraTab15:Text = "P4-Frekvenser".
      appearance482:Image = CAST(resources:GetObject("appearance482.Image"), System.Object).
      ultraTab1:ActiveAppearance = appearance482.
      appearance483:Image = CAST(resources:GetObject("appearance483.Image"), System.Object).
      ultraTab1:Appearance = appearance483.
      ultraTab1:Key = "Import".
      ultraTab1:TabPage = THIS-OBJECT:TabP4Import.
      ultraTab1:Text = "Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar19[1] = ultraTab12.
      arrayvar19[2] = ultraTab15.
      arrayvar19[3] = ultraTab1.
      THIS-OBJECT:TabManagerP4:Tabs:AddRange(arrayvar19).
      THIS-OBJECT:TabManagerP4:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP4_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP4 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP4:Name = "ultraTabSharedControlsPageP4".
      THIS-OBJECT:ultraTabSharedControlsPageP4:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* TabP3 */
      /*  */
      THIS-OBJECT:TabP3:Controls:Add(THIS-OBJECT:p3MainSC).
      THIS-OBJECT:TabP3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP3:Name = "TabP3".
      THIS-OBJECT:TabP3:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* p3MainSC */
      /*  */
      THIS-OBJECT:p3MainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3MainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p3MainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3MainSC:Name = "p3MainSC".
      THIS-OBJECT:p3MainSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p3MainSC.Panel1 */
      /*  */
      THIS-OBJECT:p3MainSC:Panel1:Controls:Add(THIS-OBJECT:p3kodSC).
      /*  */
      /* p3MainSC.Panel2 */
      /*  */
      THIS-OBJECT:p3MainSC:Panel2:Controls:Add(THIS-OBJECT:TabManagerP3).
      THIS-OBJECT:p3MainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:p3MainSC:SplitterDistance = 230.
      THIS-OBJECT:p3MainSC:TabIndex = 0.
      /*  */
      /* p3kodSC */
      /*  */
      THIS-OBJECT:p3kodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3kodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p3kodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3kodSC:Name = "p3kodSC".
      /*  */
      /* p3kodSC.Panel1 */
      /*  */
      THIS-OBJECT:p3kodSC:Panel1:Controls:Add(THIS-OBJECT:p3arbkodSC).
      /*  */
      /* p3kodSC.Panel2 */
      /*  */
      THIS-OBJECT:p3kodSC:Panel2:Controls:Add(THIS-OBJECT:p3KoderMenuSC).
      THIS-OBJECT:p3kodSC:Size = NEW System.Drawing.Size(1145, 230).
      THIS-OBJECT:p3kodSC:SplitterDistance = 400.
      THIS-OBJECT:p3kodSC:TabIndex = 2.
      /*  */
      /* p3arbkodSC */
      /*  */
      THIS-OBJECT:p3arbkodSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3arbkodSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p3arbkodSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3arbkodSC:Name = "p3arbkodSC".
      THIS-OBJECT:p3arbkodSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p3arbkodSC.Panel1 */
      /*  */
      THIS-OBJECT:p3arbkodSC:Panel1:Controls:Add(THIS-OBJECT:p3arbkodStrip).
      /*  */
      /* p3arbkodSC.Panel2 */
      /*  */
      THIS-OBJECT:p3arbkodSC:Panel2:Controls:Add(THIS-OBJECT:gridP3arbkod).
      THIS-OBJECT:p3arbkodSC:Size = NEW System.Drawing.Size(400, 230).
      THIS-OBJECT:p3arbkodSC:SplitterDistance = 25.
      THIS-OBJECT:p3arbkodSC:TabIndex = 0.
      /*  */
      /* p3arbkodStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar20 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar20[1] = THIS-OBJECT:laggTillP3arbkod.
      arrayvar20[2] = THIS-OBJECT:taBortP3arbkodButton.
      THIS-OBJECT:p3arbkodStrip:Items:AddRange(arrayvar20).
      THIS-OBJECT:p3arbkodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3arbkodStrip:Name = "p3arbkodStrip".
      THIS-OBJECT:p3arbkodStrip:Size = NEW System.Drawing.Size(400, 25).
      THIS-OBJECT:p3arbkodStrip:TabIndex = 0.
      THIS-OBJECT:p3arbkodStrip:Text = "toolStrip1".
      /*  */
      /* laggTillP3arbkod */
      /*  */
      THIS-OBJECT:laggTillP3arbkod:Image = CAST(resources:GetObject("laggTillP3arbkod.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP3arbkod:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP3arbkod:Name = "laggTillP3arbkod".
      THIS-OBJECT:laggTillP3arbkod:Size = NEW System.Drawing.Size(144, 22).
      THIS-OBJECT:laggTillP3arbkod:Text = "Lägg till P3-Arbetskod".
      THIS-OBJECT:laggTillP3arbkod:Click:Subscribe(THIS-OBJECT:laggTillP3arbkod_Click).
      /*  */
      /* taBortP3arbkodButton */
      /*  */
      THIS-OBJECT:taBortP3arbkodButton:Image = CAST(resources:GetObject("taBortP3arbkodButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP3arbkodButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP3arbkodButton:Name = "taBortP3arbkodButton".
      THIS-OBJECT:taBortP3arbkodButton:Size = NEW System.Drawing.Size(140, 22).
      THIS-OBJECT:taBortP3arbkodButton:Text = "Ta bort P3-Arbetskod".
      THIS-OBJECT:taBortP3arbkodButton:ToolTipText = "Tar bort markerad P3-Arbetskod".
      THIS-OBJECT:taBortP3arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP3arbkodButton_Click).
      /*  */
      /* gridP3arbkod */
      /*  */
      appearance484:BackColor = System.Drawing.SystemColors:Window.
      appearance484:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Appearance = appearance484.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance485:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance485:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance485:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance485:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:Appearance = appearance485.
      appearance486:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance486.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance487:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance487:BackColor2 = System.Drawing.SystemColors:Control.
      appearance487:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance487:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance487.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance488:BackColor = System.Drawing.SystemColors:Window.
      appearance488:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance488.
      appearance489:BackColor = System.Drawing.SystemColors:Highlight.
      appearance489:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance489.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance490:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CardAreaAppearance = appearance490.
      appearance491:BorderColor = System.Drawing.Color:Silver.
      appearance491:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellAppearance = appearance491.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance492:BackColor = System.Drawing.SystemColors:Control.
      appearance492:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance492:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance492:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance492:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance492.
      appearance493:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderAppearance = appearance493.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance494:BackColor = System.Drawing.SystemColors:Window.
      appearance494:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:RowAppearance = appearance494.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance495:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance495.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP3arbkod:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP3arbkod:GuruContext = ?.
      THIS-OBJECT:gridP3arbkod:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP3arbkod:Name = "gridP3arbkod".
      THIS-OBJECT:gridP3arbkod:Size = NEW System.Drawing.Size(400, 201).
      THIS-OBJECT:gridP3arbkod:TabIndex = 0.
      THIS-OBJECT:gridP3arbkod:Text = "grid1".
      THIS-OBJECT:gridP3arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP3arbkod_AfterCellUpdate).
      THIS-OBJECT:gridP3arbkod:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP3arbkod_BeforeCellUpdate).
      /*  */
      /* p3KoderMenuSC */
      /*  */
      THIS-OBJECT:p3KoderMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:p3KoderMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:p3KoderMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3KoderMenuSC:Name = "p3KoderMenuSC".
      THIS-OBJECT:p3KoderMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* p3KoderMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:p3KoderMenuSC:Panel1:Controls:Add(THIS-OBJECT:p3KodertoolStrip).
      /*  */
      /* p3KoderMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:p3KoderMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridP3).
      THIS-OBJECT:p3KoderMenuSC:Size = NEW System.Drawing.Size(741, 230).
      THIS-OBJECT:p3KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p3KoderMenuSC:TabIndex = 1.
      /*  */
      /* p3KodertoolStrip */
      /*  */
      THIS-OBJECT:p3KodertoolStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar21 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar21[1] = THIS-OBJECT:laggTillP3Button.
      arrayvar21[2] = THIS-OBJECT:taBortP3Button.
      THIS-OBJECT:p3KodertoolStrip:Items:AddRange(arrayvar21).
      THIS-OBJECT:p3KodertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p3KodertoolStrip:Name = "p3KodertoolStrip".
      THIS-OBJECT:p3KodertoolStrip:Size = NEW System.Drawing.Size(741, 25).
      THIS-OBJECT:p3KodertoolStrip:TabIndex = 0.
      THIS-OBJECT:p3KodertoolStrip:Text = "toolStrip2".
      /*  */
      /* laggTillP3Button */
      /*  */
      THIS-OBJECT:laggTillP3Button:Image = CAST(resources:GetObject("laggTillP3Button.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP3Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP3Button:Name = "laggTillP3Button".
      THIS-OBJECT:laggTillP3Button:Size = NEW System.Drawing.Size(127, 22).
      THIS-OBJECT:laggTillP3Button:Text = "Lägg till ny P3-Kod".
      THIS-OBJECT:laggTillP3Button:Click:Subscribe(THIS-OBJECT:laggTillP3Button_Click).
      /*  */
      /* taBortP3Button */
      /*  */
      THIS-OBJECT:taBortP3Button:Image = CAST(resources:GetObject("taBortP3Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP3Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP3Button:Name = "taBortP3Button".
      THIS-OBJECT:taBortP3Button:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:taBortP3Button:Text = "Ta bort P3-Kod".
      THIS-OBJECT:taBortP3Button:ToolTipText = "Tar bort markerad P3-Kod".
      THIS-OBJECT:taBortP3Button:Click:Subscribe(THIS-OBJECT:taBortP3Button_Click).
      /*  */
      /* gridP3 */
      /*  */
      appearance496:BackColor = System.Drawing.SystemColors:Window.
      appearance496:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3:DisplayLayout:Appearance = appearance496.
      THIS-OBJECT:gridP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance497:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance497:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance497:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance497:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:Appearance = appearance497.
      appearance498:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance498.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance499:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance499:BackColor2 = System.Drawing.SystemColors:Control.
      appearance499:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance499:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:PromptAppearance = appearance499.
      THIS-OBJECT:gridP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance500:BackColor = System.Drawing.SystemColors:Window.
      appearance500:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveCellAppearance = appearance500.
      appearance501:BackColor = System.Drawing.SystemColors:Highlight.
      appearance501:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveRowAppearance = appearance501.
      THIS-OBJECT:gridP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance502:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CardAreaAppearance = appearance502.
      appearance503:BorderColor = System.Drawing.Color:Silver.
      appearance503:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellAppearance = appearance503.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellPadding = 0.
      appearance504:BackColor = System.Drawing.SystemColors:Control.
      appearance504:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance504:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance504:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance504:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:Override:GroupByRowAppearance = appearance504.
      appearance505:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderAppearance = appearance505.
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance506:BackColor = System.Drawing.SystemColors:Window.
      appearance506:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3:DisplayLayout:Override:RowAppearance = appearance506.
      THIS-OBJECT:gridP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance507:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance507.
      THIS-OBJECT:gridP3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridP3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridP3:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridP3:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridP3:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridP3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridP3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridP3:GuruContext = ?.
      THIS-OBJECT:gridP3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridP3:Name = "gridP3".
      THIS-OBJECT:gridP3:Size = NEW System.Drawing.Size(741, 201).
      THIS-OBJECT:gridP3:TabIndex = 0.
      THIS-OBJECT:gridP3:Text = "grid1".
      THIS-OBJECT:gridP3:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP3_AfterCellUpdate).
      THIS-OBJECT:gridP3:CellChange:Subscribe(THIS-OBJECT:gridP3_CellChange).
      THIS-OBJECT:gridP3:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP3_BeforeCellUpdate).
      /*  */
      /* TabManagerP3 */
      /*  */
      THIS-OBJECT:TabManagerP3:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP3).
      THIS-OBJECT:TabManagerP3:Controls:Add(THIS-OBJECT:TabP3Resurs).
      THIS-OBJECT:TabManagerP3:Controls:Add(THIS-OBJECT:TabP3Frekvens).
      THIS-OBJECT:TabManagerP3:Controls:Add(THIS-OBJECT:TabP3Import).
      THIS-OBJECT:TabManagerP3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP3:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP3:Name = "TabManagerP3".
      THIS-OBJECT:TabManagerP3:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP3.
      THIS-OBJECT:TabManagerP3:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP3:TabIndex = 3.
      appearance508:Image = CAST(resources:GetObject("appearance508.Image"), System.Object).
      ultraTab10:Appearance = appearance508.
      ultraTab10:Key = "P3Resurs".
      ultraTab10:TabPage = THIS-OBJECT:TabP3Resurs.
      ultraTab10:Text = "P3-Resurser".
      appearance509:Image = CAST(resources:GetObject("appearance509.Image"), System.Object).
      ultraTab11:Appearance = appearance509.
      ultraTab11:Key = "P3Frekvens".
      ultraTab11:TabPage = THIS-OBJECT:TabP3Frekvens.
      ultraTab11:Text = "P3-Frekvenser".
      appearance510:Image = CAST(resources:GetObject("appearance510.Image"), System.Object).
      ultraTab5:ActiveAppearance = appearance510.
      appearance511:Image = CAST(resources:GetObject("appearance511.Image"), System.Object).
      ultraTab5:Appearance = appearance511.
      ultraTab5:Key = "Import".
      ultraTab5:TabPage = THIS-OBJECT:TabP3Import.
      ultraTab5:Text = " Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar22 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar22[1] = ultraTab10.
      arrayvar22[2] = ultraTab11.
      arrayvar22[3] = ultraTab5.
      THIS-OBJECT:TabManagerP3:Tabs:AddRange(arrayvar22).
      THIS-OBJECT:TabManagerP3:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP3_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP3 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP3:Name = "ultraTabSharedControlsPageP3".
      THIS-OBJECT:ultraTabSharedControlsPageP3:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* TabP2 */
      /*  */
      THIS-OBJECT:TabP2:Controls:Add(THIS-OBJECT:p2MainSC).
      THIS-OBJECT:TabP2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP2:Name = "TabP2".
      THIS-OBJECT:TabP2:Size = NEW System.Drawing.Size(1145, 553).
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
      THIS-OBJECT:p2MainSC:TabIndex = 0.
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
      DEFINE VARIABLE arrayvar23 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar23[1] = THIS-OBJECT:laggTillP2arbkod.
      arrayvar23[2] = THIS-OBJECT:taBortP2arbkodButton.
      THIS-OBJECT:p2arbkodStrip:Items:AddRange(arrayvar23).
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
      THIS-OBJECT:taBortP2arbkodButton:Size = NEW System.Drawing.Size(140, 22).
      THIS-OBJECT:taBortP2arbkodButton:Text = "Ta bort P2-Arbetskod".
      THIS-OBJECT:taBortP2arbkodButton:ToolTipText = "Tar bort markerad P2-Arbetskod".
      THIS-OBJECT:taBortP2arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP2arbkodButton_Click).
      /*  */
      /* gridP2arbkod */
      /*  */
      appearance512:BackColor = System.Drawing.SystemColors:Window.
      appearance512:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Appearance = appearance512.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance513:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance513:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance513:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance513:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:Appearance = appearance513.
      appearance514:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance514.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance515:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance515:BackColor2 = System.Drawing.SystemColors:Control.
      appearance515:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance515:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance515.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance516:BackColor = System.Drawing.SystemColors:Window.
      appearance516:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance516.
      appearance517:BackColor = System.Drawing.SystemColors:Highlight.
      appearance517:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance517.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance518:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CardAreaAppearance = appearance518.
      appearance519:BorderColor = System.Drawing.Color:Silver.
      appearance519:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellAppearance = appearance519.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance520:BackColor = System.Drawing.SystemColors:Control.
      appearance520:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance520:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance520:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance520:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance520.
      appearance521:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderAppearance = appearance521.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance522:BackColor = System.Drawing.SystemColors:Window.
      appearance522:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:RowAppearance = appearance522.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance523:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance523.
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
      THIS-OBJECT:gridP2arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP2arbkod_AfterCellUpdate).
      THIS-OBJECT:gridP2arbkod:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP2arbkod_BeforeCellUpdate).
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
      DEFINE VARIABLE arrayvar24 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar24[1] = THIS-OBJECT:laggTillP2Button.
      arrayvar24[2] = THIS-OBJECT:taBortP2Button.
      THIS-OBJECT:p2KodertoolStrip:Items:AddRange(arrayvar24).
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
      /* taBortP2Button */
      /*  */
      THIS-OBJECT:taBortP2Button:Image = CAST(resources:GetObject("taBortP2Button.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP2Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP2Button:Name = "taBortP2Button".
      THIS-OBJECT:taBortP2Button:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:taBortP2Button:Text = "Ta bort P2-Kod".
      THIS-OBJECT:taBortP2Button:ToolTipText = "Tar bort markerad P2-Kod".
      THIS-OBJECT:taBortP2Button:Click:Subscribe(THIS-OBJECT:taBortP2Button_Click).
      /*  */
      /* gridP2 */
      /*  */
      appearance524:BackColor = System.Drawing.SystemColors:Window.
      appearance524:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP2:DisplayLayout:Appearance = appearance524.
      THIS-OBJECT:gridP2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance525:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance525:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance525:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance525:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:Appearance = appearance525.
      appearance526:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance526.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance527:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance527:BackColor2 = System.Drawing.SystemColors:Control.
      appearance527:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance527:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP2:DisplayLayout:GroupByBox:PromptAppearance = appearance527.
      THIS-OBJECT:gridP2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance528:BackColor = System.Drawing.SystemColors:Window.
      appearance528:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveCellAppearance = appearance528.
      appearance529:BackColor = System.Drawing.SystemColors:Highlight.
      appearance529:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:ActiveRowAppearance = appearance529.
      THIS-OBJECT:gridP2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance530:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CardAreaAppearance = appearance530.
      appearance531:BorderColor = System.Drawing.Color:Silver.
      appearance531:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellAppearance = appearance531.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP2:DisplayLayout:Override:CellPadding = 0.
      appearance532:BackColor = System.Drawing.SystemColors:Control.
      appearance532:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance532:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance532:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance532:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP2:DisplayLayout:Override:GroupByRowAppearance = appearance532.
      appearance533:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderAppearance = appearance533.
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance534:BackColor = System.Drawing.SystemColors:Window.
      appearance534:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP2:DisplayLayout:Override:RowAppearance = appearance534.
      THIS-OBJECT:gridP2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance535:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP2:DisplayLayout:Override:TemplateAddRowAppearance = appearance535.
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
      THIS-OBJECT:gridP2:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP2_AfterCellUpdate).
      THIS-OBJECT:gridP2:CellChange:Subscribe(THIS-OBJECT:gridP2_CellChange).
      THIS-OBJECT:gridP2:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP2_BeforeCellUpdate).
      /*  */
      /* TabManagerP2 */
      /*  */
      THIS-OBJECT:TabManagerP2:AllowDrop = TRUE.
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP2).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2resurs).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2frekvens).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2materiel).
      THIS-OBJECT:TabManagerP2:Controls:Add(THIS-OBJECT:TabP2Import).
      THIS-OBJECT:TabManagerP2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP2:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP2:Name = "TabManagerP2".
      THIS-OBJECT:TabManagerP2:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP2.
      THIS-OBJECT:TabManagerP2:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP2:TabIndex = 1.
      appearance536:Image = CAST(resources:GetObject("appearance536.Image"), System.Object).
      ultraTab7:Appearance = appearance536.
      ultraTab7:Key = "P2Resurs".
      ultraTab7:TabPage = THIS-OBJECT:TabP2resurs.
      ultraTab7:Text = "P2-Resurser".
      appearance537:Image = CAST(resources:GetObject("appearance537.Image"), System.Object).
      ultraTab8:Appearance = appearance537.
      ultraTab8:Key = "P2Frekvens".
      ultraTab8:TabPage = THIS-OBJECT:TabP2frekvens.
      ultraTab8:Text = "P2-Frekvenser".
      appearance538:Image = CAST(resources:GetObject("appearance538.Image"), System.Object).
      ultraTab9:Appearance = appearance538.
      ultraTab9:Key = "P2Materiel".
      ultraTab9:TabPage = THIS-OBJECT:TabP2materiel.
      ultraTab9:Text = "P2-Materiel".
      appearance539:Image = CAST(resources:GetObject("appearance539.Image"), System.Object).
      ultraTab28:ActiveAppearance = appearance539.
      appearance540:Image = CAST(resources:GetObject("appearance540.Image"), System.Object).
      ultraTab28:Appearance = appearance540.
      ultraTab28:Key = "Import".
      ultraTab28:TabPage = THIS-OBJECT:TabP2Import.
      ultraTab28:Text = "Import Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar25 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
      arrayvar25[1] = ultraTab7.
      arrayvar25[2] = ultraTab8.
      arrayvar25[3] = ultraTab9.
      arrayvar25[4] = ultraTab28.
      THIS-OBJECT:TabManagerP2:Tabs:AddRange(arrayvar25).
      THIS-OBJECT:TabManagerP2:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP2_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP2 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP2:Name = "ultraTabSharedControlsPageP2".
      THIS-OBJECT:ultraTabSharedControlsPageP2:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* TabP1 */
      /*  */
      THIS-OBJECT:TabP1:Controls:Add(THIS-OBJECT:p1MainSC).
      THIS-OBJECT:TabP1:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabP1:Name = "TabP1".
      THIS-OBJECT:TabP1:Size = NEW System.Drawing.Size(1145, 553).
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
      THIS-OBJECT:p1MainSC:TabIndex = 0.
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
      DEFINE VARIABLE arrayvar26 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar26[1] = THIS-OBJECT:laggTIllP1arbkod.
      arrayvar26[2] = THIS-OBJECT:taBortP1arbkodButton.
      THIS-OBJECT:p1arbkodStrip:Items:AddRange(arrayvar26).
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
      THIS-OBJECT:taBortP1arbkodButton:Size = NEW System.Drawing.Size(140, 22).
      THIS-OBJECT:taBortP1arbkodButton:Text = "Ta bort P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:ToolTipText = "Tar bort markerad P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP1arbkodButton_Click).
      /*  */
      /* gridP1arbkod */
      /*  */
      appearance541:BackColor = System.Drawing.SystemColors:Window.
      appearance541:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Appearance = appearance541.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance542:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance542:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance542:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance542:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:Appearance = appearance542.
      appearance543:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance543.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance544:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance544:BackColor2 = System.Drawing.SystemColors:Control.
      appearance544:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance544:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance544.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance545:BackColor = System.Drawing.SystemColors:Window.
      appearance545:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance545.
      appearance546:BackColor = System.Drawing.SystemColors:Highlight.
      appearance546:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance546.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance547:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CardAreaAppearance = appearance547.
      appearance548:BorderColor = System.Drawing.Color:Silver.
      appearance548:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellAppearance = appearance548.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance549:BackColor = System.Drawing.SystemColors:Control.
      appearance549:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance549:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance549:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance549:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance549.
      appearance550:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderAppearance = appearance550.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance551:BackColor = System.Drawing.SystemColors:Window.
      appearance551:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowAppearance = appearance551.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance552:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance552.
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
      THIS-OBJECT:gridP1arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP1arbkod_AfterCellUpdate).
      THIS-OBJECT:gridP1arbkod:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP1arbkod_BeforeCellUpdate).
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
      DEFINE VARIABLE arrayvar27 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar27[1] = THIS-OBJECT:laggTillP1Button.
      arrayvar27[2] = THIS-OBJECT:taBortP1Button.
      THIS-OBJECT:p1KodertoolStrip:Items:AddRange(arrayvar27).
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
      THIS-OBJECT:taBortP1Button:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:taBortP1Button:Text = "Ta bort P1-Kod".
      THIS-OBJECT:taBortP1Button:ToolTipText = "Tar bort markerad P1-Kod".
      THIS-OBJECT:taBortP1Button:Click:Subscribe(THIS-OBJECT:taBortP1Button_Click).
      /*  */
      /* gridP1 */
      /*  */
      appearance553:BackColor = System.Drawing.SystemColors:Window.
      appearance553:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1:DisplayLayout:Appearance = appearance553.
      THIS-OBJECT:gridP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance554:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance554:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance554:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance554:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:Appearance = appearance554.
      appearance555:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance555.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance556:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance556:BackColor2 = System.Drawing.SystemColors:Control.
      appearance556:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance556:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:PromptAppearance = appearance556.
      THIS-OBJECT:gridP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance557:BackColor = System.Drawing.SystemColors:Window.
      appearance557:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveCellAppearance = appearance557.
      appearance558:BackColor = System.Drawing.SystemColors:Highlight.
      appearance558:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveRowAppearance = appearance558.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance559:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CardAreaAppearance = appearance559.
      appearance560:BorderColor = System.Drawing.Color:Silver.
      appearance560:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellAppearance = appearance560.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellPadding = 0.
      appearance561:BackColor = System.Drawing.SystemColors:Control.
      appearance561:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance561:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance561:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance561:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:GroupByRowAppearance = appearance561.
      appearance562:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderAppearance = appearance562.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance563:BackColor = System.Drawing.SystemColors:Window.
      appearance563:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowAppearance = appearance563.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance564:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance564.
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
      THIS-OBJECT:gridP1:AfterCellUpdate:Subscribe(THIS-OBJECT:gridP1_AfterCellUpdate).
      THIS-OBJECT:gridP1:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridP1_BeforeCellUpdate).
      /*  */
      /* TabManagerP1 */
      /*  */
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPageP1).
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:TabP1frekvens).
      THIS-OBJECT:TabManagerP1:Controls:Add(THIS-OBJECT:TabP1Import).
      THIS-OBJECT:TabManagerP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP1:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP1:Name = "TabManagerP1".
      THIS-OBJECT:TabManagerP1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP1.
      THIS-OBJECT:TabManagerP1:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:TabManagerP1:TabIndex = 2.
      appearance565:Image = CAST(resources:GetObject("appearance565.Image"), System.Object).
      ultraTab6:Appearance = appearance565.
      ultraTab6:Key = "P1Frekvens".
      ultraTab6:TabPage = THIS-OBJECT:TabP1frekvens.
      ultraTab6:Text = "P1-Frekvenser".
      appearance566:Image = CAST(resources:GetObject("appearance566.Image"), System.Object).
      ultraTab29:ActiveAppearance = appearance566.
      appearance567:Image = CAST(resources:GetObject("appearance567.Image"), System.Object).
      ultraTab29:Appearance = appearance567.
      ultraTab29:Key = "Import".
      ultraTab29:TabPage = THIS-OBJECT:TabP1Import.
      ultraTab29:Text = "Import Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar28 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar28[1] = ultraTab6.
      arrayvar28[2] = ultraTab29.
      THIS-OBJECT:TabManagerP1:Tabs:AddRange(arrayvar28).
      THIS-OBJECT:TabManagerP1:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP1_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP1:Name = "ultraTabSharedControlsPageP1".
      THIS-OBJECT:ultraTabSharedControlsPageP1:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* TabExport */
      /*  */
      THIS-OBJECT:TabExport:Controls:Add(THIS-OBJECT:splitContainerTotalExport).
      THIS-OBJECT:TabExport:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabExport:Name = "TabExport".
      THIS-OBJECT:TabExport:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* splitContainerTotalExport */
      /*  */
      THIS-OBJECT:splitContainerTotalExport:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerTotalExport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerTotalExport:Name = "splitContainerTotalExport".
      THIS-OBJECT:splitContainerTotalExport:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerTotalExport:SplitterDistance = 381.
      THIS-OBJECT:splitContainerTotalExport:TabIndex = 0.
      /*  */
      /* TabPopups */
      /*  */
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:buttonOverResurspris).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyPaslagPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyMtrlPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP1arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP2arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP3arbkodPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP1Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP2Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP3Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP4Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyP5Panel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyResursPanel).
      THIS-OBJECT:TabPopups:Controls:Add(THIS-OBJECT:nyNivaPanel).
      THIS-OBJECT:TabPopups:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabPopups:Name = "TabPopups".
      THIS-OBJECT:TabPopups:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* buttonOverResurspris */
      /*  */
      THIS-OBJECT:buttonOverResurspris:Location = NEW System.Drawing.Point(1022, 386).
      THIS-OBJECT:buttonOverResurspris:Name = "buttonOverResurspris".
      THIS-OBJECT:buttonOverResurspris:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonOverResurspris:TabIndex = 13.
      THIS-OBJECT:buttonOverResurspris:Text = "buttonOver1".
      /*  */
      /* nyPaslagPanel */
      /*  */
      THIS-OBJECT:nyPaslagPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyPaslagPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyPaslag).
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinPaslagBenamning).
      THIS-OBJECT:nyPaslagPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyPaslag).
      THIS-OBJECT:nyPaslagPanel:Location = NEW System.Drawing.Point(3, 398).
      THIS-OBJECT:nyPaslagPanel:Name = "nyPaslagPanel".
      THIS-OBJECT:nyPaslagPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyPaslagPanel:TabIndex = 12.
      THIS-OBJECT:nyPaslagPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyPaslag */
      /*  */
      THIS-OBJECT:buttonCancelNyPaslag:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyPaslag:Name = "buttonCancelNyPaslag".
      THIS-OBJECT:buttonCancelNyPaslag:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyPaslag:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyPaslag:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyPaslag:Click:Subscribe(THIS-OBJECT:buttonCancelNyPaslag_Click).
      /*  */
      /* fillinPaslagBenamning */
      /*  */
      THIS-OBJECT:fillinPaslagBenamning:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinPaslagBenamning:Name = "fillinPaslagBenamning".
      THIS-OBJECT:fillinPaslagBenamning:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinPaslagBenamning:TabIndex = 1.
      /*  */
      /* buttonNyPaslag */
      /*  */
      THIS-OBJECT:buttonNyPaslag:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyPaslag:Name = "buttonNyPaslag".
      THIS-OBJECT:buttonNyPaslag:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyPaslag:TabIndex = 3.
      THIS-OBJECT:buttonNyPaslag:Text = "Lägg till".
      THIS-OBJECT:buttonNyPaslag:Click:Subscribe(THIS-OBJECT:buttonNyPaslag_Click).
      /*  */
      /* nyMtrlPanel */
      /*  */
      THIS-OBJECT:nyMtrlPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyMtrlPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyMtrl).
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinArtNr).
      THIS-OBJECT:nyMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyMtrl).
      THIS-OBJECT:nyMtrlPanel:Location = NEW System.Drawing.Point(267, 3).
      THIS-OBJECT:nyMtrlPanel:Name = "nyMtrlPanel".
      THIS-OBJECT:nyMtrlPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyMtrlPanel:TabIndex = 11.
      THIS-OBJECT:nyMtrlPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyMtrl */
      /*  */
      THIS-OBJECT:buttonCancelNyMtrl:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyMtrl:Name = "buttonCancelNyMtrl".
      THIS-OBJECT:buttonCancelNyMtrl:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyMtrl:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyMtrl:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyMtrl:Click:Subscribe(THIS-OBJECT:buttonCancelNyMtrl_Click).
      /*  */
      /* fillinArtNr */
      /*  */
      THIS-OBJECT:fillinArtNr:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinArtNr:Name = "fillinArtNr".
      THIS-OBJECT:fillinArtNr:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinArtNr:TabIndex = 1.
      /*  */
      /* buttonNyMtrl */
      /*  */
      THIS-OBJECT:buttonNyMtrl:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyMtrl:Name = "buttonNyMtrl".
      THIS-OBJECT:buttonNyMtrl:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyMtrl:TabIndex = 3.
      THIS-OBJECT:buttonNyMtrl:Text = "Lägg till".
      THIS-OBJECT:buttonNyMtrl:Click:Subscribe(THIS-OBJECT:buttonNyMtrl_Click).
      /*  */
      /* nyP1arbkodPanel */
      /*  */
      THIS-OBJECT:nyP1arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP1arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP1arbkod).
      THIS-OBJECT:nyP1arbkodPanel:Location = NEW System.Drawing.Point(809, 51).
      THIS-OBJECT:nyP1arbkodPanel:Name = "nyP1arbkodPanel".
      THIS-OBJECT:nyP1arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP1arbkodPanel:TabIndex = 10.
      THIS-OBJECT:nyP1arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP1arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP1arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP1arbkod:Name = "buttonCancelNyP1arbkod".
      THIS-OBJECT:buttonCancelNyP1arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP1arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP1arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP1arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP1arbkod_Click).
      /*  */
      /* fillinP1arbkod */
      /*  */
      THIS-OBJECT:fillinP1arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP1arbkod:Name = "fillinP1arbkod".
      THIS-OBJECT:fillinP1arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP1arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP1arbkod */
      /*  */
      THIS-OBJECT:buttonNyP1arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP1arbkod:Name = "buttonNyP1arbkod".
      THIS-OBJECT:buttonNyP1arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP1arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP1arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP1arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP1arbkod_Click).
      /*  */
      /* nyP2arbkodPanel */
      /*  */
      THIS-OBJECT:nyP2arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP2arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP2arbkod).
      THIS-OBJECT:nyP2arbkodPanel:Location = NEW System.Drawing.Point(533, 157).
      THIS-OBJECT:nyP2arbkodPanel:Name = "nyP2arbkodPanel".
      THIS-OBJECT:nyP2arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP2arbkodPanel:TabIndex = 9.
      THIS-OBJECT:nyP2arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP2arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP2arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP2arbkod:Name = "buttonCancelNyP2arbkod".
      THIS-OBJECT:buttonCancelNyP2arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP2arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP2arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP2arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP2arbkod_Click).
      /*  */
      /* fillinP2arbkod */
      /*  */
      THIS-OBJECT:fillinP2arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP2arbkod:Name = "fillinP2arbkod".
      THIS-OBJECT:fillinP2arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP2arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP2arbkod */
      /*  */
      THIS-OBJECT:buttonNyP2arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP2arbkod:Name = "buttonNyP2arbkod".
      THIS-OBJECT:buttonNyP2arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP2arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP2arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP2arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP2arbkod_Click).
      /*  */
      /* nyP3arbkodPanel */
      /*  */
      THIS-OBJECT:nyP3arbkodPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP3arbkodPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP3arbkod).
      THIS-OBJECT:nyP3arbkodPanel:Location = NEW System.Drawing.Point(533, 51).
      THIS-OBJECT:nyP3arbkodPanel:Name = "nyP3arbkodPanel".
      THIS-OBJECT:nyP3arbkodPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP3arbkodPanel:TabIndex = 8.
      THIS-OBJECT:nyP3arbkodPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP3arbkod */
      /*  */
      THIS-OBJECT:buttonCancelNyP3arbkod:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP3arbkod:Name = "buttonCancelNyP3arbkod".
      THIS-OBJECT:buttonCancelNyP3arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP3arbkod:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP3arbkod:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP3arbkod:Click:Subscribe(THIS-OBJECT:buttonCancelNyP3arbkod_Click).
      /*  */
      /* fillinP3arbkod */
      /*  */
      THIS-OBJECT:fillinP3arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP3arbkod:Name = "fillinP3arbkod".
      THIS-OBJECT:fillinP3arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP3arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP3arbkod */
      /*  */
      THIS-OBJECT:buttonNyP3arbkod:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP3arbkod:Name = "buttonNyP3arbkod".
      THIS-OBJECT:buttonNyP3arbkod:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP3arbkod:TabIndex = 3.
      THIS-OBJECT:buttonNyP3arbkod:Text = "Lägg till".
      THIS-OBJECT:buttonNyP3arbkod:Click:Subscribe(THIS-OBJECT:buttonNyP3arbkod_Click).
      /*  */
      /* nyP1Panel */
      /*  */
      THIS-OBJECT:nyP1Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP1Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP1ArbKod).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP1LopNr).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP1).
      THIS-OBJECT:nyP1Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP1).
      THIS-OBJECT:nyP1Panel:Location = NEW System.Drawing.Point(533, 361).
      THIS-OBJECT:nyP1Panel:Name = "nyP1Panel".
      THIS-OBJECT:nyP1Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP1Panel:TabIndex = 7.
      THIS-OBJECT:nyP1Panel:Visible = FALSE.
      /*  */
      /* comboP1ArbKod */
      /*  */
      THIS-OBJECT:comboP1ArbKod:Location = NEW System.Drawing.Point(4, 38).
      THIS-OBJECT:comboP1ArbKod:Name = "comboP1ArbKod".
      THIS-OBJECT:comboP1ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP1ArbKod:TabIndex = 8.
      /*  */
      /* fillinP1LopNr */
      /*  */
      THIS-OBJECT:fillinP1LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP1LopNr:Name = "fillinP1LopNr".
      THIS-OBJECT:fillinP1LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP1LopNr:TabIndex = 7.
      /*  */
      /* buttonCancelNyP1 */
      /*  */
      THIS-OBJECT:buttonCancelNyP1:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP1:Name = "buttonCancelNyP1".
      THIS-OBJECT:buttonCancelNyP1:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP1:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP1:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP1:Click:Subscribe(THIS-OBJECT:buttonCancelNyP1_Click).
      /*  */
      /* buttonNyP1 */
      /*  */
      THIS-OBJECT:buttonNyP1:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP1:Name = "buttonNyP1".
      THIS-OBJECT:buttonNyP1:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP1:TabIndex = 3.
      THIS-OBJECT:buttonNyP1:Text = "Lägg till".
      THIS-OBJECT:buttonNyP1:Click:Subscribe(THIS-OBJECT:buttonNyP1_Click).
      /*  */
      /* nyP2Panel */
      /*  */
      THIS-OBJECT:nyP2Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP2Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP2ArbKod).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP2LopNr).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP2).
      THIS-OBJECT:nyP2Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP2).
      THIS-OBJECT:nyP2Panel:Location = NEW System.Drawing.Point(267, 180).
      THIS-OBJECT:nyP2Panel:Name = "nyP2Panel".
      THIS-OBJECT:nyP2Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP2Panel:TabIndex = 6.
      THIS-OBJECT:nyP2Panel:Visible = FALSE.
      /*  */
      /* comboP2ArbKod */
      /*  */
      THIS-OBJECT:comboP2ArbKod:Location = NEW System.Drawing.Point(4, 30).
      THIS-OBJECT:comboP2ArbKod:Name = "comboP2ArbKod".
      THIS-OBJECT:comboP2ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP2ArbKod:TabIndex = 7.
      /*  */
      /* fillinP2LopNr */
      /*  */
      THIS-OBJECT:fillinP2LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP2LopNr:Name = "fillinP2LopNr".
      THIS-OBJECT:fillinP2LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP2LopNr:TabIndex = 6.
      /*  */
      /* buttonCancelNyP2 */
      /*  */
      THIS-OBJECT:buttonCancelNyP2:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP2:Name = "buttonCancelNyP2".
      THIS-OBJECT:buttonCancelNyP2:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP2:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP2:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP2:Click:Subscribe(THIS-OBJECT:buttonCancelNyP2_Click).
      /*  */
      /* buttonNyP2 */
      /*  */
      THIS-OBJECT:buttonNyP2:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP2:Name = "buttonNyP2".
      THIS-OBJECT:buttonNyP2:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP2:TabIndex = 3.
      THIS-OBJECT:buttonNyP2:Text = "Lägg till".
      THIS-OBJECT:buttonNyP2:Click:Subscribe(THIS-OBJECT:buttonNyP2_Click).
      /*  */
      /* nyP3Panel */
      /*  */
      THIS-OBJECT:nyP3Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP3Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:comboP3ArbKod).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP3LopNr).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP3).
      THIS-OBJECT:nyP3Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP3).
      THIS-OBJECT:nyP3Panel:Location = NEW System.Drawing.Point(267, 361).
      THIS-OBJECT:nyP3Panel:Name = "nyP3Panel".
      THIS-OBJECT:nyP3Panel:Size = NEW System.Drawing.Size(260, 175).
      THIS-OBJECT:nyP3Panel:TabIndex = 5.
      THIS-OBJECT:nyP3Panel:Visible = FALSE.
      /*  */
      /* comboP3ArbKod */
      /*  */
      THIS-OBJECT:comboP3ArbKod:Location = NEW System.Drawing.Point(4, 38).
      THIS-OBJECT:comboP3ArbKod:Name = "comboP3ArbKod".
      THIS-OBJECT:comboP3ArbKod:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboP3ArbKod:TabIndex = 6.
      /*  */
      /* fillinP3LopNr */
      /*  */
      THIS-OBJECT:fillinP3LopNr:Location = NEW System.Drawing.Point(4, 87).
      THIS-OBJECT:fillinP3LopNr:Name = "fillinP3LopNr".
      THIS-OBJECT:fillinP3LopNr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinP3LopNr:TabIndex = 5.
      /*  */
      /* buttonCancelNyP3 */
      /*  */
      THIS-OBJECT:buttonCancelNyP3:Location = NEW System.Drawing.Point(169, 140).
      THIS-OBJECT:buttonCancelNyP3:Name = "buttonCancelNyP3".
      THIS-OBJECT:buttonCancelNyP3:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP3:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP3:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP3:Click:Subscribe(THIS-OBJECT:buttonCancelNyP3_Click).
      /*  */
      /* buttonNyP3 */
      /*  */
      THIS-OBJECT:buttonNyP3:Location = NEW System.Drawing.Point(92, 140).
      THIS-OBJECT:buttonNyP3:Name = "buttonNyP3".
      THIS-OBJECT:buttonNyP3:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP3:TabIndex = 3.
      THIS-OBJECT:buttonNyP3:Text = "Lägg till".
      THIS-OBJECT:buttonNyP3:Click:Subscribe(THIS-OBJECT:buttonNyP3_Click).
      /*  */
      /* nyP4Panel */
      /*  */
      THIS-OBJECT:nyP4Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP4Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP4).
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP4arbkod).
      THIS-OBJECT:nyP4Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP4).
      THIS-OBJECT:nyP4Panel:Location = NEW System.Drawing.Point(1, 290).
      THIS-OBJECT:nyP4Panel:Name = "nyP4Panel".
      THIS-OBJECT:nyP4Panel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP4Panel:TabIndex = 4.
      THIS-OBJECT:nyP4Panel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP4 */
      /*  */
      THIS-OBJECT:buttonCancelNyP4:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP4:Name = "buttonCancelNyP4".
      THIS-OBJECT:buttonCancelNyP4:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP4:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP4:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP4:Click:Subscribe(THIS-OBJECT:buttonCancelNyP4_Click).
      /*  */
      /* fillinP4arbkod */
      /*  */
      THIS-OBJECT:fillinP4arbkod:Location = NEW System.Drawing.Point(4, 10).
      THIS-OBJECT:fillinP4arbkod:Name = "fillinP4arbkod".
      THIS-OBJECT:fillinP4arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP4arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP4 */
      /*  */
      THIS-OBJECT:buttonNyP4:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP4:Name = "buttonNyP4".
      THIS-OBJECT:buttonNyP4:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP4:TabIndex = 3.
      THIS-OBJECT:buttonNyP4:Text = "Lägg till".
      THIS-OBJECT:buttonNyP4:Click:Subscribe(THIS-OBJECT:buttonNyP4_Click).
      /*  */
      /* nyP5Panel */
      /*  */
      THIS-OBJECT:nyP5Panel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyP5Panel.ClientArea */
      /*  */
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyP5).
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:fillinP5arbkod).
      THIS-OBJECT:nyP5Panel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyP5).
      THIS-OBJECT:nyP5Panel:Location = NEW System.Drawing.Point(1, 184).
      THIS-OBJECT:nyP5Panel:Name = "nyP5Panel".
      THIS-OBJECT:nyP5Panel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyP5Panel:TabIndex = 3.
      THIS-OBJECT:nyP5Panel:Visible = FALSE.
      /*  */
      /* buttonCancelNyP5 */
      /*  */
      THIS-OBJECT:buttonCancelNyP5:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyP5:Name = "buttonCancelNyP5".
      THIS-OBJECT:buttonCancelNyP5:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyP5:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyP5:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyP5:Click:Subscribe(THIS-OBJECT:buttonCancelNyP5_Click).
      /*  */
      /* fillinP5arbkod */
      /*  */
      THIS-OBJECT:fillinP5arbkod:Location = NEW System.Drawing.Point(1, 14).
      THIS-OBJECT:fillinP5arbkod:Name = "fillinP5arbkod".
      THIS-OBJECT:fillinP5arbkod:Size = NEW System.Drawing.Size(240, 40).
      THIS-OBJECT:fillinP5arbkod:TabIndex = 1.
      /*  */
      /* buttonNyP5 */
      /*  */
      THIS-OBJECT:buttonNyP5:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyP5:Name = "buttonNyP5".
      THIS-OBJECT:buttonNyP5:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyP5:TabIndex = 3.
      THIS-OBJECT:buttonNyP5:Text = "Lägg till".
      THIS-OBJECT:buttonNyP5:Click:Subscribe(THIS-OBJECT:buttonNyP5_Click).
      /*  */
      /* nyResursPanel */
      /*  */
      THIS-OBJECT:nyResursPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyResursPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyResurs).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinResursNr).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:fillinResursBenamning).
      THIS-OBJECT:nyResursPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyResurs).
      THIS-OBJECT:nyResursPanel:Location = NEW System.Drawing.Point(1, 3).
      THIS-OBJECT:nyResursPanel:Name = "nyResursPanel".
      THIS-OBJECT:nyResursPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyResursPanel:TabIndex = 2.
      THIS-OBJECT:nyResursPanel:Visible = FALSE.
      /*  */
      /* buttonCancelNyResurs */
      /*  */
      THIS-OBJECT:buttonCancelNyResurs:Location = NEW System.Drawing.Point(168, 62).
      THIS-OBJECT:buttonCancelNyResurs:Name = "buttonCancelNyResurs".
      THIS-OBJECT:buttonCancelNyResurs:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyResurs:TabIndex = 5.
      THIS-OBJECT:buttonCancelNyResurs:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyResurs:Click:Subscribe(THIS-OBJECT:buttonCancelNyResurs_Click).
      /*  */
      /* fillinResursNr */
      /*  */
      THIS-OBJECT:fillinResursNr:Location = NEW System.Drawing.Point(4, 10).
      THIS-OBJECT:fillinResursNr:Name = "fillinResursNr".
      THIS-OBJECT:fillinResursNr:Size = NEW System.Drawing.Size(240, 26).
      THIS-OBJECT:fillinResursNr:TabIndex = 1.
      /*  */
      /* fillinResursBenamning */
      /*  */
      THIS-OBJECT:fillinResursBenamning:Location = NEW System.Drawing.Point(4, 37).
      THIS-OBJECT:fillinResursBenamning:Name = "fillinResursBenamning".
      THIS-OBJECT:fillinResursBenamning:Size = NEW System.Drawing.Size(240, 26).
      THIS-OBJECT:fillinResursBenamning:TabIndex = 2.
      /*  */
      /* buttonNyResurs */
      /*  */
      THIS-OBJECT:buttonNyResurs:Location = NEW System.Drawing.Point(91, 62).
      THIS-OBJECT:buttonNyResurs:Name = "buttonNyResurs".
      THIS-OBJECT:buttonNyResurs:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyResurs:TabIndex = 2.
      THIS-OBJECT:buttonNyResurs:Text = "Lägg till".
      THIS-OBJECT:buttonNyResurs:Click:Subscribe(THIS-OBJECT:buttonNyResurs_Click).
      /*  */
      /* nyNivaPanel */
      /*  */
      THIS-OBJECT:nyNivaPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* nyNivaPanel.ClientArea */
      /*  */
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:comboNiva).
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelNyNiva).
      THIS-OBJECT:nyNivaPanel:ClientArea:Controls:Add(THIS-OBJECT:buttonNyNiva).
      THIS-OBJECT:nyNivaPanel:Location = NEW System.Drawing.Point(812, 157).
      THIS-OBJECT:nyNivaPanel:Name = "nyNivaPanel".
      THIS-OBJECT:nyNivaPanel:Size = NEW System.Drawing.Size(260, 100).
      THIS-OBJECT:nyNivaPanel:TabIndex = 12.
      THIS-OBJECT:nyNivaPanel:Visible = FALSE.
      /*  */
      /* comboNiva */
      /*  */
      THIS-OBJECT:comboNiva:Location = NEW System.Drawing.Point(4, 14).
      THIS-OBJECT:comboNiva:Name = "comboNiva".
      THIS-OBJECT:comboNiva:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboNiva:TabIndex = 8.
      /*  */
      /* buttonCancelNyNiva */
      /*  */
      THIS-OBJECT:buttonCancelNyNiva:Location = NEW System.Drawing.Point(169, 65).
      THIS-OBJECT:buttonCancelNyNiva:Name = "buttonCancelNyNiva".
      THIS-OBJECT:buttonCancelNyNiva:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonCancelNyNiva:TabIndex = 4.
      THIS-OBJECT:buttonCancelNyNiva:Text = "Avbryt".
      THIS-OBJECT:buttonCancelNyNiva:Click:Subscribe(THIS-OBJECT:buttonCancelNyNiva_Click).
      /*  */
      /* buttonNyNiva */
      /*  */
      THIS-OBJECT:buttonNyNiva:Location = NEW System.Drawing.Point(92, 65).
      THIS-OBJECT:buttonNyNiva:Name = "buttonNyNiva".
      THIS-OBJECT:buttonNyNiva:Size = NEW System.Drawing.Size(75, 30).
      THIS-OBJECT:buttonNyNiva:TabIndex = 3.
      THIS-OBJECT:buttonNyNiva:Text = "Lägg till".
      THIS-OBJECT:buttonNyNiva:Click:Subscribe(THIS-OBJECT:buttonNyNiva_Click).
      /*  */
      /* laggTillPaslagButton */
      /*  */
      THIS-OBJECT:laggTillPaslagButton:Image = CAST(resources:GetObject("laggTillPaslagButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillPaslagButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillPaslagButton:Name = "laggTillPaslagButton".
      THIS-OBJECT:laggTillPaslagButton:Size = NEW System.Drawing.Size(106, 22).
      THIS-OBJECT:laggTillPaslagButton:Text = "Lägg till påslag".
      THIS-OBJECT:laggTillPaslagButton:Click:Subscribe(THIS-OBJECT:laggTillPaslagButton_Click).
      /*  */
      /* taBortPaslagButton */
      /*  */
      THIS-OBJECT:taBortPaslagButton:Image = CAST(resources:GetObject("taBortPaslagButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortPaslagButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortPaslagButton:Name = "taBortPaslagButton".
      THIS-OBJECT:taBortPaslagButton:Size = NEW System.Drawing.Size(102, 22).
      THIS-OBJECT:taBortPaslagButton:Text = "Ta bort påslag".
      THIS-OBJECT:taBortPaslagButton:Click:Subscribe(THIS-OBJECT:taBortPaslagButton_Click).
      /*  */
      /* miniToolStrip */
      /*  */
      THIS-OBJECT:miniToolStrip:AutoSize = FALSE.
      THIS-OBJECT:miniToolStrip:CanOverflow = FALSE.
      THIS-OBJECT:miniToolStrip:Dock = System.Windows.Forms.DockStyle:None.
      THIS-OBJECT:miniToolStrip:GripStyle = System.Windows.Forms.ToolStripGripStyle:Hidden.
      THIS-OBJECT:miniToolStrip:Location = NEW System.Drawing.Point(55, 3).
      THIS-OBJECT:miniToolStrip:Name = "miniToolStrip".
      THIS-OBJECT:miniToolStrip:Size = NEW System.Drawing.Size(592, 25).
      THIS-OBJECT:miniToolStrip:TabIndex = 2.
      /*  */
      /* TabDelad */
      /*  */
      THIS-OBJECT:TabDelad:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabDelad:Name = "TabDelad".
      THIS-OBJECT:TabDelad:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabManagerEkg */
      /*  */
      appearance568:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManagerEkg:Appearance = appearance568.
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabDelad).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabResurser).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP2).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP3).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP4).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP1).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabP5).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabMtrl).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabResursPriser).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabExport).
      THIS-OBJECT:TabManagerEkg:Controls:Add(THIS-OBJECT:TabPopups).
      THIS-OBJECT:TabManagerEkg:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerEkg:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManagerEkg:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerEkg:Name = "TabManagerEkg".
      THIS-OBJECT:TabManagerEkg:SharedControlsPage = THIS-OBJECT:TabDelad.
      THIS-OBJECT:TabManagerEkg:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManagerEkg:TabIndex = 45.
      THIS-OBJECT:TabManagerEkg:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance569:Image = CAST(resources:GetObject("appearance569.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance569.
      appearance570:Image = CAST(resources:GetObject("appearance570.Image"), System.Object).
      ultraTab14:Appearance = appearance570.
      ultraTab14:Key = "Resurser".
      ultraTab14:TabPage = THIS-OBJECT:TabResurser.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Resurser".
      appearance571:Image = CAST(resources:GetObject("appearance571.Image"), System.Object).
      ultraTab13:ActiveAppearance = appearance571.
      appearance572:Image = CAST(resources:GetObject("appearance572.Image"), System.Object).
      ultraTab13:Appearance = appearance572.
      ultraTab13:Key = "Kataloger".
      ultraTab13:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab13:Text = "".
      ultraTab13:ToolTipText = "Kataloger".
      appearance573:Image = CAST(resources:GetObject("appearance573.Image"), System.Object).
      ultraTab2:Appearance = appearance573.
      ultraTab2:Key = "Resurspris".
      ultraTab2:TabPage = THIS-OBJECT:TabResursPriser.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Resurspris".
      appearance574:Image = CAST(resources:GetObject("appearance574.Image"), System.Object).
      ultraTab3:Appearance = appearance574.
      ultraTab3:Key = "Materiel".
      ultraTab3:TabPage = THIS-OBJECT:TabMtrl.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Materiel".
      appearance575:Image = CAST(resources:GetObject("appearance575.Image"), System.Object).
      ultraTab20:ActiveAppearance = appearance575.
      appearance576:Image = CAST(resources:GetObject("appearance576.Image"), System.Object).
      ultraTab20:Appearance = appearance576.
      ultraTab20:Key = "P5".
      ultraTab20:TabPage = THIS-OBJECT:TabP5.
      ultraTab20:Text = "".
      ultraTab20:ToolTipText = "P5".
      appearance577:Image = CAST(resources:GetObject("appearance577.Image"), System.Object).
      ultraTab22:ActiveAppearance = appearance577.
      appearance578:Image = CAST(resources:GetObject("appearance578.Image"), System.Object).
      ultraTab22:Appearance = appearance578.
      ultraTab22:Key = "P4".
      ultraTab22:TabPage = THIS-OBJECT:TabP4.
      ultraTab22:Text = "".
      ultraTab22:ToolTipText = "P4".
      appearance579:Image = CAST(resources:GetObject("appearance579.Image"), System.Object).
      ultraTab19:ActiveAppearance = appearance579.
      appearance580:Image = CAST(resources:GetObject("appearance580.Image"), System.Object).
      ultraTab19:Appearance = appearance580.
      ultraTab19:Key = "P3".
      ultraTab19:TabPage = THIS-OBJECT:TabP3.
      ultraTab19:Text = "".
      ultraTab19:ToolTipText = "P3".
      appearance581:Image = CAST(resources:GetObject("appearance581.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance581.
      appearance582:Image = CAST(resources:GetObject("appearance582.Image"), System.Object).
      ultraTab17:Appearance = appearance582.
      ultraTab17:Key = "P2".
      ultraTab17:TabPage = THIS-OBJECT:TabP2.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "P2".
      appearance583:Image = CAST(resources:GetObject("appearance583.Image"), System.Object).
      ultraTab16:ActiveAppearance = appearance583.
      appearance584:Image = CAST(resources:GetObject("appearance584.Image"), System.Object).
      ultraTab16:Appearance = appearance584.
      ultraTab16:Key = "P1".
      ultraTab16:TabPage = THIS-OBJECT:TabP1.
      ultraTab16:Text = "".
      ultraTab16:ToolTipText = "P1".
      ultraTab25:Key = "Export".
      ultraTab25:TabPage = THIS-OBJECT:TabExport.
      ultraTab25:Text = "Exportera".
      ultraTab4:Key = "TabPopups".
      ultraTab4:TabPage = THIS-OBJECT:TabPopups.
      ultraTab4:Text = "TabPopups".
      ultraTab4:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar29 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 11 NO-UNDO.
      arrayvar29[1] = ultraTab14.
      arrayvar29[2] = ultraTab13.
      arrayvar29[3] = ultraTab2.
      arrayvar29[4] = ultraTab3.
      arrayvar29[5] = ultraTab20.
      arrayvar29[6] = ultraTab22.
      arrayvar29[7] = ultraTab19.
      arrayvar29[8] = ultraTab17.
      arrayvar29[9] = ultraTab16.
      arrayvar29[10] = ultraTab25.
      arrayvar29[11] = ultraTab4.
      THIS-OBJECT:TabManagerEkg:Tabs:AddRange(arrayvar29).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanging).
      THIS-OBJECT:TabManagerEkg:SelectedTabChanged:Subscribe(THIS-OBJECT:TabManagerEkg_SelectedTabChanged).
      /*  */
      /* nyResursPop */
      /*  */
      THIS-OBJECT:nyResursPop:PopupControl = THIS-OBJECT:nyResursPanel.
      /*  */
      /* nyP5Pop */
      /*  */
      THIS-OBJECT:nyP5Pop:PopupControl = THIS-OBJECT:nyP5Panel.
      /*  */
      /* nyP4Pop */
      /*  */
      THIS-OBJECT:nyP4Pop:PopupControl = THIS-OBJECT:nyP4Panel.
      /*  */
      /* nyP3Pop */
      /*  */
      THIS-OBJECT:nyP3Pop:PopupControl = THIS-OBJECT:nyP3Panel.
      /*  */
      /* nyP2Pop */
      /*  */
      THIS-OBJECT:nyP2Pop:PopupControl = THIS-OBJECT:nyP2Panel.
      /*  */
      /* nyP1Pop */
      /*  */
      THIS-OBJECT:nyP1Pop:PopupControl = THIS-OBJECT:nyP1Panel.
      /*  */
      /* nyP3arbkodPop */
      /*  */
      THIS-OBJECT:nyP3arbkodPop:PopupControl = THIS-OBJECT:nyP3arbkodPanel.
      /*  */
      /* nyP2arbkodPop */
      /*  */
      THIS-OBJECT:nyP2arbkodPop:PopupControl = THIS-OBJECT:nyP2arbkodPanel.
      /*  */
      /* nyP1arbkodPop */
      /*  */
      THIS-OBJECT:nyP1arbkodPop:PopupControl = THIS-OBJECT:nyP1arbkodPanel.
      /*  */
      /* nyPaslagPop */
      /*  */
      THIS-OBJECT:nyPaslagPop:PopupControl = THIS-OBJECT:nyPaslagPanel.
      /*  */
      /* nyMtrlPop */
      /*  */
      THIS-OBJECT:nyMtrlPop:PopupControl = THIS-OBJECT:nyMtrlPanel.
      /*  */
      /* nyNivaPop */
      /*  */
      THIS-OBJECT:nyNivaPop:PopupControl = THIS-OBJECT:nyNivaPanel.
      /*  */
      /* EkgShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManagerEkg).
      THIS-OBJECT:Name = "EkgShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabRPPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslagOver:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:paslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurspaslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPNiva:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:PerformLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPRegel:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:regelMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:regelMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBasresurser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBasresurser:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripBasresurser:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripBasresurser:PerformLayout().
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP5resurs:ResumeLayout(FALSE).
      THIS-OBJECT:p5ResursSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p5ResursSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p5ResursSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p5ResursSC:ResumeLayout(FALSE).
      THIS-OBJECT:p5ResursSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p5ResursSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p5ResursSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p5ResursSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursForP5, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP5Resurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP5Import:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP5ImportGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP5ImportGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP5ImportGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP5ImportGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP5ImportGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P5ImportStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P5ImportStrip:PerformLayout().
      THIS-OBJECT:splitContainerP5Import:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP5Import, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP5Import:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridImportP5, System.ComponentModel.ISupportInitialize):EndInit().
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
      THIS-OBJECT:TabP3Resurs:ResumeLayout(FALSE).
      THIS-OBJECT:p3ResursSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3ResursSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3ResursSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3ResursSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3ResursSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3ResursSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3ResursSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3ResursSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursForP3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP3Resurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP3Frekvens:ResumeLayout(FALSE).
      THIS-OBJECT:p3FrekvensSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3FrekvensSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3FrekvensSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3FrekvensSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3FrekvensSC2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3FrekvensSC2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3FrekvensSC2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3FrekvensSC2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridP4ForP3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridP3Frekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabP3Import:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP3ImportGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerP3ImportGrund:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerP3ImportGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP3ImportGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP3ImportGrund:ResumeLayout(FALSE).
      THIS-OBJECT:P3ImportStrip:ResumeLayout(FALSE).
      THIS-OBJECT:P3ImportStrip:PerformLayout().
      THIS-OBJECT:splitContainerP3Import:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerP3Import, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerP3Import:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridImportP3F, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP3, System.ComponentModel.ISupportInitialize):EndInit().
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
      CAST(THIS-OBJECT:GridImportP2F, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridImportP2, System.ComponentModel.ISupportInitialize):EndInit().
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
      THIS-OBJECT:TabResurser:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:PerformLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogKatalogSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabResursPriser:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPrisLista:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerResurspriser:ResumeLayout(FALSE).
      THIS-OBJECT:TabMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:mtrlMainSC:Panel1:ResumeLayout(FALSE).
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
      THIS-OBJECT:TabP5:ResumeLayout(FALSE).
      THIS-OBJECT:p5MainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p5MainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p5MainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p5MainSC:ResumeLayout(FALSE).
      THIS-OBJECT:p5KoderMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p5KoderMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:p5KoderMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p5KoderMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p5KoderMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:p5KoderStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p5KoderStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP5, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerP5, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerP5:ResumeLayout(FALSE).
      THIS-OBJECT:TabP4:ResumeLayout(FALSE).
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
      THIS-OBJECT:TabP3:ResumeLayout(FALSE).
      THIS-OBJECT:p3MainSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3MainSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3MainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3MainSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3kodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3kodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3kodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3kodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3arbkodSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3arbkodSC:Panel1:PerformLayout().
      THIS-OBJECT:p3arbkodSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3arbkodSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3arbkodSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3arbkodStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p3arbkodStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP3arbkod, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3KoderMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:p3KoderMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:p3KoderMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:p3KoderMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p3KoderMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:p3KodertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p3KodertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridP3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerP3, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerP3:ResumeLayout(FALSE).
      THIS-OBJECT:TabP2:ResumeLayout(FALSE).
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
      THIS-OBJECT:TabP1:ResumeLayout(FALSE).
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
      THIS-OBJECT:TabExport:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerTotalExport, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerTotalExport:ResumeLayout(FALSE).
      THIS-OBJECT:TabPopups:ResumeLayout(FALSE).
      THIS-OBJECT:nyPaslagPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyPaslagPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyMtrlPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyMtrlPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3arbkodPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3arbkodPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP1Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP2Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP3Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP4Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP4Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyP5Panel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyP5Panel:ResumeLayout(FALSE).
      THIS-OBJECT:nyResursPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyResursPanel:ResumeLayout(FALSE).
      THIS-OBJECT:nyNivaPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:nyNivaPanel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:TabManagerEkg, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerEkg:ResumeLayout(FALSE).
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
   METHOD PRIVATE VOID StripButtonP3Rbort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("P3").
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID StripButtonP3Rin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P3").
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
	METHOD PRIVATE VOID StripButtonP5Import_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonImport_Click("P5").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID StripButtonP5Rbort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonRbort_Click("P5").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID StripButtonP5Rin_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:StripButtonRin_Click("P5").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManagerEkg_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
	  /*
	  IF VALID-OBJECT(TabManagerEkg:SelectedTab) THEN DO:
	     MESSAGE "changed" THIS-OBJECT:TabManagerEkg:SelectedTab:KEY e:TAB:KEY
	     THIS-OBJECT:TabManagerEkg:Tabs[e:TAB:KEY]:Selected
        VIEW-AS ALERT-BOX.
     END. 
       
	  
	  PROCESS EVENTS.
     */
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManagerEkg_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		IF VALID-OBJECT(TabManagerEkg:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabEkg_SelectedTabChanging(INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY).    
      END.        
		PROCESS EVENTS.
	END METHOD.
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP1_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP1:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP1:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP2_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP2:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP2:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP3_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP3:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP3:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManagerP4_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP4:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP4:SelectedTab:KEY).    
      END. 
      RETURN.

   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManagerP5_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		IF VALID-OBJECT(TabManagerP5:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP5:SelectedTab:KEY).    
      END. 
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TaBortKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:TaBortKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID taBortNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   	   
		THIS-OBJECT:ControlWinLg:TaBortresursniva(INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["RESURSNR"]:Text), INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["NIVA"]:Text)).      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID taBortP5Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:TaBortP5().
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
  
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP3arbkodButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP3ArbKod().
      RETURN.

   END METHOD.
   
    
  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortP3Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortP3().
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
	METHOD PRIVATE VOID taBortPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:TaBortPaslag().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TaBortResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   
		THIS-OBJECT:ControlDataDb:TaBortResurs(INTEGER(THIS-OBJECT:GridResurs:ActiveRow:Cells["RESURSNR"]:Text)).
      THIS-OBJECT:GridResurs:GuruFiltrera().
		RETURN.

	END METHOD.
	
	  /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortResursprisButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortResurspris(INTEGER(THIS-OBJECT:GridResurspris:ActiveRow:Cells["RESURSNR"]:Text)).            
      RETURN.

   END METHOD.

 /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonNyResurspris_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):      
      THIS-OBJECT:ControlWinLg:NyResurspris().      

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
  

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStrip2_ItemClicked( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.ToolStripItemClickedEventArgs ):
		RETURN.
	END METHOD.
	
	METHOD PUBLIC VOID StartaExport():   
      EkgExport = NEW Modules.EkgData.EkgExport(THIS-OBJECT:Root).
      THIS-OBJECT:splitContainerTotalExport:Panel1:Controls:Add(THIS-OBJECT:EkgExport).
   END METHOD.
   
  METHOD PUBLIC VOID Testar():   
      MESSAGE "I Shell"
      VIEW-AS ALERT-BOX.
   END METHOD.
   
	DESTRUCTOR PUBLIC EkgShell ( ):
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.
	END DESTRUCTOR.

END CLASS.