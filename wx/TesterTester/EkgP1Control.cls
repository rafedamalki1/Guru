 
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
USING System.Windows.Forms.Panel.


CLASS Modules.EkgData.EkgP1Control INHERITS UserControl: 
 
   DEFINE PUBLIC VARIABLE p1kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p1frekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p1arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p1arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
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
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP1Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP1ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP1Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP1Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP1Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP1Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   
  CONSTRUCTOR PUBLIC EkgP1Control (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.


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
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab29 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab29 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
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
      THIS-OBJECT:TabP1frekvens:Size = NEW System.Drawing.Size(1061, 315).
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
      THIS-OBJECT:p1FrekvensSC:Size = NEW System.Drawing.Size(1061, 315).
      THIS-OBJECT:p1FrekvensSC:SplitterDistance = 325.
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
      THIS-OBJECT:p1frekvensSC2:Size = NEW System.Drawing.Size(325, 315).
      THIS-OBJECT:p1frekvensSC2:SplitterDistance = 285.
      THIS-OBJECT:p1frekvensSC2:TabIndex = 0.
      /*  */
      /* gridP2forP1 */
      /*  */
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
      THIS-OBJECT:gridP2forP1:Size = NEW System.Drawing.Size(285, 315).
      THIS-OBJECT:gridP2forP1:TabIndex = 1.
      THIS-OBJECT:gridP2forP1:Text = "grid1".
      THIS-OBJECT:gridP2forP1:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP2ForP1_GuruAfterDropped).
      /*  */
      /* buttonOverP1FrekvensBort */
      /*  */
      THIS-OBJECT:buttonOverP1FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP1FrekvensBort:Name = "buttonOverP1FrekvensBort".
      THIS-OBJECT:buttonOverP1FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1FrekvensBort:TabIndex = 1.
      THIS-OBJECT:buttonOverP1FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP1FrekvensBort_Click).
      /*  */
      /* buttonOverP1frekvens */
      /*  */
      THIS-OBJECT:buttonOverP1frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP1frekvens:Name = "buttonOverP1frekvens".
      THIS-OBJECT:buttonOverP1frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP1frekvens:TabIndex = 0.
      THIS-OBJECT:buttonOverP1frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP1Frekvens_Click).
      /*  */
      /* gridP1Frekvens */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
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
      THIS-OBJECT:gridP1Frekvens:Size = NEW System.Drawing.Size(732, 315).
      THIS-OBJECT:gridP1Frekvens:TabIndex = 0.
      THIS-OBJECT:gridP1Frekvens:Text = "grid1".
      THIS-OBJECT:gridP1Frekvens:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP1Frekvens_GuruAfterDropped).
      THIS-OBJECT:gridP1Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp1frekvens_AfterCellUpdate).
      /*  */
      /* TabP1Import */
      /*  */
      THIS-OBJECT:TabP1Import:Controls:Add(THIS-OBJECT:splitContainerP1ImportGrund).
      THIS-OBJECT:TabP1Import:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabP1Import:Name = "TabP1Import".
      THIS-OBJECT:TabP1Import:Size = NEW System.Drawing.Size(1061, 315).
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
      THIS-OBJECT:splitContainerP1ImportGrund:Size = NEW System.Drawing.Size(1061, 315).
      THIS-OBJECT:splitContainerP1ImportGrund:SplitterDistance = 25.
      THIS-OBJECT:splitContainerP1ImportGrund:TabIndex = 5.
      /*  */
      /* P1ImportStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP1Import.
      arrayvar0[2] = THIS-OBJECT:StripButtonP1Rin.
      arrayvar0[3] = THIS-OBJECT:StripButtonP1Rbort.
      THIS-OBJECT:P1ImportStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:P1ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P1ImportStrip:Name = "P1ImportStrip".
      THIS-OBJECT:P1ImportStrip:Size = NEW System.Drawing.Size(1061, 25).
      THIS-OBJECT:P1ImportStrip:TabIndex = 0.
      THIS-OBJECT:P1ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP1Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:p1KoderToolStripMenuItem.
      arrayvar1[2] = THIS-OBJECT:p1FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP1Import:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonP1Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Import:Name = "StripButtonP1Import".
      THIS-OBJECT:StripButtonP1Import:Size = NEW System.Drawing.Size(81, 22).
      THIS-OBJECT:StripButtonP1Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP1Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      /*  */
      /* p1KoderToolStripMenuItem */
      /*  */
      THIS-OBJECT:p1KoderToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p1KoderToolStripMenuItem:Name = "p1KoderToolStripMenuItem".
      THIS-OBJECT:p1KoderToolStripMenuItem:Size = NEW System.Drawing.Size(136, 22).
      THIS-OBJECT:p1KoderToolStripMenuItem:Text = "P1 Kod".
      THIS-OBJECT:p1KoderToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1KoderToolStripMenuItem_Click).
      /*  */
      /* p1FrekvensToolStripMenuItem */
      /*  */
      THIS-OBJECT:p1FrekvensToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Name = "p1FrekvensToolStripMenuItem".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Size = NEW System.Drawing.Size(136, 22).
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Text = "P1 Frekvens".
      THIS-OBJECT:p1FrekvensToolStripMenuItem:Click:Subscribe(THIS-OBJECT:p1FrekvensToolStripMenuItem_Click).
      /*  */
      /* StripButtonP1Rin */
      /*  */
      THIS-OBJECT:StripButtonP1Rin:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rin:Name = "StripButtonP1Rin".
      THIS-OBJECT:StripButtonP1Rin:Size = NEW System.Drawing.Size(101, 22).
      THIS-OBJECT:StripButtonP1Rin:Text = "Läs in Excel listan".
      THIS-OBJECT:StripButtonP1Rin:Visible = FALSE.
      THIS-OBJECT:StripButtonP1Rin:Click:Subscribe(THIS-OBJECT:StripButtonP1Rin_Click).
      /*  */
      /* StripButtonP1Rbort */
      /*  */
      THIS-OBJECT:StripButtonP1Rbort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP1Rbort:Name = "StripButtonP1Rbort".
      THIS-OBJECT:StripButtonP1Rbort:Size = NEW System.Drawing.Size(147, 22).
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
      THIS-OBJECT:splitContainerP1Import:Size = NEW System.Drawing.Size(1061, 286).
      THIS-OBJECT:splitContainerP1Import:SplitterDistance = 353.
      THIS-OBJECT:splitContainerP1Import:TabIndex = 0.
      /*  */
      /* GridImportP1F */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridImportP1F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1F:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
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
      THIS-OBJECT:GridImportP1F:Size = NEW System.Drawing.Size(353, 286).
      THIS-OBJECT:GridImportP1F:TabIndex = 1.
      THIS-OBJECT:GridImportP1F:Text = "grid1".
      /*  */
      /* GridImportP1 */
      /*  */
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP1:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridImportP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP1:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
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
      THIS-OBJECT:GridImportP1:Size = NEW System.Drawing.Size(353, 286).
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
      THIS-OBJECT:p1MainSC:Size = NEW System.Drawing.Size(1065, 584).
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
      THIS-OBJECT:p1kodSC:Size = NEW System.Drawing.Size(1065, 230).
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
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:laggTIllP1arbkod.
      arrayvar2[2] = THIS-OBJECT:taBortP1arbkodButton.
      THIS-OBJECT:p1arbkodStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:p1arbkodStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1arbkodStrip:Name = "p1arbkodStrip".
      THIS-OBJECT:p1arbkodStrip:Size = NEW System.Drawing.Size(400, 25).
      THIS-OBJECT:p1arbkodStrip:TabIndex = 0.
      THIS-OBJECT:p1arbkodStrip:Text = "toolStrip1".
      /*  */
      /* laggTIllP1arbkod */
      /*  */
      THIS-OBJECT:laggTIllP1arbkod:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTIllP1arbkod:Name = "laggTIllP1arbkod".
      THIS-OBJECT:laggTIllP1arbkod:Size = NEW System.Drawing.Size(128, 22).
      THIS-OBJECT:laggTIllP1arbkod:Text = "Lägg till P1-Arbetskod".
      THIS-OBJECT:laggTIllP1arbkod:Click:Subscribe(THIS-OBJECT:laggTIllP1arbkod_Click).
      /*  */
      /* taBortP1arbkodButton */
      /*  */
      THIS-OBJECT:taBortP1arbkodButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP1arbkodButton:Name = "taBortP1arbkodButton".
      THIS-OBJECT:taBortP1arbkodButton:Size = NEW System.Drawing.Size(124, 22).
      THIS-OBJECT:taBortP1arbkodButton:Text = "Ta bort P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:ToolTipText = "Tar bort markerad P1-Arbetskod".
      THIS-OBJECT:taBortP1arbkodButton:Click:Subscribe(THIS-OBJECT:taBortP1arbkodButton_Click).
      /*  */
      /* gridP1arbkod */
      /*  */
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Appearance = appearance49.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance50:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance50:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance50:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:Appearance = appearance50.
      appearance51:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance51.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance52:BackColor2 = System.Drawing.SystemColors:Control.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance52:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance52.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance53:BackColor = System.Drawing.SystemColors:Window.
      appearance53:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance53.
      appearance54:BackColor = System.Drawing.SystemColors:Highlight.
      appearance54:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance54.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CardAreaAppearance = appearance55.
      appearance56:BorderColor = System.Drawing.Color:Silver.
      appearance56:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellAppearance = appearance56.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance57:BackColor = System.Drawing.SystemColors:Control.
      appearance57:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance57:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance57:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance57:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance57.
      appearance58:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderAppearance = appearance58.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance59:BackColor = System.Drawing.SystemColors:Window.
      appearance59:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowAppearance = appearance59.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance60:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance60.
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
      THIS-OBJECT:gridP1arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp1_AfterCellUpdate).
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
      THIS-OBJECT:p1KoderMenuSC:Size = NEW System.Drawing.Size(661, 230).
      THIS-OBJECT:p1KoderMenuSC:SplitterDistance = 25.
      THIS-OBJECT:p1KoderMenuSC:TabIndex = 0.
      /*  */
      /* p1KodertoolStrip */
      /*  */
      THIS-OBJECT:p1KodertoolStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:laggTillP1Button.
      arrayvar3[2] = THIS-OBJECT:taBortP1Button.
      THIS-OBJECT:p1KodertoolStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:p1KodertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p1KodertoolStrip:Name = "p1KodertoolStrip".
      THIS-OBJECT:p1KodertoolStrip:Size = NEW System.Drawing.Size(661, 25).
      THIS-OBJECT:p1KodertoolStrip:TabIndex = 0.
      THIS-OBJECT:p1KodertoolStrip:Text = "toolStrip1".
      /*  */
      /* laggTillP1Button */
      /*  */
      THIS-OBJECT:laggTillP1Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP1Button:Name = "laggTillP1Button".
      THIS-OBJECT:laggTillP1Button:Size = NEW System.Drawing.Size(111, 22).
      THIS-OBJECT:laggTillP1Button:Text = "Lägg till ny P1-Kod".
      THIS-OBJECT:laggTillP1Button:Click:Subscribe(THIS-OBJECT:laggTillP1Button_Click).
      /*  */
      /* taBortP1Button */
      /*  */
      THIS-OBJECT:taBortP1Button:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP1Button:Name = "taBortP1Button".
      THIS-OBJECT:taBortP1Button:Size = NEW System.Drawing.Size(91, 22).
      THIS-OBJECT:taBortP1Button:Text = "Ta bort P1-Kod".
      THIS-OBJECT:taBortP1Button:ToolTipText = "Tar bort markerad P1-Kod".
      THIS-OBJECT:taBortP1Button:Click:Subscribe(THIS-OBJECT:taBortP1Button_Click).
      /*  */
      /* gridP1 */
      /*  */
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP1:DisplayLayout:Appearance = appearance61.
      THIS-OBJECT:gridP1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance62:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance62:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance62:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:Appearance = appearance62.
      appearance63:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance63.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance64:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance64:BackColor2 = System.Drawing.SystemColors:Control.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance64:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP1:DisplayLayout:GroupByBox:PromptAppearance = appearance64.
      THIS-OBJECT:gridP1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance65:BackColor = System.Drawing.SystemColors:Window.
      appearance65:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveCellAppearance = appearance65.
      appearance66:BackColor = System.Drawing.SystemColors:Highlight.
      appearance66:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:ActiveRowAppearance = appearance66.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CardAreaAppearance = appearance67.
      appearance68:BorderColor = System.Drawing.Color:Silver.
      appearance68:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellAppearance = appearance68.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP1:DisplayLayout:Override:CellPadding = 0.
      appearance69:BackColor = System.Drawing.SystemColors:Control.
      appearance69:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance69:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance69:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance69:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP1:DisplayLayout:Override:GroupByRowAppearance = appearance69.
      appearance70:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderAppearance = appearance70.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      appearance71:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowAppearance = appearance71.
      THIS-OBJECT:gridP1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance72:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP1:DisplayLayout:Override:TemplateAddRowAppearance = appearance72.
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
      THIS-OBJECT:gridP1:Size = NEW System.Drawing.Size(661, 201).
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
      THIS-OBJECT:TabManagerP1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerP1:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerP1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerP1:Name = "TabManagerP1".
      THIS-OBJECT:TabManagerP1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPageP1.
      THIS-OBJECT:TabManagerP1:Size = NEW System.Drawing.Size(1065, 350).
      THIS-OBJECT:TabManagerP1:TabIndex = 2.
      ultraTab6:Key = "P1Frekvens".
      ultraTab6:TabPage = THIS-OBJECT:TabP1frekvens.
      ultraTab6:Text = "P1-Frekvenser".
      ultraTab29:Key = "Import".
      ultraTab29:TabPage = THIS-OBJECT:TabP1Import.
      ultraTab29:Text = "Import Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar4[1] = ultraTab6.
      arrayvar4[2] = ultraTab29.
      THIS-OBJECT:TabManagerP1:Tabs:AddRange(arrayvar4).
      THIS-OBJECT:TabManagerP1:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP1_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP1:Name = "ultraTabSharedControlsPageP1".
      THIS-OBJECT:ultraTabSharedControlsPageP1:Size = NEW System.Drawing.Size(1061, 315).
      /*  */
      /* EkgP1Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:AutoSize = TRUE.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p1MainSC).
      THIS-OBJECT:Name = "EkgP1Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1065, 584).
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
   METHOD PRIVATE VOID buttonCancelNyP1arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:fillinP1arbkod:GuruText:Text = "".
      THIS-OBJECT:ControlShell:nyP1arbkodPop:Close().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelNyP1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:fillinP1LopNr:GuruText:Text = "".
      THIS-OBJECT:ControlShell:nyP1Pop:Close().
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
      ELSE STRING(e:Cell:VALUE) =  THIS-OBJECT:ControlShell:beforekolvarde.      
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1arbkod_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      THIS-OBJECT:ControlShell:beforekolnamn = e:Cell:Column:ToString().        
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
      ELSE STRING(e:Cell:VALUE) =  THIS-OBJECT:ControlShell:beforekolvarde.      
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp1_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      THIS-OBJECT:ControlShell:beforekolnamn = e:Cell:Column:ToString().        
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
      IF VALID-OBJECT(TabManagerP1:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
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