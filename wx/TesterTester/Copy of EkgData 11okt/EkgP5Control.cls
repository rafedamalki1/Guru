 
 /*------------------------------------------------------------------------
    File        : EkgP5Control
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Oct 10 14:52:57 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgP5Control INHERITS UserControl: 
   
   DEFINE PUBLIC VARIABLE p5ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p5ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p5MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p5KoderStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p5KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP5Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP5ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP5Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE P5ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP5 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP5Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP5ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP5Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP5resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP5Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP5 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP5Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP5Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP5Import AS System.Windows.Forms.ToolStripButton NO-UNDO.

   CONSTRUCTOR PUBLIC EkgP5Control (INPUT r AS Guru.Root):
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
      THIS-OBJECT:ControlShell:beforekolnamn = e:Cell:Column:ToString().        
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP5Control").
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
      DEFINE VARIABLE ultraTab26 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab26 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab27 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab27 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
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
      THIS-OBJECT:p5MainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p5KoderMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:p5KoderStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP5Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP5Button = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridP5 = NEW Controls.Grid().
      THIS-OBJECT:TabManagerP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPageP5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
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
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabP5resurs */
      /*  */
      THIS-OBJECT:TabP5resurs:Controls:Add(THIS-OBJECT:p5ResursSC).
      THIS-OBJECT:TabP5resurs:Location = NEW System.Drawing.Point(1, 32).
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
      THIS-OBJECT:p5ResursSC:SplitterDistance = 346.
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
      THIS-OBJECT:p5ResursSC2:Size = NEW System.Drawing.Size(346, 284).
      THIS-OBJECT:p5ResursSC2:SplitterDistance = 310.
      THIS-OBJECT:p5ResursSC2:TabIndex = 0.
      /*  */
      /* gridResursForP5 */
      /*  */
      THIS-OBJECT:gridResursForP5:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResursForP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResursForP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
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
      THIS-OBJECT:gridResursForP5:Size = NEW System.Drawing.Size(310, 284).
      THIS-OBJECT:gridResursForP5:TabIndex = 1.
      THIS-OBJECT:gridResursForP5:Text = "grid1".
      THIS-OBJECT:gridResursForP5:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursForP5_GuruAfterDropped).
      /*  */
      /* buttonOverP5ResursBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverP5ResursBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverP5ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP5ResursBort:Name = "buttonOverP5ResursBort".
      THIS-OBJECT:buttonOverP5ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP5ResursBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP5ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP5ResursBort_Click).
      /*  */
      /* buttonOverP5Resurs */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverP5Resurs:Appearance = appearance14.
      THIS-OBJECT:buttonOverP5Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP5Resurs:Name = "buttonOverP5Resurs".
      THIS-OBJECT:buttonOverP5Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP5Resurs:TabIndex = 4.
      THIS-OBJECT:buttonOverP5Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP5Resurs_Click).
      /*  */
      /* gridP5Resurs */
      /*  */
      THIS-OBJECT:gridP5Resurs:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
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
      THIS-OBJECT:gridP5Resurs:Size = NEW System.Drawing.Size(791, 284).
      THIS-OBJECT:gridP5Resurs:TabIndex = 0.
      THIS-OBJECT:gridP5Resurs:Text = "grid1".
      THIS-OBJECT:gridP5Resurs:GuruAfterDropped:Subscribe(THIS-OBJECT:gridP5Resurs_GuruAfterDropped).
      THIS-OBJECT:gridP5Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp5resurs_AfterCellUpdate).
      /*  */
      /* TabP5Import */
      /*  */
      THIS-OBJECT:TabP5Import:Controls:Add(THIS-OBJECT:splitContainerP5ImportGrund).
      THIS-OBJECT:TabP5Import:Location = NEW System.Drawing.Point(-10000, -10000).
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
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP5Import.
      arrayvar0[2] = THIS-OBJECT:StripButtonP5Rin.
      arrayvar0[3] = THIS-OBJECT:StripButtonP5Rbort.
      THIS-OBJECT:P5ImportStrip:Items:AddRange(arrayvar0).
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
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP5:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridImportP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP5:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridImportP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
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
      THIS-OBJECT:p5MainSC:TabIndex = 1.
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
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:laggTillP5Button.
      arrayvar1[2] = THIS-OBJECT:taBortP5Button.
      THIS-OBJECT:p5KoderStrip:Items:AddRange(arrayvar1).
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
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP5:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:gridP5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP5:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:gridP5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:gridP5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP5:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP5:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP5:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:gridP5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP5:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
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
      THIS-OBJECT:gridP5:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp5_AfterCellUpdate).
      THIS-OBJECT:gridP5:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp5_BeforeCellUpdate).
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
      appearance51:Image = CAST(resources:GetObject("appearance51.Image"), System.Object).
      ultraTab26:Appearance = appearance51.
      ultraTab26:Key = "P5Resurs".
      ultraTab26:TabPage = THIS-OBJECT:TabP5resurs.
      ultraTab26:Text = "P5-Resurser".
      appearance52:Image = CAST(resources:GetObject("appearance52.Image"), System.Object).
      ultraTab27:ActiveAppearance = appearance52.
      appearance53:Image = CAST(resources:GetObject("appearance53.Image"), System.Object).
      ultraTab27:Appearance = appearance53.
      ultraTab27:Key = "Import".
      ultraTab27:TabPage = THIS-OBJECT:TabP5Import.
      ultraTab27:Text = "Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar2[1] = ultraTab26.
      arrayvar2[2] = ultraTab27.
      THIS-OBJECT:TabManagerP5:Tabs:AddRange(arrayvar2).
      /*  */
      /* ultraTabSharedControlsPageP5 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP5:Name = "ultraTabSharedControlsPageP5".
      THIS-OBJECT:ultraTabSharedControlsPageP5:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* EkgP5Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p5MainSC).
      THIS-OBJECT:Name = "EkgP5Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
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
   METHOD PRIVATE VOID laggTillP5Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP5Pop:Show().
      THIS-OBJECT:ControlShell:fillinP5arbkod:GuruText:Select().
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
   METHOD PRIVATE VOID TabManagerP5_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP5:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP5:SelectedTab:KEY).    
      END. 
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

END CLASS.