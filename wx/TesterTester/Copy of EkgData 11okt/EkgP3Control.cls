 
 /*------------------------------------------------------------------------
    File        : EkgP3Control
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Oct 10 11:30:53 CEST 3013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgP3Control INHERITS UserControl: 
 
   DEFINE PUBLIC VARIABLE p3kodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3arbkodStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p3arbkodSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3ResursSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3ResursSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3MainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3KodertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE p3KoderMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE p3KodToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE p3FrekvensToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE p3FrekvensSC2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE p3FrekvensSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP3arbkod AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP3Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP4ForP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3arbkod AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3Resurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3Frekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP3ResursBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP3Resurs AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP3FrekvensBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverP3Frekvens AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE P3ImportStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP3F AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridImportP3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPageP3 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP3arbkodButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE taBortP3Button AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerP3ImportGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerP3Import AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Resurs AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Import AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabP3Frekvens AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PUBLIC VARIABLE TabManagerP3 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP3Rin AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE StripButtonP3Rbort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonP3Import AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.

  CONSTRUCTOR PUBLIC EkgP3Control (INPUT r AS Guru.Root):
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
   METHOD PRIVATE VOID gridp3arbkod_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:ControlWinLg:P3ArbkodUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.
      ELSE STRING(e:Cell:VALUE) =  THIS-OBJECT:ControlShell:beforekolvarde.      
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridp3arbkod_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      THIS-OBJECT:ControlShell:beforekolnamn = e:Cell:Column:ToString().        
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
   METHOD PRIVATE VOID gridp3_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      THIS-OBJECT:ControlShell:beforekolnamn = e:Cell:Column:ToString().        
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
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
      THIS-OBJECT:ControlShell:beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridP3:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "FREKVENS" THEN DO:
         IF THIS-OBJECT:ControlShell:beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
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
   METHOD PRIVATE VOID gridp3resurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEBUGGER:SET-BREAK().  
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      THIS-OBJECT:ControlWinLg:P3ResursUppdate(INPUT kolnamn, INPUT kolvarde).               
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
         THIS-OBJECT:buttonOverP3ResursBort_Click(?,?).
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
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP3Control").
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
      DEFINE VARIABLE ultraTab10 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab10 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab11 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab11 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
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
      THIS-OBJECT:SuspendLayout().
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
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResursForP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResursForP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
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
      /*  */
      /* buttonOverP3ResursBort */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonOverP3ResursBort:Appearance = appearance13.
      THIS-OBJECT:buttonOverP3ResursBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP3ResursBort:Name = "buttonOverP3ResursBort".
      THIS-OBJECT:buttonOverP3ResursBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3ResursBort:TabIndex = 3.
      THIS-OBJECT:buttonOverP3ResursBort:Click:Subscribe(THIS-OBJECT:buttonOverP3ResursBort_Click).
      /*  */
      /* buttonOverP3Resurs */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonOverP3Resurs:Appearance = appearance14.
      THIS-OBJECT:buttonOverP3Resurs:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP3Resurs:Name = "buttonOverP3Resurs".
      THIS-OBJECT:buttonOverP3Resurs:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3Resurs:TabIndex = 2.
      THIS-OBJECT:buttonOverP3Resurs:Click:Subscribe(THIS-OBJECT:buttonOverP3Resurs_Click).
      /*  */
      /* gridP3Resurs */
      /*  */
      THIS-OBJECT:gridP3Resurs:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3Resurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
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
      THIS-OBJECT:gridP3Resurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp3resurs_AfterCellUpdate).
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
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP4ForP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
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
      /*  */
      /* buttonOverP3FrekvensBort */
      /*  */
      appearance39:Image = CAST(resources:GetObject("appearance39.Image"), System.Object).
      THIS-OBJECT:buttonOverP3FrekvensBort:Appearance = appearance39.
      THIS-OBJECT:buttonOverP3FrekvensBort:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:buttonOverP3FrekvensBort:Name = "buttonOverP3FrekvensBort".
      THIS-OBJECT:buttonOverP3FrekvensBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3FrekvensBort:TabIndex = 5.
      THIS-OBJECT:buttonOverP3FrekvensBort:Click:Subscribe(THIS-OBJECT:buttonOverP3FrekvensBort_Click).
      /*  */
      /* buttonOverP3Frekvens */
      /*  */
      appearance40:Image = CAST(resources:GetObject("appearance40.Image"), System.Object).
      THIS-OBJECT:buttonOverP3Frekvens:Appearance = appearance40.
      THIS-OBJECT:buttonOverP3Frekvens:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:buttonOverP3Frekvens:Name = "buttonOverP3Frekvens".
      THIS-OBJECT:buttonOverP3Frekvens:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverP3Frekvens:TabIndex = 4.
      THIS-OBJECT:buttonOverP3Frekvens:Click:Subscribe(THIS-OBJECT:buttonOverP3Frekvens_Click).
      /*  */
      /* gridP3Frekvens */
      /*  */
      THIS-OBJECT:gridP3Frekvens:AllowDrop = TRUE.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Appearance = appearance41.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance42:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance42:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance42:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:Appearance = appearance42.
      appearance43:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance43.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance44:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance44:BackColor2 = System.Drawing.SystemColors:Control.
      appearance44:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance44:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance44.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      appearance45:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveCellAppearance = appearance45.
      appearance46:BackColor = System.Drawing.SystemColors:Highlight.
      appearance46:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:ActiveRowAppearance = appearance46.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CardAreaAppearance = appearance47.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      appearance48:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellAppearance = appearance48.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:CellPadding = 0.
      appearance49:BackColor = System.Drawing.SystemColors:Control.
      appearance49:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance49:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance49:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance49:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:GroupByRowAppearance = appearance49.
      appearance50:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderAppearance = appearance50.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:RowAppearance = appearance51.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3Frekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance52.
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
      THIS-OBJECT:gridP3Frekvens:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp3frekvens_AfterCellUpdate).
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
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonP3Import.
      arrayvar0[2] = THIS-OBJECT:StripButtonP3Rin.
      arrayvar0[3] = THIS-OBJECT:StripButtonP3Rbort.
      THIS-OBJECT:P3ImportStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:P3ImportStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:P3ImportStrip:Name = "P3ImportStrip".
      THIS-OBJECT:P3ImportStrip:Size = NEW System.Drawing.Size(1141, 25).
      THIS-OBJECT:P3ImportStrip:TabIndex = 0.
      THIS-OBJECT:P3ImportStrip:Text = "toolStrip3".
      /*  */
      /* StripButtonP3Import */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:p3KodToolStripMenuItem.
      arrayvar1[2] = THIS-OBJECT:p3FrekvensToolStripMenuItem.
      THIS-OBJECT:StripButtonP3Import:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonP3Import:Image = CAST(resources:GetObject("StripButtonP3Import.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonP3Import:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonP3Import:Name = "StripButtonP3Import".
      THIS-OBJECT:StripButtonP3Import:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:StripButtonP3Import:Text = "Välj Excelfil ".
      THIS-OBJECT:StripButtonP3Import:ToolTipText = "Välj Excelfil Kod eller frekvens".
      THIS-OBJECT:StripButtonP3Import:Click:Subscribe(THIS-OBJECT:StripButtonP3Import_Click).
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
      appearance53:BackColor = System.Drawing.SystemColors:Window.
      appearance53:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Appearance = appearance53.
      THIS-OBJECT:GridImportP3F:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP3F:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance54:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance54:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance54:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:Appearance = appearance54.
      appearance55:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:BandLabelAppearance = appearance55.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance56:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance56:BackColor2 = System.Drawing.SystemColors:Control.
      appearance56:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:GroupByBox:PromptAppearance = appearance56.
      THIS-OBJECT:GridImportP3F:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP3F:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      appearance57:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveCellAppearance = appearance57.
      appearance58:BackColor = System.Drawing.SystemColors:Highlight.
      appearance58:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:ActiveRowAppearance = appearance58.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance59:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CardAreaAppearance = appearance59.
      appearance60:BorderColor = System.Drawing.Color:Silver.
      appearance60:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellAppearance = appearance60.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:CellPadding = 0.
      appearance61:BackColor = System.Drawing.SystemColors:Control.
      appearance61:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance61:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance61:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance61:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:GroupByRowAppearance = appearance61.
      appearance62:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderAppearance = appearance62.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:RowAppearance = appearance63.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP3F:DisplayLayout:Override:TemplateAddRowAppearance = appearance64.
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
      appearance65:BackColor = System.Drawing.SystemColors:Window.
      appearance65:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridImportP3:DisplayLayout:Appearance = appearance65.
      THIS-OBJECT:GridImportP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridImportP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridImportP3:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:GridImportP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridImportP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridImportP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
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
      THIS-OBJECT:p3MainSC:TabIndex = 1.
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
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:laggTillP3arbkod.
      arrayvar2[2] = THIS-OBJECT:taBortP3arbkodButton.
      THIS-OBJECT:p3arbkodStrip:Items:AddRange(arrayvar2).
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
      appearance77:BackColor = System.Drawing.SystemColors:Window.
      appearance77:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Appearance = appearance77.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance78:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance78:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance78:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:Appearance = appearance78.
      appearance79:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:BandLabelAppearance = appearance79.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance80:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance80:BackColor2 = System.Drawing.SystemColors:Control.
      appearance80:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance80:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:GroupByBox:PromptAppearance = appearance80.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      appearance81:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveCellAppearance = appearance81.
      appearance82:BackColor = System.Drawing.SystemColors:Highlight.
      appearance82:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:ActiveRowAppearance = appearance82.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance83:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CardAreaAppearance = appearance83.
      appearance84:BorderColor = System.Drawing.Color:Silver.
      appearance84:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellAppearance = appearance84.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:CellPadding = 0.
      appearance85:BackColor = System.Drawing.SystemColors:Control.
      appearance85:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance85:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance85:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance85:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:GroupByRowAppearance = appearance85.
      appearance86:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderAppearance = appearance86.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:RowAppearance = appearance87.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3arbkod:DisplayLayout:Override:TemplateAddRowAppearance = appearance88.
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
      THIS-OBJECT:gridP3arbkod:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp3arbkod_AfterCellUpdate).
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
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:laggTillP3Button.
      arrayvar3[2] = THIS-OBJECT:taBortP3Button.
      THIS-OBJECT:p3KodertoolStrip:Items:AddRange(arrayvar3).
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
      appearance89:BackColor = System.Drawing.SystemColors:Window.
      appearance89:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridP3:DisplayLayout:Appearance = appearance89.
      THIS-OBJECT:gridP3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridP3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance90:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance90:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance90:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:Appearance = appearance90.
      appearance91:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance91.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance92:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance92:BackColor2 = System.Drawing.SystemColors:Control.
      appearance92:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance92:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridP3:DisplayLayout:GroupByBox:PromptAppearance = appearance92.
      THIS-OBJECT:gridP3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridP3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      appearance93:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveCellAppearance = appearance93.
      appearance94:BackColor = System.Drawing.SystemColors:Highlight.
      appearance94:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:ActiveRowAppearance = appearance94.
      THIS-OBJECT:gridP3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridP3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance95:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CardAreaAppearance = appearance95.
      appearance96:BorderColor = System.Drawing.Color:Silver.
      appearance96:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellAppearance = appearance96.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridP3:DisplayLayout:Override:CellPadding = 0.
      appearance97:BackColor = System.Drawing.SystemColors:Control.
      appearance97:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance97:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance97:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance97:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridP3:DisplayLayout:Override:GroupByRowAppearance = appearance97.
      appearance98:TextHAlignAsString = "Left".
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderAppearance = appearance98.
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridP3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridP3:DisplayLayout:Override:RowAppearance = appearance99.
      THIS-OBJECT:gridP3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridP3:DisplayLayout:Override:TemplateAddRowAppearance = appearance100.
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
      THIS-OBJECT:gridP3:AfterCellUpdate:Subscribe(THIS-OBJECT:gridp3_AfterCellUpdate).
      THIS-OBJECT:gridP3:CellChange:Subscribe(THIS-OBJECT:gridP3_CellChange).
      THIS-OBJECT:gridP3:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridp3_BeforeCellUpdate).
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
      appearance101:Image = CAST(resources:GetObject("appearance101.Image"), System.Object).
      ultraTab10:Appearance = appearance101.
      ultraTab10:Key = "P3Resurs".
      ultraTab10:TabPage = THIS-OBJECT:TabP3Resurs.
      ultraTab10:Text = "P3-Resurser".
      appearance102:Image = CAST(resources:GetObject("appearance102.Image"), System.Object).
      ultraTab11:Appearance = appearance102.
      ultraTab11:Key = "P3Frekvens".
      ultraTab11:TabPage = THIS-OBJECT:TabP3Frekvens.
      ultraTab11:Text = "P3-Frekvenser".
      appearance103:Image = CAST(resources:GetObject("appearance103.Image"), System.Object).
      ultraTab5:ActiveAppearance = appearance103.
      appearance104:Image = CAST(resources:GetObject("appearance104.Image"), System.Object).
      ultraTab5:Appearance = appearance104.
      ultraTab5:Key = "Import".
      ultraTab5:TabPage = THIS-OBJECT:TabP3Import.
      ultraTab5:Text = " Import från Excel".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar4[1] = ultraTab10.
      arrayvar4[2] = ultraTab11.
      arrayvar4[3] = ultraTab5.
      THIS-OBJECT:TabManagerP3:Tabs:AddRange(arrayvar4).
      THIS-OBJECT:TabManagerP3:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManagerP3_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPageP3 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPageP3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPageP3:Name = "ultraTabSharedControlsPageP3".
      THIS-OBJECT:ultraTabSharedControlsPageP3:Size = NEW System.Drawing.Size(1141, 284).
      /*  */
      /* EkgP3Control */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:p3MainSC).
      THIS-OBJECT:Name = "EkgP3Control".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
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
   METHOD PRIVATE VOID laggTillP3arbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP3arbkodPop:Show().
      THIS-OBJECT:ControlShell:fillinP3ArbKod:GuruText:Select().
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillP3Button_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyP3Pop:Show().
      THIS-OBJECT:ControlShell:fillinP3LopNr:GuruText:Select().
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
	METHOD PRIVATE VOID StripButtonP3Import_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
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
   METHOD PRIVATE VOID TabManagerP3_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      IF VALID-OBJECT(TabManagerP3:SelectedTab) AND  VALID-OBJECT(e:TAB) THEN DO: 
         THIS-OBJECT:ControlWinLg:TabP1P2P3P4P5_SelectedTabChanging(INPUT THIS-OBJECT:ControlShell:TabManagerEkg:SelectedTab:KEY,INPUT e:TAB:KEY,INPUT THIS-OBJECT:TabManagerP3:SelectedTab:KEY).    
      END. 
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
   
END CLASS.