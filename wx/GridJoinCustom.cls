 
 /*------------------------------------------------------------------------
    File        : GridJoinCustom
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Fri Feb 11 10:47:12 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Microsoft.Office.Interop.Excel.*.

CLASS Controls.GridJoinCustom INHERITS Infragistics.Win.UltraWinGrid.UltraGrid:
      /* Funktioner för att spara markerade rader till OldRows med GuruSaveOldRows, samt selecta dom nya med GuruSelectNew. */
   DEFINE VARIABLE antalGridBands AS INTEGER NO-UNDO.
   DEFINE VARIABLE ultraGridBand AS Infragistics.Win.UltraWinGrid.UltraGridBand EXTENT 10 NO-UNDO .
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC STATIC PROPERTY lastkeydownI     AS INTEGER NO-UNDO  
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY lastkeydownC     AS CHARACTER NO-UNDO  
      PUBLIC GET.    PUBLIC SET.   
   DEFINE VARIABLE fargvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE valdgridrowid AS ROWID NO-UNDO.
   
   DEFINE PRIVATE VARIABLE oldRows AS System.Collections.ArrayList NO-UNDO.
   DEFINE PRIVATE VARIABLE hasRows AS LOGICAL.
   DEFINE PUBLIC VARIABLE canDelete AS LOGICAL INITIAL FALSE.
   /* För färgsättning */
   DEFINE PUBLIC VARIABLE GuruColorConditions AS System.Collections.ArrayList NO-UNDO.
   /* För Drag & Drop-funktionalitet*/
   DEFINE PUBLIC EVENT GuruAfterDropped  DELEGATE System.EventHandler.
   /* För att sätta titel och hur många poster den innehåller */   
   DEFINE PUBLIC VARIABLE TITLE AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE ActiveValCol AS CHARACTER NO-UNDO.
   
   DEFINE PUBLIC VARIABLE GuruReopening AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC PROPERTY GuruShowRowsCount AS LOGICAL INITIAL TRUE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY GuruDraggable AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY GuruDroppable AS LOGICAL INITIAL FALSE NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   DEFINE PUBLIC VARIABLE DragDropGroups AS System.Collections.ArrayList NO-UNDO.
   
      /* Osorterat */
   DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
   DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
   DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
   DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
   DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
         
   DEFINE PRIVATE TEMP-TABLE rowidlista NO-UNDO 
      FIELD id AS ROWID.
      
   DEFINE PUBLIC VARIABLE tqH AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE tBS AS Progress.Data.BindingSource NO-UNDO.
   
   DEFINE PUBLIC PROPERTY GuruContext AS System.Windows.Forms.ContextMenuStrip NO-UNDO  
      GET.       SET.
         
   DEFINE PUBLIC PROPERTY Initierad AS LOGICAL INITIAL FALSE NO-UNDO  
      PUBLIC GET.
      PRIVATE SET.         
/*      Måste sättas, "FOR EACH tabellnamn WHERE xxx"*/
   DEFINE PROPERTY ttSortQuery AS CHARACTER NO-UNDO  
      PUBLIC GET.       PRIVATE SET.
   
   DEFINE PROPERTY ttSortBy AS CHARACTER NO-UNDO  
      PUBLIC GET.       PRIVATE SET.
        
/* Använd denna property för att spara undan deletade poster i en egen tabell
   Kör RensaDel() för att rensa buffern.*/      
   DEFINE PROPERTY ttBufferHandleDel AS HANDLE NO-UNDO
      PUBLIC GET.      PUBLIC SET.                     

   DEFINE PROPERTY Guruegenskap AS Controls.GridRubrikListaJoin NO-UNDO EXTENT 10
      PUBLIC GET.   PRIVATE SET.
   
   DEFINE PRIVATE PROPERTY Gurudelfraga AS LOGICAL INITIAL TRUE NO-UNDO
   PRIVATE GET.   PRIVATE SET.
   
   DEFINE PRIVATE VARIABLE excelExporter AS Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter NO-UNDO.
   DEFINE PUBLIC  VARIABLE ExtraToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE kopieraToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelApp AS Application NO-UNDO.
   DEFINE PRIVATE VARIABLE excelApp2 AS Application NO-UNDO.
   
   
   DEFINE VARIABLE iex AS INTEGER  NO-UNDO. /*GuruTillExcel() räknare*/
   

   
   CONSTRUCTOR PUBLIC GridJoinCustom ( ):      
      SUPER ().
      InitializeComponent ( ).
      THIS-OBJECT:DragDropGroups = NEW System.Collections.ArrayList().
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC GridJoinCustom ( ):
      /*{WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups }
         CAST(oObject, Controls.Subclasses.DragDropGroup):RemoveChild(THIS-OBJECT).
      END.*/
   END DESTRUCTOR.

  
   
   /* Initierar en grid */
   METHOD PUBLIC VOID GuruInitDBS(ttdsh AS HANDLE):
      tBS:HANDLE = ttdsh.   
   END METHOD.    
   METHOD PUBLIC VOID GuruInitGridJoin(antalbands AS INTEGER,ttdsh AS HANDLE):
     
      DEFINE VARIABLE bandval AS INTEGER             NO-UNDO.
      antalGridBands = antalbands.
      
      /* slut contextmenustrip */
      DEFINE VARIABLE varindex AS System.Object {GridRubrikListaExtent.i} NO-UNDO.
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
     
   
   /*
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().  
      tBS:HANDLE = ttdsh.           
      THIS-OBJECT:DataSource = tBS.
     */
     
     
    
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().
      tBS:HANDLE = DATASET-HANDLE ttdsh.
      THIS-OBJECT:DataSource = tBS.
    
      
      bandval =  1. 
      THIS-OBJECT:GuruDefaults(). 
  
      THIS-OBJECT:Initierad = TRUE.
   
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
      CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* Grid */
      /*  */
      THIS-OBJECT:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance1:AlphaLevel = System.Convert:ToInt16(111).
      appearance1:BackColor = System.Drawing.SystemColors:Highlight.
      appearance1:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:DisplayLayout:Override:ActiveCellAppearance = appearance1.
      appearance2:AlphaLevel = System.Convert:ToInt16(111).
      appearance2:BackColor = System.Drawing.SystemColors:Highlight.
      appearance2:BackColorAlpha = Infragistics.Win.Alpha:UseAlphaLevel.
      THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance = appearance2.
      THIS-OBJECT:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.
   
   
   
   METHOD PRIVATE VOID GuruDefaults():    
      appearance1 = NEW Infragistics.Win.Appearance().
      appearance2 = NEW Infragistics.Win.Appearance().
      appearance3 = NEW Infragistics.Win.Appearance().
      appearance4 = NEW Infragistics.Win.Appearance().
      appearance5 = NEW Infragistics.Win.Appearance().
      appearance1:BackColor = System.Drawing.Color:White.      
      appearance2:BackColor = System.Drawing.Color:Transparent.      
      appearance3:TextHAlignAsString = "Left".      
      appearance4:BorderColor = System.Drawing.Color:LightGray.
      appearance4:TextVAlignAsString = "Middle".      
      appearance5:BackColor = System.Drawing.Color:LightSteelBlue.
      appearance5:BorderColor = System.Drawing.Color:Black.
      appearance5:ForeColor = System.Drawing.Color:Black.
      THIS-OBJECT:DisplayLayout:RowConnectorStyle = Infragistics.Win.UltraWinGrid.RowConnectorStyle:None.
      THIS-OBJECT:DisplayLayout:Override:HeaderAppearance = appearance3.
      THIS-OBJECT:DisplayLayout:Override:CardAreaAppearance = appearance2.
      THIS-OBJECT:DisplayLayout:Override:CellPadding = 1. /*avstånd i höjdled mellan rader i grid */
      THIS-OBJECT:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:None.
      THIS-OBJECT:DisplayLayout:Override:RowAppearance = appearance4.
      THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:DisplayLayout:Override:SelectedRowAppearance = appearance5.
      THIS-OBJECT:Text = "gridnamn:Text = text som ska stå här".
      THIS-OBJECT:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:Horizontal.   
      THIS-OBJECT:DisplayLayout:GroupByBox:Hidden = FALSE. /* Sorterings-lådan, kräver att ViewStyleBand är OutlookGroupBy */
      THIS-OBJECT:DisplayLayout:GroupByBox:Prompt = "Dra en rubrik hit för att gruppera".      
      THIS-OBJECT:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True. /*Griddens rubrik*/
      THIS-OBJECT:DisplayLayout:Override:AllowMultiCellOperations = Infragistics.Win.UltraWinGrid.AllowMultiCellOperation:Copy. /*krävs för att man ska kunna kopiera rader*/        
   END METHOD.
   
   METHOD PUBLIC VOID SetViewStyleBand(INPUT setvar AS LOGICAL):  
      IF setvar = FALSE THEN THIS-OBJECT:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:Horizontal.
      ELSE THIS-OBJECT:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.   
              
   END METHOD.
   
   
   
END CLASS.