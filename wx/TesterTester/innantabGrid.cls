 
 /*------------------------------------------------------------------------
    File        : Grid
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Fri Feb 11 10:47:12 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Controls.Grid INHERITS Infragistics.Win.UltraWinGrid.UltraGrid:
      /* Funktioner för att spara markerade rader till OldRows med GuruSaveOldRows, samt selecta dom nya med GuruSelectNew. */
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

   DEFINE PROPERTY Guruegenskap AS Controls.GridRubrikLista NO-UNDO
      PUBLIC GET.   PRIVATE SET.
   
   DEFINE PRIVATE PROPERTY Gurudelfraga AS LOGICAL INITIAL TRUE NO-UNDO
   PRIVATE GET.   PRIVATE SET.
   
   DEFINE PRIVATE VARIABLE excelExporter AS Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE kopieraToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaExcel                    AS Controls.GuruExcelEmbed                                        NO-UNDO.
   
   METHOD PUBLIC VOID AddDragDropGroup(g AS Controls.Subclasses.DragDropGroup):
      THIS-OBJECT:DragDropGroups:Add(g).
   END METHOD.
      
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_AfterEnterEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ActiveCell:Activate().
		RETURN.

	END METHOD.



   @VisualDesigner.
   METHOD PRIVATE VOID Grid_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE mDragger AS Controls.Grid NO-UNDO.
      DEFINE VARIABLE Found    AS LOGICAL       INITIAL FALSE NO-UNDO.
      IF THIS-OBJECT:GuruDroppable = TRUE THEN DO:
         {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups}
            IF CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger NE ? THEN DO:
               mDragger = CAST(CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger, Controls.Grid).
               Found = TRUE.
            END.
         END.
         IF Found = TRUE THEN DO:
            IF mDragger NE THIS-OBJECT THEN DO:
               DEFINE VARIABLE args AS Controls.Subclasses.DragDropArgs NO-UNDO.
               args = NEW Controls.Subclasses.DragDropArgs(mDragger, mDragger:Selected:Rows).
               
               THIS-OBJECT:GuruAfterDropped:Publish(THIS-OBJECT, CAST(args, System.EventArgs)). 
            END.
         END.
      END. 
      {WALLMAN\foreach.i System.Object ooObject in THIS-OBJECT:DragDropGroups}
         CAST(ooObject, Controls.Subclasses.DragDropGroup):Dragger = ?.
      END.
      RETURN.
   END METHOD.
      
   @VisualDesigner.
   METHOD PRIVATE VOID Grid_DragOver( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE po AS System.Drawing.Point.
      DEFINE VARIABLE mDragger AS Controls.Grid NO-UNDO.
      DEFINE VARIABLE Found    AS LOGICAL       INITIAL FALSE NO-UNDO.           
      IF THIS-OBJECT:GuruDroppable = TRUE THEN DO:
         {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups}
            IF CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger NE ? THEN DO:
               mDragger = CAST(CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger, Controls.Grid).
               Found = TRUE.
            END.
         END.
         IF Found = TRUE THEN DO:
            IF mDragger NE THIS-OBJECT THEN DO:
               e:Effect = System.Windows.Forms.DragDropEffects:COPY.
               po = THIS-OBJECT:PointToClient( NEW System.Drawing.Point(e:X , e:Y ) ).
               IF po:Y < 20 THEN THIS-OBJECT:ActiveRowScrollRegion:Scroll(Infragistics.Win.UltraWinGrid.RowScrollAction:LineUp).
               IF po:Y > THIS-OBJECT:HEIGHT - 20 THEN THIS-OBJECT:ActiveRowScrollRegion:Scroll(Infragistics.Win.UltraWinGrid.RowScrollAction:LineDown).
            END.      
         END.
      END. 
      RETURN.
   END METHOD.

   @VisualDesigner.
   METHOD PRIVATE VOID Grid_SelectionDrag( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
      IF THIS-OBJECT:GuruDraggable = TRUE THEN DO:
         {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups}
            CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger = THIS-OBJECT.
         END.
         THIS-OBJECT:DoDragDrop(THIS-OBJECT:SELECTED:Rows, System.Windows.Forms.DragDropEffects:Copy).
      END. 
      RETURN.

   END METHOD.

   METHOD PUBLIC VOID RemoveDragDropGroup(g AS Controls.Subclasses.DragDropGroup):
      THIS-OBJECT:DragDropGroups:Remove(g).
   END METHOD.
   
   @VisualDesigner.
   METHOD PRIVATE VOID Grid_TextChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:GuruReopening EQ FALSE THEN DO:
         THIS-OBJECT:TITLE = THIS-OBJECT:TEXT.
      END.
      RETURN.

   END METHOD.

   METHOD PUBLIC VOID SetTitle(INPUT c AS CHARACTER):
      THIS-OBJECT:TITLE = c.
   END METHOD.
   
   METHOD PUBLIC INTEGER AntalRader():
       RETURN THIS-OBJECT:tBS:Count. 
      /*THIS-OBJECT:Rows:Count*/
   END METHOD.
   
   METHOD PUBLIC VOID GuruUpdateTitle():
      IF THIS-OBJECT:Guruegenskap:NoAutoTitle = TRUE THEN RETURN.
      IF THIS-OBJECT:TEXT EQ ? THEN RETURN.
      IF THIS-OBJECT:TEXT EQ "" THEN RETURN.
      IF THIS-OBJECT:tBS EQ ? THEN RETURN.
      THIS-OBJECT:GuruReopening = TRUE.
      IF THIS-OBJECT:GuruShowRowsCount EQ TRUE THEN DO:
         THIS-OBJECT:TEXT = THIS-OBJECT:TITLE + " - " + STRING(THIS-OBJECT:tBS:Count).
      END.
      ELSE DO:
         THIS-OBJECT:TEXT = THIS-OBJECT:TITLE.
      END.
      THIS-OBJECT:GuruReopening = FALSE.
   END METHOD.
   METHOD PUBLIC VOID GuruNoAutoTitle(INPUT NoAutoTitle AS LOGICAL):
      THIS-OBJECT:GuruReopening = TRUE.
      THIS-OBJECT:Guruegenskap:NoAutoTitle = NoAutoTitle.    
   END METHOD.
   
   
   METHOD PUBLIC VOID GuruUpdateTitleE(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GuruUpdateTitle().
   END METHOD.
   
   METHOD PUBLIC VOID GuruGridFarg(INPUT infarg AS LOGICAL):
      fargvar = infarg. 
      IF fargvar = TRUE THEN THIS-OBJECT:FilterRow:Subscribe(THIS-OBJECT:Grid_FilterRow).
   END METHOD.
   
   @VisualDesigner.
   METHOD PRIVATE VOID Grid_InitializeRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeRowEventArgs ):
      IF THIS-OBJECT:GuruColorConditions EQ ? THEN DO:
         THIS-OBJECT:GuruColorConditions = NEW System.Collections.ArrayList().
      END.
      e:ROW:Appearance:ResetBackColor().
      DEFINE VARIABLE meh AS INTEGER INITIAL 0.
      DO WHILE meh LE e:ROW:Cells:Count - 1:
         e:ROW:Cells[meh]:Appearance:ResetBackColor().
         meh = meh + 1.
      END.
      {WALLMAN\foreach.i Controls.Subclasses.GuruColorCondition c in THIS-OBJECT:GuruColorConditions}
         IF c:Compare(string(e:ROW:Cells[c:CellName]:Value)) EQ TRUE THEN DO:
            IF c:WholeRow EQ TRUE THEN DO:
               e:ROW:Appearance:BackColor = c:COLOR.
            END.
            ELSE DO:
               e:ROW:Cells[c:CellNameColor]:Appearance:BackColor = c:COLOR.
            END.
         END.
      END.
      RETURN.
   
   END METHOD.
   
   
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_FilterRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.FilterRowEventArgs ):
      IF THIS-OBJECT:GuruColorConditions EQ ? THEN DO:
         THIS-OBJECT:GuruColorConditions = NEW System.Collections.ArrayList().
      END.
      
      e:ROW:Appearance:ResetBackColor().
      DEFINE VARIABLE meh AS INTEGER INITIAL 0.
      DO WHILE meh LE e:ROW:Cells:Count - 1:
         e:ROW:Cells[meh]:Appearance:ResetBackColor().
         meh = meh + 1.
      END.
    /*
      MESSAGE e:ROW:Cells[0]:value e:ROW:Cells[13]:value "lista" skip 
      e:ROW:Cells[2]:value e:ROW:Cells[14]:value  "valt"
      VIEW-AS ALERT-BOX.
      */
      {WALLMAN\foreach.i Controls.Subclasses.GuruColorCondition c in THIS-OBJECT:GuruColorConditions}
      
         IF c:Compare(string(e:ROW:Cells[c:CellName]:Value)) EQ TRUE THEN DO:
            IF c:WholeRow EQ TRUE THEN DO:
               e:ROW:Appearance:BackColor = c:COLOR.
            END.
            ELSE DO:
               e:ROW:Cells[c:CellNameColor]:Appearance:BackColor = c:COLOR.
            END.
         END.
      END.
      RETURN.
   
	END METHOD.
   
   METHOD PUBLIC VOID GuruAddConditionalColor(INPUT cellname AS CHARACTER,INPUT contype AS INTEGER,INPUT cellval AS CHARACTER, INPUT celltocolor AS CHARACTER, INPUT cellcol AS System.Drawing.Color):
      IF THIS-OBJECT:GuruColorConditions EQ ? THEN DO:
         THIS-OBJECT:GuruColorConditions = NEW System.Collections.ArrayList().
      END.
      THIS-OBJECT:GuruColorConditions:Add(NEW Controls.Subclasses.GuruColorCondition(THIS-OBJECT, cellcol, cellname, contype, cellval, false, celltocolor)).
   END METHOD.
   
   METHOD PUBLIC VOID GuruAddConditionalColor(INPUT cellname AS CHARACTER,INPUT contype AS INTEGER,INPUT cellval AS CHARACTER,INPUT cellcol AS System.Drawing.Color):
      IF THIS-OBJECT:GuruColorConditions EQ ? THEN DO:
         THIS-OBJECT:GuruColorConditions = NEW System.Collections.ArrayList().
      END.
      THIS-OBJECT:GuruColorConditions:Add(NEW Controls.Subclasses.GuruColorCondition(THIS-OBJECT, cellcol, cellname, contype, cellval)).
   END METHOD.

   METHOD PUBLIC VOID GuruSetColumnBgColor(INPUT fx AS CHARACTER, INPUT cx AS System.Drawing.Color):
      THIS-OBJECT:DisplayLayout:Bands[0]:Columns[fx]:CellAppearance:BackColor = cx.
   END METHOD.
   METHOD PUBLIC VOID GuruSetColumnFgColor(INPUT fx AS CHARACTER, INPUT cx AS System.Drawing.Color):
      THIS-OBJECT:DisplayLayout:Bands[0]:Columns[fx]:CellAppearance:ForeColor = cx.
   END METHOD.
   /* För markering och positionering */
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_AfterExitEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       IF THIS-OBJECT:ActiveRow EQ ? THEN RETURN.
       ELSE DO:
         THIS-OBJECT:ActiveRow:Selected = TRUE.
       END.
		RETURN.

	END METHOD.

   /* Allmänt */
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_Error( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.ErrorEventArgs ):
		System.Windows.Forms.MessageBox:Show("Infragistics fel: " + e:ErrorText).
		RETURN.

	END METHOD.
   CONSTRUCTOR PUBLIC Grid ( ):      
      SUPER ().
      InitializeComponent ( ).
      THIS-OBJECT:DragDropGroups = NEW System.Collections.ArrayList().
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC Grid ( ):
      /*{WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups }
         CAST(oObject, Controls.Subclasses.DragDropGroup):RemoveChild(THIS-OBJECT).
      END.*/
   END DESTRUCTOR.

   METHOD PUBLIC VOID GuruSaveOldRows():
      THIS-OBJECT:oldRows = NEW System.Collections.ArrayList().
      THIS-OBJECT:oldRows:Clear(). 
      IF THIS-OBJECT:Rows NE ? THEN DO:
         IF THIS-OBJECT:Rows:Count > 0 THEN DO:
            {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Rows}
                 THIS-OBJECT:oldRows:Add( CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Cells["TTRECID"]:VALUE ) .
            END.
            THIS-OBJECT:hasRows = TRUE.
            RETURN.
         END.
      END.
      THIS-OBJECT:hasRows = FALSE.
   END METHOD.
  
   METHOD PUBLIC VOID GuruClearSelection():
      IF THIS-OBJECT:Rows:Count > 0 THEN DO: 
         {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Rows}
            CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Selected = FALSE.
         END.
      END.
   END METHOD.
   
   METHOD PUBLIC LOGICAL GuruFindHash(INPUT a AS CHARACTER):
      IF THIS-OBJECT:hasRows = FALSE THEN RETURN TRUE.
      IF THIS-OBJECT:oldRows:Count > 0 THEN DO:
         {WALLMAN\foreach.i System.Object ocObject in THIS-OBJECT:oldRows}
            IF STRING(ocObject:ToString()) = a THEN DO:
               RETURN TRUE.
            END.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID GuruSelectNewRows():
      IF THIS-OBJECT:Rows:Count > THIS-OBJECT:oldRows:Count THEN DO: 
         {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Rows}
            IF THIS-OBJECT:oldRows:Count > 0 THEN DO:
               /* STRING(CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Cells["TTRECID"]:VALUE )*/ 
               IF GuruFindHash( STRING(CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Cells["TTRECID"]:VALUE )) NE TRUE THEN DO:
                  CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Selected = TRUE.
                  CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Activate().
               END.
            END.
            ELSE DO:
               CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Selected = TRUE.
               CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Activate().
            END.
         END.
      END.
   END METHOD.

   /*   Denna metod bestämmer om en knapp ska visas, skicka med lämplig variabel från Start.Styrsek*/
   METHOD PUBLIC VOID guruSekUser(sekv AS LOGICAL):      
      THIS-OBJECT:VISIBLE = sekv.
      THIS-OBJECT:ENABLED = sekv.               
   END METHOD.  
   /* 
         Lägger till en rad i context-menyn
         1. dra in en contextmenustrip i Visual Designern (microsoft controls)
         2. Gör i ordning menyn med olika val samt eventhandlers (on click osv...)
         3. Skicka in respektive ToolStripMenuItem hit, varje val/rad i contextmenu-strippen är en ToolStripMenuItem
         
         Observera att följande val Alltid kommer att finnas i context-menun:
            Till Excel
            Kopiera Markerad Text         
   */
   
   METHOD PUBLIC VOID VisaViaInternetMenuItemAddAdd():      
      THIS-OBJECT:ContextMenuStrip:Items:Add(VisaViaInternetMenuItem).      
   END METHOD.
   METHOD PUBLIC VOID GuruContextAdd(itam AS System.Windows.Forms.ToolStripMenuItem):      
      THIS-OBJECT:ContextMenuStrip:Items:Add(itam).      
   END METHOD.

   /* Lägga till context meny (tar bara en rad sen klagar på att collectionen ändrats) */   
   /*METHOD PUBLIC VOID GuruContextAdd(men AS System.Windows.Forms.ContextMenuStrip):      
      DEFINE VARIABLE titem AS  System.Windows.Forms.ToolStripMenuItem NO-undo.
            
      {WALLMAN/foreach.i System.Object oObject in men:Items}
         titem = NEW System.Windows.Forms.ToolStripMenuItem().
         titem = CAST (oObject, System.Windows.Forms.ToolStripMenuItem) NO-ERROR .         
         THIS-OBJECT:ContextMenuStrip:Items:Add(titem).         
      END.            
   END METHOD.
   
   /* DETTA BORDE FUNKA MED EN COPY IST FÖR ASSIGN/ elpfh*/
   
   METHOD PUBLIC VOID GuruContextAdd(men AS System.Windows.Forms.ContextMenuStrip):      
           
      {WALLMAN/foreach.i System.Object oObject in men:Items}
         DEFINE VARIABLE titem AS  System.Windows.Forms.ToolStripMenuItem NO-undo.
         titem = CAST (oObject, System.Windows.Forms.ToolStripMenuItem) NO-ERROR .         
         THIS-OBJECT:ContextMenuStrip:Items:Add(titem).         
      END.            
   END METHOD.
   
   */ 
   /*MULITGRID funkar inte efter GuruInitGrid är körd*/
   METHOD PUBLIC VOID GuruMultiGrid(multi AS LOGICAL):
      Guruegenskap:multiselect = multi.
   END METHOD.                                                                                                                                                
   /* Initierar en grid */
   METHOD PUBLIC VOID GuruInitGrid(egenskaper AS Controls.GridRubrikLista, INPUT korej AS CHARACTER  ):
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE numcol AS INTEGER NO-UNDO.
      DEFINE VARIABLE rubfinns AS LOGICAL NO-UNDO.
      DEFINE VARIABLE numrub AS INTEGER NO-UNDO. /*antal rubriker räknare */
      DEFINE VARIABLE incFields AS CHARACTER NO-UNDO.
      /*
      THIS-OBJECT:tbs:SortRequest:Subscribe(SortByColumn).
      */
      THIS-OBJECT:excelExporter = NEW Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter().
      /* Default context menu */
      THIS-OBJECT:ContextMenuStrip = NEW System.Windows.Forms.ContextMenuStrip().
      THIS-OBJECT:ContextMenuStrip:Name = "contextMenuStrip3".
      THIS-OBJECT:ContextMenuStrip:Size = NEW System.Drawing.Size(153, 48).
      /* Till Excel markering*/
      THIS-OBJECT:excelMarkToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:excelMarkToolStripMenuItem:Name = "excelToolStripMenuItem".
      THIS-OBJECT:excelMarkToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:excelMarkToolStripMenuItem:Text = "Skicka markering till Excel".
      THIS-OBJECT:excelMarkToolStripMenuItem:Click:Subscribe(THIS-OBJECT:excelMarkToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:excelMarkToolStripMenuItem).
      /* Till Excel */
      THIS-OBJECT:excelToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:excelToolStripMenuItem:Name = "excelToolStripMenuItem".
      THIS-OBJECT:excelToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:excelToolStripMenuItem:Text = "Skicka hela listan till Excel".
      THIS-OBJECT:excelToolStripMenuItem:Click:Subscribe(THIS-OBJECT:excelToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:excelToolStripMenuItem).
      
      /* Skriv ut markering */
      THIS-OBJECT:skrivutMarkToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Name = "skrivutMarkToolStripMenuItem".
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Text = "Skriv ut markering".
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Click:Subscribe(THIS-OBJECT:skrivutMarkToolStripMenuItem_Click).                
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:skrivutMarkToolStripMenuItem).
      /* Skriv ut grid */
      THIS-OBJECT:skrivutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:skrivutToolStripMenuItem:Name = "skrivutToolStripMenuItem".
      THIS-OBJECT:skrivutToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:skrivutToolStripMenuItem:Text = "Skriv ut hela listan".
      THIS-OBJECT:skrivutToolStripMenuItem:Click:Subscribe(THIS-OBJECT:skrivutToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:skrivutToolStripMenuItem).
      
      /* Kopiera Text */
      THIS-OBJECT:kopieraToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:kopieraToolStripMenuItem:Name = "kopieraToolStripMenuItem".
      THIS-OBJECT:kopieraToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:kopieraToolStripMenuItem:Text = "Kopiera markerad text".
      THIS-OBJECT:kopieraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:kopieraToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:kopieraToolStripMenuItem).
      /* Via Internet */
      THIS-OBJECT:VisaViaInternetMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItem:Name = "VisaViaInternetMenuItem".
      THIS-OBJECT:VisaViaInternetMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItem:Text = "Visa information via vald Leverantör".
      THIS-OBJECT:VisaViaInternetMenuItem:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItem_Click).            
      /* slut contextmenustrip */
      Guruegenskap = egenskaper.
      /* viktig för att kolumnerna ska hamna i Ordnings-ordning, dvs GridRubrik:Ordning. */
      /* egenskaper:Sortera().*/
      Guruegenskap:Sortera().
      /*Sätta buffer och query*/
      CREATE QUERY tqH.
      tqH:SET-BUFFERS(Guruegenskap:ttBufferHandle).
      /* 
      Sätter filter och öppnar query.
      Ersätter de tre raderna ovan för att man ska kunna ändra filter         
      */
      GuruFiltrera(Guruegenskap:filter). 
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource(tqH, "*", "").            
      THIS-OBJECT:DataSource = tBS.
      /*Robin expri*/
      THIS-OBJECT:tbs:AutoSort  = TRUE.
      THIS-OBJECT:DisplayLayout:Override:HeaderClickAction  = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortSingle.

      THIS-OBJECT:GuruDefaults(). 
      IF Guruegenskap:multiselect = FALSE THEN
         THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
      ELSE
         THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
      IF Guruegenskap:tillatSort = FALSE THEN DO:
         THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
         THIS-OBJECT:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
      END.
      IF Guruegenskap:tillatDelete = FALSE THEN DO:
         THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
      END.
      /* Egenskaper för kolumner (ordning,Rubrik) */
      numrub = 0.
      
      DO WHILE numrub < egenskaper:countRubrik():
         temprub = egenskaper:getRubrik(numrub).
         /* Ordning,Rubrik,Visible på kolumner */
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:VisiblePosition = temprub:Ordning.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:Caption = temprub:Rubrik.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Hidden = temprub:Gomma.         
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Width = temprub:Bredd.
         /*ROBIN raden under denna är flyttad upp hit 29/5/13:defaulta färger*/
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance = setApperance(temprub:Falt).
         
         /*Anger om det ska vara rowselectors (puckar), default = false, men sätter till true om det finns editerbara fält*/
         IF temprub:Readonly = FALSE THEN DO:            
            IF THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() = "False" THEN DO:            
/*               IF THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False THEN MESSAGE "funka"  VIEW-AS ALERT-BOX.*/
               THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
/*               MESSAGE "sätta" + THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() VIEW-AS ALERT-BOX.*/
            END.
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:BackColor = System.Drawing.Color:AliceBlue.
         END. 
         IF temprub:Typvar = "DEC" OR temprub:Typvar = "INT" THEN DO:
            /*THIS-OBJECT:DisplayLayout:Bands[0]:Override:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.*/
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.
         END.
         
          IF temprub:Typvar = "TTRECID" THEN DO:
            /*Ändra för att visa TTRECID-kollumen*/
            IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN THIS-OBJECT:DisplayLayout:Bands[0]:Columns["TTRECID"]:Hidden = TRUE.
            ELSE THIS-OBJECT:DisplayLayout:Bands[0]:Columns["TTRECID"]:Hidden = TRUE.
         END.
         
         IF temprub:Typvar = "DATETIME" THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:MaskInput = "~{LOC~}yyyy/mm/dd hh:mm:ss tt". 
         END.   
/*         THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.*/
         /*ROBIN Här under LÅG raden för :defaulta färger före 29/5/13*/
         IF temprub:Readonly = TRUE THEN DO:
            /*Read only cell samt att hela raden selectas om man klickar i en read-only cell*/
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
         END.
         ELSE DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
         END.
         GuruCombocell(temprub). /* combobox i grid*/
         IF temprub:Falt = "flog" THEN DO:         
         END.
         numrub = numrub + 1.
      END.
      /* Gömma ej definierade columner, dvs kolumner som finns i temp-table, 
      men inte finns i GridRubrikLista-objektet.*/
      numrub = 0.
      numcol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count.
      DO WHILE numrub < numcol:
         rubfinns = egenskaper:finnsRubrik(THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numrub]:Header:Caption).
         IF rubfinns = FALSE THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numrub]:Hidden = TRUE.
         END.
         numrub = numrub + 1.
      END.
      /* end gömma*/
      gridProperties().      
      THIS-OBJECT:tBS:ListChanged:Subscribe(GuruUpdateTitleE).
      THIS-OBJECT:Initierad = TRUE.
     
   END METHOD.
   
      /* Initierar en grid */
   METHOD PUBLIC VOID GuruInitGrid(egenskaper AS Controls.GridRubrikLista):
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE numcol AS INTEGER NO-UNDO.
      DEFINE VARIABLE rubfinns AS LOGICAL NO-UNDO.
      DEFINE VARIABLE numrub AS INTEGER NO-UNDO. /*antal rubriker räknare */
      DEFINE VARIABLE numfield AS INTEGER NO-UNDO.
      DEFINE VARIABLE numfieldrub AS INTEGER NO-UNDO.
      DEFINE VARIABLE incFields AS CHARACTER NO-UNDO.
     
      /*
      THIS-OBJECT:tbs:SortRequest:Subscribe(SortByColumn).
      */
   
      THIS-OBJECT:excelExporter = NEW Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter().
      /* Default context menu */
      THIS-OBJECT:ContextMenuStrip = NEW System.Windows.Forms.ContextMenuStrip().
      THIS-OBJECT:ContextMenuStrip:Name = "contextMenuStrip3".
      THIS-OBJECT:ContextMenuStrip:Size = NEW System.Drawing.Size(153, 48).
      /* Till Excel markering*/
      THIS-OBJECT:excelMarkToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:excelMarkToolStripMenuItem:Name = "excelToolStripMenuItem".
      THIS-OBJECT:excelMarkToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:excelMarkToolStripMenuItem:Text = "Skicka markering till Excel".
      THIS-OBJECT:excelMarkToolStripMenuItem:Click:Subscribe(THIS-OBJECT:excelMarkToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:excelMarkToolStripMenuItem).
      /* Till Excel */
      THIS-OBJECT:excelToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:excelToolStripMenuItem:Name = "excelToolStripMenuItem".
      THIS-OBJECT:excelToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:excelToolStripMenuItem:Text = "Skicka hela listan till Excel".
      THIS-OBJECT:excelToolStripMenuItem:Click:Subscribe(THIS-OBJECT:excelToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:excelToolStripMenuItem).
      
      /* Skriv ut markering */
      THIS-OBJECT:skrivutMarkToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Name = "skrivutMarkToolStripMenuItem".
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Text = "Skriv ut markering".
      THIS-OBJECT:skrivutMarkToolStripMenuItem:Click:Subscribe(THIS-OBJECT:skrivutMarkToolStripMenuItem_Click).                
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:skrivutMarkToolStripMenuItem).
      /* Skriv ut grid */
      THIS-OBJECT:skrivutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:skrivutToolStripMenuItem:Name = "skrivutToolStripMenuItem".
      THIS-OBJECT:skrivutToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:skrivutToolStripMenuItem:Text = "Skriv ut hela listan".
      THIS-OBJECT:skrivutToolStripMenuItem:Click:Subscribe(THIS-OBJECT:skrivutToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:skrivutToolStripMenuItem).
      
      /* Kopiera Text */
      THIS-OBJECT:kopieraToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:kopieraToolStripMenuItem:Name = "kopieraToolStripMenuItem".
      THIS-OBJECT:kopieraToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:kopieraToolStripMenuItem:Text = "Kopiera markerad text".
      THIS-OBJECT:kopieraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:kopieraToolStripMenuItem_Click).            
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:kopieraToolStripMenuItem).
      /* Via Internet */
      THIS-OBJECT:VisaViaInternetMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItem:Name = "VisaViaInternetMenuItem".
      THIS-OBJECT:VisaViaInternetMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItem:Text = "Visa information via vald Leverantör".
      THIS-OBJECT:VisaViaInternetMenuItem:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItem_Click).            
      /* slut contextmenustrip */
      Guruegenskap = egenskaper.
      /* viktig för att kolumnerna ska hamna i Ordnings-ordning, dvs GridRubrik:Ordning. */
      /* egenskaper:Sortera().*/
      Guruegenskap:Sortera().
      /*Sätta buffer och query*/
      CREATE QUERY tqH.
      tqH:SET-BUFFERS(Guruegenskap:ttBufferHandle).
      /* 
      Sätter filter och öppnar query.
      Ersätter de tre raderna ovan för att man ska kunna ändra filter         
      */
      GuruFiltrera(Guruegenskap:filter). 
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource(tqH, "*", "").            
      THIS-OBJECT:DataSource = tBS.
      /*Robin expri*/
      THIS-OBJECT:tbs:AutoSort  = TRUE.
      THIS-OBJECT:DisplayLayout:Override:HeaderClickAction  = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortSingle.

      THIS-OBJECT:GuruDefaults(). 
      IF Guruegenskap:multiselect = FALSE THEN
         THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
      ELSE
         THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
      IF Guruegenskap:tillatSort = FALSE THEN DO: 
         THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
         THIS-OBJECT:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
      END.
      IF Guruegenskap:tillatDelete = FALSE THEN DO:
         THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
      END.
      /* Egenskaper för kolumner (ordning,Rubrik) */
   
      numrub = 0.
      numfield = 0.
      numcol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count.
      numfieldrub = egenskaper:countRubrik().
      DO WHILE numfield < numcol:
         rubfinns = egenskaper:finnsfalt(THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numfield]:key).
         IF rubfinns = FALSE THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numfield]:Hidden = TRUE.
         END.
         ELSE DO:
         IF numrub < numfieldrub tHEN DO:
            temprub = egenskaper:getRubrik(numrub).
            /* Ordning,Rubrik,Visible på kolumner */
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:VisiblePosition = temprub:Ordning.
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:HEADER:Caption = temprub:Rubrik.
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Hidden = temprub:Gomma.         
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:Width = temprub:Bredd.
            /*ROBIN raden under denna är flyttad upp hit 29/5/13:defaulta färger*/
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance = setApperance(temprub:Falt).
            
            /*Anger om det ska vara rowselectors (puckar), default = false, men sätter till true om det finns editerbara fält*/
            IF temprub:Readonly = FALSE THEN DO:            
               IF THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() = "False" THEN DO:            
   /*               IF THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False THEN MESSAGE "funka"  VIEW-AS ALERT-BOX.*/
                  THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
   /*               MESSAGE "sätta" + THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() VIEW-AS ALERT-BOX.*/
               END.
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:BackColor = System.Drawing.Color:AliceBlue.
            END. 
            IF temprub:Typvar = "DEC" OR temprub:Typvar = "INT" THEN DO:
               /*THIS-OBJECT:DisplayLayout:Bands[0]:Override:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.*/
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.
            END.
            
             IF temprub:Typvar = "TTRECID" THEN DO:
               /*Ändra för att visa TTRECID-kollumen*/
               IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN THIS-OBJECT:DisplayLayout:Bands[0]:Columns["TTRECID"]:Hidden = TRUE.
               ELSE THIS-OBJECT:DisplayLayout:Bands[0]:Columns["TTRECID"]:Hidden = TRUE.
            END.
            
            IF temprub:Typvar = "DATETIME" THEN DO:
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:MaskInput = "~{LOC~}yyyy/mm/dd hh:mm:ss tt". 
            END.   
   /*         THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.*/
            /*ROBIN Här under LÅG raden för :defaulta färger före 29/5/13*/
            IF temprub:Readonly = TRUE THEN DO:
               /*Read only cell samt att hela raden selectas om man klickar i en read-only cell*/
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
            END.
            ELSE DO:
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
            END.
            GuruCombocell(temprub). /* combobox i grid*/
            IF temprub:Falt = "flog" THEN DO:         
            END.
            numrub = numrub + 1.
         END.
      END.
         numfield = numfield + 1.
      END.
      /* end gömma*/
      gridProperties().      
      THIS-OBJECT:tBS:ListChanged:Subscribe(GuruUpdateTitleE).
      THIS-OBJECT:Initierad = TRUE.
     
   
   END METHOD.  
      
        
   METHOD PUBLIC VOID VisaMtrlInter():
       DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE lankst AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE THEN.
      ELSE RETURN. 
      IF Guru.Konstanter:appcon THEN DO:
         RUN LEVLANKC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guruegenskap:ttBufferHandle:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, INPUT Guruegenskap:ttBufferHandle:BUFFER-FIELD("ENR"):BUFFER-VALUE, OUTPUT lankst).
      END.
      ELSE DO:
         RUN LEVLANKC.P (INPUT Guruegenskap:ttBufferHandle:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, INPUT Guruegenskap:ttBufferHandle:BUFFER-FIELD("ENR"):BUFFER-VALUE, OUTPUT lankst).
      END. 
      IF lankst NE "" THEN DO: 
         RUN OPENDOC.P (lankst,"","",NO). 
      END.
      ELSE DO:
         res = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(63)," ", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
      END.   
    
   END METHOD.  
    
   /* Till excel handler mark*/
   METHOD PRIVATE VOID excelMarkToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:excelExporter:RowExporting:Subscribe(THIS-OBJECT:tillExcelMark).
      THIS-OBJECT:GurutillExcel().
      THIS-OBJECT:excelExporter:RowExporting:Unsubscribe(THIS-OBJECT:tillExcelMark).
      RETURN.
   END METHOD.
   
  /* Till excel handler*/
   METHOD PRIVATE VOID excelToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GurutillExcel().
      RETURN.
   END METHOD.
   
   /* Skriv ut markering handler*/
   METHOD PRIVATE VOID skrivutMarkToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      /*Döljer rader*/
      THIS-OBJECT:InitializeRow:Subscribe(THIS-OBJECT:PrintInitializeRow).
      THIS-OBJECT:SkrivUt().  
      THIS-OBJECT:InitializeRow:Unsubscribe(THIS-OBJECT:PrintInitializeRow).     
      RETURN.
   END METHOD.
   
   /* Skriv ut handler*/
   METHOD PRIVATE VOID skrivutToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:SkrivUt().
      RETURN.
   END METHOD.
   
   /* Till via internet*/
   METHOD PRIVATE VOID VisaViaInternetMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:VisaMtrlInter().
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
      THIS-OBJECT:AfterExitEditMode:Subscribe(THIS-OBJECT:Grid_AfterExitEditMode).
      THIS-OBJECT:Error:Subscribe(THIS-OBJECT:Grid_Error).
      THIS-OBJECT:AfterCellUpdate:Subscribe(THIS-OBJECT:Grid_AfterExitEditMode).
      THIS-OBJECT:AfterEnterEditMode:Subscribe(THIS-OBJECT:Grid_AfterEnterEditMode).
      THIS-OBJECT:DragDrop:Subscribe(THIS-OBJECT:Grid_DragDrop).
     
      /*
      THIS-OBJECT:InitializeRow:Subscribe(THIS-OBJECT:Grid_InitializeRow).
      */
      /*
      THIS-OBJECT:FilterRow:Subscribe(THIS-OBJECT:Grid_FilterRow).
      */
      THIS-OBJECT:DragOver:Subscribe(THIS-OBJECT:Grid_DragOver).
      THIS-OBJECT:SelectionDrag:Subscribe(THIS-OBJECT:Grid_SelectionDrag).
      THIS-OBJECT:TextChanged:Subscribe(THIS-OBJECT:Grid_TextChanged).
      CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.
   
   /* Kopiera Handler */
   METHOD PRIVATE VOID kopieraToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      KopieraRad().      
      RETURN.
   END METHOD.
   /*sätter format på column*/
   METHOD PUBLIC VOID GuruGridFormatCol():

   END METHOD.
   /*TÖMMER GRID*/
   METHOD PUBLIC VOID GuruTom():
      IF NOT VALID-HANDLE(Guruegenskap:ttBufferHandle) THEN RETURN.
      ttSortQuery = "FOR EACH " + Guruegenskap:ttBufferHandle:TABLE + " WHERE TTRECID = ?" .
      tqH:QUERY-CLOSE. 
      tqH:QUERY-PREPARE(ttSortQuery).
      tqH:QUERY-OPEN ().
      /*
       THIS-OBJECT:Guruegenskap:filter = "".
       */    
   END METHOD. 
   METHOD PUBLIC VOID GuruReopen():
      EMPTY TEMP-TABLE rowidlista NO-ERROR.
      {WALLMAN/foreachultra.i System.Object oObject in THIS-OBJECT:Selected:Rows}           
         THIS-OBJECT:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
         IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE = TRUE THEN DO:
            CREATE rowidlista.
            ASSIGN 
            rowidlista.id = THIS-OBJECT:Guruegenskap:ttBufferHandle:ROWID.
         END.
      END.
      FIND FIRST rowidlista WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE rowidlista THEN DO:
         IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE = TRUE THEN DO:
            CREATE rowidlista.
            ASSIGN 
            rowidlista.id = THIS-OBJECT:Guruegenskap:ttBufferHandle:ROWID.
         END. 
      END.
      THIS-OBJECT:GuruFiltrera().
      THIS-OBJECT:GuruAvmarkera().
      THIS-OBJECT:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE).
    
   END METHOD.
   
   
   
      
   /*Väljer första raden i en grid, t.ex. vid start av programmet*/
   METHOD PUBLIC VOID GuruFirstrow():
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:FirstRowInGrid)  NO-ERROR.
   END METHOD.
   METHOD PUBLIC VOID GuruFirstrow(OUTPUT gridrowid AS ROWID):
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:FirstRowInGrid)  NO-ERROR. 
      gridrowid = THIS-OBJECT:Guruegenskap:ttBufferHandle:ROWID.
      /*THIS-OBJECT:GuruAfterDropped:Publish(THIS-OBJECT, CAST(args, System.EventArgs)).
      THIS-OBJECT:tBS:PositionChanged:Publish(). 
      */
      
   END METHOD.
   
   METHOD PUBLIC VOID GuruCurrentRowid(inrowid AS ROWID):
      valdgridrowid = inrowid.      
   END METHOD.
   METHOD PUBLIC VOID GuruGetCurrentRowid():
      THIS-OBJECT:GuruRadvaljare(valdgridrowid, FALSE).
   END METHOD.
   METHOD PUBLIC VOID GuruLastrow():
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:LastRowInGrid) NO-ERROR.      
   END METHOD.

   METHOD PUBLIC VOID GuruAvmarkera():
      THIS-OBJECT:Selected:Rows:Clear().
   END METHOD.
   
   /*Döljer rader som inte är markerade*/
   METHOD PUBLIC VOID PrintInitializeRow(INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeRowEventArgs):
      DEFINE VARIABLE oOnScreenRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.     
         e:Row:HIDDEN = TRUE.
         oOnScreenRow = GetRowFromPrintRow(e:Row).
         IF oOnScreenRow:SELECTED = TRUE THEN e:Row:HIDDEN = FALSE.
   END METHOD.
    
   /*Skriv ut grid med hjälp av Winformskontroller*/
   METHOD PUBLIC VOID SkrivUt():
      
      DEFINE VARIABLE c_pdSetup         AS System.Drawing.Printing.PrintDocument.
      DEFINE VARIABLE pageSetupDialog1  AS System.Windows.Forms.PageSetupDialog.
      DEFINE VARIABLE printSetupDialog1 AS System.Windows.Forms.PrintDialog.  
                
      c_pdSetup = NEW System.Drawing.Printing.PrintDocument().
               
      pageSetupDialog1 = NEW System.Windows.Forms.PageSetupDialog().
      pageSetupDialog1:Document = c_pdSetup.
      WAIT-FOR pageSetupDialog1:ShowDialog().
               
      printSetupDialog1 = NEW System.Windows.Forms.PrintDialog().
      WAIT-FOR printSetupDialog1:ShowDialog().
      
      THIS-OBJECT:PrintPreview(THIS-OBJECT:DisplayLayout, c_pdSetup).
      
      RETURN.
   END METHOD.

   /*
   /*Skriv ut grid med hjälp av ultragridkontroller: Används ej, behövs printSetup, dock snyggare*/
   METHOD PUBLIC VOID SkrivUtUltraGrid():
    
      DEFINE VARIABLE uGPrintPreviewDialog AS Infragistics.Win.Printing.UltraPrintPreviewDialog.  
      DEFINE VARIABLE uGPrintDocument AS Infragistics.Win.UltraWinGrid.UltraGridPrintDocument.

      uGPrintDocument = NEW Infragistics.Win.UltraWinGrid.UltraGridPrintDocument().
      uGPrintDocument:Grid = THIS-OBJECT.
      
      uGPrintPreviewDialog = NEW Infragistics.Win.Printing.UltraPrintPreviewDialog(). 
      uGPrintPreviewDialog:Document = uGPrintDocument.

      WAIT-FOR uGPrintPreviewDialog:ShowDialog(THIS-OBJECT).
      
      RETURN.
      
   END METHOD.
   */
  
   /*för excel*/
   METHOD PRIVATE CHARACTER SaveFilePrompt():
      DEFINE VARIABLE filnamn AS CHAR    NO-UNDO.
      DEFINE VARIABLE ok       AS LOGICAL NO-UNDO.
      SYSTEM-DIALOG GET-FILE filnamn
         DEFAULT-EXTENSION "xls"
         FILTERS  "Excel Workbook (*.xls)"  "*.xls", 
         "All Files (*.*)" "*.*"
         USE-FILENAME
         SAVE-AS
         UPDATE ok.
      IF ok THEN RETURN filnamn.    
      ELSE  RETURN "".       
   END METHOD.    
   
   /* Tar bort (avbryter) de rader som inte är markerade */
   METHOD PRIVATE VOID tillExcelMark(INPUT sender AS System.Object,INPUT e AS Infragistics.Win.UltraWinGrid.ExcelExport.RowExportingEventArgs ):
      DEFINE VARIABLE oExportRow   AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE oOnScreenRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE oGrid        AS Infragistics.Win.UltraWinGrid.UltraGrid    NO-UNDO.
      oExportRow = e:GridRow.          
      oGrid = CAST(e:GridRow:Band:Layout:Grid, Infragistics.Win.UltraWinGrid.UltraGrid).
      oOnScreenRow = oGrid:GetRowFromPrintRow(oExportRow). 
      IF oOnScreenRow:SELECTED = FALSE AND Guruegenskap:multiselect = TRUE THEN e:Cancel = TRUE.
      ELSE  e:Cancel = FALSE.                 
      RETURN.   
   END METHOD.   

   /* Alla poster till excel */
   METHOD PUBLIC VOID GurutillExcel():
      DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER                         NO-UNDO.
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      /*
      filnamn = THIS-OBJECT:SaveFilePrompt().
     
      filnamn = "c:\protemp10\klip.xls".
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:VisaExcel:StartExcel().
      THIS-OBJECT:VisaExcel:SaveFileAs(filnamn).
      THIS-OBJECT:VisaExcel:SlutExcel().
      /*THIS-OBJECT:VisaExcel:OpenExcel(ExelKommandout). */
      */
       filnamn = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
       filnamn = filnamn + "klipput.xls".
      THIS-OBJECT:excelExporter:Export(THIS-OBJECT,filnamn).
    
      System.Diagnostics.Process:Start(filnamn).
   
   END METHOD.        
   
   METHOD PRIVATE VOID KopieraRad():
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:Copy, false, false).
   END METHOD.
   
   /* Fångar event för pil upp,pil ned samt enter */
   METHOD PRIVATE VOID KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
      DEFINE VARIABLE ingentraff AS LOGICAL NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE lastshown AS INTEGER NO-UNDO.
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cellNamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE cellNamnTabTo AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
      keykodestring = e:KEYCODE:ToString(). /*Verkar inte fungera att jämföra e:KeyCode med System.Windows.Forms.Keys:Enter */
      cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
      IF cellNamn = "" OR THIS-OBJECT:ActiveCell:CanEnterEditMode = FALSE THEN DO:
         temprub = THIS-OBJECT:Guruegenskap:GetGridCol(ActiveValCol).
         sok = TRUE.
         
         IF temprub = ? THEN DO:
            temprub = THIS-OBJECT:Guruegenskap:getRubrik(1).
         END.
      END.   
      /*
      http://msdn.microsoft.com/en-us/library/system.windows.forms.keys.aspx
      */
      IF keykodestring = "Oem6" THEN keykodestring = "å".
      IF keykodestring = "Oem7" THEN keykodestring = "ä".
      IF keykodestring = "Oemtilde" THEN keykodestring = "ö".
      IF keykodestring = "Space" THEN keykodestring = " ". 
      IF keykodestring = "D0" THEN keykodestring = "0".
      IF keykodestring = "D1" THEN keykodestring = "1".
      IF keykodestring = "D2" THEN keykodestring = "2".
      IF keykodestring = "D3" THEN keykodestring = "3".
      IF keykodestring = "D4" THEN keykodestring = "4".
      IF keykodestring = "D5" THEN keykodestring = "5".
      IF keykodestring = "D6" THEN keykodestring = "6".
      IF keykodestring = "D7" THEN keykodestring = "7".
      IF keykodestring = "D8" THEN keykodestring = "8".
      IF keykodestring = "D9" THEN keykodestring = "9". 
      IF keykodestring = "NumPad0" THEN keykodestring = "0".
      IF keykodestring = "NumPad1" THEN keykodestring = "1".
      IF keykodestring = "NumPad2" THEN keykodestring = "2".
      IF keykodestring = "NumPad3" THEN keykodestring = "3".
      IF keykodestring = "NumPad4" THEN keykodestring = "4".
      IF keykodestring = "NumPad5" THEN keykodestring = "5".
      IF keykodestring = "NumPad6" THEN keykodestring = "6".
      IF keykodestring = "NumPad7" THEN keykodestring = "7".
      IF keykodestring = "NumPad8" THEN keykodestring = "8".
      IF keykodestring = "NumPad9" THEN keykodestring = "9".
         
    /* Kopiera valda rader till klippbordet      */
      IF keykodestring = "c" AND e:Modifiers:ToString() = "Control" THEN DO:         
         KopieraRad().
         RETURN.
      END.
      /* Välja alla rader    http://blogs.infragistics.com/winforms/articles/select-all-rows-in-wingrid-programatically.aspx      */
      IF keykodestring = "a" AND e:Modifiers:ToString() = "Control" THEN DO:
         /* försök endast välja valda rader om gridden är multiselect */
         IF THIS-OBJECT:Guruegenskap:multiselect THEN THIS-OBJECT:Selected:Rows:AddRange(THIS-OBJECT:Rows:GetAllNonGroupByRows()).
         RETURN.                     
      END.  
     
      IF keykodestring = "TAB" AND e:Modifiers:ToString() = "Shift" THEN DO:
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ActivateCell).
         cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
         i = THIS-OBJECT:Guruegenskap:getRubrik(cellNamn):Ordning.
         lastshown = i.
         DO WHILE i > 0:
            IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap:getRubrik(i):Falt]:Hidden = FALSE THEN lastshown = i. 
            i = i - 1.
            IF i < 1 THEN DO:
               ingentraff = TRUE.
               LEAVE.
            END.    
            IF THIS-OBJECT:Guruegenskap:getRubrik(i):Readonly = FALSE THEN DO:
               IF i > 0 THEN THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap:getRubrik(lastshown):Falt).
               ingentraff = FALSE. 
               LEAVE.
            END.  
            ELSE ingentraff = TRUE.               
         END.
         IF ingentraff = TRUE THEN DO:
            THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap:getRubrik(1):Falt) NO-ERROR.           
            ingentraff = FALSE.
         END.   
        
         RETURN.
      END.
      ELSE IF keykodestring = "TAB" THEN DO:
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ActivateCell).
         cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
         i = THIS-OBJECT:Guruegenskap:getRubrik(cellNamn):Ordning.
         lastshown = i.
         DO WHILE i < THIS-OBJECT:Guruegenskap:countRubrik() - 1:
            IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap:getRubrik(i):Falt]:Hidden = FALSE THEN lastshown = i. 
            i = i + 1.
            IF i > THIS-OBJECT:Guruegenskap:countRubrik() - 1 THEN DO:
               ingentraff = TRUE. 
               LEAVE.
            END.            
            IF THIS-OBJECT:Guruegenskap:getRubrik(i):Readonly = FALSE THEN DO:
               IF i > 1 THEN THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap:getRubrik(lastshown):Falt).
                ingentraff = FALSE. 
               LEAVE.
            END.
            ELSE ingentraff = TRUE. 
               
         END.
         IF ingentraff = TRUE THEN DO:
            THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap:getRubrik(THIS-OBJECT:Guruegenskap:countRubrik( ) - 1):Falt) NO-ERROR.           
            ingentraff = FALSE.
         END.
         
           
         RETURN.
      END.                   
      IF keykodestring = "Down" THEN DO:
         IF cellNamn NE "" THEN DO:
            /*ccc
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:BelowCell, False, False).
            
            
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
            THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells[cellNamn].
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False).
            */
            
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
            THIS-OBJECT:GuruCellvaljare(cellNamn).  
            
         END.   
         RETURN.             
      END.
      IF keykodestring = "Up" THEN DO:
         IF cellNamn NE "" THEN DO:
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:PrevRow).
            THIS-OBJECT:GuruCellvaljare(cellNamn).
         END.   
         RETURN.   
      END.
      IF keykodestring = "Return" THEN DO:
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
         IF cellNamn NE "" THEN DO:
            THIS-OBJECT:GuruCellvaljare(cellNamn).
         END.
         RETURN.              
      END.
      
      IF sok = TRUE AND temprub:readonly = TRUE THEN DO:
         IF Helpers.Functions:IsSiffraBokstav(keykodestring) = TRUE THEN DO:
            IF ETIME - lastkeydownI <= 500 THEN lastkeydownC = lastkeydownC + keykodestring.
            ELSE lastkeydownC = keykodestring.
            THIS-OBJECT:GuruMark(INPUT temprub:FALT,"WHERE " + STRING(temprub:FALT) + " BEGINS '"  + STRING(lastkeydownC) + "'").
            lastkeydownI = ETIME.            
         END.   
      END.    
   
      RETURN.
   END METHOD.         
      
    
   METHOD PUBLIC VOID GuruOrderby():
      ttSortBy = "".
   END METHOD.
   
   METHOD PUBLIC VOID GuruOrderby(INPUT rub AS Controls.GridRubrik):      
      ttSortBy = " BY " + Guruegenskap:ttBufferHandle:TABLE + "." + rub:Falt.         
      IF THIS-OBJECT:Initierad THEN GuruFiltrera().   
   END METHOD.
   
   METHOD PUBLIC VOID GuruOrderby(INPUT str AS CHARACTER):      
      IF str NE "" THEN  ttsortby = " " + str.
/*         ttSortBy = " BY " + Guruegenskap:ttBufferHandle:TABLE + "." + str.*/
      ELSE  ttSortBy = "".   
      IF THIS-OBJECT:Initierad THEN GuruFiltrera().
   END METHOD.
   
   METHOD PUBLIC VOID GuruFiltrera():
      IF NOT VALID-HANDLE(Guruegenskap:ttBufferHandle) THEN RETURN.      
      ttSortQuery = "FOR EACH " + Guruegenskap:ttBufferHandle:TABLE + " WHERE " + Guruegenskap:filter + " " + ttSortBy.
      tqH:QUERY-PREPARE(ttSortQuery) NO-ERROR.
      tqH:QUERY-OPEN () NO-ERROR.
   END.
   
   /*Sätter om egenskap:filter samt öppnar om queryn*/
   METHOD PUBLIC VOID GuruFiltrera(filt AS CHARACTER):
      IF NOT VALID-HANDLE(Guruegenskap:ttBufferHandle) THEN RETURN.
      IF filt = ? THEN RETURN.
      THIS-OBJECT:Guruegenskap:filter = filt + " ".
      ttSortQuery = "FOR EACH " + Guruegenskap:ttBufferHandle:TABLE + " WHERE " + Guruegenskap:filter + " " + ttSortBy.
      tqH:QUERY-CLOSE. 
      tqH:QUERY-PREPARE(ttSortQuery).
      tqH:QUERY-OPEN ().    
   END METHOD.
   /*används av ButtonOver för att markerar rätt rader.
     Kräver att en default-buffer-handle till en table inputtas, denna table behöver innehålla:
     FIELD id as ROWID
   */
   METHOD PUBLIC VOID GuruRadvaljare (INPUT ridlist AS HANDLE):
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      DEFINE VARIABLE ridQ AS HANDLE NO-UNDO.
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      DEFINE VARIABLE ridh AS HANDLE NO-UNDO.
      CREATE QUERY ridQ.
      ridQ:SET-BUFFERS(ridlist).                        
      ridQ:QUERY-PREPARE ("for each " + ridlist:TABLE + " no-lock").
      ridQ:QUERY-OPEN ().
      ridQ:GET-FIRST(NO-LOCK).
      DO WHILE ridQ:QUERY-OFF-END = FALSE :
         rid = ridlist:BUFFER-FIELD("id"):BUFFER-VALUE.
         tqH:REPOSITION-TO-ROWID (rid) NO-ERROR.     
         pos = tBS:POSITION.
            /*MARKERAR FLERA*/
         
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
         
        
         ridQ:GET-NEXT(NO-LOCK).
      END.
     
   END METHOD.
   
   /* Markerar rader baserat på ROWID */
   METHOD PUBLIC VOID GuruRadvaljare(INPUT rid AS ROWID):
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      tBS:Refresh().
      IF rid NE ? THEN DO:
         tqH:REPOSITION-TO-ROWID (rid) .
         pos = tBS:POSITION.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
         THIS-OBJECT:ActiveRowScrollRegion:FirstRow = THIS-OBJECT:Rows[pos].
      END.
   END METHOD.
  
  
  
  METHOD PUBLIC VOID SortByColumn(INPUT sortvar AS CHARACTER, INPUT villkor AS CHARACTER):
      
      
   END METHOD.
  
  
  METHOD PUBLIC VOID GuruMark(INPUT sortvar AS CHARACTER, INPUT villkor AS CHARACTER):
      THIS-OBJECT:GuruAvmarkera().
      THIS-OBJECT:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR .
      IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap:ttBufferHandle:ROWID).          
      END.
      
   END METHOD.
 
   
   /* Markerar rader baserat på ROWID (FIRST ROW) */
   METHOD PUBLIC VOID GuruRadvaljare(INPUT rid AS ROWID, INPUT firstrow AS LOGICAL): 
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      tBS:Refresh().
      IF rid NE ? THEN DO:
         tqH:REPOSITION-TO-ROWID (rid) .
         pos = tBS:POSITION.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
          IF firstrow = TRUE THEN DO:
            THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:FirstRow = THIS-OBJECT:Rows[pos].
         END.
      END.
   END METHOD.
   
   /*Markerar rad första raden baserat på radvärde i kolumn, OBS LÅNGSAM*/
   METHOD PUBLIC VOID GuruRadValjare (radVarde AS CHARACTER, kolumnNamn AS CHARACTER):
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Rows}
         IF STRING(CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Cells[kolumnNamn]:VALUE ) = radVarde THEN DO:
            CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Selected = TRUE.
            CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Activate().
            THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:FirstRow = CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow).
            RETURN.
         END.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID GuruCellvaljare(INPUT kolumnNr AS INTEGER):
      DO ON ERROR UNDO, LEAVE:
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells[kolumnNr].
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False).         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID GuruCellvaljare(INPUT kolumnNamn AS CHARACTER):
     DO ON ERROR UNDO, LEAVE:
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells[kolumnNamn].
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False).         
      END.
   END METHOD.
   /*button over*/
   METHOD PUBLIC VOID GuruCellvaljare():
      DO ON ERROR UNDO, LEAVE:
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells["antal"].
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False).
      END.
   END METHOD.
   
        
   /*använd denna efter att ny post skapats i TT i huvudprogrammet*/
   METHOD PUBLIC VOID GuruRepositionTo(INPUT rid AS ROWID):
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      THIS-OBJECT:currentActiveRow:Selected = FALSE NO-ERROR.
      tqh:QUERY-OPEN ().            
      /* Om man inte kör EnterEditMode så repositioneras det inte om ingen rad är markerad sedan tidigare
         Dessutom blir tidigare selectad rad fortfarande selectad även om man har en rad markerad */      
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode).
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode).
      THIS-OBJECT:ActiveRow = ?.
      tqH:REPOSITION-TO-ROWID (rid) NO-ERROR.
      pos = tBS:POSITION.
      THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
      THIS-OBJECT:currentActiveRow:Selected = TRUE NO-ERROR.      
   END METHOD.
   
   METHOD PUBLIC INTEGER Posn():
      DEFINE VARIABLE pos AS INTEGER NO-UNDO. 
      /* VILKEN RAD STÅR JAG PÅ*/
      pos =  tBS:POSITION.
      RETURN pos.  
   END METHOD.
   
   METHOD PUBLIC VOID Nyrad():
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      Guruegenskap:ttBufferHandle:BUFFER-CREATE ().
      /*
      Guruegenskap:ttBufferHandle:BUFFER-FIELD("andrad"):BUFFER-VALUE = TRUE. /*ändrad fält krävs*/
      */
      Guruegenskap:ttBufferHandle:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = Guruegenskap:ttBufferHandle:RECID.
      rid = Guruegenskap:ttBufferHandle:ROWID.
      tqh:QUERY-OPEN ().
      GuruRepositionTo(rid).
   END METHOD.
   
  /* Denna metod anropar gridBeforeRowsDeleted eftersom den subscribeas i gridProperties() */
   METHOD PUBLIC VOID GuruBortrader():
      THIS-OBJECT:DeleteSelectedRows() .  
   END METHOD.
   
   /* Kör denna om rader ska bort utan fråga */
   METHOD PUBLIC VOID GuruBortrader(fraga AS LOGICAL):
      DEFINE VARIABLE canDeletedefault  AS LOGICAL NO-UNDO.
      canDeletedefault = canDelete.
      canDelete = TRUE.
      Gurudelfraga = fraga.
      GuruBortrader().
      Gurudelfraga = TRUE.
      canDelete = canDeletedefault.
   END METHOD.
   
   METHOD PUBLIC VOID GuruBortalla():
            /*
      Guruegenskap:ttBufferHandle:EMPTY-TEMP-TABLE () går ej om dataset.
      */
      tqH:GET-FIRST().   
      DO WHILE tqH:QUERY-OFF-END = FALSE:         
         THIS-OBJECT:Guruegenskap:ttBufferHandle:BUFFER-DELETE() NO-ERROR.                          
         tqH:GET-NEXT(NO-LOCK).
      END.
      GuruFiltrera(). 
   END METHOD.
  
   /* Returnerar ett Appearance beroende på FÄLT-text */
   METHOD PRIVATE Infragistics.Win.Appearance setApperance(falt AS CHARACTER):
      DEFINE VARIABLE appearanceText AS Infragistics.Win.Appearance NO-UNDO.
      appearanceText = NEW  Infragistics.Win.Appearance().
      IF falt = "AONR" OR falt = "DELNR" THEN appearanceText:ForeColor = System.Drawing.Color:Green.
      ELSE IF falt = "BERNR" THEN appearanceText:ForeColor = System.Drawing.Color:Purple.
      ELSE IF falt = "KALKNR" THEN appearanceText:ForeColor = System.Drawing.Color:Blue.
      ELSE IF falt = "PLANNR" THEN appearanceText:ForeColor = System.Drawing.Color:DarkRed.
      ELSE IF falt = "ARBKOD" OR falt = "LOPNR" THEN appearanceText:ForeColor = System.Drawing.Color:Red.
      ELSE DO:
         appearanceText:ForeColor = System.Drawing.Color:Black.
/*         appearanceText:BackColor = System.Drawing.Color:Red.*/
      END.
         
      RETURN appearanceText.
   END METHOD.
   
   /* Lite defaulta värden på designen */
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
      THIS-OBJECT:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.            
      THIS-OBJECT:DisplayLayout:GroupByBox:Hidden = FALSE. /* Sorterings-lådan, kräver att ViewStyleBand är OutlookGroupBy */
      THIS-OBJECT:DisplayLayout:GroupByBox:Prompt = "Dra en rubrik hit för att gruppera".      
      THIS-OBJECT:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:True. /*Griddens rubrik*/
      THIS-OBJECT:DisplayLayout:Override:AllowMultiCellOperations = Infragistics.Win.UltraWinGrid.AllowMultiCellOperation:Copy. /*krävs för att man ska kunna kopiera rader*/        
   END METHOD.
   
   METHOD PRIVATE VOID BindSCreateRow(rSender AS System.Object, rArgs AS Progress.Data.CreateRowEventArgs):
      /*DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.*/
      rArgs:Created = FALSE.
      Guruegenskap:ttBufferHandle:BUFFER-CREATE ().
      /*tqh:QUERY-OPEN ().*/
      DO ON ERROR undo, LEAVE:
         tqH:CREATE-RESULT-LIST-ENTRY ().      
         rArgs:Created = TRUE.                           
      END.

   END METHOD.

   /* cancel på create-row*/
   METHOD PRIVATE VOID BindSCancelCreateRow(rSender AS System.Object, rArgs AS Progress.Data.CancelCreateRowEventArgs).
      tqH:GET-CURRENT(EXCLUSIVE-LOCK).    
      THIS-OBJECT:Guruegenskap:ttBufferHandle:buffer-delete(). /*behöver man göra mer?*/   
   END METHOD.

   METHOD PRIVATE VOID gridBeforeRowsDeleted(rSender AS System.Object, rArgs AS Infragistics.Win.UltraWinGrid.BeforeRowsDeletedEventArgs):
      DEFINE VARIABLE cPromptText AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lConfirmDelete AS LOGICAL NO-UNDO.
      DEFINE VARIABLE ttrr AS RECID NO-UNDO.
      DEFINE VARIABLE SetOnRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE LastSelectedRow  AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE FirstSelectedRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE found   AS LOGICAL INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE running AS LOGICAL INITIAL TRUE NO-UNDO.
      DEFINE VARIABLE iterator AS INTEGER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF canDelete = FALSE THEN DO:
         rArgs:Cancel = TRUE.
         RETURN.
      END.
      rArgs:DisplayPromptMsg = FALSE.
      IF Gurudelfraga = TRUE THEN DO:
         IF rArgs:Rows:Length > 1 THEN  cPromptText = Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(42) + " " + STRING(rArgs:Rows:Length).
         ELSE cPromptText = Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(42).      
         rrr = System.Windows.Forms.MessageBox:Show(cPromptText,"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF rrr:ToString() = "Yes" THEN lConfirmDelete = TRUE.
      END.
      ELSE lConfirmDelete = TRUE.
      IF NOT lConfirmDelete THEN rArgs:Cancel = TRUE.
      ELSE DO:
         IF rArgs:Rows:Length > 0 THEN DO:
            tBS:AutoSync = FALSE.
            /* Sätt LastSelectedRow till den faktiska sista valda raden */
            LastSelectedRow = CAST(rArgs:Rows:GetValue(rArgs:Rows:Length - 1),Infragistics.Win.UltraWinGrid.UltraGridRow).
            FirstSelectedRow = CAST(rArgs:Rows:GetValue(0),Infragistics.Win.UltraWinGrid.UltraGridRow).
            /* Har man valt i omvänd ordning, vänd på steken*/
            IF LastSelectedRow:INDEX < CAST(rArgs:Rows:GetValue(0),Infragistics.Win.UltraWinGrid.UltraGridRow):INDEX THEN DO: 
               LastSelectedRow = CAST(rArgs:Rows:GetValue(0),Infragistics.Win.UltraWinGrid.UltraGridRow).
               FirstSelectedRow =  CAST(rArgs:Rows:GetValue(rArgs:Rows:Length - 1),Infragistics.Win.UltraWinGrid.UltraGridRow).
            END.
            LastSelectedRow:Activate().
            IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE  THEN DO:
               ttrr = Guruegenskap:ttBufferHandle:RECID.
            END.   
            SetOnRow = THIS-OBJECT:ActiveRow.
            /* Kolla om någon är vald mitt i, dvs alla markerade poster inte är samlade i en klunga, isf repositonera till den sen! 
            iterator = SISTA RADEN SOM ÄR MARKERAD EV FÖRSTA (visar alltid en för lite)  alla var valda
            ttrr = recid till den som ska markeras efter
            running = true har inte hittat någon och har inte gått igenom alla.
            SetOnRow = aktiv rad.
            rArgs:Rows:Length = ANTAL MARKERADE
            THIS-OBJECT:Rows:Count ANTAL ETT POSTER
            */
            iterator = LastSelectedRow:INDEX.
            IF rArgs:Rows:Length = THIS-OBJECT:Rows:Count THEN DO:
               ttrr = ?.
               GuruBortalla().
            END.
            ELSE IF LastSelectedRow:INDEX >= 0 THEN DO:
               DO WHILE running EQ TRUE:
                  IF iterator = FirstSelectedRow:INDEX OR iterator < 0 THEN running = FALSE. 
                  IF THIS-OBJECT:Rows[iterator]:Selected EQ FALSE THEN DO:
                     SetOnRow = THIS-OBJECT:Rows[iterator].
                     ttrr = INTEGER(STRING(THIS-OBJECT:Rows[iterator]:Cells["TTRECID"]:Value)) NO-ERROR.
                     IF ttrr = ? THEN found = FALSE.
                     ELSE found = TRUE.
                     running = FALSE.
                  END.
                  iterator = iterator - 1.
               END.
                /* finns det rader kvar välj den som är nedan för*/
               IF LastSelectedRow:INDEX < THIS-OBJECT:Rows:Count - 1 THEN DO:
                  ttrr = integer( string( THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ]:Cells["TTRECID"]:Value ) )NO-ERROR.
                  SetOnRow =  THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ].
               END.  
                /* finns det rader kvar välj den som är ovan för*/
               ELSE IF FirstSelectedRow:INDEX > 0 THEN DO:
                  ttrr = integer( string( THIS-OBJECT:Rows[FirstSelectedRow:Index - 1 ]:Cells["TTRECID"]:Value ) ) NO-ERROR.
                  SetOnRow =  THIS-OBJECT:Rows[FirstSelectedRow:Index - 1 ].
               END.  
               ELSE DO:
                  /*SÖK INGEN OM DET ITE FINNS NÅGON*/
                  IF LastSelectedRow:INDEX = 0 THEN ttrr = ?.
                  ELSE DO:
                     ttrr = integer( string( THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ]:Cells["TTRECID"]:Value ) ) NO-ERROR.               
                     SetOnRow =  THIS-OBJECT:Rows[LastSelectedRow:Index + 1].
                  END.
               END.
               /*deleta dom valda*/
              {WALLMAN\foreachultra.i System.Object oObject in rArgs:Rows RSR}
                 THIS-OBJECT:DeleteRow( CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow ) ) NO-ERROR.
               END.
            END.
            ELSE DO:
               MESSAGE "va fan"
               VIEW-AS ALERT-BOX.
            END.   
            tqH:Query-Open().
           /*sync? Avmarkering?*/
            /*SÖK INGEN OM DET ITE FINNS NÅGON*/
            IF ttrr NE ? THEN DO:
               Guruegenskap:ttBufferHandle:FIND-FIRST("WHERE TTRECID  = " + string(ttrr)) NO-ERROR.
               SetOnRow:Activate().
            END.
            /*
            IF iterator = -1 THEN DO:
               ttrr = ?.
               GuruBortalla().
            END.
               
            ELSE DO: 
               /* finns det rader kvar välj den som är nedan för*/
               IF LastSelectedRow:INDEX < THIS-OBJECT:Rows:Count - 1 THEN DO:
                  ttrr = integer( string( THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ]:Cells["TTRECID"]:Value ) )NO-ERROR.
                  SetOnRow =  THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ].
               END.  
                /* finns det rader kvar välj den som är ovan för*/
               ELSE IF FirstSelectedRow:INDEX > 0 THEN DO:
                  ttrr = integer( string( THIS-OBJECT:Rows[FirstSelectedRow:Index - 1 ]:Cells["TTRECID"]:Value ) ) NO-ERROR.
                  SetOnRow =  THIS-OBJECT:Rows[FirstSelectedRow:Index - 1 ].
               END.  
               ELSE DO:
                  /*SÖK INGEN OM DET ITE FINNS NÅGON*/
                  IF LastSelectedRow:INDEX = 0 THEN ttrr = ?.
                  ELSE DO:
                     ttrr = integer( string( THIS-OBJECT:Rows[LastSelectedRow:Index + 1 ]:Cells["TTRECID"]:Value ) ) NO-ERROR.               
                     SetOnRow =  THIS-OBJECT:Rows[LastSelectedRow:Index + 1].
                  END.
               END.
               /*deleta dom valda*/
              {WALLMAN\foreachultra.i System.Object oObject in rArgs:Rows RSR}
                 THIS-OBJECT:DeleteRow( CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow ) ) NO-ERROR.
               END.
            END.
                        
            tqH:Query-Open().
           /*sync? Avmarkering?*/
            /*SÖK INGEN OM DET ITE FINNS NÅGON*/
            IF ttrr NE ? THEN DO:
               Guruegenskap:ttBufferHandle:FIND-FIRST("WHERE TTRECID  = " + string(ttrr)) NO-ERROR.
               SetOnRow:Activate().
            END.
            */   
         END.
      END.
      THIS-OBJECT:tBS:AutoSync = TRUE.
   END METHOD.
  
   METHOD PRIVATE VOID GridAfterRowsDeleted(rSender AS System.Object, rArgs AS System.EventArgs): 
      THIS-OBJECT:currentActiveRow:Selected = TRUE NO-ERROR.
      THIS-OBJECT:currentActiveRow:Activated = TRUE NO-ERROR.
      
   END METHOD.
   
   /*Lägger till val i combobox om det finns för rubriken*/
   METHOD PRIVATE VOID GuruCombocell(rub AS Controls.Gridrubrik):
      DEFINE VARIABLE list AS Infragistics.Win.ValueList NO-UNDO.
      IF rub:comboList:ValueListItems:Count > 0 AND rub:Readonly = FALSE THEN DO:
/*         rub:comboList:Appearance = THIS-OBJECT:DisplayLayout:Bands[0]:Columns[rub:Falt]:ValueList.*/
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[rub:Falt]:ValueList = rub:comboList NO-ERROR.
      END.   
   END METHOD.
   
   METHOD PRIVATE VOID gridProperties():
      /* Add new record row at the bottom of each level in the grid. AllowAddNew
   is an UltraWinGrid ennumeration. */           
      /* Subscribe to binding source events */
      THIS-OBJECT:tBS:CreateRow:Subscribe(BindSCreateRow).
      THIS-OBJECT:tBS:CancelCreateRow:Subscribe(BindSCancelCreateRow).
      /* Subscribe to grid control events */
      THIS-OBJECT:BeforeRowsDeleted:Subscribe(gridBeforeRowsDeleted).
      THIS-OBJECT:AfterRowsDeleted:Subscribe(gridAfterRowsDeleted).      
/*      THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Default.*/      
      /*om man trycker på en tangent*/
      THIS-OBJECT:KeyDown:Subscribe(THIS-OBJECT:KeyDown). /*keydown är bäst :)*/          
      /* Synca vald rad i Grid och TT */
      THIS-OBJECT:tBS:PositionChanged:Subscribe(Valuechanged).
      THIS-OBJECT:tBS:SortRequest:Subscribe(SortReq).
      /*om man klickar*/
      THIS-OBJECT:MouseClick:Subscribe(musklick).
      THIS-OBJECT:MouseDown:Subscribe(musklick).
      /*exportera rader till excel subsriben låg här före 14/6/13 robin*/
      /* repos till markerad rad vid sort*/
      THIS-OBJECT:AfterSortChange:Subscribe(THIS-OBJECT:hittaRad).
      /*R*/
      THIS-OBJECT:AfterSortChange:Subscribe(THIS-OBJECT:GridAfterSortChange).
   END METHOD.
   METHOD PRIVATE VOID SortReq (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      THIS-OBJECT:SortReq ().
          
   END METHOD.
   METHOD PRIVATE VOID SortReq():
      
   END METHOD.
   /*hitta åt den markerade raden i grid efter att användaren sorterat*/
   METHOD PRIVATE VOID hittaRad (sender AS System.Object, e AS System.EventArgs):
      /*no error annars felmed vid gruppering*/
      THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:ScrollRowIntoView(THIS-OBJECT:ActiveRow) NO-ERROR.
   END METHOD.
   
   /*Döljer rader som inte är markerade*/
   METHOD PUBLIC VOID GridAfterSortChange(INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BandEventArgs):
      ActiveValCol = string(e:Band:SortedColumns[0]).
      ttSortBy = SortExpression(e:Band).
      IF NOT VALID-HANDLE(Guruegenskap:ttBufferHandle) THEN RETURN.      
      ttSortQuery = "FOR EACH " + Guruegenskap:ttBufferHandle:TABLE + " WHERE " + Guruegenskap:filter + ttSortBy.  
      tqH:QUERY-PREPARE(ttSortQuery) NO-ERROR.
      tqH:QUERY-OPEN () NO-ERROR.
     /*Robin*/
   END METHOD.
   
   /*Bygger en sorteringsträng*/
   METHOD STATIC PUBLIC CHARACTER SortExpression (band AS Infragistics.Win.UltraWinGrid.UltraGridBand):
      DEFINE VARIABLE sortColumn AS     Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE sortString AS CHARACTER NO-UNDO.
      DO i = 0 TO band:SortedColumns:Count - 1:
         sortColumn = cast(band:SortedColumns[i],Infragistics.Win.UltraWinGrid.UltraGridColumn).
         sortString = sortString + " BY " + sortColumn:Key.
         IF Progress.Util.EnumHelper:AreEqual(sortColumn:SortIndicator, Infragistics.Win.UltraWinGrid.SortIndicator:DESCENDING) THEN
         sortString = sortString + " DESCENDING ".      
      END.
      RETURN left-trim(sortString). 
   END METHOD.
   
   /*   http://blogs.infragistics.com/forums/p/53971/279129.aspx*/
   METHOD PRIVATE VOID musklick(sender AS System.Object, e AS System.Windows.Forms.MouseEventArgs):         
      DEFINE VARIABLE point AS System.Drawing.Point NO-UNDO.      
      /* hantera högerklick borde funka... Fungerar efter 13/6/13 men används inte till något /robin  */                     
      IF e:Button:ToString() EQ "Right" THEN DO:
         IF THIS-OBJECT:SELECTED:Rows:Count NE 0 THEN DO:     
         END.   
      END.                  
   END METHOD.
   
   
   
   METHOD PRIVATE VOID Valuechanged (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      THIS-OBJECT:Valuechanged ().
          
   END METHOD.
   /*ANDERS*/
   METHOD PUBLIC VOID Valuechanged ():
      Guruegenskap:ttBufferHandle:SYNCHRONIZE ().
      /*THIS-OBJECT:SyncMan ().   */         
   END METHOD. 
   
   METHOD PUBLIC VOID SyncMan ():       
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:ActiveRow = ? THEN RETURN.
      IF THIS-OBJECT:ActiveRow:Cells["TTRECID"]:VALUE = ? THEN DO:
         RETURN.
      END.   
      villkor = "WHERE ".
      villkor = villkor + "TTRECID = " + STRING(THIS-OBJECT:ActiveRow:Cells["TTRECID"]:VALUE).
      Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
       
   END METHOD.      
   
   
   METHOD PRIVATE LOGICAL ProcessRowChanges():
      DEFINE VARIABLE hBeforeBuffer AS HANDLE NO-UNDO.
      DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
      hBeforeBuffer = Guruegenskap:ttBufferHandle:BEFORE-BUFFER.
      lResult = hBeforeBuffer:SAVE-ROW-CHANGES() NO-ERROR.
      IF NOT lResult THEN DO:
         hBeforeBuffer:REJECT-ROW-CHANGES ().
         RETURN FALSE.      
      END.
      IF NOT hBeforeBuffer:ACCEPT-ROW-CHANGES () THEN RETURN FALSE.
      RETURN TRUE.
      
   END METHOD.
   
   METHOD PRIVATE INTEGER DeleteRow (rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow):
      DEFINE VARIABLE rCells AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
      DEFINE VARIABLE rCell0 AS Infragistics.Win.UltraWinGrid.UltraGridCell NO-UNDO.
      DEFINE VARIABLE rCell1 AS Infragistics.Win.UltraWinGrid.UltraGridCell NO-UNDO.
      DEFINE VARIABLE iNum AS INTEGER NO-UNDO.
      DEFINE VARIABLE iLuneNum AS INTEGER NO-UNDO.
      DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
      DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      DEFINE VARIABLE trub AS Controls.GridRubrik.
      DEFINE VARIABLE tempCell AS Infragistics.Win.UltraWinGrid.UltraGridCell NO-UNDO.
      DEFINE VARIABLE och AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ttr AS CHARACTER NO-UNDO.
      villkor = "WHERE ".
      num = 0.      
      rCells = rCurrentRow:Cells.
      och = "".     
      ttr = rCells["TTRECID"]:VALUE.
      villkor = villkor + "TTRECID = " + ttr.   
      Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      /*om Delete är definierat, kopiera borttagen rad till del.*/
      IF ttBufferHandleDel NE ? THEN DO:
         Guruegenskap:ttBufferHandle:BUFFER-COPY (ttBufferHandleDel).
      END.
      IF Guruegenskap:ttBufferHandle:AVAILABLE THEN Guruegenskap:ttBufferHandle:BUFFER-DELETE () NO-ERROR.
     
   END METHOD.
   
 
   
   /*
      1. Deleta Delete-poster (ROWID eller egna villkor)
      2. Spara Nya poster (ändrad true) -> find first -> if not available create->buffer-copy
      3. Uppdatera ändrade poster (ändrad-fält i tt) -> find first -> if available buffer-copy      
   */
   METHOD PUBLIC VOID DBspar():
      
   
   END METHOD.
   
   /* Rensar Delete-buffern */
   METHOD PUBLIC VOID RensaDel():
      ttBufferHandleDel:EMPTY-TEMP-TABLE ().      
   END METHOD.
   
END CLASS.