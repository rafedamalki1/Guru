 
 /*------------------------------------------------------------------------
    File        : GridJoin
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Fri Feb 11 10:47:12 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Microsoft.Office.Interop.Excel.*.

CLASS Controls.GridJoin INHERITS Infragistics.Win.UltraWinGrid.UltraGrid:
   
   
      /* Funktioner för att spara markerade rader till OldRows med GuruSaveOldRows, samt selecta dom nya med GuruSelectNew. */
   DEFINE VARIABLE antalGridBands AS INTEGER NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC STATIC PROPERTY lastkeydownI     AS INTEGER NO-UNDO  
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY lastkeydownC     AS CHARACTER NO-UNDO  
      PUBLIC GET.    PUBLIC SET.   
   DEFINE VARIABLE fargvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE valdgridrowid AS ROWID NO-UNDO.
   DEFINE PUBLIC VARIABLE DropedFile AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE oldRows AS System.Collections.ArrayList NO-UNDO.
   DEFINE PRIVATE VARIABLE hasRows AS LOGICAL.
   DEFINE PUBLIC VARIABLE canDelete AS LOGICAL INITIAL FALSE.
   /* För färgsättning */
   
   DEFINE PUBLIC VARIABLE GuruRadOption AS Controls.GridRadOptions NO-UNDO.
   
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
   
   /*Hjälp-TT för att SortBandColl()*/   
   DEFINE TEMP-TABLE ttSort NO-UNDO
      FIELD namn AS CHARACTER
      FIELD nyckel AS CHARACTER
      FIELD nummer AS INTEGER
      INDEX namn1 IS PRIMARY namn.
            
/* Använd denna property för att spara undan deletade poster i en egen tabell
   Kör RensaDel() för att rensa buffern.*/      
   DEFINE PROPERTY ttBufferHandleDel AS HANDLE NO-UNDO
      PUBLIC GET.      PUBLIC SET.                     

   DEFINE PROPERTY Guruegenskap AS Controls.GridRubrikListaJoin NO-UNDO {GridRubrikListaExtent.i}
      PUBLIC GET.   PRIVATE SET.
   
   DEFINE PRIVATE PROPERTY Gurudelfraga AS LOGICAL INITIAL TRUE NO-UNDO
   PRIVATE GET.   PRIVATE SET.
   
   DEFINE PRIVATE VARIABLE excelExporter AS Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter NO-UNDO.
   DEFINE PUBLIC  VARIABLE ExtraToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItemAh AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItemES AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItemON AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaViaInternetMenuItemSE AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   
   DEFINE PRIVATE VARIABLE excelToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE excelMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE skrivutMarkToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE kopieraToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE AterstallSortMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   
   DEFINE PRIVATE VARIABLE excelApp AS Application NO-UNDO.
   DEFINE PRIVATE VARIABLE excelApp2 AS Application NO-UNDO.
   
   
   DEFINE VARIABLE iex AS INTEGER  NO-UNDO. /*GuruTillExcel() räknare*/
   
   DEFINE PUBLIC PROPERTY AntalRubriker AS INTEGER NO-UNDO /*funkar bara om man kör NyRubrik*/ 
   PUBLIC GET.   PUBLIC SET.
   DEFINE PUBLIC PROPERTY workingband AS INTEGER NO-UNDO /*funkar bara om man kör NyRubrik*/ 
   PUBLIC GET.   PUBLIC SET.
   METHOD PUBLIC INTEGER NyRubrik(wb AS INTEGER ):
      IF workingband NE wb THEN DO:
         workingband = wb.
         AntalRubriker = 0.
      END.   
      AntalRubriker = AntalRubriker + 1.
      RETURN AntalRubriker.
   END METHOD.
   
   METHOD PUBLIC VOID AddDragDropGroup(g AS Controls.Subclasses.DragDropGroup):
      THIS-OBJECT:DragDropGroups:Add(g).
   END METHOD.
      
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_AfterEnterEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   
	   IF THIS-OBJECT:ActiveCell = ? THEN RETURN. 
		THIS-OBJECT:ActiveCell:Activate().
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_CellDataError( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellDataErrorEventArgs ):
		System.Windows.Forms.MessageBox:Show("Infragistics fel: ").
		RETURN.

	END METHOD.

   @VisualDesigner.
   METHOD PRIVATE VOID Grid_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE mDragger AS Controls.Grid NO-UNDO.
      DEFINE VARIABLE Found    AS LOGICAL       INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE args AS Controls.Subclasses.DragDropArgs NO-UNDO.
      IF THIS-OBJECT:GuruDroppable = TRUE THEN DO:
         {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DragDropGroups}
            IF CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger NE ? THEN DO:
               mDragger = CAST(CAST(oObject, Controls.Subclasses.DragDropGroup):Dragger, Controls.Grid).
               Found = TRUE.
            END.
         END.
         IF Found = TRUE THEN DO:
            IF mDragger NE THIS-OBJECT THEN DO:
               
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
   
   METHOD PUBLIC VOID Grid_DragDropFile( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE objFiles AS "System.String[]" NO-UNDO.  
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.    
      
      DEFINE VARIABLE theStream AS System.IO.MemoryStream NO-UNDO.
      DEFINE VARIABLE fileGroupDescriptor AS "System.Byte[]" NO-UNDO. 
      DEFINE VARIABLE filNamn AS System.Text.StringBuilder NO-UNDO.
      DEFINE VARIABLE intbyte AS INTEGER NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE langd AS INTEGER NO-UNDO.
      DEFINE VARIABLE args AS Controls.Subclasses.DragDropArgs NO-UNDO.
      /* GENERELL DRAG AND DROP KOD*/         
                                                                                                                                                                                                                                     
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
         objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").
         DropedFile = objFiles:GetValue(0):ToString().
      END. 
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         i = 0.
         theStream = CAST(e:Data:GetData("FileGroupDescriptor"),"System.IO.MemoryStream").
         fileGroupDescriptor = NEW "System.Byte[]" (512).   
         langd = theStream:Length - 1.

         DO WHILE i < langd:
             intbyte = theStream:ReadByte().
             fileGroupDescriptor:SetValue(intbyte AS UNSIGNED-BYTE, i).
             i = i + 1.
         END.                 

         i = 76.
         filNamn = NEW System.Text.StringBuilder().

         DO WHILE i < langd:
            filNamn:Append(System.Convert:ToChar(fileGroupDescriptor:GetValue(i))).
            i = i + 1.
         END. 
         DropedFile = System.IO.Path:GetTempPath() + filNamn:ToString().
      END.
      THIS-OBJECT:GuruAfterDropped:Publish(THIS-OBJECT, CAST(args, System.EventArgs)).
   END METHOD.
   METHOD PUBLIC VOID Grid_DragEnter(INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      /*Drag and drop*/
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
        e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE DO:
         e:Effect = System.Windows.Forms.DragDropEffects:NONE.   
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
            CAST(oObject, Controls.Subclasses.DragDropGroup):DraggerJoin = THIS-OBJECT.
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
   
   METHOD PUBLIC VOID GuruUpdateTitle(INPUT bandval AS INTEGER):
      IF THIS-OBJECT:Guruegenskap[bandval]:NoAutoTitle = TRUE THEN RETURN.
      IF THIS-OBJECT:TEXT EQ ? THEN RETURN.
      IF THIS-OBJECT:TEXT EQ "" THEN RETURN.
      IF THIS-OBJECT:tBS EQ ? THEN RETURN.
      
      THIS-OBJECT:GuruReopening = TRUE.
      IF THIS-OBJECT:GuruShowRowsCount EQ TRUE THEN DO:
         THIS-OBJECT:TEXT = THIS-OBJECT:TITLE + " - " + "Antal poster - " + STRING(THIS-OBJECT:tBS:Count).
      END.
      ELSE DO:
         THIS-OBJECT:TEXT = THIS-OBJECT:TITLE.
      END.
      THIS-OBJECT:GuruReopening = FALSE.
   END METHOD.
    METHOD PUBLIC VOID GuruUpdateTitle(INPUT bandval AS INTEGER,INPUT titlevar AS CHARACTER):
      
      IF THIS-OBJECT:Guruegenskap[bandval]:NoAutoTitle = FALSE THEN RETURN.
      IF THIS-OBJECT:TEXT EQ ? THEN RETURN.
      IF THIS-OBJECT:TEXT EQ "" THEN RETURN.
      IF THIS-OBJECT:tBS EQ ? THEN RETURN.
      THIS-OBJECT:GuruReopening = TRUE.
      THIS-OBJECT:TEXT = THIS-OBJECT:TITLE + " " + titlevar.
      THIS-OBJECT:GuruReopening = FALSE.
   END METHOD.
   METHOD PUBLIC VOID GuruNoAutoTitle(INPUT bandval AS INTEGER,INPUT NoAutoTitle AS LOGICAL):
      THIS-OBJECT:GuruReopening = TRUE.
      THIS-OBJECT:Guruegenskap[bandval]:NoAutoTitle = NoAutoTitle.    
   END METHOD.
   
   
   METHOD PUBLIC VOID GuruUpdateTitleE(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE bandval AS INTEGER.
      bandval = 1.
      THIS-OBJECT:GuruUpdateTitle(INPUT bandval).
   END METHOD.
   METHOD PUBLIC VOID GuruHidden(INPUT bandval AS INTEGER, faltnamn AS CHARACTER, gomma AS LOGICAL ):
     
      THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandval]:Bands]:Columns[faltnamn]:Hidden = gomma.
     
   END METHOD.
   
   METHOD PUBLIC VOID GuruHiddenBand(INPUT bandval AS INTEGER, gomma AS LOGICAL ):
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Hidden = gomma.
   END METHOD.
   
   METHOD PUBLIC VOID GuruTextOrient(INPUT bandval AS INTEGER, INPUT val AS CHARACTER):
     IF val = "45G" THEN DO:
        THIS-OBJECT:DisplayLayout:Bands[bandval]:Override:ColumnHeaderTextOrientation = Infragistics.Win.TextOrientationInfo:Horizontal45Degrees.
     END.
     ELSE IF val = "Vertical" THEN DO: 
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Override:ColumnHeaderTextOrientation = Infragistics.Win.TextOrientationInfo:Vertical.
     END.
   END METHOD.
   
   METHOD PUBLIC VOID GuruExpand(val AS LOGICAL):
      /* {WALLMAN/foreach.i System.Object oObject in THIS-OBJECT:DisplayLayout:Bands}           
         CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridBand):SortedColumns:Clear().
       END.
       THIS-OBJECT:Rows:Refresh(Infragistics.Win.UltraWinGrid.RefreshRow:ReloadData).*/
       THIS-OBJECT:Rows:ExpandAll(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID GuruAddColumn(INPUT bandval AS INTEGER, temprubadd AS Controls.GridRubrik):
      DEFINE VARIABLE rubfalt AS CHARACTER NO-UNDO. 
      rubfalt = temprubadd:Falt.
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:HEADER:VisiblePosition = temprubadd:Ordning.
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:HEADER:Caption = temprubadd:Rubrik.
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Hidden = temprubadd:Gomma.         
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:Width = temprubadd:Bredd.

      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellAppearance = setApperance(rubfalt).
      
      /*Anger om det ska vara rowselectors (puckar), default = false, men sätter till true om det finns editerbara fält*/
      IF temprubadd:Readonly = FALSE THEN DO:            
         IF THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() = "False" THEN DO:            
            THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
         END.
         THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellAppearance:BackColor = System.Drawing.ColorTranslator:FromHtml("#E5F3F9").
      END. 
      
      IF temprubadd:Typvar = "TTRECID" THEN DO:
         /*Ändra för att visa TTRECID-kollumen*/
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns["TTRECID"]:Hidden = TRUE.
         ELSE THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns["TTRECID"]:Hidden = TRUE .
      END.
     
      {FORMATIGRIDS.I}
      IF temprubadd:Readonly = TRUE THEN DO:
         THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
         THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect. /* FFFFFFFFFFFFF */
      END.
      ELSE DO:
         THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
         THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
      END.
     
   END METHOD.
   
   /*FÖR ATT SÄTTA FONT FARG MM*/
   METHOD PUBLIC VOID GuruGridRadOption(INPUT paav AS LOGICAL):
      IF paav = TRUE THEN THIS-OBJECT:FilterRow:Subscribe(THIS-OBJECT:Grid_FilterRowOption).
   END METHOD.
   METHOD PUBLIC VOID GuruAddRadOption(INPUT cellname AS CHARACTER,INPUT cellval AS CHARACTER,INPUT cellcol AS System.Drawing.Color,INPUT fontvar AS System.Drawing.Font,INPUT WholeRow AS LOGICAL,INPUT faltlista AS CHARACTER,INPUT funktion AS INTEGER,INPUT jmfhbem AS INTEGER):
      IF THIS-OBJECT:GuruRadOption EQ ? THEN DO:
         THIS-OBJECT:GuruRadOption = NEW Controls.GridRadOptions().
      END.
      THIS-OBJECT:GuruRadOption:CellName = cellname.     /*Vilken cell ska jmf*/
      THIS-OBJECT:GuruRadOption:CellCondition =  cellval. /*värde*/ 
      THIS-OBJECT:GuruRadOption:COLOR = cellcol.   /*färg*/      
      THIS-OBJECT:GuruRadOption:CellFont = fontvar.   /*font*/
      THIS-OBJECT:GuruRadOption:WholeRow =  WholeRow.  /*hela raden*/
      THIS-OBJECT:GuruRadOption:CellNameColor = faltlista.   /*  lista på fäly "a,b,c," ska avslutas med , */
      THIS-OBJECT:GuruRadOption:CellFunc = funktion.   /* 0 = bg  1 = fg 2 = font*/
      THIS-OBJECT:GuruRadOption:ConditionType = jmfhbem. 
   END METHOD.
   
    
   METHOD PUBLIC VOID GuruSetColumnBgColor(INPUT bandval AS INTEGER,INPUT fx AS CHARACTER, INPUT cx AS System.Drawing.Color):
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[fx]:CellAppearance:BackColor = cx.
   END METHOD.
   METHOD PUBLIC VOID GuruSetColumnFgColor(INPUT bandval AS INTEGER, INPUT fx AS CHARACTER, INPUT cx AS System.Drawing.Color):
      THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[fx]:CellAppearance:ForeColor = cx.
   END METHOD.
    
   
    /*FÖR ATT SÄTTA FONT FARG MM*/
   METHOD PRIVATE VOID Grid_FilterRowOption( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.FilterRowEventArgs ):
      DEFINE VARIABLE meh AS INTEGER INITIAL 0.
      DEFINE VARIABLE cellnamelista AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cellnamecolorvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bandin AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO. 

      IF THIS-OBJECT:GuruRadOption EQ ? THEN RETURN.
      e:ROW:Appearance:ResetBackColor().
      e:ROW:Appearance:ResetForeColor().
      DO WHILE meh LE e:ROW:Cells:Count - 1:
         e:ROW:Cells[meh]:Appearance:ResetBackColor().
         e:ROW:Cells[meh]:Appearance:ResetForeColor().
         meh = meh + 1.
      END.
      IF THIS-OBJECT:GuruRadOption:CellName = " " THEN DO:
         RETURN.
      END.   
      IF e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellName]:value = ? THEN DO:
         RETURN.
      END.
      /*bakgrundsfärg*/
      IF THIS-OBJECT:GuruRadOption:CellFunc = 0 THEN DO:
         IF THIS-OBJECT:GuruRadOption:CellCondition =  STRING(e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellName]:Value)  THEN DO:
            IF THIS-OBJECT:GuruRadOption:WholeRow EQ TRUE THEN DO:
               e:ROW:Appearance:BackColor = THIS-OBJECT:GuruRadOption:COLOR.                
            END.
            ELSE DO:
               cellnamelista  = THIS-OBJECT:GuruRadOption:CellNameColor.
               IF INDEX(cellnamelista,",") = 0 THEN e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellNameColor]:Appearance:BackColor = THIS-OBJECT:GuruRadOption:COLOR.
               ELSE DO:
                  REPEAT: 
                     IF INDEX(cellnamelista,",",1) NE 0 THEN DO:
                        cellnamecolorvar = SUBSTRING(cellnamelista,1,INDEX(cellnamelista,",",1) - 1).
                        e:ROW:Cells[cellnamecolorvar]:Appearance:BackColor = THIS-OBJECT:GuruRadOption:COLOR.
                        cellnamelista = SUBSTRING(cellnamelista,INDEX(cellnamelista,",",1) + 1).
                     END.
                     ELSE   LEAVE.
                  END.
               END.   
            END.
         END.
      END.
      /*färg på text*/
      IF THIS-OBJECT:GuruRadOption:CellFunc = 1 THEN DO:
         IF THIS-OBJECT:GuruRadOption:CellCondition =  STRING(e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellName]:Value)  THEN DO:
            IF THIS-OBJECT:GuruRadOption:WholeRow EQ TRUE THEN DO:
               meh = 0.
               DO WHILE meh LE e:ROW:Cells:Count - 1:
                  e:ROW:Cells[meh]:Appearance:ForeColor = THIS-OBJECT:GuruRadOption:COLOR.
                  meh = meh + 1.
               END.
            END.
            ELSE DO:
               cellnamelista  = THIS-OBJECT:GuruRadOption:CellNameColor.
               IF INDEX(cellnamelista,",") = 0 THEN e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellNameColor]:Appearance:ForeColor = THIS-OBJECT:GuruRadOption:COLOR.
               ELSE DO:
                  REPEAT: 
                     IF INDEX(cellnamelista,",",1) NE 0 THEN DO:
                        cellnamecolorvar = SUBSTRING(cellnamelista,1,INDEX(cellnamelista,",",1) - 1).
                        e:ROW:Cells[cellnamecolorvar]:Appearance:ForeColor = THIS-OBJECT:GuruRadOption:COLOR.
                        cellnamelista = SUBSTRING(cellnamelista,INDEX(cellnamelista,",",1) + 1).
                     END.
                     ELSE   LEAVE.
                  END.
               END.   
            END.
         END.
      END.
      /*font*/
      
      IF THIS-OBJECT:GuruRadOption:CellFunc = 2 THEN DO:
         
         IF THIS-OBJECT:GuruRadOption:CellCondition =  STRING(e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellName]:Value)  THEN DO:
            IF THIS-OBJECT:GuruRadOption:WholeRow EQ TRUE THEN DO:
               e:ROW:Appearance:FontData:Name = THIS-OBJECT:GuruRadOption:CellFont:Name.
               
            END.
            ELSE DO:
               cellnamelista  = THIS-OBJECT:GuruRadOption:CellNameColor.
               IF INDEX(cellnamelista,",") = 0 THEN e:ROW:Cells[THIS-OBJECT:GuruRadOption:CellNameColor]:Appearance:FontData:Name = THIS-OBJECT:GuruRadOption:CellFont:Name.
               ELSE DO:
                  REPEAT: 
                     IF INDEX(cellnamelista,",",1) NE 0 THEN DO:
                        cellnamecolorvar = SUBSTRING(cellnamelista,1,INDEX(cellnamelista,",",1) - 1).
                        e:ROW:Cells[cellnamecolorvar]:Appearance:FontData:Name = THIS-OBJECT:GuruRadOption:CellFont:Name.
                        /*
                        e:ROW:Cells[cellnamecolorvar]:Appearance:FontData:ItalicAsString = "True".
                         appearance2:FontData:BoldAsString = "True".
                           appearance2:FontData:ItalicAsString = "True".
                        */
                        cellnamelista = SUBSTRING(cellnamelista,INDEX(cellnamelista,",",1) + 1).
                     END.
                     ELSE   LEAVE.
                  END.
               END.   
            END.

         END.
      END.  
      
      RETURN.     

   END METHOD.
  
   
   /* För markering och positionering    */
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_AfterExitEditMode(INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
	   bandval = 1.
      DO:
         DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
         
         IF THIS-OBJECT:ActiveCell = ? THEN DO:
            RETURN.
         END.
            
         IF THIS-OBJECT:ActiveCell:Disposed EQ TRUE THEN DO:
            IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN .
            ELSE System.Windows.Forms.MessageBox:Show("Cell is disposed, ignoring.").
            RETURN.
         END.
          
         IF THIS-OBJECT:ActiveCell:VALUE = ? THEN DO:
            temprub = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(THIS-OBJECT:ActiveCell:Column:ToString()).
            IF temprub:DATATYP = "DECIMAL" OR temprub:DATATYP = "INTEGER" THEN DO: 
               THIS-OBJECT:ActiveCell:VALUE = "0".
            END.
            ELSE IF temprub:DATATYP = "CHARACTER" THEN DO:
               THIS-OBJECT:ActiveCell:VALUE = "".
            END.
            ELSE IF temprub:DATATYP = "DATE" THEN DO:
               THIS-OBJECT:ActiveCell:VALUE = "01/01/01".
            END. 
         END.
              
         IF THIS-OBJECT:ActiveRow NE ? THEN DO: 
            THIS-OBJECT:ActiveRow:Selected = TRUE.
         END.
        
         RETURN.
      END.
      CATCH ex AS System.ObjectDisposedException :
         System.Windows.Forms.MessageBox:Show("Disposed Exception, Objektnamn: " + ex:ObjectName).
      END CATCH.

	END METHOD.


   /* Allmänt */
	@VisualDesigner.
	METHOD PRIVATE VOID Grid_Error( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.ErrorEventArgs ):
		System.Windows.Forms.MessageBox:Show("Infragistics fel: " + e:ErrorText).
		RETURN.

	END METHOD.
   CONSTRUCTOR PUBLIC GridJoin ( ):      
      SUPER ().
      InitializeComponent ( ).
      THIS-OBJECT:DragDropGroups = NEW System.Collections.ArrayList().
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC GridJoin ( ):
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
      THIS-OBJECT:ContextMenuStrip:Items:Add(VisaViaInternetMenuItemAH).
      THIS-OBJECT:ContextMenuStrip:Items:Add(VisaViaInternetMenuItemES).
      THIS-OBJECT:ContextMenuStrip:Items:Add(VisaViaInternetMenuItemON).
      THIS-OBJECT:ContextMenuStrip:Items:Add(VisaViaInternetMenuItemSE).    
   END METHOD.
   /*ANVÄNDS FÖR ATT LÄGGA TILL EGNA MENYRADER*/ 
   METHOD PUBLIC VOID ExtramenuItemAdd(INPUT menytext AS CHARACTER):
      THIS-OBJECT:ExtraToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:ExtraToolStripMenuItem:Name = menytext.
      THIS-OBJECT:ExtraToolStripMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:ExtraToolStripMenuItem:Text = menytext.
      THIS-OBJECT:ContextMenuStrip:Items:Add(ExtraToolStripMenuItem).
      
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
   
    METHOD PUBLIC VOID GuruMultiGrid(bandval AS INTEGER, multi AS LOGICAL):
       Guruegenskap[bandval]:multiselect = multi.
    END METHOD.                                                                                                                                                
     
   /* Initierar en grid */
   METHOD PUBLIC VOID GuruInitGridJoinBindS(egenskaper AS Controls.GridRubrikListaJoin {GridRubrikListaExtent.i},antalbands AS INTEGER, tbsin AS Progress.Data.BindingSource):
      /*
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().
      */  
      tBS = tbsin.
      THIS-OBJECT:DataSource = tBS.
      Guruegenskap = egenskaper.
      antalGridBands = antalbands.
      THIS-OBJECT:GuruInitGrid().
   END METHOD.
   METHOD PUBLIC VOID GuruInitGridJoin(egenskaper AS Controls.GridRubrikListaJoin {GridRubrikListaExtent.i},antalbands AS INTEGER, DATASET-HANDLE ttdsh):
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().  
      tBS:HANDLE = ttdsh.
      THIS-OBJECT:DataSource = tBS.
      Guruegenskap = egenskaper.
      antalGridBands = antalbands.
      THIS-OBJECT:GuruInitGrid().
   END METHOD.
   METHOD PUBLIC VOID GuruInitGrid():
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      DEFINE VARIABLE bandvaljoin AS INTEGER             NO-UNDO.
      DEFINE VARIABLE temprubadd       AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE numcol        AS INTEGER             NO-UNDO.
      DEFINE VARIABLE rubfinns      AS LOGICAL             NO-UNDO.
      DEFINE VARIABLE numrub        AS INTEGER             NO-UNDO. /*antal rubriker räknare */
      DEFINE VARIABLE numfield      AS INTEGER             NO-UNDO.
      DEFINE VARIABLE numfieldrub   AS INTEGER             NO-UNDO.
      DEFINE VARIABLE incFields     AS CHARACTER           NO-UNDO.
      DEFINE VARIABLE rubfalt AS CHARACTER NO-UNDO.
     /*
      antalGridBands = antalbands.
      */
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
      
      THIS-OBJECT:VisaViaInternetMenuItemAh = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItemAh:Name = "VisaViaInternetMenuItemAh".
      THIS-OBJECT:VisaViaInternetMenuItemAh:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItemAh:Text = "Visa information via Ahlsell".
      THIS-OBJECT:VisaViaInternetMenuItemAh:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItemAh_Click). 
      
      THIS-OBJECT:VisaViaInternetMenuItemES = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItemES:Name = "VisaViaInternetMenuItemES".
      THIS-OBJECT:VisaViaInternetMenuItemES:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItemES:Text = "Visa information via Elektroskandia".
      THIS-OBJECT:VisaViaInternetMenuItemES:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItemES_Click).
      
      THIS-OBJECT:VisaViaInternetMenuItemON = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItemON:Name = "VisaViaInternetMenuItemON".
      THIS-OBJECT:VisaViaInternetMenuItemON:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItemON:Text = "Visa information via Onninen".
      THIS-OBJECT:VisaViaInternetMenuItemON:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItemON_Click).
      
      THIS-OBJECT:VisaViaInternetMenuItemSE = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:VisaViaInternetMenuItemSE:Name = "VisaViaInternetMenuItemON".
      THIS-OBJECT:VisaViaInternetMenuItemSE:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:VisaViaInternetMenuItemSE:Text = "Visa information via Selga".
      THIS-OBJECT:VisaViaInternetMenuItemSE:Click:Subscribe(THIS-OBJECT:VisaViaInternetMenuItemSE_Click).
      
      /*Återställ Sortering */
      THIS-OBJECT:AterstallSortMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:AterstallSortMenuItem:Name = "AterstallSortMenuItem".
      THIS-OBJECT:AterstallSortMenuItem:Size = NEW System.Drawing.Size(152, 22).
      THIS-OBJECT:AterstallSortMenuItem:Text = "Återställ sortering".
      THIS-OBJECT:AterstallSortMenuItem:Click:Subscribe(THIS-OBJECT:AterstallSortMenuItem_Click). 
      THIS-OBJECT:ContextMenuStrip:Items:Add(THIS-OBJECT:AterstallSortMenuItem).
            
      /* slut contextmenustrip */
      
      DEFINE VARIABLE varindex AS System.Object {GridRubrikListaExtent.i} NO-UNDO.
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      /*THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().  
      tBS:HANDLE = ttdsh.
      THIS-OBJECT:DataSource = tBS.
      Guruegenskap = egenskaper.
      */
      CREATE QUERY tqH.
      bandvaljoin = 1.
      /*query here*/
      
      DO WHILE bandvaljoin LE antalGridBands + 1 :
         tqH:ADD-BUFFER(Guruegenskap[bandvaljoin]:ttBufferHandle) NO-ERROR.
         bandvaljoin = bandvaljoin + 1.
      END. 
   
      bandvaljoin = 1.
      
    /*  THIS-OBJECT:tbs:AutoSort  = TRUE.*/
    /*
      THIS-OBJECT:DisplayLayout:Override:HeaderClickAction  = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
      */
      bandvaljoin =  1. 
      THIS-OBJECT:GuruDefaults(). 
      
            
      THIS-OBJECT:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
      bandvaljoin = 1.
      
      DO WHILE bandvaljoin LE antalGridBands :
         IF Guruegenskap[bandvaljoin]:multiselect = FALSE THEN THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
         ELSE THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
         /*      
         IF Guruegenskap[bandvaljoin]:tillatSort = FALSE THEN DO: 
            THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
            THIS-OBJECT:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
         END.
         IF Guruegenskap[bandvaljoin]:tillatDelete = FALSE THEN DO:
            THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
         END.
         */
         /* Egenskaper för kolumner (ordning,Rubrik) */
         
         numrub = 0.
         numfield = 0.
         numcol = THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns:Count.
         numfieldrub = Guruegenskap[bandvaljoin]:countRubrik().
         DO WHILE numfield < numcol:
            rubfinns = Guruegenskap[bandvaljoin]:finnsfalt(THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[numfield]:key).
            IF rubfinns = FALSE THEN DO:
               THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[numfield]:Hidden = TRUE.
               /*
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numfield]:Hidden = TRUE.
               */
            END.
            ELSE  DO:
               IF numrub < numfieldrub THEN  DO:
                  temprubadd = Guruegenskap[bandvaljoin]:getRubrik(numrub).
                  rubfalt = temprubadd:Falt.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:HEADER:VisiblePosition = temprubadd:Ordning.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:HEADER:Caption = temprubadd:Rubrik.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:Hidden = temprubadd:Gomma.         
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:Width = temprubadd:Bredd.

                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellAppearance = setApperance(rubfalt).
                  
                  /*Anger om det ska vara rowselectors (puckar), default = false, men sätter till true om det finns editerbara fält*/
                  IF temprubadd:Readonly = FALSE THEN DO:            
                     IF THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() = "False" THEN DO:            
                        THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
                     END.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellAppearance:BackColor = System.Drawing.ColorTranslator:FromHtml("#E5F3F9").
                  END. 
                  
                  IF temprubadd:Typvar = "TTRECID" THEN DO:
                     /*Ändra för att visa TTRECID-kollumen*/
                     IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns["TTRECID"]:Hidden = TRUE.
                     ELSE THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns["TTRECID"]:Hidden = TRUE .
                  END.
                  IF temprubadd:Typvar = "DEC" OR temprubadd:Typvar = "INT" THEN DO:
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[temprubadd:Falt]:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right.
                  END.
                  
                  bandval = Guruegenskap[bandvaljoin]:Bands.
                  {FORMATIGRIDS.I}
                  IF temprubadd:Readonly = TRUE THEN DO:
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect. /* FFFFFFFFFFFFF */
                  END.
                  ELSE DO:
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
                  END.
                  GuruCombocell(temprubadd). /* combobox i grid*/
                  IF rubfalt = "flog" THEN DO:         
                  END.
                  numrub = numrub + 1.
               END.
            END.
            numfield = numfield + 1.
         END.
         
         bandvaljoin = bandvaljoin + 1.
      END.
      
      gridProperties().
      THIS-OBJECT:tBS:ListChanged:Subscribe(GuruUpdateTitleE).
      THIS-OBJECT:Initierad = TRUE.
      
   
   END METHOD.  
      
   METHOD PUBLIC VOID GridData(DATASET-HANDLE ttdsh):
      IF tBS NE ? THEN  tBS:HANDLE = ?.
       THIS-OBJECT:Layouts:Clear().
       tBS:HANDLE = ttdsh.
       {WALLMAN/foreach.i System.Object oObject in THIS-OBJECT:DisplayLayout:Bands}           
         CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridBand):SortedColumns:Clear().
       END.
       THIS-OBJECT:Rows:Refresh(Infragistics.Win.UltraWinGrid.RefreshRow:ReloadData).
       THIS-OBJECT:Rows:ExpandAll(TRUE).
       
   END METHOD.   
   METHOD PUBLIC VOID GridBind(tbsin AS Progress.Data.BindingSource ):
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      DEFINE VARIABLE bandvaljoin AS INTEGER             NO-UNDO.
      DEFINE VARIABLE temprubadd       AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE numcol        AS INTEGER             NO-UNDO.
      DEFINE VARIABLE rubfinns      AS LOGICAL             NO-UNDO.
      DEFINE VARIABLE numrub        AS INTEGER             NO-UNDO. /*antal rubriker räknare */
      DEFINE VARIABLE numfield      AS INTEGER             NO-UNDO.
      DEFINE VARIABLE numfieldrub   AS INTEGER             NO-UNDO.
      DEFINE VARIABLE incFields     AS CHARACTER           NO-UNDO.
      DEFINE VARIABLE rubfalt AS CHARACTER NO-UNDO.
      tBS = tBSIN.
/*     
      THIS-OBJECT:tBS = NEW Progress.Data.BindingSource().  
      tBS:HANDLE = ttdsh.
      */      
      THIS-OBJECT:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
      bandvaljoin = 1. 
      DO WHILE bandvaljoin LE antalGridBands :
         IF Guruegenskap[bandvaljoin]:multiselect = FALSE THEN THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
         ELSE THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
               
         IF Guruegenskap[bandvaljoin]:tillatSort = FALSE THEN DO: 
            THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:Select.
            THIS-OBJECT:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
         END.
         IF Guruegenskap[bandvaljoin]:tillatDelete = FALSE THEN DO:
            THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
         END.
         /* Egenskaper för kolumner (ordning,Rubrik) */
         
         numrub = 0.
         numfield = 0.
         numcol = THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns:Count.
         numfieldrub = Guruegenskap[bandvaljoin]:countRubrik().
         DO WHILE numfield < numcol:
            rubfinns = Guruegenskap[bandvaljoin]:finnsfalt(THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[numfield]:key).
            IF rubfinns = FALSE THEN DO:
               THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[numfield]:Hidden = TRUE.
               /*
               THIS-OBJECT:DisplayLayout:Bands[0]:Columns[numfield]:Hidden = TRUE.
               */
            END.
            ELSE  DO:
               IF numrub < numfieldrub THEN  DO:
                  temprubadd = Guruegenskap[bandvaljoin]:getRubrik(numrub).
                  rubfalt = temprubadd:Falt.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:HEADER:VisiblePosition = temprubadd:Ordning.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:HEADER:Caption = temprubadd:Rubrik.
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:Hidden = temprubadd:Gomma.         
                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:Width = temprubadd:Bredd.

                  THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellAppearance = setApperance(rubfalt).
                  
                  /*Anger om det ska vara rowselectors (puckar), default = false, men sätter till true om det finns editerbara fält*/
                  IF temprubadd:Readonly = FALSE THEN DO:            
                     IF THIS-OBJECT:DisplayLayout:Override:RowSelectors:ToString() = "False" THEN DO:            
                        THIS-OBJECT:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True.
                     END.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellAppearance:BackColor = System.Drawing.ColorTranslator:FromHtml("#E5F3F9").
                  END. 
                  
                  IF temprubadd:Typvar = "TTRECID" THEN DO:
                     /*Ändra för att visa TTRECID-kollumen*/
                     IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns["TTRECID"]:Hidden = TRUE.
                     ELSE THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns["TTRECID"]:Hidden = TRUE .
                  END.
                  bandval = Guruegenskap[bandvaljoin]:Bands.
                   {FORMATIGRIDS.I}
                     
                  IF temprubadd:Readonly = TRUE THEN DO:
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect. /* FFFFFFFFFFFFF */
                  END.
                  ELSE DO:
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
                     THIS-OBJECT:DisplayLayout:Bands[Guruegenskap[bandvaljoin]:Bands]:Columns[rubfalt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
                  END.
                  GuruCombocell(temprubadd). /* combobox i grid*/
                  IF rubfalt = "flog" THEN DO:         
                  END.
                  numrub = numrub + 1.
               END.
            END.
            numfield = numfield + 1.
         END.
         
         bandvaljoin = bandvaljoin + 1.
      END.
      /*
      gridProperties().
      */
   END METHOD.  
        
   METHOD PUBLIC VOID VisaMtrlInter(bandval AS INTEGER):
      DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE lankst AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE THEN.
      ELSE RETURN. 
      valenr = Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD("ENR"):BUFFER-VALUE.
      IF Guru.Konstanter:appcon THEN DO:
         RUN LEVLANKC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, INPUT-OUTPUT valenr, OUTPUT lankst).
      END.
      ELSE DO:
         RUN LEVLANKC.P (INPUT Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE, INPUT-OUTPUT valenr, OUTPUT lankst).
      END. 
  
      IF Guru.Konstanter:globforetag = "VAST" THEN DO:
         IF lankst BEGINS "http://www.elektroskandia.se" THEN DO:
            MESSAGE "Med inloggning svara Ja"  VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Eloktroskandia" UPDATE svarpris AS LOGICAL.
            IF svarpris THEN. 
            ELSE lankst = "http://www.elektroskandia.se/enkel?prubId=0&artNr=" + valenr + "&utanPris=FALSE".             
         END.               
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
      THIS-OBJECT:GuruTillExcel().
      THIS-OBJECT:excelExporter:RowExporting:Unsubscribe(THIS-OBJECT:tillExcelMark).
      RETURN.
   END METHOD.
   
  /* Till excel handler*/
   METHOD PRIVATE VOID excelToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:GuruTillExcel().
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
      THIS-OBJECT:VisaMtrlInter(1).
      RETURN.
   END METHOD.
   METHOD PRIVATE VOID VisaViaInternetMenuItemAh_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:VisaMtrlInter(1).
      RETURN.
   END METHOD.
   METHOD PRIVATE VOID VisaViaInternetMenuItemES_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:VisaMtrlInter(1).
      RETURN.
   END METHOD.
   METHOD PRIVATE VOID VisaViaInternetMenuItemON_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:VisaMtrlInter(1).
      RETURN.
   END METHOD.
   METHOD PRIVATE VOID VisaViaInternetMenuItemSE_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:VisaMtrlInter(1).
      RETURN.
   END METHOD.
   
   
   METHOD PRIVATE VOID AterstallSortMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       {WALLMAN/foreach.i System.Object oObject in THIS-OBJECT:DisplayLayout:Bands}           
         CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridBand):SortedColumns:Clear().
       END.
       THIS-OBJECT:Rows:Refresh(Infragistics.Win.UltraWinGrid.RefreshRow:ReloadData).
       THIS-OBJECT:Rows:ExpandAll(TRUE).
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
      /* GridJoin */
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
      THIS-OBJECT:AfterCellUpdate:Subscribe(THIS-OBJECT:Grid_AfterExitEditMode).
      THIS-OBJECT:AfterEnterEditMode:Subscribe(THIS-OBJECT:Grid_AfterEnterEditMode).
      THIS-OBJECT:AfterExitEditMode:Subscribe(THIS-OBJECT:Grid_AfterExitEditMode).
      THIS-OBJECT:SelectionDrag:Subscribe(THIS-OBJECT:Grid_SelectionDrag).
      THIS-OBJECT:Error:Subscribe(THIS-OBJECT:Grid_Error).
      THIS-OBJECT:CellDataError:Subscribe(THIS-OBJECT:Grid_CellDataError).
      THIS-OBJECT:TextChanged:Subscribe(THIS-OBJECT:Grid_TextChanged).
      THIS-OBJECT:DragDrop:Subscribe(THIS-OBJECT:Grid_DragDrop).
      THIS-OBJECT:DragOver:Subscribe(THIS-OBJECT:Grid_DragOver).
      THIS-OBJECT:InitializePrintPreview:Subscribe(THIS-OBJECT:Grid_InitializePrintPreview).
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
   METHOD PUBLIC VOID GuruReadOnly(INPUT bandval AS INTEGER, INPUT readonlyvar AS LOGICAL, cellNamn AS CHARACTER ):
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      temprub = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(cellNamn).
      temprub:Readonly = readonlyvar.
      IF temprub:Readonly = TRUE THEN DO:
         /*Read only cell samt att hela raden selectas om man klickar i en read-only cell*/
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:BackColor = System.Drawing.Color:White. 
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect. /* FFFFFFFFFFFFF */
      END.
      ELSE DO:
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellAppearance:BackColor = System.Drawing.ColorTranslator:FromHtml("#E5F3F9").
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
         THIS-OBJECT:DisplayLayout:Bands[0]:Columns[temprub:Falt]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
      END.
      
   END METHOD.
   /*TÖMMER GRID*/
   METHOD PUBLIC VOID GuruTom(INPUT bandval AS INTEGER):
      IF NOT VALID-HANDLE(Guruegenskap[bandval]:ttBufferHandle) THEN RETURN.
      ttSortQuery = "FOR EACH " + Guruegenskap[bandval]:ttBufferHandle:TABLE + " WHERE TTRECID = ?" .
      tqH:QUERY-CLOSE. 
      tqH:QUERY-PREPARE(ttSortQuery).
      tqH:QUERY-OPEN ().
      /*Anders Olsson Elpool i Umeå AB  22 dec 2014 12:56:47 
      när behövs detta  
      THIS-OBJECT:Guruegenskap[bandval]:filter = "".
      */
           
   END METHOD. 
   /*Anders Olsson Elpool i Umeå AB  21 jun 2017 09:38:05 
   funkar inte! 
   
   METHOD PUBLIC VOID GuruReopenJoin():
       DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
       
       ttSortQuery = "FOR EACH " + Guruegenskap[1]:ttBufferHandle:TABLE. 
       bandval = 2.
       DO WHILE bandval LE antalGridBands :
         ttSortQuery = ttSortQuery + ",EACH " + Guruegenskap[bandval]:ttBufferHandle:TABLE.
         bandval = bandval + 1.
      END. 
     
      tqH:QUERY-PREPARE(ttSortQuery) NO-ERROR.
      tqH:QUERY-OPEN () NO-ERROR.
   END METHOD.           
   
      
   
   
   METHOD PUBLIC VOID GuruReopen(bandval AS INTEGER):
      DEFINE VARIABLE gridrowid AS ROWID NO-UNDO.
      EMPTY TEMP-TABLE rowidlista NO-ERROR.
      {WALLMAN/foreachultra.i System.Object oObject in THIS-OBJECT:Selected:Rows}           
         THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
         IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE = TRUE THEN DO:
            CREATE rowidlista.
            ASSIGN 
            rowidlista.id = THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID.
         END.
      END.
      FIND FIRST rowidlista WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE rowidlista THEN DO:
         IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE = TRUE THEN DO:
            CREATE rowidlista.
            ASSIGN 
            rowidlista.id = THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID.
         END. 
      END.
      THIS-OBJECT:GuruFiltrera(bandval).
      THIS-OBJECT:GuruAvmarkera().
      FIND FIRST rowidlista WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE rowidlista THEN DO:
         
      END.   
      ELSE DO:
         THIS-OBJECT:GuruFirstrow(INPUT bandval,OUTPUT gridrowid).
         IF gridrowid NE ? THEN DO:
            CREATE rowidlista.
            ASSIGN 
            rowidlista.id = gridrowid.
         END.    
      END.
      THIS-OBJECT:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE).
   END METHOD.
   */
   /*Väljer första raden i en grid, t.ex. vid start av programmet*/
   METHOD PUBLIC VOID GuruFirstrow(INPUT bandval AS INTEGER,OUTPUT gridrowid AS ROWID):
      
      
  
      /*
THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:Bands[bandval]:FirstRowInGrid)  NO-ERROR. 
      gridrowid = THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID.      
      
      THIS-OBJECT:GuruAfterDropped:Publish(THIS-OBJECT, CAST(args, System.EventArgs)).
      THIS-OBJECT:tBS:PositionChanged:Publish(). 
      */
      
   END METHOD.
   METHOD PUBLIC VOID GuruResetCurrentRowid():
      valdgridrowid = ?.
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
   
   METHOD PUBLIC VOID Grid_InitializePrintPreview(INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CancelablePrintPreviewEventArgs):
      e:DefaultLogicalPageLayoutInfo:FitWidthToPages = 1.
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
   
   
   /*Skriv ut grid med hjälp av ultragridkontroller: Används ej, behövs printSetup, dock snyggare
   METHOD PUBLIC VOID SkrivUt():
    
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
      IF oOnScreenRow:SELECTED = FALSE THEN e:Cancel = TRUE.
      ELSE  e:Cancel = FALSE.                
      RETURN.   
   END METHOD.   
   
  /* Alla poster till excel */
   METHOD PUBLIC VOID GuruTillExcel():
      DEFINE VARIABLE filnamn AS CHARACTER INITIAL "" NO-UNDO.
      DEFINE VARIABLE wbi AS Infragistics.Documents.Excel.Workbook NO-UNDO.
      DEFINE VARIABLE wSheet AS Microsoft.Office.Interop.Excel.Worksheet NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.    
      DEFINE VARIABLE fil AS System.IO.FileInfo.
      filnamn = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}.
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
      iex = iex + 1.
      IF iex > 10 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(227), "").
         iex = 0.
         RETURN.
      END.
      filnamn = filnamn + STRING(iex) + "klipput.xlsx".
      wbi = THIS-OBJECT:excelExporter:Export(THIS-OBJECT,Infragistics.Documents.Excel.WorkbookFormat:Excel2007).       
      wbi:Save(filnamn).
      THIS-OBJECT:excelApp = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      
      
      THIS-OBJECT:excelApp:Workbooks:Open(filnamn, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
      wSheet = CAST(THIS-OBJECT:excelApp:ActiveSheet, Microsoft.Office.Interop.Excel.Worksheet).
      wSheet:PageSetup:PaperSize = Microsoft.Office.Interop.Excel.XlPaperSize:xlPaperA4.
      wSheet:PageSetup:Orientation = Microsoft.Office.Interop.Excel.XlPageOrientation:xlLandscape.
      wSheet:PageSetup:Zoom = FALSE.
      wSheet:PageSetup:FitToPagesWide = 1.
      wSheet:PageSetup:FitToPagesTall = FALSE.
      IF Guru.GlobalaVariabler:ShowLevelsRow = 0 THEN Guru.GlobalaVariabler:ShowLevelsRow = 3.
      wSheet:Outline:ShowLevels(Guru.GlobalaVariabler:ShowLevelsRow ,0).
       /*
       <com-handle>: ShowLevels (RowLevels:=8, ColumnLevels:=8). 
     wSheet:Outline:ShowLevels(2,100).
     <anytype>-RowLevels,
     <anytype>-ColumnLevels ).
     */
      THIS-OBJECT:excelApp:Visible = TRUE.
     
      
      CATCH e AS System.IO.IOException:
         THIS-OBJECT:GuruTillExcel().
      END CATCH.
    
      /*CATCH e AS System.IO.IOException :
         MESSAGE "FANNS"
         VIEW-AS ALERT-BOX.
         THIS-OBJECT:excelExporter:Dispose().
      END CATCH.*/
      
      /*System.Diagnostics.Process:Start(filnamn).*/
      /* Ta bort filen:
      DEFINE VARIABLE fil AS System.IO.FileInfo.
      fil = new System.IO.FileInfo(filnamn).
      fil:Delete().
      */
   END METHOD.  
   
   METHOD PRIVATE VOID KopieraRad():
      THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:Copy, false, false).
   END METHOD.
   METHOD PUBLIC VOID MarkAll(INPUT bandval AS INTEGER):
      IF THIS-OBJECT:Guruegenskap[bandval]:multiselect THEN THIS-OBJECT:Selected:Rows:AddRange(THIS-OBJECT:Rows:GetAllNonGroupByRows()).
   END METHOD.
   /* Fångar event för pil upp,pil ned samt enter */
   METHOD PRIVATE VOID KeyDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.KeyEventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE lastshown AS INTEGER NO-UNDO.
      DEFINE VARIABLE ingentraff AS LOGICAL NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE keykodestring AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cellNamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cellvalue AS CHARACTER NO-UNDO.
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE cellNamnTabTo AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.   
      /*
      RETURN.
      
      keykodestring = e:KEYCODE:ToString(). /*Verkar inte fungera att jämföra e:KeyCode med System.Windows.Forms.Keys:Enter */
      cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
      cellvalue = THIS-OBJECT:ActiveCell:VALUE:ToString() NO-ERROR.
     
      IF cellNamn = "" OR THIS-OBJECT:ActiveCell:CanEnterEditMode = FALSE THEN DO:
         temprub = THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(ActiveValCol).
         sok = TRUE.
         
         IF temprub = ? THEN DO:
            temprub = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(1).
         END.
      END.  
      ELSE IF temprub = ? THEN DO:
         temprub = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(cellNamn).
      END.
      /*
      IF keykodestring = "delete" OR keykodestring = "back" THEN DO:
         IF THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "decimal" OR THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "integer" THEN DO:
             
            e:Handled = TRUE.
            RETURN.
         END.
      END.  
      */
      
      
      IF keykodestring = "OemPeriod" AND SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN DO:
         IF THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "decimal" THEN DO:
             rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(162), "").
            e:Handled = TRUE.
            RETURN.
         END.
      END.
      IF keykodestring = "OemComma" AND SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
         IF THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "decimal" THEN DO:
             rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(163), "").
            e:Handled = TRUE.
            RETURN.
         END.
      END.
      
      /* Lås integers och decimaler till 18 tecken -- */
      /*if LENGTH(cellvalue) > 18 THEN DO:
          IF THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "decimal" OR THIS-OBJECT:Guruegenskap[bandval]:GetGridCol(cellNamn):DATATYP = "integer"  THEN DO:
              IF keykodestring NE "delete" AND keykodestring NE "back" THEN DO:
                  e:Handled = TRUE.
                  RETURN.
              END.
          END.
      END.*/
      
      /*MESSAGE LENGTH(cellvalue)
      VIEW-AS ALERT-BOX.*/
      
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
         IF THIS-OBJECT:Guruegenskap[bandval]:multiselect THEN THIS-OBJECT:Selected:Rows:AddRange(THIS-OBJECT:Rows:GetAllNonGroupByRows()).
         RETURN.                     
      END.  
      IF keykodestring = "TAB" AND e:Modifiers:ToString() = "Shift" THEN DO:
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ActivateCell).
         cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
         i = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(cellNamn):Ordning.
         DO WHILE i > 0:
            i = i - 1.
            IF i < 1 THEN DO:
              /*inga skrivbara fält*/
               ingentraff = TRUE. 
               LEAVE.
            END.    
            IF THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Readonly = FALSE AND THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt]:Hidden = FALSE THEN DO:
               /*ny placering*/
               THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt).
               ingentraff = FALSE. 
               LEAVE.
            END.  
            ELSE ingentraff = TRUE.               
         END.
         IF ingentraff = TRUE THEN DO:
            /*bara en column är skrivbar*/
            IF tBS:POSITION = 0 THEN DO:
             /*UTNYTTJAR TTRECID COLUMN = 0*/
               THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(0):Falt).
               THIS-OBJECT:GuruReopen(INPUT bandval).
            END.
            ELSE DO:
               /*upp en rad*/
               THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:PrevRow) NO-ERROR.               
            END. 
            i = THIS-OBJECT:Guruegenskap[bandval]:countRubrik().
            DO WHILE i > 0:
               i = i - 1.
               IF i < 1 THEN .
               ELSE IF THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Readonly = FALSE AND THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt]:Hidden = FALSE THEN DO:
                  /*ny placering*/
                  THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt).
                  LEAVE.
               END.
            END.  
         END.   
         ingentraff = FALSE. 
         e:Handled = TRUE.
         RETURN.
      END.
      ELSE IF keykodestring = "TAB" THEN DO:
          
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ActivateCell).
         cellNamn = THIS-OBJECT:ActiveCell:Column:ToString() NO-ERROR. /* no-error annars meddelande om man inte står i en cell*/
         i = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(cellNamn):Ordning.
         DO WHILE i < THIS-OBJECT:Guruegenskap[bandval]:countRubrik():
            i = i + 1.
            IF i >= THIS-OBJECT:Guruegenskap[bandval]:countRubrik() THEN DO:
               /*inga skrivbara fält*/
               ingentraff = TRUE. 
               LEAVE.
            END.            
            IF THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Readonly = FALSE AND THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt]:Hidden = FALSE THEN DO:
               /*ny placering*/
               THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt).
               ingentraff = FALSE. 
               LEAVE.
            END.
            ELSE ingentraff = TRUE. 
               
         END.
         IF ingentraff = TRUE THEN DO:
            /*bara en column är skrivbar*/
            IF THIS-OBJECT:tBS:Count - 1 = tBS:POSITION THEN DO:
            
             /*UTNYTTJAR TTRECID COLUMN = 0*/
               THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(0):Falt).
               THIS-OBJECT:GuruReopen(INPUT bandval).
            END.
            ELSE DO:
               /*ner en rad*/
               THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow)  NO-ERROR.               
            END.          
            i = 0.
            DO WHILE i < THIS-OBJECT:Guruegenskap[bandval]:countRubrik():
               i = i + 1.
               IF i >= THIS-OBJECT:Guruegenskap[bandval]:countRubrik() THEN LEAVE.                  
               IF THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Readonly = FALSE AND THIS-OBJECT:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt]:Hidden = FALSE THEN DO:
                  /*ny placering*/
                  THIS-OBJECT:GuruCellvaljare(THIS-OBJECT:Guruegenskap[bandval]:getRubrik(i):Falt).
                  LEAVE.
               END.
            END.
         END.
         
         ingentraff = FALSE. 
         e:Handled = TRUE.  
         
         
            
         RETURN.
      END.     
       
      IF keykodestring = "Down" THEN DO:
         e:Handled = TRUE.
         IF temprub:readonly = TRUE THEN DO:
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
            RETURN.
         END.    
         
         IF cellNamn NE "" THEN DO:
           
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
            THIS-OBJECT:GuruCellvaljare(cellNamn).
         END.   
         RETURN.             
      END.
      IF keykodestring = "Up" THEN DO:
         e:Handled = TRUE.
         IF temprub:readonly = TRUE THEN DO:
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:PrevRow).
            RETURN.
         END.
         
         IF cellNamn NE "" THEN DO:
            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:PrevRow).
            THIS-OBJECT:GuruCellvaljare(cellNamn).
         END.   
         RETURN.   
      END.
      IF keykodestring = "Return" THEN DO:
         e:Handled = TRUE.
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:NextRow).
         IF cellNamn NE "" THEN DO:
            THIS-OBJECT:GuruCellvaljare(cellNamn).
         END.
         RETURN.              
      END.
     
      IF sok = TRUE AND temprub:readonly = TRUE THEN DO:
         IF Helpers.Functions:IsSiffraBokstav(keykodestring) = TRUE THEN DO:
            IF MTIME - lastkeydownI <= 500 THEN lastkeydownC = lastkeydownC + keykodestring.
            ELSE lastkeydownC = keykodestring.
            THIS-OBJECT:GuruFind(INPUT bandval,INPUT temprub,STRING(lastkeydownC)).
            lastkeydownI = MTIME.            
      END.    
   */
      RETURN.
   END METHOD.         
      
    
   METHOD PUBLIC VOID GuruOrderby():
      ttSortBy = "".
   END METHOD.
   
   METHOD PUBLIC VOID GuruOrderby(INPUT bandval AS INTEGER, INPUT rub AS Controls.GridRubrik):      
      ttSortBy = " BY " + Guruegenskap[bandval]:ttBufferHandle:TABLE + "." + rub:Falt.         
      IF THIS-OBJECT:Initierad THEN GuruFiltrera(INPUT bandval).   
   END METHOD.
   
   METHOD PUBLIC VOID GuruOrderby(INPUT bandval AS INTEGER,INPUT str AS CHARACTER):      
      IF str NE "" THEN  ttsortby = " " + str.
/*         ttSortBy = " BY " + Guruegenskap[bandval]:ttBufferHandle:TABLE + "." + str.*/
      ELSE  ttSortBy = "".   
      IF THIS-OBJECT:Initierad THEN GuruFiltrera(INPUT bandval).
   END METHOD.
   METHOD PUBLIC VOID GuruFiltreraTom(INPUT bandval AS INTEGER):
      THIS-OBJECT:Guruegenskap[bandval]:filter = "".
   END METHOD.
   METHOD PUBLIC CHARACTER GuruFiltreraGet(INPUT bandval AS INTEGER):
      RETURN  THIS-OBJECT:Guruegenskap[bandval]:filter.
   END METHOD.
   METHOD PUBLIC VOID GuruFiltrera(INPUT bandval AS INTEGER):
      IF NOT VALID-HANDLE( Guruegenskap[bandval]:ttBufferHandle) THEN RETURN.      
      ttSortQuery = "FOR EACH " + Guruegenskap[bandval]:ttBufferHandle:TABLE + " WHERE " + Guruegenskap[bandval]:filter + " " + ttSortBy.
      tqH:QUERY-PREPARE(ttSortQuery) NO-ERROR.
      tqH:QUERY-OPEN () NO-ERROR.
   END.
   
   /*Sätter om egenskap:filter samt öppnar om queryn*/
   METHOD PUBLIC VOID GuruFiltrera(INPUT bandval AS INTEGER,filt AS CHARACTER):
      IF NOT VALID-HANDLE(Guruegenskap[bandval]:ttBufferHandle) THEN RETURN.
      IF filt = ? THEN RETURN.
      THIS-OBJECT:Guruegenskap[bandval]:filter = filt + " ".
      ttSortQuery = "FOR EACH " + Guruegenskap[bandval]:ttBufferHandle:TABLE + " WHERE " + Guruegenskap[bandval]:filter + " " + ttSortBy.
      tqH:QUERY-CLOSE. 
      tqH:QUERY-PREPARE(ttSortQuery).
      tqH:QUERY-OPEN ().    
   END METHOD.
   /*används av ButtonOver för att markerar rätt rader.
     Kräver att en default-buffer-handle till en table inputtas, denna table behöver innehålla:
     FIELD id as ROWID
   */
   
  METHOD PUBLIC VOID SortByColumn(INPUT sortvar AS CHARACTER, INPUT villkor AS CHARACTER):
      
      
   END METHOD.
  
  
  METHOD PUBLIC VOID GuruMark(INPUT bandval AS INTEGER,INPUT sortvar AS CHARACTER, INPUT villkor AS CHARACTER):
      THIS-OBJECT:GuruAvmarkera().
      THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR .
      IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID).          
      END.
      
   END METHOD.
  
   METHOD PUBLIC VOID GuruFind(INPUT bandval AS INTEGER,INPUT temprub AS Controls.GridRubrik,INPUT villkor AS CHARACTER):
      DEFINE VARIABLE startRid AS ROWID NO-UNDO.
      DEFINE VARIABLE colformat AS CHARACTER NO-UNDO.
      DEFINE VARIABLE logfalse AS CHARACTER NO-UNDO.
      DEFINE VARIABLE logtrue AS CHARACTER NO-UNDO.
      DEFINE VARIABLE logvalue AS LOGICAL NO-UNDO.
      /*
      IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE = TRUE THEN DO:
         */
      IF Guruegenskap[bandval]:ttBufferHandle:TABLE   = ? THEN DO:   
         startRid = THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID.
      END.
      IF temprub:Typvar = "LOG" THEN DO:
         colformat = STRING(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD(temprub:Falt):FORMAT).
         logtrue = ENTRY (1,  colformat, "/").
         logfalse = ENTRY (2,  colformat, "/").
         IF villkor = "J" THEN logvalue = TRUE.
         ELSE IF villkor = "N" THEN logvalue = FALSE.
         IF logtrue BEGINS villkor THEN logvalue = TRUE.
         ELSE IF logfalse BEGINS villkor THEN logvalue = FALSE. 
         
      END.   
      THIS-OBJECT:GuruAvmarkera().
      REPEAT:
         /*söker från där du står och ned*/
         tqH:GET-NEXT(NO-LOCK).
         IF tqH:QUERY-OFF-END THEN LEAVE.
         IF tqH:QUERY-OFF-END THEN DO:
            tqH:GET-FIRST(NO-LOCK).
            IF tqH:QUERY-OFF-END THEN LEAVE.
         END.
         IF temprub:Typvar = "LOG" THEN DO:
            IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD(temprub:Falt):BUFFER-VALUE = logvalue THEN DO:
               THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID).
               RETURN.
            END.   
         END.  
         ELSE IF STRING(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD(temprub:Falt):BUFFER-VALUE) BEGINS villkor THEN DO:
           /*träff*/
            THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID).
            RETURN.
         END.    
         tqH:GET-NEXT(NO-LOCK).
         IF tqH:QUERY-OFF-END THEN LEAVE.      
      END.
      /*börjar om från början*/
      tqH:GET-FIRST(NO-LOCK).
      IF tqH:QUERY-OFF-END THEN LEAVE.
      REPEAT:
         IF STRING(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD(temprub:Falt):BUFFER-VALUE) BEGINS villkor THEN DO:
           /*träff*/
            THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID).
            RETURN.
         END.
         tqH:GET-NEXT(NO-LOCK).
         IF tqH:QUERY-OFF-END THEN LEAVE.
      END.
      THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:FIND-BY-ROWID(startRid). 
      IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE = TRUE THEN DO:
         THIS-OBJECT:GuruRadvaljare(THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:ROWID).          
      END.
   END METHOD.
  
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
         tqH:REPOSITION-TO-ROWID (rid) NO-ERROR.
         pos = tBS:POSITION.
         IF pos < 0 THEN RETURN.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
         THIS-OBJECT:ActiveRowScrollRegion:FirstRow = THIS-OBJECT:Rows[pos].
      END.
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  31 okt 2014 10:41:19 
         För att klara start lägen. 
         */
         tqH:REPOSITION-TO-ROW (1) NO-ERROR.
         pos = tBS:POSITION.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
      END.   
   END METHOD. 
   /* Markerar rader baserat på ROWID (FIRST ROW) */
   METHOD PUBLIC VOID GuruRadvaljare(INPUT rid AS ROWID, INPUT firstrow AS LOGICAL): 
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      tBS:Refresh().
      IF rid NE ? THEN DO:
         tqH:REPOSITION-TO-ROWID (rid) NO-ERROR.
       
         pos = tBS:POSITION.
         IF pos < 0 THEN RETURN.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
         IF firstrow = TRUE THEN DO:
            THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:FirstRow = THIS-OBJECT:Rows[pos].
         END.
      END.
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  31 okt 2014 10:41:19 
         För att klara start lägen. 
         */
         tqH:REPOSITION-TO-ROW (1) NO-ERROR.
         pos = tBS:POSITION.
         THIS-OBJECT:Rows[pos]:Activate() NO-ERROR.
         THIS-OBJECT:Rows[pos]:Selected = TRUE NO-ERROR.
        
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
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells[kolumnNr] NO-ERROR.
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False) NO-ERROR.         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID GuruCellvaljare(INPUT kolumnNamn AS CHARACTER):
     DO ON ERROR UNDO, LEAVE:
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells[kolumnNamn] NO-ERROR.
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False) NO-ERROR.         
      END.
   END METHOD.
   /*button over*/
   METHOD PUBLIC VOID GuruCellvaljare():
      DO ON ERROR UNDO, LEAVE:
         THIS-OBJECT:ActiveCell  = THIS-OBJECT:currentActiveRow:Cells["antal"] NO-ERROR.
         THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, False, False) NO-ERROR.
      END.
   END METHOD.
   
        
   /*använd denna efter att ny post skapats i TT i huvudprogrammet
   repo multigrid repo gridjoin
   */
   METHOD PUBLIC VOID GuruRepositionTo(INPUT rid AS RECID):
      DEFINE VARIABLE GridRowColl AS Infragistics.Win.UltraWinGrid.RowsCollection NO-UNDO.
      DEFINE VARIABLE GridRow     AS Infragistics.Win.UltraWinGrid.UltraGridRow   NO-UNDO.
      DEFINE VARIABLE iNumTopRows AS INTEGER                                      NO-UNDO.
      DEFINE VARIABLE ii          AS INTEGER                                      NO-UNDO.
      GridRowColl = THIS-OBJECT:Rows.
      
      iNumTopRows = GridRowColl:Count.
      ii = 0.   
      DO WHILE ii < iNumTopRows:
         GridRow = GridRowColl[ii].
         
         IF iNumTopRows = 1 THEN THIS-OBJECT:Traverse(INPUT GridRow, INPUT RID).
         ELSE IF GridRow:GetCellValue("TTRECID"):ToString() = STRING(rid) THEN DO:
            GridRow:Activate() . 
            RETURN. 
         END.
         ii = ii + 1.
     END.    
   END METHOD.
   METHOD PRIVATE VOID Traverse( INPUT ParentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow,INPUT rid AS RECID ):
        
        DEFINE VARIABLE ChildBandsColl AS Infragistics.Win.UltraWinGrid.ChildBandsCollection NO-UNDO.
        DEFINE VARIABLE ChildBand      AS Infragistics.Win.UltraWinGrid.UltraGridChildBand   NO-UNDO.
        DEFINE VARIABLE ChildRowsColl  AS Infragistics.Win.UltraWinGrid.RowsCollection       NO-UNDO.
        DEFINE VARIABLE ChildRow       AS Infragistics.Win.UltraWinGrid.UltraGridRow         NO-UNDO.
        DEFINE VARIABLE CurrentCell    AS Infragistics.Win.UltraWinGrid.UltraGridCell        NO-UNDO.
        DEFINE VARIABLE iNumBands      AS INTEGER                                            NO-UNDO.
        DEFINE VARIABLE iNumRows       AS INTEGER                                            NO-UNDO.
        DEFINE VARIABLE iNumCols       AS INTEGER                                            NO-UNDO.
        DEFINE VARIABLE i              AS INTEGER                                            NO-UNDO.
        DEFINE VARIABLE j              AS INTEGER                                            NO-UNDO.
        i = 0.
        IF ParentRow:HasChild() THEN DO:
            ChildBandsColl = ParentRow:ChildBands.
            iNumBands = ParentRow:ChildBands:Count.
            DO WHILE i < iNumBands:
                ChildBand = ChildBandsColl[i].
                ChildRowsColl = ChildBand:Rows.
                iNumRows = ChildRowsColl:Count.
                j = 0.
                DO WHILE j < iNumRows:
                   ChildRow = ChildRowsColl[j].
                   IF ChildRow:GetCellValue("TTRECID"):ToString() = STRING(rid) THEN DO:
                      ChildRow:Activate() .
                      RETURN.
                   END.
                    Traverse(ChildRow,RID).
                    j = j + 1.
                END.
                i = i + 1.
            END.
        END.
        /* This row has no children: display the Text and Value properties for each cell in the row */
        ELSE DO:
            CurrentCell = ParentRow:Cells[0].
            IF ParentRow:GetCellValue("TTRECID"):ToString() = STRING(rid) THEN DO:
               ParentRow:Activate() .
               RETURN.
            END.    
              /*
            IF CurrentCell:Text = STRING(rid) THEN ParentRow:Activate() .
            */
        END.

    END METHOD.
   
   
   
   
   
   METHOD PUBLIC VOID GuruMulteQ(INPUT ttsqin AS character):
      ttSortQuery = "FOR EACH " + ttsqin.
      
      tqH:QUERY-CLOSE. 
      tqH:QUERY-PREPARE(ttSortQuery).
      tqH:QUERY-OPEN (). 
      THIS-OBJECT:Rows:ExpandAll(TRUE).   
   END METHOD.
   
   
   /*SÖK HÄR*/
   
   
      
   METHOD PUBLIC INTEGER Posn():
      DEFINE VARIABLE pos AS INTEGER NO-UNDO. 
      /* VILKEN RAD STÅR JAG PÅ*/
      pos =  tBS:POSITION.
      RETURN pos.  
   END METHOD.
   
   METHOD PUBLIC VOID Nyrad(INPUT bandval AS INTEGER):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      Guruegenskap[bandval]:ttBufferHandle:BUFFER-CREATE ().
      Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = Guruegenskap[bandval]:ttBufferHandle:RECID.
      rid = Guruegenskap[bandval]:ttBufferHandle:ROWID.
      tqh:QUERY-OPEN ().
      GuruRepositionTo(Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD("TTRECID"):BUFFER-VALUE).
   END METHOD.
   
  /* Denna metod anropar gridBeforeRowsDeleted eftersom den subscribeas i gridProperties() */
   METHOD PUBLIC VOID GuruBortrader(INPUT bandval AS INTEGER):
      
      IF THIS-OBJECT:Guruegenskap[bandval]:multiselect THEN THIS-OBJECT:DeleteSelectedRows() .
      ELSE THIS-OBJECT:ActiveRow:Delete().
   END METHOD.
   
   /* Kör denna om rader ska bort utan fråga */
   METHOD PUBLIC VOID GuruBortrader(INPUT bandval AS INTEGER,fraga AS LOGICAL):
      DEFINE VARIABLE canDeletedefault  AS LOGICAL NO-UNDO.
      canDeletedefault = canDelete.
      canDelete = TRUE.
      Gurudelfraga = fraga.
      GuruBortrader(INPUT bandval).
      Gurudelfraga = TRUE.
      canDelete = canDeletedefault.
   END METHOD.
   
   METHOD PUBLIC VOID GuruBortalla(INPUT bandval AS INTEGER):
            /*
      Guruegenskap[bandval]:ttBufferHandle:EMPTY-TEMP-TABLE () går ej om dataset.
      */
      
      tqH:GET-FIRST().   
      DO WHILE tqH:QUERY-OFF-END = FALSE:    
         THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:BUFFER-DELETE() NO-ERROR.
         tqH:GET-NEXT(NO-LOCK).
      END.
      GuruFiltrera(INPUT bandval).
       
   END METHOD.
  
   /* Returnerar ett Appearance beroende på FÄLT-text */
   METHOD PRIVATE Infragistics.Win.Appearance setApperance(falt AS CHARACTER):
      DEFINE VARIABLE appearanceText AS Infragistics.Win.Appearance NO-UNDO.
      appearanceText = NEW  Infragistics.Win.Appearance().
      IF falt = "AONR" OR falt = "DELNR" THEN appearanceText:ForeColor = System.Drawing.Color:Green.
      ELSE IF falt = "BERNR" THEN appearanceText:ForeColor = System.Drawing.Color:Purple.
      ELSE IF falt = "KALKNR"   THEN appearanceText:ForeColor = System.Drawing.Color:Blue.
      ELSE IF falt = "PLANNR" THEN appearanceText:ForeColor = System.Drawing.Color:DarkRed.
      ELSE IF falt = "ARBKOD" OR falt = "LOPNR" OR falt = "ARBKODLOPNR" THEN appearanceText:ForeColor = System.Drawing.Color:Red.
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
   
   METHOD PRIVATE VOID BindSCreateRow(rSender AS System.Object, rArgs AS Progress.Data.CreateRowEventArgs):
      /*DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.*/
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      rArgs:Created = FALSE.
      Guruegenskap[bandval]:ttBufferHandle:BUFFER-CREATE().
      /*tqh:QUERY-OPEN ().*/
      DO ON ERROR undo, LEAVE:
         tqH:CREATE-RESULT-LIST-ENTRY ().      
         rArgs:Created = TRUE.                           
      END.

   END METHOD.

   /* cancel på create-row*/
   METHOD PRIVATE VOID BindSCancelCreateRow(rSender AS System.Object, rArgs AS Progress.Data.CancelCreateRowEventArgs).
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      tqH:GET-CURRENT(EXCLUSIVE-LOCK).    
      THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:buffer-delete(). /*behöver man göra mer?*/   
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
      DEFINE VARIABLE bandval  AS INTEGER NO-UNDO.
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
            IF THIS-OBJECT:Guruegenskap[bandval]:ttBufferHandle:AVAILABLE  THEN DO:
               ttrr = Guruegenskap[bandval]:ttBufferHandle:RECID.
            END.   
            SetOnRow = THIS-OBJECT:ActiveRow.
            /* Kolla om någon är vald mitt i, dvs alla markerade poster inte är samlade i en klunga, isf repositonera till den sen! 
            iterator = SISTA RADEN SOM ÄR MARKERAD EV FÖRSTA (visar alltid en för lite)  alla var valda
            ttrr = recid till den som ska markeras efter
            running = true har inte hittat någon och har inte gått igenom alla.
            SetOnRow = aktiv rad.
            rArgs:Rows:Length = ANTAL MARKERADE
            THIS-OBJECT:Rows:Count ANTAL  POSTER
            */
            
            iterator = LastSelectedRow:INDEX.
            /*alla i gridden bort*/
           
            IF rArgs:Rows:Length = THIS-OBJECT:Rows:Count THEN DO:
               ttrr = ?.
               GuruBortalla(INPUT bandval).
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
               MESSAGE "Fel, var god kontakta Elpool Support. Uppge 'gridBeforeRowsDeleted'"
               VIEW-AS ALERT-BOX.
            END.   
            tqH:Query-Open().
           /*sync? Avmarkering?*/
            /*SÖK INGEN OM DET ITE FINNS NÅGON*/
            IF ttrr NE ? THEN DO:
               Guruegenskap[bandval]:ttBufferHandle:FIND-FIRST("WHERE TTRECID  = " + string(ttrr)) NO-ERROR.
               SetOnRow:Activate().
            END.
              
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
   METHOD  PUBLIC CHARACTER GridFaltToString (INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs):
      DEFINE VARIABLE temprub AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      IF e:Cell:VALUE = ? THEN DO:
          e:Cell:CancelUpdate().
          /*
         temprub = THIS-OBJECT:Guruegenskap[bandval]:getRubrik(e:Cell:Column:ToString()).
         IF temprub:DATATYP = "DECIMAL" OR temprub:DATATYP = "INTEGER" THEN  kolvarde = "0".
         ELSE IF temprub:DATATYP = "CHARACTER" THEN  kolvarde = "".
         ELSE IF temprub:DATATYP = "DATE" THEN  kolvarde = "01/01/01". 
         THIS-OBJECT:ActiveRow:Cells[e:Cell:Column:ToString()]:VALUE = kolvarde.
         */
      END.
      RETURN kolvarde.
         
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
      /*
      THIS-OBJECT:MouseClick:Subscribe(musklick).
      THIS-OBJECT:MouseDown:Subscribe(musklick).
      */
      /* repos till markerad rad vid sort*/
      /*
      THIS-OBJECT:AfterSortChange:Subscribe(THIS-OBJECT:hittaRad).
      /*R*/
     
      THIS-OBJECT:AfterSortChange:Subscribe(THIS-OBJECT:GridAfterSortChange).
      */
   END METHOD.
   METHOD PRIVATE VOID SortReq (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      THIS-OBJECT:SortReq ().
          
   END METHOD.
   METHOD PRIVATE VOID SortReq():
      
   END METHOD.
   /*hitta åt den markerade raden i grid efter att användaren sorterat*/
   METHOD PRIVATE VOID hittaRad (sender AS System.Object, e AS System.EventArgs):
      /*no error annars felmed vid gruppering*/
      MESSAGE "hitta"
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:ScrollRowIntoView(THIS-OBJECT:ActiveRow) NO-ERROR.
   END METHOD.
   
   METHOD PUBLIC VOID GridAfterSortChange(INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BandEventArgs):
     DEFINE VARIABLE bandval AS INTEGER NO-UNDO.   
     bandval = 1.   
     MESSAGE "after"
      VIEW-AS ALERT-BOX.
      ActiveValCol = string(e:Band:SortedColumns[0]) NO-ERROR.
      ttSortBy = SortExpression(e:Band).
      IF NOT VALID-HANDLE(Guruegenskap[bandval]:ttBufferHandle) THEN RETURN.
      ttSortQuery = "FOR EACH " + Guruegenskap[bandval]:ttBufferHandle:TABLE + " WHERE " + Guruegenskap[bandval]:filter + ttSortBy.  
      tqH:QUERY-PREPARE(ttSortQuery) NO-ERROR.
      tqH:QUERY-OPEN () NO-ERROR.
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
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
       bandval = 1.
      DO WHILE bandval LE antalGridBands :
         Guruegenskap[bandval]:ttBufferHandle:SYNCHRONIZE ().
         bandval = bandval + 1.
      END.   
      /*THIS-OBJECT:SyncMan ().   */         
   END METHOD. 
   
   METHOD PUBLIC VOID SyncMan ():       
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      IF THIS-OBJECT:ActiveRow = ? THEN RETURN.
      IF THIS-OBJECT:ActiveRow:Cells["TTRECID"]:VALUE = ? THEN DO:
         RETURN.
      END.   
      villkor = "WHERE ".
      villkor = villkor + "TTRECID = " + STRING(THIS-OBJECT:ActiveRow:Cells["TTRECID"]:VALUE).
      Guruegenskap[bandval]:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
       
   END METHOD.      
   
   
   METHOD PRIVATE LOGICAL ProcessRowChanges():
      DEFINE VARIABLE hBeforeBuffer AS HANDLE NO-UNDO.
      DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      bandval = 1.
      hBeforeBuffer = Guruegenskap[bandval]:ttBufferHandle:BEFORE-BUFFER.
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
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      villkor = "WHERE ".
      num = 0.      
      rCells = rCurrentRow:Cells.
      och = "".     
      ttr = rCells["TTRECID"]:VALUE.
      villkor = villkor + "TTRECID = " + ttr.   
      Guruegenskap[bandval]:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      /*om Delete är definierat, kopiera borttagen rad till del.*/
      IF ttBufferHandleDel NE ? THEN DO:
         Guruegenskap[bandval]:ttBufferHandle:BUFFER-COPY (ttBufferHandleDel).
      END.
      IF Guruegenskap[bandval]:ttBufferHandle:AVAILABLE THEN Guruegenskap[bandval]:ttBufferHandle:BUFFER-DELETE () NO-ERROR.
     
   END METHOD.
   
   METHOD PUBLIC VOID ClearGroup(band AS INTEGER):
      THIS-OBJECT:DisplayLayout:Bands[band]:ClearGroupByColumns().
   END METHOD.
   METHOD PUBLIC VOID SetGroupHeader(band AS INTEGER, grupp AS CHARACTER, rubrikNamn AS CHARACTER):
     DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
     DEFINE VARIABLE group1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
     band1 = THIS-OBJECT:DisplayLayout:Bands[band]. 
     group1 = band1:Groups[grupp].
     group1:HEADER:Caption = rubrikNamn.
      
  END METHOD.  
   METHOD PUBLIC VOID SkapaGrupp(band AS INTEGER, gruppNamn AS CHARACTER, rubrikNamn AS CHARACTER):
      DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      DEFINE VARIABLE group1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      band1 = THIS-OBJECT:DisplayLayout:Bands[band].
      band1:Groups:Add(gruppNamn, rubrikNamn).
   END METHOD.
   
   METHOD PUBLIC VOID SetGroup(band AS INTEGER, grupp AS CHARACTER, kollumn AS CHARACTER):
      DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      DEFINE VARIABLE group1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      DEFINE VARIABLE col1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO. 
      band1 = THIS-OBJECT:DisplayLayout:Bands[band]. 
      group1 = band1:Groups[grupp].         
      col1 = band1:Columns[kollumn]. 
      col1:Hidden = FALSE.
      group1:Columns:Add(col1, 0, 0).
   END METHOD.
   METHOD PUBLIC VOID SetGroupORD(band AS INTEGER, grupp AS CHARACTER, kollumn AS CHARACTER):
      DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      DEFINE VARIABLE group1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      DEFINE VARIABLE col1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO. 
      band1 = THIS-OBJECT:DisplayLayout:Bands[band]. 
      group1 = band1:Groups[grupp].         
      col1 = band1:Columns[kollumn]. 
      col1:Hidden = FALSE.
      group1:Columns:Add(col1).
   END METHOD.
   /*gömer en hel grupp*/
   METHOD PUBLIC VOID VisibleGroup(band AS INTEGER, grupp AS CHARACTER, gomavisa AS LOGICAL):
      DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      DEFINE VARIABLE group1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      DEFINE VARIABLE col1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO. 
      band1 = THIS-OBJECT:DisplayLayout:Bands[band]. 
      group1 = band1:Groups[grupp].
      group1:HIDDEN = gomavisa.
      
   END METHOD.        
   METHOD PUBLIC VOID VisibleGroupHeaders(band AS INTEGER, gomavisa AS LOGICAL):
      DEFINE VARIABLE band1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      band1 = THIS-OBJECT:DisplayLayout:Bands[band]. 
      band1:GroupHeadersVisible = gomavisa.
      
   END METHOD.
   /*Används för att sortera kolumner i bokstavsordning efter generering, hårdkodningar för schaktvisning*/ 
   METHOD PUBLIC VOID SortBandColl(INPUT bandval AS INTEGER):
      DEFINE VARIABLE iCount AS INTEGER INITIAL 0.
      EMPTY TEMP-TABLE ttSort.
      iCount = 0.
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns}
         CREATE ttSort.
         ASSIGN 
         ttSort.namn = CAST(oObject,Infragistics.Win.UltraWinGrid.UltraGridColumn):Header:Caption
         ttSort.nyckel = CAST(oObject,Infragistics.Win.UltraWinGrid.UltraGridColumn):Key.
      END.
      iCount = 4.
      FOR EACH ttSort NO-LOCK BY ttSort.namn:
         IF THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Key = "OVRIGT" THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Header:VisiblePosition = (THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns:Count - 1).
         END.
         ELSE IF THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Key = "FRANTILL" THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Header:VisiblePosition = 1. 
         END.
         ELSE IF THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Key = "UTFORT" THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Header:VisiblePosition = 0. 
         END.
         ELSE IF THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Key = "SPROFIL" THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Header:VisiblePosition = 2. 
         END.
         ELSE DO:
            THIS-OBJECT:DisplayLayout:Bands[bandval]:Columns[ttSort.nyckel]:Header:VisiblePosition = iCount.
            iCount = iCount + 1.
         END.
      END.
        
   END METHOD.
   /*Synkar radvärde med ttvärde*/
   METHOD PUBLIC VOID valueSynch(cellNamn AS CHARACTER):
      DEFINE VARIABLE bandval AS INTEGER NO-UNDO.
      THIS-OBJECT:ActiveRow:Cells[cellNamn]:VALUE = Guruegenskap[bandval]:ttBufferHandle:BUFFER-FIELD(cellNamn):BUFFER-VALUE.     
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
   METHOD PUBLIC LOGICAL RadInGrid():
      IF THIS-OBJECT:ActiveRow = ? THEN DO:   
         RETURN FALSE.   
      END.
      ELSE RETURN TRUE.
   END METHOD.
END CLASS.