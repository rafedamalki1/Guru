 
 /*------------------------------------------------------------------------
    File        : GuruGridColorFilter
    Purpose     : 
    Syntax      : 
    Description : S?tter bakgrundsf?rg p? cols/rows beroende p? input

Sets backgroundcolor on cols/rows depending on input
    Author(s)   : elpfh
    Created     : Mon Feb 27 08:28:58 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.Subclasses.GuruGridColorFilter INHERITS System.Object: 
   DEFINE VARIABLE RowCache AS System.Collections.ArrayList NO-UNDO.
   CONSTRUCTOR PUBLIC GuruGridColorFilter(INPUT g AS Controls.Grid, INPUT iscol AS LOGICAL, INPUT colid AS CHARACTER,INPUT vil AS CHARACTER ):
      THIS-OBJECT:Grid = g.
      THIS-OBJECT:DefaultBgColor = THIS-OBJECT:Grid:BackColor.
      THIS-OBJECT:DefaultTextColor = THIS-OBJECT:Grid:ForeColor.
      THIS-OBJECT:TextColor = THIS-OBJECT:DefaultTextColor.
      THIS-OBJECT:BgColor = THIS-OBJECT:DefaultBgColor.
      THIS-OBJECT:IsColumns = iscol.
      THIS-OBJECT:ColumnIdentifier = colid.
      THIS-OBJECT:Villkor = vil.
      THIS-OBJECT:Activated = FALSE.
      THIS-OBJECT:SetText = TRUE.
      THIS-OBJECT:SetBg = TRUE.
      THIS-OBJECT:RowCache = NEW System.Collections.ArrayList().
       
   END CONSTRUCTOR.
   
   METHOD PUBLIC LOGICAL GuruFindHash(INPUT a AS CHARACTER):
      IF THIS-OBJECT:RowCache:Count > 0 THEN DO:  
         {WALLMAN\foreach.i System.Object ocObject in THIS-OBJECT:RowCache}
            IF STRING(ocObject:ToString()) = a THEN DO:
               RETURN TRUE.
            END.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
   METHOD PUBLIC VOID SetCache(): 
      THIS-OBJECT:RowCache:Clear().
      {WALLMAN\foreachultra.i System.Object ocObject in THIS-OBJECT:Grid:Rows}
         THIS-OBJECT:RowCache:Add(STRING(CAST(ocObject, Infragistics.Win.UltraWinGrid.UltraGridRow):Cells["TTRECID"]:Value)).
         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID UpdateGrid():
      /* Om f?rgfiltret ?r aktiverat */
      IF THIS-OBJECT:Activated THEN DO:
         /* Spara det gamla gurufiltret, s?tt ett nytt filter f?r att hitta r?tt rader efter query, cachea dom raderna, filtrera tillbaks till det gamla filtret */
         DEFINE VARIABLE OldFilter AS CHARACTER NO-UNDO.
         OldFilter = THIS-OBJECT:Grid:Guruegenskap:filter.
         IF TRIM(OldFilter) = "" THEN DO: 
            THIS-OBJECT:Grid:GuruFiltrera(THIS-OBJECT:Villkor).
         END.
         ELSE DO:
            THIS-OBJECT:Grid:GuruFiltrera(OldFilter + " AND " + THIS-OBJECT:Villkor).
         END.
         
         THIS-OBJECT:SetCache().
         
         THIS-OBJECT:Grid:GuruFiltrera(OldFilter).
         
         /* G? igenom alla gridrader*/
         DEFINE VARIABLE cRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.         
         {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Grid:Rows}
            cRow = CAST(oObject, Infragistics.Win.UltraWinGrid.UltraGridRow).
            /* Om nuvarande rad hittas i cachen vi genererade innan*/
            IF THIS-OBJECT:GuruFindHash(STRING(cRow:Cells["TTRECID"]:Value)) = TRUE THEN DO:
               /* Separera rader och kolumner*/
               IF THIS-OBJECT:IsColumns = TRUE THEN DO:
                  /* F?rga kolumner*/
                  IF THIS-OBJECT:SetBg THEN cRow:Cells[THIS-OBJECT:ColumnIdentifier]:Appearance:BackColor = THIS-OBJECT:BgColor.
                  IF THIS-OBJECT:SetText THEN cRow:Cells[THIS-OBJECT:ColumnIdentifier]:Appearance:ForeColor = THIS-OBJECT:TextColor.
               END.
               ELSE DO:
                  /* F?rga rader*/
                  IF THIS-OBJECT:SetBg THEN cRow:Appearance:BackColor = THIS-OBJECT:BgColor.
                  IF THIS-OBJECT:SetText THEN cRow:CellAppearance:ForeColor = THIS-OBJECT:TextColor.
               END.
            END.
         END.
      END.
   END METHOD.
   
   /* Tillh?rande grid */
   DEFINE PUBLIC PROPERTY Grid AS Controls.Grid NO-UNDO
      PUBLIC GET.
      PRIVATE SET.
   
   /* F?rgen som ska s?ttas */
   DEFINE PUBLIC PROPERTY TextColor AS System.Drawing.Color NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
      
   /* F?rgen som ska s?ttas */
   DEFINE PUBLIC PROPERTY BgColor AS System.Drawing.Color NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
   
   /* Standardf?rgen */
   DEFINE PUBLIC PROPERTY DefaultBgColor AS System.Drawing.Color NO-UNDO
      PUBLIC GET.
      PRIVATE SET. 

   /* Standardf?rgen */
   DEFINE PUBLIC PROPERTY DefaultTextColor AS System.Drawing.Color NO-UNDO
      PUBLIC GET.
      PRIVATE SET. 
   
   /* Vilken kolumn som ska s?ttas/kollas */
   DEFINE PUBLIC PROPERTY ColumnIdentifier AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   /* F?rgar kolumn ColumnIdentifier om true, annars rows */
   DEFINE PUBLIC PROPERTY IsColumns AS LOGICAL NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
      
   /* F?rgar text? F?rgar bg?*/
   DEFINE PUBLIC PROPERTY SetText AS LOGICAL NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
   
   DEFINE PUBLIC PROPERTY SetBg AS LOGICAL NO-UNDO
      PUBLIC GET.
      PUBLIC SET. 
      
   DEFINE PUBLIC PROPERTY Villkor AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   /* Om filtret ?r aktiverat*/
   DEFINE PUBLIC PROPERTY Activated AS LOGICAL NO-UNDO
      PUBLIC GET.
      PUBLIC SET.

END CLASS.