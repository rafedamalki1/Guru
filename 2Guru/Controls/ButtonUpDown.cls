 
 /*------------------------------------------------------------------------
    File        : ButtonUpDown
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri Nov 28 14:02:21 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.Misc.UltraButton.



CLASS Controls.ButtonUpDown INHERITS UltraButton: 
	
	DEFINE TEMP-TABLE rowidlista NO-UNDO 
      FIELD id AS ROWID
      FIELD ORGORDNING AS INTEGER
      FIELD NYORDNING AS INTEGER
      INDEX ORGORDNING ORGORDNING
      INDEX NYORDNING NYORDNING.
  
   /*Gridden som posterna hämtas FRÅN */
   DEFINE PROPERTY GridGuruUpDown AS Controls.Grid NO-UNDO
      PUBLIC GET.
      PRIVATE SET.
   DEFINE PRIVATE PROPERTY GuruUpDown AS LOGICAL INITIAL FALSE NO-UNDO
      PRIVATE GET.
      PRIVATE SET. 
   DEFINE PRIVATE PROPERTY GuruUpDownField AS CHARACTER  NO-UNDO
      PRIVATE GET.
      PRIVATE SET. 
   DEFINE VARIABLE ToolTipper AS System.Windows.Forms.ToolTip NO-UNDO.
    
   DEFINE PRIVATE VARIABLE InternalTooltip AS CHARACTER NO-UNDO.
    
   DEFINE PUBLIC PROPERTY GuruTooltip AS CHARACTER  NO-UNDO
   PUBLIC GET():
       RETURN THIS-OBJECT:InternalTooltip.
       END GET.
   PUBLIC SET(a AS CHARACTER):
       THIS-OBJECT:SetTooltip(a).
       THIS-OBJECT:InternalTooltip = a.
       END SET.		
	CONSTRUCTOR PUBLIC ButtonUpDown (  ):
		
		
      SUPER().
      THIS-OBJECT:InitializeTooltip().

	END CONSTRUCTOR.
	METHOD PRIVATE VOID InitializeTooltip():
      THIS-OBJECT:ToolTipper = NEW System.Windows.Forms.ToolTip().
      THIS-OBJECT:ToolTipper:AutoPopDelay = 0.
      THIS-OBJECT:ToolTipper:InitialDelay = 1.
      THIS-OBJECT:ToolTipper:ReshowDelay = 1.
      THIS-OBJECT:ToolTipper:ShowAlways = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID SetTooltip(t AS CHARACTER):
      THIS-OBJECT:ToolTipper:SetToolTip(THIS-OBJECT, t). 
   END METHOD.
   METHOD PUBLIC VOID GuruKoppla(grid1 AS Controls.Grid, ordvarCH AS CHARACTER , updown AS LOGICAL):
      ASSIGN
      GridGuruUpDown = grid1
      GuruUpDown = updown
      GuruUpDownField = ordvarCH.
   END METHOD.
	METHOD PUBLIC LOGICAL RadFromGrid():
      IF THIS-OBJECT:GridGuruUpDown:ActiveRow = ? THEN DO:   
         RETURN FALSE.   
      END.
      ELSE RETURN TRUE.
   END METHOD.
	
   METHOD PUBLIC VOID GuruUpDown(maxordning AS INTEGER,villkor  AS CHARACTER):  
      DEFINE VARIABLE gridrowid AS ROWID NO-UNDO. 
         
      DEFINE VARIABLE rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE forsta AS INTEGER NO-UNDO.
      DEFINE VARIABLE sista AS INTEGER NO-UNDO.
      EMPTY TEMP-TABLE rowidlista.
      
      /* Markerade poster*/
      {WALLMAN/foreach.i System.Object oObject in GridGuruUpDown:Selected:Rows}                  
         rCurrentRow = CAST (oObject, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .      
         IF Findskap(rCurrentRow, villkor) = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(173),"",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            RETURN.
         END.
      END. /*foreach end*/
      
      /*Anders Olsson Elpool i Umeå AB  28 nov 2014 15:34:18 
      kollar markeringen. 
      */ 
      IF villkor = "" THEN.
      ELSE villkor = " WHERE " + villkor.
      queryvar =  "FOR EACH " + THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:TABLE + villkor.
      qH = Guru.konstanter:globalroot:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle,queryvar).
      IF maxordning = 0 THEN DO:
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            qH:GET-NEXT().
         END.
         maxordning = qH:NUM-RESULTS.
      END.      
     
      FOR EACH rowidlista WHERE NO-LOCK:
         IF GuruUpDown = TRUE THEN rowidlista.NYORDNING = rowidlista.ORGORDNING - 1.
         ELSE IF GuruUpDown = FALSE THEN rowidlista.NYORDNING = rowidlista.ORGORDNING + 1.
         IF rowidlista.NYORDNING = 0 OR  rowidlista.NYORDNING >  maxordning THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(173),"",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            RETURN.
         END.   
      END.
      FIND FIRST rowidlista WHERE USE-INDEX ORGORDNING NO-LOCK NO-ERROR.
      forsta = rowidlista.ORGORDNING.
      FIND LAST rowidlista WHERE USE-INDEX ORGORDNING NO-LOCK NO-ERROR.
      sista = rowidlista.ORGORDNING.
     
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         FIND FIRST rowidlista WHERE rowidlista.id = THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:ROWID NO-LOCK NO-ERROR.
         IF AVAILABLE rowidlista THEN DO:
            THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE = rowidlista.NYORDNING.            
         END.   
         ELSE IF GuruUpDown = TRUE THEN DO:
            IF THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE = forsta - 1 THEN 
            THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE = sista.
         END.
         ELSE DO:
            IF THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE = sista + 1 THEN  
            THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE = forsta.
         END.  
         qH:GET-NEXT().
      END.   
      
      FIND FIRST rowidlista  WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE rowidlista THEN DO:
         GridGuruUpDown:GuruAvmarkera(). /*så bara nya poster markeras*/
         GridGuruUpDown:GuruFiltrera().  /* Öppnar Queryn så att de nya posterna syns */   
         GridGuruUpDown:GuruRadvaljare(TEMP-TABLE rowidlista:DEFAULT-BUFFER-HANDLE). /* väljer tillagda poster i GridGuruUpDown */

      END.
   END METHOD.
  
   METHOD PRIVATE LOGICAL Findskap(rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow, villkor AS CHARACTER):
      DEFINE VARIABLE rCells AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
      DEFINE VARIABLE ttr AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      rCells = rCurrentRow:Cells.      
      ttr = rCells["TTRECID"]:VALUE.
      IF villkor = "" THEN  GridGuruUpDown:Guruegenskap:ttBufferHandle:FIND-FIRST( "WHERE " + "TTRECID = " + ttr ) NO-ERROR.
      ELSE DO:
         GridGuruUpDown:Guruegenskap:ttBufferHandle:FIND-FIRST( "WHERE " + "TTRECID = " + ttr + " AND " + villkor) NO-ERROR.
         IF THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:AVAILABLE THEN.
         ELSE DO:
            RETURN FALSE.
         END.
      END.     
      CREATE rowidlista.
      ASSIGN 
      rowidlista.id = GridGuruUpDown:Guruegenskap:ttBufferHandle:ROWID  
      rowidlista.ORGORDNING = THIS-OBJECT:GridGuruUpDown:Guruegenskap:ttBufferHandle:BUFFER-FIELD(GuruUpDownField):BUFFER-VALUE.
      /*maxordning = maxordning + 1.*/
      RETURN TRUE.
   END METHOD.
	
	
	DESTRUCTOR PUBLIC ButtonUpDown ( ):

	END DESTRUCTOR.

END CLASS.