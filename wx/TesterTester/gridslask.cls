/*------------------------------------------------------------------------
    File        : gridslask
    Purpose     :  Urklipp från Grid.cls - Metoder som inte används m.m 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Wen Jun 19 10:47:12 CET 2013
    Notes       : 
  ----------------------------------------------------------------------
  
  /* Markera cell baserat på ROWID column */
   METHOD PUBLIC VOID GuruCellValjare(INPUT grid1 AS Controls.Grid):
     /*
      IF rid NE ? THEN DO:
         tqH:REPOSITION-TO-ROWID (rid) NO-ERROR.
         grid1:FocusedRowHandle = tBS:POSITION.
         THIS-OBJECT:FocusedColumn = THIS-OBJECT:VisibleColumns[2].
      END.
      */
       
   END METHOD.
         
     /*om man dubbelklickar*/
      THIS-OBJECT:DoubleClickRow:Subscribe(THIS-OBJECT:doubleClickRow).  
   
     METHOD PRIVATE VOID doubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):  
      /*
      DEFINE VARIABLE gridrowid AS ROWID NO-UNDO.
      
      gridrowid = Guruegenskap:ttBufferHandle:ROWID.     

      MESSAGE     
      string(gridrowid)
      Guruegenskap:ttBufferHandle:BUFFER-FIELD(3):NAME
      Guruegenskap:ttBufferHandle:BUFFER-FIELD(3):BUFFER-VALUE
      VIEW-AS ALERT-BOX.
      */    
   END METHOD.
   
   THIS-OBJECT:BeforeRowUpdate:Subscribe(gridBeforeRowUpdate).
   
   
   METHOD PRIVATE VOID gridBeforeRowUpdate(rSender AS System.Object, rArgs AS Infragistics.Win.UltraWinGrid.CancelableRowEventArgs):
      DEFINE VARIABLE lResult AS LOGICAL.
      /* anders varför
      IF tBS:RowModified THEN DO:
         /*FINNS POSTEN I BUFFERTEN*/
         IF THIS-OBJECT:Guruegenskap:ttBufferHandle:AVAILABLE THEN DO:
            lResult = tBS:Assign().
            /*
            Guruegenskap:ttBufferHandle:BUFFER-FIELD("andrad"):BUFFER-VALUE = TRUE.
            */         
            IF NOT lResult THEN DO:
               rArgs:Cancel = TRUE.
               LEAVE.
            END.
            THIS-OBJECT:GuruReopen(). /* Blir detta rätt positionering osv?*/
         END.   
      END.
      */      
   END METHOD.  
   
   */
   
   