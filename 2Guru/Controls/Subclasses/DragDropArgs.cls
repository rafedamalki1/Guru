 
 /*------------------------------------------------------------------------
    File        : DragDropArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Mar 05 09:33:33 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.EventArgs.



CLASS Controls.Subclasses.DragDropArgs INHERITS EventArgs: 
   CONSTRUCTOR PUBLIC DragDropArgs(Gd AS Controls.Grid, Rs AS Infragistics.Win.UltraWinGrid.SelectedRowsCollection):
      THIS-OBJECT:Grid = Gd.
      THIS-OBJECT:Rows = Rs.
   END CONSTRUCTOR.
   
   DEFINE PROPERTY Grid AS Controls.Grid
      PUBLIC GET.
      PRIVATE SET.
      
   DEFINE PROPERTY Rows AS Infragistics.Win.UltraWinGrid.SelectedRowsCollection 
      PUBLIC GET.
      PRIVATE SET.
END CLASS.