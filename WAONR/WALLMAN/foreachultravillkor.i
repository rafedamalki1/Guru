
/*------------------------------------------------------------------------
    File        : foreachultravillkor.i
    Purpose     : slippa köra definiera variabler osv för foreach.i varje gång man ska göra något med selected rows i ultragrid.
                     + custom villkor

    Syntax      :

    Description : for each selected row in ultragrid 

    Author(s)   : elpkl
    Created     : Thu Nov 17 15:36:17 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE rCells      AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
DEFINE VARIABLE ttr         AS CHARACTER                                     NO-UNDO.
DEFINE VARIABLE villkor     AS CHARACTER                                     NO-UNDO.           
DEFINE VARIABLE rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow    NO-UNDO.

DEFINE VARIABLE {2}           AS {1}                            NO-UNDO . 
DEFINE VARIABLE {2}Enumerator AS System.Collections.IEnumerator NO-UNDO . 
    
ASSIGN 
   {2}Enumerator = {4}:GetEnumerator() .
    
   {2}Enumerator:Reset() .
    
   DO WHILE {2}Enumerator:MoveNext() ON ERROR UNDO, THROW:
      ASSIGN 
      {2} = CAST({2}Enumerator:Current, {1}) .     
      rCurrentRow = CAST ({2}, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .             
      rCells = rCurrentRow:Cells.      
      ttr = rCells["TTRECID"]:VALUE.
      villkor = "WHERE " + "TTRECID = " + ttr + " " + {5}.      
