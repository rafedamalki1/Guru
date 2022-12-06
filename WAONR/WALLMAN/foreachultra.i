/*foreachultra.i*/
   DEFINE VARIABLE rCells{5}      AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
   DEFINE VARIABLE ttr{5}         AS CHARACTER                                     NO-UNDO.
   DEFINE VARIABLE villkor{5}     AS CHARACTER                                     NO-UNDO.           
   DEFINE VARIABLE rCurrentRow{5} AS Infragistics.Win.UltraWinGrid.UltraGridRow    NO-UNDO.
    DEFINE VARIABLE {2}           AS {1}                            NO-UNDO . 
   DEFINE VARIABLE {2}Enumerator AS System.Collections.IEnumerator NO-UNDO . 
   ASSIGN 
   {2}Enumerator = {4}:GetEnumerator() .
   {2}Enumerator:Reset() .
      DO WHILE {2}Enumerator:MoveNext() ON ERROR UNDO, THROW:
         ASSIGN 
         {2} = CAST({2}Enumerator:Current, {1}) .  
         rCurrentRow{5} = CAST ({2}, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .             
         rCells{5} = rCurrentRow{5}:Cells.      
         ttr{5} = rCells{5}["TTRECID"]:VALUE.
         villkor{5} = "WHERE " + "TTRECID = " + ttr{5}.      
      