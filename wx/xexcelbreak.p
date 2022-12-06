/*EXCMTRL.P
UTLÄSNING AV MTRL KOPPLAT TILL KONSTRUKTIONER TILL EXCEL*/   
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.   
DEFINE VARIABLE chCell                  AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE chPageBreak                  AS COM-HANDLE NO-UNDO.

   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 6.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "ENR".
   chWorkSheet:Range("B1"):Value = "BENÄMNING".
   chWorkSheet:Range("C1"):Value = "ENHET".
      
   iColumn = 2.
   OPEN QUERY mq FOR EACH MTRL WHERE MTRL.LEVKOD = "99" AND
   MTRL.KALKNR = 0 NO-LOCK BY MTRL.ENR.
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(MTRL):                     
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = MTRL.ENR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(MTRL.BENAMNING,1,40).
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = MTRL.ENHET.
      


/*   
ASSIGN 
chCell = chWorkSheet:Range("A" + STRING(iLastRowOnThisPage + 1)) 
chPageBreak = chWorkSheet:HPageBreaks:Add(chCell). 


With Worksheets(1)
    .HPageBreaks.Add .Range("F25")
    .VPageBreaks.Add .Range("F25")
End With
    
     chWorkSheet:HPageBreaks:ADD Before:Range("A2"). */
/*       chWorkSheet:HPageBreaks:Add():Before:ActiveCell. */
/*       ActiveWindow.SelectedSheets.HPageBreaks.Add Before:=ActiveCell */
/*                                                                      */
/*       Range("B1").Select                                             */
/*     ActiveWindow.SelectedSheets.VPageBreaks.Add Before:=ActiveCell   */
/*     Range("A2").Select                                               */
/*     ActiveWindow.SelectedSheets.HPageBreaks.Add Before:=ActiveCell   */


      GET NEXT mq NO-LOCK.
   END. 
   cColumn = STRING(20).
   cRange = "A" + cColumn.
   chCell = chWorkSheet:Range(cRange).
/*    chPageBreak = chWorkSheet:HPageBreaks:ADD(chCell). /* Page Break */ */


         chExcelApplication:ActiveWindow:SelectedSheets:HPageBreaks:Add(chCell). 
/* where:                                                                 */
/* chExcelApplication com-handle to your excell application               */
/* chBefore - com-handle - here you must have the RANGE where you want to */
/* instert the break line                                                 */
   CLOSE QUERY mq.   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.

