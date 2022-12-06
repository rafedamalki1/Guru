/*XEXCELBLAD.P*/   
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE chActiveSheet           AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.   
DEFINE VARIABLE chCell                  AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE chPageBreak             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE listavar AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt
FIELD ETT AS CHARACTER FORMAT "X(32)"
FIELD TVA AS CHARACTER FORMAT "X(32)"
FIELD TRE AS CHARACTER FORMAT "X(32)".

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = TRUE.
chWorkbook = chExcelApplication:Workbooks:Add().
chWorkSheet = chExcelApplication:Sheets:Item(1).
/* chWorkSheet:PageSetup:CenterHeader = "Elpool i Umeå AB". */
/* chWorkSheet:PageSetup:RightHeader = "&P".                */

chWorkSheet:PageSetup:LeftFooter = "Genererad av GURU från Elpool i Umeå AB". 
chWorkSheet:PageSetup:RightFooter = "Skapad: " + STRING(TODAY) + " " + STRING(TIME,"HH:MM"). 
listavar = 1.

IF listavar = 1 THEN DO:
   chWorkSheet:Columns("A"):ColumnWidth = 16.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 6.
   chWorkSheet:Columns("D"):ColumnWidth = 10.
   chWorkSheet:Columns("E"):ColumnWidth = 10.
   chWorkSheet:Columns("F"):ColumnWidth = 10.
   
   iColumn = 1.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn + ":" + "D" + cColumn + STRING(9).
   chCell = chWorkSheet:Range(cRange).
/*    chCell:Interior:ColorIndex = 45.  /*Ljusorange*/ */
/*    chCell:Interior:ColorIndex = 34. /*Pastellblå*/ */
/*    chCell:Interior:ColorIndex = 15. /*Grå 25%*/ */
/*    chCell:Interior:ColorIndex = 40. /*Aprikos*/ */
/*    chCell:Interior:ColorIndex = 37. /*Ljusblå*/ */
/*    chCell:Interior:ColorIndex = 36. /*Ljusgul*/ */
/*    chCell:Interior:ColorIndex = 35. /*Pastellgrön*/ */

   RUN skaparam_UI.
/*    chWorkSheet:Range(cRange):Borders(1):Weight = 2. /*Vertikalt alla kolumner innanför markering*/ */
/*    chWorkSheet:Range(cRange):Borders(2):Weight = 2. /*Vertikalt höger om kolumn*/ */
/*    chWorkSheet:Range(cRange):Borders(3):Weight = 2. /*Horizontellt alla rader innanför markering*/ */
/*    chWorkSheet:Range(cRange):Borders(4):Weight = 2. /*Horizontellt nedan kolumn*/ */
/*    chWorkSheet:Range(cRange):Borders(7):Weight = 2. /*Vertikalt vänster om markering*/ */
/*    chWorkSheet:Range(cRange):Borders(8):Weight = 2. /*Horizontellt ovanför markering*/ */
/*    chWorkSheet:Range(cRange):Borders(9):Weight = 2. /*Horizontellt nedan markering*/ */
/*    chWorkSheet:Range(cRange):Borders(10):Weight = 2. /*Vertikalt höger om markering*/ */
/*    chWorkSheet:Range(cRange):Borders(11):Weight = 2. /*Vertikalt alla kolumner innanför markering*/ */


PROCEDURE skaparam_UI:
      chWorkSheet:Range(cRange):Borders(7):Weight = 2. /*Vertikalt vänster om markering*/
      chWorkSheet:Range(cRange):Borders(8):Weight = 2. /*Horizontellt ovanför markering*/
      chWorkSheet:Range(cRange):Borders(9):Weight = 2. /*Horizontellt nedan markering*/
      chWorkSheet:Range(cRange):Borders(10):Weight = 2. /*Vertikalt höger om markering*/
END PROCEDURE.
      
        
   /*    chWorkSheet:Range("A1:D1"):Font:Bold = TRUE. */
   

   chWorkSheet:Range("A1"):Value = "Test".
   chWorkSheet:Range("B1"):Value = "Test".
   chWorkSheet:Range("C1"):Value = "Test".
   EMPTY TEMP-TABLE tt NO-ERROR. 
   CREATE tt.
   ASSIGN
   tt.ETT = "Hopp"
   tt.TVA = "Kopp"
   tt.TRE = "Lopp".
   
   iColumn = 2.
   OPEN QUERY qtt FOR EACH tt.
   GET FIRST qtt NO-LOCK.
   DO WHILE AVAILABLE(tt):                     
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = tt.ETT.
      chCell = chWorkSheet:Range(cRange).
      chCell:Interior:ColorIndex = 45.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = tt.TVA.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = tt.TRE.
      GET NEXT qtt NO-LOCK.
   END. 
   CLOSE QUERY qtt.
   
   chWorkSheet:Range(cRange):clearcomments().
   DEFINE VARIABLE un AS CHARACTER NO-UNDO.
   un = chExcelApplication:UserName.
   chExcelApplication:UserName = "Lena".
   chWorkSheet:Range(cRange):addcomment("Det här är en kommentar").
   chExcelApplication:UserName = un.
   
   

   /* chWorkSheet = chExcelApplication:Sheets:ADD(). */
   /* chWorkSheet:MOVE(,chWorkbook:Sheets(4)).       */
   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.



END.


/*
Open Excel      
  CREATE "Excel.Application" vchExcel.  n/a

Show/Hide Excel 
  vchExcel:VISIBLE = [TRUE/FALSE]       Y

Adding a Workbook 
  vchWorkBook = vchExcel:WorkBooks:ADD()        Y

Selecting a Sheet 
  vchWorkSheet = vchExcel:Sheets:ITEM(1)        Y

Adding data to a cell 
  vchWorkSheet:Range("A1"):VALUE = 123
  vchWorkSheet:Range("A1"):VALUE = "this is a test"     Y

Bold on/off 
  vchWorkSheet:Range("A1"):FONT:Bold = [TRUE/FALSE]     Y

Merge cells 
  vchWorkSheet:Range("A1:I1"):Merge     N

Left align 
  vchWorkSheet:Range("A1"):HorizontalAlignment = 3      Y

Right align 
  vchWorkSheet:Range("A1"):HorizontalAlignment = 4      Y

Top align 
  vchWorkSheet:Range("A1"):VerticalAlignment = 1        Y

Auto size column 
  vchWorkSheet:Range("A1"):EntireColumn:AutoFit         N

Auto size row 
  vchWorkSheet:Range("A1"):EntireRow:AutoFit    N

Set column width 
  vchWorkSheet:Range("A1"):ColumnWidth = 90     Y

Number format 
  vchWorkSheet:Range("A1"):NumberFormat =
"###,###,##0.00;[Red]-###,###,##0.00"
  You can change the number of decimals places by adding more 0's, 
  use a comma or not, and add more #'s for a bigger number.     Y

Save As 
  vchWorkBook:SaveAs("c:\tmp\ttst.xls",,,,,,,)
  There are 7 comma's and I've always had to use them.*/
