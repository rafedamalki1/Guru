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
DEFINE VARIABLE cRangefont              AS CHARACTER.
DEFINE VARIABLE radnrS                 AS CHARACTER.
CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().

   chWorkSheet = chExcelApplication:Sheets:Item(1).  
   iColumn = 2.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = " blad 1".

   chWorkSheet = chExcelApplication:Sheets:Item(2).  
   iColumn = 2.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = " blad 2".

   chWorkSheet = chExcelApplication:Sheets:Item(3).  
   iColumn = 2.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = " blad 3".
   
   iColumn = 1.
   chWorkSheet = chExcelApplication:Sheets:ADD(2,,,).
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = "blad 4".
   chWorkSheet:Name = "SID 4".

   chWorkSheet = chExcelApplication:Sheets:Item(4).  
   iColumn = 3.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = " rad 24".

   

   iColumn = 1.
   chWorkSheet = chExcelApplication:Sheets:ADD().
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = "SIDAN 45".
   chWorkSheet:Name = "SID 45".   
   
   
   
