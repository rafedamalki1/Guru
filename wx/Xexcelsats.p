/*UTLÄSNING AV SATSREGISTER TILL EXCEL*/   
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

DEFINE VARIABLE nysats LIKE SATS.KOD NO-UNDO.
DEFINE BUFFER satsbuff FOR SATS.
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11.
   chWorkSheet:Columns("B"):ColumnWidth = 30.
   chWorkSheet:Columns("C"):ColumnWidth = 11.
   chWorkSheet:Columns("D"):ColumnWidth = 30.
   chWorkSheet:Columns("E"):ColumnWidth = 3.
   chWorkSheet:Columns("F"):ColumnWidth = 5.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "KOD".
   chWorkSheet:Range("B1"):Value = "BENÄMNING".
   chWorkSheet:Range("C1"):Value = "E-NUMMER".
   chWorkSheet:Range("D1"):Value = "BENÄMNING".
   chWorkSheet:Range("E1"):Value = "ENH".
   chWorkSheet:Range("F1"):Value = "ANTAL".    
   
   nysats = "".
   OPEN QUERY satsq FOR EACH SATS WHERE SATS = FALSE NO-LOCK.
   GET FIRST satsq NO-LOCK.
   DO WHILE AVAILABLE(SATS): 
      FIND FIRST satsbuff WHERE satsbuff.KOD = SATS.KOD AND satsbuff.SATS = TRUE
      NO-LOCK NO-ERROR. 
      IF nysats NE SATS.KOD THEN       
      iColumn = iColumn + 2.
      ELSE iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = SATS.KOD.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(satsbuff.BENAMNING,1,30).
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = SATS.ENR2.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(SATS.BENAMNING2,1,30).
      cRange = "E" + cColumn.
      chWorkSheet:Range(cRange):Value = SATS.ENHET2.
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Value = DECIMAL(STRING(SATS.ANTAL,">>>>9")). 
      nysats = SATS.KOD.                  
      GET NEXT satsq NO-LOCK.
   END.  
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet.
