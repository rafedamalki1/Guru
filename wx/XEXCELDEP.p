/*UTLÄSNING AV MTRL KOPPLAT TILL KONSTRUKTIONER TILL EXCEL*/   
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

DEFINE TEMP-TABLE e_temp
   FIELD ENR LIKE MTRLDEP.ENR
   FIELD BENAMNING LIKE MTRLDEP.BENAMNING
   FIELD ENHET LIKE MTRLDEP.ENHET
   INDEX ENR ENR ASCENDING.
      
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 6.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "E-NUMMER".
   chWorkSheet:Range("B1"):Value = "BENÄMNING".
   chWorkSheet:Range("C1"):Value = "ENHET".
     
   iColumn = 2.
   cColumn = STRING(iColumn).
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Value = "STÖRNINGSDEPÅ".
   iColumn = iColumn + 2.
   OPEN QUERY KQ FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = 22 
   AND MTRLDEP.IBDATUM = ? AND MTRLDEP.LAGER = TRUE USE-INDEX ENR NO-LOCK. 
   GET FIRST KQ NO-LOCK.
   DO WHILE AVAILABLE(MTRLDEP):
      IF MTRLDEP.FACKID = "" THEN DO:
         iColumn = iColumn + 1.
         cColumn = STRING(iColumn).
         cRange = "A" + cColumn.
         chWorkSheet:Range(cRange):Value = MTRLDEP.ENR.
         cRange = "B" + cColumn.
         chWorkSheet:Range(cRange):Value = SUBSTRING(MTRLDEP.BENAMNING,1,40).
         cRange = "C" + cColumn.
         chWorkSheet:Range(cRange):Value = MTRLDEP.ENHET.
      END.
      ELSE DO:
         CREATE e_temp.
         ASSIGN
         e_temp.ENR = MTRLDEP.ENR
         e_temp.BENAMNING = MTRLDEP.BENAMNING
         e_temp.ENHET = MTRLDEP.ENHET.
      END.   
      GET NEXT KQ NO-LOCK.
   END.  
   iColumn = iColumn + 3.
   cColumn = STRING(iColumn).
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Value = "ÖVRIGT".
   iColumn = iColumn + 2.
   FOR EACH e_temp USE-INDEX ENR: 
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.ENR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(e_temp.BENAMNING,1,40).
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.ENHET.
   END.      
   CLOSE QUERY KQ.
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet.
