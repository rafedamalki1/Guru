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

   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 20.
   chWorkSheet:Columns("D"):ColumnWidth = 20.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "ANVANDARE".
   chWorkSheet:Range("B1"):Value = "NAMN".
   chWorkSheet:Range("C1"):Value = "ANTAL BEREDNINGAR".
   chWorkSheet:Range("D1"):Value = "ANTAL KALKYLER".
      
   iColumn = 2.
   OPEN QUERY oq FOR EACH OMRADETAB USE-INDEX OMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(OMRADETAB):      
      iColumn = iColumn  + 2.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = OMRADETAB.OMRADE + " " + OMRADETAB.NAMN.      
      OPEN QUERY aq FOR EACH PERSONALTAB WHERE PERSONALTAB.OMRADE = OMRADETAB.OMRADE
      USE-INDEX OMR NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(PERSONALTAB):
         FOR EACH BEREDNING WHERE BEREDNING.ANVANDARE = 
         PERSONALTAB.PERSONALKOD NO-LOCK.         
            ACCUMULATE BEREDNING.BERNR (COUNT).
         END.
         FOR EACH FASTSPEC WHERE FASTSPEC.ANVANDARE = 
         PERSONALTAB.PERSONALKOD NO-LOCK.         
            ACCUMULATE FASTSPEC.KALKNR (COUNT).
         END.
         iColumn = iColumn + 1.
         cColumn = STRING(iColumn).
         cRange = "A" + cColumn.
         chWorkSheet:Range(cRange):Value = PERSONALTAB.PERSONALKOD.
         cRange = "B" + cColumn.
         chWorkSheet:Range(cRange):Value = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         cRange = "C" + cColumn.
         chWorkSheet:Range(cRange):Value = ACCUM COUNT BEREDNING.BERNR.
         cRange = "D" + cColumn.
         chWorkSheet:Range(cRange):Value = ACCUM COUNT FASTSPEC.KALKNR.
         GET NEXT aq NO-LOCK.
      END.
      CLOSE QUERY aq.
      GET NEXT oq NO-LOCK.
   END.
   CLOSE QUERY oq.
    
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
