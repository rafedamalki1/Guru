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

DEFINE VARIABLE nysats LIKE MTRLBER.ENR NO-UNDO.

DEFINE TEMP-TABLE e_temp
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD DATUM AS CHARACTER
   FIELD TXT AS CHARACTER   
   INDEX AONR IS PRIMARY AONR DELNR.
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 7.
   chWorkSheet:Columns("B"):ColumnWidth = 6.
   chWorkSheet:Columns("C"):ColumnWidth = 10.
   chWorkSheet:Columns("D"):ColumnWidth = 20.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "AONR".
   chWorkSheet:Range("B1"):Value = "DELNR".
   chWorkSheet:Range("C1"):Value = "SISTA TID".
   chWorkSheet:Range("D1"):Value = "Kommentar".
   
   nysats = "".
   OPEN QUERY satsq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 NO-LOCK.
   GET FIRST satsq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB): 
      FIND LAST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND
      TIDREGITAB.DELNR = AONRTAB.DELNR USE-INDEX AONR
      NO-LOCK NO-ERROR. 
      CREATE e_temp.
      ASSIGN
      e_temp.AONR = AONRTAB.AONR
      e_temp.DELNR = AONRTAB.DELNR.
      IF AVAILABLE TIDREGITAB THEN DO:
         e_temp.DATUM = STRING(TIDREGITAB.DATUM,"99/99/99").
      END.   
      ELSE DO:   
         e_temp.TXT = "Tid saknas".                  
      END.
      GET NEXT satsq NO-LOCK.
   END.  
   iColumn = 2.
   FOR EACH e_temp USE-INDEX AONR:
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.AONR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.DELNR.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.DATUM.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = e_temp.TXT.
   END.   
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet.
