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
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
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
   
   nysats = "".
   OPEN QUERY satsq FOR EACH MTRLBER WHERE MTRLBER.LEVKOD = "1" NO-LOCK.
   GET FIRST satsq NO-LOCK.
   DO WHILE AVAILABLE(MTRLBER): 
      FIND FIRST e_temp WHERE e_temp.ENR = MTRLBER.ENR NO-LOCK NO-ERROR. 
      IF AVAILABLE e_temp THEN DO:
         nysats = nysats.
      END.   
      ELSE DO:   
         CREATE e_temp.
         ASSIGN
         e_temp.ENR = MTRLBER.ENR
         e_temp.BENAMNING = MTRLBER.BENAMNING
         e_temp.ENHET = MTRLBER.ENHET.                     
      END.
      GET NEXT satsq NO-LOCK.
   END.  
   OPEN QUERY kq FOR EACH BERSKAP WHERE BERSKAP.LEVKOD = "1" AND
   BERSKAP.ENR NE "" NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(BERSKAP): 
      FIND FIRST e_temp WHERE e_temp.ENR = BERSKAP.ENR NO-LOCK NO-ERROR. 
      IF AVAILABLE e_temp THEN DO:
         nysats = nysats.
      END.   
      ELSE DO:   
         CREATE e_temp.
         ASSIGN
         e_temp.ENR = BERSKAP.ENR
         e_temp.BENAMNING = BERSKAP.BENAMNING
         e_temp.ENHET = BERSKAP.ENHET.                     
      END.
      GET NEXT kq NO-LOCK.
   END.
   OPEN QUERY kq2 FOR EACH BERSTOLP WHERE BERSTOLP.LEVKOD = "1" NO-LOCK.
   GET FIRST kq2 NO-LOCK.
   DO WHILE AVAILABLE(BERSTOLP): 
      FIND FIRST e_temp WHERE e_temp.ENR = BERSTOLP.ENR NO-LOCK NO-ERROR. 
      IF AVAILABLE e_temp THEN DO:
         nysats = nysats.
      END.   
      ELSE DO:   
         CREATE e_temp.
         ASSIGN
         e_temp.ENR = BERSTOLP.ENR
         e_temp.BENAMNING = BERSTOLP.BENAMNING
         e_temp.ENHET = BERSTOLP.ENHET.                     
      END.
      GET NEXT kq2 NO-LOCK.
   END.
   iColumn = 2.
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
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet.
