/*xexcellena.p*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED



{GLOBVAR.I}
{EXECLIN2.I}
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE efarg AS INTEGER NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE mtrl_temp 
   {MTRLTEMPTT.I}
   CREATE mtrl_temp.
   ASSIGN 
   mtrl_temp.ENR = "121212"
   mtrl_temp.LEVKOD = "13"
   mtrl_temp.ENHET = "FISK"
   mtrl_temp.BENAMNING = "BRA TEXT".

CREATE mtrl_temp.
   ASSIGN 
   mtrl_temp.ENR = "121212"
   mtrl_temp.LEVKOD = "13"
   mtrl_temp.ENHET = "FISK"
   mtrl_temp.BENAMNING = "BRA TEXT".
   CREATE mtrl_temp.
   ASSIGN 
   mtrl_temp.ENR = "121212"
   mtrl_temp.LEVKOD = "13"
   mtrl_temp.ENHET = "FISK"
   mtrl_temp.BENAMNING = "BRA TEXT".
CREATE mtrl_temp.
   ASSIGN 
   mtrl_temp.ENR = "121212"
   mtrl_temp.LEVKOD = "13"
     mtrl_temp.ENHET = "FISK"
   mtrl_temp.BENAMNING = "BRA TEXT FISK".

 
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11 NO-ERROR.
   chWorkSheet:Columns("B"):ColumnWidth = 40 NO-ERROR.
   chWorkSheet:Columns("C"):ColumnWidth = 8 NO-ERROR.
   chWorkSheet:Columns("D"):ColumnWidth = 10 NO-ERROR.
   

   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE NO-ERROR.

   chWorkSheet:Range("A1"):Value = "enr" NO-ERROR.
   chWorkSheet:Range("B1"):Value = "BENÄMNING" NO-ERROR.
   chWorkSheet:Range("C1"):Value = "ENHET" NO-ERROR.
   chWorkSheet:Range("D1"):Value = "LEVKOD" NO-ERROR.
   chWorkSheet:Range("A:A"):NumberFormat = "@" NO-ERROR.
   
   iColumn = 2.
   FOR EACH mtrl_temp USE-INDEX ENR:
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = mtrl_temp.ENR NO-ERROR.      
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(mtrl_temp.BENAMNING,1,40) NO-ERROR.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = mtrl_temp.ENHET NO-ERROR.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = mtrl_temp.LEVKOD NO-ERROR.
   END.
/*   run sokiexcel_UI (input "FISK",input "A1",INPUT "D6"*/
   
   
