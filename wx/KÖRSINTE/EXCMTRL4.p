/*EXCMTRL4.P*/
/*UTLÄSNING AV MTRL KOPPLAT TILL KONSTRUKTIONER TILL EXCEL*/   
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
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
DEFINE SHARED VARIABLE appcon           AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE apphand          AS HANDLE NO-UNDO. 

DEFINE NEW SHARED TEMP-TABLE mtrl_temp 
   {MTRLTEMPTT.I}

DEFINE INPUT PARAMETER levvar AS LOGICAL.
DEFINE INPUT PARAMETER ersatte AS LOGICAL.
DEFINE INPUT PARAMETER lkod AS CHARACTER.

/*{EGENBEN.I}*/   

   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE NO-ERROR.
   chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
   chWorkSheet:Columns("A"):ColumnWidth = 11 NO-ERROR.
   chWorkSheet:Columns("B"):ColumnWidth = 40 NO-ERROR.
   chWorkSheet:Columns("C"):ColumnWidth = 6 NO-ERROR.
   chWorkSheet:Columns("D"):ColumnWidth = 10 NO-ERROR.
   chWorkSheet:Columns("E"):ColumnWidth = 10 NO-ERROR.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE NO-ERROR.

   chWorkSheet:Range("A1"):Value = Guru.Konstanter:genk NO-ERROR.
   chWorkSheet:Range("B1"):Value = "BENÄMNING" NO-ERROR.
   chWorkSheet:Range("C1"):Value = "ENHET" NO-ERROR.
   chWorkSheet:Range("D1"):Value = "PRIS" NO-ERROR.
   {EXCELFEL.I}    
   IF levvar = TRUE THEN DO:
      chWorkSheet:Range("E1"):Value = "LEVERANTÖR" NO-ERROR.
   END.
   chWorkSheet:Range("A:A"):NumberFormat = "@" NO-ERROR.
   EMPTY TEMP-TABLE mtrl_temp NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN SPECSOL3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT levvar,INPUT ersatte,INPUT lkod, OUTPUT TABLE mtrl_temp).
   END.
   ELSE DO:
      RUN SPECSOL3.P
      (INPUT levvar,INPUT ersatte,INPUT lkod, OUTPUT TABLE mtrl_temp).
   END.    
   /*IF Guru.Konstanter:appcon THEN DO:                           
      RUN SPECSOL2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT levvar, OUTPUT TABLE mtrl_temp).
   END.
   ELSE DO:
      RUN SPECSOL2.P
      (INPUT levvar, OUTPUT TABLE mtrl_temp).
   END.      */
   iColumn = 2.
   FOR EACH mtrl_temp USE-INDEX ENR:
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn) NO-ERROR.
      cRange = "A" + cColumn.
      /*
      chWorkSheet:Range(cRange):Value = mtrl_temp.ENR + "                        .".
      
      <com-handle>:Value [ = <anytype>-Var ].
      [ <anytype>-Var = ] <com-handle>: AutoFormat ( 
	  Integer-Format,
	  <anytype>-Number,
	  <anytype>-Font,
	  <anytype>-Alignment,
	  <anytype>-Border,
	  <anytype>-Pattern,
	  <anytype>-Width ).
     */
      chWorkSheet:Range(cRange):Value = mtrl_temp.ENR NO-ERROR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(mtrl_temp.BENAMNING,1,40) NO-ERROR.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = mtrl_temp.ENHET NO-ERROR.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = mtrl_temp.PRIS NO-ERROR.
      IF levvar = TRUE THEN DO:
         cRange = "E" + cColumn.
         chWorkSheet:Range(cRange):Value = mtrl_temp.FORNR NO-ERROR.
      END.
      {EXCELFEL.I}    
   END.   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
   {EXCELFEL.I}    
