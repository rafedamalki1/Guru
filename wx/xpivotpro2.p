DEFINE VARIABLE hExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE varforetypchar AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE plusaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE plusdnr AS INTEGER NO-UNDO.

DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
   



DEFINE new  SHARED VARIABLE dlcvar AS CHARACTER NO-UNDO.
DEFINE new  SHARED VARIABLE guruvar AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE karnproc NO-UNDO   
   FIELD karnproc AS CHARACTER 
   FIELD delproc AS CHARACTER
   FIELD aonr AS CHARACTER
   FIELD delnr AS INTEGER
   INDEX karn karnproc delproc aonr delnr.   
   
    
/*DEFINE VARIABLE vvv AS HANDLE NO-UNDO.*/

{EXECLIN2.I}

  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del1"
  karnproc.aonr = "38111"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del1"
  karnproc.aonr = "38112"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del1"
  karnproc.aonr = "38113"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del1"
  karnproc.aonr = "38114"
  karnproc.delnr = 0.
/*  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del2"
  karnproc.aonr = "43111"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del2"
  karnproc.aonr = "43112"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "76"
  karnproc.delproc = "del2"
  karnproc.aonr = "43113"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "72"
  karnproc.delproc = "del3"
  karnproc.aonr = "65111"
  karnproc.delnr = 0.
  CREATE karnproc.
  ASSIGN
  karnproc.karnproc = "72"
  karnproc.delproc = "del3"
  karnproc.aonr = "65111"
  karnproc.delnr = 0.*/



CREATE "Excel.Application" hExcel.
hExcel:VISIBLE = TRUE.
hWorkbook = hExcel:Workbooks:Add().
hWorkSheet = hExcel:Sheets:Item(1).
/*hExcel:DisplayAlerts=FALSE.*/
chWorkSheet:Columns("A"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("B"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("C"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("D"):ColumnWidth = 5 NO-ERROR.
  

chWorkSheet:Range("A1:P1"):Font:Bold = TRUE NO-ERROR.

 

chWorkSheet:Range("A:A"):NumberFormat = "@" NO-ERROR.
chWorkSheet:Range("B:B"):NumberFormat = "@" NO-ERROR.
chWorkSheet:Range("C:C"):NumberFormat = "@" NO-ERROR.
chWorkSheet:Range("D:D"):NumberFormat = "@" NO-ERROR.

iColumn  = 15.

cColumn = STRING(iColumn).            
cRange = "A" + cColumn.   
hWorkSheet:Range(cRange):Select.
hExcel:ActiveCell:FormulaR1C1 = "kärnprocess" NO-ERROR.   
cRange = "B" + cColumn.
hWorkSheet:Range(cRange):Select.
hExcel:ActiveCell:FormulaR1C1 = "delprocess" NO-ERROR.  
cRange = "C" + cColumn.
hWorkSheet:Range(cRange):Select.
hExcel:ActiveCell:FormulaR1C1 = "aonr" NO-ERROR.  
cRange = "D" + cColumn.
hWorkSheet:Range(cRange):Select.
hExcel:ActiveCell:FormulaR1C1 = "delnr" NO-ERROR.

OPEN QUERY satsq FOR EACH karnproc  NO-LOCK USE-INDEX karn.
GET FIRST satsq NO-LOCK.
DO WHILE AVAILABLE(karnproc): 
   iColumn = iColumn + 1.
   cColumn = STRING(iColumn).            
   cRange = "A" + cColumn.   
   hWorkSheet:Range(cRange):Select.
   hExcel:ActiveCell:FormulaR1C1 = karnproc.karnproc NO-ERROR.   
   cRange = "B" + cColumn.
   hWorkSheet:Range(cRange):Select.
   hExcel:ActiveCell:FormulaR1C1 = karnproc.delproc NO-ERROR.      
   cRange = "C" + cColumn.
   hWorkSheet:Range(cRange):Select.
   hExcel:ActiveCell:FormulaR1C1 = karnproc.aonr NO-ERROR.      
   cRange = "D" + cColumn.
   hWorkSheet:Range(cRange):Select.
   hExcel:ActiveCell:FormulaR1C1 = karnproc.delnr NO-ERROR.
               
   GET NEXT satsq NO-LOCK.
END.     
CLOSE QUERY satsq.


hWorkSheet:Range("A16:D" + string(icolumn)):Select.
    hExcel:ActiveWorkbook:PivotCaches:Add(1,"Blad1!R16C1:R" + string(icolumn) + "C4"):CreatePivotTable("R1C1","Mytable").
    hExcel:ActiveSheet:PivotTables("Mytable"):SmallGrid = "False".
/*    hExcel:ActiveSheet:PivotTables("Mytable"):AddFields ("kärnprocess","delprocess","AONR").*/
    hExcel:ActiveSheet:PivotTables("Mytable"):AddFields ("72","DEL3","65111").
    hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("DELNR"):ORIENTATION = 4.
/*     hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("kärnprocess"):POSITION = 1.
     hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("kärnprocess"):LayoutCompactRow = False.

     hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("delprocess"):ORIENTATION = 9.
     hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("delprocess"):POSITION = 1.
     hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("delprocess"):LayoutCompactRow = False.*/
  
      

    /*hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("8888"):ORIENTATION = 4.*/
/*    hExcel:Charts:ADD().
    hExcel:ActiveChart:SetSourceData(hExcel:Worksheets("Blad1"):Cells(1,1)).
    hExcel:ActiveChart:Location(1).*/
    RELEASE OBJECT hWorksheet.
    RELEASE OBJECT hWorkbook.
    RELEASE OBJECT hExcel.
