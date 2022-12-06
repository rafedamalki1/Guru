DEFINE VARIABLE hExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet AS COM-HANDLE NO-UNDO.
CREATE "Excel.Application" hExcel.
hExcel:VISIBLE = TRUE.
hWorkbook = hExcel:Workbooks:Add().
hWorkSheet = hExcel:Sheets:Item(1).
hExcel:DisplayAlerts=FALSE.
hWorkSheet:Range("D15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "33333".
hWorkSheet:Range("D16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "4444".
hWorkSheet:Range("D17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "666".
hWorkSheet:Range("D18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "77777".
hWorkSheet:Range("E15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "8888".
hWorkSheet:Range("E16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "888".
hWorkSheet:Range("E17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "888".
hWorkSheet:Range("E18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "888".
hWorkSheet:Range("C15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "fr".
hWorkSheet:Range("C16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "dsss".
hWorkSheet:Range("C17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "vvvv".
hWorkSheet:Range("C18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "ffff".
hWorkSheet:Range("C15:E18"):Select.
    hExcel:ActiveWorkbook:PivotCaches:Add(1,"Blad1!R15C3:R18C5"):CreatePivotTable("R1C1","Mytable").
    hExcel:ActiveSheet:PivotTables("Mytable"):SmallGrid = "False".
    hExcel:ActiveSheet:PivotTables("Mytable"):AddFields ("fr","33333").
    hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("8888"):ORIENTATION = 4.
   /* hExcel:Charts:ADD().
    hExcel:ActiveChart:SetSourceData(hExcel:Worksheets("Blad1"):Cells(1,1)).
    hExcel:ActiveChart:Location(1).*/
    RELEASE OBJECT hWorksheet.
    RELEASE OBJECT hWorkbook.
    RELEASE OBJECT hExcel.
