DEFINE VARIABLE hExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet AS COM-HANDLE NO-UNDO.
CREATE "Excel.Application" hExcel.
hExcel:VISIBLE = TRUE.
hWorkbook = hExcel:Workbooks:Add().
hWorkSheet = hExcel:Sheets:Item(1).
hExcel:DisplayAlerts=FALSE.
hWorkSheet:Range("C15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "Kärnprocess".
hWorkSheet:Range("C16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "72".
hWorkSheet:Range("C17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "72".
hWorkSheet:Range("C18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "73".
hWorkSheet:Range("C19"):Select.
hExcel:ActiveCell:FormulaR1C1 = "73".
hWorkSheet:Range("C20"):Select.
hExcel:ActiveCell:FormulaR1C1 = "73".

hWorkSheet:Range("D15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "Delprocess".
hWorkSheet:Range("D16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "del1".
hWorkSheet:Range("D17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "del2".
hWorkSheet:Range("D18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "del3".

hWorkSheet:Range("D19"):Select.
hExcel:ActiveCell:FormulaR1C1 = "del3".
hWorkSheet:Range("D20"):Select.
hExcel:ActiveCell:FormulaR1C1 = "del4".

hWorkSheet:Range("E15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "aonr".
hWorkSheet:Range("E16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "121212".
hWorkSheet:Range("E17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "131313".
hWorkSheet:Range("E18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "141414".
hWorkSheet:Range("E19"):Select.
hExcel:ActiveCell:FormulaR1C1 = "151515".
hWorkSheet:Range("E20"):Select.
hExcel:ActiveCell:FormulaR1C1 = "141414".

hWorkSheet:Range("F15"):Select.
hExcel:ActiveCell:FormulaR1C1 = "timmar".
hWorkSheet:Range("F16"):Select.
hExcel:ActiveCell:FormulaR1C1 = "55".
hWorkSheet:Range("F17"):Select.
hExcel:ActiveCell:FormulaR1C1 = "68".
hWorkSheet:Range("F18"):Select.
hExcel:ActiveCell:FormulaR1C1 = "89".
hWorkSheet:Range("F19"):Select.
hExcel:ActiveCell:FormulaR1C1 = "11".

hWorkSheet:Range("F20"):Select.
hExcel:ActiveCell:FormulaR1C1 = "12".

hWorkSheet:Range("C15:F20"):Select.
    hExcel:ActiveWorkbook:PivotCaches:Add(1,"Blad1!R15C3:R20C6"):CreatePivotTable("R1C1","Mytable").
    hExcel:ActiveSheet:PivotTables("Mytable"):SmallGrid = "False".
    hExcel:ActiveSheet:PivotTables("Mytable"):AddFields ("Kärnprocess","Delprocess","AONR").
    
/*    hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("Delprocess"):ORIENTATION = 4.
    hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("AONR"):ORIENTATION = 4.*/
    hExcel:ActiveSheet:PivotTables("Mytable"):PivotFields("timmar"):ORIENTATION = 4.
    
    
  /* hExcel:Charts:ADD().
    hExcel:ActiveChart:SetSourceData(hExcel:Worksheets("Blad1"):Cells(1,1)).
    hExcel:ActiveChart:Location(1).*/
    RELEASE OBJECT hWorksheet.
    RELEASE OBJECT hWorkbook.
    RELEASE OBJECT hExcel.
