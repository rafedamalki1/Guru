/* 
 * This sample extracts data from a Progress database
 * and graphs the information using the Automation Objects
 * from the Excel server in Office 95/97.
 * You must connect to a sports database before running this.
 * This sample program leaves Excel open.  You should close it manually
 * when the program completes.
 */

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 18.
chWorkSheet:Columns("B"):ColumnWidth = 12.
chWorkSheet:Columns("C"):ColumnWidth = 12.
chWorkSheet:Range("A1:C1"):Font:Bold = TRUE.
chWorkSheet:Range("A1"):Value = "personaltab".
chWorkSheet:Range("B1"):Value = "Total Sales".
chWorkSheet:Range("C1"):Value = "Annual Quota".

/* Iterate through the personaltab table and populate
   the Worksheet appropriately */
FOR EACH personaltab:
    dAnnualQuota = 0.
    iTotalNumberOfOrders = 0.
    dTotalSalesAmount = 0.
    iColumn = iColumn + 1.
    FOR EACH tidregitab OF personaltab:
        iTotalNumberOfOrders = iTotalNumberOfOrders + 1.
        FIND aonrtab WHERE aonrtab.aonr = tidregitab.aonr NO-ERROR.
        IF AVAILABLE aonrtab THEN 
            dTotalSalesAmount = dTotalSalesAmount + 1.
    END.
    
    DO iMonth = 1 TO 12:
        dAnnualQuota = dAnnualQuota + 1.
    END.
    
    cColumn = STRING(iColumn).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = personaltab.fornamn.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = dTotalSalesAmount.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = dAnnualQuota.
END.

chWorkSheet:Range("B2:C10"):Select().
chExcelApplication:Selection:Style = "Currency".

/* create embedded chart using the data in the Worksheet */
chWorksheetRange = chWorksheet:Range("A1:C10").
chWorksheet:ChartObjects:Add(10,150,425,300):Activate.
chExcelApplication:ActiveChart:ChartWizard(chWorksheetRange, 3, 1, 2, 1, 1, TRUE,
    "1996 Sales Figures", "Sales Person", "Annual Sales").

/* create chart using the data in the Worksheet */
chChart=chExcelApplication:Charts:Add().
chChart:Name = "Test Chart".
chChart:Type = 11.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.
RELEASE OBJECT chChart.
RELEASE OBJECT chWorksheetRange. 

