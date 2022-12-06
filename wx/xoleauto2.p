/* 
 * This sample extracts data from a Progress database
 * and graphs the information using the Automation Objects
 * from the Excel server in Office 95/97.
 * You must connect to a sports database before running this.
 * This sample program leaves Excel open.  You should close it manually
 * when the program completes.
 */
DEFINE VARIABLE titelvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE slut_allt
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTALHSP AS INTEGER
   FIELD ANTALLSP AS INTEGER.
DEFINE TEMP-TABLE slut_allt2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut_allt3
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 1
 slut_allt2.ANTAL = 2425144.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 2
 slut_allt2.ANTAL = 708646.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 3
 slut_allt2.ANTAL = 1107587.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 4
 slut_allt2.ANTAL = 797118.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 5
 slut_allt2.ANTAL = 1383293.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 6
 slut_allt2.ANTAL = 195457.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 7
 slut_allt2.ANTAL = 401588.
 CREATE slut_allt2.
 ASSIGN
 slut_allt2.FELOID = 8
 slut_allt2.ANTAL = 144562.
titelvar = "Antal kunder varaktighetsfördelning ".
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
/*
/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 18.
chWorkSheet:Columns("B"):ColumnWidth = 12.
chWorkSheet:Columns("C"):ColumnWidth = 12.
chWorkSheet:Range("A1:C1"):Font:Bold = TRUE.
chWorkSheet:Range("A1"):Value = "personaltab".
chWorkSheet:Range("B1"):Value = "Total Sales".
chWorkSheet:Range("C1"):Value = "Annual Quota".
*/
/* Iterate through the personaltab table and populate
   the Worksheet appropriately */
chWorkSheet:Range("B1"):Value = "ÅRTAL".
FOR EACH slut_allt2 BY slut_allt2.FELOID:
    
    IF slut_allt2.FELOID = 1 THEN DO:
       cRange = "B2".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A2".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 2 THEN DO:
       cRange = "B3".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A3".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 3 THEN DO:
       cRange = "B4".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A4".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 4 THEN DO:
       cRange = "B5".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A5".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 5 THEN DO:
       cRange = "B6".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A6".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 6 THEN DO:
       cRange = "B7".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A7".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 7 THEN DO:
       cRange = "B8".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A8".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    IF slut_allt2.FELOID = 8 THEN DO:
       cRange = "B9".
       chWorkSheet:Range(cRange):Value = slut_allt2.ANTAL.
       cRange = "A9".
       chWorkSheet:Range(cRange):Value = slut_allt2.FELOID.
    END.
    
END.

                       


/* create embedded chart using the data in the Worksheet */
chWorksheetRange = chWorksheet:Range("A1:B9").
chWorksheet:ChartObjects:Add(10,300,420,400):Activate.

/*ChartWizard UTSEEND PÅ STAPLAR,FÄRGER MM,KLASS,VET EJ,ANTAL RUBRIK RADER,TEXT PÅ HÖGER SIDA,RUBRIK,
  X-RUBRIK,Y-RUBRIK)
*/
chExcelApplication:ActiveChart:ChartWizard(chWorksheetRange, 3, 6, 2, 1, 1, TRUE,
    titelvar, "", "").


/* create chart using the data in the Worksheet */
chWorkSheet:Range("A2:A9"):Select().   
/*
chExcelApplication:Selection:Style = "Currency".
*/
chChart=chExcelApplication:Charts:Add().
chChart:Name = "Test Chart".
chChart:Type = 2.
  
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.
RELEASE OBJECT chChart.
RELEASE OBJECT chWorksheetRange. 

