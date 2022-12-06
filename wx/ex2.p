DEFINE TEMP-TABLE excellTT NO-UNDO
      FIELD KOLUMNRAD    AS CHARACTER
      FIELD KOLUMN       AS CHARACTER
      FIELD KOLUMNNUMMER AS INTEGER
      FIELD RAD          AS INTEGER
      FIELD VARDET       AS CHARACTER
      FIELD FONTNAMN     AS CHARACTER
      FIELD BOLD         AS LOGICAL
      FIELD FARG         AS INTEGER
      FIELD RADHOJD      AS DECIMAL
      INDEX KOLUMN    KOLUMN RAD
      INDEX RAD       RAD KOLUMNNUMMER 
      INDEX KOLUMNRAD KOLUMNRAD.

INPUT FROM C:\TEMP\p2.d.

   REPEAT:
      CREATE excellTT.
      ASSIGN.
      IMPORT excellTT.
   END.
   INPUT CLOSE.


DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkSheet              AS COM-HANDLE.
 DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWindow                AS COM-HANDLE.
DEFINE VARIABLE chWorkbook             AS COM-HANDLE.
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
DEFINE VARIABLE excelkommando AS CHARACTER NO-UNDO.


CREATE "Excel.Application" chExcelApplication.
chExcelApplication:VISIBLE = FALSE.
  
excelkommando = "C:\TEMP\kalkvisningtemplate.xlsx".     
 ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
  chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
  chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
  chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
DEFINE VARIABLE AA AS INTEGER NO-UNDO.
DEFINE VARIABLE bb AS INTEGER NO-UNDO.
aa = TIME.
RUN valueDataOutTT.
aa = TIME - aa . 

chWindow:VIEW = 3 NO-ERROR.
chExcelApplication:VISIBLE = TRUE.
RUN SlutExcel.

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:VISIBLE = FALSE.
excelkommando = "C:\TEMP\kalkvisningtemplate2.xlsx".     
ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
bb = TIME.
RUN valueDataOutTT.
bb = TIME - bb . 
   
MESSAGE 
   "time for xlPageLayoutView =" aa SKIP
   "time for xlNormalView ="  bb VIEW-AS ALERT-BOX.
chWindow:VIEW = 3 NO-ERROR.
chExcelApplication:VISIBLE = TRUE.

RUN SlutExcel.
 EMPTY TEMP-TABLE excellTT NO-ERROR.




PROCEDURE valueDataOutTT :
      FOR EACH excellTT WHERE NO-LOCK:
         IF excellTT.KOLUMNRAD = "" OR excellTT.KOLUMNRAD = ? THEN.
         ELSE DO:
            
            chWorkSheet:Range(excellTT.KOLUMNRAD):VALUE =  excellTT.VARDET.
         END.   
      END.
     
            
END PROCEDURE.
PROCEDURE SlutExcel:   
      chExcelApplication:VISIBLE = TRUE NO-ERROR.
      chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
      RELEASE OBJECT chWorksheet NO-ERROR. 
      
      
      RELEASE OBJECT chWorkbook NO-ERROR.                   
      RELEASE OBJECT chExcelApplication NO-ERROR.           
      
      RELEASE OBJECT chCell NO-ERROR.                       
      RELEASE OBJECT chWorksheet NO-ERROR.                  
      RELEASE OBJECT chChart NO-ERROR.                      
      RELEASE OBJECT chWorksheetRange NO-ERROR.             
      
      
      ASSIGN
      chWorkbook = ?  
      chExcelApplication = ?
      
      chCell = ?
      chWorksheet = ?
      chChart = ?
      chWorksheetRange = ?.
      
      
   
   END PROCEDURE.
