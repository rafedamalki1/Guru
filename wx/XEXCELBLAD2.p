/*XEXCELBLAD2.P*/   
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE chActiveSheet           AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.   
DEFINE VARIABLE chCell                  AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE chPageBreak             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE listavar AS INTEGER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt
FIELD ETT AS CHARACTER FORMAT "X(32)"
FIELD TVA AS CHARACTER FORMAT "X(32)"
FIELD TRE AS CHARACTER FORMAT "X(32)".
kommando = "".
kommando = SEARCH("\PROTEMP9\lista1.xls").
CREATE "Excel.Application" chExcelApplication.

/* message chExcelApplication:application:version view-as alert-box. */
chExcelApplication:Visible = TRUE.
IF kommando NE "" THEN DO:
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
   chWorkSheet = chExcelApplication:Sheets:Item(1).

   chExcelApplication:DisplayAlerts = False.


/*    chWorkBook:SaveAs(kommando,     /* FileName */           */
/*                   44,              /* FileFormat 44=html */ */
/*                   ,                /* Password */           */
/*                   ,                /* WriteResPassword */   */
/*                   False,           /* ReadOnlyRecommend */  */
/*                   False,           /* CreateBackup */       */
/*                  ).                /* IntegerAccessMode */  */


   chWorkBook:SaveAs(kommando,43,,,,,,,). 
/*    chWorkBook:SaveAs(kommando, -4143,,,,,,, TRUE). */
   chExcelApplication:DisplayAlerts = True.
   
   chWorkBook:SaveAs(kommando,,,,,,,).
/*    chWorkBook:SaveAs(kommando,43,,,,,) NO-ERROR. */


/*    chWorkSheet:Range("A1:AU61"):Select.           */
/*    chExcelApplication:SELECTION():COPY().         */
/*    chWorkSheet = chExcelApplication:Sheets:ADD(). */
/*    chWorkSheet:MOVE(,chWorkbook:Sheets(1)).       */
   

   
/*    chWorkSheet:Range("A1:AU61"):Select.               */
/*    chExcelApplication:SELECTION():COPY().             */
/*                                                       */
/*    chWorkSheet:Range("A62:AU122"):Select.             */
/*    chWorkSheet = chExcelApplication:ActiveSheet.      */
/*    chWorkSheet:PASTE().                               */
/*                                                       */
   chWorkSheet:Range("A1"):Select.
   chWorkSheet:Range("A6"):Value = "Test".
   chWorkSheet:Range("V6"):Value = "123123".
   chWorkSheet:Range("AP6"):Value = string(05/11/07).

   iColumn = 62.  
   DO WHILE iColumn < 70:
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn + ":" + "F" + cColumn.
      chCell = chWorkSheet:Range(cRange).
      chCell:Interior:ColorIndex = 34.
      RUN skaparam_UI.
      
      cRange = "G" + cColumn  + ":" + "AK" + cColumn.
      chCell = chWorkSheet:Range(cRange).
      chCell:Interior:ColorIndex = 34.
      RUN skaparam_UI.
   
      cRange = "AL" + cColumn  + ":" + "AO" + cColumn.
      chCell = chWorkSheet:Range(cRange).
      chCell:Interior:ColorIndex = 34.
      RUN skaparam_UI.
   
      cRange = "AP" + cColumn  + ":" + "AU" + cColumn.
      chCell = chWorkSheet:Range(cRange).
      chCell:Interior:ColorIndex = 34.
      RUN skaparam_UI.
      iColumn = iColumn + 1.
   END.
   chExcelApplication:VISIBLE = TRUE.


   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
END.

PROCEDURE skaparam_UI:
      chWorkSheet:Range(cRange):Borders(7):Weight = 2. /*Vertikalt vänster om markering*/
      chWorkSheet:Range(cRange):Borders(8):Weight = 2. /*Horizontellt ovanför markering*/
      chWorkSheet:Range(cRange):Borders(9):Weight = 2. /*Horizontellt nedan markering*/
      chWorkSheet:Range(cRange):Borders(10):Weight = 2. /*Vertikalt höger om markering*/
END PROCEDURE.
   


   
