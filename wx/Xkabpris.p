/*INLÄSNING AV ELEKTROSKANDIAS LIN- OCH KABELPRISER*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE tidin 
   FIELD ENR                AS CHARACTER FORMAT "X(11)"
   FIELD BEN                 AS CHARACTER FORMAT "X(40)"
   FIELD ENH                AS CHARACTER FORMAT "X(11)"
   FIELD PRIS               AS DECIMAL FORMAT ">>>>>9.99"   
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE tidin2   
   FIELD ENR                AS CHARACTER FORMAT "X(11)"
   FIELD BEN                AS CHARACTER FORMAT "X(40)"
   FIELD ENH                AS CHARACTER FORMAT "X(11)" 
   FIELD PRIS               AS DECIMAL FORMAT ">>>>>9.99"   
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
  
{muswait.i}         
RUN in_UI.
{musarrow.i}

PROCEDURE in_UI: 
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.    
   FOR EACH tidin2:
      DELETE tidin2.
   END.      
      ASSIGN
      filnamn = "F:\ELPNJ\VESAB\ELEKTRO\XX.SKV"
      wtidvar = "F:\PRO8\GURU\WTID\kabpris.q"
      dlcvar = "F:\PRO8\dlc\bin\quoter".             
   
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.      
   RUN skapasats_UI.           
   OS-DELETE VALUE(wtidvar).
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


   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 11.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 6.
   chWorkSheet:Columns("C"):ColumnWidth = 10.
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = "ENR".
   chWorkSheet:Range("B1"):Value = "BENÄMNING".
   chWorkSheet:Range("C1"):Value = "ENHET".
   chWorkSheet:Range("D1"):Value = "PRIS".
      
   iColumn = 2.
   FOR EACH tidin2:                     
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = tidin2.ENR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = SUBSTRING(tidin2.BEN,1,40).
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = tidin2.ENH.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = tidin2.PRIS.
   END.   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
END PROCEDURE.

PROCEDURE skapasats_UI:   
   FOR EACH tidin NO-LOCK:                                
      DO TRANSACTION:                                 
         FIND FIRST MTRL WHERE MTRL.ENR = tidin.ENR AND
         MTRL.LEVKOD = "1" AND MTRL.KALKNR = 0 USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.                     
         IF NOT AVAILABLE MTRL THEN DO:
            CREATE tidin2.
            ASSIGN
            tidin2.ENR = tidin.ENR
            tidin2.BEN = tidin.BEN
            tidin2.ENH = tidin.ENH 
            tidin2.PRIS = tidin.PRIS / 100.    
         END.
      END.                 
   END.    
END PROCEDURE.   

                
