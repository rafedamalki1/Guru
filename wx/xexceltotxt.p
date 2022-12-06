DEFINE TEMP-TABLE ktemp NO-UNDO
   FIELD KUNDNR AS CHARACTER
   FIELD ADRESS     AS CHARACTER
   FIELD FORNAMN     AS CHARACTER
   FIELD EFTERNAMN   AS CHARACTER  
   FIELD TELEFON     AS CHARACTER
   INDEX PKOD IS PRIMARY KUNDNR. 

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.

DEFINE VARIABLE sokvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE utvarkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = FALSE.
/* kommando = SEARCH("fil.xls"). */
kommando = "c:\temp\fil.xls".
IF kommando = ? THEN DO:          
   MESSAGE "Hittade inte fil.xls" VIEW-AS ALERT-BOX.
   RETURN.       
END.  
chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
chWorkSheet = chExcelApplication:Sheets:Item(1).
chExcelApplication:DisplayAlerts = FALSE.
chWorkBook:SaveAs("c:\temp\fil.csv",6,,,,,) NO-ERROR.
NO-RETURN-VALUE chWorkbook:CLOSE().
NO-RETURN-VALUE chExcelApplication:QUIT().

RELEASE OBJECT chExcelApplication NO-ERROR.      
RELEASE OBJECT chWorkbook NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.

sokvar = "c:\temp\fil.csv".
EMPTY TEMP-TABLE ktemp NO-ERROR. 
IF sokvar NE ? THEN DO:  
   INPUT FROM VALUE(sokvar).
   REPEAT:
      CREATE ktemp.
      ASSIGN.
      IMPORT DELIMITER ";" ktemp .        
   END.
END.     
INPUT CLOSE.
OS-DELETE VALUE("c:\temp\fil.csv").
sokvar = "c:\temp\fil.txt".
OUTPUT TO VALUE(sokvar).

FOR EACH ktemp:
   PUT UNFORMATTED ktemp.KUNDNR + "&" + ktemp.TELEFON SKIP.   
END.
OUTPUT CLOSE.

