
 /*------------------------------------------------------------------------
    File        : excel.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed Oct 26 16:06:07 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

USING GuruStart.*.

CLASS Guru.excel INHERITS Form: 
   DEFINE VARIABLE antalbladstart     AS INTEGER NO-UNDO.
   DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chChart            AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWorksheetRange   AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chPageBreak        AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE cActiveCell        AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWindow           AS COM-HANDLE NO-UNDO.   
   DEFINE VARIABLE dubbelradhj AS LOGICAL NO-UNDO.
   DEFINE VARIABLE BladvarKoll AS LOGICAL NO-UNDO. 
   DEFINE PUBLIC VARIABLE chWorksheet  AS COM-HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE visavarexcel AS LOGICAL NO-UNDO.
   
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
    METHOD PUBLIC VOID valueDataOutTT(INPUT TABLE excellTT):
      
      FOR EACH excellTT WHERE NO-LOCK:
         IF excellTT.KOLUMNRAD = "" OR excellTT.KOLUMNRAD = ? THEN.
           ELSE chWorkSheet:Range(excellTT.KOLUMNRAD):VALUE =  excellTT.VARDET NO-ERROR.
           
      END.
          
      EMPTY TEMP-TABLE excellTT NO-ERROR.
            
   END METHOD.
   METHOD PUBLIC VOID StartExcel():
      
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = TRUE.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      
   END METHOD.
   METHOD PUBLIC VOID SlutExcel():   
      chExcelApplication:VISIBLE = TRUE NO-ERROR.
      chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
   
      IF BladvarKoll = TRUE THEN DO: 
         RELEASE OBJECT chWorksheet NO-ERROR. 
         chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
         chWorkSheet:SELECT NO-ERROR.      
         BladvarKoll = FALSE.
      END.
      ELSE IF BladvarKoll = FALSE THEN DO:
         chWorkSheet:SELECT NO-ERROR.      
      END.
      RELEASE OBJECT chWorkbook NO-ERROR.                   
      RELEASE OBJECT chExcelApplication NO-ERROR.           
      RELEASE OBJECT chPageBreak NO-ERROR.                  
      RELEASE OBJECT chCell NO-ERROR.                       
      RELEASE OBJECT chWorksheet NO-ERROR.                  
      RELEASE OBJECT chChart NO-ERROR.                      
      RELEASE OBJECT chWorksheetRange NO-ERROR.             
      RELEASE OBJECT cActiveCell NO-ERROR. 
      RELEASE OBJECT chWindow NO-ERROR.
      ASSIGN
      chWorkbook = ?  
      chExcelApplication = ?
      chPageBreak = ?
      chCell = ?
      chWorksheet = ?
      chChart = ?
      chWorksheetRange = ?
      cActiveCell = ?.
      chWindow = ?.
   
   END METHOD.   	   
   CONSTRUCTOR PUBLIC excel ( ):
          
      SUPER().
      CATCH e AS Progress.Lang.Error:         
         UNDO, THROW e.
      END CATCH.
   END CONSTRUCTOR.

END CLASS.