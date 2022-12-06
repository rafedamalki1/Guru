 /*
   ActiveSheet.Unprotect
    ActiveSheet.Protect DrawingObjects:=False, Contents:=True, Scenarios:= True
    */
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   
kommando = "c:\protemp9\elpao\skydd.xls".
/*EXECLIN.I*/

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
   
    
DEFINE VARIABLE vvv AS HANDLE NO-UNDO.
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
  CREATE karnproc.
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
  karnproc.delnr = 0.
  RUN startexcel_UI.
  /*CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE NO-ERROR.*/
   
    /*chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).*/
   /*chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.*/   
   chWorkSheet:Columns("A"):ColumnWidth = 10 NO-ERROR.
   chWorkSheet:Columns("B"):ColumnWidth = 15 NO-ERROR.
   chWorkSheet:Columns("C"):ColumnWidth = 10 NO-ERROR.
   chWorkSheet:Columns("D"):ColumnWidth = 5 NO-ERROR.
     
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE NO-ERROR.

   chWorkSheet:Range("A1"):Value = "kärnprocess" NO-ERROR.   
   chWorkSheet:Range("B1"):Value = "delprocess" NO-ERROR.
   chWorkSheet:Range("C1"):Value = "aonr" NO-ERROR.   
   chWorkSheet:Range("D1"):Value = "delnr" NO-ERROR.   
      

   chWorkSheet:Range("A:A"):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range("B:B"):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range("C:C"):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range("D:D"):NumberFormat = "@" NO-ERROR.
      
      
   {EXCELFEL.I}
   

   OPEN QUERY satsq FOR EACH karnproc  NO-LOCK USE-INDEX karn.
   GET FIRST satsq NO-LOCK.
   DO WHILE AVAILABLE(karnproc): 
      {EXCELFEL.I}
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).            
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = karnproc.karnproc NO-ERROR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = karnproc.delproc NO-ERROR.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = karnproc.aonr NO-ERROR.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = karnproc.delnr NO-ERROR.  
                  
      GET NEXT satsq NO-LOCK.
   END.     
   CLOSE QUERY satsq.        
   
   RUN nyttbladexcel_UI.
   RUN valjblad_UI (INPUT bladvar).
   chWorkSheet:MOVE(,chWorkbook:Sheets(bladvar)) NO-ERROR.
   
   
   /*chWorkSheetRange = chWorkSheet:PivotCaches:Create(xlDatabase, "Blad1!R1C1:R11C3", xlPivotTableVersion12 ).CreatePivotTable.
        
   chWorkSheetRange:TableDestination = "Blad5!R3C1" NO-ERROR.
   chWorkSheetRange:TableName = "Pivottabell2" NO-ERROR.
   chWorkSheetRange:DefaultVersion = xlPivotTableVersion12 NO-ERROR.



          
        /*ActiveWorkbook.PivotCaches.Create(SourceType:=xlDatabase, SourceData:= _
        "Blad1!R1C1:R11C3", Version:=xlPivotTableVersion12).CreatePivotTable _
        TableDestination:="Blad5!R3C1", TableName:="Pivottabell2", DefaultVersion _
        :=xlPivotTableVersion12*/
   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
  
  
  
          
  
  RUN slutexcel_UI.
