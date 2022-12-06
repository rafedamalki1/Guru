&Scoped-define NEW NEW
&Scoped-define SHARED SHARED


{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE flextot LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE kolldatum AS DATE NO-UNDO.
DEFINE VARIABLE lunorm AS INTEGER NO-UNDO.
DEFINE VARIABLE lufl AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE varansf LIKE ANSTFORMTAB.KOD NO-UNDO. 
DEFINE VARIABLE plflex AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE  VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE overch AS LOGICAL NO-UNDO.

DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
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



DEFINE VARIABLE fdatum AS DATE NO-UNDO.
/*DEFINE  TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".*/
DEFINE TEMP-TABLE ftemp   
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD OSTART AS DECIMAL 
   FIELD OSLUT AS DECIMAL
   FIELD FSTART AS DECIMAL 
   FIELD FSLUT AS DECIMAL 
   FIELD MKFLEX AS INTEGER
   FIELD MKTFLEX AS DECIMAL
   FIELD TFLEX AS INTEGER
   FIELD TOTFLEX AS DECIMAL
   FIELD LFLEX AS DECIMAL
   FIELD MFLEX AS DECIMAL
   FIELD KFLEX AS DECIMAL.

DEFINE TEMP-TABLE flut   
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMRADETAB AS CHARACTER
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD LFLEX AS DECIMAL 
   FIELD MKTFLEX AS DECIMAL
   FIELD TOTFLEX AS DECIMAL. 
 DEFINE TEMP-TABLE dagtemp
   FIELD DATUM LIKE TIDREGITAB.DATUM  
   field dag like TIDREGITAB.DAG 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD    
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   field START like TIDREGITAB.START 
   field SLUT like TIDREGITAB.SLUT 
   field TOTALT like TIDREGITAB.TOTALT
   INDEX DATUM IS PRIMARY personalkod omrade DATUM ASCENDING . 
 
    
/*OUTPUT TO C:\LULFLEX2014.TXT.*/
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = FALSE NO-ERROR.
chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
 
chWorkSheet:Columns("A"):ColumnWidth = 11 NO-ERROR.
chWorkSheet:Columns("B"):ColumnWidth = 30 NO-ERROR.
chWorkSheet:Columns("C"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("D"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("E"):ColumnWidth = 5 NO-ERROR.
chWorkSheet:Columns("F"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("G"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("H"):ColumnWidth = 15 NO-ERROR.

chWorkSheet:Range("A1:H1"):Font:Bold = TRUE NO-ERROR.

chWorkSheet:Range("A1"):Value = "Enhet" NO-ERROR.
chWorkSheet:Range("B1"):Value = "Namn" NO-ERROR.
chWorkSheet:Range("C1"):Value = "Omrade" NO-ERROR.
chWorkSheet:Range("D1"):Value = "Datum" NO-ERROR.
chWorkSheet:Range("E1"):Value = "Dag" NO-ERROR.
chWorkSheet:Range("F1"):Value = "Start" NO-ERROR.
chWorkSheet:Range("G1"):Value = "Slut" NO-ERROR.
chWorkSheet:Range("H1"):Value = "Totalt" NO-ERROR.
chWorkSheet:Range("G:G"):NumberFormat = "@" NO-ERROR.
iColumn = 2.   
FIND FIRST FORETAG  NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
regar = 2014.
FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK:
   ASSIGN
   persrec = RECID(PERSONALTAB).
   regdatum = bdatum. 
   
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.                            
   OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   AND TIDREGITAB.DATUM GE DATE(01,01,regar) AND TIDREGITAB.DATUM LE DATE(12,31,regar) AND TIDREGITAB.OKOD1 NE ""  NO-LOCK.
   GET FIRST toq NO-LOCK.
   DO WHILE AVAILABLE (TIDREGITAB):
      ASSIGN  regdatum = TIDREGITAB.DATUM.
      ASSIGN regvnr = TIDREGITAB.VECKONUMMER.
      RUN SLUTARB.P.
      overch = false.
      if regstart = regslut then.
      else IF TIDREGITAB.DATUM GE 05/15/2014 OR TIDREGITAB.DATUM LE 09/01/2014 THEN DO:
         IF TIDREGITAB.SLUT GE 6.3 AND TIDREGITAB.SLUT LE 7.3 THEN overch = true.
         IF TIDREGITAB.START GE 16 AND TIDREGITAB.START < 17 THEN overch = true.
      END.   
      ELSE IF YEAR(TIDREGITAB.DATUM) = 2014 THEN DO:
         IF TIDREGITAB.SLUT GE 6.3 AND TIDREGITAB.SLUT LE 7.3 THEN overch = true.
         IF TIDREGITAB.START GE 16.3 AND TIDREGITAB.START < 17.3 THEN overch = true.
      END.  
      if overch = true then do:
         CREATE dagtemp.
         ASSIGN    
         dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
         dagtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,10) + " " + SUBSTRING(PERSONALTAB.EFTERNAMN,1,20)
         dagtemp.OMRADE = PERSONALTAB.OMRADE                  
         dagtemp.DATUM =  TIDREGITAB.DATUM
         dagtemp.dag = TIDREGITAB.DAG
         dagtemp.start = TIDREGITAB.START
         dagtemp.SLUT = TIDREGITAB.SLUT
         dagtemp.TOTALT = TIDREGITAB.TOTALT.
      END.               
      GET NEXT toq NO-LOCK.
   END.
   
END.
            
FOR EACH dagtemp:                
   iColumn = iColumn + 1.
   cColumn = STRING(iColumn) NO-ERROR.
   cRange = "A" + cColumn.                     
   chWorkSheet:Range(cRange):Value = dagtemp.PERSONALKOD NO-ERROR.
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.NAMN NO-ERROR.
   cRange = "C" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.OMRADE NO-ERROR.
   cRange = "D" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.DATUM NO-ERROR.
   cRange = "E" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.DAG NO-ERROR.
   cRange = "F" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.START NO-ERROR.
   cRange = "G" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.SLUT NO-ERROR.                     
   cRange = "H" + cColumn.
   chWorkSheet:Range(cRange):Value = dagtemp.TOTALT NO-ERROR.
  
END.
chExcelApplication:displayalerts = FALSE.      
chWorkbook:SaveAs("C:\OVERSAMLULE2014.XLSX",,,,,,,,,).        
chExcelApplication:displayalerts = TRUE.

RELEASE OBJECT chWorksheetRange NO-ERROR.             
/*RELEASE OBJECT cActiveCell NO-ERROR.*/      
RELEASE OBJECT chWorksheet NO-ERROR.                  
NO-RETURN-VALUE chWorkbook:CLOSE().
NO-RETURN-VALUE chExcelApplication:QUIT().
RELEASE OBJECT chWorkbook NO-ERROR.    
RELEASE OBJECT chExcelApplication NO-ERROR.           
/*RELEASE OBJECT chWindow NO-ERROR.*/


/*OUTPUT TO C:\LULFLEX2014.TXT.
FOR EACH flut:
   EXPORT FLUT.
END.*/
OUTPUT CLOSE.   