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
DEFINE VARIABLE KOLLPERS AS CHARACTER NO-UNDO.

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
 
 DEFINE TEMP-TABLE dagtemp
   FIELD DATUM LIKE TIDREGITAB.DATUM  
   field dag like TIDREGITAB.DAG 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD    
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   FIELD BEROVER AS INTEGER
   field OSTART like TIDREGITAB.START 
   field OSLUT like TIDREGITAB.SLUT
   field Okod1 like TIDREGITAB.okod1
   field Okod2 like TIDREGITAB.okod2
   field Okod3 like TIDREGITAB.okod3
   field Oant1 like TIDREGITAB.oant1
   field Oant2 like TIDREGITAB.oant2
   field Oant3 like TIDREGITAB.oant3
   field Ototalt like TIDREGITAB.oant3
   field BERSTART like TIDREGITAB.START 
   field BERSLUT like TIDREGITAB.SLUT 
   field BERTOTALT like TIDREGITAB.TOTALT
   field BERedskap like TIDREGITAB.beredskap
   INDEX DATUM IS PRIMARY personalkod omrade DATUM BEROVER BERSTART OSTART ASCENDING . 
 FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).

END FUNCTION.
 
    
/*OUTPUT TO C:\LULFLEX2014.TXT.*/
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = FALSE NO-ERROR.
chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
 
chWorkSheet:Columns("A"):ColumnWidth = 7 NO-ERROR.
chWorkSheet:Columns("B"):ColumnWidth = 25 NO-ERROR.
chWorkSheet:Columns("C"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("D"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("E"):ColumnWidth = 5 NO-ERROR.
chWorkSheet:Columns("F"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("G"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("H"):ColumnWidth = 7 NO-ERROR.
chWorkSheet:Columns("I"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("J"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("K"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("L"):ColumnWidth = 7 NO-ERROR.
chWorkSheet:Columns("M"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("N"):ColumnWidth = 7 NO-ERROR.
chWorkSheet:Columns("O"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("P"):ColumnWidth = 7 NO-ERROR.
chWorkSheet:Columns("Q"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("R"):ColumnWidth = 10 NO-ERROR.

chWorkSheet:Range("A1:H1"):Font:Bold = TRUE NO-ERROR.

chWorkSheet:Range("A1"):Value = "Enhet" NO-ERROR.
chWorkSheet:Range("B1"):Value = "Namn" NO-ERROR.
chWorkSheet:Range("C1"):Value = "Omrade" NO-ERROR.
chWorkSheet:Range("D1"):Value = "Datum" NO-ERROR.
chWorkSheet:Range("E1"):Value = "Dag" NO-ERROR.

chWorkSheet:Range("F1"):Value = "Beredskap" NO-ERROR.
chWorkSheet:Range("F2"):Value = "start" NO-ERROR.
chWorkSheet:Range("G1"):Value = "Beredskap" NO-ERROR.
chWorkSheet:Range("G2"):Value = "Slut" NO-ERROR.
chWorkSheet:Range("H1"):Value = "Beredskap" NO-ERROR.
chWorkSheet:Range("H2"):Value = "Löneart" NO-ERROR.
chWorkSheet:Range("I1"):Value = "Beredskap" NO-ERROR.
chWorkSheet:Range("I2"):Value = "Timmar" NO-ERROR.
chWorkSheet:Range("I3"):Value = "hundradelar" NO-ERROR.

chWorkSheet:Range("J1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("J2"):Value = "Start" NO-ERROR.
chWorkSheet:Range("K1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("K2"):Value = "Slut" NO-ERROR.
chWorkSheet:Range("L1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("L2"):Value = "lart" NO-ERROR.
chWorkSheet:Range("M1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("M2"):Value = "antal" NO-ERROR.
chWorkSheet:Range("N1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("N2"):Value = "lart" NO-ERROR.
chWorkSheet:Range("O1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("O2"):Value = "antal" NO-ERROR.
chWorkSheet:Range("P1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("P2"):Value = "lart" NO-ERROR.
chWorkSheet:Range("Q1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("Q2"):Value = "antal" NO-ERROR.
chWorkSheet:Range("R1"):Value = "Övertid" NO-ERROR.
chWorkSheet:Range("R2"):Value = "Summa tim" NO-ERROR.
chWorkSheet:Range("R3"):Value = "hundradelar" NO-ERROR.


/*chWorkSheet:Range("G:G"):NumberFormat = "@" NO-ERROR.*/
   
   
iColumn = 4.   
FIND FIRST FORETAG  NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
regar = 2015.
FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK:   
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF AVAILABLE JURPERS THEN DO:
      IF JURPERS.JUDID = "03" THEN DO:    
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.                            
         OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND TIDREGITAB.DATUM GE DATE(01,01,regar) AND TIDREGITAB.DATUM LE DATE(12,31,regar) AND TIDREGITAB.OKOD1 NE ""  NO-LOCK.
         GET FIRST toq NO-LOCK.
         DO WHILE AVAILABLE (TIDREGITAB):
            CREATE dagtemp.
            ASSIGN    
            dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
            dagtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,10) + " " + SUBSTRING(PERSONALTAB.EFTERNAMN,1,20)
            dagtemp.OMRADE = PERSONALTAB.OMRADE                  
            dagtemp.DATUM =  TIDREGITAB.DATUM
            dagtemp.dag = TIDREGITAB.DAG
            dagtemp.berover = 2
            dagtemp.Ostart = TIDREGITAB.START
            dagtemp.OSLUT = TIDREGITAB.SLUT
            dagtemp.oant1 = TIDREGITAB.OANT1
            dagtemp.oant2 = TIDREGITAB.OANT2
            dagtemp.oant3 = TIDREGITAB.OANT3
            dagtemp.okod1 = TIDREGITAB.Okod1
            dagtemp.okod2 = TIDREGITAB.Okod2
            dagtemp.okod3 = TIDREGITAB.Okod3
            dagtemp.OTOTALT = klock100(TIDREGITAB.OANT1) + + klock100(TIDREGITAB.OANT2) + + klock100(TIDREGITAB.OANT3).      
            GET NEXT toq NO-LOCK.
         END.
         
         OPEN QUERY tbq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND TIDREGITAB.DATUM GE DATE(01,01,regar) AND TIDREGITAB.DATUM LE DATE(12,31,regar) AND TIDREGITAB.BEREDSKAP NE ""  NO-LOCK.
         GET FIRST tbq NO-LOCK.
         DO WHILE AVAILABLE (TIDREGITAB):
            CREATE dagtemp.
            ASSIGN    
            dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
            dagtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,10) + " " + SUBSTRING(PERSONALTAB.EFTERNAMN,1,20)
            dagtemp.OMRADE = PERSONALTAB.OMRADE                  
            dagtemp.DATUM =  TIDREGITAB.DATUM
            dagtemp.dag = TIDREGITAB.DAG
            dagtemp.berover = 1
            dagtemp.berstart = TIDREGITAB.BEREDSKAPSTART
            dagtemp.berSLUT = TIDREGITAB.BEREDSKAPSLUT
            dagtemp.berTOTALT = klock100(TIDREGITAB.BERANTAL)
            dagtemp.beredskap = TIDREGITAB.BEREDSKAP.
            GET NEXT tbq NO-LOCK.
         END.
      END.   
   END.   
END.
kollpers = "".            
FOR EACH dagtemp:
   iColumn = iColumn + 1.
   IF kollpers NE dagtemp.PERSONALKOD THEN iColumn = iColumn + 1.                 
   
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
   IF dagtemp.BEREDSKAP NE "" THEN DO:
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.BERSTART NO-ERROR.
      cRange = "G" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.BERSLUT NO-ERROR.                     
      cRange = "H" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.BEREDSKAP NO-ERROR.
      cRange = "I" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.BERTOTALT NO-ERROR.
   END.
   IF dagtemp.OKOD1 = "" AND dagtemp.OKOD2 = "" AND dagtemp.OKOD3 = "" THEN .
   ELSE DO:
      cRange = "J" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OSTART,">9.99") NO-ERROR.
      cRange = "K" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OSLUT,">9.99") NO-ERROR.
      cRange = "L" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.OKOD1 NO-ERROR.
      cRange = "M" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OANT1,">9.99") NO-ERROR.
      cRange = "N" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.OKOD2 NO-ERROR.
      cRange = "O" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OANT2,">9.99") NO-ERROR.
      cRange = "P" + cColumn.
      chWorkSheet:Range(cRange):Value = dagtemp.OKOD3 NO-ERROR.                     
      cRange = "Q" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OANT3,">9.99") NO-ERROR.
      cRange = "R" + cColumn.
      chWorkSheet:Range(cRange):Value = STRING(dagtemp.OTOTALT,">9.99") NO-ERROR.
  
  END.
  kollpers = dagtemp.PERSONALKOD.
END.
chExcelApplication:displayalerts = FALSE.      
chWorkbook:SaveAs("C:\OVERSBERAMLULE2015.XLSX",,,,,,,,,).        
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