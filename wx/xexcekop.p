         
                              
DEFINE VARIABLE markag AS INTEGER NO-UNDO.
DEFINE VARIABLE extrarader AS INTEGER NO-UNDO.
DEFINE VARIABLE extrarader2 AS INTEGER NO-UNDO.
DEFINE VARIABLE extram AS INTEGER NO-UNDO.
DEFINE VARIABLE emg AS INTEGER NO-UNDO.
DEFINE VARIABLE uColumn  AS INTEGER INITIAL 0.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytcol AS LOGICAL NO-UNDO.
DEFINE VARIABLE bytacol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytccol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytgcol AS CHARACTER NO-UNDO.
DEFINE VARIABLE radrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE vman AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE hjsakr AS CHARACTER NO-UNDO.


DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
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
DEFINE VARIABLE radnrS                  AS CHARACTER.
DEFINE VARIABLE pristext                AS CHARACTER.
DEFINE VARIABLE valvardnr AS INTEGER NO-UNDO.
DEFINE VARIABLE omravd AS INTEGER NO-UNDO.
DEFINE VARIABLE aovar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE ortvar AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fkommun AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fvaker AS CHARACTER NO-UNDO.   
DEFINE VARIABLE vkab AS CHARACTER NO-UNDO.
DEFINE VARIABLE vkabstl AS CHARACTER NO-UNDO.
DEFINE VARIABLE sidvar AS INTEGER NO-UNDO.

/*test lena
{BERANN.I} */

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED

/*/*{EGENBEN.I}*/*/

{STARTFORAPP.I}
/*{GLOBVAR2DEL1.I}           */

{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
/*{EXECLIN2.I}*/
   ASSIGN
   fnamn = "KABELSKAPSKORT.xls"
   sidvar = 1.


kommando = SEARCH(fnamn).   
IF kommando = ? THEN DO:
   MESSAGE "Hittade inte " fnamn VIEW-AS ALERT-BOX.
   RETURN.       
END.  
kommando2 = SESSION:TEMP-DIR + fnamn.
kommando = "C:\Program\WebClientApps\Elpool i Ume? AB\GuruOnWeb\elpao\" + fnamn.
kommando2 = "C:\Program FILES\WebClientApps\Elpool i Ume? AB\GuruOnWeb\elpaoABCDEFGHIJKLMN\" + fnamn.
OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.

/*
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = TRUE.
   
   {OPENEXCEL.I}
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
   chWorkSheet = chExcelApplication:Sheets:Item(sidvar).
   chWorkSheet:COPY(,chWorkSheet). 
   sidvar = 2.
   chWorkSheet = chExcelApplication:Sheets:Item(sidvar).
   chWorkSheet:COPY(,chWorkSheet). 

   /*chWorkSheet = chExcelApplication:sheets:copy(Sheets("Kabelsk?pskort") -after).*/
   /*chWorkSheet = chExcelApplication:sheets:COPY() -After. */

/*   
nO-RETURN-VALUE <com-handle>: Copy (<anytype>-After ).  
   chWorkSheet = chExcelApplication:Sheets:Copy(). 


chWorkSheet = chExcelApplication:COPY():Sheets("Kabelsk?pskort").*/

      /*
      Sheets("Kabelsk?pskort")Copy Before:=Sheets(1).        
      
   chWorkSheet = chExcelApplication:Sheets:Add().      
   Sheets.Add     
                                      
   Sheets("Blad1").Move Before:=Sheets(1)  
   
   [ Com-Handle-Var = ] <com-handle>: Add ( <anytype>-Before).
   */


  */

