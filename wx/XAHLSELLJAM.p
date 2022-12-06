/*INLÄSNING AV PRISFIL AHLSELL*/       
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{ALLDEF.I}
{GLOBVAR2DEL1.I}


DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
{PROVAG.I}
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

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
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.   
DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE mkattmp   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER   
   FIELD NPRIS              AS DECIMAL
   FIELD NPRIS2              AS DECIMAL
   FIELD NPRIS3              AS DECIMAL
   INDEX ENR ENR DESCENDING.
   
DEFINE TEMP-TABLE felex NO-UNDO LIKE mkattmp.

DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   FIELD BENAMNING          AS CHARACTER
   FIELD BENAMNING2          AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   FIELD ENHET              AS CHARACTER
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

DEFINE VARIABLE filnamn1 AS CHARACTER NO-UNDO.      
DEFINE VARIABLE filnamn2 AS CHARACTER NO-UNDO.   
DEFINE VARIABLE filnamn3 AS CHARACTER NO-UNDO.   
/*DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.*/

   
fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
OS-CREATE-DIR VALUE(fildir) NO-ERROR.
mappvar = fildir.
EMPTY TEMP-TABLE mkattmp NO-ERROR. 
SYSTEM-DIALOG GET-FILE fildir
TITLE          "Välj den excelfil som Ni vill läsa in som materielkatalog"
FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
INITIAL-DIR    mappvar
UPDATE OKvald.      
IF OKvald = TRUE THEN DO:                
   {muswait.i}
   EMPTY TEMP-TABLE mkattmp NO-ERROR. 
   EMPTY TEMP-TABLE felex NO-ERROR. 
   RUN XKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex ).   
END.


ASSIGN
filnamn1 = "\\server04\d\elpool\elpnj\ESGraninge\Prisjämförelse\Ahlsell\EonEs.skv"
filnamn2 = "\\server04\d\elpool\elpnj\ESGraninge\Prisjämförelse\Ahlsell\Prislista Elpool.skv"
filnamn3 = "\\server04\d\elpool\elpnj\ESGraninge\Prisjämförelse\Ahlsell\AhlsellES.skv".

RUN in_UI.
RUN utexcel_ui.
PROCEDURE in_UI: 
   
   EMPTY TEMP-TABLE tidinah NO-ERROR.   
   INPUT FROM VALUE(filnamn2) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH mkattmp:
      FIND FIRST tidinah  WHERE tidinah.ENR = mkattmp.ENR NO-LOCK NO-ERROR.
      IF AVAILABLE tidinah THEN DO:
         mkattmp.NPRIS2 = tidinah.NPRIS / 100.
      END.
      ELSE mkattmp.NPRIS2 = 0.         

   END.

   EMPTY TEMP-TABLE tidinah NO-ERROR.   
   INPUT FROM VALUE(filnamn3) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH mkattmp:
      FIND FIRST tidinah  WHERE tidinah.ENR = mkattmp.ENR NO-LOCK NO-ERROR.
      IF AVAILABLE tidinah THEN DO:
         mkattmp.NPRIS3 = tidinah.NPRIS / 100.
      END.
      ELSE mkattmp.NPRIS3 = 0.
   END.

   EMPTY TEMP-TABLE tidinah NO-ERROR.   
   INPUT FROM VALUE(filnamn1) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH mkattmp:
      FIND FIRST tidinah  WHERE tidinah.ENR = mkattmp.ENR NO-LOCK NO-ERROR.
      IF AVAILABLE tidinah THEN DO:
         mkattmp.NPRIS = tidinah.NPRIS / 100.
      END.
      ELSE mkattmp.NPRIS = 0.
   END.
   
END PROCEDURE.

PROCEDURE utexcel_UI: 
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add().
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:Columns("A"):ColumnWidth = 10.
   chWorkSheet:Columns("B"):ColumnWidth = 40.
   chWorkSheet:Columns("C"):ColumnWidth = 5.
   chWorkSheet:Columns("D"):ColumnWidth = 10.
   chWorkSheet:Columns("E"):ColumnWidth = 10.   
   
   chWorkSheet:Range("A1:P1"):Font:Bold = TRUE.

   chWorkSheet:Range("A1"):Value = Guru.Konstanter:genk.   
   chWorkSheet:Range("B1"):Value = "Benämning".
   chWorkSheet:Range("C1"):Value = "Enhet".   
   chWorkSheet:Range("D1"):Value = "Nettopris".   
   chWorkSheet:Range("E1"):Value = "Nettopris".   
   chWorkSheet:Range("F1"):Value = "Nettopris".   

   chWorkSheet:Range("A2:P2"):Font:Bold = TRUE.
   chWorkSheet:Range("D2"):Value = "20080402".   
   chWorkSheet:Range("E2"):Value = "20080626".   
   chWorkSheet:Range("F2"):Value = "20071026".   

   chWorkSheet:Range("A:A"):NumberFormat = "@".
   chWorkSheet:Range("B:B"):NumberFormat = "@".
   chWorkSheet:Range("C:C"):NumberFormat = "@".
   chWorkSheet:Range("D:D"):NumberFormat = "@".
   chWorkSheet:Range("E:E"):NumberFormat = "@".
   chWorkSheet:Range("F:F"):NumberFormat = "@".
   iColumn = 3.
   /*
   SESSION:NUMERIC-FORMAT = "european".
   */
   OPEN QUERY satsq FOR EACH mkattmp  NO-LOCK USE-INDEX ENR.
   GET FIRST satsq NO-LOCK.
   DO WHILE AVAILABLE(mkattmp): 
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).      
      cRange = "A" + cColumn.
      chWorkSheet:Range(cRange):Value = mkattmp.ENR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = mkattmp.BENAMNING.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = mkattmp.ENHET.
      cRange = "D" + cColumn.
      /*MESSAGE mkattmp.ENR mkattmp.NPRIS2 mkattmp.NPRIS mkattmp.NPRIS3 VIEW-AS ALERT-BOX.*/
      decichar = REPLACE(STRING(ROUND(mkattmp.NPRIS2,2)),".",",").   
      chWorkSheet:Range(cRange):Value = decichar.
      cRange = "E" + cColumn.
      decichar = REPLACE(STRING(ROUND(mkattmp.NPRIS,2)),".",",").   
      chWorkSheet:Range(cRange):Value = decichar.      
      cRange = "F" + cColumn.
      decichar = REPLACE(STRING(ROUND(mkattmp.NPRIS3,2)),".",",").   
      chWorkSheet:Range(cRange):Value = decichar.   
      /*chWorkSheet:Range(cRange):Value = string(mkattmp.NPRIS2,">>>>>9.99").
      cRange = "E" + cColumn.
      chWorkSheet:Range(cRange):Value = string(mkattmp.NPRIS,">>>>>9.99").      
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Value = string(mkattmp.NPRIS3,">>>>>9.99").      */
      GET NEXT satsq NO-LOCK.
   END.     
   CLOSE QUERY satsq.        
   /*
   SESSION:NUMERIC-FORMAT = "AMERICAN".
   */
                              
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.

END PROCEDURE.

