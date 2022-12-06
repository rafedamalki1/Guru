/*EXECLIN2.I*/
/*FÖR HJÄLP SE 
http://msdn.microsoft.com/sv-se/library/
  office development
  välj office version
  välj excel version
  välj developer reference
  välj excel object model reference "börja leta här"
  välj workbook objekt
  välj tex. methods
*/
DEFINE VARIABLE iColNum AS INTEGER NO-UNDO.
   DEFINE VARIABLE cColName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iSistaColnum AS INTEGER NO-UNDO.
   
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
DEFINE VARIABLE totheight AS DECIMAL NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chChart            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheetRange   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chPageBreak        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cActiveCell        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWindow           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCount             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iIndex             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMonth             AS INTEGER    NO-UNDO.
DEFINE VARIABLE dAnnualQuota       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dTotalSalesAmount  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iRad               AS INTEGER    INITIAL 0.
DEFINE VARIABLE iRadslut           AS INTEGER    INITIAL 0.
DEFINE VARIABLE cRad               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRange             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRange1             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRange2             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRangefont         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE radnrS             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE allac              AS CHARACTER  EXTENT 100 NO-UNDO. /*alla kolumner*/
DEFINE VARIABLE allacolumtext      AS CHARACTER  EXTENT 100 NO-UNDO. /*inläsning från excel*/
DEFINE VARIABLE allachar           AS LOGICAL    EXTENT 100 NO-UNDO. /*skall kolumn vara text även om den innehåller tal*/
DEFINE VARIABLE startc             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE slutc              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE slutbredd          AS INTEGER    NO-UNDO.
DEFINE VARIABLE raknare            AS INTEGER    NO-UNDO.
DEFINE VARIABLE rubrikvar          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE utnr               AS INTEGER    EXTENT 100 NO-UNDO.
DEFINE VARIABLE estartnr           AS INTEGER    EXTENT 100 NO-UNDO.
DEFINE VARIABLE bredd              AS INTEGER    EXTENT 100 NO-UNDO.
DEFINE VARIABLE nrcol              AS INTEGER    EXTENT 100 NO-UNDO.
DEFINE VARIABLE breddantal         AS INTEGER    NO-UNDO.
DEFINE VARIABLE datchar            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE arvar              AS INTEGER    NO-UNDO.
DEFINE VARIABLE manvar             AS INTEGER    NO-UNDO.
DEFINE VARIABLE dagvar             AS INTEGER    NO-UNDO.

DEFINE VARIABLE bladvar            AS INTEGER    NO-UNDO.
DEFINE VARIABLE bladvarkoll        AS LOGICAL NO-UNDO.
DEFINE VARIABLE radanmrak          AS INTEGER    NO-UNDO.
DEFINE VARIABLE helaben            AS CHARACTER  NO-UNDO.           /*lägga ut hela benämningen*/
DEFINE VARIABLE startnu            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE slutnu             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE bermtrlvar             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE bild AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelmus AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynbrwexcelbuffh AS HANDLE NO-UNDO.


/*RAD variabler*/
DEFINE VARIABLE vilkencol AS CHARACTER NO-UNDO.
DEFINE VARIABLE helafalt AS LOGICAL   NO-UNDO.
DEFINE VARIABLE coltext  AS CHARACTER NO-UNDO.
DEFINE VARIABLE coltextrange  AS CHARACTER NO-UNDO.
DEFINE VARIABLE radklar  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE antalbladstart AS INTEGER NO-UNDO.

DEFINE VARIABLE excelbrwhand AS HANDLE NO-UNDO.
DEFINE VARIABLE excelbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE exceltempbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE excelbrwqh AS HANDLE NO-UNDO.
DEFINE VARIABLE exceltempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
DEFINE VARIABLE excellabelvar AS CHARACTER NO-UNDO EXTENT 50.
DEFINE VARIABLE excellabelvar2 AS CHARACTER NO-UNDO EXTENT 50.
DEFINE VARIABLE excellabelrad2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE colhjalpi AS INTEGER NO-UNDO.
DEFINE VARIABLE maxantalcolexcel AS INTEGER NO-UNDO.
DEFINE VARIABLE sparadocnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE visavarexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE dubbelradhj AS LOGICAL NO-UNDO.
maxantalcolexcel = 100.


{AONRUTSID.I}
antalbladstart = 4.

/*utnr = positioner i tidut.UT
  estartnr = start pos i excel dvs utnr[4] av tidut.UT hamnar pos estartnr[4]
  i excel*/

RUN allac_UI.

FUNCTION getcolname RETURNS CHARACTER (INPUT i AS INTEGER):
   IF i = 1 THEN RETURN "a".
   IF i = 2 THEN RETURN "b".
   IF i = 3 THEN RETURN "c".
   IF i = 4 THEN RETURN "d".
   IF i = 5 THEN RETURN "e".
   IF i = 6 THEN RETURN "f".
   IF i = 7 THEN RETURN "g".
   IF i = 8 THEN RETURN "h".
   IF i = 9 THEN RETURN "i".
   IF i = 10 THEN RETURN "j".
   IF i = 11 THEN RETURN "k".
   IF i = 12 THEN RETURN "l".
   IF i = 13 THEN RETURN "m".
   IF i = 14 THEN RETURN "n".
   IF i = 15 THEN RETURN "o".
   IF i = 16 THEN RETURN "p".
   IF i = 17 THEN RETURN "q".
   IF i = 18 THEN RETURN "r".
   IF i = 19 THEN RETURN "s".
   IF i = 20 THEN RETURN "t".
   IF i = 21 THEN RETURN "u".
   IF i = 22 THEN RETURN "v".
   IF i = 23 THEN RETURN "x".
   IF i = 24 THEN RETURN "y".
   IF i = 25 THEN RETURN "z".
   IF i = 26 THEN RETURN "aa".
   IF i = 27 THEN RETURN "ab".
   IF i = 28 THEN RETURN "ac".
   IF i = 29 THEN RETURN "ad".
   IF i = 30 THEN RETURN "ae".
   IF i = 31 THEN RETURN "af".
   IF i = 32 THEN RETURN "ag".
   IF i = 33 THEN RETURN "ah".
   IF i = 34 THEN RETURN "ai".
   IF i = 35 THEN RETURN "aj".      
END FUNCTION.

FUNCTION  getcolnum RETURNS INTEGER ( i AS CHARACTER):
      IF i = "a" THEN RETURN 1  .
      IF i = "b" THEN RETURN 2  .
      IF i = "c" THEN RETURN 3  .
      IF i = "d" THEN RETURN 4  .
      IF i = "e" THEN RETURN 5  .
      IF i = "f" THEN RETURN 6  .
      IF i = "g" THEN RETURN 7  .
      IF i = "h" THEN RETURN 8  .
      IF i = "i" THEN RETURN 9  .
      IF i = "j" THEN RETURN 10 .
      IF i = "k" THEN RETURN 11 .
      IF i = "l" THEN RETURN 12 .
      IF i = "m" THEN RETURN 13 .
      IF i = "n" THEN RETURN 14 .
      IF i = "o" THEN RETURN 15 .
      IF i = "p" THEN RETURN 16 .
      IF i = "q" THEN RETURN 17 .
      IF i = "r" THEN RETURN 18 .
      IF i = "s" THEN RETURN 19 .
      IF i = "t" THEN RETURN 20 .
      IF i = "u" THEN RETURN 21 .
      IF i = "v" THEN RETURN 22 .
      IF i = "x" THEN RETURN 23 .
      IF i = "y" THEN RETURN 24 .
      IF i = "z" THEN RETURN 25 .
      IF i = "aa" THEN RETURN 26 .
      IF i = "ab" THEN RETURN 27 .
      IF i = "ac" THEN RETURN 28 .
      IF i = "ad" THEN RETURN 29 .
      IF i = "ae" THEN RETURN 30 .
      IF i = "af" THEN RETURN 31 .
      IF i = "ag" THEN RETURN 32 .
      IF i = "ah" THEN RETURN 33 .
      IF i = "ai" THEN RETURN 34 .
      IF i = "aj" THEN RETURN 35 .      
END FUNCTION.

PROCEDURE visaexel_UI :
   DEFINE INPUT PARAMETER vis AS LOGICAL NO-UNDO.
   visavarexcel = vis.
END PROCEDURE.
PROCEDURE allacb_UI :
   DEFINE INPUT-OUTPUT PARAMETER iab AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ab AS CHARACTER NO-UNDO.   
   iab = iab + 1.
   IF iab > maxantalcolexcel THEN RETURN.
   allac[iab] = ab.
END PROCEDURE.
PROCEDURE allac_UI :
   DEFINE VARIABLE iab AS INTEGER NO-UNDO.      
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "A").        
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "B").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "C").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "D").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "E").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "F").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "G").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "H").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "I").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "J").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "K").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "L").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "M").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "N").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "O").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "P").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "Q").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "R").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "S").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "T").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "U").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "V").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "W").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "X").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "Y").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "Z").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AA").        
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AB").
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AC").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AD").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AE").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AF").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AG").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AH").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AI").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AJ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AK").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AL").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AM").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AN").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AO").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AP").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AQ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AR").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AS").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AT").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AU").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AV").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AW").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AX").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AY").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "AZ"). 
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BA").        
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BB").
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BC").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BD").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BE").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BF").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BG").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BH").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BI").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BJ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BK").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BL").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BM").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BN").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BO").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BP").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BQ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BR").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BS").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BT").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BU").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BV").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BW").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BX").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BY").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "BZ").       
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CA").        
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CB").
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CC").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CD").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CE").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CF").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CG").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CH").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CI").          
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CJ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CK").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CL").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CM").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CN").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CO").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CP").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CQ").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CR").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CS").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CT").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CU").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CV").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CW").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CX").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CY").         
   RUN allacb_UI (INPUT-OUTPUT iab,INPUT "CZ").       
END PROCEDURE.

PROCEDURE Rowdown:
   iRad = iRad + 1.
   RUN ColumnRad.
END PROCEDURE.
PROCEDURE Rowdup:
   iRad = iRad - 1.
   RUN ColumnRad.
END PROCEDURE.
PROCEDURE ColRight:
   
   iColNum = getcolnum(cColName) + 1. 
   cColName = getcolname(iColNum).     
   RUN ColumnRad.
END PROCEDURE.
PROCEDURE Colleft:
   iColNum = getcolnum(cColName) - 1. 
   cColName = getcolname(iColNum).     
   RUN ColumnRad.
END PROCEDURE.
PROCEDURE ColumnRad:
   iColNum = getcolnum(cColName).
   IF iSistaColnum < iColNum THEN iSistaColnum = iColNum.
   cRange = cColName + STRING(iRad). 
END PROCEDURE.



PROCEDURE excelhead_UI :
   DEFINE INPUT PARAMETER leftcenterright AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER head AS CHARACTER NO-UNDO.
   /*SIDHUVUD*/
   head = head + " sid &S".
   IF leftcenterright = 1 THEN chWorkSheet:PAGESETUP:LEFTHEADER = head NO-ERROR.
   IF leftcenterright = 2 THEN chWorkSheet:PAGESETUP:CENTERHEADER = head NO-ERROR.
   IF leftcenterright = 3 THEN chWorkSheet:PAGESETUP:RIGHTHEADER = head NO-ERROR.
  /*
   {EXCELFEL.I}
   */
END PROCEDURE.


PROCEDURE center_UI :
   DEFINE INPUT PARAMETER leftcenterright AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER head AS CHARACTER NO-UNDO.                  

   /*chWorkSheet:Columns("C:C"):SELECT
      
      chWorkSheet:Columns("B"):HorizontalAlignment = 3.. */

   chWorkSheet:COLUMNS(head):HorizontalAlignment = leftcenterright. /**/


   /*leftcenterright = 4 höger 
   leftcenterright = 2 vänster
   leftcenterright = 3 centrerat*/
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE dubbelrad_UI :
   DEFINE VARIABLE inrad AS CHARACTER NO-UNDO.   
   inrad = STRING(iRad) + ":" + STRING(iRad).
   IF dubbelradhj = FALSE THEN RETURN.
   /*
   Rows("5:5").Select
    With Selection
        .HorizontalAlignment = xlGeneral
        .VerticalAlignment = xlBottom
        .WrapText = True
        .Orientation = 0
        .AddIndent = False
        .IndentLevel = 0
        .ShrinkToFit = False
        .ReadingOrder = xlContext
        .MergeCells = False
    End With
    WrapText = True
    Selection.RowHeight = 29.25
    */
    chWorkSheet:Range(cRange):RowHeight = chWorkSheet:Range(cRange):RowHeight * 2. 
    chWorkSheet:Range(inrad):WrapText = TRUE. 
END PROCEDURE.

PROCEDURE addkom_UI :
    DEFINE INPUT  PARAMETER textvar AS CHARACTER NO-UNDO.       
    chWorkSheet:Range(cRange):addcomment(textvar) NO-ERROR.
END PROCEDURE.  

PROCEDURE tabortkom_UI :
   chWorkSheet:Range(cRange):clearcomments() NO-ERROR.
END PROCEDURE.
PROCEDURE gridlines_UI :
   DEFINE INPUT PARAMETER print AS LOGICAL NO-UNDO.

   chWorksheet:PageSetup:PrintGridlines = print NO-ERROR.  
END PROCEDURE.
/*För att inte se screenvalue under pågående*/      
PROCEDURE noscreenexcel_UI.

   chExcelApplication:ScreenUpdating = FALSE.
END.



/*
   Sätter default-storlek för utskrift.
   
   Inputs för olika storlekar:
   1 = Letter paper (8.5 in. by 11 in.)
   8 = A3 paper (297 mm by 420 mm)
   9 = A4 paper (210 mm by 297 mm)
   
   Alla olika papper finns på:
   http://msdn.microsoft.com/en-us/library/documentformat.openxml.spreadsheet.pagesetup.papersize.aspx              
   */
PROCEDURE paperSize_UI :
   DEFINE INPUT  PARAMETER storlek AS INTEGER NO-UNDO.
   chWorksheet:PageSetup:PaperSize = storlek NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 OR felexcel = TRUE THEN DO:
      felexcel = FALSE.
      chWorksheet:PageSetup:PaperSize = 8 NO-ERROR.
   END.
    
   
    
END PROCEDURE.



PROCEDURE insertCol_UI :
   DEFINE INPUT  PARAMETER startcol AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER antal AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER hurofta AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE colnam AS CHARACTER NO-UNDO.
   
   i = startcol.
   DO WHILE i <= (antal * hurofta):                  
      colnam = getcolname(i).      
      chWorkSheet:Columns(colnam):EntireColumn:Insert NO-ERROR.
      i = i + hurofta.
   END.
   /*chWorksheet:Range("I3").Select.
   chWorksheet:Selection:EntireColumn:Insert.*/      
END PROCEDURE.

PROCEDURE flyttacol_UI :
   DEFINE INPUT  PARAMETER startcol AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER flyttcol AS INTEGER NO-UNDO.   
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE colnam AS CHARACTER NO-UNDO.
   
   i = startcol.                    
   colnam = getcolname(i).      
   chWorkSheet:Columns(colnam):EntireColumn:Insert NO-ERROR.
   i = flyttcol + 1.
   colnam = getcolname(i).      
   chWorkSheet:Columns(colnam):EntireColumn:Cut .
   i = startcol.
   colnam = getcolname(i).     
   chWorkSheet:Columns(colnam):SELECT. 
   chWorkSheet:Paste .
      
END PROCEDURE.


PROCEDURE delCol_UI :
   DEFINE INPUT  PARAMETER startcol AS INTEGER NO-UNDO.
  
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE colnam AS CHARACTER NO-UNDO.
   i = startcol.
   colnam = getcolname(i).      
   chWorkSheet:Columns(colnam):EntireColumn:Delete NO-ERROR.
   /*i = startcol.
   DO WHILE i <= (antal * hurofta):                  
      colnam = getcolname(i).      
      chWorkSheet:Columns(colnam):EntireColumn:Delete NO-ERROR.
      i = i + hurofta.
   END.*/
   /*chWorksheet:Range("I3").Select.
   chWorksheet:Selection:EntireColumn:Insert.*/      
END PROCEDURE.
PROCEDURE delColCh_UI :
   DEFINE INPUT  PARAMETER colnam AS CHARACTER NO-UNDO.
   chWorkSheet:Columns(colnam):EntireColumn:Delete NO-ERROR.
         
END PROCEDURE.
PROCEDURE Csel_UI :
   DEFINE INPUT  PARAMETER cin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER varut AS CHARACTER NO-UNDO.
   cin = cin + cRad. 
   chWorkSheet:Range(cin):SELECT NO-ERROR.
   ASSIGN
   cActiveCell = chExcelApplication:ActiveCell 
   cActiveCell:VALUE = varut NO-ERROR. 
   RELEASE OBJECT cActiveCell NO-ERROR.   
   
END PROCEDURE.

/*För att se screenvalue när det är klart*/
PROCEDURE screenexcel_UI.
   chExcelApplication:ScreenUpdating = TRUE NO-ERROR.
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
END.
/*Lägga in bild*/
/*KLAR EJ EXCEL 2007

PROCEDURE imageexcel_UI.
   DEFINE INPUT PARAMETER link AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vilkencol AS CHARACTER NO-UNDO.
   IF cRad = "" THEN DO:
      ASSIGN cRad = "1".
   END.  
   bild  = vilkencol + cRad.   
/*    chWorkSheet:Rows(cRad):SELECT NO-ERROR. */
   chWorkSheet:range(bild):SELECT NO-ERROR. 
   chWorkSheet:Pictures:INSERT(link).
   chExcelApplication:VISIBLE = TRUE.
  /*     chWorkSheet:rows(cRad):DESELECT. */  
   IF cRad = "1" THEN cRad = "0".
END PROCEDURE.
    */
/*sätta lösen på blad*/
PROCEDURE losen_UI :
   DEFINE INPUT PARAMETER losen AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER l1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER l2 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER l3 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER l4 AS LOGICAL NO-UNDO.
   chWorkSheet:EnableAutoFilter = TRUE NO-ERROR.
   chWorkSheet:Protect(losen,l1,l2,l3,l4) NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE tabortlosen_UI :
    DEFINE INPUT PARAMETER losen AS CHARACTER NO-UNDO.
    chWorkSheet:Unprotect(losen) NO-ERROR. 
    {EXCELFEL.I}
END PROCEDURE.
/*Lägga in bild*/
PROCEDURE imageexcel_UI.
   DEFINE INPUT PARAMETER link AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vilkencol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inrow AS INTEGER NO-UNDO.
   
   cRad = STRING(inrow).
   IF cRad = "" THEN DO:
      ASSIGN cRad = "1".
   END.  
   bild  = vilkencol + cRad.   
   IF totheight = 0 THEN totheight = 1.
   /*
   chWorkSheet:Rows(cRad):SELECT NO-ERROR. 
   */
   chWorkSheet:range(bild):SELECT NO-ERROR. 

   chWorkSheetRange = chWorkSheet:Pictures:INSERT(link) NO-ERROR.
   chWorkSheetRange:TOP = totheight NO-ERROR.
   chWorkSheetRange:LEFT = 1 NO-ERROR.
   RELEASE OBJECT chWorkSheetRange NO-ERROR.
   chWorkSheetRange = ?.

/*    chWorkSheet:Pictures:INSERT(link). */
   
   chExcelApplication:VISIBLE = visavarexcel NO-ERROR.
  /*     chWorkSheet:rows(cRad):DESELECT. */  
   IF cRad = "1" THEN cRad = "0".
END PROCEDURE.

PROCEDURE imageexcelBygg_UI:
   DEFINE INPUT PARAMETER linkin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vilkencol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inrow AS INTEGER NO-UNDO.
   DEFINE VARIABLE bisz AS DECIMAL NO-UNDO.
   linkin = REPLACE(linkin,"/","\").
   cRad = STRING(inrow).
   IF cRad = "" THEN DO:
      ASSIGN cRad = "1".
   END.  
   bild  = vilkencol + cRad.   
   /*
   chWorkSheet:Rows(cRad):SELECT NO-ERROR. 
   */
   chWorkSheet:range(bild):SELECT NO-ERROR.            
     
   chWorkSheetRange = chWorkSheet:Pictures:INSERT(linkin) NO-ERROR.
   /*      
   chWorkSheetRange:TOP = totheight. 
   chWorkSheetRange:LEFT = 1  .
   bisz = 360.
   */
   
   
 
   RELEASE OBJECT chWorkSheetRange NO-ERROR.
   chWorkSheetRange = ?.
  
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
  
   IF cRad = "1" THEN cRad = "0".
   {EXCELFEL.I}  
END PROCEDURE.

PROCEDURE satestat_UI:
   /*endast för gamla program */
   IF estartnr[2] = 0 THEN DO:
      raknare = 1.
      DO WHILE utnr[raknare] > 0:
         estartnr[raknare] = utnr[raknare].
         raknare = raknare + 1.
      END.
      IF utnr[raknare] = 0 THEN DO:
         ASSIGN
         startc = "A"
         slutc = allac[raknare - 1].
      END.
   END.
END PROCEDURE.

PROCEDURE colbredd_UI :
   /*skall användas från och med nu*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   ASSIGN
      i = 2.
   estartnr[1] = 1.
   DO WHILE bredd[i] > 0:
      estartnr[i] = estartnr[i - 1] + bredd[i - 1] + 1.
      i = i + 1.
      IF i > maxantalcolexcel THEN DO:
         i = i - 1.
         LEAVE.
      END.
   END.
   IF bredd[i] = 0 THEN DO:
      ASSIGN
      startc = "A"
      slutc = allac[i - 1].
   END.
   ELSE DO:
      ASSIGN
      startc = "A"
      slutc = allac[i].
   END.
END PROCEDURE.

PROCEDURE readexcel_UI:
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = 1.
   REPEAT:
      {EXCELFEL.I}
     
      IF allac[i] = startc THEN startnu = TRUE.
      IF startnu = TRUE THEN DO:
         cRange = allac[i] + STRING(iRad).
         
         IF chExcelApplication:Range(cRange):Interior:ColorIndex = 5 THEN DO:
            allacolumtext[i] = "#blå#".
         END.
         ELSE IF chExcelApplication:Range(cRange):Interior:ColorIndex = 3 THEN DO:
            allacolumtext[i] = "#röd#".
         END.
         ELSE IF chExcelApplication:Range(cRange):Interior:ColorIndex = 6 THEN DO:
            allacolumtext[i] = "#gul#".
         END.
         ELSE IF chExcelApplication:Range(cRange):Interior:ColorIndex = 10 THEN DO:
            allacolumtext[i] = "#grö#".
         END.
         ELSE allacolumtext[i] = chWorkSheet:Range(cRange):TEXT NO-ERROR.
      END.
      IF slutnu = TRUE THEN DO:
         ASSIGN
         startnu = FALSE
         slutnu = FALSE.
         LEAVE.
      END.
      i = i + 1.
      IF allac[ i ] = slutc THEN slutnu = TRUE.
   END.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE readexcelstart_UI:
   DEFINE INPUT PARAMETER kommando AS CHARACTER NO-UNDO.
   felexcel = FALSE.
   CREATE "Excel.Application" chExcelApplication.
   {OPENEXCEL.I}
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   ASSIGN chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
   ASSIGN iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
   
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE readexcelstartvflik_UI:
   DEFINE INPUT PARAMETER kommando AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vflik AS INTEGER  NO-UNDO.
   felexcel = FALSE.
   CREATE "Excel.Application" chExcelApplication.
   {OPENEXCEL.I}
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   ASSIGN chWorkSheet = chExcelApplication:Sheets:ITEM(vflik) NO-ERROR.
   ASSIGN iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
   
   {EXCELFEL.I}
END PROCEDURE.


PROCEDURE open_UI:
   DEFINE INPUT  PARAMETER kommando AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE sparatxtxlsx AS CHARACTER NO-UNDO. 
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = FALSE.
   {OPENEXCEL.I}
   IF INDEX(kommando,".txt") > 0 THEN sparatxtxlsx = REPLACE(kommando,".txt",".xls").
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   IF sparatxtxlsx NE "" THEN DO:
      chWorkbook:SaveAs(sparatxtxlsx,56,,,,,,,,).
      NO-RETURN-VALUE chWorkbook:CLOSE() NO-ERROR.
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(sparatxtxlsx) .
   END.  
   chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.  
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWorkSheet:Columns("a:ab"):EntireColumn:AutoFit NO-ERROR.
   
   {EXCELFEL.I}
END PROCEDURE.
/*FUNKAR INTE
PROCEDURE openCh_UI:
   DEFINE INPUT  PARAMETER kommando AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE sparatxtxlsx AS CHARACTER NO-UNDO. 
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   MESSAGE "ss"
   VIEW-AS ALERT-BOX.
   chExcelApplication:VISIBLE = FALSE.
   chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWorkSheet:Range("A:A"):NumberFormat = "@" .
   {OPENEXCEL.I}
   IF INDEX(kommando,".txt") > 0 THEN sparatxtxlsx = REPLACE(kommando,".txt",".xls").
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   IF sparatxtxlsx NE "" THEN DO:
      chWorkbook:SaveAs(sparatxtxlsx,56,,,,,,,,).
      NO-RETURN-VALUE chWorkbook:CLOSE() NO-ERROR.
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(sparatxtxlsx) .
   END.  
   chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.  
   chWorksheet = chWindow:Worksheets:ITEM(1) NO-ERROR.
   chWorkSheet:Columns("a:ab"):EntireColumn:AutoFit NO-ERROR.
   
   {EXCELFEL.I}
END PROCEDURE.
*/
PROCEDURE startexcel_UI:
   
   felexcel = FALSE.
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   
   chExcelApplication:VISIBLE = TRUE.
   chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
   /*ROBIN TESTAR*/
   {EXLELVERSION.I}
   RUN nyttbladexcel_UI.   
   RUN noscreenexcel_UI.
   /*
   chWorkSheet = chExcelApplication:Sheets:Item(1).  /*tar blad nr 1*/
   */
   {EXCELFEL.I}
END PROCEDURE.


PROCEDURE startexcelval_UI:
   DEFINE INPUT PARAMETER startval AS LOGICAL NO-UNDO.
   
   felexcel = FALSE.
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = startval.
   chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
   /*ROBIN TESTAR*/
  {EXLELVERSION.I}
   RUN nyttbladexcel_UI.   
   RUN noscreenexcel_UI.
   /*
   chWorkSheet = chExcelApplication:Sheets:Item(1).  /*tar blad nr 1*/
   */
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE startexceld_UI:
   
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = FALSE NO-ERROR.
   chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   /*ROBIN TESTAR*/
   {EXLELVERSION.I}
   RUN nyttbladexcel_UI.  
   RUN noscreenexcel_UI. 
   /*
   chWorkSheet = chExcelApplication:Sheets:Item(1).  /*tar blad nr 1*/
   */
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE flerbladslut_UI :
   RELEASE OBJECT chWorksheet NO-ERROR. 
   chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
   chWorkSheet:SELECT NO-ERROR. 
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE slutreadexcel_UI:
   RUN flerbladslut_UI. 
   NO-RETURN-VALUE chWorkbook:CLOSE() NO-ERROR.
   NO-RETURN-VALUE chExcelApplication:QUIT() NO-ERROR.
   bladvarkoll = ?.   
   RUN slutexcel_UI.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE slut2excel_UI:
   RUN slutexcel_UI.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE slutmedprint_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
/* Worksheets("Blad1").PrintOut
   sparadocnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN sparadocnamn = webclienttempdir.
   OS-CREATE-DIR VALUE(sparadocnamn) NO-ERROR.
   sparadocnamn = sparadocnamn +  globanv  + STRING(TIME) + ".xls".   
   chExcelApplication:VISIBLE = FALSE.
   chExcelApplication:displayalerts = FALSE.
   chWorkbook:SaveAs(sparadocnamn,56,,,,,,,,).
   ExecuteExcel4Macro "PRINT(1,,,1,,,,,,,,3,,,TRUE,,FALSE)" alla blad
ExecuteExcel4Macro "PRINT(1,,,1,,,,,,,,2,,,TRUE,,FALSE)"    aktivt blad
   expression.PrintOut(From, To, Copies, Preview, ActivePrinter, PrintToFile, Collate, PrToFileName)

 
   */
   
   chExcelApplication:DisplayAlerts = FALSE. 
   IF vad = 1 THEN chWorkbook:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).
   IF vad = 2 THEN chWorkSheet:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).
   IF vad = 3 THEN chWorkbook:PrintOut(,,,,,,,).
   NO-RETURN-VALUE chWorkbook:CLOSE().
   NO-RETURN-VALUE chExcelApplication:QUIT().
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
   ASSIGN
   iCount  = 0            
   iIndex  = 0            
   iMonth   = 0           
   dAnnualQuota  = 0       
   dTotalSalesAmount  = 0  
   iRad                = 0 
   iRadslut            = 0 
   cRad               = ""
   cRange             = ""
   cRange1             = ""
   cRange2             = ""
   cRangefont         = ""
   radnrS             = ""
   allac              = "" /*alla kolumner*/
   allacolumtext      = "" /*inläsning från excell*/
   allachar            = FALSE
   startc             = ""
   slutc              = ""
   slutbredd           = 0 
   raknare             = 0 
   rubrikvar           = FALSE  
   utnr                = 0 
   estartnr            = 0 
   bredd               = 0 
   nrcol               = 0 
   breddantal          = 0 
   datchar            = ""
   arvar               = 0 
   manvar              = 0 
   dagvar              = 0 
   
   bladvar             = 0 
   radanmrak           = 0 
   helaben            = ""           /*lägga ut hela benämningen*/
   startnu             = FALSE
   slutnu              = FALSE
   bermtrlvar              = FALSE
   bild = ""
   excelmus  = FALSE
   vilkencol = ""
   helafalt = FALSE
   coltext  = ""
   coltextrange  = ""
   radklar  = FALSE.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE slutmedprintmark_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   /* går in flera gånger och skriver ut olika flikar  RELEASE efter alla*/
   
   /*IF vad = 1 THEN chWorkbook:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).*/
   IF vad = 2 THEN chWorkSheet:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).
   {EXCELFEL.I}
END PROCEDURE.


                   /*
PROCEDURE slutmedsaveas_UI :
   sparadocnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN sparadocnamn = webclienttempdir.
   OS-CREATE-DIR VALUE(sparadocnamn) NO-ERROR.
   sparadocnamn = sparadocnamn +  globanv  + STRING(TIME) + ".xls".   
   chExcelApplication:VISIBLE = FALSE.
   chExcelApplication:displayalerts = FALSE.
   chWorkbook:SaveAs(sparadocnamn,56,,,,,,,,). 
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
   ASSIGN
   iCount  = 0            
   iIndex  = 0            
   iMonth   = 0           
   dAnnualQuota  = 0       
   dTotalSalesAmount  = 0  
   iRad                = 0 
   iRadslut            = 0 
   cRad               = ""
   cRange             = ""
   cRange1             = ""
   cRange2             = ""
   cRangefont         = ""
   radnrS             = ""
   allac              = "" /*alla kolumner*/
   allacolumtext      = "" /*inläsning från excell*/
   allachar            = FALSE
   startc             = ""
   slutc              = ""
   slutbredd           = 0 
   raknare             = 0 
   rubrikvar           = FALSE  
   utnr                = 0 
   estartnr            = 0 
   bredd               = 0 
   nrcol               = 0 
   breddantal          = 0 
   datchar            = ""
   arvar               = 0 
   manvar              = 0 
   dagvar              = 0 
   
   bladvar             = 0 
   radanmrak           = 0 
   helaben            = ""           /*lägga ut hela benämningen*/
   startnu             = FALSE
   slutnu              = FALSE
   bermtrlvar              = FALSE
   bild = ""
   excelmus  = FALSE
   vilkencol = ""
   helafalt = FALSE
   coltext  = ""
   coltextrange  = ""
   radklar  = FALSE.
   {EXCELFEL.I}
END PROCEDURE.
*/
PROCEDURE slutexcel_UI:
   RUN screenexcel_UI.
   excelmus = SESSION:SET-WAIT-STATE("").
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
   chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
   
   IF bladvarkoll = TRUE THEN DO: 
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      chWorkSheet:SELECT NO-ERROR.      
      bladvarkoll = FALSE.
      /*
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(bladvar).
      chWorkSheet:MOVE(,chWorkbook:Sheets(3)).
      */
   END. 
   ELSE IF bladvarkoll = FALSE THEN DO:
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
   ASSIGN
   iCount  = 0            
   iIndex  = 0            
   iMonth   = 0           
   dAnnualQuota  = 0       
   dTotalSalesAmount  = 0  
   iRad                = 0 
   iRadslut            = 0 
   cRad               = ""
   cRange             = ""
   cRange1             = ""
   cRange2             = ""
   cRangefont         = ""
   radnrS             = ""
      /*
   allac              = "" /*alla kolumner*/
   */
   allacolumtext      = "" /*inläsning från excell*/
   allachar            = FALSE
   startc             = ""
   slutc              = ""
   slutbredd           = 0 
   raknare             = 0 
   rubrikvar           = FALSE  
   utnr                = 0 
   estartnr            = 0 
   bredd               = 0 
   nrcol               = 0 
   breddantal          = 0 
   datchar            = ""
   arvar               = 0 
   manvar              = 0 
   dagvar              = 0 
   
   bladvar             = 0 
   radanmrak           = 0 
   helaben            = ""           /*lägga ut hela benämningen*/
   startnu             = FALSE
   slutnu              = FALSE
   bermtrlvar              = FALSE
   bild = ""
   excelmus  = FALSE
   vilkencol = ""
   helafalt = FALSE
   coltext  = ""
   coltextrange  = ""
   radklar  = FALSE.
   
   {EXCELFEL.I}
   
END PROCEDURE.

PROCEDURE slutexceld_UI:
   RUN screenexcel_UI.
   excelmus = SESSION:SET-WAIT-STATE("").
   chExcelApplication:VISIBLE = TRUE.
   chExcelApplication:DisplayAlerts = TRUE.   /*all prompts will be shutoff/on*/   
   
   IF bladvarkoll = TRUE THEN DO: 
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      chWorkSheet:SELECT NO-ERROR.      
      bladvarkoll = FALSE NO-ERROR.
      /*
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(bladvar).
      chWorkSheet:MOVE(,chWorkbook:Sheets(3)).
      */
   END.
   ELSE IF bladvarkoll = FALSE THEN DO:
      chWorkSheet:SELECT NO-ERROR.      
   END.     
   
   RELEASE OBJECT chPageBreak NO-ERROR.                  
   RELEASE OBJECT chCell NO-ERROR.                          
   RELEASE OBJECT chChart NO-ERROR.                      
   RELEASE OBJECT chWorksheetRange NO-ERROR.             
   RELEASE OBJECT cActiveCell NO-ERROR.      
   RELEASE OBJECT chWorksheet NO-ERROR.                  
   NO-RETURN-VALUE chWorkbook:CLOSE().
   NO-RETURN-VALUE chExcelApplication:QUIT().
   RELEASE OBJECT chWorkbook NO-ERROR.    
   RELEASE OBJECT chExcelApplication NO-ERROR.           
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
   ASSIGN
   iCount  = 0            
   iIndex  = 0            
   iMonth   = 0           
   dAnnualQuota  = 0       
   dTotalSalesAmount  = 0  
   iRad                = 0 
   iRadslut            = 0 
   cRad               = ""
   cRange             = ""
   cRange1             = ""
   cRange2             = ""
   cRangefont         = ""
   radnrS             = ""
   allac              = "" /*alla kolumner*/
   allacolumtext      = "" /*inläsning från excell*/
   allachar            = FALSE
   startc             = ""
   slutc              = ""
   slutbredd           = 0 
   raknare             = 0 
   rubrikvar           = FALSE  
   utnr                = 0 
   estartnr            = 0 
   bredd               = 0 
   nrcol               = 0 
   breddantal          = 0 
   datchar            = ""
   arvar               = 0 
   manvar              = 0 
   dagvar              = 0 
   
   bladvar             = 0 
   radanmrak           = 0 
   helaben            = ""           /*lägga ut hela benämningen*/
   startnu             = FALSE
   slutnu              = FALSE
   bermtrlvar              = FALSE
   bild = ""
   excelmus  = FALSE
   vilkencol = ""
   helafalt = FALSE
   coltext  = ""
   coltextrange  = ""
   radklar  = FALSE.
   
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE nyttbladexcel_UI:
   bladvar = bladvar + 1.
   /*chWorkSheet = chExcelApplication:Sheets:Item(bladvar).   */
   IF bladvar < antalbladstart THEN DO: 
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(bladvar) NO-ERROR.
   END.
   ELSE DO:
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ADD NO-ERROR.
      chWorkSheet:MOVE(,chWorkbook:Sheets(bladvar)) NO-ERROR.
             
   END.
   {EXCELFEL.I}   
END PROCEDURE.
PROCEDURE namnbladexcel_UI:
   DEFINE INPUT PARAMETER namnvar AS CHARACTER NO-UNDO.
   chWorkSheet:NAME = namnvar NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE valjblad_UI:
   DEFINE INPUT PARAMETER valbald AS INTEGER NO-UNDO.
   chWorkSheet = chExcelApplication:Sheets:ITEM(valbald) NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE kopierabaldefter_UI:
   DEFINE INPUT PARAMETER eftersida AS INTEGER NO-UNDO.
   RELEASE OBJECT chWorksheet NO-ERROR. 
   chWorkSheet = chExcelApplication:Sheets:Item(eftersida) NO-ERROR.
   chWorkSheet:COPY(,chWorkSheet) NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR. 
   chWorkSheet = chExcelApplication:Sheets:ITEM(eftersida + 1) NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE kopierabaldfore_UI:
   DEFINE INPUT PARAMETER foresida AS INTEGER NO-UNDO.
   RELEASE OBJECT chWorksheet NO-ERROR. 
   chWorkSheet = chExcelApplication:Sheets:Item(foresida) NO-ERROR.
   chWorkSheet:COPY(chWorkSheet,) NO-ERROR. 
   RELEASE OBJECT chWorksheet NO-ERROR. 
   chWorkSheet = chExcelApplication:Sheets:ITEM(foresida) NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE fontexcel_UI :
   DEFINE INPUT PARAMETER varfont AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg AS INTEGER   NO-UNDO.  
   iRad = iRad + 1.
   cRad = STRING(iRad).
   cRange = startc + cRad.
   radnrS = slutc + cRad.   /*Slutkolumn*/
   cRangefont = cRange + ":" + radnrS.
   chWorkSheet:Range(cRangefont):FONT:NAME = varfont NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:SIZE = varsize NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:Bold = varbold NO-ERROR.
   chWorkSheet:Range(cRangefont):RowHeight = varhojd NO-ERROR.
   IF varfarg NE 0 THEN chWorkSheet:Range(cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
   {EXCELFEL.I} 
END PROCEDURE.
 

PROCEDURE sokiRepexcel_UI :
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER startcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER slutcol AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER byta AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER stAddress AS CHARACTER NO-UNDO.
   DEFINE VARIABLE rnValue AS COM-HANDLE NO-UNDO.
   cRangefont = startcol + ":" + slutcol.
   chCell = chWorkSheet:Range(cRangefont) NO-ERROR.
   rnValue = chCell:Find(vad,,,,,,,,). 
   IF rnValue = 0 THEN DO:

      RELEASE OBJECT chCell NO-ERROR.
      RETURN.
      /*
      rnValue = chWorkSheet:Range(startcol).
        */
   END.
   stAddress = rnValue:Address(,,,,). 
   chWorkSheet:Range(stAddress):VALUE = byta.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}

END PROCEDURE.

PROCEDURE sokiexcel_UI :
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER startcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER slutcol AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER stAddress AS CHARACTER NO-UNDO.
   DEFINE VARIABLE rnValue AS COM-HANDLE NO-UNDO.
   cRangefont = startcol + ":" + slutcol.
   chCell = chWorkSheet:Range(cRangefont) NO-ERROR.
   rnValue = chCell:Find(vad,,,,,,,,). 
   IF rnValue = 0 THEN DO:

      RELEASE OBJECT chCell NO-ERROR.
      RETURN.
      /*
      rnValue = chWorkSheet:Range(startcol).
        */
   END.
   stAddress = rnValue:Address(,,,,). 
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}

END PROCEDURE.


PROCEDURE understryk_UI:
   DEFINE INPUT PARAMETER varstryk AS INTEGER NO-UNDO. /*3= över 4= under*/
   DEFINE INPUT PARAMETER vartjock AS INTEGER NO-UNDO. /*2= tunn 4= tjock*/
   cRange = startc + cRad.
   radnrS = slutc + cRad.   /*Slutkolumn*/
   cRangefont = cRange + ":" + radnrS.
   chWorkSheet:Range(cRangefont):Borders(varstryk):Weight = vartjock NO-ERROR.
END PROCEDURE.
PROCEDURE kolumnexcel_UI:
   REPEAT:
      IF estartnr[raknare] = 0 THEN LEAVE.
      IF estartnr[raknare + 1] = 0 THEN LEAVE.
      chWorkSheet:COLUMNS(allac[raknare]):ColumnWidth = 1 + estartnr[raknare + 1] - estartnr[raknare] NO-ERROR.
      raknare = raknare + 1.     
      IF raknare = maxantalcolexcel THEN LEAVE.
      {EXCELFEL.I}      
   END.
   chWorkSheet:COLUMNS(slutc):ColumnWidth = slutbredd NO-ERROR.          /*Slutkolumn*/
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE Kryssruta_UI:
   DEFINE INPUT PARAMETER traffcoll AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER toppv AS INTEGER NO-UNDO.
   DEFINE VARIABLE topvalue AS INTEGER NO-UNDO.
   DEFINE VARIABLE leftvalue AS INTEGER NO-UNDO.
   cRangefont = traffcoll.
   chWorkSheet:Range(traffcoll):SELECT NO-ERROR.
   topvalue = chWorkSheet:Range(traffcoll):TOP + toppv.
   leftvalue = chWorkSheet:Range(traffcoll):LEFT + 15.
   chWorkSheet:CheckBoxes:Add(leftvalue, topvalue, 6, 6) NO-ERROR.
   chWorkSheet:CheckBoxes:Text = "".
    {EXCELFEL.I}      
END PROCEDURE.

   /*se visainkopu.w för taggar även visinkopapp.p för hur taggarna sätts*/
PROCEDURE rubrikerexcel_UI:
   DEFINE INPUT PARAMETER varut   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg AS INTEGER   NO-UNDO.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   
   chWorkSheet:Range(cRange):VALUE = varut NO-ERROR.
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   {EXCELFEL.I}
   /*RELEASE OBJECT chWorksheet NO-ERROR.                  */
END PROCEDURE.
PROCEDURE rubrikerexceltwocol_UI:
   DEFINE INPUT PARAMETER varut   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER coltwostart AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER coltwoname AS CHARACTER NO-UNDO.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   cRange = "A" + cRad.
   chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,1,coltwostart - 1) NO-ERROR.
   cRange = coltwoname + cRad.
   chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,coltwostart) NO-ERROR.
   cRange = "A" + cRad.
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   {EXCELFEL.I}
   /*RELEASE OBJECT chWorksheet NO-ERROR.                  */
END PROCEDURE.
PROCEDURE specrubrikexcel_UI:
   DEFINE INPUT PARAMETER varut   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg AS INTEGER   NO-UNDO.
   
   cRange = "A" + cRad.
   cRangefont = "A" + cRad + ":" + "E" + cRad.
   chWorkSheet:Range(cRangefont):FONT:NAME = varfont NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:SIZE = varsize NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:Bold = varbold NO-ERROR.
   chWorkSheet:Range(cRangefont):RowHeight = varhojd NO-ERROR.
   IF varfarg NE 0 THEN DO:
      chWorkSheet:Range(cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
   END.
   chWorkSheet:Range(cRange):SELECT NO-ERROR.
   ASSIGN
   cActiveCell = chExcelApplication:ActiveCell
   cActiveCell:VALUE = varut NO-ERROR. 
   RELEASE OBJECT cActiveCell NO-ERROR.         
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE specrubrikexcel2_UI:
   DEFINE INPUT PARAMETER varut   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg AS INTEGER   NO-UNDO.
   
   cRange = "F" + cRad.
   cRangefont = "F" + cRad + ":" + "J" + cRad.
   chWorkSheet:Range(cRangefont):FONT:NAME = varfont NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:SIZE = varsize NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:Bold = varbold NO-ERROR.
   chWorkSheet:Range(cRangefont):RowHeight = varhojd NO-ERROR.
   IF varfarg NE 0 THEN DO:
      chWorkSheet:Range(cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
   END.
   chWorkSheet:Range(cRange):SELECT NO-ERROR.
   ASSIGN
   cActiveCell = chExcelApplication:ActiveCell 
   cActiveCell:VALUE = varut NO-ERROR. 
   RELEASE OBJECT cActiveCell NO-ERROR.         
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   {EXCELFEL.I}
END PROCEDURE.


PROCEDURE brwstart_UI :
   DEFINE INPUT PARAMETER formatdeci AS DECIMAL NO-UNDO.
   excellabelrad2 = FALSE.
   excelbrwqh = excelbrwhand:QUERY.
   excelbuffh = excelbrwqh:GET-BUFFER-HANDLE .    
   exceltempcolh = excelbrwhand:GET-BROWSE-COLUMN(1). 
   colhjalpi = 1.
   DO WHILE VALID-HANDLE(exceltempcolh):
      IF exceltempcolh:VISIBLE = TRUE THEN DO:
         ASSIGN
         excellabelvar[colhjalpi] = exceltempcolh:LABEL
         bredd[colhjalpi] = exceltempcolh:WIDTH-CHARS.
         IF exceltempcolh:DATA-TYPE = "DECIMAL" THEN DO:
            IF formatdeci > 2 THEN DO:
               bredd[colhjalpi] = bredd[colhjalpi] + (formatdeci - 2). 
            END.
         END.       
         IF INDEX(excellabelvar[colhjalpi],"!") NE 0 THEN DO:
            ASSIGN
            excellabelrad2 = TRUE
            excellabelvar2[colhjalpi] =  SUBSTRING(excellabelvar[colhjalpi],INDEX(excellabelvar[colhjalpi],"!") + 1)
            excellabelvar[colhjalpi] = SUBSTRING(excellabelvar[colhjalpi],1,INDEX(excellabelvar[colhjalpi],"!") - 1).
            
         END.
         excellabelvar[colhjalpi] = REPLACE(excellabelvar[colhjalpi],"!"," ").  
         IF LENGTH(excellabelvar[colhjalpi]) > bredd[colhjalpi] THEN  bredd[colhjalpi] = INTEGER(LENGTH(excellabelvar[colhjalpi])).      
         colhjalpi = colhjalpi + 1.      
      END.
      exceltempcolh = exceltempcolh:NEXT-COLUMN.         
         
   END.
   colhjalpi = colhjalpi - 1.
   slutbredd = bredd[colhjalpi].
   
END PROCEDURE.

PROCEDURE brwrubrik_UI :
   raknare = 0.
   REPEAT:
      raknare = raknare  + 1.
      IF raknare > colhjalpi THEN LEAVE.
      IF raknare = 1 THEN
      RUN postexcel_UI (INPUT excellabelvar[raknare],INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT TRUE,INPUT raknare).        
      ELSE RUN postexcel_UI (INPUT excellabelvar[raknare],INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT FALSE,INPUT raknare).
      {EXCELFEL.I}
   END.
   IF excellabelrad2 = TRUE THEN DO:
      raknare = 0.
      REPEAT:
         raknare = raknare  + 1.
         IF raknare > colhjalpi THEN LEAVE.
         IF raknare = 1 THEN
         RUN postexcel_UI (INPUT excellabelvar2[raknare],INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT TRUE,INPUT raknare).        
         ELSE RUN postexcel_UI (INPUT excellabelvar2[raknare],INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT FALSE,INPUT raknare).
         {EXCELFEL.I}
      END.
   END.
END PROCEDURE.

PROCEDURE posterut_UI :
   DEFINE INPUT PARAMETER formatdeci AS INTEGER NO-UNDO.
   DEFINE VARIABLE fhelp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fraknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE craknare AS CHARACTER NO-UNDO.
   fraknare = 1.
   fhelp = "#0,".
   IF formatdeci = 0 THEN fhelp = "#0".
   REPEAT:
      IF fraknare > formatdeci THEN LEAVE. 
      fhelp = fhelp + "0".
      fraknare = fraknare + 1.
   END.   
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   raknare = 0.
   excelbrwqh:GET-FIRST(NO-LOCK).
   REPEAT:
      IF excelbrwqh:QUERY-OFF-END THEN LEAVE.                      
      exceltempcolh = excelbrwhand:GET-BROWSE-COLUMN(1).     
      DO WHILE VALID-HANDLE(exceltempcolh):
         

         IF exceltempcolh:NAME = ? THEN.
         ELSE IF exceltempcolh:VISIBLE = FALSE THEN.
         ELSE DO:
            raknare = raknare  + 1.
            exceltempbufh = excelbuffh:BUFFER-FIELD(exceltempcolh:NAME).
            IF exceltempbufh:BUFFER-VALUE = ? THEN.
            ELSE DO:
               decichar = STRING(exceltempbufh:BUFFER-VALUE).
               IF exceltempbufh:DATA-TYPE = "DECIMAL" THEN DO:
                  decichar = STRING(ROUND(exceltempbufh:BUFFER-VALUE,formatdeci)). 
               END.   
               IF raknare = 1 THEN
               RUN postexcel_UI (INPUT decichar,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT TRUE,INPUT raknare).        
               ELSE RUN postexcel_UI (INPUT decichar,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0,INPUT FALSE,INPUT raknare).
               IF exceltempbufh:DATA-TYPE = "DECIMAL" THEN DO:
                  chWorkSheet:Range(cRange):NumberFormat = fhelp.
                  chWorkSheet:Range(cRange):VALUE = DECIMAL(decichar) NO-ERROR.
                  craknare = chWorkSheet:Range(cRange):VALUE.
                  IF chWorkSheet:COLUMNS(allac[raknare]):ColumnWidth < LENGTH(craknare) THEN DO: 
                     chWorkSheet:COLUMNS(allac[raknare]):ColumnWidth = LENGTH(craknare).
                  END.
                  
               END. 
               {EXCELFEL.I}
            END.
         END.
         exceltempcolh = exceltempcolh:NEXT-COLUMN.         
      END.              
      excelbrwqh:GET-NEXT(NO-LOCK).
      raknare = 0.
   END.

END PROCEDURE.

/*se VISINKOPU.W för taggar även visinkopapp.p för hur taggarna sätts*/
PROCEDURE postexcel_UI:
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.  /*ej numerisktfält*/
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER nyrad             AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER colvar            AS INTEGER NO-UNDO.
   IF nyrad = FALSE THEN iRad = iRad - 1.
   cRange = allac[colvar] + cRad.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   cRange = allac[colvar] + cRad.

   IF varnummer = TRUE THEN chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range(cRange):VALUE = varut NO-ERROR.
   {EXCELFEL.I}

END PROCEDURE.

   /*se visainkopu.w för taggar även visinkopapp.p för hur taggarna sätts*/
PROCEDURE posterexcel_UI:
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hjchar AS CHARACTER NO-UNDO.
   IF bladvar = 1 THEN bladvarkoll = TRUE.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   raknare = 1.
   REPEAT:
      {EXCELFEL.I}
      cRange = allac[raknare] + cRad.
      IF estartnr[raknare] = 0 THEN LEAVE.
      IF estartnr[raknare + 1] = 0 THEN LEAVE.
      IF allachar[raknare] = TRUE OR varnummer = TRUE THEN chWorkSheet:Range(cRange):NumberFormat = "@".
      IF allac[raknare] = helaben THEN DO:
         IF SUBSTRING(varut,150,50) NE "" THEN chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,150,50) NO-ERROR.
         ELSE chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut, estartnr[raknare], estartnr[raknare + 1] - estartnr[raknare]) NO-ERROR.         
      END.
      ELSE DO:
         /*
         IF helaben = "" THEN chWorkSheet:Range(cRange):Value = SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 1] - estartnr[raknare]).
         ELSE DO:
            IF SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 1] - estartnr[raknare]) = "" THEN musz = musz.
            ELSE chWorkSheet:Range(cRange):Value = SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 1] - estartnr[raknare]).
         END.
         */
         IF varblanknoll = TRUE THEN DO:
            IF SUBSTRING(varut,estartnr[raknare], estartnr[raknare + 1] - estartnr[raknare]) = "" THEN DO:
               IF estartnr[raknare] >= varstartblanknoll AND estartnr[raknare] <= varslutblanknoll THEN DO:
                  SUBSTRING(varut, estartnr[raknare], estartnr[raknare + 1] - estartnr[raknare]) =
                     FILL(" ",estartnr[raknare + 1] - (estartnr[raknare] + 1)) + "0".
               END.
            END.
         END.  
         /*Berli4ex.p använder följande kod*/
         IF allac[raknare] = coltextrange THEN DO:
            IF coltextrange NE "" THEN DO:
               coltextrange = coltextrange + cRad.
               chWorkSheet:Range(coltextrange):VALUE = TRIM(SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 2] - estartnr[raknare])) NO-ERROR.
               raknare = raknare + 1.
               coltextrange = " ".
               radklar = TRUE.
            END.
         END.

         IF allac[raknare] = coltext AND helafalt = TRUE THEN DO:
            IF coltext = "A" THEN DO:
               chWorkSheet:Range(cRange):VALUE = TRIM(SUBSTRING(varut,1,150)) NO-ERROR.
            END.
            ELSE IF coltext = "B" THEN DO:
               chWorkSheet:Range(cRange):VALUE = TRIM(SUBSTRING(varut,18,150)) NO-ERROR.
            END.
            ELSE IF coltext = "C" THEN DO:
               chWorkSheet:Range(cRange):VALUE = TRIM(SUBSTRING(varut,47,150)) NO-ERROR.
            END.
            ELSE IF coltext = "E" THEN DO:
               chWorkSheet:Range(cRange):VALUE = TRIM(SUBSTRING(varut,57,150)) NO-ERROR.
            END.
            ELSE IF bermtrlvar = TRUE THEN DO:
               chWorkSheet:Range(cRange):VALUE = TRIM(SUBSTRING(varut,18,150)) NO-ERROR.
            END.
            radklar = TRUE. /*Inget mer på den raden*/
         END.
         ELSE IF radklar = FALSE THEN DO:
            IF TRIM(SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 1] - estartnr[raknare])) NE "" THEN
            chWorkSheet:Range(cRange):Value = TRIM(SUBSTRING(varut,estartnr[raknare],estartnr[raknare + 1] - estartnr[raknare])) NO-ERROR.
         END.
         radklar = FALSE.         
      END.
      raknare = raknare + 1.
   END.
   
   cRange = slutc + cRad.
   IF allachar[raknare] = TRUE OR varnummer = TRUE THEN chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
   IF allac[raknare] = helaben THEN DO:
      IF SUBSTRING(varut,150,50) NE "" THEN chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,150,150) NO-ERROR.
      ELSE chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,estartnr[raknare], estartnr[raknare + 1] - estartnr[raknare]) NO-ERROR.
   END.
   ELSE DO:
      IF varblanknoll = TRUE THEN DO:
         IF SUBSTRING(varut,estartnr[raknare],slutbredd - 1) = "" THEN DO:
            IF estartnr[raknare] >= varstartblanknoll AND estartnr[raknare] <= varslutblanknoll THEN DO:
               SUBSTRING(varut,estartnr[raknare], slutbredd - 1) = FILL(" ", slutbredd) + "0".
            END.
         END.
      END.      
      IF radklar = FALSE THEN DO:
   
         chWorkSheet:Range(cRange):VALUE = TRIM( SUBSTRING(varut,estartnr[raknare],slutbredd - 1)) NO-ERROR. /*Slutkolumn*/
      END.
   END.
   {EXCELFEL.I}
/*    RELEASE OBJECT chWorksheet NO-ERROR. */
END PROCEDURE.
PROCEDURE speunderstryk_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varstryk AS INTEGER NO-UNDO. /*3= över 4= under*/
   DEFINE INPUT PARAMETER vartjock AS INTEGER NO-UNDO. /*2= tunn 4= tjock*/
   DEFINE INPUT PARAMETER ustart AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER uslut AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
   IF vart = 1 THEN DO:
      cRange = ustart + cRad.
      radnrS = uslut + cRad.   /*Slutkolumn*/
      cRangefont = cRange + ":" + radnrS.
      chWorkSheet:Range(cRangefont):Borders(varstryk):Weight = vartjock NO-ERROR.
   END.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE speposter_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
   iRad = iRad - 1.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   IF vart = 1 THEN DO:
      cRange = allac[16] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,200,20) NO-ERROR.
      cRange = allac[17] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@".
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,220,20) NO-ERROR.
      cRange = allac[18] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@".
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,240,20) NO-ERROR.
   END.
   {EXCELFEL.I}   
END PROCEDURE.

PROCEDURE speposter2_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
   iRad = iRad - 1.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   IF vart = 1 THEN DO:
      cRange = allac[26] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,300,20) NO-ERROR.
      cRange = allac[27] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,320,20) NO-ERROR.
      cRange = allac[28] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,340,20) NO-ERROR.
   END.
   {EXCELFEL.I}   
END PROCEDURE.

PROCEDURE speposter3_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
   iRad = iRad - 1.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   IF vart = 1 THEN DO:
      
      IF SUBSTRING(varut,260,20) NE "" THEN DO:
         cRange = allac[19] + cRad.
         chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
         chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,260,20) NO-ERROR.
      END.
      IF SUBSTRING(varut,280,20) NE "" THEN DO:
         cRange = allac[20] + cRad.
         chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
         chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,280,20) NO-ERROR.
      END.         
   END.
   {EXCELFEL.I}   
END PROCEDURE.

PROCEDURE speposter4_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
   iRad = iRad - 1.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   IF vart = 1 THEN DO:
      cRange = allac[10] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,260,20) NO-ERROR.      
   END.
   {EXCELFEL.I}   
END PROCEDURE.

PROCEDURE speposter5_UI:
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varfont           AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varsize           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varbold           AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varhojd           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varfarg           AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varnummer         AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varblanknoll      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER varstartblanknoll AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER varslutblanknoll  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE hcrad AS CHARACTER NO-UNDO.
 
   iRad = iRad - 1.
   RUN fontexcel_UI (INPUT varfont,INPUT varsize,INPUT varbold,INPUT varhojd,INPUT varfarg).
   IF vart = 1 THEN DO:
      chWorkSheet:Range(allac[1] + cRad):VALUE = "" NO-ERROR.
      chWorkSheet:Range(allac[2] + cRad):VALUE = "" NO-ERROR.
      chWorkSheet:Range(allac[3] + cRad):VALUE = "" NO-ERROR.
      chWorkSheet:Range(allac[4] + cRad):VALUE = "" NO-ERROR.
      cRange = allac[1] + cRad  + ":" + allac[3] + cRad.
            
      chWorksheetRange = chWorkSheet:Range(cRange) NO-ERROR.            
      chWorksheetRange:WrapText = TRUE NO-ERROR.
      chWorksheetRange:Orientation = 0 NO-ERROR.
      chWorksheetRange:AddIndent = FALSE NO-ERROR.
      chWorksheetRange:ShrinkToFit = FALSE NO-ERROR.
      chWorksheetRange:MergeCells = TRUE NO-ERROR.             
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,1,15) NO-ERROR.      
   END.
   {EXCELFEL.I}   
END PROCEDURE.

PROCEDURE valueDataOut:
   DEFINE INPUT  PARAMETER outpos AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER outdata AS CHARACTER NO-UNDO.
   chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
END PROCEDURE.

PROCEDURE put_UI :
   DEFINE INPUT PARAMETER colut AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER varut             AS CHARACTER NO-UNDO.
   cRange = colut + STRING(iRad).
   chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range(cRange):VALUE = varut NO-ERROR.      
   
   {EXCELFEL.I}  
END PROCEDURE.

PROCEDURE sidbryt_UI:
   chCell = chWorkSheet:Range(cRange) NO-ERROR.
   chPageBreak = chWorkSheet:HPageBreaks:ADD(chCell) NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   RELEASE OBJECT chPageBreak NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
{PROEXCELFARG.I}
/*bakgrundsfärg alla col*/
PROCEDURE bgcell_UI:
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   cRad = STRING(iRad).
   cRange2 = startc + cRad + ":" + slutc + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   chCell:Interior:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
/*FÄLTFÄRG*/
PROCEDURE bgcellfarg_UI:   
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bgfg AS LOGICAL NO-UNDO.   
   cRad = STRING(iRad).
   cRange2 = stcol + cRad + ":" + stcol + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   IF bgfg = FALSE THEN chCell:FONT:ColorIndex = bgfarg NO-ERROR.
   ELSE IF bgfg = TRUE THEN chCell:Interior:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
/*hEL COL FÄRG*/
PROCEDURE bgcellcol_UI:   
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bgfg AS LOGICAL NO-UNDO. 
   cRad = STRING(iRad).
   cRange2 = stcol + ":" + stcol.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   IF bgfg = FALSE THEN chCell:FONT:ColorIndex = bgfarg NO-ERROR.
   ELSE IF bgfg = TRUE THEN chCell:Interior:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

/*bakgrundsfärg från col till col*/
PROCEDURE bgcellc_UI:
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER slcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   cRad = STRING(iRad).
   cRange2 = stcol + cRad + ":" + slcol + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   chCell:Interior:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

/*bakgrundsfärg från col*/
PROCEDURE bgcellc3_UI:   
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   
   cRad = STRING(iRad).
   cRange2 = stcol + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   chCell:Interior:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

/*förgrundsfärg från col till col*/
PROCEDURE bgcell2_UI:   
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER slcol AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.   
   cRad = STRING(iRad).
   cRange2 = stcol + cRad + ":" + slcol + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   chCell:FONT:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
/*förgrundsfärg från col*/
PROCEDURE bgcell3_UI:   
   DEFINE INPUT PARAMETER stcol AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bgfarg AS INTEGER NO-UNDO.
   
   cRad = STRING(iRad).
   cRange2 = stcol + cRad.
   chCell = chWorkSheet:Range(cRange2) NO-ERROR.
   chCell:FONT:ColorIndex = bgfarg NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE sidbrytbredd_UI:
   DEFINE INPUT PARAMETER orientvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE num AS INTEGER NO-UNDO.
   chWorkSheet:SELECT NO-ERROR.
   chWorkSheet:PageSetup:Orientation = orientvar NO-ERROR .
   chWindow:VIEW = 2 NO-ERROR.
   chPageBreak = chWorkSheet:VPageBreaks:ITEM(1) NO-ERROR.
   chPageBreak:DragOff(-4161,1) NO-ERROR.
   chWindow:VIEW = 1 NO-ERROR.
   /*
   RELEASE OBJECT chWindow NO-ERROR.
   chWindow = ?.
   */
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE brwexcelrubriker_UI :
   DEFINE INPUT PARAMETER dynbrwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER saveas AS LOGICAL NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE labelvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
   DEFINE VARIABLE bg AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).  
   labelvar = "".
   tempcounter = 0.
   slutbredd = 0.
   DO WHILE VALID-HANDLE(tempcolh):
      tempcounter = tempcounter + 1. 
      IF bredd[tempcounter] < tempcolh:WIDTH THEN bredd[tempcounter] = tempcolh:WIDTH.
      IF bredd[tempcounter] < LENGTH(STRING(tempcolh:LABEL)) THEN bredd[tempcounter] = LENGTH(STRING(tempcolh:LABEL)).
      IF tempcolh:COLUMN-FGCOLOR = ? THEN.
      ELSE IF tempcolh:COLUMN-FGCOLOR = 0 THEN.
      ELSE DO:
         RUN profexcelfarg_UI (INPUT tempcolh:COLUMN-FGCOLOR,OUTPUT bg). 
         RUN bgcell3_UI (INPUT allac[tempcounter], INPUT bg). 
      END.
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   IF slutbredd < bredd[tempcounter] THEN slutbredd = bredd[tempcounter].
   breddantal = tempcounter.
    tempcolh = dynbrwh:FIRST-COLUMN. 
   tempcounter = 0.
   labelvar = "".  
   cRange = allac[1] + STRING(irad) + ":" + allac[breddantal] + STRING(irad).
   chWorkSheet:Range(cRange):FONT:Bold = YES NO-ERROR.
   chWorkSheet:Range(cRange):RowHeight = 20 NO-ERROR.
   DO WHILE VALID-HANDLE(tempcolh):   
      tempcounter = tempcounter + 1. 
      IF tempcounter > maxantalcolexcel THEN LEAVE.
      cRange = allac[tempcounter] + STRING(irad).
      chWorkSheet:Range(cRange):VALUE = REPLACE(tempcolh:LABEL,"!"," ") NO-ERROR.
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   RUN dubbelrad_UI.
   tempcounter = irad.
END PROCEDURE.

PROCEDURE brwexcelquery_UI :
   DEFINE INPUT PARAMETER dynbrwh AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE VARIABLE CustomQuery AS HANDLE NO-UNDO.
   CREATE QUERY CustomQuery.
   CustomQuery:SET-BUFFERS(dynbrwexcelbuffh).
   CustomQuery:QUERY-PREPARE(q).
   CustomQuery:QUERY-OPEN().
   irad = irad + 1. 
   CustomQuery:GET-FIRST().
   DO WHILE CustomQuery:QUERY-OFF-END = FALSE:
      dubbelradhj = FALSE.
      RUN brwexceldata_UI (INPUT dynbrwh,INPUT irad).
      irad = irad + 1.
      CustomQuery:GET-NEXT().
   END.
END PROCEDURE.
PROCEDURE brwexceldata_UI :
   DEFINE INPUT PARAMETER dynbrwh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER radervar AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE decideci AS DECIMAL NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE exeluth AS HANDLE NO-UNDO.
   DEFINE VARIABLE bg AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).
   exeluth = dynbrwexcelbuffh.
   tempcounter = 0.
   DO WHILE VALID-HANDLE(tempcolh):
      tempcounter = tempcounter + 1. 
      IF tempcounter > maxantalcolexcel THEN LEAVE.
      IF tempcolh:NAME = ? THEN.
      ELSE DO:
         tempbufh = exeluth:BUFFER-FIELD(tempcolh:NAME).
         IF tempbufh:BUFFER-VALUE = ? THEN.
         ELSE DO:
            
            cRange = allac[tempcounter] + STRING(radervar).
            IF STRING(tempbufh:BUFFER-VALUE) = "" THEN.
            ELSE IF STRING(tempcolh:NAME) = "DELNR" THEN DO:
               decichar =  STRING(tempbufh:BUFFER-VALUE, Guru.Konstanter:varforetypchar[1]).
               IF Guru.Konstanter:varforetypchar[1] = "999"       THEN   decichar =  STRING(tempbufh:BUFFER-VALUE,"999").        
               ELSE IF Guru.Konstanter:varforetypchar[1] = "9999" THEN   decichar =  STRING(tempbufh:BUFFER-VALUE,"9999").
               ELSE IF Guru.Konstanter:varforetypchar[1] = ">99"  THEN   decichar =  STRING(tempbufh:BUFFER-VALUE,">99").
               ELSE IF Guru.Konstanter:varforetypchar[1] = ">>99"  THEN   decichar =  STRING(tempbufh:BUFFER-VALUE,">>99").
               ELSE IF Guru.Konstanter:varforetypchar[1] = ">>>99"  THEN   decichar =  STRING(tempbufh:BUFFER-VALUE,">>>99").
               chWorkSheet:Range(cRange):NumberFormat = "@".
               /*
               chWorkSheet:Range(cRange):VALUE = " " + decichar NO-ERROR.
               chWorkSheet:Range(cRange):VALUE = "A" NO-ERROR.
               */
               chWorkSheet:Range(cRange):VALUE = decichar NO-ERROR.
            END.   
            ELSE IF STRING(tempbufh:BUFFER-VALUE) = "0" THEN.            
            ELSE IF tempcolh:DATA-TYPE = "DECIMAL" AND ROUND(tempbufh:BUFFER-VALUE,0) = 0 THEN.
            ELSE IF tempcolh:DATA-TYPE = "INTEGER"   AND tempbufh:BUFFER-VALUE = 0 THEN.
            ELSE DO:
               IF tempcolh:DATA-TYPE = "DECIMAL" THEN DO:  
                  /*
                  decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE,2)),".",",").               
                  */
                  decichar = STRING(ROUND(tempbufh:BUFFER-VALUE,2)).
                  chWorkSheet:Range(cRange):VALUE = decichar NO-ERROR.
               END.
               ELSE IF tempcolh:DATA-TYPE = "DATE" THEN DO:                                    
                  decichar = STRING(tempbufh:BUFFER-VALUE,"9999/99/99").               
                  chWorkSheet:Range(cRange):VALUE = decichar NO-ERROR.
               END.
               ELSE DO:
                  chWorkSheet:Range(cRange):NumberFormat = "@".
                  chWorkSheet:Range(cRange):VALUE = STRING(tempbufh:BUFFER-VALUE) NO-ERROR. 
               END.
                 
            END.
            IF tempcolh:COLUMN-FGCOLOR = ? THEN.
            ELSE IF tempcolh:COLUMN-FGCOLOR = 0 THEN.
            ELSE DO:
               RUN profexcelfarg_UI (INPUT tempcolh:COLUMN-FGCOLOR,OUTPUT bg). 
               RUN bgcell3_UI (INPUT allac[tempcounter], INPUT bg).               
            END.  
            DEBUGGER:SET-BREAK().
           
            IF bredd[tempcounter] < LENGTH(STRING(tempbufh:BUFFER-VALUE)) THEN bredd[tempcounter] = LENGTH(STRING(tempbufh:BUFFER-VALUE)).
           
         END.
      END.
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
END PROCEDURE.

PROCEDURE SaveAsPdf_UI:
   DEFINE INPUT  PARAMETER dirandfil AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER visa AS LOGICAL NO-UNDO.
   chworkbook:ExportAsFixedFormat(0,dirandfil,0,visa,,,,,) NO-ERROR. 

END PROCEDURE.

PROCEDURE SiffraBokstav_UI :
   DEFINE INPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   
   ascivarde = ASC(varde).
   /*siffror*/
   IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
   
   /*stora bokstäver*/
   ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
   
   /*små bokstäver*/
   ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
   
   /*Å Ä Ö å ä ö*/
   ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
      ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.
   ELSE artecken = FALSE.
END PROCEDURE.

/*
   Sub Makro1()
'
' Makro1 Makro
'

'
chWorkSheet:PAGESETUP:LEFTHEADER = head NO-ERROR.
chWorkbook:SaveAs(sparadocnamn,56,,,,,,,,).
    ActiveSheet.ExportAsFixedFormat Type:=xlTypePDF, Filename:= _
        "D:\FAKTURORGURU\efakt\kuvert.pdf", Quality:=xlQualityStandard, _
        IncludeDocProperties:=True, IgnorePrintAreas:=False, OpenAfterPublish:= _
        True
End Sub
ActiveWorkbook.ExportAsFixedFormat Type:=xlTypePDF FileName:="sales.pdf" Quality:=xlQualityStandard DisplayFileAfterPublish:=True 

*/
