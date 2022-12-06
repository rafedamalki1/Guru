 
 /*------------------------------------------------------------------------
    File        : Excelfunc.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Feb 06 14:02:45 CET 2012
    Notes       : 
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
       
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Global.Excelfunc: 
   /*   
   DEFINE PUBLIC STATIC PROPERTY felexcel AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC  PROPERTY chWorksheet        AS COM-HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY visavarexcel AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   */
   DEFINE PUBLIC VARIABLE felexcel AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE chWorksheet  AS COM-HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE visavarexcel AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
   DEFINE PUBLIC VARIABLE ColRad AS CHARACTER NO-UNDO.  /*aktuell column och rad ex. e7*/
   DEFINE PUBLIC VARIABLE cColname AS CHARACTER NO-UNDO. /*aktuell column ex. e*/
   DEFINE PUBLIC VARIABLE iColnum AS INTEGER NO-UNDO. /*aktuell nummer på columnen ex e = 5*/
   DEFINE PUBLIC VARIABLE iSistaColnum AS INTEGER NO-UNDO. /*den col som fins längs till höger*/
   DEFINE PUBLIC VARIABLE iRad AS INTEGER NO-UNDO. /*aktuell rad ex 7*/
 
      
   DEFINE VARIABLE link AS CHARACTER NO-UNDO.
   DEFINE VARIABLE totheight AS DECIMAL NO-UNDO.
   DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chChart            AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWorksheetRange   AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chPageBreak        AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE cActiveCell        AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE chWindow           AS COM-HANDLE NO-UNDO.   
   DEFINE VARIABLE dubbelradhj AS LOGICAL NO-UNDO.
   DEFINE VARIABLE aoutvar AS CHARACTER NO-UNDO FORMAT "X(30)".

   CONSTRUCTOR PUBLIC Excelfunc(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.    
      IF  Guru.GlobalaVariabler:plusaonr = "" OR  Guru.GlobalaVariabler:plusaonr = ? THEN aoutvar = "".
      ELSE aoutvar = Guru.Konstanter:gaok + ": " +  Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1]).
        
               
   END CONSTRUCTOR.
   METHOD PUBLIC VOID Openexcel(INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE.
      {OPENEXCELC.I}
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
      {EXCELFEL.I}
   END METHOD.   
   METHOD PUBLIC CHARACTER getcolname ( i AS INTEGER):
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
   END METHOD.
   METHOD PUBLIC INTEGER getcolnum ( i AS CHARACTER):
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
   END METHOD.
   METHOD PUBLIC CHARACTER Getrad(cRad AS CHARACTER):
      IF ASC(SUBSTRING(cRad,2,1)) >= 48 AND ASC(SUBSTRING(cRad,2,1)) <= 57 THEN RETURN SUBSTRING(cRad,2).
      RETURN SUBSTRING(cRad,3).
      
   END METHOD.
   METHOD PUBLIC INTEGER Getkol(cRad AS CHARACTER):      
      IF ASC(SUBSTRING(cRad,2,1)) >= 48 AND ASC(SUBSTRING(cRad,2,1)) <= 57 THEN RETURN getcolnum(SUBSTRING(cRad,1,1)).
      RETURN getcolnum(SUBSTRING(cRad,1,2)).
      
   END METHOD.
   
   
   METHOD PUBLIC VOID Rowdown():
      iRad = iRad + 1.
      ColumnRad().
   END METHOD.
   METHOD PUBLIC VOID Rowdup():
      iRad = iRad - 1.
      ColumnRad().
   END METHOD.
   METHOD PUBLIC VOID ColRight():
      iColnum = getcolnum(cColname) + 1. 
      cColname = getcolname(iColnum).     
      ColumnRad().
   END METHOD.
   METHOD PUBLIC VOID Colleft():
      iColnum = getcolnum(cColname) - 1. 
      cColname = getcolname(iColnum).     
      ColumnRad().
   END METHOD.
   METHOD PUBLIC VOID ColumnRad():
      iColnum = getcolnum(cColname).
      IF iSistaColnum < iColnum THEN iSistaColnum = iColnum.
      ColRad = cColname + STRING(iRad). 
       
   END METHOD.
   METHOD PUBLIC CHARACTER GetDatat():
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      invar = chWorkSheet:Range(ColRad):TEXT.
      RETURN invar.              
   END METHOD.
   METHOD PUBLIC CHARACTER GetDatat(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      invar = chWorkSheet:Range(outpos):TEXT.      
      RETURN invar.              
   END METHOD.
   METHOD PUBLIC VOID DataOut(outdata AS CHARACTER):
      chWorkSheet:Range(ColRad):VALUE = outdata NO-ERROR.        
   END METHOD.
   METHOD PUBLIC VOID DataOut(outpos AS CHARACTER,outdata AS CHARACTER):
      chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.        
   END METHOD.
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL, outdata AS CHARACTER):   
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = chWorkSheet:Range(ColRad):TEXT.
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      chWorkSheet:Range(ColRad):VALUE  = outdata NO-ERROR.              
   END METHOD.
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL,outpos AS CHARACTER,outdata AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = chWorkSheet:Range(outpos):TEXT.
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
     
   END METHOD.
   
   
   METHOD PUBLIC VOID FontExcel(cStart AS CHARACTER, cRangefont AS CHARACTER,varfont AS CHARACTER,varsize AS INTEGER,varbold AS LOGICAL,varhojd AS INTEGER,varfarg AS INTEGER ):
      
      IF varfont NE "" THEN chWorkSheet:Range(cStart,cRangefont):FONT:NAME = varfont NO-ERROR.
      IF varsize NE 0 THEN chWorkSheet:Range(cStart,cRangefont):FONT:SIZE = varsize NO-ERROR.
      IF varbold NE ? THEN chWorkSheet:Range(cStart,cRangefont):FONT:Bold = varbold NO-ERROR.
      IF varhojd NE 0 THEN chWorkSheet:Range(cStart,cRangefont):RowHeight = varhojd NO-ERROR.
      IF varfarg NE 0 THEN chWorkSheet:Range(cStart,cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
        
   END METHOD.
   METHOD PUBLIC VOID Summa(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE cinvar AS CHARACTER NO-UNDO.
              
      formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
      chWorkSheet:Range(cRangefont):FormulaR1C1 = formelvar.
      /*
      =SUM(RC[-3]:RC[-1]) hor
      =SUMMA(R[-2]C:R[-1]C) våg
   */
      IF decivar = ? THEN. 
      ELSE DO:
         cinvar = chWorkSheet:Range(cRangefont ):TEXT.
         IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
            cinvar = replace(cinvar,",",".").
         END.   
         IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN DO:
            cinvar = replace(cinvar,".",",").
         END.   
         invar = ROUND(DECIMAL(cinvar),decivar).
         
         chWorkSheet:Range(cRangefont):Formula = STRING(invar). 
      END.     
   
   END METHOD.
   METHOD PUBLIC VOID Summah(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE cinvar AS CHARACTER NO-UNDO.
          
      formelvar = "=SUMMA(RC[-" + STRING(INTEGER(Getkol(cRangefont)) - INTEGER(Getkol(cStart))) + "]:RC[-1])".
      chWorkSheet:Range(cRangefont):FormulaR1C1 = formelvar.
      /*
      =SUM(RC[-3]:RC[-1]) hor
      =SUMMA(R[-2]C:R[-1]C) våg
   */
      IF decivar = ? THEN. 
      ELSE DO:
         cinvar = chWorkSheet:Range(cRangefont ):TEXT.
         IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
            cinvar = replace(cinvar,",",".").
         END.   
         IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN DO:
            cinvar = replace(cinvar,".",",").
         END.   
         invar = ROUND(DECIMAL(cinvar),decivar).
         
         chWorkSheet:Range(cRangefont):Formula = STRING(invar). 
      END.     
   
   END METHOD.
   METHOD PUBLIC VOID ColWidth(cStart AS CHARACTER, widthsize AS DECIMAL):
      chWorkSheet:Range(cStart):ColumnWidth = widthsize.       
   END METHOD.
   METHOD PUBLIC DECIMAL ColWidth(cStart AS CHARACTER):
      DEFINE VARIABLE cwidth AS CHARACTER NO-UNDO.
      cwidth = STRING(chWorkSheet:Range(cStart):ColumnWidth).
      RETURN DECIMAL(cwidth).       
   END METHOD.
   METHOD PUBLIC VOID Valjblad(valbald AS INTEGER):
      chWorkSheet = chExcelApplication:Sheets:ITEM(valbald) NO-ERROR.
      {EXCELFEL.I} 
   END METHOD.
   METHOD PUBLIC CHARACTER sokiexcel(INPUT vad AS CHARACTER,INPUT startcol AS CHARACTER,INPUT slutcol AS CHARACTER):
      DEFINE VARIABLE stAddress AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rnValue AS COM-HANDLE NO-UNDO.
      startcol = startcol + ":" + slutcol.
      chCell = chWorkSheet:Range(startcol) NO-ERROR.
      rnValue = chCell:Find(vad,,,,,,,,). 
      IF rnValue = 0 THEN DO:
   
         RELEASE OBJECT chCell NO-ERROR.
         stAddress = "Ingen träff".
         RETURN stAddress.
         
      END.
      stAddress = rnValue:Address(,,,,). 
      RELEASE OBJECT chCell NO-ERROR.
      RETURN stAddress.
           
   END METHOD. 

/*   
   METHOD PUBLIC VOID Border(cStart AS CHARACTER, cRangefont AS CHARACTER):
      chWorkSheet:Range(cStart,cRangefont):BorderAround(XlLineStyle:xlContinuous, XlBorderWeight:xlMedium, XlColorIndex:xlColorIndexAutomatic, System.Reflection.Missing:Value).
        
   END METHOD.
  
   METHOD PUBLIC VOID Linedubble(cStart AS CHARACTER, cRangefont AS CHARACTER):
      Elpool.Helpers:GetBorder(chWorkSheet:Range(cStart,cRangefont), XlBordersIndex:xlEdgeBottom):LineStyle = XlLineStyle:xlDouble.       
   END METHOD.
  /*anders typ???*/
   METHOD PUBLIC VOID BordHoriz(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
       Elpool.Helpers:GetBorder(chWorkSheet:Range( cStart,cRangefont  ), XlBordersIndex:xlInsideHorizontal):LineStyle = XlLineStyle:xlDot.
      
   END METHOD.
   METHOD PUBLIC VOID BordHorizW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      Elpool.Helpers:GetBorder(chWorkSheet:Range( cStart,cRangefont), XlBordersIndex:xlInsideHorizontal):Weight = XlBorderWeight:xlThin.
   END METHOD.
   METHOD PUBLIC VOID BordVert(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      Elpool.Helpers:GetBorder(chWorkSheet:Range( cStart,cRangefont), XlBordersIndex:xlInsideVertical):LineStyle = XlLineStyle:xlDot.
   END METHOD.
   METHOD PUBLIC VOID BordVertW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      Elpool.Helpers:GetBorder(chWorkSheet:Range( cStart,cRangefont), XlBordersIndex:xlInsideVertical):Weight = XlBorderWeight:xlThin.
   END METHOD.
   */
   /*
   METHOD PUBLIC INTEGER allacb (iab AS INTEGER,INPUT ab AS CHARACTER ) :      
      iab = iab + 1.
      IF iab > maxantalcolexcel THEN.
      ELSE allac[iab] = ab.
      RETURN iab.
   END METHOD.  
   METHOD PUBLIC  VOID allac_UI () :
      DEFINE VARIABLE iab AS INTEGER NO-UNDO.      
      iab = allacb (iab, "A").        
      iab = allacb (iab, "B").             
      iab = allacb (iab, "C").         
      iab = allacb (iab, "D").         
      iab = allacb (iab, "E").         
      iab = allacb (iab, "F").         
      iab = allacb (iab, "G").         
      iab = allacb (iab, "H").          
      iab = allacb (iab, "I").          
      iab = allacb (iab, "J").         
      iab = allacb (iab, "K").         
      iab = allacb (iab, "L").         
      iab = allacb (iab, "M").         
      iab = allacb (iab, "N").         
      iab = allacb (iab, "O").         
      iab = allacb (iab, "P").         
      iab = allacb (iab, "Q").         
      iab = allacb (iab, "R").         
      iab = allacb (iab, "S").         
      iab = allacb (iab, "T").         
      iab = allacb (iab, "U").         
      iab = allacb (iab, "V").         
      iab = allacb (iab, "W").         
      iab = allacb (iab, "X").         
      iab = allacb (iab, "Y").         
      iab = allacb (iab, "Z").         
      iab = allacb (iab, "AA").        
      iab = allacb (iab, "AB").
      iab = allacb (iab, "AC").          
      iab = allacb (iab, "AD").          
      iab = allacb (iab, "AE").          
      iab = allacb (iab, "AF").          
      iab = allacb (iab, "AG").          
      iab = allacb (iab, "AH").          
      iab = allacb (iab, "AI").          
      iab = allacb (iab, "AJ").         
      iab = allacb (iab, "AK").         
      iab = allacb (iab, "AL").         
      iab = allacb (iab, "AM").         
      iab = allacb (iab, "AN").         
      iab = allacb (iab, "AO").         
      iab = allacb (iab, "AP").         
      iab = allacb (iab, "AQ").         
      iab = allacb (iab, "AR").         
      iab = allacb (iab, "AS").         
      iab = allacb (iab, "AT").         
      iab = allacb (iab, "AU").         
      iab = allacb (iab, "AV").         
      iab = allacb (iab, "AW").         
      iab = allacb (iab, "AX").         
      iab = allacb (iab, "AY").         
      iab = allacb (iab, "AZ"). 
      iab = allacb (iab, "BA").        
      iab = allacb (iab, "BB").
      iab = allacb (iab, "BC").          
      iab = allacb (iab, "BD").          
      iab = allacb (iab, "BE").          
      iab = allacb (iab, "BF").          
      iab = allacb (iab, "BG").          
      iab = allacb (iab, "BH").          
      iab = allacb (iab, "BI").          
      iab = allacb (iab, "BJ").         
      iab = allacb (iab, "BK").         
      iab = allacb (iab, "BL").         
      iab = allacb (iab, "BM").         
      iab = allacb (iab, "BN").         
      iab = allacb (iab, "BO").         
      iab = allacb (iab, "BP").         
      iab = allacb (iab, "BQ").         
      iab = allacb (iab, "BR").         
      iab = allacb (iab, "BS").         
      iab = allacb (iab, "BT").         
      iab = allacb (iab, "BU").         
      iab = allacb (iab, "BV").         
      iab = allacb (iab, "BW").         
      iab = allacb (iab, "BX").         
      iab = allacb (iab, "BY").         
      iab = allacb (iab, "BZ").       
      iab = allacb (iab, "CA").        
      iab = allacb (iab, "CB").
      iab = allacb (iab, "CC").          
      iab = allacb (iab, "CD").          
      iab = allacb (iab, "CE").          
      iab = allacb (iab, "CF").          
      iab = allacb (iab, "CG").          
      iab = allacb (iab, "CH").          
      iab = allacb (iab, "CI").          
      iab = allacb (iab, "CJ").         
      iab = allacb (iab, "CK").         
      iab = allacb (iab, "CL").         
      iab = allacb (iab, "CM").         
      iab = allacb (iab, "CN").         
      iab = allacb (iab, "CO").         
      iab = allacb (iab, "CP").         
      iab = allacb (iab, "CQ").         
      iab = allacb (iab, "CR").         
      iab = allacb (iab, "CS").         
      iab = allacb (iab, "CT").         
      iab = allacb (iab, "CU").         
      iab = allacb (iab, "CV").         
      iab = allacb (iab, "CW").         
      iab = allacb (iab, "CX").         
      iab = allacb (iab, "CY").         
      iab = allacb (iab, "CZ").
             
   END METHOD.      
   METHOD PUBLIC CHARACTER getcolname ( i AS INTEGER):
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
   END METHOD.
   METHOD PUBLIC STATIC VOID visaexel ( vis AS LOGICAL):
      visavarexcel = vis.
   END METHOD.
  
   METHOD PUBLIC STATIC VOID excelhead (leftcenterrightin AS INTEGER, headin AS CHARACTER):
      DEFINE VARIABLE head AS CHARACTER NO-UNDO.
      DEFINE VARIABLE leftcenterright AS INTEGER NO-UNDO.
      /*SIDHUVUD*/
      ASSIGN 
      leftcenterright = leftcenterrightin
      head = headin + " sid &S".
      IF leftcenterright = 1 THEN chWorkSheet:PAGESETUP:LEFTHEADER = head NO-ERROR.
      IF leftcenterright = 2 THEN chWorkSheet:PAGESETUP:CENTERHEADER = head NO-ERROR.
      IF leftcenterright = 3 THEN chWorkSheet:PAGESETUP:RIGHTHEADER = head NO-ERROR.
      {EXCELFEL.I}
   END METHOD.
   
   */
   
 /*

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
   chWorksheet:PageSetup:PrintGridlines = print.  
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
   chWorksheet:PageSetup:PaperSize = storlek.      
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

PROCEDURE delCol_UI :
   DEFINE INPUT  PARAMETER startcol AS INTEGER NO-UNDO.
   /*DEFINE INPUT  PARAMETER antal AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER hurofta AS INTEGER NO-UNDO.*/
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



/*För att se screenvalue när det är klart*/
PROCEDURE screenexcel_UI.
   chExcelApplication:ScreenUpdating = TRUE.
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
   
   CREATE "Excel.Application" chExcelApplication.
   {OPENEXCEL.I}
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   ASSIGN chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
   ASSIGN iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE open_UI:
   DEFINE INPUT  PARAMETER kommando AS CHARACTER  NO-UNDO.
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = FALSE.
   {OPENEXCEL.I}
   /*
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   */
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWorkSheet:Columns("a:ab"):EntireColumn:AutoFit NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE startexcel_UI:
   
   felexcel = FALSE.
   excelmus = SESSION:SET-WAIT-STATE("GENERAL").
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = TRUE.
   chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
   
   RUN nyttbladexcel_UI.   
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
   
   RUN nyttbladexcel_UI.   
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
   RUN nyttbladexcel_UI.   
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
   sparadocnamn = sparadocnamn +  Guru.Konstanter:globanv  + STRING(TIME) + ".xls".   
   chExcelApplication:VISIBLE = FALSE.
   chExcelApplication:displayalerts = FALSE.
   chWorkbook:SaveAs(sparadocnamn,56,,,,,,,,).
   ExecuteExcel4Macro "PRINT(1,,,1,,,,,,,,3,,,TRUE,,FALSE)" alla blad
ExecuteExcel4Macro "PRINT(1,,,1,,,,,,,,2,,,TRUE,,FALSE)"    aktivt blad
   expression.PrintOut(From, To, Copies, Preview, ActivePrinter, PrintToFile, Collate, PrToFileName)

 
   */
   
   IF vad = 1 THEN chWorkbook:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).
   IF vad = 2 THEN chWorkSheet:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).
  
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

                   /*
PROCEDURE slutmedsaveas_UI :
   sparadocnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN sparadocnamn = webclienttempdir.
   OS-CREATE-DIR VALUE(sparadocnamn) NO-ERROR.
   sparadocnamn = sparadocnamn +  Guru.Konstanter:globanv  + STRING(TIME) + ".xls".   
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
      cRange = allac[19] + cRad.
      chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
      chWorkSheet:Range(cRange):VALUE = SUBSTRING(varut,260,20) NO-ERROR.      
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
   chWorkSheet:PageSetup:Orientation = orientvar NO-ERROR.
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


*/


   
   
   
   
   
   DESTRUCTOR PUBLIC Excelfunc ( ):

   END DESTRUCTOR.
END CLASS.

