  
/*------------------------------------------------------------------------
   File        : GuruExcelFunction.cls
   Purpose     : 
   Syntax      : 
   Description : Embeds an Excelsheet into your app
   Author(s)   : elpfh
   Created     : Thu Feb 02 15:55:13 CET 2012
   Notes       : Kontrollen måste initieras med Initialize() innan den kan användas
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Microsoft.Office.Interop.Excel.*.
USING System.Runtime.InteropServices.*.
USING System.Reflection.*.


CLASS Controls.GuruExcelFunction INHERITS UserControl: 
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
      
   

   
   CONSTRUCTOR PUBLIC GuruExcelFunction (  ):
    
      IF  Guru.GlobalaVariabler:plusaonr = "" OR  Guru.GlobalaVariabler:plusaonr = ? THEN aoutvar = "".
      ELSE aoutvar = Guru.Konstanter:gaok + ": " +  Guru.GlobalaVariabler:plusaonr + " " + STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1]).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

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
         stAddress = "".
         RETURN stAddress.
         
      END.
      stAddress = rnValue:Address(,,,,). 
      RELEASE OBJECT chCell NO-ERROR.
      RETURN stAddress.
           
   END METHOD. 
   
   DESTRUCTOR PUBLIC GuruExcelFunction ( ):

   END DESTRUCTOR.


END CLASS.
