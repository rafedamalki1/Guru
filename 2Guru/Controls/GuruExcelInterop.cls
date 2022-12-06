 
 /*------------------------------------------------------------------------
    File        : GuruExcelInterop.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Wed Mar 30 14:41:36 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Microsoft.Office.Interop.Excel.*.
USING Infragistics.Documents.Excel.*.

USING System.Runtime.InteropServices.*.
USING System.Reflection.*.


CLASS Controls.GuruExcelInterop INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE ExcelAppHandle     AS Application                               NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWbHandle      AS Workbook                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWorkbooks     AS Workbooks                                 NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWorksheets    AS Sheets                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWorksheet     AS System.Object                             NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelActiveWindow  AS Window                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelRange         AS Range                                     NO-UNDO.   
   
   
   
   
   
   DEFINE PUBLIC  PROPERTY direkticell        AS LOGICAL                                   NO-UNDO
   PUBLIC GET. PUBLIC SET.   
   
   DEFINE         VARIABLE bladvar            AS INTEGER                                   NO-UNDO.   
   DEFINE         VARIABLE antalbladstart     AS INTEGER                                   NO-UNDO.
  
   DEFINE         VARIABLE dubbelradhj        AS LOGICAL                                   NO-UNDO.
   DEFINE         VARIABLE BladvarKoll        AS LOGICAL                                   NO-UNDO.  
   DEFINE PUBLIC  VARIABLE SaveColRad             AS CHARACTER                              NO-UNDO.  /*sparad column och rad ex. e7*/
   DEFINE PUBLIC  VARIABLE ColRad             AS CHARACTER                                 NO-UNDO.  /*aktuell column och rad ex. e7*/
   DEFINE PUBLIC  VARIABLE cColName           AS CHARACTER                                 NO-UNDO. /*aktuell column ex. e*/
   DEFINE PUBLIC  VARIABLE iColNum            AS INTEGER                                   NO-UNDO. /*aktuell nummer på columnen ex e = 5*/
   DEFINE PUBLIC  VARIABLE iSistaColnum       AS INTEGER                                   NO-UNDO. /*den col som fiNns längs till höger*/
   DEFINE VARIABLE iSaveRad                   AS INTEGER                                   NO-UNDO. /*sparrad*/
   DEFINE PUBLIC  VARIABLE iRad               AS INTEGER                                   NO-UNDO. /*aktuell rad ex 7*/
   
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
      INDEX KOLUMN    KOLUMN    RAD
      INDEX RAD       RAD       KOLUMNNUMMER 
      INDEX KOLUMNRAD KOLUMNRAD.
   DEFINE BUFFER excellTTbuff FOR excellTT. 
   
   CONSTRUCTOR PUBLIC GuruExcelInterop (  ):
      SUPER().
      
      THIS-OBJECT:antalbladstart = 4.
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID InteropInitialize (path AS CHARACTER):
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         
         THIS-OBJECT:ExcelActiveWindow = THIS-OBJECT:ExcelAppHandle:ActiveWindow.
        
         THIS-OBJECT:ExcelWorkbooks = THIS-OBJECT:ExcelAppHandle:Workbooks.
         
         THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
         IF path = "" THEN THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelWorkbooks:ADD("").
         ELSE THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelWorkbooks:ADD(path).            
         THIS-OBJECT:ExcelWorksheets = THIS-OBJECT:ExcelWbHandle:Sheets.
         THIS-OBJECT:ExcelWbHandle:Activate().
         THIS-OBJECT:ExcelWorksheet = THIS-OBJECT:ExcelWbHandle:ActiveSheet.
         /*THIS-OBJECT:ExcelActiveWindow:DisplayWorkbookTabs = TRUE.*/
         THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
         THIS-OBJECT:ExcelAppHandle:Visible = TRUE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.
         
   END METHOD.
   
   METHOD PUBLIC VOID ReleaseExcel(stang AS LOGICAL):
      IF stang = TRUE THEN DO :
         THIS-OBJECT:ExcelWbHandle:CLOSE(FALSE,System.Reflection.Missing:Value,System.Reflection.Missing:Value) NO-ERROR. 
         THIS-OBJECT:ExcelWorkbooks:CLOSE() NO-ERROR. 
         THIS-OBJECT:ExcelAppHandle:Application:Quit() NO-ERROR.
         THIS-OBJECT:ExcelAppHandle:Quit() NO-ERROR.
      END.   
      
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelRange) NO-ERROR. 
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelWorksheet) NO-ERROR. 
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelWorksheets) NO-ERROR. 
      
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelWbHandle) NO-ERROR. 
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelWorkbooks) NO-ERROR. 
      
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelActiveWindow) NO-ERROR.  
      Marshal:ReleaseComObject(THIS-OBJECT:ExcelAppHandle) NO-ERROR. 
      
      THIS-OBJECT:ExcelRange = ? NO-ERROR.
      THIS-OBJECT:ExcelWorksheet = ? NO-ERROR.
      THIS-OBJECT:ExcelWorksheets = ? NO-ERROR.
      THIS-OBJECT:ExcelWbHandle = ? NO-ERROR.
      THIS-OBJECT:ExcelWorkbooks = ? NO-ERROR.
      THIS-OBJECT:ExcelActiveWindow = ? NO-ERROR.
      THIS-OBJECT:ExcelAppHandle = ? NO-ERROR.
      
   END METHOD.
   
   METHOD PUBLIC VOID SidBrytBredd(orientvar AS INTEGER):
      DEFINE VARIABLE PB  AS Microsoft.Office.Interop.Excel.VPageBreak.
      DEFINE VARIABLE num AS INTEGER                                   NO-UNDO.
         IF orientvar = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:Orientation = Microsoft.Office.Interop.Excel.XlPageOrientation:xlPortrait.
         THIS-OBJECT:GetWorkSheet():PageSetup:Orientation = Microsoft.Office.Interop.Excel.XlPageOrientation:xlLandscape.
         
         ExcelActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlNormalView.
         ExcelActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageBreakPreview.
   END METHOD.
      
   METHOD PUBLIC Microsoft.Office.Interop.Excel.Worksheet GetWorkSheet():
      RETURN CAST(THIS-OBJECT:ExcelWorksheet, Microsoft.Office.Interop.Excel.Worksheet).
   END METHOD.
      
   METHOD PUBLIC CHARACTER IhopCeller(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      /* 
      leftcenterright = 2 vänster
      leftcenterright = 3 centrerat
      leftcenterright = 4 höger 
      */
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),tillfran).
      THIS-OBJECT:ExcelRange:MergeCells = TRUE.
         
   END METHOD.   
   
   METHOD PUBLIC CHARACTER Center(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),tillfran).
      THIS-OBJECT:ExcelRange:HorizontalAlignment = leftcenterright. 
   /*leftcenterright = 4 höger 
   leftcenterright = 2 vänster
   leftcenterright = 3 centrerat*/
    END METHOD.
    
    METHOD  PUBLIC CHARACTER CenterUpDown(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),tillfran).
      THIS-OBJECT:ExcelRange:VerticalAlignment = leftcenterright.
   /*leftcenterright = 1 UPP 
   leftcenterright = 2 CENTER
   leftcenterright = 3 NER*/

    END METHOD.  
   METHOD PUBLIC CHARACTER valueDataIn(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN 
      DO:
         THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),outpos).
         invar = THIS-OBJECT:ExcelRange:TEXT. 
      END.
      ELSE 
      DO:
         FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = outpos NO-LOCK NO-ERROR.
         IF AVAILABLE excellTT THEN 
         DO:
            invar = excellTT.VARDET.
         END.   
      END.      
      RETURN invar.  
   END METHOD.
   
   METHOD PUBLIC VOID valueDataOutFlerrad(outpos AS CHARACTER,outdata AS CHARACTER):
      DEFINE VARIABLE radnr AS INTEGER NO-UNDO.
      DEFINE VARIABLE kolumnvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE outvar AS CHARACTER NO-UNDO.
      kolumnvar = Getkol(outpos).
      radnr = INTEGER(Getrad(outpos)) - 1.
      REPEAT: 
         IF INDEX(outdata,",",1) = 0 THEN DO: 
            IF outdata = "" THEN LEAVE.
            outvar = outdata.
            radnr = radnr + 1. 
            THIS-OBJECT:Rowdown().
            ColRad = kolumnvar + STRING(radnr).
            THIS-OBJECT:valueDataOut(ColRad,outvar).
            LEAVE.
         END.
         outvar = SUBSTRING(outdata,1,INDEX(outdata,",",1) - 1).
         outdata = SUBSTRING(outdata,INDEX(outdata,",",1) + 1).
         radnr = radnr + 1.
         THIS-OBJECT:Rowdown().
         ColRad = kolumnvar + STRING(radnr).
         THIS-OBJECT:valueDataOut(ColRad,outvar).
         
      END.   
           
   END METHOD.
   
   
   METHOD PUBLIC VOID valueDataOut(outpos AS CHARACTER,outdata AS CHARACTER):
      IF direkticell = TRUE THEN DO:
         THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),outpos).
         THIS-OBJECT:ExcelRange:VALUE2 = outdata.
      END.
      ELSE DO:
         FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = outpos NO-LOCK NO-ERROR.
         IF NOT AVAILABLE excellTT THEN DO:
            CREATE excellTT.
            ASSIGN 
            excellTT.KOLUMNRAD    = outpos
            excellTT.KOLUMN       = Getkol(outpos)
            excellTT.KOLUMNNUMMER = GetkolNum(outpos) 
            excellTT.RAD          = INTEGER(Getrad(outpos)).
         END.   
         ASSIGN 
         excellTT.VARDET = outdata.         
      END.      
   END METHOD.
   METHOD PUBLIC VOID valueDataOutlg(outpos AS CHARACTER,outlg AS logical):
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),outpos).
      THIS-OBJECT:ExcelRange:VALUE2 = outlg.
   END METHOD.
   
   /*skrivar ut all data från tt*/
   METHOD PUBLIC VOID valueDataOutTT():
      IF direkticell = TRUE THEN RETURN.
      FOR EACH excellTT WHERE NO-LOCK:
         IF excellTT.KOLUMNRAD = "" OR excellTT.KOLUMNRAD = ? THEN.
         ELSE 
         DO:
               /*            
               Elpool.Helpers:GetRange(GetWorkSheet(),excellTT.KOLUMNRAD):TEXT = excellTT.VARDET.
               */
               THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),excellTT.KOLUMNRAD).
               THIS-OBJECT:ExcelRange:VALUE2 = excellTT.VARDET.       
         END.   
      END.
      EMPTY TEMP-TABLE excellTT NO-ERROR.
   END METHOD.

   METHOD PUBLIC VOID PageSetuppHeaderImage(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      IF vad = 1 THEN 
      DO:
         THIS-OBJECT:GetWorkSheet():PageSetup:LeftHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).
         THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeader = "&V&G".            
      END.   
      IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).
      IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).                    
   END METHOD. 
   
   METHOD PUBLIC VOID PageSetuppHeader(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftHeader = detta.
      IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeader = detta.
      IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightHeader = detta.

   /*'&"Calibri"&30' */     
   END METHOD. 
   METHOD PUBLIC VOID PageSetuppFooter(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftFooter = detta.
      IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterFooter = detta.
      IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightFooter = detta.
   /*'&"Calibri"&30' */     
   END METHOD. 
   
   METHOD PUBLIC VOID PageSetuppMargin(INPUT vad AS INTEGER, INPUT detta AS DECIMAL):
      IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftMargin = detta.
      IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:TopMargin = detta.
      IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightMargin = detta.
      IF vad = 4 THEN THIS-OBJECT:GetWorkSheet():PageSetup:BottomMargin = detta.
      IF vad = 5 THEN THIS-OBJECT:GetWorkSheet():PageSetup:HeaderMargin = detta.
      IF vad = 6 THEN THIS-OBJECT:GetWorkSheet():PageSetup:FooterMargin = detta.
   /*'&"Calibri"&30' */     
   END METHOD. 
    
   METHOD PUBLIC VOID PageSetSidnum(INPUT vad AS INTEGER, topdown AS LOGICAL):
      DEFINE VARIABLE detta AS CHARACTER NO-UNDO.
      detta = " sid &S".
         IF topdown = TRUE THEN 
         DO:
            IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftHeader = detta.
            IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeader = detta.
            IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightHeader = detta.
         END.   
         ELSE 
         DO:
            IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftFooter = detta.
            IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterFooter = detta.
            IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightFooter = detta.
         END.              
   /*'&"Calibri"&30' */     
   END METHOD. 
   
   METHOD PUBLIC VOID ViewType(INPUT vad AS INTEGER):
      /* VAD 1 = vad 2 från gamla och tvärt om */
         IF vad = 1 THEN  THIS-OBJECT:ExcelActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageBreakPreview.
         IF vad = 2 THEN  THIS-OBJECT:ExcelActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlNormalView.
         IF vad = 3 THEN  THIS-OBJECT:ExcelActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageLayoutView.      
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
      IF i = 23 THEN RETURN "W".
      IF i = 24 THEN RETURN "x".
      IF i = 25 THEN RETURN "y".
      IF i = 26 THEN RETURN "z".
      IF i = 27 THEN RETURN "aa".
      IF i = 28 THEN RETURN "ab".
      IF i = 29 THEN RETURN "ac".
      IF i = 30 THEN RETURN "ad".
      IF i = 31 THEN RETURN "ae".
      IF i = 32 THEN RETURN "af".
      IF i = 33 THEN RETURN "ag".
      IF i = 34 THEN RETURN "ah".
      IF i = 35 THEN RETURN "ai".
      IF i = 36 THEN RETURN "aj".
      /*behövs fler kolumner till infra schakt Lena 20210517 */
      IF i = 37 THEN RETURN "ak".
      IF i = 38 THEN RETURN "al".
      IF i = 39 THEN RETURN "am".
      IF i = 40 THEN RETURN "an".
      IF i = 41 THEN RETURN "ao".
      IF i = 42 THEN RETURN "ap".
      IF i = 43 THEN RETURN "aq".
      IF i = 44 THEN RETURN "ar".
      IF i = 45 THEN RETURN "as".
      IF i = 46 THEN RETURN "at".
      IF i = 47 THEN RETURN "au".
      IF i = 48 THEN RETURN "av".
      IF i = 49 THEN RETURN "aw".
      IF i = 50 THEN RETURN "ax".
      IF i = 51 THEN RETURN "ay".
      IF i = 52 THEN RETURN "az".
      IF i = 53 THEN RETURN "ba".
      IF i = 54 THEN RETURN "bb".
      IF i = 55 THEN RETURN "bc".      
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
      IF i = "W" THEN RETURN 23 .
      IF i = "x" THEN RETURN 24 .
      IF i = "y" THEN RETURN 25 .
      IF i = "z" THEN RETURN 26 .
      IF i = "aa" THEN RETURN 27 .
      IF i = "ab" THEN RETURN 28 .
      IF i = "ac" THEN RETURN 29 .
      IF i = "ad" THEN RETURN 30 .
      IF i = "ae" THEN RETURN 31 .
      IF i = "af" THEN RETURN 32 .
      IF i = "ag" THEN RETURN 33 .
      IF i = "ah" THEN RETURN 34 .
      IF i = "ai" THEN RETURN 35 .
      IF i = "aj" THEN RETURN 36 .
      
      
      IF i = "ak" THEN RETURN 37 .
      IF i = "al" THEN RETURN 38 .
      IF i = "am" THEN RETURN 39 .
      IF i = "an" THEN RETURN 40 .
      IF i = "ao" THEN RETURN 41 .
      IF i = "ap" THEN RETURN 42 .
      IF i = "aq" THEN RETURN 43 .
      IF i = "ar" THEN RETURN 44 .
      IF i = "as" THEN RETURN 45 .
      IF i = "at" THEN RETURN 46 .
      IF i = "au" THEN RETURN 47 .
      IF i = "av" THEN RETURN 48 .
      IF i = "aw" THEN RETURN 49 .
      IF i = "ax" THEN RETURN 50 .
      IF i = "ay" THEN RETURN 51 .
      IF i = "az" THEN RETURN 52 .
      IF i = "ba" THEN RETURN 53 .
      IF i = "bb" THEN RETURN 54 .
      IF i = "bc" THEN RETURN 55 .
      
   END METHOD.
   METHOD PUBLIC CHARACTER Getrad(cRad AS CHARACTER):
      IF ASC(SUBSTRING(cRad,2,1)) >= 48 AND ASC(SUBSTRING(cRad,2,1)) <= 57 THEN RETURN SUBSTRING(cRad,2).
      RETURN SUBSTRING(cRad,3).
      
   END METHOD.
   METHOD PUBLIC INTEGER GetkolNum(cRad AS CHARACTER):      
      IF ASC(SUBSTRING(cRad,2,1)) >= 48 AND ASC(SUBSTRING(cRad,2,1)) <= 57 THEN RETURN getcolnum(SUBSTRING(cRad,1,1)).
      RETURN getcolnum(SUBSTRING(cRad,1,2)).
      
   END METHOD.
   METHOD PUBLIC CHARACTER Getkol(cRad AS CHARACTER):      
      IF ASC(SUBSTRING(cRad,2,1)) >= 48 AND ASC(SUBSTRING(cRad,2,1)) <= 57 THEN RETURN SUBSTRING(cRad,1,1).
      RETURN SUBSTRING(cRad,1,2).      
   END METHOD.
   
   METHOD PUBLIC VOID SaveRad():
      iSaveRad = iRad.
   END METHOD.
   
   METHOD PUBLIC VOID SaveColRad():
      SaveColRad = ColRad.
   END METHOD.
   
   METHOD PUBLIC VOID SetSaveRad():
      iRad = iSaveRad.
      ColumnRad(). 
   END METHOD.
   
   METHOD PUBLIC VOID SetSaveColRad():
      ColRad  = SaveColRad.
      iRad = INTEGER(Getrad(ColRad)). 
   END METHOD.
   
   METHOD PUBLIC VOID SetRad(inrad AS INTEGER):
      iRad = inrad.
      ColumnRad(). 
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
      iColNum = getcolnum(cColName) + 1. 
      cColName = getcolname(iColNum).     
      ColumnRad().
   END METHOD.
   
   METHOD PUBLIC VOID Colleft():
      iColNum = getcolnum(cColName) - 1. 
      cColName = getcolname(iColNum).     
      ColumnRad().
   END METHOD.
   
   METHOD PUBLIC VOID ColumnRad():
      iColNum = getcolnum(cColName).
      IF iSistaColnum < iColNum THEN iSistaColnum = iColNum.
      ColRad = cColName + STRING(iRad). 
   END METHOD.
   METHOD PUBLIC VOID FontExcel(cStart AS CHARACTER, cRangefont AS CHARACTER,varfont AS CHARACTER,varsize AS INTEGER,varbold AS LOGICAL,varhojd AS INTEGER,varfarg AS INTEGER ):
      IF varfont NE "" THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:NAME = varfont NO-ERROR.
      IF varsize NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:SIZE = varsize NO-ERROR.
      IF varbold NE ? THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:Bold = varbold NO-ERROR.
      IF varhojd NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):RowHeight = varhojd NO-ERROR.
      IF varfarg NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
      
   END METHOD.
   /*
   PROCEDURE profexcelfarg_UI :
   DEFINE INPUT PARAMETER profg AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER exfg AS INTEGER NO-UNDO.
   exfg = profg.
   IF profg = 1 THEN exfg = 11. /*MÖRKBLÅ*/
   IF profg = 2 THEN exfg = 10. /*GRÖN AONR*/
   IF profg = 3 THEN exfg = 14. /*BLÅGRÖN*/
   IF profg = 4 THEN exfg = 9.  /*MÖRKRÖD PLANR*/
   IF profg = 5 THEN exfg = 54.  /*lila*/
   IF profg = 6 THEN exfg = 12.  /*grönbrun*/
   IF profg = 7 THEN exfg = 16.  /*mörkgrå*/
   IF profg = 8 THEN exfg = 15. /*GRÅ*/
   IF profg = 9 THEN exfg = 5.  /*BLÅ*/
   IF profg = 10 THEN exfg = 4. /*LJUSGRÖN*/
   IF profg = 11 THEN exfg = 8. /*LJUSBLÅ*/
   IF profg = 12 THEN exfg = 3. /*RÖD*/
   IF profg = 13 THEN exfg = 7. /*ROSA*/
   IF profg = 14 THEN exfg = 6. /*GUL*/  
   
END PROCEDURE.
*/
   METHOD PUBLIC VOID ColWidth(cStart AS CHARACTER, widthsize AS DECIMAL):
      Elpool.Helpers:GetRange(GetWorkSheet(),cStart):ColumnWidth = widthsize. 
             
   END METHOD.
   METHOD PUBLIC CHARACTER DeciMalKomma(invar AS CHARACTER):
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
         invar = REPLACE(invar,",",".").
      END.   
      IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN DO:
         invar = REPLACE(invar,".",",").
      END.
      /*   
      /*OBS !!!!! detta körs även för char. konsevens att alla mellanslag försvinner.  Robin 20150417*/
      invar = REPLACE(invar,"KR","").
      invar = REPLACE(invar," ","").
      invar = TRIM(invar).*/
      RETURN invar.              
   END METHOD.
   METHOD PUBLIC INTEGER HittaGetData(outcol AS CHARACTER, sokvar AS CHARACTER):
      DEFINE VARIABLE invar AS INTEGER NO-UNDO.
      IF direkticell = TRUE THEN 
      DO:
         /*
         
         IF UseMsInterop = TRUE THEN /* EJ KLARinvar = Elpool.Helpers:GetRange(GetWorkSheet(),outpos):TEXT*/. 
         ELSE /* EJ KLAR invar = chWorkSheet:Range(outpos):TEXT*/.
         */
      END.
      ELSE DO:
         FIND FIRST excellTT WHERE excellTT.KOLUMN = outcol AND excellTT.VARDET = sokvar NO-LOCK NO-ERROR.
         IF AVAILABLE excellTT THEN 
         DO:
            invar = excellTT.RAD.
         END.   
      END.      
      RETURN invar.        
   END METHOD.
   /*läs data direkt i cell*/
   METHOD PUBLIC CHARACTER GetData():
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      invar = THIS-OBJECT:valueDataIn(ColRad).
      invar = DeciMalKomma(invar). 
      RETURN invar.              
   END METHOD.
   /*läs data direkt i angiven cell*/
   METHOD PUBLIC CHARACTER GetData(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      invar = THIS-OBJECT:valueDataIn(outpos).
      invar = DeciMalKomma(invar).    
      RETURN invar.              
   END METHOD. 
   /*skriv data direkt i cell i aktiv cell*/
   
   METHOD PUBLIC VOID DataOut(outdata AS CHARACTER):
      THIS-OBJECT:valueDataOut(ColRad,outdata).
   END METHOD.
   METHOD PUBLIC VOID DataOutFlerrad(outdata AS CHARACTER):
      THIS-OBJECT:valueDataOutFlerrad(ColRad,outdata).
   END METHOD.
   
   /*skriv data direkt i angiven cell*/
   METHOD PUBLIC VOID DataOut(outpos AS CHARACTER,outdata AS CHARACTER):
      THIS-OBJECT:valueDataOut(outpos,outdata).      
   END METHOD.
   
   METHOD PUBLIC VOID DataOut(outpos AS CHARACTER,outlg AS LOGICAL):
      THIS-OBJECT:valueDataOutlg(outpos,outlg).      
   END METHOD.
   /*läs och skriv data direkt i cell*/
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL, outdata AS CHARACTER):   
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = THIS-OBJECT:valueDataIn(ColRad).
         invar = DeciMalKomma(invar).
         IF invar = "" THEN outdata = STRING(decimal(outdata)).
         ELSE  outdata = STRING(decimal(outdata) + decimal(invar)).
      END.
      THIS-OBJECT:valueDataOut(ColRad,outdata). 
   END METHOD.
    /*läs och skriv data direkt i cell*/
   METHOD PUBLIC VOID DataOutKrBort(uppdat AS LOGICAL, outdata AS CHARACTER):   
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = THIS-OBJECT:valueDataIn(ColRad).
         invar = THIS-OBJECT:KrBortDeciMal(invar).
         invar = DeciMalKomma(invar).
         IF invar = "" THEN outdata = STRING(decimal(outdata)).
         ELSE  outdata = STRING(decimal(outdata) + decimal(invar)).
      END.
      THIS-OBJECT:valueDataOut(ColRad,outdata). 
   END METHOD.
   METHOD PUBLIC CHARACTER KrBortDeciMal(invar AS CHARACTER):
      invar = REPLACE(invar,"KR","").
      invar = REPLACE(invar," ","").
      invar = TRIM(invar).
      
      RETURN invar.              
   END METHOD.
   
   /*läs och skriv data direkt i angiven cell*/
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL,outpos AS CHARACTER,outdata AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = THIS-OBJECT:valueDataIn(outpos).
         IF invar = ? OR invar = "?" THEN invar = STRING(0).
         invar = DeciMalKomma(invar).
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      THIS-OBJECT:valueDataOut(outpos,outdata).
   END METHOD.
   
   /*läs och skriv data direkt i angiven cell samt formaterar cell*/
   METHOD PUBLIC VOID DataOutFormat(uppdat AS LOGICAL,outpos AS CHARACTER,outdata AS CHARACTER,decivisa AS CHARACTER, mindrenoll AS LOGICAL):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         invar = THIS-OBJECT:valueDataIn(outpos).
         invar = DeciMalKomma(invar).
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      IF decivisa NE ? THEN DO:
         IF mindrenoll = FALSE THEN THIS-OBJECT:KolumnFormat(outpos,decivisa).
         ELSE DO:
            /*om neg värde*/
            
            IF SUBSTRING(outdata,1,1) = "." OR SUBSTRING(outdata,1,1) = "," OR SUBSTRING(outdata,1,2) = "0," OR SUBSTRING(outdata,1,2) = "0." OR 
            SUBSTRING(outdata,1,2) = "-." OR SUBSTRING(outdata,1,2) = "-," OR SUBSTRING(outdata,1,3) = "-0," OR SUBSTRING(outdata,1,3) = "-0."
            THEN THIS-OBJECT:KolumnFormat(outpos,"0.00").
            ELSE THIS-OBJECT:KolumnFormat(outpos,decivisa).
             
         END. 
      END.
      THIS-OBJECT:valueDataOut(outpos,outdata).
   END METHOD.
   
   
   METHOD PUBLIC VOID Summa(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER, radtext AS CHARACTER, kollkolumn AS CHARACTER, tamed AS LOGICAL):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE cinvar    AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN RETURN.
      FOR EACH excellTT WHERE excellTT.KOLUMN = Getkol(cStart) AND excellTT.RAD >= INTEGER(Getrad(cStart)) AND excellTT.RAD < INTEGER(Getrad(cRangefont)) NO-LOCK:
         FIND FIRST excellTTbuff  WHERE excellTTbuff.KOLUMN = kollkolumn AND excellTTbuff.RAD = excellTT.RAD AND excellTTbuff.VARDET BEGINS radtext  NO-LOCK NO-ERROR.
         IF AVAILABLE excellTTbuff THEN DO:
            IF tamed = TRUE THEN DO:
               IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
               ELSE invar = invar + DECIMAL(excellTT.VARDET).
            END.             
         END.
         ELSE DO:
            IF tamed = FALSE THEN DO:
               IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
               ELSE invar = invar + DECIMAL(excellTT.VARDET).
            END.
         END.         
      END.
      FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = cRangefont NO-LOCK NO-ERROR.
      IF NOT AVAILABLE excellTT THEN DO:
         CREATE excellTT.
         ASSIGN 
         excellTT.KOLUMNRAD    = cRangefont
         excellTT.KOLUMN       = Getkol(cRangefont)
         excellTT.KOLUMNNUMMER = GetkolNum(cRangefont)
         excellTT.RAD          = INTEGER(Getrad(cRangefont)).
      END.   
      IF decivar = ? THEN  excellTT.VARDET = STRING(invar).         
      ELSE DO:
         invar = ROUND(invar,decivar).
         excellTT.VARDET = STRING(invar).
      END.   
   
   END METHOD.
   METHOD PUBLIC VOID Summa(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE cinvar    AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont).
            THIS-OBJECT:ExcelRange:FormulaR1C1 = formelvar.
         IF decivar = ? THEN. 
         ELSE DO:
            cinvar = THIS-OBJECT:valueDataIn(cRangefont).
            cinvar = DeciMalKomma(cinvar).
            invar = ROUND(DECIMAL(cinvar),decivar).
            THIS-OBJECT:valueDataOut(cRangefont,STRING(invar)).         
         END.    
      END.
      ELSE DO:
         FOR EACH excellTT WHERE excellTT.KOLUMN = Getkol(cStart) AND excellTT.RAD >= INTEGER(Getrad(cStart)) AND excellTT.RAD < INTEGER(Getrad(cRangefont)) NO-LOCK:
            IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
            ELSE invar = invar + DECIMAL(excellTT.VARDET). 
         END.
         FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = cRangefont NO-LOCK NO-ERROR.
         IF NOT AVAILABLE excellTT THEN DO:
            CREATE excellTT.
            ASSIGN 
            excellTT.KOLUMNRAD    = cRangefont
            excellTT.KOLUMN       = Getkol(cRangefont)
            excellTT.KOLUMNNUMMER = GetkolNum(cRangefont)
            excellTT.RAD          = INTEGER(Getrad(cRangefont)).
         END.   
         IF decivar = ? THEN  excellTT.VARDET = STRING(invar).         
         ELSE DO:
            invar = ROUND(invar,decivar).
            excellTT.VARDET = STRING(invar).
         END.   
      END.   
   END METHOD.
   
    /*Anders Olsson Elpool i Umeå AB  14 okt 2014 09:57:36 
    summa matriser
    summering skriv värde + fritext  startcell, slutcell, avrunda tillantal decimaler, text,kolumn, ta med,visa FORMAT, om mindre än noll
    om decivar = ? ingen avrundning, om decivisa = ? visa alla decimaler
    
   */

   METHOD PUBLIC VOID SummaM(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER, radtext AS CHARACTER, kollkolumn AS CHARACTER, tamed AS LOGICAL,decivisa AS CHARACTER, mindrenoll AS LOGICAL):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE cinvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE excelvarde AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN RETURN.
      FOR EACH excellTT WHERE excellTT.KOLUMN = Getkol(cStart) AND excellTT.RAD >= INTEGER(Getrad(cStart)) AND excellTT.RAD < INTEGER(Getrad(cRangefont)) NO-LOCK:
         FIND FIRST excellTTbuff  WHERE excellTTbuff.KOLUMN = kollkolumn AND excellTTbuff.RAD = excellTT.RAD AND excellTTbuff.VARDET BEGINS radtext  NO-LOCK NO-ERROR.
         IF AVAILABLE excellTTbuff THEN DO:
            IF tamed = TRUE THEN DO:
               IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
               ELSE invar = invar + DECIMAL(excellTT.VARDET).
            END.             
         END.
         ELSE DO:
            IF tamed = FALSE THEN DO:
               IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
               ELSE invar = invar + DECIMAL(excellTT.VARDET).
            END.
         END.         
      END.
      FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = cRangefont NO-LOCK NO-ERROR.
      IF NOT AVAILABLE excellTT THEN DO:
         CREATE excellTT.
         ASSIGN 
         excellTT.KOLUMNRAD    = cRangefont
         excellTT.KOLUMN       = Getkol(cRangefont)
         excellTT.KOLUMNNUMMER = GetkolNum(cRangefont)
         excellTT.RAD          = INTEGER(Getrad(cRangefont)).
      END.   
      IF decivar = ? THEN  excellTT.VARDET = STRING(invar).         
      ELSE DO:
         invar = ROUND(invar,decivar).
         excellTT.VARDET = STRING(invar).
      END.   
      IF decivisa NE ? THEN DO:
         IF mindrenoll = FALSE THEN THIS-OBJECT:KolumnFormat(cRangefont,decivisa).
         ELSE DO:
            excelvarde = THIS-OBJECT:GetData(cRangefont).
            /*om neg värde*/
            IF SUBSTRING(excelvarde,1,1) = "." OR SUBSTRING(excelvarde,1,1) = "," OR SUBSTRING(excelvarde,1,2) = "0," OR SUBSTRING(excelvarde,1,2) = "0." OR 
            SUBSTRING(excelvarde,1,2) = "-." OR SUBSTRING(excelvarde,1,2) = "-," OR SUBSTRING(excelvarde,1,3) = "-0," OR SUBSTRING(excelvarde,1,3) = "-0."
            THEN THIS-OBJECT:KolumnFormat(cRangefont,"0.00").
            ELSE THIS-OBJECT:KolumnFormat(cRangefont,decivisa).
               
         END. 
      END.
   END METHOD.
   /*Anders Olsson Elpool i Umeå AB  14 okt 2014 09:57:36 
    summering vertikalt  startcell, slutcell, avrunda tillantal decimaler, visa FORMAT, om mindre än noll
    om decivar = ? ingen avrundning, om decivisa = ? visa alla decimaler
    
   */
   METHOD PUBLIC VOID SummaV(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER,decivisa AS CHARACTER, mindrenoll AS LOGICAL):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE cinvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE excelvarde AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont).
            THIS-OBJECT:ExcelRange:FormulaR1C1 = formelvar.
         IF decivar = ? THEN. 
         ELSE DO:
            cinvar = THIS-OBJECT:valueDataIn(cRangefont).
            cinvar = DeciMalKomma(cinvar).
            invar = ROUND(DECIMAL(cinvar),decivar).
            THIS-OBJECT:valueDataOut(cRangefont,STRING(invar)).         
         END.    
      END.
      ELSE DO:
         FOR EACH excellTT WHERE excellTT.KOLUMN = Getkol(cStart) AND excellTT.RAD >= INTEGER(Getrad(cStart)) AND excellTT.RAD < INTEGER(Getrad(cRangefont)) NO-LOCK:
            IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
            ELSE invar = invar + DECIMAL(excellTT.VARDET). 
         END.
         FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = cRangefont NO-LOCK NO-ERROR.
         IF NOT AVAILABLE excellTT THEN DO:
            CREATE excellTT.
            ASSIGN 
            excellTT.KOLUMNRAD    = cRangefont
            excellTT.KOLUMN       = Getkol(cRangefont)
            excellTT.KOLUMNNUMMER = GetkolNum(cRangefont)
            excellTT.RAD          = INTEGER(Getrad(cRangefont)).
         END.   
         IF decivar = ? THEN  excellTT.VARDET = STRING(invar).         
         ELSE DO:
            invar = ROUND(invar,decivar).
            excellTT.VARDET = STRING(invar).
         END.   
      END.  
      IF decivisa NE ? THEN DO:
         IF mindrenoll = FALSE THEN THIS-OBJECT:KolumnFormat(cRangefont,decivisa).
         ELSE DO:
            excelvarde = THIS-OBJECT:GetData(cRangefont).
            /*om neg värde*/
            IF SUBSTRING(excelvarde,1,1) = "." OR SUBSTRING(excelvarde,1,1) = "," OR SUBSTRING(excelvarde,1,2) = "0," OR SUBSTRING(excelvarde,1,2) = "0." OR 
            SUBSTRING(excelvarde,1,2) = "-." OR SUBSTRING(excelvarde,1,2) = "-," OR SUBSTRING(excelvarde,1,3) = "-0," OR SUBSTRING(excelvarde,1,3) = "-0."
            THEN THIS-OBJECT:KolumnFormat(cRangefont,"0.00").
            ELSE THIS-OBJECT:KolumnFormat(cRangefont,decivisa).            
         END. 
      END.          
   END METHOD.
   
   /*Anders Olsson Elpool i Umeå AB  14 okt 2014 09:57:36 
    summering horisont  startcell, slutcell, avrunda tillantal decimaler, visa FORMAT
    om decivar = ? ingen avrundning, om decivisa = ? visa alla decimaler
   */
   /**/
   METHOD PUBLIC VOID SummaH(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER,decivisa AS CHARACTER  ):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE cinvar    AS CHARACTER NO-UNDO.
      
      IF direkticell = TRUE THEN DO:
         formelvar = "=SUMMA(RC[-" + STRING(INTEGER(GetkolNum(cRangefont)) - INTEGER(GetkolNum(cStart))) + "]:RC[-1])".   
          THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont).
          THIS-OBJECT:ExcelRange:FormulaR1C1 = formelvar.   
         /*Runda av summan*/
         IF decivar = ? THEN. 
         ELSE DO:
            cinvar = THIS-OBJECT:valueDataIn(cRangefont).
            cinvar = DeciMalKomma(cinvar).
            invar = ROUND(DECIMAL(cinvar),decivar).
            THIS-OBJECT:valueDataOut(cRangefont,STRING(invar)).          
         END.   
         IF decivisa NE ? THEN THIS-OBJECT:KolumnFormat(cRangefont,decivisa).  
      END.
      ELSE DO:
         FOR EACH excellTT WHERE excellTT.RAD = INTEGER(Getrad(cStart)) AND excellTT.KOLUMNNUMMER >= GetkolNum(cStart) AND excellTT.KOLUMNNUMMER < GetkolNum(cRangefont):
            IF excellTT.VARDET = "" OR excellTT.VARDET = ? THEN.
            ELSE invar = invar + DECIMAL(excellTT.VARDET).
         END.
         FIND FIRST excellTT WHERE excellTT.KOLUMNRAD = cRangefont NO-LOCK NO-ERROR.
         IF NOT AVAILABLE excellTT THEN DO:
            CREATE excellTT.
            ASSIGN 
            excellTT.KOLUMNRAD    = cRangefont
            excellTT.KOLUMN       = Getkol(cRangefont)
            excellTT.KOLUMNNUMMER = GetkolNum(cRangefont)
            excellTT.RAD          = INTEGER(Getrad(cRangefont)).
         END.   
         IF decivar = ? THEN  excellTT.VARDET = STRING(invar).         
         ELSE DO:
            invar = ROUND(invar,decivar).
            excellTT.VARDET = STRING(invar).
         END.
         IF decivisa NE ? THEN DO:
            /*om neg värde*/
            THIS-OBJECT:KolumnFormat(cRangefont, decivisa).
         END.      
      END.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileAs (INPUT path AS CHARACTER):
      /*Robin Sjöberg Elpool i Umeå AB  18 feb 2014 15:51:21 
      Ändrat  från saveas till savecopy för att undvika att att filen blir skrivskyddad när man öppnar den i excel medan man fortfarande kör kalkylen.
      */
      /*THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:SaveAs(path, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,Microsoft.Office.Interop.Excel.XlSaveAsAccessMode:xlNoChange,System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).*/
      
      THIS-OBJECT:ExcelWbHandle:SaveAs(path, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,Microsoft.Office.Interop.Excel.XlSaveAsAccessMode:xlNoChange,System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID namnbladexcel(namnvar AS CHARACTER):
      THIS-OBJECT:GetWorkSheet():NAME.
   END METHOD.
   
   METHOD PUBLIC CHARACTER KolumnFormatGet (kolvar AS CHARACTER):
      DEFINE VARIABLE formvar AS CHARACTER NO-UNDO.
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(), kolvar).
      formvar = THIS-OBJECT:ExcelRange:NumberFormat.  

      RETURN formvar.
   END METHOD.
   
   METHOD PUBLIC VOID KolumnFormat(kolvar AS CHARACTER, formvar AS CHARACTER):
      THIS-OBJECT:ExcelRange = Elpool.Helpers:GetRange(GetWorkSheet(), kolvar).
      THIS-OBJECT:ExcelRange:NumberFormat = formvar.   
   END METHOD.
   
   METHOD PUBLIC VOID NyttBladExcel():
      THIS-OBJECT:ExcelWbHandle:Sheets:ADD(?,?,ExcelWbHandle:Sheets:count,?). 
   END METHOD.
   
   /*Retunerarbladvar*/
   METHOD PUBLIC INTEGER NyttBladExcelVar():
      /*IF UseMsInterop = TRUE THEN ExcelWbHandle:Sheets:ADD(?,?,ExcelWbHandle:Sheets:count,?). 
      ELSE 
      DO:
         bladvar = bladvar + 1.
         /*chWorkSheet = chExcelApplication:Sheets:Item(bladvar).   */
         IF bladvar < antalbladstart THEN 
         DO: 
            RELEASE OBJECT chWorksheet NO-ERROR. 
            chWorkSheet = chExcelApplication:Sheets:ITEM(bladvar) NO-ERROR.
         END.
         ELSE 
         DO:
            RELEASE OBJECT chWorksheet NO-ERROR. 
            chWorkSheet = chExcelApplication:Sheets:ADD NO-ERROR.
            chWorkSheet:MOVE(,chWorkbook:Sheets(bladvar)) NO-ERROR.
         END.
         RETURN bladvar.
      END.    */   
   END METHOD.
   
   
   /* 
   METHOD PUBLIC CHARACTER SokIExcel(INPUT vad AS CHARACTER,INPUT startcol AS CHARACTER,INPUT slutcol AS CHARACTER, INPUT exakt AS LOGICAL):
      DEFINE VARIABLE stAddress AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE rnValue   AS COM-HANDLE NO-UNDO.
      startcol = startcol + ":" + slutcol.
      chCell = chWorkSheet:Range(startcol) NO-ERROR.
      rnValue = chCell:Find(vad,,,,,,,,). 
      IF rnValue = 0 THEN DO:
   
         RELEASE OBJECT chCell NO-ERROR.
         stAddress = "".
         RETURN stAddress.
         
      END.
        
      stAddress = rnValue:Address(,,,,). 
      IF exakt = TRUE THEN DO:
         IF TRIM(vad) = TRIM(GetData(stAddress)) THEN .
         ELSE  stAddress = "".
      END. 
      RELEASE OBJECT chCell NO-ERROR.
      RETURN stAddress.
           
   END METHOD. */
   
   METHOD PUBLIC VOID ValjBlad(valbald AS INTEGER):
     THIS-OBJECT:ExcelWorksheets:select(valbald) NO-ERROR.      
   END METHOD.
   
   METHOD PUBLIC INTEGER GetBladvar():
      return bladvar.
   END METHOD. 
   
   METHOD PUBLIC VOID SetBladvar(newbladvar AS INTEGER):
      bladvar = newbladvar.
      ValjBlad(bladvar).
   END METHOD.
   
   METHOD PUBLIC VOID printmark():                  
       THIS-OBJECT:GetWorkSheet():PrintOut(System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,SESSION:PRINTER-NAME, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value).       
   END METHOD.
   
   METHOD PUBLIC VOID SetVisible(val AS LOGICAL):
      THIS-OBJECT:ExcelAppHandle:Visible = val.
   END METHOD.
   
   METHOD PUBLIC VOID SetDisplayAlerts(val AS LOGICAL):
      THIS-OBJECT:ExcelAppHandle:DisplayAlerts = val.
   END METHOD.
   
      METHOD PUBLIC VOID SetScreenUpdating(val AS LOGICAL):
      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = val.
   END METHOD.
   
   DESTRUCTOR PUBLIC GuruExcelInterop ( ):
      THIS-OBJECT:ReleaseExcel(FALSE).
   END DESTRUCTOR.
   
END CLASS.