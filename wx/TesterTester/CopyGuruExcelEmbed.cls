  
/*------------------------------------------------------------------------
   File        : CopyGuruExcelEmbed.cls
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


CLASS Controls.GuruExcelEmbed INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE components     AS System.ComponentModel.IContainer    NO-UNDO.
   DEFINE PUBLIC VARIABLE felexcel        AS LOGICAL NO-UNDO.
   DEFINE PRIVATE VARIABLE DlgSave        AS System.Windows.Forms.SaveFileDialog NO-UNDO.
   DEFINE PRIVATE VARIABLE DlgOpen        AS System.Windows.Forms.OpenFileDialog NO-UNDO.
   DEFINE PUBLIC  VARIABLE ExcelAppHandle AS Application                         NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWbHandle  AS Workbook                            NO-UNDO.
   
   DEFINE PUBLIC VARIABLE isInited       AS LOGICAL                             NO-UNDO.
   DEFINE PUBLIC VARIABLE chWorksheet  AS COM-HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE visavarexcel AS LOGICAL NO-UNDO.
   
   DEFINE VARIABLE link AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE bladvar            AS INTEGER    NO-UNDO.
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
   
   
   
   
   
   
   DEFINE PUBLIC VARIABLE ColRad AS CHARACTER NO-UNDO.  /*aktuell column och rad ex. e7*/
   DEFINE PUBLIC VARIABLE cColName AS CHARACTER NO-UNDO. /*aktuell column ex. e*/
   DEFINE PUBLIC VARIABLE iColNum AS INTEGER NO-UNDO. /*aktuell nummer på columnen ex e = 5*/
   DEFINE PUBLIC VARIABLE iSistaColnum AS INTEGER NO-UNDO. /*den col som fins längs till höger*/
   DEFINE PUBLIC VARIABLE iRad AS INTEGER NO-UNDO. /*aktuell rad ex 7*/
   DEFINE PUBLIC VARIABLE embedvar AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE iRadslut AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE totheight AS DECIMAL NO-UNDO.
   
   METHOD PUBLIC VOID OpenExcel(INPUT excelkommando AS CHARACTER):

      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE.
      {OpenExcelC.I}
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
     /* {EXCELFEL.I}*/
   END METHOD. 
   METHOD PUBLIC VOID ReadExcelStart(INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      {OpenExcelC.I}
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      ASSIGN chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      ASSIGN iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
      /*{EXCELFEL.I}*/
   END METHOD.
   METHOD PUBLIC VOID StartExcel():
      felexcel = FALSE.
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
   END METHOD.
   
   METHOD PUBLIC VOID StartExcelVal(INPUT startval AS LOGICAL):
      felexcel = FALSE.

      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = startval.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      
      RUN nyttbladexcel_UI.   
      {EXCELFEL.I}
  END METHOD.
   
  METHOD PUBLIC VOID StartExceld():      
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE NO-ERROR.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      RUN nyttbladexcel_UI.   
      /*
      chWorkSheet = chExcelApplication:Sheets:Item(1).  /*tar blad nr 1*/
      */
      {EXCELFEL.I}
   END METHOD.
   METHOD PUBLIC VOID FlerBladSlut():
      RELEASE OBJECT chWorksheet NO-ERROR. 
      chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      chWorkSheet:SELECT NO-ERROR. 
      {EXCELFEL.I}
   END METHOD.
   METHOD PUBLIC VOID SlutReadExcel():
      FlerBladSlut(). 
      NO-RETURN-VALUE chWorkbook:CLOSE() NO-ERROR.
      NO-RETURN-VALUE chExcelApplication:QUIT() NO-ERROR.
      BladvarKoll = ?.   
      SlutExcel().
      {EXCELFEL.I}
   END METHOD.
   METHOD PUBLIC VOID SlutExcel():   
      chExcelApplication:VISIBLE = TRUE NO-ERROR.
      chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
   
      IF BladvarKoll = TRUE THEN DO: 
         RELEASE OBJECT chWorksheet NO-ERROR. 
         chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
         chWorkSheet:SELECT NO-ERROR.      
         BladvarKoll = FALSE.
         /*
         RELEASE OBJECT chWorksheet NO-ERROR. 
         chWorkSheet = chExcelApplication:Sheets:ITEM(bladvar).
         chWorkSheet:MOVE(,chWorkbook:Sheets(3)).
         */
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
    
      {EXCELFEL.I}
   
   END METHOD.
   METHOD PUBLIC VOID ScreenUpp(INPUT uppvar AS LOGICAL): 
      IF embedvar = TRUE THEN DO:
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = uppvar.
         THIS-OBJECT:ExcelAppHandle:Visible = uppvar.
      END.   
      ELSE DO:
         chExcelApplication:ScreenUpdating = uppvar.
         chExcelApplication:Visible = uppvar.
      END.   
   END METHOD.     
   
   METHOD  PUBLIC Microsoft.Office.Interop.Excel.Worksheet GetWorkSheet():
      RETURN CAST(THIS-OBJECT:ExcelWbHandle:ActiveSheet, Microsoft.Office.Interop.Excel.Worksheet).
   END METHOD.
   METHOD  PUBLIC VOID imageexcel(INPUT link AS CHARACTER):
      ColumnRad().   
      IF totheight = 0 THEN totheight = 1.
      chWorkSheet:range(ColRad):SELECT NO-ERROR. 
      chWorkSheetRange = chWorkSheet:Pictures:INSERT(link) NO-ERROR.
      chWorkSheetRange:TOP = totheight NO-ERROR.
      chWorkSheetRange:LEFT = 1 NO-ERROR.
      RELEASE OBJECT chWorkSheetRange NO-ERROR.
      chWorkSheetRange = ?.
      chExcelApplication:VISIBLE = visavarexcel NO-ERROR.
   END METHOD.
   METHOD  PUBLIC VOID imageexcelhed(INPUT link AS CHARACTER):
      ColumnRad().   
      chWorkSheet:range(ColRad):SELECT NO-ERROR. 
      chWorkSheetRange = chWorkSheet:PageSetup:LeftHeaderPicture:INSERT(link) NO-ERROR.
   END METHOD.
   
   
   /* Fulkod, bör fixas när WinApi-handle inte längre behövs */
   DEFINE PRIVATE VARIABLE WinApi AS HANDLE NO-UNDO.
     
   &GLOB GWL_STYLE -16
   &GLOB GWL_EXSTYLE -20
   &GLOB WS_MAXIMIZEBOX 65536
   &GLOB WS_MINIMIZEBOX 131072
   &GLOB WS_THICKFRAME  262144
   &GLOB WS_CAPTION 12582912
   &GLOB WS_BORDER 8388608
   &GLOB WS_EX_APPWINDOW 262144
   
PROCEDURE MoveWindow EXTERNAL "user32":
   DEFINE INPUT PARAMETER hWnd      AS LONG.
   DEFINE INPUT PARAMETER x      AS LONG.
   DEFINE INPUT PARAMETER y      AS LONG.
   DEFINE INPUT PARAMETER cx     AS LONG.
   DEFINE INPUT PARAMETER cy     AS LONG.
   DEFINE INPUT PARAMETER repaint      AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.
   DEFINE VARIABLE ReturnVal AS INTEGER NO-UNDO.
   /*   --- /Fulkod  */
   
   

   
   CONSTRUCTOR PUBLIC GuruExcelEmbed (  ):
      
      
      SUPER().
      InitializeComponent().
      isInited = FALSE.

      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID DlgOpen_FileOk( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
      THIS-OBJECT:OpenFile(THIS-OBJECT:DlgOpen:FileName).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID DlgSave_FileOk( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
      THIS-OBJECT:SaveFileAs(THIS-OBJECT:DlgSave:FileName).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GuruExcelEmbed_Resize( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF isInited = TRUE THEN 
      DO:
         IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN 
         DO:
            RUN MoveWindow (THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:Height, 1, OUTPUT ReturnVal).
         END.
      END.
      THIS-OBJECT:Refresh().
      RETURN.


   END METHOD.

   METHOD PRIVATE VOID InitializeComponent(  ):
      
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:DlgSave = NEW System.Windows.Forms.SaveFileDialog().
      THIS-OBJECT:DlgOpen = NEW System.Windows.Forms.OpenFileDialog().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* DlgSave */
      /*  */
      THIS-OBJECT:DlgSave:Filter = "Excel Spreadsheet|*.xls*|Alla filer|*.*".
      THIS-OBJECT:DlgSave:Title = "Spara Excel-fil".
      THIS-OBJECT:DlgSave:FileOk:Subscribe(THIS-OBJECT:DlgSave_FileOk).
      /*  */
      /* DlgOpen */
      /*  */
      THIS-OBJECT:DlgOpen:Filter = "Excel Spreadsheet|*.xls*|Alla filer|*.*".
      THIS-OBJECT:DlgOpen:Title = "Öppna excel-fil".
      THIS-OBJECT:DlgOpen:FileOk:Subscribe(THIS-OBJECT:DlgOpen_FileOk).
      /*  */
      /* GuruExcelEmbed */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Name = "GuruExcelEmbed".
      THIS-OBJECT:Resize:Subscribe(THIS-OBJECT:GuruExcelEmbed_Resize).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

   DESTRUCTOR PUBLIC GuruExcelEmbed ( ):

      IF VALID-OBJECT(components) THEN 
      DO:
         CAST(components, System.IDisposable):Dispose().
      END.

   END DESTRUCTOR.

   METHOD PUBLIC VOID Print ( ):
      IF THIS-OBJECT:isInited = FALSE THEN RETURN.
      
      Elpool.Helpers:PrintDialog(THIS-OBJECT:ExcelAppHandle).
      
   END METHOD.

   METHOD PUBLIC VOID Initialize (path AS CHARACTER ):
      IF THIS-OBJECT:isInited = FALSE THEN DO:
         THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
         
         /*     Elpool.Helpers:SetLocal(THIS-OBJECT:ExcelAppHandle).*/

         
         /*THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Add(System.Reflection.Missing:Value).
         path = SEARCH("Modules\Kalkyl\kalkvisningtemplate.xls").
*/
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(System.IO.Path:GetFullPath(path), System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
         THIS-OBJECT:ExcelWbHandle:Activate().

         THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.
          
         THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
         THIS-OBJECT:ExcelAppHandle:Visible = true.
         THIS-OBJECT:Fix().          

         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.
        
         isInited = TRUE.
      END.
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID Fix():
      
      /* Sätter excels parentwindow till exceltabben */
      DEFINE VARIABLE oldhWnd AS INTEGER NO-UNDO. 
      RUN SetParent IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:hwnd, INPUT THIS-OBJECT:Handle:ToInt32(), OUTPUT oldhWnd). 
      /*(THIS-OBJECT:VisaExcelApp:Hwnd, THIS-OBJECT:VisaExcelTab:Handle:ToInt32(), OUTPUT ReturnVal).*/
     
      
      /* Hämtar stilparametrar, tar bort titel, ram/border, och möjlighet för kund att förstora och minimera */
      DEFINE VARIABLE style AS INTEGER NO-UNDO.
      RUN GetWindowLongA IN WinApi(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, {&GWL_STYLE}, OUTPUT style). 
      style = style - {&WS_CAPTION} - {&WS_THICKFRAME} - {&WS_BORDER} - {&WS_MAXIMIZEBOX} - {&WS_MINIMIZEBOX}  . /* + {&WS_CAPTION}.*/
      RUN SetWindowLongA IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT {&GWL_STYLE}, INPUT style, OUTPUT oldhWnd).
         
     
      /* extended stilparams */
         
      DEFINE VARIABLE exstyle AS INTEGER NO-UNDO.
      RUN GetWindowLongA IN WinApi(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, {&GWL_EXSTYLE}, OUTPUT exstyle). 
      exstyle = exstyle -  {&WS_EX_APPWINDOW}.
      RUN SetWindowLongA IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT {&GWL_EXSTYLE}, INPUT exstyle, OUTPUT oldhWnd).
     
      /* TODO - Göm ifrån taskbar och implementera excel-fil-hanterare i kontrollen(tabbar?) ? */
     
      /* Flyttar och passar in fönstret i exceltabben */
      IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN 
      DO:
         RUN MoveWindow (THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:Height, 1, OUTPUT ReturnVal). 
      END.
     
      THIS-OBJECT:Refresh().

   END METHOD.

   METHOD PUBLIC VOID Reload(INPUT path AS CHARACTER):
      
      IF THIS-OBJECT:isInited EQ FALSE THEN RETURN.
      THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
      THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
 

      THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(System.IO.Path:GetFullPath(path), System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
      THIS-OBJECT:ExcelWbHandle:Activate().
      THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.

       
      THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
      THIS-OBJECT:ExcelAppHandle:Visible = true.
      THIS-OBJECT:Fix().

      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.

      THIS-OBJECT:Refresh().
   END METHOD.

   METHOD PUBLIC VOID OpenFile ( INPUT path AS CHARACTER ):
      IF System.IO.File:Exists(path) = TRUE THEN 
      DO:
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(path, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
         THIS-OBJECT:ExcelWbHandle:Activate().
      END.
      ELSE 
      DO:
         System.Windows.Forms.MessageBox:Show("Filen ~" " + path +  " ~" hittades inte!").
      END.
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID OpenFileWithDialog():
      WAIT-FOR DlgOpen:ShowDialog().
      RETURN.
   END METHOD.

   METHOD PUBLIC VOID SaveFile ( ):
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:Save().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileAs ( INPUT path AS CHARACTER  ):
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:SaveAs(path, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,Microsoft.Office.Interop.Excel.XlSaveAsAccessMode:xlNoChange,System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileWithDialog():
      WAIT-FOR DlgSave:ShowDialog().   
      RETURN.
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
   METHOD PUBLIC CHARACTER DeciMalKomma(invar AS CHARACTER):
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
         invar = REPLACE(invar,",",".").
      END.   
      IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN DO:
         invar = REPLACE(invar,".",",").
      END.   
      RETURN invar.              
   END METHOD.
   METHOD PUBLIC CHARACTER GetData():
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF embedvar = TRUE THEN  invar = Elpool.Helpers:GetRange(GetWorkSheet(),ColRad):TEXT.
      ELSE invar = chWorkSheet:Range(ColRad):TEXT.   
      invar = DeciMalKomma(invar). 
      RETURN invar.              
   END METHOD.
   METHOD PUBLIC CHARACTER GetData(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
       
      IF embedvar = TRUE THEN invar = Elpool.Helpers:GetRange(GetWorkSheet(),outpos):TEXT. 
      ELSE invar = chWorkSheet:Range(outpos):TEXT.  
      invar = DeciMalKomma(invar).    
      RETURN invar.              
   END METHOD.
   
   METHOD PUBLIC VOID valueDataOut(outpos AS CHARACTER,outdata AS CHARACTER):
     
      IF embedvar = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),outpos):VALUE2 = outdata.
      END.   
      ELSE chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
     
   END METHOD.
    
   METHOD PUBLIC VOID DataOut(outdata AS CHARACTER):
      /*
      usedRanged
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),ColRad):Formula = outdata.
      chWorkSheet:Range(ColRad):VALUE = outdata NO-ERROR.
      */
      THIS-OBJECT:valueDataOut(ColRad,outdata).
   END METHOD.
   
   METHOD PUBLIC VOID DataOut(outpos AS CHARACTER,outdata AS CHARACTER):
      /*
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),outpos):Formula = outdata.  
      ELSE chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
      */
      THIS-OBJECT:valueDataOut(outpos,outdata).      
   END METHOD.
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL, outdata AS CHARACTER):   
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         IF embedvar = TRUE THEN invar = Elpool.Helpers:GetRange(GetWorkSheet(),ColRad):TEXT.
         ELSE invar = chWorkSheet:Range(ColRad):TEXT.
         invar = DeciMalKomma(invar).
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      /*
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),ColRad):Formula = outdata. 
      ELSE chWorkSheet:Range(ColRad):VALUE  = outdata NO-ERROR.
      */              
      THIS-OBJECT:valueDataOut(ColRad,outdata). 
   END METHOD.
   METHOD PUBLIC VOID DataOut(uppdat AS LOGICAL,outpos AS CHARACTER,outdata AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF uppdat = TRUE THEN DO:
         IF embedvar = TRUE THEN invar = Elpool.Helpers:GetRange(GetWorkSheet(),outpos):TEXT.
         ELSE invar = chWorkSheet:Range(outpos):TEXT.
         invar = DeciMalKomma(invar).
         outdata = STRING(decimal(outdata) + decimal(invar)). 
      END.
      /*
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),outpos):Formula = outdata.
      ELSE chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
      */
      THIS-OBJECT:valueDataOut(outpos,outdata).
      /*
    
   ASSIGN
   cActiveCell = chExcelApplication:ActiveCell
   cActiveCell:VALUE = varut NO-ERROR.
   cActiveCell:value = outdata.
   Worksheets("Sheet1").Range("A1").Value = 3.14159
   Worksheets("Sheet1").Range("A1").Formula = "=$A$4+$A$10"
   */
   END METHOD.
   
   
   METHOD PUBLIC VOID FontExcel(cStart AS CHARACTER, cRangefont AS CHARACTER,varfont AS CHARACTER,varsize AS INTEGER,varbold AS LOGICAL,varhojd AS INTEGER,varfarg AS INTEGER ):
      IF embedvar = TRUE THEN DO: 
         IF varfont NE "" THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:NAME = varfont NO-ERROR.
         IF varsize NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:SIZE = varsize NO-ERROR.
         IF varbold NE ? THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:Bold = varbold NO-ERROR.
         IF varhojd NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):RowHeight = varhojd NO-ERROR.
         IF varfarg NE 0 THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
      END.  
      ELSE DO:
         IF varfont NE "" THEN chWorkSheet:Range(cStart,cRangefont):FONT:NAME = varfont NO-ERROR.
         IF varsize NE 0 THEN chWorkSheet:Range(cStart,cRangefont):FONT:SIZE = varsize NO-ERROR.
         IF varbold NE ? THEN chWorkSheet:Range(cStart,cRangefont):FONT:Bold = varbold NO-ERROR.
         IF varhojd NE 0 THEN chWorkSheet:Range(cStart,cRangefont):RowHeight = varhojd NO-ERROR.
         IF varfarg NE 0 THEN chWorkSheet:Range(cStart,cRangefont):FONT:ColorIndex = varfarg NO-ERROR.
      END.   
   END METHOD.
   METHOD PUBLIC VOID Summa(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE cinvar AS CHARACTER NO-UNDO.
      IF embedvar = TRUE THEN DO:
         formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
         Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):FormulaR1C1 = formelvar.
      END.   
      ELSE DO:
         formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
         chWorkSheet:Range(cRangefont):Formula = formelvar  NO-ERROR.
      END.   
   
      /*
      =SUM(RC[-3]:RC[-1]) hor
      =SUMMA(R[-2]C:R[-1]C) våg
   */
      IF decivar = ? THEN. 
      ELSE DO:
         IF embedvar = TRUE THEN cinvar = Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont ):TEXT.
         ELSE cinvar = chWorkSheet:Range(cRangefont ):TEXT.
         cinvar = DeciMalKomma(cinvar).
            
         invar = ROUND(DECIMAL(cinvar),decivar).
         IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):Formula = STRING(invar).
         ELSE chWorkSheet:Range(cRangefont):Formula = STRING(invar). 
      END.     
   
   END METHOD.
    
   METHOD PUBLIC VOID Summah(cStart AS CHARACTER, cRangefont AS CHARACTER,decivar AS INTEGER):
      DEFINE VARIABLE formelvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE cinvar AS CHARACTER NO-UNDO.
      formelvar = "=SUMMA(RC[-" + STRING(INTEGER(Getkol(cRangefont)) - INTEGER(Getkol(cStart))) + "]:RC[-1])".
      IF embedvar = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):FormulaR1C1 = formelvar.
      END.   
      ELSE DO:
         chWorkSheet:Range(cRangefont):Formula = formelvar.  
            
      END.      
      /*
      =SUM(RC[-3]:RC[-1]) hor
      =SUMMA(R[-2]C:R[-1]C) våg
   */
      /*Runda av summan*/
      IF decivar = ? THEN. 
      ELSE DO:
         IF embedvar = TRUE THEN cinvar = Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont ):TEXT.
         ELSE cinvar = chWorkSheet:Range(cRangefont ):TEXT.
         cinvar = DeciMalKomma(cinvar).
         invar = ROUND(DECIMAL(cinvar),decivar).
         IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):Formula = STRING(invar). 
         ELSE  chWorkSheet:Range(cRangefont):Formula = STRING(invar). 
      END.     
   
   END METHOD.
   METHOD PUBLIC VOID ColWidth(cStart AS CHARACTER, widthsize AS DECIMAL):
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart):ColumnWidth = widthsize. 
      ELSE chWorkSheet:Range(cStart):ColumnWidth = widthsize.             
   END METHOD.
   METHOD PUBLIC DECIMAL ColWidth(cStart AS CHARACTER):
      DEFINE VARIABLE cwidth AS CHARACTER NO-UNDO.
      IF embedvar = TRUE THEN cwidth = STRING(Elpool.Helpers:GetRange(GetWorkSheet(),cStart):ColumnWidth).
      ELSE cwidth = STRING(chWorkSheet:Range(cStart):ColumnWidth).
      RETURN DECIMAL(cwidth).       
   END METHOD.
   
   METHOD PUBLIC VOID Border(cStart AS CHARACTER, cRangefont AS CHARACTER):
      IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):BorderAround(XlLineStyle:xlContinuous, XlBorderWeight:xlMedium, XlColorIndex:xlColorIndexAutomatic, System.Reflection.Missing:Value).
        
   END METHOD.
   METHOD PUBLIC VOID Linedubble(cStart AS CHARACTER, cRangefont AS CHARACTER):
      IF embedvar = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont), XlBordersIndex:xlEdgeBottom):LineStyle = XlLineStyle:xlDouble.       
   END METHOD.
  /*anders typ???*/
   METHOD PUBLIC VOID BordHoriz(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
       IF embedvar = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont  ), XlBordersIndex:xlInsideHorizontal):LineStyle = XlLineStyle:xlDot.
      
   END METHOD.
   METHOD PUBLIC VOID BordHorizW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      IF embedvar = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideHorizontal):Weight = XlBorderWeight:xlThin.
   END METHOD.
   METHOD PUBLIC VOID BordVert(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      IF embedvar = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideVertical):LineStyle = XlLineStyle:xlDot.
   END METHOD.
   METHOD PUBLIC VOID BordVertW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      IF embedvar = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideVertical):Weight = XlBorderWeight:xlThin.
   END METHOD.
   METHOD PUBLIC VOID AddKom(INPUT outpos AS CHARACTER,INPUT textvar AS CHARACTER): 
       IF embedvar = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),outpos):addcomment(textvar).      
       ELSE chWorkSheet:Range(outpos):addcomment(textvar) NO-ERROR.
   END METHOD.  
   METHOD PUBLIC VOID ValjBlad(valbald AS INTEGER):
      IF embedvar = TRUE THEN /*Elpool.Helpers:Sheets(valbald)*/.      
      ELSE chWorkSheet = chExcelApplication:Sheets:ITEM(valbald) NO-ERROR.
      {EXCELFEL.I} 
   END METHOD.
   METHOD PUBLIC VOID NyttBladExcel():
      IF embedvar = TRUE THEN /*Elpool.Helpers:Sheets:ADD()*/. 
      ELSE DO:
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
      END.   
      
   END METHOD.
   
   METHOD PUBLIC CHARACTER SokIExcel(INPUT vad AS CHARACTER,INPUT startcol AS CHARACTER,INPUT slutcol AS CHARACTER, INPUT exakt AS LOGICAL):
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
      IF exakt = TRUE THEN DO:
         IF TRIM(vad) = TRIM(GetData(stAddress)) THEN .
         ELSE  stAddress = "".
      END. 
      RELEASE OBJECT chCell NO-ERROR.
      RETURN stAddress.
           
   END METHOD. 
   
END CLASS.
 
 /*
 
 /*  interop.excel.range    
      'DeclarationProperty Value ( _
   RangeValueDataType AsObject _
) AsObjectGetSet'UsageDim instance As Range
Dim RangeValueDataType AsObjectDim value AsObject

value = instance.Value(RangeValueDataType)

instance.Value(RangeValueDataType) = value
      'DeclarationProperty Value2 AsObjectGetSet'UsageDim instance As Range
Dim value AsObject

value = instance.Value2

instance.Value2 = value
   
   'DeclarationProperty Formula AsObjectGetSet'UsageDim instance As Range
Dim value AsObject

value = instance.Formula

instance.Formula = value   
  */    
 
 /*------------------------------------------------------------------------
    File        : GuruExcelEmbed
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


CLASS Controls.GuruExcelEmbed INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   
   DEFINE PRIVATE VARIABLE DlgSave AS System.Windows.Forms.SaveFileDialog NO-UNDO.
   DEFINE PRIVATE VARIABLE DlgOpen AS System.Windows.Forms.OpenFileDialog NO-UNDO.
   DEFINE PUBLIC VARIABLE ExcelAppHandle AS Application NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWbHandle  AS Workbook NO-UNDO.
   
   DEFINE Public VARIABLE isInited AS LOGICAL NO-UNDO.
   
   METHOD PUBLIC Microsoft.Office.Interop.Excel.Worksheet GetWorkSheet():
      RETURN CAST(THIS-OBJECT:ExcelWbHandle:ActiveSheet, Microsoft.Office.Interop.Excel.Worksheet).
   END METHOD.
   
   /* Fulkod, bör fixas när WinApi-handle inte längre behövs */
   DEFINE PRIVATE VARIABLE WinApi AS HANDLE NO-UNDO.
     
   &GLOB GWL_STYLE -16
   &GLOB GWL_EXSTYLE -20
   &GLOB WS_MAXIMIZEBOX 65536
   &GLOB WS_MINIMIZEBOX 131072
   &GLOB WS_THICKFRAME  262144
   &GLOB WS_CAPTION 12582912
   &GLOB WS_BORDER 8388608
   &GLOB WS_EX_APPWINDOW 262144
   
   PROCEDURE MoveWindow EXTERNAL "user32":
      DEFINE INPUT PARAMETER hWnd      AS LONG.
      DEFINE INPUT PARAMETER x      AS LONG.
      DEFINE INPUT PARAMETER y      AS LONG.
      DEFINE INPUT PARAMETER cx     AS LONG.
      DEFINE INPUT PARAMETER cy     AS LONG.
      DEFINE INPUT PARAMETER repaint      AS LONG.
      DEFINE RETURN PARAMETER ReturnValue AS LONG.
   END PROCEDURE.
   DEFINE VARIABLE ReturnVal AS INTEGER NO-UNDO.
   /*   --- /Fulkod  */
   
   

   
   CONSTRUCTOR PUBLIC GuruExcelEmbed (  ):
      
      
      SUPER().
      InitializeComponent().
      isInited = FALSE.

      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID DlgOpen_FileOk( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
      THIS-OBJECT:OpenFile(THIS-OBJECT:DlgOpen:FileName).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID DlgSave_FileOk( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
      THIS-OBJECT:SaveFileAs(THIS-OBJECT:DlgSave:FileName).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GuruExcelEmbed_Resize( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF isInited = TRUE THEN DO:
         IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN DO:
            Elpool.WinApi:MoveWindow(THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:Height, true).
         END.
      END.
      /*THIS-OBJECT:Refresh().*/
      
      RETURN.


   END METHOD.

   METHOD PRIVATE VOID InitializeComponent(  ):
      
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:DlgSave = NEW System.Windows.Forms.SaveFileDialog().
      THIS-OBJECT:DlgOpen = NEW System.Windows.Forms.OpenFileDialog().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* DlgSave */
      /*  */
      THIS-OBJECT:DlgSave:Filter = "Excel Spreadsheet|*.xls*|Alla filer|*.*".
      THIS-OBJECT:DlgSave:Title = "Spara Excel-fil".
      THIS-OBJECT:DlgSave:FileOk:Subscribe(THIS-OBJECT:DlgSave_FileOk).
      /*  */
      /* DlgOpen */
      /*  */
      THIS-OBJECT:DlgOpen:Filter = "Excel Spreadsheet|*.xls*|Alla filer|*.*".
      THIS-OBJECT:DlgOpen:Title = "Öppna excel-fil".
      THIS-OBJECT:DlgOpen:FileOk:Subscribe(THIS-OBJECT:DlgOpen_FileOk).
      /*  */
      /* GuruExcelEmbed */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Name = "GuruExcelEmbed".
      THIS-OBJECT:Resize:Subscribe(THIS-OBJECT:GuruExcelEmbed_Resize).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

   DESTRUCTOR PUBLIC GuruExcelEmbed ( ):

      IF VALID-OBJECT(components) THEN DO:
         CAST(components, System.IDisposable):Dispose().
      END.

   END DESTRUCTOR.

   METHOD PUBLIC VOID Print ( ):
      IF THIS-OBJECT:isInited = FALSE THEN RETURN.
      
      Elpool.Helpers:PrintDialog(THIS-OBJECT:ExcelAppHandle).
      
   END METHOD.

   METHOD PUBLIC VOID Initialize ( ):
      IF THIS-OBJECT:isInited = FALSE THEN DO:
         THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         /*THIS-OBJECT:ExcelAppHandle:Visible = FALSE.*/
         THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
         
    /*     Elpool.Helpers:SetLocal(THIS-OBJECT:ExcelAppHandle).*/

         
         /*THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Add(System.Reflection.Missing:Value).*/
         DEFINE VARIABLE path AS CHARACTER NO-UNDO.
         path = SEARCH("template.xlsx").
        
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(path, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
         THIS-OBJECT:ExcelWbHandle:Activate().

         THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.
         THIS-OBJECT:ExcelAppHandle:UserControl = TRUE.
         THIS-OBJECT:Fix().
                  
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.
         /*THIS-OBJECT:ExcelAppHandle:Visible = TRUE.*/

         isInited = TRUE.
      END.
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID Fix():
      IF THIS-OBJECT:isInited EQ FALSE THEN RETURN.
      /* Sätter excels parentwindow till exceltabben */
      DEFINE VARIABLE oldhWnd AS INTEGER NO-UNDO.
      Elpool.WinApi:SetParent(INPUT THIS-OBJECT:ExcelAppHandle:hwnd, INPUT THIS-OBJECT:Handle:ToInt32()).
      /*RUN SetParent IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:hwnd, INPUT THIS-OBJECT:Handle:ToInt32(), OUTPUT oldhWnd). 
      /*(THIS-OBJECT:VisaExcelApp:Hwnd, THIS-OBJECT:VisaExcelTab:Handle:ToInt32(), OUTPUT ReturnVal).*/*/

      /* Hämtar stilparametrar, tar bort titel, ram/border, och möjlighet för kund att förstora och minimera */
      DEFINE VARIABLE style AS INTEGER NO-UNDO.

      style = Elpool.WinApi:GetWindowLong(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT Elpool.WinApi:GWL_STYLE).
      style = style - Elpool.WinApi:WS_BORDER - Elpool.WinApi:WS_CAPTION - Elpool.WinApi:WS_MAXIMIZEBOX - Elpool.WinApi:WS_MINIMIZEBOX - Elpool.WinApi:WS_THICKFRAME. 
      Elpool.WinApi:SetWindowLong(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT Elpool.WinApi:GWL_STYLE, INPUT style).
         
     
      /* extended stilparams */
      DEFINE VARIABLE exstyle AS INTEGER NO-UNDO.
      exstyle = Elpool.WinApi:GetWindowLong(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT Elpool.WinApi:GWL_EXSTYLE).
      exstyle = exstyle - Elpool.WinApi:WS_EX_WINDOWEDGE.
      Elpool.WinApi:SetWindowLong(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT Elpool.WinApi:GWL_EXSTYLE, INPUT exstyle).
 
      /* Flyttar och passar in fönstret i exceltabben */
      IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN DO:
         Elpool.WinApi:MoveWindow(THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:Height, true).
      END.
      
     /*THIS-OBJECT:Refresh().*/

   END METHOD.

   METHOD PUBLIC VOID Reload():
      IF THIS-OBJECT:isInited EQ FALSE THEN RETURN.

      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
      /*THIS-OBJECT:ExcelAppHandle:Visible = FALSE.*/
      DELETE OBJECT THIS-OBJECT:ExcelAppHandle NO-ERROR.
      
      THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
      THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      path = SEARCH("template.xlsx").
      THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(path, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
      THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.

      THIS-OBJECT:Fix().
      
      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.
      
      /*THIS-OBJECT:ExcelAppHandle:Visible = TRUE.*/
      /*THIS-OBJECT:Refresh().*/

   END METHOD.

   METHOD PUBLIC VOID OpenFile ( INPUT path AS CHARACTER ):
      IF System.IO.File:Exists(path) = TRUE THEN DO:
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(path, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
      END.
      ELSE DO:
         System.Windows.Forms.MessageBox:Show("Filen ~" " + path +  " ~" hittades inte!").
      END.
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID OpenFileWithDialog():
      WAIT-FOR DlgOpen:ShowDialog().
      RETURN.
   END METHOD.

   METHOD PUBLIC VOID SaveFile ( ):
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:Save().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileAs ( INPUT path AS CHARACTER  ):
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:SaveAs(path, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,Microsoft.Office.Interop.Excel.XlSaveAsAccessMode:xlNoChange,System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileWithDialog():
      WAIT-FOR DlgSave:ShowDialog().   
      RETURN.
   END METHOD.
   
   

END CLASS.*/