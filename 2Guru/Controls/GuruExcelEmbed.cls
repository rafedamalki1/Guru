  
/*------------------------------------------------------------------------
   File        : GuruExcelEmbed.cls
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
USING Microsoft.Office.Interop.Word.*.
USING Infragistics.Documents.Excel.*.

USING System.Runtime.InteropServices.*.
USING System.Reflection.*.


CLASS Controls.GuruExcelEmbed INHERITS UserControl: 

   DEFINE PUBLIC  VARIABLE chWordApp          AS COM-HANDLE                                NO-UNDO.
   DEFINE PUBLIC  VARIABLE chDoc              AS COM-HANDLE                                NO-UNDO. 
   
   DEFINE PUBLIC  VARIABLE WordAppHandle      AS Microsoft.Office.Interop.Word.Application NO-UNDO.
   DEFINE PRIVATE VARIABLE WordDocHandle      AS Document                                  NO-UNDO.

    
   DEFINE PRIVATE VARIABLE components         AS System.ComponentModel.IContainer          NO-UNDO.
   DEFINE PUBLIC  VARIABLE felexcel           AS LOGICAL                                   NO-UNDO.
   DEFINE PRIVATE VARIABLE DlgSave            AS System.Windows.Forms.SaveFileDialog       NO-UNDO.
   DEFINE PRIVATE VARIABLE DlgOpen            AS System.Windows.Forms.OpenFileDialog       NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE ExcelAppHandle     AS Application                               NO-UNDO.
   DEFINE PUBLIC  VARIABLE ExcelWbHandle      AS Workbook                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE ExcelWorksheet     AS Worksheet                                 NO-UNDO.
   /*
   DEFINE PUBLIC  VARIABLE InfraWorkbook      AS Infragistics.Documents.Excel.Workbook     NO-UNDO.
   DEFINE PUBLIC  VARIABLE InfraWorksheet     AS Infragistics.Documents.Excel.Worksheet    NO-UNDO.
   */
   DEFINE         VARIABLE overunder          AS INTEGER                                   NO-UNDO.
   DEFINE         VARIABLE linetyp            AS INTEGER                                   NO-UNDO.  
  
   DEFINE         VARIABLE workname           AS CHARACTER                                 NO-UNDO.
  
   DEFINE PRIVATE VARIABLE ExcelWindow        AS Window                                    NO-UNDO.
  
   DEFINE PUBLIC  VARIABLE isInited           AS LOGICAL                                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE chWorksheet        AS COM-HANDLE                                NO-UNDO.
   DEFINE PUBLIC  VARIABLE visavarexcel       AS LOGICAL                                   NO-UNDO.
   
   
   
   DEFINE         VARIABLE bladvar            AS INTEGER                                   NO-UNDO.   
   DEFINE         VARIABLE antalbladstart     AS INTEGER                                   NO-UNDO.
   DEFINE         VARIABLE chExcelApplication AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chWorkbook         AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chChart            AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chWorksheetRange   AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chCell             AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chPageBreak        AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE cActiveCell        AS COM-HANDLE                                NO-UNDO.
   DEFINE         VARIABLE chWindow           AS COM-HANDLE                                NO-UNDO.   
   DEFINE         VARIABLE dubbelradhj        AS LOGICAL                                   NO-UNDO.
   DEFINE         VARIABLE BladvarKoll        AS LOGICAL                                   NO-UNDO.  
   DEFINE PUBLIC  VARIABLE SaveColRad             AS CHARACTER                                 NO-UNDO.  /*sparad column och rad ex. e7*/
   DEFINE PUBLIC  VARIABLE ColRad             AS CHARACTER                                 NO-UNDO.  /*aktuell column och rad ex. e7*/
   DEFINE PUBLIC  VARIABLE cColName           AS CHARACTER                                 NO-UNDO. /*aktuell column ex. e*/
   DEFINE PUBLIC  VARIABLE iColNum            AS INTEGER                                   NO-UNDO. /*aktuell nummer på columnen ex e = 5*/
   DEFINE PUBLIC  VARIABLE iSistaColnum       AS INTEGER                                   NO-UNDO. /*den col som fiNns längs till höger*/
   DEFINE VARIABLE iSaveRad                   AS INTEGER                                   NO-UNDO. /*sparrad*/
   DEFINE PUBLIC  VARIABLE iRad               AS INTEGER                                   NO-UNDO. /*aktuell rad ex 7*/
     /*om true ett exel enligt nytt sätt false på gammalt sätt och utanför guru*/
   DEFINE PUBLIC  PROPERTY UseMsInterop        AS LOGICAL  NO-UNDO                                 
   PUBLIC GET. PUBLIC SET.
      
     /*om true ett exel enligt nytt sätt false på gammalt sätt och utanför guru*/
   DEFINE PUBLIC  VARIABLE OutExterntWin      AS LOGICAL                                   NO-UNDO.  /*om true ett exel blad outanför Guru men på nytt sätt*/
   DEFINE PUBLIC  PROPERTY direkticell        AS LOGICAL                                   NO-UNDO
   PUBLIC GET. PUBLIC SET.
    /* direkticell = TRUE skriver direkt i cellen direkticell = false skapar TT som sedan skrivs i cellen.*/
   DEFINE PUBLIC  VARIABLE iRadslut           AS INTEGER                                   NO-UNDO.
   DEFINE PUBLIC  VARIABLE totheight          AS DECIMAL                                   NO-UNDO.
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
   
   DEFINE PUBLIC VARIABLE versNr AS CHARACTER NO-UNDO.
   
   METHOD PUBLIC VOID OpenExcel(visa AS LOGICAL,INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = visa.
      {OpenExcelC.I}
      ASSIGN 
      chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      THIS-OBJECT:ViewType(1).
      
      chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
   
   END METHOD.
   METHOD PUBLIC VOID DoljFram(visa AS LOGICAL,INPUT kolsvep AS CHARACTER):
      
      chWorkSheet:Columns(kolsvep):EntireColumn:Hidden = visa.
 
   END METHOD. 
    
   METHOD PUBLIC VOID OpenExcel(INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE.
      {OpenExcelC.I}
      ASSIGN 
      chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      THIS-OBJECT:ViewType(1).
      
      chWorkSheet:Columns("a:aJ"):EntireColumn:AutoFit NO-ERROR.
   
   END METHOD.
   METHOD PUBLIC VOID OpenExcelejautofit(INPUT excelkommando AS CHARACTER):
      /*för mall som inte klarar autofit Lena 20160512*/
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE.
      {OpenExcelC.I}
      ASSIGN 
      chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      THIS-OBJECT:ViewType(1).
      
   
   END METHOD.
    
   METHOD PUBLIC VOID ReadExcelStart(INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      {OpenExcelC.I}
      ASSIGN 
         chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      ASSIGN 
         chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      ASSIGN 
         iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
   /*{EXCELFEL.I}*/
   END METHOD.
   METHOD PUBLIC VOID StartExcel():
      felexcel = FALSE.
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = TRUE.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      NyttBladExcel().   
      /*
      chWorkSheet = chExcelApplication:Sheets:Item(1).  /*tar blad nr 1*/
      */
      {EXCELFEL.I}
   END METHOD.
   
   METHOD PUBLIC VOID ReleaseExcel(stang AS LOGICAL):
      IF stang = TRUE THEN DO :
         THIS-OBJECT:ExcelAppHandle:Application:Quit() NO-ERROR.
         THIS-OBJECT:ExcelAppHandle:Quit() NO-ERROR.
      END.   
      
      
      THIS-OBJECT:ExcelWorksheet = ? NO-ERROR.
      THIS-OBJECT:ExcelWbHandle = ? NO-ERROR.
      THIS-OBJECT:ExcelAppHandle = ? NO-ERROR.
      
   END METHOD.
   
   
   METHOD PUBLIC VOID StartExcelVal(INPUT startval AS LOGICAL):
      felexcel = FALSE.
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = startval.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
      NyttBladExcel().   
      {EXCELFEL.I}
   END METHOD.
   
   METHOD PUBLIC VOID StartExceld():      
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE NO-ERROR.
      chWorkbook = chExcelApplication:Workbooks:ADD() NO-ERROR. /*nytt exceldoc*/
      chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
      NyttBladExcel().   
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
      SlutExcel(3).
      {EXCELFEL.I}
   END METHOD.
   
   METHOD PUBLIC VOID SidBrytBredd(orientvar AS INTEGER):
      DEFINE VARIABLE PB  AS Microsoft.Office.Interop.Excel.VPageBreak.
      DEFINE VARIABLE num AS INTEGER                                   NO-UNDO.
      
      IF UseMsInterop = TRUE THEN DO:
         
         IF orientvar = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:Orientation = Microsoft.Office.Interop.Excel.XlPageOrientation:xlPortrait.
         THIS-OBJECT:GetWorkSheet():PageSetup:Orientation = Microsoft.Office.Interop.Excel.XlPageOrientation:xlLandscape.
         
         THIS-OBJECT:ExcelAppHandle:ActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlNormalView.
         /*
         Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):BorderAround(XlLineStyle:xlContinuous, XlBorderWeight:xlMedium, XlColorIndex:xlColorIndexAutomatic, System.Reflection.Missing:Value).
         chPageBreak = THIS-OBJECT:GetWorkSheet():VPageBreaks(1).
         Worksheets(1).VPageBreaks(1).Location = Worksheets(1).Range("e5")
         Worksheets(1).HPageBreaks(1).Location = Worksheets(1).Range("e5")
         
         chPageBreak:DragOff(-4161,1) NO-ERROR.
         DEFINE VARIABLE PB AS Microsoft.Office.Interop.Excel.VPageBreak.
         PB = THIS-OBJECT:GetWorkSheet():VPageBreaks[1].
         PB:DragOff(Microsoft.Office.Interop.Excel.XLDirection:xlDown, 1).
         ActiveSheet.VPageBreaks(1).DragOff Direction:=xlToRight, RegionIndex:=1
         
         PB = THIS-OBJECT:GetWorkSheet():VPageBreaks[1].
         
         PB:DragOff(Microsoft.Office.Interop.Excel.XLDirection:xlToRight,1).
         
         THIS-OBJECT:GetWorkSheet():VPageBreaks(1):DragOff(Microsoft.Office.Interop.Excel.XLDirection:xlToRight,1).
         */
         THIS-OBJECT:ExcelAppHandle:ActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageBreakPreview.
      END.
      ELSE DO:    
         chWorkSheet:SELECT NO-ERROR.
         chWorkSheet:PageSetup:Orientation = orientvar NO-ERROR.
         THIS-OBJECT:ViewType(2).

         chPageBreak = chWorkSheet:VPageBreaks:ITEM(1) NO-ERROR.
         chPageBreak:DragOff(-4161,1) NO-ERROR.
         THIS-OBJECT:ViewType(1).

      END.
   END METHOD.
   METHOD PUBLIC VOID DelCol(colnummervar AS INTEGER):
      IF UseMsInterop = TRUE THEN RETURN. 
      chWorkSheet:Columns(getcolname(colnummervar)):EntireColumn:Delete NO-ERROR.
   END METHOD.
   
   METHOD PUBLIC VOID DelCol(colnummervar AS CHARACTER):
      IF UseMsInterop = TRUE THEN RETURN. 
      chWorkSheet:Columns(colnummervar):EntireColumn:Delete NO-ERROR.
   END METHOD.
   METHOD PUBLIC VOID SlutOutExterntWin():
      
      IF OutExterntWin = TRUE THEN DO:
         ExcelWbHandle:CLOSE(System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value) NO-ERROR. 
         ExcelAppHandle:Quit() NO-ERROR.
         THIS-OBJECT:ExcelAppHandle = ? NO-ERROR.
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID SlutOutExternt():

      IF OutExterntWin = TRUE THEN DO:
         /*ExcelWbHandle:CLOSE(System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value) NO-ERROR. 
         ExcelAppHandle:Quit() NO-ERROR.*/
         /*RELEASE OBJECT ExcelWbHandle NO-ERROR.                   
         RELEASE OBJECT ExcelAppHandle NO-ERROR.*/
         /*THIS-OBJECT:ExcelWbHandle = ? NO-ERROR.*/
         THIS-OBJECT:ExcelAppHandle = ? NO-ERROR.
      END.
             
   END METHOD.
   
   METHOD PUBLIC VOID SlutExcel(viewtyp AS INTEGER):  

      IF UseMsInterop = TRUE THEN RETURN. 
      THIS-OBJECT:ViewType(viewtyp).
      chExcelApplication:VISIBLE = TRUE NO-ERROR.
      chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
      bladvar = 0.
      IF BladvarKoll = TRUE THEN 
      DO: 
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
      ELSE IF BladvarKoll = FALSE THEN 
         DO:
            chWorkSheet:SELECT NO-ERROR.      
         END.
         /*
      RELEASE OBJECT chWorkbook NO-ERROR.                   
      RELEASE OBJECT chExcelApplication NO-ERROR.           
      RELEASE OBJECT chPageBreak NO-ERROR.                  
      RELEASE OBJECT chCell NO-ERROR.                       
      RELEASE OBJECT chWorksheet NO-ERROR.                  
      RELEASE OBJECT chChart NO-ERROR.                      
      RELEASE OBJECT chWorksheetRange NO-ERROR.             
      RELEASE OBJECT cActiveCell NO-ERROR. 
      RELEASE OBJECT chWindow NO-ERROR.
      */
      IF chWorkbook NE ? THEN  RELEASE OBJECT chWorkbook NO-ERROR.                    
      IF chExcelApplication NE ? THEN     RELEASE OBJECT chExcelApplication NO-ERROR.           
      IF chPageBreak NE ? THEN            RELEASE OBJECT chPageBreak NO-ERROR.                  
      IF chCell NE ? THEN RELEASE OBJECT chCell NO-ERROR.                       
      IF chWorksheet NE ? THEN            RELEASE OBJECT chWorksheet NO-ERROR.                  
      IF chChart NE ? THEN                RELEASE OBJECT chChart NO-ERROR.                      
      IF chWorksheetRange NE ? THEN       RELEASE OBJECT chWorksheetRange NO-ERROR.             
      IF cActiveCell NE ? THEN            RELEASE OBJECT cActiveCell NO-ERROR. 
      IF chWindow NE ? THEN               RELEASE OBJECT chWindow NO-ERROR.

      ASSIGN
         chWorkbook         = ?  
         chExcelApplication = ?
         chPageBreak        = ?
         chCell             = ?
         chWorksheet        = ?
         chChart            = ?
         chWorksheetRange   = ?
         cActiveCell        = ?.
      chWindow = ?.
    
      {EXCELFEL.I}
   
   END METHOD.
   
   METHOD PUBLIC VOID printmark_UI ():
      IF UseMsInterop = TRUE THEN DO:         
         /* går in flera gånger och skriver ut olika flikar  RELEASE efter alla*/
         THIS-OBJECT:GetWorkSheet():PrintOut( System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,TRUE,STRING(SESSION:PRINTER-NAME), System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value). 
      END.
   END METHOD.
   
   METHOD PUBLIC VOID ScreenUpp(INPUT uppvar AS LOGICAL): 
 
      IF UseMsInterop = TRUE THEN DO:
         
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
      /*Robin Sjöberg Elpool i Umeå AB  14 sep 2015 16:03:35 
      IF THIS-OBJECT:versNr EQ "14.0" THEN DO:
         THIS-OBJECT:ExcelWbHandle:CheckCompatibility = FALSE.
      END.
      MESSAGE THIS-OBJECT:versNr THIS-OBJECT:ExcelWbHandle:CheckCompatibility
      VIEW-AS ALERT-BOX. 
      */
      THIS-OBJECT:SaveFileAs(THIS-OBJECT:DlgSave:FileName).
      RETURN.
  
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GuruExcelEmbed_Resize( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF OutExterntWin = TRUE THEN RETURN.
      IF isInited = TRUE THEN 
      DO:
       
         IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN 
         DO:
            RUN MoveWindow (THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:HEIGHT, 1, OUTPUT ReturnVal).
         END.
         
      END.
      THIS-OBJECT:Refresh().
      RETURN.
   END METHOD.

   METHOD PRIVATE VOID InitializeComponent(  ):
      antalbladstart = 4.
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
      THIS-OBJECT:DlgSave:Filter = "Excel-Arbetsbok |*.xlsx*|Alla filer|*.*".
      THIS-OBJECT:DlgSave:Title = "Spara Excel-fil".
      THIS-OBJECT:DlgSave:DefaultExt = "xlsx".
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
   /*
   METHOD PUBLIC VOID Print ( ):
      IF THIS-OBJECT:isInited = FALSE THEN RETURN.
      Elpool.Helpers:PrintDialog(THIS-OBJECT:ExcelAppHandle).
   END METHOD.
   */
    
   /*Robin Sjöberg Elpool i Umeå AB  3 apr 2014 13:55:41 
     Går inte att avaktiva förhandsgranska knappen i PrintDialogen, detta skapar ett läge där man inte kan komma ur embedded excel.
     Därför skapar vi kopia av filen, öppnar den i nytt fönster samt startar printdialog i det den nya sessionen.
      Testade lösningsförslag: 
      x Input arg6 = false i PrintDialog metoden i DLL, gör ingen skillnad
      x Skriv ut med infragisticskontroller, finns inget stöd
      x Hitta printpreview event för att "interupta" , ej funnet
      x COM-Handle-anrop
      
      
      Ej testad lösning:
      x Gör en egen "printdialog"
   */

   METHOD PUBLIC VOID Print( ):
      IF THIS-OBJECT:isInited = FALSE THEN RETURN.
      IF THIS-OBJECT:ExcelAppHandle:ActiveWorkbook = ? THEN RETURN.
      DEFINE VARIABLE filnamn         AS CHARACTER   INITIAL "" NO-UNDO.
      DEFINE VARIABLE ExcelAppHandle2 AS Application NO-UNDO.
      DEFINE VARIABLE ExcelWbHandle2  AS Workbook    NO-UNDO.
      filnamn = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
      filnamn = filnamn + "skrivfil" + string(time) + ".xls".
      

      
      ExcelAppHandle2 = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      ExcelAppHandle2:Visible = TRUE.
      THIS-OBJECT:SaveFileAs(filnamn).
      ExcelWbHandle2 = ExcelAppHandle2:Workbooks:Open(System.IO.Path:GetFullPath(filnamn), System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
      ExcelWbHandle2:PrintPreview(FALSE).
      /*Elpool.Helpers:PrintDialog(ExcelAppHandle2).*/
      
      OS-DELETE VALUE(filnamn) NO-ERROR.
      ExcelAppHandle2:Quit().
   END METHOD.
    /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:47:08 
    Startar Excel Interop versions koll av excel 
    */
   METHOD PUBLIC VOID InteropInitialize (path AS CHARACTER ):
      
      IF THIS-OBJECT:isInited = FALSE THEN DO:
         THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         THIS-OBJECT:versNr = THIS-OBJECT:VersionKontroll().
         THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
         IF OutExterntWin = TRUE THEN DO:
            /*behövs inte
            path = THIS-OBJECT:ExcelTemplateStart(path).
            */
            THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:add(path).            
         END.
            
         ELSE THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(System.IO.Path:GetFullPath(path),
         System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,
         System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,
         System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,
         System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
         THIS-OBJECT:ExcelWbHandle:Activate().
         IF OutExterntWin = TRUE THEN THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = TRUE.
         ELSE THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.
         THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
         THIS-OBJECT:ExcelAppHandle:Visible = true.
         THIS-OBJECT:ExcelWindow = THIS-OBJECT:ExcelAppHandle:ActiveWindow.
         /*behövs här för att hålla koll på alla excel fönster*/
         THIS-OBJECT:Fix().
         
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.
         isInited = TRUE.
         
      END.
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID flerArk (path AS CHARACTER ):
         THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
         THIS-OBJECT:ExcelAppHandle = ? NO-ERROR.
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         THIS-OBJECT:versNr = THIS-OBJECT:VersionKontroll().
         THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
         THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
            /*behövs inte
            path = THIS-OBJECT:ExcelTemplateStart(path).
            */
            THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:add(path).
            THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = TRUE.
      
            THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
            THIS-OBJECT:ExcelAppHandle:Visible = true.
            THIS-OBJECT:ExcelWindow = THIS-OBJECT:ExcelAppHandle:ActiveWindow.
            /*behövs här för att hålla koll på alla excel fönster*/
            THIS-OBJECT:Fix().
            
            THIS-OBJECT:ExcelAppHandle:ScreenUpdating = TRUE.            
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID Fix():
      DEFINE VARIABLE oldhWnd AS INTEGER NO-UNDO.
      DEFINE VARIABLE style   AS INTEGER NO-UNDO.
      DEFINE VARIABLE exstyle AS INTEGER NO-UNDO.
      IF OutExterntWin = TRUE THEN RETURN.
      /* Sätter excels parentwindow till exceltabben */
       
      RUN SetParent IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:hwnd, INPUT THIS-OBJECT:Handle:ToInt32(), OUTPUT oldhWnd). 
     
      /*(THIS-OBJECT:VisaExcelApp:Hwnd, THIS-OBJECT:VisaExcelTab:Handle:ToInt32(), OUTPUT ReturnVal).*/
      /* Hämtar stilparametrar, tar bort titel, ram/border, och möjlighet för kund att förstora och minimera */
      
      RUN GetWindowLongA IN WinApi(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, {&GWL_STYLE}, OUTPUT style). 
      style = style - {&WS_CAPTION} - {&WS_THICKFRAME} - {&WS_BORDER} - {&WS_MAXIMIZEBOX} - {&WS_MINIMIZEBOX}  . /* + {&WS_CAPTION}.*/
      RUN SetWindowLongA IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT {&GWL_STYLE}, INPUT style, OUTPUT oldhWnd).
      /* extended stilparams */
      
      RUN GetWindowLongA IN WinApi(INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, {&GWL_EXSTYLE}, OUTPUT exstyle). 
      exstyle = exstyle -  {&WS_EX_APPWINDOW}.
      RUN SetWindowLongA IN WinApi (INPUT THIS-OBJECT:ExcelAppHandle:Hwnd, INPUT {&GWL_EXSTYLE}, INPUT exstyle, OUTPUT oldhWnd).
     
      /* TODO - Göm ifrån taskbar och implementera excel-fil-hanterare i kontrollen(tabbar?) ? */
     
      /* Flyttar och passar in fönstret i exceltabben */
      IF THIS-OBJECT:ExcelAppHandle:Hwnd NE ? THEN DO:
         RUN MoveWindow (THIS-OBJECT:ExcelAppHandle:Hwnd, 0,0, THIS-OBJECT:Width, THIS-OBJECT:HEIGHT, 1, OUTPUT ReturnVal).
         
      END.
     
      THIS-OBJECT:Refresh().

   END METHOD.

   METHOD PUBLIC VOID Reload(INPUT path AS CHARACTER):

      IF THIS-OBJECT:isInited EQ FALSE THEN RETURN.
      THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      THIS-OBJECT:ExcelAppHandle:DisplayAlerts = FALSE.
      THIS-OBJECT:ExcelAppHandle:ScreenUpdating = FALSE.
     
      THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(System.IO.Path:GetFullPath(path), System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
      
      THIS-OBJECT:ExcelWbHandle:Activate().
      THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayWorkbookTabs = FALSE.
      THIS-OBJECT:ExcelAppHandle:UserControl = FALSE.
      THIS-OBJECT:ExcelAppHandle:Visible = FALSE.
      /*behövs här för att inte visa fönstret i onödan*/
      THIS-OBJECT:Fix().
      /*
      THIS-OBJECT:ScreenUpp(true).*/ 
      THIS-OBJECT:Refresh().
   END METHOD.

   METHOD PUBLIC VOID OpenFile ( INPUT path AS CHARACTER ):
      IF System.IO.File:Exists(path) = TRUE THEN DO:
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open(path, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value ).
         THIS-OBJECT:ExcelWbHandle:Activate().
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
  METHOD PUBLIC VOID AktiveraWB():
     THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:Activate().
  END METHOD. 
   METHOD PUBLIC VOID SaveFile ( ):
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:Save().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID SaveFileAs ( INPUT path AS CHARACTER  ):
      /*Robin Sjöberg Elpool i Umeå AB  18 feb 2014 15:51:21 
      Ändrat  från saveas till savecopy för att undvika att att filen blir skrivskyddad när man öppnar den i excel medan man fortfarande kör kalkylen.
      */
      /*THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:SaveAs(path, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,Microsoft.Office.Interop.Excel.XlSaveAsAccessMode:xlNoChange,System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).*/
      THIS-OBJECT:ExcelAppHandle:ActiveWorkbook:SaveCopyAs(path).
      RETURN.
   END METHOD.

   METHOD PUBLIC VOID SaveFileWithDialog():
      WAIT-FOR DlgSave:ShowDialog().
      DlgSave:FileName = "".
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
   
   /*EVENTS*/
   
   METHOD PUBLIC CHARACTER IhopCeller(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      /* 
   leftcenterright = 2 vänster
   leftcenterright = 3 centrerat
   leftcenterright = 4 höger 
   */
    DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF UseMsInterop = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),tillfran):MergeCells = TRUE.
         
      END.   
      ELSE DO:
         chWorkSheet:Range(tillfran):HorizontalAlignment = leftcenterright.
         /* chWorkSheet:Range(tillfran):Wraptext.*/
         chWorkSheet:Range(tillfran):Merge.
      END.  
      
   END METHOD.
   METHOD PUBLIC CHARACTER IhopCellerOver(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      IF UseMsInterop = TRUE THEN .
      ELSE DO:
         chWorkSheet:Range(tillfran):HorizontalAlignment = leftcenterright.
         chWorkSheet:Range(tillfran):Mergecells.
      END.
      
   END METHOD.    
   METHOD  PUBLIC CHARACTER Center(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      IF UseMsInterop = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),tillfran):HorizontalAlignment = leftcenterright.
         
      END.   
      ELSE DO:
         chWorkSheet:Range(tillfran):HorizontalAlignment = leftcenterright.
         /* chWorkSheet:Range(tillfran):Wraptext.*/
   
      END.   
  
   /*leftcenterright = 4 höger 
   leftcenterright = 2 vänster
   leftcenterright = 3 centrerat*/


    END METHOD.
    
    METHOD  PUBLIC CHARACTER CenterUpDown(tillfran AS CHARACTER, INPUT leftcenterright AS INTEGER):
      IF UseMsInterop = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),tillfran):VerticalAlignment = leftcenterright.
         
      END.   
      ELSE DO:
         chWorkSheet:Range(tillfran):VerticalAlignment = leftcenterright.
         /* chWorkSheet:Range(tillfran):Wraptext.*/
   
      END.   
  
   /*leftcenterright = 1 UPP 
   leftcenterright = 2 CENTER
   leftcenterright = 3 NER*/


    END METHOD.  
   METHOD PUBLIC CHARACTER valueDataIn(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      IF direkticell = TRUE THEN 
      DO:
         IF UseMsInterop = TRUE THEN invar = Elpool.Helpers:GetRange(GetWorkSheet(),outpos):TEXT. 
         ELSE DO:
             invar = chWorkSheet:Range(outpos):TEXT.
             IF invar = "" THEN invar = chWorkSheet:Range(outpos):VALUE.
         END.    
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
         IF UseMsInterop = TRUE THEN DO:
            Elpool.Helpers:GetRange(GetWorkSheet(),outpos):VALUE2 = outdata.
         END.   
         ELSE chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
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
   /*skrivar ut all data från tt*/
   METHOD PUBLIC VOID valueDataOutTT():
      IF direkticell = TRUE THEN RETURN.
      FOR EACH excellTT WHERE NO-LOCK:
         IF excellTT.KOLUMNRAD = "" OR excellTT.KOLUMNRAD = ? THEN.
         ELSE 
         DO:
            IF UseMsInterop = TRUE THEN 
            DO:
               /*
               
               Elpool.Helpers:GetRange(GetWorkSheet(),excellTT.KOLUMNRAD):TEXT = excellTT.VARDET.
               */
               Elpool.Helpers:GetRange(GetWorkSheet(),excellTT.KOLUMNRAD):VALUE2 = excellTT.VARDET.
               
            END.   
            ELSE chWorkSheet:Range(excellTT.KOLUMNRAD):VALUE =  excellTT.VARDET NO-ERROR.
         END.   
      END.
      EMPTY TEMP-TABLE excellTT NO-ERROR.
   END METHOD.

   METHOD PUBLIC VOID PageSetuppHeaderImage(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      
      IF THIS-OBJECT:UseMsInterop = TRUE THEN  
      DO:
         IF vad = 1 THEN 
         DO:
            THIS-OBJECT:GetWorkSheet():PageSetup:LeftHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).
            THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeader = "&V&G".            
         END.   
         IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).
         IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightHeaderPicture:Filename = System.IO.Path:GetFullPath(detta).                    
      END.
      ELSE 
      DO:
         ColumnRad().   
         chWorkSheet:range(ColRad):SELECT NO-ERROR. 
         IF vad = 1 THEN 
         DO: 
            chWorkSheet:PageSetup:LeftHeaderPicture:Filename = detta .
            chWorkSheet:PageSetup:LeftHeader = "&V&G".
         END.   
         IF vad = 2 THEN 
         DO:
            chWorkSheet:PageSetup:CenterHeaderPicture:Filename = detta .
            chWorkSheet:PageSetup:CenterHeader = "&V&G".
         END.   
         IF vad = 3 THEN 
         DO:
            chWorkSheet:PageSetup:RightHeaderPicture::Filename = detta .
            chWorkSheet:PageSetup:RightHeader = "&V&G".
         END.
      END.
   END METHOD. 
   METHOD PUBLIC VOID PageSetuppHeader(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      
      IF THIS-OBJECT:UseMsInterop = TRUE THEN DO:
         IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftHeader = detta.
         IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterHeader = detta.
         IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightHeader = detta.
      END.    
      ELSE DO:
         IF vad = 1 THEN chWorkSheet:PageSetup:LeftHeader = detta NO-ERROR.
         IF vad = 2 THEN chWorkSheet:PageSetup:CenterHeader = detta NO-ERROR.
         IF vad = 3 THEN chWorkSheet:PageSetup:RightHeader = detta NO-ERROR.
      END.       
   /*'&"Calibri"&30' */     
   END METHOD. 
   METHOD PUBLIC VOID PageSetuppFooter(INPUT vad AS INTEGER, INPUT detta AS CHARACTER):
      IF THIS-OBJECT:UseMsInterop = TRUE THEN  
      DO:
         IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftFooter = detta.
         IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:CenterFooter = detta.
         IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightFooter = detta.
      END.    
      ELSE 
      DO:
         IF vad = 1 THEN chWorkSheet:PageSetup:LeftFooter = detta NO-ERROR.
         IF vad = 2 THEN chWorkSheet:PageSetup:CenterFooter = detta NO-ERROR.
         IF vad = 3 THEN chWorkSheet:PageSetup:RightFooter = detta NO-ERROR.
      END.       
   /*'&"Calibri"&30' */     
   END METHOD. 
   
   METHOD PUBLIC VOID PageSetuppMargin(INPUT vad AS INTEGER, INPUT detta AS DECIMAL):
      IF THIS-OBJECT:UseMsInterop = TRUE THEN  
      DO:
         IF vad = 1 THEN THIS-OBJECT:GetWorkSheet():PageSetup:LeftMargin = detta.
         IF vad = 2 THEN THIS-OBJECT:GetWorkSheet():PageSetup:TopMargin = detta.
         IF vad = 3 THEN THIS-OBJECT:GetWorkSheet():PageSetup:RightMargin = detta.
         IF vad = 4 THEN THIS-OBJECT:GetWorkSheet():PageSetup:BottomMargin = detta.
         IF vad = 5 THEN THIS-OBJECT:GetWorkSheet():PageSetup:HeaderMargin = detta.
         IF vad = 6 THEN THIS-OBJECT:GetWorkSheet():PageSetup:FooterMargin = detta.
      END.    
      ELSE 
      DO:
         IF vad = 1 THEN chWorkSheet:PageSetup:LeftMargin = detta.
         IF vad = 2 THEN chWorkSheet:PageSetup:TopMargin = detta.
         IF vad = 3 THEN chWorkSheet:PageSetup:RightMargin = detta.
         IF vad = 4 THEN chWorkSheet:PageSetup:BottomMargin = detta.
         IF vad = 5 THEN chWorkSheet:PageSetup:HeaderMargin = detta.
         IF vad = 6 THEN chWorkSheet:PageSetup:FooterMargin = detta.
      END.       
   /*'&"Calibri"&30' */     
   END METHOD. 
   
   
      
   METHOD PUBLIC VOID PageSetSidnum(INPUT vad AS INTEGER, topdown AS LOGICAL):
      DEFINE VARIABLE detta AS CHARACTER NO-UNDO.
      detta = " sid &S".
      IF THIS-OBJECT:UseMsInterop = TRUE THEN  
      DO:
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
      END.    
      ELSE 
      DO:
         IF topdown = TRUE THEN 
         DO:
            IF vad = 1 THEN chWorkSheet:PageSetup:LeftHeader = detta NO-ERROR.
            IF vad = 2 THEN chWorkSheet:PageSetup:CenterHeader = detta NO-ERROR.
            IF vad = 3 THEN chWorkSheet:PageSetup:RightHeader = detta NO-ERROR.
         END.
         ELSE 
         DO:
            IF vad = 1 THEN chWorkSheet:PageSetup:LeftFooter = detta NO-ERROR.
            IF vad = 2 THEN chWorkSheet:PageSetup:CenterFooter = detta NO-ERROR.
            IF vad = 3 THEN chWorkSheet:PageSetup:RightFooter = detta NO-ERROR.
         END.      
      END.       
   /*'&"Calibri"&30' */     
   END METHOD. 
   
   METHOD PUBLIC VOID ViewType(INPUT vad AS INTEGER):
     
      IF UseMsInterop = FALSE THEN DO: 
         /* VAD 1 = vad 2 från gamla och tvärt om */         
         chWindow:VIEW = vad NO-ERROR.   
      END.
      ELSE DO:
         IF vad = 1 THEN  THIS-OBJECT:ExcelAppHandle:ActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageBreakPreview.
         IF vad = 2 THEN  THIS-OBJECT:ExcelAppHandle:ActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlNormalView.
         IF vad = 3 THEN  THIS-OBJECT:ExcelAppHandle:ActiveWindow:View = Microsoft.Office.Interop.Excel.XlWindowView:xlPageLayoutView.
         
      END.            
   END METHOD.    
   METHOD PUBLIC INTEGER HittaGetData(outcol AS CHARACTER, sokvar AS CHARACTER):
      DEFINE VARIABLE invar AS INTEGER NO-UNDO.
      IF direkticell = TRUE THEN 
      DO:
         IF UseMsInterop = TRUE THEN /* EJ KLARinvar = Elpool.Helpers:GetRange(GetWorkSheet(),outpos):TEXT*/. 
         ELSE /* EJ KLAR invar = chWorkSheet:Range(outpos):TEXT*/.
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
   
   METHOD PUBLIC VOID DataOutFormatch(outpos AS CHARACTER,outdata AS CHARACTER):
      chWorkSheet:Range(outpos):NumberFormat = "@".
      chWorkSheet:Range(outpos):VALUE = outdata NO-ERROR.
      
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
         IF UseMsInterop = TRUE THEN DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):FormulaR1C1 = formelvar.
         END.   
         ELSE DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            chWorkSheet:Range(cRangefont):Formula = formelvar  NO-ERROR.
         END.
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
         IF UseMsInterop = TRUE THEN DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):FormulaR1C1 = formelvar.
         END.   
         ELSE DO:
            formelvar = "=SUMMA(R[-" + STRING(INTEGER(Getrad(cRangefont)) - INTEGER(Getrad(cStart))) + "]C:R[-1]C)".
            chWorkSheet:Range(cRangefont):Formula = formelvar  NO-ERROR.
         END.
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
         IF UseMsInterop = TRUE THEN DO:
            Elpool.Helpers:GetRange(GetWorkSheet(),cRangefont):FormulaR1C1 = formelvar.
         END.   
         ELSE DO:
            chWorkSheet:Range(cRangefont):Formula = formelvar.  
         END.      
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
   METHOD PUBLIC VOID ShrinkToFit(cStart AS CHARACTER, cRange AS CHARACTER, vad AS LOGICAL):
      DEFINE VARIABLE inrad AS CHARACTER NO-UNDO.   
      inrad = STRING(iRad) + ":" + STRING(iRad).
      IF UseMsInterop = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRange):ShrinkToFit = vad NO-ERROR.
      END.
      ELSE DO:
         chWorkSheet:Range(inrad):ShrinkToFit = vad.
      END.
   END METHOD.
   METHOD PUBLIC VOID TextWrap(cStart AS CHARACTER):
      DEFINE VARIABLE inrad AS CHARACTER NO-UNDO.   
      inrad = STRING(iRad) + ":" + STRING(iRad).
      IF UseMsInterop = TRUE THEN DO:
         Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cStart):WrapText = TRUE NO-ERROR.
      END.
      ELSE DO:
         chWorkSheet:Range(inrad):WrapText = TRUE.
      END.
   END METHOD.
   METHOD PUBLIC CHARACTER TextBryt(intext AS CHARACTER, hurofta AS INTEGER):
      DEFINE VARIABLE outtext AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varvraknare AS INTEGER NO-UNDO.
      outtext = "".
      varvraknare = 1.
      REPEAT:
         outtext = outtext + SUBSTRING(intext,varvraknare,hurofta)  + Guru.Konstanter:globradbrytch.
         varvraknare = varvraknare + hurofta.
         IF varvraknare > LENGTH(intext) THEN LEAVE.         
      END.
      RETURN outtext.       
   END METHOD.   
   
   METHOD PUBLIC VOID FontExcel(cStart AS CHARACTER, cRangefont AS CHARACTER,varfont AS CHARACTER,varsize AS INTEGER,varbold AS LOGICAL,varhojd AS INTEGER,varfarg AS INTEGER ):
      IF UseMsInterop = TRUE THEN DO: 
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
    
   
   METHOD PUBLIC VOID ColWidth(cStart AS CHARACTER, widthsize AS DECIMAL):
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart):ColumnWidth = widthsize. 
      ELSE chWorkSheet:Range(cStart):ColumnWidth = widthsize.             
   END METHOD.
   
   METHOD PUBLIC DECIMAL ColWidth(cStart AS CHARACTER):
      DEFINE VARIABLE cwidth AS CHARACTER NO-UNDO.
      IF UseMsInterop = TRUE THEN cwidth = STRING(Elpool.Helpers:GetRange(GetWorkSheet(),cStart):ColumnWidth).
      ELSE cwidth = STRING(chWorkSheet:Range(cStart):ColumnWidth).
      RETURN DECIMAL(cwidth).       
   END METHOD.
   
   METHOD PUBLIC VOID Border(cStart AS CHARACTER, cRangefont AS CHARACTER):
      
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont):BorderAround(XlLineStyle:xlContinuous, XlBorderWeight:xlMedium, XlColorIndex:xlColorIndexAutomatic, System.Reflection.Missing:Value).
      ELSE DO:
         cRangefont = cStart + ":" + cRangefont.
      END.   
   END METHOD.
   METHOD PUBLIC VOID Linedubble(cStart AS CHARACTER, cRangefont AS CHARACTER):
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(),cStart,cRangefont), XlBordersIndex:xlEdgeBottom):LineStyle = XlLineStyle:xlDouble.       
      ELSE DO:
         cRangefont = cStart + ":" + cRangefont.
      END.  
   END METHOD.
  
   METHOD PUBLIC VOID BordHoriz(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):
      /*3= över 4= under*/     
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont  ), XlBordersIndex:xlInsideHorizontal):LineStyle = XlLineStyle:xlDot.
      ELSE DO:
         cRangefont = cStart + ":" + cRangefont.
         IF typ BEGINS "x" THEN typ = "4:7".
         overunder =  INTEGER(SUBSTRING(typ,1,INDEX(typ,":") - 1)).
         linetyp =  INTEGER(SUBSTRING(typ,INDEX(typ,":") + 1)).
         chWorkSheet:Range(cRangefont):Borders(overunder):LineStyle = linetyp.               
      END.  
   END METHOD.
   
   METHOD PUBLIC VOID BordHorizW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideHorizontal):Weight = XlBorderWeight:xlThin.
   END METHOD.
   
   METHOD PUBLIC VOID BordVert(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      /*1= VÄNSTER 2= HÖGER*/ 
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideVertical):LineStyle = XlLineStyle:xlDot.
      ELSE 
      DO:
         cRangefont = cStart + ":" + cRangefont.
         IF typ BEGINS "x" THEN typ = "4:7".
         overunder =  INTEGER(SUBSTRING(typ,1,INDEX(typ,":") - 1)).
         linetyp =  INTEGER(SUBSTRING(typ,INDEX(typ,":") + 1)).
         chWorkSheet:Range(cRangefont):Borders(overunder):LineStyle = linetyp.               
      END.  
   END METHOD.
   METHOD PUBLIC VOID BordVertW(cStart AS CHARACTER, cRangefont AS CHARACTER, typ AS CHARACTER):       
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(GetWorkSheet(), cStart,cRangefont), XlBordersIndex:xlInsideVertical):Weight = XlBorderWeight:xlThin.
   END METHOD.
   METHOD PUBLIC VOID AddKom(INPUT outpos AS CHARACTER,INPUT textvar AS CHARACTER): 
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(),outpos):addcomment(textvar).      
      ELSE chWorkSheet:Range(outpos):addcomment(textvar) NO-ERROR.
   END METHOD.  
   METHOD PUBLIC VOID ValjBlad(valbald AS INTEGER):
      RELEASE OBJECT chWorkSheet NO-ERROR.
      IF UseMsInterop = TRUE THEN ExcelWbHandle:Sheets:SELECT(valbald).      
      ELSE chWorkSheet = chExcelApplication:Sheets:ITEM(valbald) NO-ERROR.
      iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
      
      IF chWorkSheet = ? THEN NyttBladExcel().
      /*IF UseMsInterop = TRUE THEN MESSAGE THIS-OBJECT:GetWorkSheet():NAME
      VIEW-AS ALERT-BOX.     
      ELSE  
      MESSAGE chWorkSheet:NAME
      VIEW-AS ALERT-BOX.*/
      
      
      {EXCELFEL.I} 
   END METHOD.
  
   METHOD PUBLIC VOID namnbladexcel(namnvar AS CHARACTER):
      IF UseMsInterop = TRUE THEN THIS-OBJECT:GetWorkSheet():NAME = namnvar.     
      ELSE chWorkSheet:NAME = namnvar.
      {EXCELFEL.I} 
   END METHOD.
   METHOD PUBLIC CHARACTER GetNamnBlad():
      IF UseMsInterop = TRUE THEN  RETURN THIS-OBJECT:GetWorkSheet():NAME.     
      ELSE RETURN chWorkSheet:NAME.
   END METHOD.   
   METHOD  PUBLIC CHARACTER KolumnFormatGet (kolvar AS CHARACTER):
      DEFINE VARIABLE formvar AS CHARACTER NO-UNDO.

      IF UseMsInterop = TRUE THEN formvar = Elpool.Helpers:GetRange(GetWorkSheet(), kolvar):NumberFormat.  
      ELSE formvar = chWorkSheet:Range(kolvar):NumberFormat.
      RETURN formvar.
   END METHOD.
   METHOD PUBLIC VOID KolumnFormat(kolvar AS CHARACTER, formvar AS CHARACTER):
      IF UseMsInterop = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(), kolvar):NumberFormat = formvar.  
      ELSE chWorkSheet:Range(kolvar):NumberFormat = formvar.
   
   END METHOD.
   
   METHOD PUBLIC VOID KopierBladEfter():
      chWorkSheet = chWorkSheet:COPY(,chWorksheet) .
      
   END METHOD.
   
   METHOD PUBLIC VOID NyttBladExcel():
      IF UseMsInterop = TRUE THEN ExcelWbHandle:Sheets:ADD(?,?,ExcelWbHandle:Sheets:count,?). 
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
   
   /*Retunerarbladvar*/
   METHOD PUBLIC INTEGER NyttBladExcelVar():
      IF UseMsInterop = TRUE THEN ExcelWbHandle:Sheets:ADD(?,?,ExcelWbHandle:Sheets:count,?). 
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
      END.       
   END METHOD.
   
   
   
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
           
   END METHOD. 

   METHOD PUBLIC INTEGER GetBladvar():
      RETURN bladvar.
   END METHOD. 
   
   METHOD PUBLIC VOID SetBladvar(newbladvar AS INTEGER):
      bladvar = newbladvar.
      ValjBlad(bladvar).
   END METHOD.
   
   METHOD PUBLIC VOID bladvarVersion():
      {EXLELVERSION.I}
      ELSE antalbladstart = 4.
      
         
      {EXCELFEL.I}
   END METHOD.
   
   METHOD PUBLIC CHARACTER VersionKontroll():
       /*Robin Sjöberg Elpool i Umeå AB  21 sep 2015 11:18:19 
      THIS-OBJECT:OutExterntWin = TRUE. För alla, excel embedded "skrotat"
      */
      DEFINE VARIABLE versNr AS CHARACTER NO-UNDO.
      THIS-OBJECT:OutExterntWin = TRUE.
      
      RETURN versNr.


   END METHOD.
   /*
   METHOD PUBLIC VOID UseInterop(INPUT interopvar AS LOGICAL):
      THIS-OBJECT:UseMsInterop = interopvar.
      /*
      RETURN THIS-OBJECT:UseMsInterop.
      */
   END METHOD.
   */
   /*
   METHOD PUBLIC VOID cellFormat(INPUT range AS CHARACTER, newFormat AS CHARACTER):
       IF UseMsInterop = TRUE THEN Elpool.Helpers:GetRange(GetWorkSheet(), range):NumberFormat = newFormat.      
       ELSE chWorkSheet:Range(range):NumberFormat = newFormat.
   END METHOD.
   
   
   METHOD PUBLIC VOID OpenWord(INPUT path AS CHARACTER):
         THIS-OBJECT:WordAppHandle = NEW Microsoft.Office.Interop.Word.ApplicationClass().
         THIS-OBJECT:WordAppHandle = Elpool.Helpers:OppnaDoc(WordAppHandle, path).
         THIS-OBJECT:WordAppHandle:Visible = TRUE.
   END METHOD. 
   
   METHOD PUBLIC VOID OpenUnderlag(INPUT path AS CHARACTER):
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open("C:\underlagklg11.xlsx", System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
         THIS-OBJECT:ExcelAppHandle:Visible = TRUE.
         /*THIS-OBJECT:ExcelAppHandle:ActiveWindow:DisplayGridlines = FALSE.*/
   END METHOD. 
   
   METHOD PUBLIC VOID StangUnderlag():
         THIS-OBJECT:ExcelAppHandle:Quit().
   END METHOD.
   
   METHOD PUBLIC VOID Sidbrytning():
      THIS-OBJECT:WordAppHandle = Elpool.Helpers:Sidbrytning(WordAppHandle).    
   END METHOD.
   
   METHOD PUBLIC VOID tillBokmarke(bokmarke AS CHARACTER):
      THIS-OBJECT:WordAppHandle = Elpool.Helpers:TillBokmarke(WordAppHandle, bokmarke).    
   END METHOD.   
   
   METHOD PUBLIC VOID KopieraOmrade(omrade AS CHARACTER):
      Elpool.Helpers:KopieraOmrade(GetWorkSheet(), omrade).    
   END METHOD.
   
   METHOD PUBLIC VOID KlistraIn():
      THIS-OBJECT:WordAppHandle = Elpool.Helpers:KlistraIn(WordAppHandle).    
   END METHOD.
   
   METHOD PUBLIC VOID VridBild(rotation AS INTEGER):
      THIS-OBJECT:WordAppHandle = Elpool.Helpers:VridBild(WordAppHandle, rotation).    
   END METHOD.
   
   METHOD PUBLIC VOID HanteraBild(hojd AS INTEGER, bredd AS INTEGER, topdistans AS INTEGER, leftdistans AS INTEGER):
      THIS-OBJECT:WordAppHandle = Elpool.Helpers:HanteraBild(WordAppHandle, hojd, bredd, topdistans, leftdistans).    
   END METHOD.
   
   METHOD PUBLIC VOID ValjSheet(namn AS CHARACTER):
      THIS-OBJECT:ExcelAppHandle = Elpool.Helpers:ValjSheet(THIS-OBJECT:ExcelAppHandle, namn).     
   END METHOD.

   METHOD PUBLIC VOID KopieraTillWord():
       
   END METHOD.
   */
   
   METHOD PUBLIC VOID Kopiera(omrade AS CHARACTER):
   /*DEFINE VARIABLE path AS CHARACTER.
   THIS-OBJECT:WordAppHandle = NEW Microsoft.Office.Interop.Word.ApplicationClass().
   THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
   ASSIGN path = "C:\kattest.docx".
   THIS-OBJECT:WordAppHandle = Elpool.Helpers:OppnaDoc(WordAppHandle, path).
         
   THIS-OBJECT:WordDocHandle = NEW Microsoft.Office.Interop.Word.DocumentClass().
   THIS-OBJECT:WordDocHandle = THIS-OBJECT:WordAppHandle:ActiveDocument.
   THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open("C:\EKGKLG1.xlsx", System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
   THIS-OBJECT:WordDocHandle = Elpool.Helpers:KopieraBild2(GetWorkSheet(), WordDocHandle, omrade).
   THIS-OBJECT:WordDocHandle:PrintPreview().*/
        
   /*chWordApp:Selection:PasteExcelTable(FALSE, FALSE, FALSE) NO-ERROR.*/
   /*chEWorkSheet:range(omrade):Copy NO-ERROR.*/
   /*chWordApp:Selection:PasteSpecial(System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value, System.Reflection.Missing:Value,, System.Reflection.Missing:Value, System.Reflection.Missing:Value) NO-ERROR.*/
   /*chWordApp:Selection:Paste NO-ERROR.*/
   /*chWordApp:Selection:PasteAndFormat(System.Reflection.Missing:Value) NO-ERROR.*/
   /*chWordApp:ActiveDocument:Paste NO-ERROR.*/
   /*chWordApp:Pictures:Paste NO-ERROR.*/
   /*chDoc:Content:Paste.*/
   /*chWordApp:Documents:SaveAs("C:\test2.docx",,,,,,,,,) NO-ERROR.*/
   END METHOD.
   
   /*
   METHOD PUBLIC VOID Kopiera(omrade AS CHARACTER):
         /*DEFINE VARIABLE path AS CHARACTER.
         DEFINE VARIABLE bokhuvud AS CHARACTER.
         DEFINE VARIABLE boksida AS CHARACTER.
         DEFINE VARIABLE bokfot AS CHARACTER.
         
         THIS-OBJECT:WordAppHandle = NEW Microsoft.Office.Interop.Word.ApplicationClass().
         THIS-OBJECT:ExcelAppHandle = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
         ASSIGN path = "C:\kattest.docx".
         THIS-OBJECT:WordAppHandle = Elpool.Helpers:OppnaDoc(WordAppHandle, path).
         
         THIS-OBJECT:WordDocHandle = NEW Microsoft.Office.Interop.Word.DocumentClass().
         THIS-OBJECT:WordDocHandle = THIS-OBJECT:WordAppHandle:ActiveDocument.
         
         ASSIGN boksida = "sida".
         ASSIGN bokhuvud = "huvud".
         ASSIGN bokfot = "fot".
         
         THIS-OBJECT:ExcelWbHandle = THIS-OBJECT:ExcelAppHandle:Workbooks:Open("C:\EKGKLG1.xlsx", System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
         THIS-OBJECT:WordAppHandle = Elpool.Helpers:KopieraBild(GetWorkSheet(), WordAppHandle, omrade, boksida).
         /*THIS-OBJECT:WordAppHandle = Elpool.Helpers:LiggandeSida(WordAppHandle, boksida).
         THIS-OBJECT:WordAppHandle = Elpool.Helpers:LiggandeHuvud(WordAppHandle, boksida, bokhuvud).
         ASSIGN boksida = "sida2".
         THIS-OBJECT:WordAppHandle = Elpool.Helpers:LiggandeFot(WordAppHandle, boksida, bokfot).*/
         THIS-OBJECT:WordDocHandle:PrintPreview().
         THIS-OBJECT:ExcelAppHandle:Quit().*/
   END METHOD.*/
   
   /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:40:49 
   dessa två körs inte! 
   */   
   METHOD PUBLIC CHARACTER ExcelNamn(INPUT worknamein AS CHARACTER):
      workname = worknamein.
   END METHOD.
   METHOD PUBLIC CHARACTER ExcelTemplateStart(INPUT ExelKommandout AS CHARACTER):
      DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
      kommando = SESSION:TEMP-DIRECTORY + Guru.konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando) NO-ERROR.
      kommando = workname.
      
      OS-COPY VALUE(ExelKommandout) VALUE(kommando).
      RETURN kommando.
   END METHOD.
END CLASS.


 /*
 
      =SUM(RC[-3]:RC[-1]) hor
      =SUMMA(R[-2]C:R[-1]C) våg
          
   ASSIGN
   cActiveCell = chExcelApplication:ActiveCell
   cActiveCell:VALUE = varut NO-ERROR.
   cActiveCell:value = outdata.
   Worksheets("Sheet1").Range("A1").Value = 3.14159
   Worksheets("Sheet1").Range("A1").Formula = "=$A$4+$A$10"
   interop.excel.range    
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
 
 

