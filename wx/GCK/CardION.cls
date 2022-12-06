/*------------------------------------------------------------------------
   File        : WebUser
   Purpose     : GCK CARDSKIPPER IDROTTONLINE GIMONÄS CK
   Syntax      : 
   Description : 
   Author(s)   : elpao
   Created     : Fri Oct 28 15:56:22 CEST 2016
   Notes       : 
 ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
USING Microsoft.Office.Interop.Excel.*.  

CLASS wx.GCK.CardION INHERITS Form   : 
   
   {wx\GCK\CardION.i}
   
   
   DEFINE PRIVATE VARIABLE excelExporter AS Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter NO-UNDO.
      
   DEFINE PUBLIC  VARIABLE cColName               AS CHARACTER                              NO-UNDO. /*aktuell column ex. e*/
   DEFINE PUBLIC  VARIABLE iColNum                AS INTEGER                                NO-UNDO. /*aktuell nummer på columnen ex e = 5*/
   DEFINE PRIVATE VARIABLE IdrottOnLineBS         AS Progress.Data.BindingSource            NO-UNDO.
   DEFINE PUBLIC  VARIABLE iSistaColnum           AS INTEGER                                NO-UNDO. /*den col som fiNns längs till höger*/
   DEFINE PRIVATE VARIABLE excelApp AS Application NO-UNDO.
   DEFINE PRIVATE VARIABLE components             AS System.ComponentModel.IContainer       NO-UNDO.
   DEFINE PUBLIC  VARIABLE DropedFile             AS CHARACTER                              NO-UNDO.
   DEFINE PRIVATE EVENT GuruAfterDropped  DELEGATE System.EventHandler.
   
   DEFINE PRIVATE VARIABLE IdrottOnLine         AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSpUser AS System.Windows.Forms.SplitContainer     NO-UNDO.
   DEFINE PUBLIC  VARIABLE iRadslut             AS INTEGER                                 NO-UNDO.
   DEFINE PUBLIC  VARIABLE iRad                 AS INTEGER                                 NO-UNDO.
   DEFINE         VARIABLE chExcelApplication   AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chWorkbook           AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chChart              AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chWorksheetRange     AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chCell               AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chPageBreak          AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE cActiveCell          AS COM-HANDLE                              NO-UNDO.
   DEFINE         VARIABLE chWindow             AS COM-HANDLE                              NO-UNDO.  
   DEFINE PUBLIC  VARIABLE chWorksheet          AS COM-HANDLE                              NO-UNDO.   
   DEFINE PRIVATE VARIABLE TillExcel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   CONSTRUCTOR PUBLIC CardION( ):
	
      SUPER().

      InitializeComponent ( ).
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.		

   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC CardION( ):

      IF VALID-OBJECT(components) THEN 
      DO:
         CAST(components, System.IDisposable):Dispose().
      END.

   END DESTRUCTOR.
	
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

	

   METHOD PUBLIC VOID CreateForm():
      CREATE BUFFER IdrottonLinebuff FOR TABLE "IdrottonLine".
	   THIS-OBJECT:excelExporter = NEW Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
   END METHOD.
		
   METHOD PUBLIC VOID CreateGridSparrU():
            
       
   END METHOD.  
   



   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID IdrottOnLine_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		
      
      DEFINE VARIABLE objFiles AS "System.String[]"                NO-UNDO.  
      DEFINE VARIABLE args     AS Controls.Subclasses.DragDropArgs NO-UNDO.          
                                                                                                                                                                                                                                   
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN 
      DO:
         objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").
         DropedFile = objFiles:GetValue(0):ToString().
      END. 
      THIS-OBJECT:GuruAfterDropped:Publish(THIS-OBJECT, CAST(args, System.EventArgs)).
      THIS-OBJECT:IdrottOnLine_AfterDropped(?,?).
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID IdrottOnLine_DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN 
      DO:
         e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN 
         DO:
            e:Effect = System.Windows.Forms.DragDropEffects:COPY.
         END.
         ELSE 
         DO:
            e:Effect = System.Windows.Forms.DragDropEffects:NONE.   
         END.
      
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID IdrottOnLine_DragOver( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		   
      RETURN.

   END METHOD.
	
   METHOD PUBLIC VOID IdrottOnLine_AfterDropped(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     MESSAGE "INLÄSNING STARTAR"
     VIEW-AS ALERT-BOX.
     
      IF DropedFile NE "" THEN  DO:
         
         THIS-OBJECT:ReadExcelStart(INPUT DropedFile). 
      END.  
   END METHOD.

   METHOD PUBLIC VOID ReadExcelStart(INPUT excelkommando AS CHARACTER):
      CREATE "Excel.Application" chExcelApplication.
      ASSIGN 
         chWorkbook = chExcelApplication:Workbooks:OPEN(excelkommando) NO-ERROR.
      ASSIGN 
         chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
      ASSIGN 
         iRadslut = chWorksheet:Cells:SpecialCells(11):ROW NO-ERROR.
      irad = 4.  
      DO WHILE iRad <= iRadslut:
         IF THIS-OBJECT:GetData("B") = "" OR THIS-OBJECT:GetData("B") = ? THEN iRad = iRadslut + 10.
         ELSE 
         DO:
            CREATE Cardskipper.
            Cardskipper.FOD = TRIM(THIS-OBJECT:GetData("B")).
            Cardskipper.FODNR = TRIM(THIS-OBJECT:GetData("C")).
            Cardskipper.K = TRIM(THIS-OBJECT:GetData("E")). 
            Cardskipper.FN = TRIM(THIS-OBJECT:GetData("F")). 
            Cardskipper.EN = TRIM(THIS-OBJECT:GetData("G")). 
            Cardskipper.KG = TRIM(THIS-OBJECT:GetData("H")). 
            Cardskipper.KPNR = TRIM(THIS-OBJECT:GetData("J")). 
            Cardskipper.KPO = TRIM(THIS-OBJECT:GetData("K")). 
            Cardskipper.TM = TRIM(THIS-OBJECT:GetData("S")).
            Cardskipper.EPK = TRIM(THIS-OBJECT:GetData("T")).
            Cardskipper.FOD = REPLACE(Cardskipper.FOD,"-",""). 
            THIS-OBJECT:Rowdown().
         END.
        
      END.  
      THIS-OBJECT:SlutReadExcel().
      FOR EACH Cardskipper WHERE NO-LOCK:
         CREATE IdrottonLine.
         BUFFER-COPY Cardskipper TO IdrottonLine.
         IdrottonLine.FOD = Cardskipper.FOD + "-" + Cardskipper.FODNR. 
      END.
      THIS-OBJECT:GuruFiltrera().
      
   END METHOD.
   METHOD PUBLIC VOID GuruFiltrera():
      DEFINE VARIABLE CustomQuery AS HANDLE    NO-UNDO.
      DEFINE VARIABLE ttSortQuery AS CHARACTER NO-UNDO.
      ttSortQuery = "FOR EACH IdrottonLine".
       
      
      CREATE QUERY CustomQuery.
      CustomQuery:SET-BUFFERS(IdrottonLinebuff).
      CustomQuery:QUERY-PREPARE(ttSortQuery) .
      CustomQuery:QUERY-OPEN () NO-ERROR.
      THIS-OBJECT:IdrottOnLineBS = NEW Progress.Data.BindingSource(CustomQuery, "*", "").            
      THIS-OBJECT:IdrottOnLine:DataSource = IdrottOnLineBS.
   /*  CustomQuerytqH:QUERY-OPEN () NO-ERROR.*/
  
   END.
   
   
   METHOD PUBLIC VOID Rowdown():
      iRad = iRad + 1.
     
   END METHOD.
   
   /*läs data direkt i angiven cell*/
   METHOD PUBLIC CHARACTER GetData(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      outpos = outpos + STRING(irad).
      invar = THIS-OBJECT:valueDataIn(outpos).
          
      RETURN invar.              
   END METHOD. 

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TillExcel_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:TillExcel().
		RETURN.

	END METHOD.

   METHOD PUBLIC CHARACTER valueDataIn(outpos AS CHARACTER):
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      invar = chWorkSheet:Range(outpos):TEXT.
      IF invar = "" THEN invar = chWorkSheet:Range(outpos):VALUE.
      RETURN invar.  
   END METHOD.
   METHOD PUBLIC VOID SlutReadExcel():
      NO-RETURN-VALUE chWorkbook:CLOSE() NO-ERROR.
      NO-RETURN-VALUE chExcelApplication:QUIT() NO-ERROR.
      SlutExcel(3).

   END METHOD.
   METHOD PUBLIC VOID SlutExcel(viewtyp AS INTEGER):  

      chExcelApplication:VISIBLE = TRUE NO-ERROR.
      chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
      chWorkSheet:SELECT NO-ERROR.      
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
         chWorkbook         = ?  
         chExcelApplication = ?
         chPageBreak        = ?
         chCell             = ?
         chWorksheet        = ?
         chChart            = ?
         chWorksheetRange   = ?
         cActiveCell        = ?.
      chWindow = ?.
    
   END METHOD.
   /* Alla poster till excel */
   METHOD PUBLIC VOID TillExcel():
      DEFINE VARIABLE iex     AS INTEGER                               NO-UNDO.
      DEFINE VARIABLE filnamn AS CHARACTER                             INITIAL "" NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER                             NO-UNDO.
      DEFINE VARIABLE wbi     AS Infragistics.Documents.Excel.Workbook NO-UNDO.
      DEFINE VARIABLE fil     AS System.IO.FileInfo.
      DEFINE VARIABLE rrr     AS System.Windows.Forms.DialogResult     NO-UNDO.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      /*
      path = "C:\delad\pro116\GuruAnders\wx\GCK\IdrottonlineMall.xlsx".
      */
      path = "C:\delad\pro116\GuruAnders\wx\GCK\MemberImportgck.xlsx".
      filnamn = "D:\gck\".
      filnamn = filnamn + "Idrottonline" + STRING(YEAR(TODAY)) +  ".xlsx". 
      OS-COPY VALUE(path) VALUE(filnamn).
      CREATE "Excel.Application" chExcelApplication.
      chExcelApplication:Visible = TRUE .
      chExcelApplication:ScreenUpdating = TRUE.
  
      ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(filnamn) NO-ERROR.
      chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
      irad = 2.
      
      OPEN QUERY poster FOR EACH IdrottonLine
      NO-LOCK.
      GET FIRST poster NO-LOCK.
      DO WHILE AVAILABLE(IdrottonLine):
         IF IdrottonLine.FN NE "" THEN DO:
            ASSIGN 
            chWorkSheet:Range("A" + STRING(irad)):Value = IdrottonLine.PR
            chWorkSheet:Range("B" + STRING(irad)):Value = IdrottonLine.FN
            chWorkSheet:Range("C" + STRING(irad)):VALUE = IdrottonLine.AFN
            chWorkSheet:Range("D" + STRING(irad)):VALUE = IdrottonLine.EN
            chWorkSheet:Range("E" + STRING(irad)):VALUE = IdrottonLine.K
            chWorkSheet:Range("F" + STRING(irad)):VALUE = IdrottonLine.NAT
            chWorkSheet:Range("G" + STRING(irad)):VALUE = IdrottonLine.IID
            chWorkSheet:Range("H" + STRING(irad)):VALUE = IdrottonLine.FOD
            chWorkSheet:Range("I" + STRING(irad)):VALUE = IdrottonLine.TM
            chWorkSheet:Range("J" + STRING(irad)):VALUE = IdrottonLine.EPK.
            
            /*
            chWorkSheet:Range("K" + STRING(irad)):VALUE = IdrottonLine.KAC
            chWorkSheet:Range("L" + STRING(irad)):VALUE = IdrottonLine.KG
            chWorkSheet:Range("M" + STRING(irad)):VALUE = IdrottonLine.KPNR
            chWorkSheet:Range("N" + STRING(irad)):VALUE = IdrottonLine.KPO
            chWorkSheet:Range("O" + STRING(irad)):VALUE = IdrottonLine.KL
            chWorkSheet:Range("P" + STRING(irad)):VALUE = IdrottonLine.AACO
            chWorkSheet:Range("Q" + STRING(irad)):VALUE = IdrottonLine.AGA
            chWorkSheet:Range("R" + STRING(irad)):VALUE = IdrottonLine.APNR
            chWorkSheet:Range("S" + STRING(irad)):VALUE = IdrottonLine.APO
            chWorkSheet:Range("T" + STRING(irad)):VALUE = IdrottonLine.ALA
            chWorkSheet:Range("U" + STRING(irad)):VALUE = IdrottonLine.TB
            chWorkSheet:Range("V" + STRING(irad)):VALUE = IdrottonLine.TA
            chWorkSheet:Range("W" + STRING(irad)):VALUE = IdrottonLine.EPP
            chWorkSheet:Range("X" + STRING(irad)):VALUE = IdrottonLine.EPA
            chWorkSheet:Range("Y" + STRING(irad)):VALUE = IdrottonLine.MNR
            chWorkSheet:Range("Z" + STRING(irad)):VALUE = IdrottonLine.MDT
            chWorkSheet:Range("AA" + STRING(irad)):VALUE = IdrottonLine.MDTOM
            chWorkSheet:Range("AB" + STRING(irad)):VALUE = IdrottonLine.INF
            chWorkSheet:Range("AC" + STRING(irad)):VALUE = IdrottonLine.FAM
            chWorkSheet:Range("AD" + STRING(irad)):VALUE = IdrottonLine.FAMD
            chWorkSheet:Range("AE" + STRING(irad)):VALUE = IdrottonLine.LGID
            chWorkSheet:Range("AF" + STRING(irad)):VALUE = IdrottonLine.TGID
            chWorkSheet:Range("AG" + STRING(irad)):VALUE = IdrottonLine.SKRP.
            */
            irad = irad + 1.
         END.
         GET NEXT poster NO-LOCK.
      END.   
      CLOSE QUERY poster.
      
      RELEASE OBJECT chExcelApplication NO-ERROR.      
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorksheet NO-ERROR.
      ASSIGN
      chExcelApplication = ?  
      chWorkbook = ?
      chWorksheet = ?.
      MESSAGE "Filen finns i " filnamn
      VIEW-AS ALERT-BOX. 
      /*
      
      filnamn = filnamn + STRING(iex) + "klipput.xlsx".
      wbi = THIS-OBJECT:excelExporter:Export(THIS-OBJECT:IdrottOnLine,Infragistics.Documents.Excel.WorkbookFormat:Excel2007).       
      wbi:Save(filnamn).
      THIS-OBJECT:excelApp = NEW Microsoft.Office.Interop.Excel.ApplicationClass().
      THIS-OBJECT:excelApp:Workbooks:Open(filnamn, System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value,System.Reflection.Missing:Value).
      THIS-OBJECT:excelApp:Visible = TRUE.
    
      CATCH e AS System.IO.IOException:
      
         MESSAGE e:MESSAGE 
            VIEW-AS ALERT-BOX.
         THIS-OBJECT:TillExcel().
      END CATCH.
      */
   END METHOD.     
      
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
	
   METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
      THIS-OBJECT:Close ( ). 
   END METHOD.

   METHOD PRIVATE VOID cancelButton_Click (sender AS System.Object, e AS System.EventArgs):
      THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel.
      THIS-OBJECT:Close ( ).
   END METHOD.
    
   METHOD PRIVATE VOID InitializeComponent ( ):

      /* NOTE: The following method is automatically generated.
		   
         We strongly suggest that the contents of this method only be modified using the 
         Visual Designer to avoid any incompatible modifications.
		   
         Modifying the contents of this method using a code editor will invalidate any support for this file. */
		
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("IdrottonLine", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AFN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EN").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("K").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("NAT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("IID").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FOD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EPK").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KAC").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KG").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KPNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KPO").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KL").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AACO").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AGA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("APNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("APO").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ALA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TB").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EPP").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EPA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn26 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn26 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MDT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn27 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn27 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MDTOM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn28 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn28 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("INF").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn29 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn29 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FAM").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn30 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn30 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FAMD").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn31 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn31 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LGID").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn32 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn32 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TGID").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn33 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn33 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SKRP").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
      appearance5 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("IdrottonLine").
      THIS-OBJECT:splitContainerSpUser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:IdrottOnLine = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
      THIS-OBJECT:IdrottOnLineBS = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:TillExcel = NEW Infragistics.Win.Misc.UltraButton().
      CAST(THIS-OBJECT:splitContainerSpUser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSpUser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSpUser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSpUser:SuspendLayout().
      CAST(THIS-OBJECT:IdrottOnLine, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:IdrottOnLineBS, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerSpUser */
      /*  */
      THIS-OBJECT:splitContainerSpUser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSpUser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSpUser:Name = "splitContainerSpUser".
      THIS-OBJECT:splitContainerSpUser:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerSpUser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSpUser:Panel1:Controls:Add(THIS-OBJECT:IdrottOnLine).
      /*  */
      /* splitContainerSpUser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSpUser:Panel2:Controls:Add(THIS-OBJECT:TillExcel).
      THIS-OBJECT:splitContainerSpUser:Size = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:splitContainerSpUser:SplitterDistance = 500.
      THIS-OBJECT:splitContainerSpUser:TabIndex = 2.
      /*  */
      /* IdrottOnLine */
      /*  */
      THIS-OBJECT:IdrottOnLine:AllowDrop = TRUE.
      THIS-OBJECT:IdrottOnLine:DataSource = THIS-OBJECT:IdrottOnLineBS.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Appearance = appearance1.
      ultraGridColumn1:Header:VisiblePosition = 0.
      ultraGridColumn2:Header:VisiblePosition = 1.
      ultraGridColumn3:Header:VisiblePosition = 2.
      ultraGridColumn4:Header:VisiblePosition = 3.
      ultraGridColumn5:Header:VisiblePosition = 4.
      ultraGridColumn6:Header:VisiblePosition = 5.
      ultraGridColumn7:Header:VisiblePosition = 6.
      ultraGridColumn8:Header:VisiblePosition = 7.
      ultraGridColumn9:Header:VisiblePosition = 8.
      ultraGridColumn10:Header:VisiblePosition = 9.
      ultraGridColumn11:Header:VisiblePosition = 10.
      ultraGridColumn12:Header:VisiblePosition = 11.
      ultraGridColumn13:Header:VisiblePosition = 12.
      ultraGridColumn14:Header:VisiblePosition = 13.
      ultraGridColumn15:Header:VisiblePosition = 14.
      ultraGridColumn16:Header:VisiblePosition = 15.
      ultraGridColumn17:Header:VisiblePosition = 16.
      ultraGridColumn18:Header:VisiblePosition = 17.
      ultraGridColumn19:Header:VisiblePosition = 18.
      ultraGridColumn20:Header:VisiblePosition = 19.
      ultraGridColumn21:Header:VisiblePosition = 20.
      ultraGridColumn22:Header:VisiblePosition = 21.
      ultraGridColumn23:Header:VisiblePosition = 22.
      ultraGridColumn24:Header:VisiblePosition = 23.
      ultraGridColumn25:Header:VisiblePosition = 24.
      ultraGridColumn26:Header:VisiblePosition = 25.
      ultraGridColumn27:Header:VisiblePosition = 26.
      ultraGridColumn28:Header:VisiblePosition = 27.
      ultraGridColumn29:Header:VisiblePosition = 28.
      ultraGridColumn30:Header:VisiblePosition = 29.
      ultraGridColumn31:Header:VisiblePosition = 30.
      ultraGridColumn32:Header:VisiblePosition = 31.
      ultraGridColumn33:Header:VisiblePosition = 32.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 33 NO-UNDO.
      arrayvar0[1] = ultraGridColumn1.
      arrayvar0[2] = ultraGridColumn2.
      arrayvar0[3] = ultraGridColumn3.
      arrayvar0[4] = ultraGridColumn4.
      arrayvar0[5] = ultraGridColumn5.
      arrayvar0[6] = ultraGridColumn6.
      arrayvar0[7] = ultraGridColumn7.
      arrayvar0[8] = ultraGridColumn8.
      arrayvar0[9] = ultraGridColumn9.
      arrayvar0[10] = ultraGridColumn10.
      arrayvar0[11] = ultraGridColumn11.
      arrayvar0[12] = ultraGridColumn12.
      arrayvar0[13] = ultraGridColumn13.
      arrayvar0[14] = ultraGridColumn14.
      arrayvar0[15] = ultraGridColumn15.
      arrayvar0[16] = ultraGridColumn16.
      arrayvar0[17] = ultraGridColumn17.
      arrayvar0[18] = ultraGridColumn18.
      arrayvar0[19] = ultraGridColumn19.
      arrayvar0[20] = ultraGridColumn20.
      arrayvar0[21] = ultraGridColumn21.
      arrayvar0[22] = ultraGridColumn22.
      arrayvar0[23] = ultraGridColumn23.
      arrayvar0[24] = ultraGridColumn24.
      arrayvar0[25] = ultraGridColumn25.
      arrayvar0[26] = ultraGridColumn26.
      arrayvar0[27] = ultraGridColumn27.
      arrayvar0[28] = ultraGridColumn28.
      arrayvar0[29] = ultraGridColumn29.
      arrayvar0[30] = ultraGridColumn30.
      arrayvar0[31] = ultraGridColumn31.
      arrayvar0[32] = ultraGridColumn32.
      arrayvar0[33] = ultraGridColumn33.
      ultraGridBand1:Columns:AddRange(arrayvar0).
      THIS-OBJECT:IdrottOnLine:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:IdrottOnLine:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:MaxRowScrollRegions = 1.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:IdrottOnLine:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:IdrottOnLine:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:IdrottOnLine:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:IdrottOnLine:Name = "IdrottOnLine".
      THIS-OBJECT:IdrottOnLine:Size = NEW System.Drawing.Size(1194, 500).
      THIS-OBJECT:IdrottOnLine:TabIndex = 0.
      THIS-OBJECT:IdrottOnLine:Text = "ultraGrid1".
      THIS-OBJECT:IdrottOnLine:DragDrop:Subscribe(THIS-OBJECT:IdrottOnLine_DragDrop).
      THIS-OBJECT:IdrottOnLine:DragEnter:Subscribe(THIS-OBJECT:IdrottOnLine_DragEnter).
      THIS-OBJECT:IdrottOnLine:DragOver:Subscribe(THIS-OBJECT:IdrottOnLine_DragOver).
      /*  */
      /* IdrottOnLineBS */
      /*  */
      THIS-OBJECT:IdrottOnLineBS:MaxDataGuess = 0.
      THIS-OBJECT:IdrottOnLineBS:NoLOBs = FALSE.
      THIS-OBJECT:IdrottOnLineBS:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc1:ChildTables = arrayvar1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 33 NO-UNDO.
      arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("PR", "Prova-på", Progress.Data.DataType:CHARACTER).
      arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("FN", "Förnamn", Progress.Data.DataType:CHARACTER).
      arrayvar2[3] = NEW Progress.Data.ColumnPropDesc("AFN", "Alt. förnamn", Progress.Data.DataType:CHARACTER).
      arrayvar2[4] = NEW Progress.Data.ColumnPropDesc("EN", "Efternamn", Progress.Data.DataType:CHARACTER).
      arrayvar2[5] = NEW Progress.Data.ColumnPropDesc("K", "Kön", Progress.Data.DataType:CHARACTER).
      arrayvar2[6] = NEW Progress.Data.ColumnPropDesc("NAT", "Nationalitet", Progress.Data.DataType:CHARACTER).
      arrayvar2[7] = NEW Progress.Data.ColumnPropDesc("IID", "IdrottsID", Progress.Data.DataType:CHARACTER).
      arrayvar2[8] = NEW Progress.Data.ColumnPropDesc("FOD", "Födelsedat./Personnr. (ååååmmdd-xxxx)", Progress.Data.DataType:INTEGER).
      arrayvar2[9] = NEW Progress.Data.ColumnPropDesc("TM", "Telefon mobil", Progress.Data.DataType:CHARACTER).
      arrayvar2[10] = NEW Progress.Data.ColumnPropDesc("EPK", "E-post kontakt", Progress.Data.DataType:CHARACTER).
      arrayvar2[11] = NEW Progress.Data.ColumnPropDesc("KAC", "Kontaktadress - c/o adress", Progress.Data.DataType:CHARACTER).
      arrayvar2[12] = NEW Progress.Data.ColumnPropDesc("KG", "Kontaktadress - Gatuadress", Progress.Data.DataType:CHARACTER).
      arrayvar2[13] = NEW Progress.Data.ColumnPropDesc("KPNR", "Kontaktadress - Postnummer", Progress.Data.DataType:CHARACTER).
      arrayvar2[14] = NEW Progress.Data.ColumnPropDesc("KPO", "Kontaktadress - Postort", Progress.Data.DataType:CHARACTER).
      arrayvar2[15] = NEW Progress.Data.ColumnPropDesc("KL", "Kontaktadress - Land", Progress.Data.DataType:CHARACTER).
      arrayvar2[16] = NEW Progress.Data.ColumnPropDesc("AACO", "Arbetsadress - c/o", Progress.Data.DataType:CHARACTER).
      arrayvar2[17] = NEW Progress.Data.ColumnPropDesc("AGA", "Arbetsadress - Gatuadress", Progress.Data.DataType:CHARACTER).
      arrayvar2[18] = NEW Progress.Data.ColumnPropDesc("APNR", "Arbetsadress - Postnummer", Progress.Data.DataType:INTEGER).
      arrayvar2[19] = NEW Progress.Data.ColumnPropDesc("APO", "Arbetsadress - Postort", Progress.Data.DataType:CHARACTER).
      arrayvar2[20] = NEW Progress.Data.ColumnPropDesc("ALA", "Arbetsadress - Land", Progress.Data.DataType:CHARACTER).
      arrayvar2[21] = NEW Progress.Data.ColumnPropDesc("TB", "Telefon bostad", Progress.Data.DataType:CHARACTER).
      arrayvar2[22] = NEW Progress.Data.ColumnPropDesc("TA", "Telefon arbete", Progress.Data.DataType:CHARACTER).
      arrayvar2[23] = NEW Progress.Data.ColumnPropDesc("EPP", "E-post privat", Progress.Data.DataType:CHARACTER).
      arrayvar2[24] = NEW Progress.Data.ColumnPropDesc("EPA", "E-post arbete", Progress.Data.DataType:CHARACTER).
      arrayvar2[25] = NEW Progress.Data.ColumnPropDesc("MNR", "Medlemsnr.", Progress.Data.DataType:CHARACTER).
      arrayvar2[26] = NEW Progress.Data.ColumnPropDesc("MDT", "Medlem sedan", Progress.Data.DataType:DATE).
      arrayvar2[27] = NEW Progress.Data.ColumnPropDesc("MDTOM", "Medlem t.o.m.", Progress.Data.DataType:DATE).
      arrayvar2[28] = NEW Progress.Data.ColumnPropDesc("INF", "Info", Progress.Data.DataType:CHARACTER).
      arrayvar2[29] = NEW Progress.Data.ColumnPropDesc("FAM", "Familj", Progress.Data.DataType:CHARACTER).
      arrayvar2[30] = NEW Progress.Data.ColumnPropDesc("FAMD", "Fam.Admin", Progress.Data.DataType:CHARACTER).
      arrayvar2[31] = NEW Progress.Data.ColumnPropDesc("LGID", "Lägg till GruppID", Progress.Data.DataType:CHARACTER).
      arrayvar2[32] = NEW Progress.Data.ColumnPropDesc("TGID", "Ta bort GruppID", Progress.Data.DataType:CHARACTER).
      arrayvar2[33] = NEW Progress.Data.ColumnPropDesc("SKRP", "Skridskonätet pin", Progress.Data.DataType:CHARACTER).
      tableDesc1:Columns = arrayvar2.
      THIS-OBJECT:IdrottOnLineBS:TableSchema = tableDesc1.
      /*  */
      /* TillExcel */
      /*  */
      THIS-OBJECT:TillExcel:Location = NEW System.Drawing.Point(430, 3).
      THIS-OBJECT:TillExcel:Name = "TillExcel".
      THIS-OBJECT:TillExcel:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:TillExcel:TabIndex = 0.
      THIS-OBJECT:TillExcel:Text = "Till Excel".
      THIS-OBJECT:TillExcel:Click:Subscribe(THIS-OBJECT:TillExcel_Click).
      /*  */
      /* CardION */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerSpUser).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "CardION".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "Från CardSkipper till Idrottonline".
      THIS-OBJECT:splitContainerSpUser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSpUser:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSpUser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSpUser:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:IdrottOnLine, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:IdrottOnLineBS, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD. 
 
   METHOD PUBLIC VOID ShowModalDialog(  ):
		
      WAIT-FOR THIS-OBJECT:ShowDialog().

   END METHOD.


 
END CLASS.
