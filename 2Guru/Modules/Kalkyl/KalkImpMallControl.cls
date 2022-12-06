 
 /*------------------------------------------------------------------------
    File        : KalkImpMallControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 16:48:43 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkImpMallControl INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
   DEFINE PUBLIC VARIABLE ButtonOverMarkMallar AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonMallarStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonExcelStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBackMarkMallar AS Controls.ButtonOver NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerImport AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerImpVal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerImpMallar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerImpKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerImpBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE MallarlStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridValdaMallar AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridMallarKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridMallar AS Controls.Grid NO-UNDO.
   DEFINE         VARIABLE DGMall                       AS Controls.Subclasses.DragDropGroup                              NO-UNDO.
	DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.	
	CONSTRUCTOR PUBLIC KalkImpMallControl ( INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:CreateForm(). 
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridMallar().
      THIS-OBJECT:CreateGridMallarKoder().
      THIS-OBJECT:CreateGridValdaMallar().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridFunctions():
      
      /* Drag dropp*/
      
      /*mallar*/
      THIS-OBJECT:DGMall = NEW Controls.Subclasses.DragDropGroup("Mallar").
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridMallar).
      THIS-OBJECT:DGMall:AddChild(THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:ButtonOverMarkMallar:GuruKoppla(THIS-OBJECT:GridMallar,THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:ButtonOverMarkMallar:GuruDublett = TRUE.
      THIS-OBJECT:ButtonBackMarkMallar:GuruKoppla(THIS-OBJECT:GridValdaMallar, THIS-OBJECT:GridMallar).
      THIS-OBJECT:ButtonBackMarkMallar:GuruRemoveFrom = TRUE.
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridMallarKoder():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45, "KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,45,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      THIS-OBJECT:GridMallarKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:KalkdbControl:KalkmallKodertth)).
      /*default filter  sortering*/     
      THIS-OBJECT:GridMallarKoder:GuruOrderby("BY ARBKOD BY LOPNR").
      THIS-OBJECT:GridMallarKoder:Text = "Koder för mallen".
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:GridMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkdbControl:KalkmallHuvudtth)).  
      THIS-OBJECT:GridMallar:Text = "Mallar".
      THIS-OBJECT:GridMallar:tBS:PositionChanged:Subscribe(ValueMallarCh).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridValdaMallar():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("Typ","Typ",TRUE,45).
      THIS-OBJECT:GridValdaMallar:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkdbControl:KalkmallValdtth)).
      THIS-OBJECT:GridValdaMallar:Text = "Valda mallar".
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridValdaMallar_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID MallarlStrip_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
	   DEFINE VARIABLE objFiles AS "System.String[]" NO-UNDO.  
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.    
      
      DEFINE VARIABLE theStream AS System.IO.MemoryStream NO-UNDO.
      DEFINE VARIABLE fileGroupDescriptor AS "System.Byte[]" NO-UNDO. 
      DEFINE VARIABLE filNamn AS System.Text.StringBuilder NO-UNDO.
      DEFINE VARIABLE intbyte AS INTEGER NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE langd AS INTEGER NO-UNDO.
      
      /* GENERELL DRAG AND DROP KOD*/                                                                                                                                                                                                                                        
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
         objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").
         cFile = objFiles:GetValue(0):ToString().
      END. 
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         i = 0.
         theStream = CAST(e:Data:GetData("FileGroupDescriptor"),"System.IO.MemoryStream").
         fileGroupDescriptor = NEW "System.Byte[]" (512).   
         langd = theStream:Length - 1.

         DO WHILE i < langd:
             intbyte = theStream:ReadByte().
             fileGroupDescriptor:SetValue(intbyte AS UNSIGNED-BYTE, i).
             i = i + 1.
         END.                 

         i = 76.
         filNamn = NEW System.Text.StringBuilder().

         DO WHILE i < langd:
            filNamn:Append(System.Convert:ToChar(fileGroupDescriptor:GetValue(i))).
            i = i + 1.
         END. 
         cFile = System.IO.Path:GetTempPath() + filNamn:ToString().
      END.

      /* INLÄSNINGSKOD*/
      IF cFile NE "" THEN DO:
         IF CAPS(SUBSTRING(cFile, LENGTH(cFile) - 3)) = ".XLS" OR CAPS(SUBSTRING(cFile, LENGTH(cFile) - 4)) = ".XLSX" THEN DO: 
            res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(50) + "      " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(51), THIS-OBJECT:Root:LanguageManager:GetString(52), System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).
            
            THIS-OBJECT:KalkdbControl:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
            IF res:ToString() = "Yes" THEN THIS-OBJECT:ControlShell:ExcelImportLista(1,cFile).
            IF res:ToString() = "No" THEN THIS-OBJECT:ControlShell:ExcelImportLista(2,cFile).
            IF res:ToString() = "Cancel" THEN RETURN.
            THIS-OBJECT:ControlShell:FelMeddCheck().
            res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(212) + " ", "Kalkyl", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         END.
      END.
                                                                                                                                                                                                                                                  
      RETURN.
      

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID MallarlStrip_DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		/*Drag and drop*/
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
        e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE DO:
         e:Effect = System.Windows.Forms.DragDropEffects:NONE.   
      END.
		RETURN.

	END METHOD.

   METHOD PRIVATE VOID ValueMallarCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:KalkdbControl:KalkmallHuvudtth:AVAILABLE THEN  DO:
         THIS-OBJECT:GridMallarKoder:GuruFiltrera("KalkmallKodertt.MALLNR = " + STRING(THIS-OBJECT:KalkdbControl:KalkmallHuvudtth:BUFFER-FIELD("MALLNR"):BUFFER-VALUE)).
         THIS-OBJECT:GridMallarKoder:GuruFirstrow().
      END.
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridMallar_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		THIS-OBJECT:ButtonOverMarkMallar_Click(?,?).
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkImpMallControl").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
      appearance24 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
      appearance34 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
      appearance35 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
      appearance36 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
      appearance37 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
      appearance38 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerImport = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpMallar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImpVal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMallar = NEW Controls.Grid().
      THIS-OBJECT:ButtonBackMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonOverMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:GridMallarKoder = NEW Controls.Grid().
      THIS-OBJECT:GridValdaMallar = NEW Controls.Grid().
      THIS-OBJECT:splitContainerImpBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:MallarlStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonMallarStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonExcelStrip = NEW System.Windows.Forms.ToolStripButton().
      CAST(THIS-OBJECT:splitContainerImport, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImport:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImport:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImport:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpMallar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpMallar:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpKoder:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImpVal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpVal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpVal:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImpVal:SuspendLayout().
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerImpBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImpBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImpBild:SuspendLayout().
      THIS-OBJECT:MallarlStrip:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerImport */
      /*  */
      THIS-OBJECT:splitContainerImport:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:splitContainerImport:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImport:Name = "splitContainerImport".
      /*  */
      /* splitContainerImport.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImport:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpMallar).
      /*  */
      /* splitContainerImport.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImport:Panel2:Controls:Add(THIS-OBJECT:splitContainerImpBild).
      THIS-OBJECT:splitContainerImport:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerImport:SplitterDistance = 679.
      THIS-OBJECT:splitContainerImport:TabIndex = 18.
      /*  */
      /* splitContainerImpMallar */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpMallar:Name = "splitContainerImpMallar".
      /*  */
      /* splitContainerImpMallar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpKoder).
      /*  */
      /* splitContainerImpMallar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpMallar:Panel2:Controls:Add(THIS-OBJECT:GridValdaMallar).
      THIS-OBJECT:splitContainerImpMallar:Size = NEW System.Drawing.Size(679, 546).
      THIS-OBJECT:splitContainerImpMallar:SplitterDistance = 291.
      THIS-OBJECT:splitContainerImpMallar:TabIndex = 0.
      /*  */
      /* splitContainerImpKoder */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpKoder:Name = "splitContainerImpKoder".
      THIS-OBJECT:splitContainerImpKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerImpKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerImpVal).
      /*  */
      /* splitContainerImpKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpKoder:Panel2:Controls:Add(THIS-OBJECT:GridMallarKoder).
      THIS-OBJECT:splitContainerImpKoder:Size = NEW System.Drawing.Size(291, 546).
      THIS-OBJECT:splitContainerImpKoder:SplitterDistance = 267.
      THIS-OBJECT:splitContainerImpKoder:TabIndex = 0.
      /*  */
      /* splitContainerImpVal */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpVal:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerImpVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpVal:Name = "splitContainerImpVal".
      /*  */
      /* splitContainerImpVal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Panel1:Controls:Add(THIS-OBJECT:GridMallar).
      /*  */
      /* splitContainerImpVal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpVal:Panel2:Controls:Add(THIS-OBJECT:ButtonBackMarkMallar).
      THIS-OBJECT:splitContainerImpVal:Panel2:Controls:Add(THIS-OBJECT:ButtonOverMarkMallar).
      THIS-OBJECT:splitContainerImpVal:Size = NEW System.Drawing.Size(291, 267).
      THIS-OBJECT:splitContainerImpVal:SplitterDistance = 251.
      THIS-OBJECT:splitContainerImpVal:TabIndex = 0.
      /*  */
      /* GridMallar */
      /*  */
      THIS-OBJECT:GridMallar:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallar:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallar:GuruContext = ?.
      THIS-OBJECT:GridMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallar:Name = "GridMallar".
      THIS-OBJECT:GridMallar:Size = NEW System.Drawing.Size(251, 267).
      THIS-OBJECT:GridMallar:TabIndex = 0.
      THIS-OBJECT:GridMallar:Text = "grid1".
      THIS-OBJECT:GridMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMallar_GuruAfterDropped).
      THIS-OBJECT:GridMallar:DoubleClickRow:Subscribe(THIS-OBJECT:GridMallar_DoubleClickRow).
      /*  */
      /* ButtonBackMarkMallar */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:ButtonBackMarkMallar:Appearance = appearance13.
      THIS-OBJECT:ButtonBackMarkMallar:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:ButtonBackMarkMallar:Name = "ButtonBackMarkMallar".
      THIS-OBJECT:ButtonBackMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBackMarkMallar:TabIndex = 13.
      THIS-OBJECT:ButtonBackMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonBackMarkMallar_Click).
      /*  */
      /* ButtonOverMarkMallar */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:ButtonOverMarkMallar:Appearance = appearance14.
      THIS-OBJECT:ButtonOverMarkMallar:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:ButtonOverMarkMallar:Name = "ButtonOverMarkMallar".
      THIS-OBJECT:ButtonOverMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonOverMarkMallar:TabIndex = 11.
      THIS-OBJECT:ButtonOverMarkMallar:Click:Subscribe(THIS-OBJECT:ButtonOverMarkMallar_Click).
      /*  */
      /* GridMallarKoder */
      /*  */
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMallarKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMallarKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMallarKoder:GuruContext = ?.
      THIS-OBJECT:GridMallarKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMallarKoder:Name = "GridMallarKoder".
      THIS-OBJECT:GridMallarKoder:Size = NEW System.Drawing.Size(291, 275).
      THIS-OBJECT:GridMallarKoder:TabIndex = 2.
      THIS-OBJECT:GridMallarKoder:Text = "grid1".
      /*  */
      /* GridValdaMallar */
      /*  */
      THIS-OBJECT:GridValdaMallar:AllowDrop = TRUE.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaMallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaMallar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaMallar:GuruContext = ?.
      THIS-OBJECT:GridValdaMallar:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaMallar:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaMallar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaMallar:Name = "GridValdaMallar".
      THIS-OBJECT:GridValdaMallar:Size = NEW System.Drawing.Size(384, 546).
      THIS-OBJECT:GridValdaMallar:TabIndex = 1.
      THIS-OBJECT:GridValdaMallar:Text = "grid1".
      THIS-OBJECT:GridValdaMallar:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaMallar_GuruAfterDropped).
      THIS-OBJECT:GridValdaMallar:InitializeLayout:Subscribe(THIS-OBJECT:GridValdaMallar_InitializeLayout).
      /*  */
      /* splitContainerImpBild */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImpBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImpBild:Name = "splitContainerImpBild".
      /*  */
      /* splitContainerImpBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Panel1:Controls:Add(THIS-OBJECT:MallarlStrip).
      /*  */
      /* splitContainerImpBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerImpBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerImpBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerImpBild:Size = NEW System.Drawing.Size(428, 546).
      THIS-OBJECT:splitContainerImpBild:SplitterDistance = 200.
      THIS-OBJECT:splitContainerImpBild:TabIndex = 0.
      /*  */
      /* MallarlStrip */
      /*  */
      THIS-OBJECT:MallarlStrip:AllowDrop = TRUE.
      THIS-OBJECT:MallarlStrip:ImageScalingSize = NEW System.Drawing.Size(32, 32).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonMallarStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonExcelStrip.
      THIS-OBJECT:MallarlStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:MallarlStrip:LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle:Table.
      THIS-OBJECT:MallarlStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:MallarlStrip:Name = "MallarlStrip".
      THIS-OBJECT:MallarlStrip:Size = NEW System.Drawing.Size(200, 78).
      THIS-OBJECT:MallarlStrip:TabIndex = 17.
      THIS-OBJECT:MallarlStrip:Text = "toolStrip1".
      THIS-OBJECT:MallarlStrip:DragDrop:Subscribe(THIS-OBJECT:MallarlStrip_DragDrop).
      THIS-OBJECT:MallarlStrip:DragEnter:Subscribe(THIS-OBJECT:MallarlStrip_DragEnter).
      /*  */
      /* ButtonMallarStrip */
      /*  */
      THIS-OBJECT:ButtonMallarStrip:Image = CAST(resources:GetObject("ButtonMallarStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonMallarStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonMallarStrip:Name = "ButtonMallarStrip".
      THIS-OBJECT:ButtonMallarStrip:Size = NEW System.Drawing.Size(116, 36).
      THIS-OBJECT:ButtonMallarStrip:Text = "Koppla mallar".
      THIS-OBJECT:ButtonMallarStrip:Click:Subscribe(THIS-OBJECT:ButtonMallarStrip_Click).
      /*  */
      /* ButtonExcelStrip */
      /*  */
      THIS-OBJECT:ButtonExcelStrip:Image = CAST(resources:GetObject("ButtonExcelStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonExcelStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonExcelStrip:Name = "ButtonExcelStrip".
      THIS-OBJECT:ButtonExcelStrip:Size = NEW System.Drawing.Size(109, 36).
      THIS-OBJECT:ButtonExcelStrip:Text = "Import Excel".
      THIS-OBJECT:ButtonExcelStrip:ToolTipText = "Alt1: arbetskod i kolumn A, löpnr i B och antal i C. Alt2: arbetskod och löpnr i kolumn A och antal i C. ".
      THIS-OBJECT:ButtonExcelStrip:Click:Subscribe(THIS-OBJECT:ButtonExcelStrip_Click).
      /*  */
      /* KalkImpMallControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerImport).
      THIS-OBJECT:Name = "KalkImpMallControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerImport:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImport:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImport, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImport:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpMallar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpMallar:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpKoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpVal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpVal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImpVal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpVal:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMallar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridMallarKoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridValdaMallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImpBild:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerImpBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImpBild:ResumeLayout(FALSE).
      THIS-OBJECT:MallarlStrip:ResumeLayout(FALSE).
      THIS-OBJECT:MallarlStrip:PerformLayout().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   
   /*------------------------------------------------------------------------------
      Purpose:                                                      
      Notes:                                                     
------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaMallar THEN 
      DO:
         THIS-OBJECT:ButtonBackMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBackMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridValdaMallar:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ButtonBackMarkMallar:GuruOver().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetString(75)).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonOverMarkMallar_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ButtonOverMarkMallar:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:ButtonOverMarkMallar:GuruOver().
      END.
      ELSE rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), THIS-OBJECT:Root:LanguageManager:GetString(74)).
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaMallar_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMallar THEN DO:
         THIS-OBJECT:ButtonOverMarkMallar_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonExcelStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(50) + "      " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(51), THIS-OBJECT:Root:LanguageManager:GetString(52), System.Windows.Forms.MessageBoxButtons:YesNoCancel, System.Windows.Forms.MessageBoxIcon:Question).
      
      THIS-OBJECT:KalkdbControl:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      IF res:ToString() = "Yes" THEN THIS-OBJECT:ControlShell:ExcelImportLista(1,fildir).
      IF res:ToString() = "No" THEN THIS-OBJECT:ControlShell:ExcelImportLista(2,fildir).
      IF res:ToString() = "Cancel" THEN RETURN.
      THIS-OBJECT:ControlShell:FelMeddCheck().
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(212) + " ", "Kalkyl", System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonMallarStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:KalkdbControl:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ControlShell:ImportMallar().
      THIS-OBJECT:ControlShell:FelMeddCheck().
      RETURN.

   END METHOD.
   
	DESTRUCTOR PUBLIC KalkImpMallControl ( ):
     /* {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.