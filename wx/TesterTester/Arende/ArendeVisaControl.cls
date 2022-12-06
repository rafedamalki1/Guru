 
 /*------------------------------------------------------------------------
    File        : KalkVisaControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 19:28:38 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Arende.ArendeVisaControl INHERITS UserControl: 
	
	{ARENDEKATH.i}
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Arende.ArendeShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE vantapakalkyl AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripVisa AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonSkrivUt AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonGenerera AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonExporteraExcel AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVisa AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaPanelWaiting AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaPanelEmpty AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.

		
	CONSTRUCTOR PUBLIC ArendeVisaControl ( INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      {ARENDEKATDBH.i Root:DatabaseManager:Arende}
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeVisaControl").
      THIS-OBJECT:splitContainerVisa = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripVisa = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonGenerera = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonExporteraExcel = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonSkrivUt = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:VisaPanelEmpty = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:VisaPanelWaiting = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:vantapakalkyl = NEW System.Windows.Forms.Label().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      CAST(THIS-OBJECT:splitContainerVisa, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisa:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisa:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVisa:SuspendLayout().
      THIS-OBJECT:toolStripVisa:SuspendLayout().
      THIS-OBJECT:VisaPanelWaiting:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerVisa */
      /*  */
      THIS-OBJECT:splitContainerVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisa:Name = "splitContainerVisa".
      THIS-OBJECT:splitContainerVisa:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerVisa.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisa:Panel1:Controls:Add(THIS-OBJECT:toolStripVisa).
      /*  */
      /* splitContainerVisa.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisa:Panel2:Controls:Add(THIS-OBJECT:VisaPanelEmpty).
      THIS-OBJECT:splitContainerVisa:Panel2:Controls:Add(THIS-OBJECT:VisaPanelWaiting).
      THIS-OBJECT:splitContainerVisa:Panel2:Controls:Add(THIS-OBJECT:VisaExcel).
      THIS-OBJECT:splitContainerVisa:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVisa:SplitterDistance = 25.
      THIS-OBJECT:splitContainerVisa:TabIndex = 4.
      /*  */
      /* toolStripVisa */
      /*  */
      THIS-OBJECT:toolStripVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripButtonGenerera.
      arrayvar0[2] = THIS-OBJECT:toolStripButtonExporteraExcel.
      arrayvar0[3] = THIS-OBJECT:toolStripButtonSkrivUt.
      THIS-OBJECT:toolStripVisa:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripVisa:Name = "toolStripVisa".
      THIS-OBJECT:toolStripVisa:Size = NEW System.Drawing.Size(1111, 25).
      THIS-OBJECT:toolStripVisa:TabIndex = 0.
      THIS-OBJECT:toolStripVisa:Text = "toolStrip1".
      /*  */
      /* toolStripButtonGenerera */
      /*  */
      THIS-OBJECT:toolStripButtonGenerera:Image = CAST(resources:GetObject("toolStripButtonGenerera.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonGenerera:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonGenerera:Name = "toolStripButtonGenerera".
      THIS-OBJECT:toolStripButtonGenerera:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:toolStripButtonGenerera:Text = "Generera kalkyl".
      THIS-OBJECT:toolStripButtonGenerera:Click:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      /*  */
      /* toolStripButtonExporteraExcel */
      /*  */
      THIS-OBJECT:toolStripButtonExporteraExcel:Image = CAST(resources:GetObject("toolStripButtonExporteraExcel.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonExporteraExcel:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonExporteraExcel:Name = "toolStripButtonExporteraExcel".
      THIS-OBJECT:toolStripButtonExporteraExcel:Size = NEW System.Drawing.Size(95, 22).
      THIS-OBJECT:toolStripButtonExporteraExcel:Text = "Spara excelfil".
      THIS-OBJECT:toolStripButtonExporteraExcel:Click:Subscribe(THIS-OBJECT:SparaExcelE).
      /*  */
      /* toolStripButtonSkrivUt */
      /*  */
      THIS-OBJECT:toolStripButtonSkrivUt:Image = CAST(resources:GetObject("toolStripButtonSkrivUt.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonSkrivUt:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonSkrivUt:Name = "toolStripButtonSkrivUt".
      THIS-OBJECT:toolStripButtonSkrivUt:Size = NEW System.Drawing.Size(66, 22).
      THIS-OBJECT:toolStripButtonSkrivUt:Text = "Skriv ut".
      THIS-OBJECT:toolStripButtonSkrivUt:Click:Subscribe(THIS-OBJECT:SkrivUtE).
      /*  */
      /* VisaPanelEmpty */
      /*  */
      THIS-OBJECT:VisaPanelEmpty:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaPanelEmpty:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPanelEmpty:Name = "VisaPanelEmpty".
      THIS-OBJECT:VisaPanelEmpty:Size = NEW System.Drawing.Size(1111, 517).
      THIS-OBJECT:VisaPanelEmpty:TabIndex = 2.
      /*  */
      /* VisaPanelWaiting */
      /*  */
      THIS-OBJECT:VisaPanelWaiting:BackColor = System.Drawing.SystemColors:Info.
      THIS-OBJECT:VisaPanelWaiting:Controls:Add(THIS-OBJECT:vantapakalkyl).
      THIS-OBJECT:VisaPanelWaiting:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPanelWaiting:Name = "VisaPanelWaiting".
      THIS-OBJECT:VisaPanelWaiting:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:VisaPanelWaiting:TabIndex = 1.
      /*  */
      /* vantapakalkyl */
      /*  */
      THIS-OBJECT:vantapakalkyl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:vantapakalkyl:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Italic, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:vantapakalkyl:Image = CAST(resources:GetObject("vantapakalkyl.Image"), System.Drawing.Image).
      THIS-OBJECT:vantapakalkyl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:vantapakalkyl:Name = "vantapakalkyl".
      THIS-OBJECT:vantapakalkyl:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:vantapakalkyl:TabIndex = 0.
      THIS-OBJECT:vantapakalkyl:Text = "Genererar kalkyl, var god vänta..".
      THIS-OBJECT:vantapakalkyl:TextAlign = System.Drawing.ContentAlignment:MiddleCenter.
      THIS-OBJECT:vantapakalkyl:UseCompatibleTextRendering = TRUE.
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(358, 44).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(522, 330).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      /*  */
      /* KalkVisaControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerVisa).
      THIS-OBJECT:Name = "KalkVisaControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVisa:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisa:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerVisa:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisa, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:PerformLayout().
      THIS-OBJECT:VisaPanelWaiting:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO. 
      
      IF THIS-OBJECT:VisaExcel:Visible EQ TRUE THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN DO:
            THIS-OBJECT:ControlShell:VisningKalkIni(OUTPUT path).
            THIS-OBJECT:ControlShell:GenereraKalkylVisning().   
         END.
      END.
      ELSE DO:
         THIS-OBJECT:ControlShell:VisningKalkIni(OUTPUT path).
         THIS-OBJECT:ControlShell:GenereraKalkylVisning().
      END.
   END METHOD.
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID SparaExcelE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:SaveFileWithDialog().
   END METHOD.
	DESTRUCTOR PUBLIC ArendeVisaControl ( ):

	END DESTRUCTOR.

END CLASS.