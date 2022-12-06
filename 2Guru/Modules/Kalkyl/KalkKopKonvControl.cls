 
 /*------------------------------------------------------------------------
    File        : KalkKopKonvControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 18:28:51 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkKopKonvControl INHERITS UserControl: 
	
   DEFINE PUBLIC VARIABLE ButtonFlyttaKalkylStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE HuvudBenamning AS Controls.FillinChar NO-UNDO.
	DEFINE PUBLIC VARIABLE KalkHuvudControl                 AS Modules.Kalkyl.KalkHuvudControl                                   NO-UNDO.
	DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
   DEFINE PUBLIC VARIABLE ButtonKopieraKalkylStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonKonvKalkylStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonImportXml AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonExportXml AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKopiera AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKopKonvSplit2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKopKonvSplit AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKopKonv AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE KopiKalkStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE KopLabelOmr AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE KopLabelKatalog AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE KopHuvudOmrade AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE KopHuvudKatalog AS Controls.ComboSimple NO-UNDO.
   DEFINE PUBLIC VARIABLE KonvTillTyp AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE KonvKalkStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKopSlaihop AS System.Windows.Forms.SplitContainer NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkKopKonvControl ( INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkHuvudControl = THIS-OBJECT:ControlShell:KalkHuvudControl. 
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
      THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
      THIS-OBJECT:CreateForm(). 
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:ButtonImportXml:Visible = FALSE.
         
         THIS-OBJECT:ButtonExportXml:Visible = FALSE.
      END.   
      /*
      THIS-OBJECT:HuvudBenamning:GuruText:Text = THIS-OBJECT:KalkHuvudControl:HuvudBenamning:GuruText:Text.
      */
      THIS-OBJECT:splitContainerKopSlaihop:Panel2Collapsed = TRUE. 
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonFlyttaKalkylStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(213),"", System.Windows.Forms.MessageBoxButtons:OkCancel, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "ok" THEN DO:
		   THIS-OBJECT:Root:DatabaseManager:Kalkyl:FlyttaKalkyl(INPUT INTEGER(STRING(THIS-OBJECT:KopHuvudKatalog:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
         THIS-OBJECT:ControlShell:ImportLista().
         THIS-OBJECT:ControlShell:FelMeddCheck().
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
        
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(215), THIS-OBJECT:Root:LanguageManager:GetString(214), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
        /*
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(78), THIS-OBJECT:Root:LanguageManager:GetString(77), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
         */
         THIS-OBJECT:ControlShell:WasExcelShown = FALSE.
		END.
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:KopCreateComboOmrade().
      THIS-OBJECT:CreateComboKopKatalog().
      
   END METHOD.
    METHOD PUBLIC VOID CreateIni():
      THIS-OBJECT:KopHuvudOmrade:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      THIS-OBJECT:KopHuvudKatalog:GuruFiltrera().
      THIS-OBJECT:KopHuvudKatalog:VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
      ASSIGN 
      THIS-OBJECT:KopLabelOmr:TEXT               = Guru.Konstanter:gomrk
      THIS-OBJECT:KonvTillTyp:GuruLabel:Text     = " Konvertera till Kalkyltyp".
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         /*
         THIS-OBJECT:ButtonExportXml:Visible = FALSE.
        splitContainerKopKonvSplit2 Panel2
        */
        THIS-OBJECT:ButtonFlyttaKalkylStrip:Visible = TRUE.
        THIS-OBJECT:ButtonKopieraKalkylStrip:Visible = FALSE.
        THIS-OBJECT:ButtonImportXml:Visible = FALSE.
        THIS-OBJECT:ButtonExportXml:Visible = FALSE.
        THIS-OBJECT:HuvudBenamning:Visible = FALSE.
         THIS-OBJECT:KopLabelOmr:Visible = FALSE.
        THIS-OBJECT:KopHuvudOmrade:Visible = FALSE.
        THIS-OBJECT:splitContainerKopKonv:Panel2Collapsed = TRUE.
     
        /*
        THIS-OBJECT:splitContainerKopKonvSplit2:Panel2Collapsed = TRUE.
        */
      END.    
      ELSE THIS-OBJECT:ButtonFlyttaKalkylStrip:Visible = FALSE.
   END METHOD.
   METHOD PUBLIC VOID KopCreateComboOmrade():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = Guru.Konstanter:OmradeTTh.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  
      THIS-OBJECT:KopHuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboKopKatalog():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KalkdbControl:KatalogTTh.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      THIS-OBJECT:KopHuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").
   END METHOD.
   
	METHOD PUBLIC VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkKopKonvControl").
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
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerKopiera = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKopKonv = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKopKonvSplit = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KopiKalkStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonFlyttaKalkylStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopieraKalkylStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonImportXml = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonExportXml = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerKopSlaihop = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:HuvudBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:KopHuvudKatalog = NEW Controls.ComboSimple().
      THIS-OBJECT:KopLabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:KopLabelOmr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:KopHuvudOmrade = NEW Controls.Combo().
      THIS-OBJECT:splitContainerKopKonvSplit2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KonvKalkStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonKonvKalkylStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:KonvTillTyp = NEW Controls.ComboSimpleLabel().
      CAST(THIS-OBJECT:splitContainerKopiera, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopiera:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopiera:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKopKonv, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopKonv:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonv:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonv:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKopKonvSplit, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopKonvSplit:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonvSplit:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonvSplit:SuspendLayout().
      THIS-OBJECT:KopiKalkStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKopSlaihop, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopSlaihop:SuspendLayout().
      CAST(THIS-OBJECT:KopHuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KopHuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKopKonvSplit2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKopKonvSplit2:SuspendLayout().
      THIS-OBJECT:KonvKalkStrip:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKopiera */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopiera:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopiera:Name = "splitContainerKopiera".
      /*  */
      /* splitContainerKopiera.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Panel1:Controls:Add(THIS-OBJECT:splitContainerKopKonv).
      /*  */
      /* splitContainerKopiera.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopiera:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKopiera.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKopiera:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerKopiera:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerKopiera:SplitterDistance = 1000.
      THIS-OBJECT:splitContainerKopiera:TabIndex = 50.
      /*  */
      /* splitContainerKopKonv */
      /*  */
      THIS-OBJECT:splitContainerKopKonv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopKonv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopKonv:Name = "splitContainerKopKonv".
      /*  */
      /* splitContainerKopKonv.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopKonv:Panel1:Controls:Add(THIS-OBJECT:splitContainerKopKonvSplit).
      /*  */
      /* splitContainerKopKonv.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopKonv:Panel2:Controls:Add(THIS-OBJECT:splitContainerKopKonvSplit2).
      THIS-OBJECT:splitContainerKopKonv:Size = NEW System.Drawing.Size(1000, 546).
      THIS-OBJECT:splitContainerKopKonv:SplitterDistance = 624.
      THIS-OBJECT:splitContainerKopKonv:TabIndex = 0.
      /*  */
      /* splitContainerKopKonvSplit */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopKonvSplit:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopKonvSplit:Name = "splitContainerKopKonvSplit".
      THIS-OBJECT:splitContainerKopKonvSplit:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKopKonvSplit.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit:Panel1:Controls:Add(THIS-OBJECT:KopiKalkStrip).
      /*  */
      /* splitContainerKopKonvSplit.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit:Panel2:Controls:Add(THIS-OBJECT:splitContainerKopSlaihop).
      THIS-OBJECT:splitContainerKopKonvSplit:Size = NEW System.Drawing.Size(624, 546).
      THIS-OBJECT:splitContainerKopKonvSplit:SplitterDistance = 39.
      THIS-OBJECT:splitContainerKopKonvSplit:TabIndex = 48.
      /*  */
      /* KopiKalkStrip */
      /*  */
      THIS-OBJECT:KopiKalkStrip:AllowDrop = TRUE.
      THIS-OBJECT:KopiKalkStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KopiKalkStrip:ImageScalingSize = NEW System.Drawing.Size(32, 32).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonFlyttaKalkylStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonKopieraKalkylStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonImportXml.
      arrayvar0[4] = THIS-OBJECT:ButtonExportXml.
      THIS-OBJECT:KopiKalkStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:KopiKalkStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KopiKalkStrip:Name = "KopiKalkStrip".
      THIS-OBJECT:KopiKalkStrip:Size = NEW System.Drawing.Size(624, 39).
      THIS-OBJECT:KopiKalkStrip:TabIndex = 47.
      THIS-OBJECT:KopiKalkStrip:Text = "toolStrip1".
      THIS-OBJECT:KopiKalkStrip:DragDrop:Subscribe(THIS-OBJECT:KopiKalkStrip_DragDrop).
      THIS-OBJECT:KopiKalkStrip:DragEnter:Subscribe(THIS-OBJECT:KopiKalkStrip_DragEnter).
      /*  */
      /* ButtonFlyttaKalkylStrip */
      /*  */
      THIS-OBJECT:ButtonFlyttaKalkylStrip:Image = CAST(resources:GetObject("ButtonFlyttaKalkylStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonFlyttaKalkylStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonFlyttaKalkylStrip:Name = "ButtonFlyttaKalkylStrip".
      THIS-OBJECT:ButtonFlyttaKalkylStrip:Size = NEW System.Drawing.Size(188, 36).
      THIS-OBJECT:ButtonFlyttaKalkylStrip:Text = "Flytta kalkyl till vald katalog".
      THIS-OBJECT:ButtonFlyttaKalkylStrip:Click:Subscribe(THIS-OBJECT:ButtonFlyttaKalkylStrip_Click).
      /*  */
      /* ButtonKopieraKalkylStrip */
      /*  */
      THIS-OBJECT:ButtonKopieraKalkylStrip:Image = CAST(resources:GetObject("ButtonKopieraKalkylStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopieraKalkylStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopieraKalkylStrip:Name = "ButtonKopieraKalkylStrip".
      THIS-OBJECT:ButtonKopieraKalkylStrip:Size = NEW System.Drawing.Size(199, 36).
      THIS-OBJECT:ButtonKopieraKalkylStrip:Text = "Kopiera kalkyl till vald katalog".
      THIS-OBJECT:ButtonKopieraKalkylStrip:Click:Subscribe(THIS-OBJECT:ButtonKopieraKalkylStrip_Click).
      /*  */
      /* ButtonImportXml */
      /*  */
      THIS-OBJECT:ButtonImportXml:Image = CAST(resources:GetObject("ButtonImportXml.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonImportXml:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonImportXml:Name = "ButtonImportXml".
      THIS-OBJECT:ButtonImportXml:Size = NEW System.Drawing.Size(277, 36).
      THIS-OBJECT:ButtonImportXml:Text = "Importera kalkyl från XML-fil till vald katalog".
      THIS-OBJECT:ButtonImportXml:Click:Subscribe(THIS-OBJECT:ButtonImportXml_Click).
      /*  */
      /* ButtonExportXml */
      /*  */
      THIS-OBJECT:ButtonExportXml:Image = CAST(resources:GetObject("ButtonExportXml.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonExportXml:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonExportXml:Name = "ButtonExportXml".
      THIS-OBJECT:ButtonExportXml:Size = NEW System.Drawing.Size(184, 36).
      THIS-OBJECT:ButtonExportXml:Text = "Exportera kalkyl till XML-fil".
      THIS-OBJECT:ButtonExportXml:ToolTipText = "Exportera kalkyl till XML-fil".
      THIS-OBJECT:ButtonExportXml:Click:Subscribe(THIS-OBJECT:ButtonExportXml_Click).
      /*  */
      /* splitContainerKopSlaihop */
      /*  */
      THIS-OBJECT:splitContainerKopSlaihop:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopSlaihop:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopSlaihop:Name = "splitContainerKopSlaihop".
      THIS-OBJECT:splitContainerKopSlaihop:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKopSlaihop.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:Controls:Add(THIS-OBJECT:HuvudBenamning).
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:Controls:Add(THIS-OBJECT:KopHuvudKatalog).
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:Controls:Add(THIS-OBJECT:KopLabelKatalog).
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:Controls:Add(THIS-OBJECT:KopLabelOmr).
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:Controls:Add(THIS-OBJECT:KopHuvudOmrade).
      THIS-OBJECT:splitContainerKopSlaihop:Size = NEW System.Drawing.Size(624, 503).
      THIS-OBJECT:splitContainerKopSlaihop:SplitterDistance = 322.
      THIS-OBJECT:splitContainerKopSlaihop:TabIndex = 47.
      /*  */
      /* HuvudBenamning */
      /*  */
      THIS-OBJECT:HuvudBenamning:Location = NEW System.Drawing.Point(24, 23).
      THIS-OBJECT:HuvudBenamning:Name = "HuvudBenamning".
      THIS-OBJECT:HuvudBenamning:Rubrik = "GuruLabel".
      THIS-OBJECT:HuvudBenamning:Size = NEW System.Drawing.Size(389, 28).
      THIS-OBJECT:HuvudBenamning:TabIndex = 51.
      THIS-OBJECT:HuvudBenamning:Tag = "".
      /*  */
      /* KopHuvudKatalog */
      /*  */
      THIS-OBJECT:KopHuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:KopHuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:KopHuvudKatalog:Location = NEW System.Drawing.Point(83, 114).
      THIS-OBJECT:KopHuvudKatalog:Name = "KopHuvudKatalog".
      THIS-OBJECT:KopHuvudKatalog:Size = NEW System.Drawing.Size(330, 21).
      THIS-OBJECT:KopHuvudKatalog:TabIndex = 47.
      /*  */
      /* KopLabelKatalog */
      /*  */
      appearance1:TextHAlignAsString = "Left".
      THIS-OBJECT:KopLabelKatalog:Appearance = appearance1.
      THIS-OBJECT:KopLabelKatalog:Location = NEW System.Drawing.Point(24, 114).
      THIS-OBJECT:KopLabelKatalog:Name = "KopLabelKatalog".
      THIS-OBJECT:KopLabelKatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:KopLabelKatalog:TabIndex = 48.
      THIS-OBJECT:KopLabelKatalog:Text = "Katalog".
      /*  */
      /* KopLabelOmr */
      /*  */
      appearance2:TextHAlignAsString = "Left".
      THIS-OBJECT:KopLabelOmr:Appearance = appearance2.
      THIS-OBJECT:KopLabelOmr:Location = NEW System.Drawing.Point(24, 61).
      THIS-OBJECT:KopLabelOmr:Name = "KopLabelOmr".
      THIS-OBJECT:KopLabelOmr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:KopLabelOmr:TabIndex = 50.
      /*  */
      /* KopHuvudOmrade */
      /*  */
      appearance3:BackColor = System.Drawing.SystemColors:Window.
      appearance3:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Appearance = appearance3.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance4:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance4.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance5.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance6:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance6:BackColor2 = System.Drawing.SystemColors:Control.
      appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance6.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      appearance7:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance7.
      appearance8:BackColor = System.Drawing.SystemColors:Highlight.
      appearance8:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance8.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance9:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance9.
      appearance10:BorderColor = System.Drawing.Color:Silver.
      appearance10:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellAppearance = appearance10.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance11:BackColor = System.Drawing.SystemColors:Control.
      appearance11:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance11:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance11:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance11:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance11.
      appearance12:TextHAlignAsString = "Left".
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance12.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:RowAppearance = appearance13.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance14.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:KopHuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:KopHuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:KopHuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:KopHuvudOmrade:Location = NEW System.Drawing.Point(83, 61).
      THIS-OBJECT:KopHuvudOmrade:Name = "KopHuvudOmrade".
      THIS-OBJECT:KopHuvudOmrade:Size = NEW System.Drawing.Size(330, 22).
      THIS-OBJECT:KopHuvudOmrade:TabIndex = 49.
      THIS-OBJECT:KopHuvudOmrade:ValueChanged:Subscribe(THIS-OBJECT:KopHuvudOmrade_ValueChanged).
      /*  */
      /* splitContainerKopKonvSplit2 */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopKonvSplit2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopKonvSplit2:Name = "splitContainerKopKonvSplit2".
      THIS-OBJECT:splitContainerKopKonvSplit2:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKopKonvSplit2.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel1:Controls:Add(THIS-OBJECT:KonvKalkStrip).
      /*  */
      /* splitContainerKopKonvSplit2.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel2:Controls:Add(THIS-OBJECT:KonvTillTyp).
      THIS-OBJECT:splitContainerKopKonvSplit2:Size = NEW System.Drawing.Size(372, 546).
      THIS-OBJECT:splitContainerKopKonvSplit2:SplitterDistance = 39.
      THIS-OBJECT:splitContainerKopKonvSplit2:TabIndex = 50.
      /*  */
      /* KonvKalkStrip */
      /*  */
      THIS-OBJECT:KonvKalkStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KonvKalkStrip:ImageScalingSize = NEW System.Drawing.Size(32, 32).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:ButtonKonvKalkylStrip.
      THIS-OBJECT:KonvKalkStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:KonvKalkStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KonvKalkStrip:Name = "KonvKalkStrip".
      THIS-OBJECT:KonvKalkStrip:Size = NEW System.Drawing.Size(372, 39).
      THIS-OBJECT:KonvKalkStrip:TabIndex = 49.
      THIS-OBJECT:KonvKalkStrip:Text = "toolStrip1".
      /*  */
      /* ButtonKonvKalkylStrip */
      /*  */
      THIS-OBJECT:ButtonKonvKalkylStrip:Image = CAST(resources:GetObject("ButtonKonvKalkylStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKonvKalkylStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKonvKalkylStrip:Name = "ButtonKonvKalkylStrip".
      THIS-OBJECT:ButtonKonvKalkylStrip:Size = NEW System.Drawing.Size(194, 36).
      THIS-OBJECT:ButtonKonvKalkylStrip:Text = "Konvertera kalkyl till vald typ".
      THIS-OBJECT:ButtonKonvKalkylStrip:Click:Subscribe(THIS-OBJECT:ButtonKonvKalkylStrip_Click).
      /*  */
      /* KonvTillTyp */
      /*  */
      THIS-OBJECT:KonvTillTyp:Location = NEW System.Drawing.Point(0, 56).
      THIS-OBJECT:KonvTillTyp:Name = "KonvTillTyp".
      THIS-OBJECT:KonvTillTyp:Size = NEW System.Drawing.Size(400, 28).
      THIS-OBJECT:KonvTillTyp:TabIndex = 47.
      /*  */
      /* KalkKopKonvControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerKopiera).
      THIS-OBJECT:Name = "KalkKopKonvControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerKopiera:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKopiera, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopiera:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonv:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonv:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKopKonv, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopKonv:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonvSplit:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonvSplit:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKopKonvSplit:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKopKonvSplit, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopKonvSplit:ResumeLayout(FALSE).
      THIS-OBJECT:KopiKalkStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KopiKalkStrip:PerformLayout().
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopSlaihop:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKopSlaihop, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopSlaihop:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KopHuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KopHuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKopKonvSplit2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKopKonvSplit2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopKonvSplit2:ResumeLayout(FALSE).
      THIS-OBJECT:KonvKalkStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KonvKalkStrip:PerformLayout().
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
   METHOD PRIVATE VOID ButtonKopieraKalkylStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
     
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Kopierakalkyl(INPUT INTEGER(STRING(THIS-OBJECT:KopHuvudKatalog:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
     /*
      IF felmed = TRUE THEN RETURN.
      */
      
      THIS-OBJECT:ControlShell:ImportLista().
      THIS-OBJECT:ControlShell:FelMeddCheck().
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(78), THIS-OBJECT:Root:LanguageManager:GetString(77), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:ControlShell:WasExcelShown = FALSE.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonImportXml_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:XmlImport() EQ TRUE THEN DO:
         THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KopierakalkylXML(INPUT INTEGER(STRING(THIS-OBJECT:KopHuvudKatalog:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
         IF felmed = TRUE THEN RETURN.
         THIS-OBJECT:ControlShell:ImportLista().
         THIS-OBJECT:ControlShell:FelMeddCheck().
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(82), THIS-OBJECT:Root:LanguageManager:GetString(81), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
         THIS-OBJECT:ControlShell:WasExcelShown = FALSE.
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonExportXml_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:XmlExport(THIS-OBJECT:ControlShell:KalkNrvar,THIS-OBJECT:ControlShell:Omradevar).
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KopHuvudOmrade_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:SparaComboV(KopHuvudOmrade).
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonKonvKalkylStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
       
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KonverteraKalkyl(INPUT INTEGER(STRING(THIS-OBJECT:KonvTillTyp:GuruCombo:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
      
      THIS-OBJECT:ControlShell:UpdateRibbonShow().  
      THIS-OBJECT:ControlShell:SparaDefs(?,?).
      
      THIS-OBJECT:ControlShell:ImportLista().
      THIS-OBJECT:ControlShell:FelMeddCheck().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
      
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(80), THIS-OBJECT:Root:LanguageManager:GetString(79), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
      THIS-OBJECT:ControlShell:WasExcelShown = FALSE.
      RETURN.
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KopiKalkStrip_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		DEFINE VARIABLE objFiles AS "System.String[]" NO-UNDO.  
		DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.    
                                                                                                                                                                                                                                              
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
         objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").
         IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:XmlImportDo(objFiles:GetValue(0):ToString()) EQ TRUE THEN DO:
            THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:KopierakalkylXML(INPUT INTEGER(STRING(THIS-OBJECT:KopHuvudKatalog:VALUE)),INPUT STRING(THIS-OBJECT:KopHuvudOmrade:VALUE),OUTPUT felmed).
            IF felmed = TRUE THEN RETURN.
            THIS-OBJECT:ControlShell:ImportLista().
            THIS-OBJECT:ControlShell:FelMeddCheck().
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(82), THIS-OBJECT:Root:LanguageManager:GetString(81), System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
            THIS-OBJECT:ControlShell:WasExcelShown = FALSE.
         END.
      END. 
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         RETURN.
      END.                                                                                                                                                                                                                                                        
		RETURN.
	END METHOD.  

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KopiKalkStrip_DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
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
	END METHOD.

	   
	DESTRUCTOR PUBLIC KalkKopKonvControl ( ):
     /* {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.