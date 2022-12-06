 
 /*------------------------------------------------------------------------
    File        : EkgKataloger
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Mon Oct 14 09:34:02 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgKatalogerControl INHERITS UserControl: 
  {EKGTH.I}
   DEFINE PUBLIC VARIABLE gridKataloger AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KopieraKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogerValjButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE KatalogKatalogSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortKatalogButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   
 CONSTRUCTOR PUBLIC EkgKatalogerControl (INPUT r AS Guru.Root):
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      {EKGDBH.i ControlDataDb}
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridKataloger().
   END METHOD. 
   METHOD PUBLIC VOID CreateGridKataloger(): 
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ARTAL","",FALSE,70).     
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250). 
      rubrikergrid[4] = NEW Controls.GridRubrik("STARTDATUM","",TRUE,70).
      rubrikergrid[5] = NEW Controls.GridRubrik("SLUTDATUM","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[7] = NEW Controls.GridRubrik("ANVANDSAVNAMN","",TRUE,200).
      /*Guru.Konstanter:kalk2sekvar[15] resurs*/
      IF Guru.Konstanter:kalk2sekvar[13] = TRUE THEN rubrikergrid[8] = NEW Controls.GridRubrik("LASTA","",FALSE,70, "LOG").
      ELSE rubrikergrid[8] = NEW Controls.GridRubrik("LASTA","",TRUE,70, "LOG").
      IF Guru.Konstanter:kalk2sekvar[14] = TRUE THEN rubrikergrid[9] = NEW Controls.GridRubrik("ANVANDS","",FALSE,70, "LOG").
      ELSE rubrikergrid[9] = NEW Controls.GridRubrik("ANVANDS","",TRUE,70, "LOG").
      THIS-OBJECT:GridKataloger:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 9, INPUT THIS-OBJECT:ekgsubkatalogerTTh)).
      THIS-OBJECT:GridKataloger:Text = "Kataloger".
      THIS-OBJECT:GridKataloger:GuruUpdateTitle().
      
      /*DÖLJ*/
      THIS-OBJECT:GridKataloger:DisplayLayout:Bands[0]:Columns["STARTDATUM"]:Hidden = TRUE.
      THIS-OBJECT:GridKataloger:DisplayLayout:Bands[0]:Columns["SLUTDATUM"]:Hidden = TRUE.
      THIS-OBJECT:GridKataloger:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = FALSE.
      THIS-OBJECT:GridKataloger:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = FALSE. 
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN RETURN.
      THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde). 
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      IF beforekolnamn = "BENAMNING" THEN DO:
          IF THIS-OBJECT:ControlDataDb:LastKat(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) = TRUE THEN DO:
            e:Cancel = TRUE.
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(149),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
            RETURN.   
          END.
          rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(148),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
          IF rrr:ToString() = "Yes" THEN.
             ELSE DO:
             e:Cancel = TRUE.
          END.
      END.
      IF beforekolnamn = "ARTAL" THEN DO:
          IF THIS-OBJECT:ControlDataDb:LastKat(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) = TRUE THEN DO:
            e:Cancel = TRUE.
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(149),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information).
            RETURN.   
          END.
          rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(150),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
          IF rrr:ToString() = "Yes" THEN.
             ELSE DO:
             e:Cancel = TRUE.
          END.
      END. 
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      beforekolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF THIS-OBJECT:gridKataloger:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      IF kolnamn = "LASTA" THEN DO:
         IF beforekolvarde = "FALSE" THEN DO:            
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(143),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN kolvarde = "TRUE".
            ELSE kolvarde = beforekolvarde.
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(144),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() = "Yes" THEN kolvarde = "FALSE".
            ELSE kolvarde = beforekolvarde.
         END.       

/*       IF beforekolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".*/ 
         THIS-OBJECT:ControlWinLg:KatalogUppdate(INPUT STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE), INPUT kolnamn, INPUT kolvarde, INPUT beforekolnamn ,INPUT beforekolvarde).
      END. 
      IF kolnamn = "ANVANDS" THEN DO:
         IF beforekolvarde = "TRUE" THEN DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOff(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
         ELSE DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOn(STRING(THIS-OBJECT:gridKataloger:ActiveRow:Cells["EKGSUBID"]:VALUE)).
         END.
      END. 
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridKataloger_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).
      RETURN.

   END METHOD.
 
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KatalogerValjButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:gridKataloger:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      THIS-OBJECT:ControlWinLg:ValjKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).   
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID KopieraKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:ControlWinLg:KopieraKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      RETURN.
 

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortKatalogButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ControlDataDb:LastKat(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(149),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information). 
         RETURN.
      END.
      IF THIS-OBJECT:ControlDataDb:AnvandsKat(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(151),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Information). 
         RETURN.
      END.
      THIS-OBJECT:ControlWinLg:TaBortKatalog(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, STRING(THIS-OBJECT:ekgsubkatalogerTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE)).      
      RETURN.

   END METHOD.   
     


   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgKatalogerControl").
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
      THIS-OBJECT:KatalogMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogKatalogSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:KatalogertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:KatalogerValjButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:KopieraKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortKatalogButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridKataloger = NEW Controls.Grid().
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogMainSC:SuspendLayout().
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:KatalogKatalogSC:Panel1:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:SuspendLayout().
      THIS-OBJECT:KatalogKatalogSC:SuspendLayout().
      THIS-OBJECT:KatalogertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* KatalogMainSC */
      /*  */
      THIS-OBJECT:KatalogMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogMainSC:Name = "KatalogMainSC".
      /*  */
      /* KatalogMainSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel1:Controls:Add(THIS-OBJECT:KatalogKatalogSC).
      /*  */
      /* KatalogMainSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImage = CAST(resources:GetObject("KatalogMainSC.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:KatalogMainSC:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Stretch.
      THIS-OBJECT:KatalogMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:KatalogMainSC:SplitterDistance = 770.
      THIS-OBJECT:KatalogMainSC:TabIndex = 2.
      /*  */
      /* KatalogKatalogSC */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KatalogKatalogSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:KatalogKatalogSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogKatalogSC:Name = "KatalogKatalogSC".
      THIS-OBJECT:KatalogKatalogSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* KatalogKatalogSC.Panel1 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel1:Controls:Add(THIS-OBJECT:KatalogertoolStrip).
      /*  */
      /* KatalogKatalogSC.Panel2 */
      /*  */
      THIS-OBJECT:KatalogKatalogSC:Panel2:Controls:Add(THIS-OBJECT:gridKataloger).
      THIS-OBJECT:KatalogKatalogSC:Size = NEW System.Drawing.Size(770, 553).
      THIS-OBJECT:KatalogKatalogSC:SplitterDistance = 25.
      THIS-OBJECT:KatalogKatalogSC:TabIndex = 0.
      /*  */
      /* KatalogertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:KatalogerValjButton.
      arrayvar0[2] = THIS-OBJECT:KopieraKatalogButton.
      arrayvar0[3] = THIS-OBJECT:TaBortKatalogButton.
      THIS-OBJECT:KatalogertoolStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:KatalogertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KatalogertoolStrip:Name = "KatalogertoolStrip".
      THIS-OBJECT:KatalogertoolStrip:Size = NEW System.Drawing.Size(770, 25).
      THIS-OBJECT:KatalogertoolStrip:TabIndex = 1.
      THIS-OBJECT:KatalogertoolStrip:Text = "toolStrip1".
      /*  */
      /* KatalogerValjButton */
      /*  */
      THIS-OBJECT:KatalogerValjButton:Image = CAST(resources:GetObject("KatalogerValjButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KatalogerValjButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KatalogerValjButton:Name = "KatalogerValjButton".
      THIS-OBJECT:KatalogerValjButton:Size = NEW System.Drawing.Size(88, 22).
      THIS-OBJECT:KatalogerValjButton:Text = "Välj katalog".
      THIS-OBJECT:KatalogerValjButton:Click:Subscribe(THIS-OBJECT:KatalogerValjButton_Click).
      /*  */
      /* KopieraKatalogButton */
      /*  */
      THIS-OBJECT:KopieraKatalogButton:Image = CAST(resources:GetObject("KopieraKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:KopieraKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:KopieraKatalogButton:Name = "KopieraKatalogButton".
      THIS-OBJECT:KopieraKatalogButton:Size = NEW System.Drawing.Size(109, 22).
      THIS-OBJECT:KopieraKatalogButton:Text = "Kopiera katalog".
      THIS-OBJECT:KopieraKatalogButton:Click:Subscribe(THIS-OBJECT:KopieraKatalogButton_Click).
      /*  */
      /* TaBortKatalogButton */
      /*  */
      THIS-OBJECT:TaBortKatalogButton:Image = CAST(resources:GetObject("TaBortKatalogButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortKatalogButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortKatalogButton:Name = "TaBortKatalogButton".
      THIS-OBJECT:TaBortKatalogButton:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:TaBortKatalogButton:Text = "Ta bort katalog".
      THIS-OBJECT:TaBortKatalogButton:Click:Subscribe(THIS-OBJECT:TaBortKatalogButton_Click).
      /*  */
      /* gridKataloger */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridKataloger:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridKataloger:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridKataloger:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridKataloger:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridKataloger:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridKataloger:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridKataloger:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridKataloger:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridKataloger:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridKataloger:GuruContext = ?.
      THIS-OBJECT:gridKataloger:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridKataloger:Name = "gridKataloger".
      THIS-OBJECT:gridKataloger:Size = NEW System.Drawing.Size(770, 524).
      THIS-OBJECT:gridKataloger:TabIndex = 0.
      THIS-OBJECT:gridKataloger:Text = "grid1".
      THIS-OBJECT:gridKataloger:AfterCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_AfterCellUpdate).
      THIS-OBJECT:gridKataloger:CellChange:Subscribe(THIS-OBJECT:gridKataloger_CellChange).
      THIS-OBJECT:gridKataloger:BeforeCellUpdate:Subscribe(THIS-OBJECT:gridKataloger_BeforeCellUpdate).
      THIS-OBJECT:gridKataloger:DoubleClickRow:Subscribe(THIS-OBJECT:gridKataloger_DoubleClickRow).
      /*  */
      /* EkgKatalogerControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:KatalogMainSC).
      THIS-OBJECT:Name = "EkgKatalogerControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:KatalogMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogKatalogSC:Panel1:PerformLayout().
      THIS-OBJECT:KatalogKatalogSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KatalogKatalogSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:KatalogKatalogSC:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:KatalogertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridKataloger, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.

END CLASS.