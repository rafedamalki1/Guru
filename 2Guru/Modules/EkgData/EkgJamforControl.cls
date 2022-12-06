 
 /*------------------------------------------------------------------------
    File        : EkgJamforControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Oct 18 09:58:41 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.



CLASS Modules.EkgData.EkgJamforControl INHERITS UserControl: 
   
   
   DEFINE PRIVATE VARIABLE JamforKatalogerSplitSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE JamforKatalogerMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE jamforMtrlToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE jamforP3ToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE jamforP2ToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE jamforP1ToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PUBLIC VARIABLE GridJamforKatalog2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridJamforKatalog1 AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE JamforSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE JamfortoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE JamforMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE StripButtonJamfor AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   
   
  CONSTRUCTOR PUBLIC EkgJamforControl (INPUT r AS Guru.Root):
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      
      
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
     THIS-OBJECT:JamforSC:Panel2Collapsed = TRUE.
     THIS-OBJECT:JamforMainSC:Panel2Collapsed = TRUE.
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      /*P1*/
      THIS-OBJECT:CreateGridJamforKatalog1().
      THIS-OBJECT:CreateGridJamforKatalog2().
      

   END METHOD.
   METHOD PUBLIC VOID CreateGridJamforKatalog1():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("ARTAL","",TRUE,70).     
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250). 
      rubrikergrid[4] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      
      THIS-OBJECT:GridJamforKatalog1:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh)).
      THIS-OBJECT:GridJamforKatalog1:Text = "Kataloger".
      THIS-OBJECT:GridJamforKatalog1:GuruUpdateTitle().
       
   END METHOD. 
   METHOD PUBLIC VOID CreateGridJamforKatalog2():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("ARTAL","",TRUE,70).     
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250). 
      rubrikergrid[4] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      
      THIS-OBJECT:GridJamforKatalog2:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh)).
      THIS-OBJECT:GridJamforKatalog2:Text = "Kataloger".
      THIS-OBJECT:GridJamforKatalog2:GuruUpdateTitle().
       
   END METHOD. 
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgJamforControl").
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
      THIS-OBJECT:JamforMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:JamforSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:JamforKatalogerMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:JamfortoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:StripButtonJamfor = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:jamforP1ToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:jamforP2ToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:jamforP3ToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:jamforMtrlToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:JamforKatalogerSplitSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridJamforKatalog1 = NEW Controls.Grid().
      THIS-OBJECT:GridJamforKatalog2 = NEW Controls.Grid().
      CAST(THIS-OBJECT:JamforMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:JamforMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:JamforMainSC:SuspendLayout().
      CAST(THIS-OBJECT:JamforSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:JamforSC:Panel1:SuspendLayout().
      THIS-OBJECT:JamforSC:SuspendLayout().
      CAST(THIS-OBJECT:JamforKatalogerMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:JamforKatalogerMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:JamforKatalogerMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:JamforKatalogerMenuSC:SuspendLayout().
      THIS-OBJECT:JamfortoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:JamforKatalogerSplitSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:JamforKatalogerSplitSC:Panel1:SuspendLayout().
      THIS-OBJECT:JamforKatalogerSplitSC:Panel2:SuspendLayout().
      THIS-OBJECT:JamforKatalogerSplitSC:SuspendLayout().
      CAST(THIS-OBJECT:GridJamforKatalog1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridJamforKatalog2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* JamforMainSC */
      /*  */
      THIS-OBJECT:JamforMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:JamforMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:JamforMainSC:Name = "JamforMainSC".
      /*  */
      /* JamforMainSC.Panel1 */
      /*  */
      THIS-OBJECT:JamforMainSC:Panel1:Controls:Add(THIS-OBJECT:JamforSC).
      THIS-OBJECT:JamforMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:JamforMainSC:SplitterDistance = 895.
      THIS-OBJECT:JamforMainSC:TabIndex = 2.
      /*  */
      /* JamforSC */
      /*  */
      THIS-OBJECT:JamforSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:JamforSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:JamforSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:JamforSC:Name = "JamforSC".
      THIS-OBJECT:JamforSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* JamforSC.Panel1 */
      /*  */
      THIS-OBJECT:JamforSC:Panel1:Controls:Add(THIS-OBJECT:JamforKatalogerMenuSC).
      THIS-OBJECT:JamforSC:Size = NEW System.Drawing.Size(895, 553).
      THIS-OBJECT:JamforSC:SplitterDistance = 189.
      THIS-OBJECT:JamforSC:TabIndex = 1.
      /*  */
      /* JamforKatalogerMenuSC */
      /*  */
      THIS-OBJECT:JamforKatalogerMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:JamforKatalogerMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:JamforKatalogerMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:JamforKatalogerMenuSC:Name = "JamforKatalogerMenuSC".
      THIS-OBJECT:JamforKatalogerMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* JamforKatalogerMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:JamforKatalogerMenuSC:Panel1:Controls:Add(THIS-OBJECT:JamfortoolStrip).
      /*  */
      /* JamforKatalogerMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:JamforKatalogerMenuSC:Panel2:Controls:Add(THIS-OBJECT:JamforKatalogerSplitSC).
      THIS-OBJECT:JamforKatalogerMenuSC:Size = NEW System.Drawing.Size(895, 189).
      THIS-OBJECT:JamforKatalogerMenuSC:SplitterDistance = 25.
      THIS-OBJECT:JamforKatalogerMenuSC:TabIndex = 0.
      /*  */
      /* JamfortoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:StripButtonJamfor.
      THIS-OBJECT:JamfortoolStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:JamfortoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:JamfortoolStrip:Name = "JamfortoolStrip".
      THIS-OBJECT:JamfortoolStrip:Size = NEW System.Drawing.Size(895, 25).
      THIS-OBJECT:JamfortoolStrip:TabIndex = 2.
      THIS-OBJECT:JamfortoolStrip:Text = "toolStrip1".
      /*  */
      /* StripButtonJamfor */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:jamforP1ToolStripMenuItem.
      arrayvar1[2] = THIS-OBJECT:jamforP2ToolStripMenuItem.
      arrayvar1[3] = THIS-OBJECT:jamforP3ToolStripMenuItem.
      arrayvar1[4] = THIS-OBJECT:jamforMtrlToolStripMenuItem.
      THIS-OBJECT:StripButtonJamfor:DropDownItems:AddRange(arrayvar1).
      THIS-OBJECT:StripButtonJamfor:Image = CAST(resources:GetObject("StripButtonJamfor.Image"), System.Drawing.Image).
      THIS-OBJECT:StripButtonJamfor:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:StripButtonJamfor:Name = "StripButtonJamfor".
      THIS-OBJECT:StripButtonJamfor:Size = NEW System.Drawing.Size(72, 22).
      THIS-OBJECT:StripButtonJamfor:Text = "Jämför".
      THIS-OBJECT:StripButtonJamfor:ToolTipText = "Välj nivå att jämföra".
      /*  */
      /* jamforP1ToolStripMenuItem */
      /*  */
      THIS-OBJECT:jamforP1ToolStripMenuItem:Image = CAST(resources:GetObject("jamforP1ToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:jamforP1ToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:jamforP1ToolStripMenuItem:Name = "jamforP1ToolStripMenuItem".
      THIS-OBJECT:jamforP1ToolStripMenuItem:Size = NEW System.Drawing.Size(190, 56).
      THIS-OBJECT:jamforP1ToolStripMenuItem:Text = "Jämför P1".
      THIS-OBJECT:jamforP1ToolStripMenuItem:Click:Subscribe(THIS-OBJECT:jamforP1ToolStripMenuItem_Click).
      /*  */
      /* jamforP2ToolStripMenuItem */
      /*  */
      THIS-OBJECT:jamforP2ToolStripMenuItem:Image = CAST(resources:GetObject("jamforP2ToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:jamforP2ToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:jamforP2ToolStripMenuItem:Name = "jamforP2ToolStripMenuItem".
      THIS-OBJECT:jamforP2ToolStripMenuItem:Size = NEW System.Drawing.Size(190, 56).
      THIS-OBJECT:jamforP2ToolStripMenuItem:Text = "Jämför P2".
      THIS-OBJECT:jamforP2ToolStripMenuItem:Click:Subscribe(THIS-OBJECT:jamforP2ToolStripMenuItem_Click).
      /*  */
      /* jamforP3ToolStripMenuItem */
      /*  */
      THIS-OBJECT:jamforP3ToolStripMenuItem:Image = CAST(resources:GetObject("jamforP3ToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:jamforP3ToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:jamforP3ToolStripMenuItem:Name = "jamforP3ToolStripMenuItem".
      THIS-OBJECT:jamforP3ToolStripMenuItem:Size = NEW System.Drawing.Size(190, 56).
      THIS-OBJECT:jamforP3ToolStripMenuItem:Text = "Jämför P3".
      THIS-OBJECT:jamforP3ToolStripMenuItem:Click:Subscribe(THIS-OBJECT:jamforP3ToolStripMenuItem_Click).
      /*  */
      /* jamforMtrlToolStripMenuItem */
      /*  */
      THIS-OBJECT:jamforMtrlToolStripMenuItem:Image = CAST(resources:GetObject("jamforMtrlToolStripMenuItem.Image"), System.Drawing.Image).
      THIS-OBJECT:jamforMtrlToolStripMenuItem:ImageScaling = System.Windows.Forms.ToolStripItemImageScaling:None.
      THIS-OBJECT:jamforMtrlToolStripMenuItem:Name = "jamforMtrlToolStripMenuItem".
      THIS-OBJECT:jamforMtrlToolStripMenuItem:Size = NEW System.Drawing.Size(190, 56).
      THIS-OBJECT:jamforMtrlToolStripMenuItem:Text = "Jämför Materiel".
      THIS-OBJECT:jamforMtrlToolStripMenuItem:Click:Subscribe(THIS-OBJECT:jamforMtrlToolStripMenuItem_Click).
      /*  */
      /* JamforKatalogerSplitSC */
      /*  */
      THIS-OBJECT:JamforKatalogerSplitSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:JamforKatalogerSplitSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:JamforKatalogerSplitSC:Name = "JamforKatalogerSplitSC".
      /*  */
      /* JamforKatalogerSplitSC.Panel1 */
      /*  */
      THIS-OBJECT:JamforKatalogerSplitSC:Panel1:Controls:Add(THIS-OBJECT:GridJamforKatalog1).
      /*  */
      /* JamforKatalogerSplitSC.Panel2 */
      /*  */
      THIS-OBJECT:JamforKatalogerSplitSC:Panel2:Controls:Add(THIS-OBJECT:GridJamforKatalog2).
      THIS-OBJECT:JamforKatalogerSplitSC:Size = NEW System.Drawing.Size(895, 160).
      THIS-OBJECT:JamforKatalogerSplitSC:SplitterDistance = 375.
      THIS-OBJECT:JamforKatalogerSplitSC:TabIndex = 0.
      /*  */
      /* GridJamforKatalog1 */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridJamforKatalog1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridJamforKatalog1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridJamforKatalog1:GuruContext = ?.
      THIS-OBJECT:GridJamforKatalog1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridJamforKatalog1:Name = "GridJamforKatalog1".
      THIS-OBJECT:GridJamforKatalog1:Size = NEW System.Drawing.Size(375, 160).
      THIS-OBJECT:GridJamforKatalog1:TabIndex = 1.
      THIS-OBJECT:GridJamforKatalog1:Text = "grid1".
      /*  */
      /* GridJamforKatalog2 */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridJamforKatalog2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridJamforKatalog2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridJamforKatalog2:GuruContext = ?.
      THIS-OBJECT:GridJamforKatalog2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridJamforKatalog2:Name = "GridJamforKatalog2".
      THIS-OBJECT:GridJamforKatalog2:Size = NEW System.Drawing.Size(516, 160).
      THIS-OBJECT:GridJamforKatalog2:TabIndex = 1.
      THIS-OBJECT:GridJamforKatalog2:Text = "grid1".
      /*  */
      /* EkgJamforControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:JamforMainSC).
      THIS-OBJECT:Name = "EkgJamforControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:JamforMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:JamforMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:JamforMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:JamforSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:JamforSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:JamforSC:ResumeLayout(FALSE).
      THIS-OBJECT:JamforKatalogerMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:JamforKatalogerMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:JamforKatalogerMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:JamforKatalogerMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:JamforKatalogerMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:JamfortoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:JamfortoolStrip:PerformLayout().
      THIS-OBJECT:JamforKatalogerSplitSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:JamforKatalogerSplitSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:JamforKatalogerSplitSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:JamforKatalogerSplitSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridJamforKatalog1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridJamforKatalog2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.


   METHOD PRIVATE LOGICAL jamforKoll(INPUT listp AS CHARACTER ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
      IF THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE = THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE THEN felvar = TRUE.
      ELSE DO: 
         IF listp = "" THEN DO: 
            IF THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE NE THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE THEN felvar = TRUE.
         END.
      END.
      IF felvar = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(140), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE .
      END.
      ELSE RETURN TRUE.   

   END METHOD.
    /*EVENTS*/
  
   METHOD PUBLIC VOID JmfKLGData(INPUT listaP AS CHARACTER):
     DEFINE VARIABLE path AS CHARACTER NO-UNDO.
     DEFINE VARIABLE blad AS INTEGER NO-UNDO.
     THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
     THIS-OBJECT:ControlWinLg:VisaExcel:direkticell = TRUE.
     THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE.
     THIS-OBJECT:ControlWinLg:VisaExcel:OutExterntWin = FALSE.
     
     IF THIS-OBJECT:ControlWinLg:VisaExcel:UseMsInterop = FALSE THEN  THIS-OBJECT:ControlWinLg:VisaExcel:StartExcelVal(INPUT FALSE).
     THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(FALSE).
     Guru.Konstanter:AmericanSet().
     
     THIS-OBJECT:JmfPageHead(INPUT listaP).
     THIS-OBJECT:JmfKLGDataKoder(INPUT listaP).
     THIS-OBJECT:ControlWinLg:VisaExcel:ScreenUpp(TRUE).  
     
     THIS-OBJECT:ControlWinLg:VisaExcel:SidBrytBredd(2).
     THIS-OBJECT:ControlWinLg:VisaExcel:SlutExcel(3).
     Guru.Konstanter:EuropeanAmericanReset().
     THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
  END METHOD.  
  METHOD PUBLIC VOID JmfPageHead(INPUT listaP AS CHARACTER):
     DEFINE VARIABLE dataout AS CHARACTER NO-UNDO.
     DEFINE VARIABLE link              AS CHARACTER NO-UNDO.
     THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
     THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
     link = THIS-OBJECT:Root:DatabaseManager:Global:LaddaLogga("EBRBILD").
    
     /*
     link = THIS-OBJECT:Root:DatabaseManager:Global:LaddaLogga( Guru.Konstanter:globforetag).
     RUN LOGGORIN.P (INPUT Guru.Konstanter:globforetag, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
     */
     dataout = THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + " " + THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE + " -(" + 
     STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + ") och " + Guru.Konstanter:globradbrytch +
     THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + " " + THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE + " -(" + 
     STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + ")".
   
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetUppFooter(INPUT 1, INPUT STRING(TODAY)).
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetUppFooter(INPUT 2, INPUT  '&",Bold"' + "EBR utges av Svensk Energi").
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetSidnum(INPUT 3, INPUT FALSE).
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetuppMargin(INPUT 2, INPUT 60).
     IF link NE ? THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:PageSetUppHeaderImage(INPUT 1, INPUT link). 
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
     END.
     dataout = "Jämförelse mellan "  + Guru.Konstanter:globradbrytch + dataout.
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetUppHeader(INPUT 2, INPUT '&",Bold"' + dataout).
     dataout = "KOSTNADSKATALOG" + Guru.Konstanter:globradbrytch +
     '&",Bold"' + "Distributionanläggningar" + Guru.Konstanter:globradbrytch +
     THIS-OBJECT:SpanUt(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE) + Guru.Konstanter:globradbrytch.
     IF listaP = "P1" THEN DO:
        dataout = dataout + "Planeringskatalog P1".                    
     END.   
     IF listaP = "P2" THEN DO:
        dataout = dataout + "Projekteringskatalog P2".        
     END.
     IF listaP = "P3" THEN DO:
        dataout = dataout + "Produktionskatalog, P3".                     
     END.
     IF listaP = "M" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:PageSetuppMargin(INPUT 2, INPUT 80).
        dataout = dataout + "Artikelregister".
        
     END.  
     THIS-OBJECT:ControlWinLg:VisaExcel:PageSetUppHeader(INPUT 3, INPUT dataout).        
       
  END METHOD.  
  METHOD CHARACTER SpanUt (INPUT spannvar AS CHARACTER):
     IF spannvar = "KLG1" THEN RETURN "0,4-24kV".
     ELSE RETURN "36 - 145kV". 
  END METHOD.     
  METHOD PUBLIC VOID JmfKLGDataKoder(INPUT listaP AS CHARACTER): 
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
     DEFINE VARIABLE anmkop AS CHARACTER NO-UNDO.
     DEFINE VARIABLE path AS CHARACTER NO-UNDO.
     THIS-OBJECT:ControlDataDb:LaddaJmfKatalog(INPUT listaP, INPUT THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE, INPUT THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE).
     /*bladnamn + bredd*/
    
     
     THIS-OBJECT:ControlWinLg:ExportKolFix(INPUT STRING("10000")).
     IF listaP = "P1" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel(listaP).
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),4).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A1","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B1","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C1","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G1","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K1","Summa kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M1","Kommentar").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:F1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G1:H1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K1:L1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M1:M2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","L1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","M1","3:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","M2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        
        queryvar =  "FOR EACH " + STRING(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:TABLE + " BY P1ARBKOD").
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
          
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:TABLE + " WHERE ekgp1TT.EKGSUBID = " + 
           STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + " AND ekgp1TT.P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "'" + " BY P1ARBKOD BY P1LOPNR".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:FIND-FIRST("WHERE ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + 
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE ) NO-ERROR.
              IF THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:AVAILABLE THEN DO:
                 THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
                 THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),THIS-OBJECT:ControlDataDb:ekgarbkodextraanmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
                 THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              END.
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              IF THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = ? THEN THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "". 
              IF anmkop NE "" THEN DO:
                 IF anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)) THEN THIS-OBJECT:ControlWinLg:VisaExcel:DataOut('-"-').
                 ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
              END.   
              ELSE THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE))).
              anmkop = TRIM(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:FIND-FIRST("WHERE EKGSUBID = " + 
              STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + 
              " AND P1ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P1ARBKOD"):BUFFER-VALUE + "' AND P1LOPNR = " + 
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P1LOPNR"):BUFFER-VALUE) NO-ERROR.
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "F".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.
           
           qH:GET-NEXT().         
        END.
        
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "N" THEN LEAVE.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
     END. 
     IF listaP = "P2" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel(listaP).
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),4).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("P" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("Q" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("R" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("S" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("T" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("U" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("V" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Timmar").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M1","Kostnader").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2","Arbete").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("O2","Matriel").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("Q2","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("S2","Övrigt").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("U2","Summa kr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A3","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B3","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C3","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D3","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
      
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("O3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("P3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("Q3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("R3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("S3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("T3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("U3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("V3",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:L1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M1:V1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A2:A3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B2:B3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C2:C3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D2:D3",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E2:F2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G2:H2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I2:J2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K2:L2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M2:N2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("O2:P2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("Q2:R2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("S2:T2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("U2:V2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","V1","3:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","V1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E2","V2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A3","V3","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + STRING(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:TABLE + " BY P2ARBKOD").
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"V" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:TABLE + " WHERE ekgp2TT.EKGSUBID = " + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + " AND ekgp2TT.P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'" + " BY P2ARBKOD BY P2LOPNR".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:FIND-FIRST("WHERE EKGSUBID = " + 
              STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + 
              " AND P2ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND P2LOPNR = " + 
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P2LOPNR"):BUFFER-VALUE) NO-ERROR.
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "F".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.
           qH:GET-NEXT().         
        END. 
        
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "X" THEN LEAVE.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "2",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:BordVert("D1","D1","2:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordVert("L1","L1","2:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordVert("V1","V1","2:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"V" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
     END.
     IF listaP = "P3" THEN DO:
        /*bladnamn + bredd*/
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel(listaP).
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),7.14). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),4).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43.71).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),6).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("K" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("L" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("M" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("N" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("O" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("P" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E1","Bered").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G1","Mont").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I1","Maskin").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K1","EA").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M1","Antal man").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("O1","Kommentar").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A2","Arb" + Guru.Konstanter:globradbrytch + "kod").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B2","Löp" + Guru.Konstanter:globradbrytch + "nr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C2","Arbete" + Guru.Konstanter:globradbrytch + "Konstruktion").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D2","En-" + Guru.Konstanter:globradbrytch + "het"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("K2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("L2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("M2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("N2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("P2","Tids-" + Guru.Konstanter:globradbrytch + "underlag").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:D2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("E1:F1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("G1:H1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("I1:J1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("K1:L1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("M1:N1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("O1:O2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("P1:P2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1","P1","3:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("E1","N1","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","P2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        queryvar =  "FOR EACH " + STRING(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:TABLE + " BY P3ARBKOD").
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh,queryvar).
        qH:GET-FIRST().
        DO WHILE qH:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:FontExcel("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad) ,"V" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),"",0,TRUE ,0,0 ).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           
           query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:TABLE + " WHERE ekgp3TT.EKGSUBID = " + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + " AND ekgp3TT.P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgArbkodJmfTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + "'" + " BY P3ARBKOD BY P3LOPNR".
           q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh,query2var).
           q2H:GET-FIRST().
           DO WHILE q2H:QUERY-OFF-END = FALSE:
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "O".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
              THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
              THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("TIDSUNDER"):BUFFER-VALUE)).
                           
              THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:FIND-FIRST("WHERE EKGSUBID = " +  STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + 
              " AND P3ARBKOD = '"  + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P3ARBKOD"):BUFFER-VALUE + 
              "' AND P3LOPNR = " + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("P3LOPNR"):BUFFER-VALUE) NO-ERROR.
              THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "F".
              THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
              THIS-OBJECT:JmfKLGDataKoderOut(listaP).
              THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
              q2H:GET-NEXT().         
           END.
           qH:GET-NEXT().         
        END. 
        
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "Q" THEN LEAVE.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"V" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
     END.
     IF listaP = "M" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:ValjBlad(1).
        THIS-OBJECT:ControlWinLg:VisaExcel:namnbladexcel(listaP).
        THIS-OBJECT:ControlWinLg:VisaExcel:iSistaColnum = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:iRad = 1.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10). 
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("B" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),43).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("C" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("D" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("E" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("F" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("G" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("H" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("I" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColWidth("J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad),10).
         /*ut rubriker*/
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D1","Pris"). 
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F1","Baspris").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H1","Kalkylpris").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("A2","Artnr").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("B2","Benämning").
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("C2","Enhet").
        
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("D2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("E2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("F2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("G2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("H2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("I2",STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE) + "-" + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE)).        
        THIS-OBJECT:ControlWinLg:VisaExcel:valueDataOut("J2","Låst").
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("A1:A2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("B1:B2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("C1:C2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("J1:J2",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("D1:E1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("F1:G1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:IhopCeller("H1:I1",2).
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A2","J2","4:7").
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
        
        query2var =  "FOR EACH " + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:TABLE + " WHERE ekgmtrlTT.EKGSUBID = " + STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf1TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE + " BY EKGSUBID BY ARTNR ").
        q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh,query2var).
        q2H:GET-FIRST().
        DO WHILE q2H:QUERY-OFF-END = FALSE:
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE)).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().    
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           
           THIS-OBJECT:JmfKLGDataKoderOut(listaP).
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("LASTA"):BUFFER-VALUE,"Ja/Nej") ).
          
           THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:FIND-FIRST("WHERE EKGSUBID = " +  
           STRING(THIS-OBJECT:ControlDataDb:ekgsubkatalogerJmf2TTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE) + 
           " AND ARTNR = '"  + THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ARTNR"):BUFFER-VALUE  + "'") NO-ERROR.
           
           THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "E".
           THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
           THIS-OBJECT:JmfKLGDataKoderOut(listaP).
           THIS-OBJECT:ControlWinLg:VisaExcel:Rowdown().
           q2H:GET-NEXT().         
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:cColname = "A".
        THIS-OBJECT:ControlWinLg:VisaExcel:ColumnRad().
        REPEAT:
           THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
           IF THIS-OBJECT:ControlWinLg:VisaExcel:cColName = "K" THEN LEAVE.
           THIS-OBJECT:ControlWinLg:VisaExcel:BordVert(THIS-OBJECT:ControlWinLg:VisaExcel:cColName + "1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"2:7").
        END.
        THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"J" + STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"4:7").
     END. 
     
     
     THIS-OBJECT:ControlWinLg:VisaExcel:BordVert("A1","A"+ STRING(THIS-OBJECT:ControlWinLg:VisaExcel:iRad - 1),"1:7").
     THIS-OBJECT:ControlWinLg:VisaExcel:ColLeft().
     THIS-OBJECT:ControlWinLg:VisaExcel:BordHoriz("A1",THIS-OBJECT:ControlWinLg:VisaExcel:cColName + STRING(1),"3:7").
  END METHOD. 
  
  METHOD PRIVATE VOID JmfKLGDataKoderOut( INPUT listaP AS CHARACTER ):
     IF THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:AVAILABLE THEN.
     ELSE RETURN.
     IF listaP = "P1" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
              
     END.   
     IF listaP = "P2" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ARBETEKOST"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MATERIELKOST"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MASKINKOST"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("OVRIGT"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE)).
     END.
     IF listaP = "P3" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BERTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MONTTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("MASKTIM"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("EA"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("ANTALMAN"):BUFFER-VALUE)).
                       
     END.
    
     IF listaP = "M" THEN DO:
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("BASPRIS"):BUFFER-VALUE)).
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:ColRight().
        THIS-OBJECT:ControlWinLg:VisaExcel:DataOut(STRING(THIS-OBJECT:ControlDataDb:ekgLopnrJmfTTh:BUFFER-FIELD("KALKYLPRIS"):BUFFER-VALUE)).        
     END.
  END METHOD.  
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID jamforP1ToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:jamforKoll("") = TRUE THEN THIS-OBJECT:JmfKLGData("P1").
		ELSE RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID jamforP2ToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:jamforKoll("") = TRUE THEN THIS-OBJECT:JmfKLGData("P2").
      ELSE RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID jamforP3ToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:jamforKoll("") = TRUE THEN THIS-OBJECT:JmfKLGData("P3").
      ELSE RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID jamforMtrlToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:jamforKoll("M") = TRUE THEN THIS-OBJECT:JmfKLGData("M").
      ELSE RETURN.

	END METHOD.

END CLASS.