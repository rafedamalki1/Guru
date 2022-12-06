 
 /*------------------------------------------------------------------------
    File        : EkgResursControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Mon Oct 14 10:01:22 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgResursControl INHERITS UserControl: 
  {EKGTH.I}
   DEFINE PUBLIC VARIABLE resurserMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resurserMainSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurs AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE ResursertoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE SparaResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortResursButton AS System.Windows.Forms.ToolStripButton NO-UNDO.

 CONSTRUCTOR PUBLIC EkgResursControl (INPUT r AS Guru.Root):
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
      THIS-OBJECT:CreateGridResurs().      
   END METHOD.
   METHOD PUBLIC VOID CreateGridResurs():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",FALSE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",FALSE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("AKTIV","",FALSE,70, "LOG").
      rubrikergrid[5] = NEW Controls.GridRubrik("ANMARKNING","",FALSE,250).
      
      THIS-OBJECT:GridResurs:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ekgresursTTh)).
      THIS-OBJECT:GridResurs:Text = "Resurser".
      
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurs_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.           
      RETURN.
   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyResursPop:Show().     
      THIS-OBJECT:ControlShell:fillinResursNr:GuruText:Select().
      /*THIS-OBJECT:fillinResursBenamning:GuruText:Select().*/
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SparaResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlWinLg:SparaResurs().
		RETURN.

	END METHOD.

   


   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgResursControl").
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
      THIS-OBJECT:resurserMainSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurserMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ResursertoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:SparaResursButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurs = NEW Controls.Grid().
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMainSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMainSC:SuspendLayout().
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurserMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurserMenuSC:SuspendLayout().
      THIS-OBJECT:ResursertoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* resurserMainSC */
      /*  */
      THIS-OBJECT:resurserMainSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMainSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMainSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMainSC:Name = "resurserMainSC".
      /*  */
      /* resurserMainSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMainSC:Panel1:Controls:Add(THIS-OBJECT:resurserMenuSC).
      THIS-OBJECT:resurserMainSC:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:resurserMainSC:SplitterDistance = 897.
      THIS-OBJECT:resurserMainSC:TabIndex = 1.
      /*  */
      /* resurserMenuSC */
      /*  */
      THIS-OBJECT:resurserMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurserMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurserMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurserMenuSC:Name = "resurserMenuSC".
      THIS-OBJECT:resurserMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurserMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel1:Controls:Add(THIS-OBJECT:ResursertoolStrip).
      /*  */
      /* resurserMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurserMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResurs).
      THIS-OBJECT:resurserMenuSC:Size = NEW System.Drawing.Size(897, 553).
      THIS-OBJECT:resurserMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurserMenuSC:TabIndex = 3.
      /*  */
      /* ResursertoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillResursButton.
      arrayvar0[2] = THIS-OBJECT:TaBortResursButton.
      arrayvar0[3] = THIS-OBJECT:SparaResursButton.
      THIS-OBJECT:ResursertoolStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:ResursertoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ResursertoolStrip:Name = "ResursertoolStrip".
      THIS-OBJECT:ResursertoolStrip:Size = NEW System.Drawing.Size(897, 25).
      THIS-OBJECT:ResursertoolStrip:TabIndex = 2.
      THIS-OBJECT:ResursertoolStrip:Text = "ResursertoolStrip".
      /*  */
      /* laggTillResursButton */
      /*  */
      THIS-OBJECT:laggTillResursButton:Image = CAST(resources:GetObject("laggTillResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillResursButton:Name = "laggTillResursButton".
      THIS-OBJECT:laggTillResursButton:Size = NEW System.Drawing.Size(103, 22).
      THIS-OBJECT:laggTillResursButton:Text = "Lägg till resurs".
      THIS-OBJECT:laggTillResursButton:Click:Subscribe(THIS-OBJECT:laggTillResursButton_Click).
      /*  */
      /* TaBortResursButton */
      /*  */
      THIS-OBJECT:TaBortResursButton:Image = CAST(resources:GetObject("TaBortResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortResursButton:Name = "TaBortResursButton".
      THIS-OBJECT:TaBortResursButton:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:TaBortResursButton:Text = "Ta bort resurs".
      THIS-OBJECT:TaBortResursButton:Click:Subscribe(THIS-OBJECT:TaBortResursButton_Click).
      /*  */
      /* SparaResursButton */
      /*  */
      THIS-OBJECT:SparaResursButton:Image = CAST(resources:GetObject("SparaResursButton.Image"), System.Drawing.Image).
      THIS-OBJECT:SparaResursButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:SparaResursButton:Name = "SparaResursButton".
      THIS-OBJECT:SparaResursButton:Size = NEW System.Drawing.Size(125, 22).
      THIS-OBJECT:SparaResursButton:Text = "Spara förändringar".
      THIS-OBJECT:SparaResursButton:Click:Subscribe(THIS-OBJECT:SparaResursButton_Click).
      /*  */
      /* gridResurs */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurs:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResurs:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurs:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurs:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurs:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurs:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurs:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurs:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurs:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurs:GuruContext = ?.
      THIS-OBJECT:gridResurs:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurs:Name = "gridResurs".
      THIS-OBJECT:gridResurs:Size = NEW System.Drawing.Size(897, 524).
      THIS-OBJECT:gridResurs:TabIndex = 0.
      THIS-OBJECT:gridResurs:Text = "grid1".
      THIS-OBJECT:gridResurs:AfterCellUpdate:Subscribe(THIS-OBJECT:gridresurs_AfterCellUpdate).
      /*  */
      /* EkgResursControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:resurserMainSC).
      THIS-OBJECT:Name = "EkgResursControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:resurserMainSC:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMainSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMainSC:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurserMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurserMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurserMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurserMenuSC:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:ResursertoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurs, System.ComponentModel.ISupportInitialize):EndInit().
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
   METHOD PRIVATE VOID TaBortResursButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:ControlDataDb:TaBortResurs(INTEGER(THIS-OBJECT:GridResurs:ActiveRow:Cells["RESURSNR"]:Text)).
      THIS-OBJECT:GridResurs:GuruFiltrera().
      RETURN.

   END METHOD.
   
END CLASS.