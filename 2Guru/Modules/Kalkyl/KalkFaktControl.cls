 
 /*------------------------------------------------------------------------
    File        : KalkFaktControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 15:56:32 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS  Modules.Kalkyl.KalkFaktControl INHERITS UserControl: 
	DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerFaktorerPriser AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerFaktPrisBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridPriser AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFaktorer AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
		
	CONSTRUCTOR PUBLIC KalkFaktControl ( INPUT r AS Guru.Root  ):
		
		
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

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkFaktControl").
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
      THIS-OBJECT:splitContainerFaktPrisBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerFaktorerPriser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFaktorer = NEW Controls.Grid().
      THIS-OBJECT:GridPriser = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerFaktPrisBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFaktPrisBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerFaktorerPriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFaktorerPriser:SuspendLayout().
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerFaktPrisBild */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFaktPrisBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFaktPrisBild:Name = "splitContainerFaktPrisBild".
      /*  */
      /* splitContainerFaktPrisBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerFaktorerPriser).
      /*  */
      /* splitContainerFaktPrisBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFaktPrisBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerFaktPrisBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerFaktPrisBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerFaktPrisBild:Size = NEW System.Drawing.Size(1111, 545).
      THIS-OBJECT:splitContainerFaktPrisBild:SplitterDistance = 776.
      THIS-OBJECT:splitContainerFaktPrisBild:TabIndex = 4.
      /*  */
      /* splitContainerFaktorerPriser */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFaktorerPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFaktorerPriser:Name = "splitContainerFaktorerPriser".
      /*  */
      /* splitContainerFaktorerPriser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:Controls:Add(THIS-OBJECT:GridFaktorer).
      /*  */
      /* splitContainerFaktorerPriser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:Controls:Add(THIS-OBJECT:GridPriser).
      THIS-OBJECT:splitContainerFaktorerPriser:Size = NEW System.Drawing.Size(776, 545).
      THIS-OBJECT:splitContainerFaktorerPriser:SplitterDistance = 404.
      THIS-OBJECT:splitContainerFaktorerPriser:TabIndex = 2.
      /*  */
      /* GridFaktorer */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFaktorer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFaktorer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFaktorer:GuruContext = ?.
      THIS-OBJECT:GridFaktorer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFaktorer:Name = "GridFaktorer".
      THIS-OBJECT:GridFaktorer:Size = NEW System.Drawing.Size(404, 545).
      THIS-OBJECT:GridFaktorer:TabIndex = 0.
      THIS-OBJECT:GridFaktorer:Text = "grid1".
      THIS-OBJECT:GridFaktorer:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFaktorer_AfterCellUpdate).
      THIS-OBJECT:GridFaktorer:InitializeLayout:Subscribe(THIS-OBJECT:GridFaktorer_InitializeLayout).
      /*  */
      /* GridPriser */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPriser:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridPriser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPriser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPriser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridPriser:GuruContext = ?.
      THIS-OBJECT:GridPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPriser:Name = "GridPriser".
      THIS-OBJECT:GridPriser:Size = NEW System.Drawing.Size(368, 545).
      THIS-OBJECT:GridPriser:TabIndex = 1.
      THIS-OBJECT:GridPriser:Text = "grid1".
      THIS-OBJECT:GridPriser:AfterCellUpdate:Subscribe(THIS-OBJECT:GridPriser_AfterCellUpdate).
      THIS-OBJECT:GridPriser:InitializeLayout:Subscribe(THIS-OBJECT:GridPriser_InitializeLayout).
      /*  */
      /* KalkFaktControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerFaktPrisBild).
      THIS-OBJECT:Name = "KalkFaktControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 545).
      THIS-OBJECT:splitContainerFaktPrisBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFaktPrisBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFaktPrisBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFaktorerPriser:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFaktorerPriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFaktorerPriser:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   
    METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      /* fakt */
      THIS-OBJECT:CreateGridFaktorer().
      THIS-OBJECT:CreateGridEgnaPriser().
   END METHOD.
   METHOD PUBLIC VOID CreateGridFaktorer():
      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("FAKTOR","Faktor",FALSE,75,"DEC", "KALKYL").
      THIS-OBJECT:GridFaktorer:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KalkdbControl:FaktorerTTh)).  
      THIS-OBJECT:GridFaktorer:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridFaktorer:Text = "Faktorer".
      THIS-OBJECT:GridFaktorer:ExtramenuItemAdd("Alla ska ha samma faktor").
      THIS-OBJECT:GridFaktorer:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:SammaFaktor).
   END METHOD.
   METHOD PUBLIC VOID SammaFaktor(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      kolnamn = "FAKTOR".
      kolvarde = THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE().
      
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:FaktorerTTh:TABLE + " WHERE " + THIS-OBJECT:GridFaktorer:Guruegenskap:filter + " NO-LOCK".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:FaktorerTTh,queryvar).
      qH:GET-FIRST().   
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:GridFaktorer:GuruReOpen().
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgnaPriser():
      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS","Pris",FALSE,75,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("PROCENT","Procentpåslag",FALSE,85,"DEC", "DEC0"). /*enbart hjälp vid räkning*/
     
      THIS-OBJECT:GridPriser:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:KalkdbControl:EgnaPriserTTh)).
      THIS-OBJECT:GridPriser:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridPriser:Text = "Egna priser".
      THIS-OBJECT:GridPriser:GuruLastrow().
      THIS-OBJECT:GridPriser:GuruFirstrow().
      THIS-OBJECT:GridPriser:ExtramenuItemAdd("Alla ska ha samma Procentpåslag").
      THIS-OBJECT:GridPriser:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:SammaPaslag).
   END METHOD.
    METHOD PUBLIC VOID SammaPaslag(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      kolnamn = "PROCENT".
      kolvarde = THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE().
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:EgnaPriserTTh:TABLE + " WHERE " + " NO-LOCK".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:EgnaPriserTTh,queryvar).
      qH:GET-FIRST().   
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Beraknaprisproc(INPUT kolnamn, INPUT kolvarde).
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:GridPriser:GuruReOpen().
      
   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFaktorer_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().

      IF THIS-OBJECT:GridFaktorer:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.  
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
      
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFaktorer_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
      
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridPriser_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      /*Robin Sjöberg Elpool i Umeå AB  4 okt 2016 14:43:16 
            Här finns ingen THIS-OBJECT:KalkdbControl:EgnaPriserTTh:AVAILABLE 
      */

      IF THIS-OBJECT:GridPriser:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Beraknaprisproc(INPUT kolnamn, INPUT kolvarde).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SavePrisFakt().
      
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridPriser_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
      
   END METHOD.
   
	DESTRUCTOR PUBLIC KalkFaktControl ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.