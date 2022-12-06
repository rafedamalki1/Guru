 
 /*------------------------------------------------------------------------
    File        : KalkMtrlControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 16:16:55 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkMtrlControl INHERITS UserControl: 
   DEFINE PUBLIC VARIABLE ButtonKopBerStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
   DEFINE PUBLIC VARIABLE ButtonImportMtrlStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ButtonBerMtrlStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerMtrlValj AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerMtrlEget AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerMtrl AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerEgetMtrlBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE MtrlBerStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE MatButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE MatButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE GridMateriel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridEgetMateriel AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO. 
   DEFINE         VARIABLE DGMat                        AS Controls.Subclasses.DragDropGroup                              NO-UNDO. 		
	CONSTRUCTOR PUBLIC KalkMtrlControl ( INPUT r AS Guru.Root ):
		
		
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

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopBerStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Root:WindowManager:Visa(FALSE).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningKop().
      THIS-OBJECT:Root:WindowManager:Visa(TRUE).

	END METHOD.

   METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:ForetagAnpass().
     
   END METHOD.
   METHOD PUBLIC VOID ForetagAnpass():
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
        /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
         THIS-OBJECT:ButtonKopBerStrip:VISIBLE = TRUE.
         THIS-OBJECT:ButtonBerMtrlStrip:VISIBLE = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:ButtonKopBerStrip:VISIBLE = FALSE.
         THIS-OBJECT:ButtonBerMtrlStrip:VISIBLE = FALSE.
         THIS-OBJECT:splitContainerMtrlEget:SplitterDistance = 500.
            
      END.
   END METHOD.
   
   
   METHOD PUBLIC VOID CreateGrids():
      /* Materiel */
      THIS-OBJECT:CreateGridMateriel().
      THIS-OBJECT:CreateGridEgetMateriel().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridFunctions():
      
      /* Drag dropp*/
   
      THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridMateriel).
      THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridEgetMateriel).
      THIS-OBJECT:MatButtonOverMark:GuruKoppla(THIS-OBJECT:GridMateriel,THIS-OBJECT:GridEgetMateriel).
      THIS-OBJECT:MatButtonOverMark:GuruDublett = FALSE.
      THIS-OBJECT:MatButtonBackMark:GuruKoppla(THIS-OBJECT:GridEgetMateriel, THIS-OBJECT:GridMateriel).
      THIS-OBJECT:MatButtonBackMark:GuruRemoveFrom = TRUE.
      
   END METHOD.
   /* Materiel */
   METHOD PUBLIC VOID CreateGridMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      /*
      rubrikergrid[6] = NEW Controls.GridRubrik("KUND","", TRUE ,50 ).
      */
      THIS-OBJECT:GridMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT Guru.GlobalaVariabler:MaterielTTh)).
      
      THIS-OBJECT:GridMateriel:Text = "Materiel".
      IF Guru.Konstanter:globnetprissortvar = 1 THEN THIS-OBJECT:GridMateriel:GuruOrderbyDefault("BY kund DESCENDING by enr").
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         THIS-OBJECT:GridMateriel:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridMateriel:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
         /* /*OBSE CASESENSITIVE*/ 
         THIS-OBJECT:GridMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         */
      END. 
      THIS-OBJECT:GridMateriel:VisaViaInternetMenuItemAddAdd().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridEgetMateriel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("MATRIS", "Matris", FALSE ,50,"INT","INT").
      rubrikergrid[2] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,250).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET", "Enhet", TRUE ,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("BERKVANT", "Antal", FALSE,50,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("NPRIS", "Nettopris", FALSE ,100,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
      
      THIS-OBJECT:GridEgetMateriel:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:KalkdbControl:kalktmtrlTTh)).
      THIS-OBJECT:GridEgetMateriel:Text = "Materielspecifikation".
      IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN DO:
         THIS-OBJECT:GridEgetMateriel:DisplayLayout:Bands[0]:Columns["NPRIS"]:Hidden = TRUE.         
      END.
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
         /*
         THIS-OBJECT:GridEgetMateriel:GuruGridFarg(TRUE).
         THIS-OBJECT:GridEgetMateriel:GuruAddConditionalColor("KUND", Elpool.Helpers:CT_NOTEQUALS, "False", Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28])).
         */
         THIS-OBJECT:GridEgetMateriel:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridEgetMateriel:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
      END.  
      THIS-OBJECT:GridEgetMateriel:VisaViaInternetMenuItemAddAdd().
            
   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridMateriel_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		THIS-OBJECT:MatButtonOverMark_Click(?,?).
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
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkMtrlControl").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
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
      THIS-OBJECT:splitContainerMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerMtrlValj = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMateriel = NEW Controls.Grid().
      THIS-OBJECT:MatButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:MatButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerMtrlEget = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridEgetMateriel = NEW Controls.Grid().
      THIS-OBJECT:splitContainerEgetMtrlBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:MtrlBerStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonBerMtrlStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopBerStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonImportMtrlStrip = NEW System.Windows.Forms.ToolStripButton().
      CAST(THIS-OBJECT:splitContainerMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrl:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMtrlValj, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrlValj:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlValj:SuspendLayout().
      CAST(THIS-OBJECT:GridMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerMtrlEget, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrlEget:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlEget:SuspendLayout().
      CAST(THIS-OBJECT:GridEgetMateriel, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerEgetMtrlBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerEgetMtrlBild:SuspendLayout().
      THIS-OBJECT:MtrlBerStrip:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerMtrl */
      /*  */
      THIS-OBJECT:splitContainerMtrl:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrl:Name = "splitContainerMtrl".
      /*  */
      /* splitContainerMtrl.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel1:Controls:Add(THIS-OBJECT:splitContainerMtrlValj).
      /*  */
      /* splitContainerMtrl.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrl:Panel2:Controls:Add(THIS-OBJECT:splitContainerMtrlEget).
      THIS-OBJECT:splitContainerMtrl:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerMtrl:SplitterDistance = 465.
      THIS-OBJECT:splitContainerMtrl:TabIndex = 20.
      /*  */
      /* splitContainerMtrlValj */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlValj:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerMtrlValj:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlValj:Name = "splitContainerMtrlValj".
      /*  */
      /* splitContainerMtrlValj.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel1:Controls:Add(THIS-OBJECT:GridMateriel).
      /*  */
      /* splitContainerMtrlValj.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonOverMark).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:Controls:Add(THIS-OBJECT:MatButtonBackMark).
      THIS-OBJECT:splitContainerMtrlValj:Size = NEW System.Drawing.Size(463, 544).
      THIS-OBJECT:splitContainerMtrlValj:SplitterDistance = 426.
      THIS-OBJECT:splitContainerMtrlValj:TabIndex = 1.
      /*  */
      /* GridMateriel */
      /*  */
      THIS-OBJECT:GridMateriel:AllowDrop = TRUE.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMateriel:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMateriel:GuruContext = ?.
      THIS-OBJECT:GridMateriel:GuruDraggable = TRUE.
      THIS-OBJECT:GridMateriel:GuruDroppable = TRUE.
      THIS-OBJECT:GridMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMateriel:Name = "GridMateriel".
      THIS-OBJECT:GridMateriel:Size = NEW System.Drawing.Size(426, 544).
      THIS-OBJECT:GridMateriel:TabIndex = 0.
      THIS-OBJECT:GridMateriel:Text = "grid1".
      THIS-OBJECT:GridMateriel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridMateriel_GuruAfterDropped).
      THIS-OBJECT:GridMateriel:DoubleClickRow:Subscribe(THIS-OBJECT:GridMateriel_DoubleClickRow).
      /*  */
      /* MatButtonOverMark */
      /*  */
      appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
      THIS-OBJECT:MatButtonOverMark:Appearance = appearance27.
      THIS-OBJECT:MatButtonOverMark:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:MatButtonOverMark:Name = "MatButtonOverMark".
      THIS-OBJECT:MatButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:MatButtonOverMark:TabIndex = 10.
      THIS-OBJECT:MatButtonOverMark:Click:Subscribe(THIS-OBJECT:MatButtonOverMark_Click).
      /*  */
      /* MatButtonBackMark */
      /*  */
      appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
      THIS-OBJECT:MatButtonBackMark:Appearance = appearance28.
      THIS-OBJECT:MatButtonBackMark:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:MatButtonBackMark:Name = "MatButtonBackMark".
      THIS-OBJECT:MatButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:MatButtonBackMark:TabIndex = 11.
      THIS-OBJECT:MatButtonBackMark:Click:Subscribe(THIS-OBJECT:MatButtonBackMark_Click).
      /*  */
      /* splitContainerMtrlEget */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlEget:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlEget:Name = "splitContainerMtrlEget".
      /*  */
      /* splitContainerMtrlEget.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Panel1:Controls:Add(THIS-OBJECT:GridEgetMateriel).
      /*  */
      /* splitContainerMtrlEget.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlEget:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerMtrlEget.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerMtrlEget:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerMtrlEget:Panel2:Controls:Add(THIS-OBJECT:splitContainerEgetMtrlBild).
      THIS-OBJECT:splitContainerMtrlEget:Size = NEW System.Drawing.Size(640, 544).
      THIS-OBJECT:splitContainerMtrlEget:SplitterDistance = 351.
      THIS-OBJECT:splitContainerMtrlEget:TabIndex = 3.
      /*  */
      /* GridEgetMateriel */
      /*  */
      THIS-OBJECT:GridEgetMateriel:AllowDrop = TRUE.
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgetMateriel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgetMateriel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgetMateriel:GuruContext = ?.
      THIS-OBJECT:GridEgetMateriel:GuruDraggable = TRUE.
      THIS-OBJECT:GridEgetMateriel:GuruDroppable = TRUE.
      THIS-OBJECT:GridEgetMateriel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgetMateriel:Name = "GridEgetMateriel".
      THIS-OBJECT:GridEgetMateriel:Size = NEW System.Drawing.Size(351, 544).
      THIS-OBJECT:GridEgetMateriel:TabIndex = 1.
      THIS-OBJECT:GridEgetMateriel:Text = "grid1".
      THIS-OBJECT:GridEgetMateriel:GuruAfterDropped:Subscribe(THIS-OBJECT:GridEgetMateriel_GuruAfterDropped).
      THIS-OBJECT:GridEgetMateriel:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgetMateriel_AfterCellUpdate).
      /*  */
      /* splitContainerEgetMtrlBild */
      /*  */
      THIS-OBJECT:splitContainerEgetMtrlBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerEgetMtrlBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerEgetMtrlBild:Name = "splitContainerEgetMtrlBild".
      THIS-OBJECT:splitContainerEgetMtrlBild:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerEgetMtrlBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel1:Controls:Add(THIS-OBJECT:MtrlBerStrip).
      /*  */
      /* splitContainerEgetMtrlBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerEgetMtrlBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerEgetMtrlBild:Size = NEW System.Drawing.Size(285, 544).
      THIS-OBJECT:splitContainerEgetMtrlBild:SplitterDistance = 150.
      THIS-OBJECT:splitContainerEgetMtrlBild:TabIndex = 4.
      /*  */
      /* MtrlBerStrip */
      /*  */
      THIS-OBJECT:MtrlBerStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:MtrlBerStrip:ImageScalingSize = NEW System.Drawing.Size(32, 32).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonBerMtrlStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonKopBerStrip.
      arrayvar0[3] = THIS-OBJECT:ButtonImportMtrlStrip.
      THIS-OBJECT:MtrlBerStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:MtrlBerStrip:LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle:VerticalStackWithOverflow.
      THIS-OBJECT:MtrlBerStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:MtrlBerStrip:Name = "MtrlBerStrip".
      THIS-OBJECT:MtrlBerStrip:Size = NEW System.Drawing.Size(285, 150).
      THIS-OBJECT:MtrlBerStrip:TabIndex = 3.
      THIS-OBJECT:MtrlBerStrip:Text = "toolStrip1".
      /*  */
      /* ButtonBerMtrlStrip */
      /*  */
      THIS-OBJECT:ButtonBerMtrlStrip:Image = CAST(resources:GetObject("ButtonBerMtrlStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBerMtrlStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBerMtrlStrip:Name = "ButtonBerMtrlStrip".
      THIS-OBJECT:ButtonBerMtrlStrip:Size = NEW System.Drawing.Size(283, 36).
      THIS-OBJECT:ButtonBerMtrlStrip:Text = "Berednings Materiel         ".
      THIS-OBJECT:ButtonBerMtrlStrip:Click:Subscribe(THIS-OBJECT:ButtonBerMtrlStrip_Click).
      /*  */
      /* ButtonKopBerStrip */
      /*  */
      THIS-OBJECT:ButtonKopBerStrip:Image = CAST(resources:GetObject("ButtonKopBerStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopBerStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopBerStrip:Name = "ButtonKopBerStrip".
      THIS-OBJECT:ButtonKopBerStrip:Size = NEW System.Drawing.Size(283, 36).
      THIS-OBJECT:ButtonKopBerStrip:Text = "Kopiera Beredning           ".
      THIS-OBJECT:ButtonKopBerStrip:Click:Subscribe(THIS-OBJECT:ButtonKopBerStrip_Click).
      /*  */
      /* ButtonImportMtrlStrip */
      /*  */
      THIS-OBJECT:ButtonImportMtrlStrip:Image = CAST(resources:GetObject("ButtonImportMtrlStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonImportMtrlStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonImportMtrlStrip:Name = "ButtonImportMtrlStrip".
      THIS-OBJECT:ButtonImportMtrlStrip:Size = NEW System.Drawing.Size(283, 36).
      THIS-OBJECT:ButtonImportMtrlStrip:Text = "Import Materiel                 ".
      THIS-OBJECT:ButtonImportMtrlStrip:ToolTipText = "Enr i första kolumn, antal i andra kolumn".
      THIS-OBJECT:ButtonImportMtrlStrip:Click:Subscribe(THIS-OBJECT:ButtonImportMtrlStrip_Click).
      /*  */
      /* KalkMtrlControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerMtrl).
      THIS-OBJECT:Name = "KalkMtrlControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrl:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlValj:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrlValj, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlValj:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlEget:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMtrlEget:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrlEget, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlEget:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridEgetMateriel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerEgetMtrlBild:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerEgetMtrlBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerEgetMtrlBild:ResumeLayout(FALSE).
      THIS-OBJECT:MtrlBerStrip:ResumeLayout(FALSE).
      THIS-OBJECT:MtrlBerStrip:PerformLayout().
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
   METHOD PRIVATE VOID GridMateriel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridEgetMateriel THEN 
      DO:
         THIS-OBJECT:MatButtonBackMark_Click(?,?).
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonOverMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:MatButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:MatButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreatevalMaterial(THIS-OBJECT:ControlShell:KalkNrvar,THIS-OBJECT:ControlShell:Omradevar,1).
         THIS-OBJECT:GridEgetMateriel:GuruTom().
         THIS-OBJECT:MatButtonOverMark:GuruOverefteruppdat().
      END.
      ELSE DO:
         DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
            
   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID MatButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridEgetMateriel:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:MatButtonBackMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgetMateriel_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMateriel THEN DO:
         THIS-OBJECT:MatButtonOverMark_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgetMateriel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridEgetMateriel:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF kolnamn = "MATRIS" AND kolvarde=  "0" THEN DO:
         kolvarde = "1".
         THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:GridEgetMateriel:GuruReopen().
      END. 
      ELSE THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().  
     
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonBerMtrlStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:Root:WindowManager:Visa(FALSE).
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrl().
      THIS-OBJECT:Root:WindowManager:Visa(TRUE).
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID ButtonImportMtrlStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE qH    AS HANDLE                         NO-UNDO.
      DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
      DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Global:EmptyTT(Guru.GlobalaVariabler:EMaterielTTh).
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den Excelfil som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 1.
      Guru.Konstanter:AmericanSet().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ReadExcelStart(INPUT fildir).   
      DO WHILE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRadslut:
         invar = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:GetData().
         IF THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
            IF invar = "" THEN.
            ELSE DO:
               Guru.GlobalaVariabler:EMaterielTTh:BUFFER-CREATE().
               Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = Guru.GlobalaVariabler:EMaterielTTh:RECID.
               Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = TRIM(invar).
            END.
         END.    
         IF THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:
            IF invar = "" OR invar = ? THEN invar = "1".
            Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE = TRIM(invar).
           
         END.         
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight().
         IF THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "C" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".                 
         IF THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:Rowdown().
      END.  
     
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:GridMateriel:GuruTom().
      
        
      
      
      THIS-OBJECT:KalkdbControl:tiduth:EMPTY-TEMP-TABLE() NO-ERROR.     
      IF STRING(THIS-OBJECT:ControlShell:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:ControlShell:RibbonToolLev:SelectedIndex]:DataVALUE) = "Alla leverantörer" THEN lev = "".
      ELSE lev = STRING( CAST(THIS-OBJECT:ControlShell:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:ControlShell:RibbonToolLev:SelectedIndex]:DataVALUE, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellVALUE("LEVKOD").*/
      THIS-OBJECT:Root:DatabaseManager:Global:FetchMateriel(lev).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:EMaterielTTh,"").
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         Guru.GlobalaVariabler:MaterielTTh:FIND-FIRST("WHERE mtrltemp.ENR = '" + Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE + "'", NO-LOCK) NO-ERROR.
         IF Guru.GlobalaVariabler:MaterielTTh:AVAILABLE THEN DO:
            Guru.GlobalaVariabler:MaterielTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE =  Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE.
         END.
         ELSE DO:
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(159) + " " + Guru.GlobalaVariabler:EMaterielTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
         END.   
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:GridMateriel:GuruReopen().
      Guru.GlobalaVariabler:MaterielTTh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:MaterielTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridMateriel:MarkAll().
         THIS-OBJECT:MatButtonOverMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreatevalMaterial(THIS-OBJECT:ControlShell:KalkNrvar,THIS-OBJECT:ControlShell:Omradevar,1).
         THIS-OBJECT:GridEgetMateriel:GuruTom().
         THIS-OBJECT:MatButtonOverMark:GuruOverefteruppdat().
         THIS-OBJECT:GridMateriel:GuruAvmarkera().
         THIS-OBJECT:GridEgetMateriel:GuruAvmarkera().
      END.
      THIS-OBJECT:GridMateriel:GuruTom().
      THIS-OBJECT:GridEgetMateriel:GuruFirstrow().
      
      THIS-OBJECT:ControlShell:FelMeddCheck(). 
   END METHOD.

		DESTRUCTOR PUBLIC KalkMtrlControl ( ):
     /* {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.