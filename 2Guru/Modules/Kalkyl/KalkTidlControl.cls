 
 /*------------------------------------------------------------------------
    File        : KalkTidlControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 18:49:35 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkTidlControl INHERITS UserControl: 
	
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.

   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerTidlage AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridTidlage AS Controls.Grid NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkTidlControl ( INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
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
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkTidlControl").
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
      THIS-OBJECT:splitContainerTidlage = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridTidlage = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerTidlage, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerTidlage:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerTidlage:SuspendLayout().
      CAST(THIS-OBJECT:GridTidlage, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerTidlage */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerTidlage:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerTidlage:Name = "splitContainerTidlage".
      /*  */
      /* splitContainerTidlage.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Panel1:Controls:Add(THIS-OBJECT:GridTidlage).
      /*  */
      /* splitContainerTidlage.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerTidlage.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerTidlage:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerTidlage:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerTidlage:SplitterDistance = 740.
      THIS-OBJECT:splitContainerTidlage:TabIndex = 1.
      /*  */
      /* GridTidlage */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridTidlage:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridTidlage:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridTidlage:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridTidlage:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridTidlage:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridTidlage:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridTidlage:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridTidlage:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridTidlage:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridTidlage:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridTidlage:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridTidlage:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridTidlage:GuruContext = ?.
      THIS-OBJECT:GridTidlage:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridTidlage:Name = "GridTidlage".
      THIS-OBJECT:GridTidlage:Size = NEW System.Drawing.Size(740, 546).
      THIS-OBJECT:GridTidlage:TabIndex = 0.
      THIS-OBJECT:GridTidlage:Text = "grid1".
      THIS-OBJECT:GridTidlage:AfterCellUpdate:Subscribe(THIS-OBJECT:GridTidlage_AfterCellUpdate).
      /*  */
      /* KalkTidlControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerTidlage).
      THIS-OBJECT:Name = "KalkTidlControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerTidlage:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerTidlage, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerTidlage:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridTidlage, System.ComponentModel.ISupportInitialize):EndInit().
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
   METHOD PRIVATE VOID GridTidlage_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE datvar AS DATE NO-UNDO.
      DEFINE VARIABLE tidvar AS INTEGER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridTidlage:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      IF THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE NE kolvarde THEN DO:
         THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD("ANVANDARE1"):BUFFER-VALUE = Guru.Konstanter:globanv.
         Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
         IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
            ASSIGN
            THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD("NAMNANVANDARE1"):BUFFER-VALUE = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
         END.
         IF kolnamn = "DATUMTID" THEN DO:
            Helpers.Functions:GetDateTime(INPUT THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE, OUTPUT datvar, OUTPUT tidvar).
            IF tidvar = 0 THEN DO:
               THIS-OBJECT:KalkdbControl:kalkttidlageTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = DATETIME(datvar, MTIME ).
            END.   
         END.   
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().       
      END.
     
      RETURN.

   END METHOD.
	DESTRUCTOR PUBLIC KalkTidlControl ( ):
     /* {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.