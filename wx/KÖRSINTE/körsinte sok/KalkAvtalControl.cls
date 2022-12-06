
 
 /*------------------------------------------------------------------------
    File        : KalkAvtakControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 18:13:50 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkAvtalControl INHERITS UserControl: 
	
   DEFINE PUBLIC VARIABLE KalkdbControl                AS Modules.Kalkyl.Kalkyldb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	
   DEFINE PUBLIC VARIABLE ComboAvtal AS Controls.Combo NO-UNDO.
   DEFINE PUBLIC VARIABLE AvtalText2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE AvtalText AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripVisa AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonGenereraAvtal AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAvtalMeny AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE ultraAvtal AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerAvtal AS System.Windows.Forms.SplitContainer NO-UNDO.

		
	CONSTRUCTOR PUBLIC KalkAvtalControl ( INPUT r AS Guru.Root ):
		
		
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
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkAvtalControl").
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
      THIS-OBJECT:splitContainerAvtal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:AvtalText2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ComboAvtal = NEW Controls.Combo().
      THIS-OBJECT:AvtalText = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ultraAvtal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:splitContainerAvtalMeny = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripVisa = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonGenereraAvtal = NEW System.Windows.Forms.ToolStripButton().
      CAST(THIS-OBJECT:splitContainerAvtal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAvtal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAvtal:SuspendLayout().
      CAST(THIS-OBJECT:AvtalText2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ComboAvtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:AvtalText, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAvtalMeny, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAvtalMeny:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAvtalMeny:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAvtalMeny:SuspendLayout().
      THIS-OBJECT:toolStripVisa:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerAvtal */
      /*  */
      THIS-OBJECT:splitContainerAvtal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAvtal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAvtal:Name = "splitContainerAvtal".
      /*  */
      /* splitContainerAvtal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAvtal:Panel1:Controls:Add(THIS-OBJECT:AvtalText2).
      THIS-OBJECT:splitContainerAvtal:Panel1:Controls:Add(THIS-OBJECT:ComboAvtal).
      THIS-OBJECT:splitContainerAvtal:Panel1:Controls:Add(THIS-OBJECT:AvtalText).
      THIS-OBJECT:splitContainerAvtal:Panel1:Controls:Add(THIS-OBJECT:ultraAvtal).
      /*  */
      /* splitContainerAvtal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAvtal:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerAvtal.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerAvtal:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerAvtal:Size = NEW System.Drawing.Size(1111, 517).
      THIS-OBJECT:splitContainerAvtal:SplitterDistance = 800.
      THIS-OBJECT:splitContainerAvtal:TabIndex = 45.
      /*  */
      /* AvtalText2 */
      /*  */
      THIS-OBJECT:AvtalText2:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
      THIS-OBJECT:AvtalText2:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:AvtalText2:Location = NEW System.Drawing.Point(3, 79).
      THIS-OBJECT:AvtalText2:Name = "AvtalText2".
      THIS-OBJECT:AvtalText2:ReadOnly = TRUE.
      THIS-OBJECT:AvtalText2:Size = NEW System.Drawing.Size(601, 29).
      THIS-OBJECT:AvtalText2:TabIndex = 6.
      THIS-OBJECT:AvtalText2:Text = "Mallarna ska ge dig en hjälp att få med alla grundkalkylkoder!".
      /*  */
      /* ComboAvtal */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:ComboAvtal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:ComboAvtal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:ComboAvtal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:ComboAvtal:DisplayLayout:MaxRowScrollRegions = 1.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:ComboAvtal:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:ComboAvtal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:ComboAvtal:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:ComboAvtal:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:ComboAvtal:Location = NEW System.Drawing.Point(90, 149).
      THIS-OBJECT:ComboAvtal:Name = "ComboAvtal".
      THIS-OBJECT:ComboAvtal:Size = NEW System.Drawing.Size(700, 22).
      THIS-OBJECT:ComboAvtal:TabIndex = 42.
      THIS-OBJECT:ComboAvtal:ValueChanged:Subscribe(THIS-OBJECT:ComboAvtal_ValueChanged).
      /*  */
      /* AvtalText */
      /*  */
      THIS-OBJECT:AvtalText:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
      THIS-OBJECT:AvtalText:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:AvtalText:Location = NEW System.Drawing.Point(3, 24).
      THIS-OBJECT:AvtalText:Name = "AvtalText".
      THIS-OBJECT:AvtalText:ReadOnly = TRUE.
      THIS-OBJECT:AvtalText:Size = NEW System.Drawing.Size(601, 29).
      THIS-OBJECT:AvtalText:TabIndex = 5.
      THIS-OBJECT:AvtalText:Text = "Har du tagit del av de kalkylmallar som finns tillgängliga?".
      /*  */
      /* ultraAvtal */
      /*  */
      THIS-OBJECT:ultraAvtal:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:ultraAvtal:Location = NEW System.Drawing.Point(3, 149).
      THIS-OBJECT:ultraAvtal:Name = "ultraAvtal".
      THIS-OBJECT:ultraAvtal:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:ultraAvtal:TabIndex = 43.
      THIS-OBJECT:ultraAvtal:Text = "Kalkylavtal".
      /*  */
      /* splitContainerAvtalMeny */
      /*  */
      THIS-OBJECT:splitContainerAvtalMeny:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAvtalMeny:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerAvtalMeny:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAvtalMeny:Name = "splitContainerAvtalMeny".
      THIS-OBJECT:splitContainerAvtalMeny:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerAvtalMeny.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAvtalMeny:Panel1:Controls:Add(THIS-OBJECT:toolStripVisa).
      /*  */
      /* splitContainerAvtalMeny.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAvtalMeny:Panel2:Controls:Add(THIS-OBJECT:splitContainerAvtal).
      THIS-OBJECT:splitContainerAvtalMeny:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerAvtalMeny:SplitterDistance = 25.
      THIS-OBJECT:splitContainerAvtalMeny:TabIndex = 44.
      /*  */
      /* toolStripVisa */
      /*  */
      THIS-OBJECT:toolStripVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripButtonGenereraAvtal.
      THIS-OBJECT:toolStripVisa:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripVisa:Name = "toolStripVisa".
      THIS-OBJECT:toolStripVisa:Size = NEW System.Drawing.Size(1111, 25).
      THIS-OBJECT:toolStripVisa:TabIndex = 1.
      THIS-OBJECT:toolStripVisa:Text = "toolStrip1".
      /*  */
      /* toolStripButtonGenereraAvtal */
      /*  */
      THIS-OBJECT:toolStripButtonGenereraAvtal:Image = CAST(resources:GetObject("toolStripButtonGenereraAvtal.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonGenereraAvtal:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonGenereraAvtal:Name = "toolStripButtonGenereraAvtal".
      THIS-OBJECT:toolStripButtonGenereraAvtal:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:toolStripButtonGenereraAvtal:Text = "Generera kalkyl".
      THIS-OBJECT:toolStripButtonGenereraAvtal:Click:Subscribe(THIS-OBJECT:toolStripButtonGenereraAvtal_Click).
      /*  */
      /* KalkAvtalControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerAvtalMeny).
      THIS-OBJECT:Name = "KalkAvtalControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerAvtal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAvtal:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerAvtal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAvtal:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:AvtalText2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ComboAvtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:AvtalText, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAvtalMeny:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAvtalMeny:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerAvtalMeny:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAvtalMeny, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAvtalMeny:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:PerformLayout().
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
   METHOD PRIVATE VOID ComboAvtal_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      THIS-OBJECT:ControlShell:SparaComboV(ComboAvtal).
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonGenereraAvtal_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:GenereraAvtalsKalkyl().
		RETURN.

	END METHOD.

   METHOD PRIVATE CHARACTER  TraffColf(INPUT ordvar AS INTEGER) :
      IF ordvar = 1 THEN RETURN  "G". 
      ELSE IF ordvar = 1 THEN RETURN "G".
         ELSE IF ordvar = 2 THEN RETURN "H".
            ELSE IF ordvar = 3 THEN RETURN "I".
               ELSE IF ordvar = 4 THEN RETURN "J".
                  ELSE IF ordvar = 5 THEN RETURN "K".
                     ELSE IF ordvar = 6 THEN RETURN "L".
                        ELSE IF ordvar = 7 THEN RETURN "M".
                           ELSE IF ordvar = 8 THEN RETURN "N".
                              ELSE IF ordvar = 9 THEN RETURN "O".
                                 ELSE IF ordvar = 10 THEN RETURN "P".
   END METHOD.
   METHOD PUBLIC VOID EonesAvtal(INPUT ExelKommando AS CHARACTER):
      DEFINE VARIABLE avtalvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sterad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frisl       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE skrivkol  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE traffcollrad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE skrivkollangd AS INTEGER NO-UNDO.
      DEFINE VARIABLE oversatt137 AS LOGICAL  NO-UNDO.
      
      avtalvar = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:EonesAvtal(INPUT-OUTPUT avtalvar,INPUT-OUTPUT sterad,INPUT-OUTPUT slerad,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT frist,INPUT-OUTPUT frisl, INPUT-OUTPUT oversatt137, INPUT-OUTPUT ExelKommando,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      skrivkol = "C".
      skrivkollangd = 2.
      /*IF LENGTH(skrivkol) = 1 THEN skrivkollangd = 2.
      ELSE  IF LENGTH(skrivkol) = 2 THEN skrivkollangd = 3.*/                      
      DEBUGGER:SET-BREAK().
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(6).
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN 
           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(12).
      ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F4",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F3",SUBSTRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,40)).
      END.      
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2016" OR
      SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2017"  THEN DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E3",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E2",SUBSTRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,40)).
      END.    
      ELSE DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",SUBSTRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,40)).
      END.           
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN 
      DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",utfardatvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",beredarvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F9",refvar).
      END.
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",utfardatvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F9",beredarvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F10",refvar).
      END. 
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2016" OR
      SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2017"  THEN    DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E6",utfardatvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E7",beredarvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I6",refvar).
      END. 
      ELSE DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).
      END.   
      /*
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN.
      ELSE DO:
          THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DoljFram(FALSE, "AX").
      END.*/   
      DEBUGGER:SET-BREAK().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         
         IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(6).
         ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20112011" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(12).
         ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "133" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 20 THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " m",INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
            IF traffcol = ? OR traffcol = "" THEN traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.         
         ELSE IF oversatt137 = TRUE AND  THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 58 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(20,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 54 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(21,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 50 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(23,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).                     
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 53 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(24,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).                     
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 20 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(25,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).                     
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 59 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(26,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 61 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(87,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 21 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(27,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 22 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(29,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 23 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(28,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 26 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(30,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 30 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(34,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 68 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(35,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 65 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(36,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 44 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(43,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 45 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(44,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 72 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(45,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 73 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(46,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 76 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(49,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 33 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(50,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 34 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(51,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 35 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(52,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 36 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(56,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 80 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(88,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 81 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(89,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 84 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(90,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 85 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(91,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 87 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(92,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 88 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(93,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 89 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(94,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 90 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(95,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 91 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(96,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.
         ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 92 THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(97,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).         
         END.

         
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ESP" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.      
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            DEBUGGER:SET-BREAK().
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
               STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
            END.   
         END.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EBR" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "-" + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKJ" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKL" THEN DO:            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR 
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " +
             STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         END.
         
         ELSE traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
         STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT skrivkol + STRING(sterad),INPUT skrivkol + STRING(slerad),TRUE).
         IF traffcol = ? OR traffcol = "" THEN DO:          
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END. 
         ELSE DO:

            IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( 7).
            ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( 13).              
            ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2014" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2015" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2016"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2017" THEN
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( flik2).
            ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( flik1).
               
             
            traffcol = REPLACE(traffcol,"$","").         
            IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
               traffcol = TraffColf(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 10).
            END.            
            
            ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:
               traffcol = TraffColf(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,2)) + 7).
            END.                        
            ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2016" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2017" THEN DO:            
               
               traffcol = TraffColf(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
            END. 
            ELSE DO:
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
            END.                                 
            
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).
            IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:            

                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                
               END.
            END.
            ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2018" THEN DO:
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:            
                 
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                
               END.
            END.
            ELSE DO:
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:            
                 
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,skrivkollangd))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                
               END.
            END.   
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.
      
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( 6).
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN 
           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad( 12).
      
      ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(INPUT flik2).     
       
      queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
         OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
         OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:               
            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).              
            IF traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " +  
               STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "c" + STRING(sterad),INPUT "c" + STRING(slerad),TRUE).
            END.   
            
            IF traffcol NE "" THEN DO:
               traffcollrad = STRING(SUBSTRING(traffcol,4)).
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE). 
                  traffcol = REPLACE(traffcol,"$","").                  
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "J" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "K" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "O" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "Q" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/
                  /* 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                    */
                     traffcol = "L" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "AA" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Ledning").                  
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "L" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "AA" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kabel").                  
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "L" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "AA" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kranbil").
                     /*
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"RUNT.OM.MASK").
                     */                  
                  END.
                  IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                     traffcol = "L" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "AA" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Lastbil").                  
                  END.
                  IF pristypvar = "MASKIN5" THEN DO: /*F7*/ 
                     traffcol = "L" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "AA" + traffcollrad.
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kranbil").                  
                  END.                 
                  subqH:GET-NEXT(NO-LOCK).
               END.
            END.
            ELSE DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").            
            END.
         END.
         
         ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2016" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2017"
         THEN DO:
         
            /*nytt mallutseende 20160101*/
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "E" + STRING(frist),INPUT "E" + STRING(frisl),TRUE).              
            IF traffcol NE "" THEN DO:
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE: 
                          
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE). 
                  traffcol = REPLACE(traffcol,"$","").                  
                  traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                  
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:
     
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ))).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ))).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Ledning").                  
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kabel").                  
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"RUNT.OM.MASK").                  
                  END.
                  IF pristypvar = "MASKIN4" OR pristypvar = "PRIS1" THEN DO: /*F6*/
                     /*MESSAGE pristypvar THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE                     
                     VIEW-AS ALERT-BOX.*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Lastbil").                  
                  END.
                  IF pristypvar = "MASKIN5" OR pristypvar = "PRIS2" THEN DO: /*F7*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,"Kranbil").                  
                  END.                 
                  subqH:GET-NEXT(NO-LOCK).
               END.
            END.
            ELSE DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").            
            END.
         END.
         ELSE DO:
            /*nytt utseende 20180805*/
     
     
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(frist),"D" + STRING(frisl),TRUE).
            IF traffcol = "" OR traffcol = ?  THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(frist),"D" + STRING(frisl),TRUE).   
            END.                 
            IF traffcol NE "" THEN DO:
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE: 
                          
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE). 
                  traffcol = REPLACE(traffcol,"$","").                  
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                  
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:
     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ))).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ))).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                                       
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                                       
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                                       
                  END.
                  IF pristypvar = "MASKIN4" OR pristypvar = "PRIS1" THEN DO: /*F6*/
                     /*MESSAGE pristypvar THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE                     
                     VIEW-AS ALERT-BOX.*/ 
                     traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                                       
                  END.
                  IF pristypvar = "MASKIN5" OR pristypvar = "PRIS2" THEN DO: /*F7*/ 
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                                       
                  END.                 
                  subqH:GET-NEXT(NO-LOCK).
               END.
            END.     
         END.            
         qH:GET-NEXT().
      END.
      /*IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2011"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2012" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
      OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN.
      ELSE DO:
          THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DoljFram(true, "AX").
      END.*/
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
   END METHOD.
   METHOD PUBLIC VOID AkeaFaktura():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kodraknare AS INTEGER NO-UNDO.
      DEFINE VARIABLE antalposter AS INTEGER NO-UNDO.
      DEFINE VARIABLE bladnr AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxrows AS INTEGER NO-UNDO.
      DEFINE VARIABLE maxblad AS DECIMAL NO-UNDO.
      DEFINE VARIABLE arbetsleadare AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvartel AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredartel AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsvtel AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbetsleadaretel AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvarepost AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarepost AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsvepost AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbetsleadareepost AS CHARACTER NO-UNDO.
      DEFINE VARIABLE appdbh AS HANDLE NO-UNDO.
      appdbh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:AppServerHandle.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:AkeaAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      RUN ExtraPerUppgiter_UI IN appdbh (OUTPUT arbetsleadare,OUTPUT utfardatvartel,OUTPUT beredartel,OUTPUT arbannsvtel,OUTPUT arbetsleadaretel,OUTPUT utfardatvarepost,OUTPUT beredarepost,OUTPUT arbannsvepost,OUTPUT arbetsleadareepost).
      
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A4",ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J4",refvar).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A6",Guru.Konstanter:plusaonr).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutFormatch("D6",STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E6",kontakt).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A8",arbannsv). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",arbannsvtel). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I8",arbannsvepost).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("K6",STRING(TODAY)). 
      maxrows = 58.
      
      
      queryvar =  "PRESELECT EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      antalposter = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQueryPS(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      IF antalposter = ? THEN antalposter = 0.
      IF antalposter > maxrows THEN DO:
         maxblad = THIS-OBJECT:Root:DatabaseManager:Global:AvrundaUpp(antalposter / maxrows).
         bladnr = 2.  
         REPEAT: 
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:KopierBladEfter().
            bladnr = bladnr + 1.
            IF maxblad < bladnr THEN LEAVE. 
         END.
      END.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1).  
      kodraknare = 11.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar). 
      qH:GET-FIRST().
      bladnr = 1.
      DO WHILE qH:QUERY-OFF-END = FALSE:  
         kodraknare = kodraknare + 1.
         IF kodraknare > maxrows + 11 THEN DO:
            bladnr = bladnr + 1.
            kodraknare = 12.
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(bladnr).  
         END.    
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE > 99 THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A" + STRING(kodraknare),THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"999")) .
         END.
         ELSE DO:   
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A" + STRING(kodraknare),THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")) .
         END.   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C" + STRING(kodraknare),THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("G" + STRING(kodraknare),THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
         /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("H" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).*/
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("H" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J" + STRING(kodraknare),STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE)) .
         
         qH:GET-NEXT().
      END.
     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   METHOD PUBLIC VOID Infraberavtal(INPUT ExelKommando AS CHARACTER):
           
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE BerKoderqueryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE BerKoderqH AS HANDLE NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE oversatt137 AS LOGICAL NO-UNDO.
      
      DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE colvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE colvarT AS CHARACTER NO-UNDO.
      DEFINE VARIABLE valtblad AS INTEGER NO-UNDO.
      DEFINE VARIABLE mangdvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE formall AS CHARACTER NO-UNDO.
      
      DEBUGGER:SET-BREAK().
       
      avtalvar = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT avtalvar,INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
         INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv, INPUT-OUTPUT oversatt137, INPUT-OUTPUT ExelKommando   ).
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
         

      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I9",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).           
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).             
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",arbannsv).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F11",kontakt).
              
         valtblad = 5.
         /*FORS och ATS har bara EON */
         formall  = "eon".
         IF Guru.Konstanter:globforetag = "KRIN"  THEN DO:
            /*OBS! inga FRI koder till mängdprotokoll se valtblad = 1
            OBS! sök även på valtblad = 10*/
            
            IF ExelKommando MATCHES "*mängd*" THEN mangdvar = "m".
            IF ExelKommando MATCHES "*EON*" THEN formall  = "eon".
            ELSE formall = "krin".
            IF ExelKommando MATCHES "*mängdprotokoll*" THEN valtblad = 1.
            ELSE IF ExelKommando MATCHES "*mängd*" AND formall = "krin" THEN valtblad = 10.
            
         END.   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad).     
          
         ASSIGN     
         bernumvar = 1 
         colvar = ""
         colvarT = "".
         DEBUGGER:SET-BREAK().

         THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE" + " USE-INDEX ORD",NO-LOCK) NO-ERROR.
         bernumvar = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:Bervallbuffh:TABLE + " USE-INDEX ORD " .         
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:Bervallbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            /*MESSAGE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE
            THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE
            VIEW-AS ALERT-BOX.*/
            IF THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE = "" THEN .
            ELSE DO:
               THIS-OBJECT:KalkdbControl:BerAvtalKodertth:FIND-FIRST(" WHERE BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE)  + " USE-INDEX BERNUM ",NO-LOCK) NO-ERROR.    
               IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:AVAILABLE THEN.
               ELSE DO:
                  IF formall = "KRIN" THEN DO:
                     {MANGDCOLKRIN.I}
                     ELSE IF colvar = "" THEN DO:                     
                        ASSIGN colvar = "K" colvarT = "L".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */                    
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                   
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        /* Kraftringen vill inte längre ha Kontruktion i Till-fältet 20200812*/
                        /*traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).*/
                     END.                     
                     ELSE DO:
                        colvar = "XX".               
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                        THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("1Det ryms bara 100 konstruktioner i mallen!").     
                                            
                     END. 
                  END.
                  ELSE DO:
                        
                     {MANGDCOL.I}                     
                     ELSE IF colvar = "" THEN DO:                     
                        ASSIGN colvar = "J" colvarT = "K".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */                    
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                   
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                     END.                     
                     ELSE DO:
                        DEBUGGER:SET-BREAK().
                        colvar = "XX".               
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                        THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("1Det ryms bara 100 konstruktioner i mallen!").
                                                    
                     END.
                  END.   
                                    
                  IF colvar NE "" THEN DO:
                     /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */    
                     traffcol = colvar + STRING(12).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                 
                     traffcol = colvar + STRING(14).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                     IF Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "ats" THEN.
                     ELSE DO:
                        traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                     END.   
                  END.                         
               END.   
               BerKoderqueryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:BerAvtalKodertth:TABLE + " WHERE BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE)  + " USE-INDEX BERNUM " .
               BerKoderqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:BerAvtalKodertth,BerKoderqueryvar).
               BerKoderqH:GET-FIRST().
               DO WHILE BerKoderqH:QUERY-OFF-END = FALSE:
                  
                  IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = bernumvar THEN DO:
                     IF colvar = "" THEN DO:
                        IF formall = "KRIN" THEN ASSIGN colvar = "K" colvarT = "L".
                        ELSE ASSIGN colvar = "J" colvarT = "K".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */     
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        /* Kraftringen vill inte längre ha Kontruktion i Till-fältet 20200812*/
                        /*traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).*/
                     END.
                  END.   
                  ELSE DO:  
                     IF formall = "KRIN" THEN DO:
                        {MANGDCOLKRIN.I}
                        ELSE DO:                       
                           colvar = "XX".               
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("2Det ryms bara 100 konstruktioner i mallen!").
                                                      
                        END.    
                     END.
                     ELSE DO:   
                        {MANGDCOL.I}
                        ELSE DO:  
                           DEBUGGER:SET-BREAK().                     
                           colvar = "XX".               
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("2Det ryms bara 100 konstruktioner i mallen!").   
                                                    
                        END.
                     END.                        
                     
                     IF colvar NE "" THEN DO:
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */ 
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                    
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        IF Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "ats"  THEN.
                        ELSE DO:
                           traffcol = colvart + STRING(14).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                        END.   
                      END.                        
                  END.
                                         
                  IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
                     IF valtblad = 1 THEN .
                     ELSE DO:
                        
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
                        
                        IF valtblad = 10 THEN DO:
                           /*Kraftringens mall mot eon mängd använd sterad3 istället för sterad2*/ 
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad3),"D" + STRING(slerad3),TRUE).                                   
                           IF traffcol = ? OR traffcol = "" THEN DO:   
                              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad3),"D" + STRING(slerad3),TRUE).
                           END.    
                        END.
                        ELSE DO:   
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).                                   
                           IF traffcol = ? OR traffcol = "" THEN DO:   
                              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
                           END.
                        END.      
                        IF traffcol = ? OR traffcol = "" THEN DO:                                    
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
                        END.   
                        ELSE DO:                     
                           traffcol = REPLACE(traffcol,"$","").            
                           traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                           traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                        
                           queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".      
                           subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
                           subqH:GET-FIRST().   
                           DO WHILE subqH:QUERY-OFF-END = FALSE:         
                              pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                              
                             IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                                 traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                                 traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "MATERIEL" THEN DO:     
                                 traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                                 traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                              END.                                    
                              IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                                 traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                              END.
                              IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                                 traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                              END.
                              IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                                 traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                              END.
                              IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                                 traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                              END.
                              subqH:GET-NEXT(NO-LOCK).           
                           END. 
                        END.
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad).                     
                                 
                        IF valtblad = 10 THEN DO:
                           /*Kraftringens mall mot eon mängd HÅRDKODAD STARTRAD*/
                           /*startrad för "FRI"-koder 1039 + 10 = 1059*/
                           traffcol = colvar + STRING(1039 + INTEGER(SUBSTRING(traffcol,2))).
                        END.
                        ELSE DO:   
                           /*startrad för "FRI"-koder 1031 + 21 = 1052*/
                           traffcol = colvar + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).
                          
                        END.        
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                        /* skrivskyddat fält
                        traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).     
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).     
                        */                                          
                     END.           
                                 
            
                  END.            
                  ELSE DO:
                    THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad). 
                    /*MESSAGE THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE
                    VIEW-AS ALERT-BOX.*/
                    IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE > 0 THEN DO:
                       IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" 
                       OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" 
                       OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137G"  THEN DO:
                            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,1,3)) + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE P" OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE T" OR THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE U"  THEN DO:
                         
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        
                        ELSE DO:
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).                  
                        END.                             
                        IF traffcol = ? OR traffcol = "" THEN DO:
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
                        END.   
                        ELSE DO:                   
                           IF colvar NE "" THEN DO:    
                              traffcol = REPLACE(traffcol,"$","").
                              traffcol = colvar + STRING(INTEGER(SUBSTRING(traffcol,2))).
                              
                              IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "KM" THEN DO: 
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * 1000 )).
                              END.   
                              ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "ha" THEN DO: 
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * 10000 )).
                              END. 
                              ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                          END.                  
                                      
                     /*      bernumvar =  THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.*/                                                       
                        END.
                     END.
                     
                  END.
                  bernumvar =  THIS-OBJECT:KalkdbControl:BerAvtalKodertth:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.         
                  traffcol = "".       
                  BerKoderqH:GET-NEXT().
               END.
            END.   
            qH:GET-NEXT().
         END.
         
                        
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(BerKoderqH). 
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
         
   END METHOD.
   METHOD PUBLIC VOID Infraberavtalsum(INPUT ExelKommando AS CHARACTER):
           
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE BerKoderqueryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE BerKoderqH AS HANDLE NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE oversatt137 AS LOGICAL NO-UNDO.
      
      DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE colvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE colvarT AS CHARACTER NO-UNDO.
      DEFINE VARIABLE valtblad AS INTEGER NO-UNDO.
      DEFINE VARIABLE mangdvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE formall AS CHARACTER NO-UNDO.
      DEFINE VARIABLE nrak AS INTEGER NO-UNDO.
      
      DEBUGGER:SET-BREAK().
       
      avtalvar = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT avtalvar,INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
         INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv, INPUT-OUTPUT oversatt137, INPUT-OUTPUT ExelKommando   ).
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
         

      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I9",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).           
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).             
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",arbannsv).
              
         valtblad = 5.
         /*FORS och ATS har bara EON */
         formall  = "eon".
         /*IF Guru.Konstanter:globforetag = "KRIN"  THEN DO:
            /*OBS! inga FRI koder till mängdprotokoll se valtblad = 1
            OBS! sök även på valtblad = 10*/
            
            IF ExelKommando MATCHES "*mängd*" THEN mangdvar = "m".
            IF ExelKommando MATCHES "*EON*" THEN formall  = "eon".
            ELSE formall = "krin".
            IF ExelKommando MATCHES "*mängdprotokoll*" THEN valtblad = 1.
            ELSE IF ExelKommando MATCHES "*mängd*" AND formall = "krin" THEN valtblad = 10.
            
         END.*/   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad).     
          
         ASSIGN     
         bernumvar = 1 
         colvar = ""
         colvarT = "".
         DEBUGGER:SET-BREAK().
         nrak = 1.
         THIS-OBJECT:KalkdbControl:NBervallbuffh:EMPTY-TEMP-TABLE() NO-ERROR.
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:Bervallbuffh:TABLE + " USE-INDEX ORD " .         
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:Bervallbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
             /*MESSAGE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE
             VIEW-AS ALERT-BOX.*/       
             /*IF THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE = "Lista" OR THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE = "Tom" THEN nrak = nrak + 1.*/
             IF THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE = "Lista"  THEN nrak = nrak + 1.
             ELSE THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE = nrak.
             
             THIS-OBJECT:KalkdbControl:NBervallbuffh:FIND-FIRST("WHERE NGRUPP = " + STRING(nrak)  + " USE-INDEX ORD ",NO-LOCK) NO-ERROR.
             IF THIS-OBJECT:KalkdbControl:NBervallbuffh:AVAILABLE THEN.
             ELSE DO:
                THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-CREATE().
                THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:Bervallbuffh).
             END.
                     
             qH:GET-NEXT().
         END. 
         THIS-OBJECT:KalkdbControl:SumNstn().
         
         
         THIS-OBJECT:KalkdbControl:NBervallbuffh:FIND-FIRST("WHERE" + " USE-INDEX ORD",NO-LOCK) NO-ERROR.
         bernumvar = THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE.
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:NBervallbuffh:TABLE + " USE-INDEX ORD " .         
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:NBervallbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            /*MESSAGE THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE
            THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE
            VIEW-AS ALERT-BOX.*/
            IF THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE = "" THEN .
            ELSE DO:
               THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:FIND-FIRST(" WHERE NGRUPP = " + STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE)  + " USE-INDEX NGRUPP ",NO-LOCK) NO-ERROR.    
               IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:AVAILABLE THEN.
               ELSE DO:
                  IF formall = "KRIN" THEN DO:
                     {MANGDCOLKRIN.I}
                     ELSE IF colvar = "" THEN DO:                     
                        ASSIGN colvar = "K" colvarT = "L".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */                    
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                   
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        /* Kraftringen vill inte längre ha Kontruktion i Till-fältet 20200812*/
                        /*traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).*/
                     END.                     
                     ELSE DO:
                        colvar = "XX".               
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                        THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("1Det ryms bara 100 konstruktioner i mallen!").     
                                            
                     END. 
                  END.
                  ELSE DO:
                        
                     {MANGDCOL.I}                     
                     ELSE IF colvar = "" THEN DO:                     
                        ASSIGN colvar = "J" colvarT = "K".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */                    
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                   
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                     END.                     
                     ELSE DO:
                        DEBUGGER:SET-BREAK().
                        colvar = "XX".               
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                        THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("1Det ryms bara 100 konstruktioner i mallen!").
                                                    
                     END.
                  END.   
                                    
                  IF colvar NE "" THEN DO:
                     /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */    
                     traffcol = colvar + STRING(12).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                 
                     traffcol = colvar + STRING(14).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                     IF Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "ats" THEN.
                     ELSE DO:
                        traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                     END.   
                  END.                         
               END.   
               BerKoderqueryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:TABLE + " WHERE NGRUPP = " + STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE)  + " USE-INDEX NGRUPP " .
               BerKoderqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth,BerKoderqueryvar).
               BerKoderqH:GET-FIRST().
               DO WHILE BerKoderqH:QUERY-OFF-END = FALSE:
                  
                  IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE = bernumvar THEN DO:
                     IF colvar = "" THEN DO:
                        IF formall = "KRIN" THEN ASSIGN colvar = "K" colvarT = "L".
                        ELSE ASSIGN colvar = "J" colvarT = "K".
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */     
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        /* Kraftringen vill inte längre ha Kontruktion i Till-fältet 20200812*/
                        /*traffcol = colvart + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).*/
                     END.
                  END.   
                  ELSE DO:  
                     IF formall = "KRIN" THEN DO:
                        {MANGDCOLKRIN.I}
                        ELSE DO:                       
                           colvar = "XX".               
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("2Det ryms bara 100 konstruktioner i mallen!").
                                                      
                        END.    
                     END.
                     ELSE DO:   
                        {MANGDCOL.I}
                        ELSE DO:  
                           DEBUGGER:SET-BREAK().                     
                           colvar = "XX".               
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.                      
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("2Det ryms bara 100 konstruktioner i mallen!").   
                                                    
                        END.
                     END.                        
                     
                     IF colvar NE "" THEN DO:
                        /*Rubrik på konstruktion ska in på rad 14 FRIID i första och konstrution i andra */ 
                        traffcol = colvar + STRING(12).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE)).                    
                        traffcol = colvar + STRING(14).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE)).
                        IF Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "ats"  THEN.
                        ELSE DO:
                           traffcol = colvart + STRING(14).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:NBervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE)).
                        END.   
                      END.                        
                  END.
                                         
                  IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
                     IF valtblad = 1 THEN .
                     ELSE DO:
                        
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
                        
                        IF valtblad = 10 THEN DO:
                           /*Kraftringens mall mot eon mängd använd sterad3 istället för sterad2*/ 
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad3),"D" + STRING(slerad3),TRUE).                                   
                           IF traffcol = ? OR traffcol = "" THEN DO:   
                              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad3),"D" + STRING(slerad3),TRUE).
                           END.    
                        END.
                        ELSE DO:   
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).                                   
                           IF traffcol = ? OR traffcol = "" THEN DO:   
                              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
                           END.
                        END.      
                        IF traffcol = ? OR traffcol = "" THEN DO:                                    
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("FRI" + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
                        END.   
                        ELSE DO:                     
                           traffcol = REPLACE(traffcol,"$","").            
                           traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                           traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                        
                           queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".      
                           subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
                           subqH:GET-FIRST().   
                           DO WHILE subqH:QUERY-OFF-END = FALSE:         
                              pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                              
                             IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                                 traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                                 traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "MATERIEL" THEN DO:     
                                 traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                              END.
                              IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                                 traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                              END.                                    
                              IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                                 traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                              END.
                              IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                                 traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                              END.
                              IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                                 traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                              END.
                              IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                                 traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                              END.
                              subqH:GET-NEXT(NO-LOCK).           
                           END. 
                        END.
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad).                     
                                 
                        IF valtblad = 10 THEN DO:
                           /*Kraftringens mall mot eon mängd HÅRDKODAD STARTRAD*/
                           /*startrad för "FRI"-koder 1039 + 10 = 1059*/
                           traffcol = colvar + STRING(1039 + INTEGER(SUBSTRING(traffcol,2))).
                        END.
                        ELSE DO:   
                           /*startrad för "FRI"-koder 1031 + 21 = 1052*/
                           traffcol = colvar + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).
                          
                        END.        
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                        /* skrivskyddat fält
                        traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).     
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).     
                        */                                          
                     END.           
                                 
            
                  END.            
                  ELSE DO:
                    THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(valtblad). 
                    /*MESSAGE THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE
                    VIEW-AS ALERT-BOX.*/
                    IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE > 0 THEN DO:
                       IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" 
                       OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" 
                       OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137G"  THEN DO:
                            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,1,3)) + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE P" OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE T" OR THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE U"  THEN DO:
                         
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                        END.
                        
                        ELSE DO:
                           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).                  
                        END.                             
                        IF traffcol = ? OR traffcol = "" THEN DO:
                           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                           THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
                        END.   
                        ELSE DO:                   
                           IF colvar NE "" THEN DO:    
                              traffcol = REPLACE(traffcol,"$","").
                              traffcol = colvar + STRING(INTEGER(SUBSTRING(traffcol,2))).
                              
                              IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "KM" THEN DO: 
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * 1000 )).
                              END.   
                              ELSE IF THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "ha" THEN DO: 
                                 THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * 10000 )).
                              END. 
                              ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                          END.                  
                                      
                     /*      bernumvar =  THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.*/                                                       
                        END.
                     END.
                     
                  END.
                  bernumvar =  THIS-OBJECT:KalkdbControl:BerAvtalKodersumtth:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE.         
                  traffcol = "".       
                  BerKoderqH:GET-NEXT().
               END.
            END.   
            qH:GET-NEXT().
         END.
         
                        
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(BerKoderqH). 
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
         
   END METHOD.
   METHOD PUBLIC VOID InfraAvtal(INPUT ExelKommando AS CHARACTER):
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE oversatt137 AS LOGICAL NO-UNDO.
      
      DEBUGGER:SET-BREAK().
      avtalvar = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT avtalvar,INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
         INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
         INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
         INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv, INPUT-OUTPUT oversatt137 , INPUT-OUTPUT ExelKommando).
         
         
      IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(3).
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2010" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).
      ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2011" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(5).
      ELSE THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
      
      IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2009" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2010" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2011" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2013"  THEN DO:   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("H6",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E4",ortnamn).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E7",kontakt).      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E6",refvar).      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("E5",refvar). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("H8",arbannsv).
      END.
      ELSE DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I6",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F10",kontakt).      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F9",refvar). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",arbannsv).
      END.     
           
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         /*ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN.*/
          ELSE DO:
               IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN DO:
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B912",TRUE).
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN DO:
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B914",TRUE).               
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B13","B1056",TRUE).
               END.         
               ELSE IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE begins "2013"  THEN DO:
                  IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
                      traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).               
                  END.
                  ELSE DO:
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B" + STRING(sterad1),"B" + STRING(slerad1),TRUE).                  
                  END.
               END.   
               ELSE DO:            
                  IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
                      traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                  END.
                  ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE P" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE T" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE U"  THEN DO:
                   /*  MESSAGE  "CCC" THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE
                     VIEW-AS ALERT-BOX.*/                         
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).
                     IF traffcol = ? OR traffcol = "" THEN DO:
                        traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM( "'" + SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).
                     END.                 
                  END.                  
                  ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137G"  THEN DO:
                      traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,1,3)) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
                  END. 
                   
                   
                                    
                  ELSE IF oversatt137 = TRUE AND  THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 58 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(20,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 54 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(21,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 50 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(23,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).                     
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 53 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(24,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).                     
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 20 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(25,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).                     
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 59 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(26,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 61 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(87,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 21 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(27,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 22 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(29,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 23 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(28,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 26 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(30,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 30 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(34,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 68 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(35,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 65 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(36,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 44 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(43,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 45 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(44,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 72 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(45,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 73 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(46,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 76 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(49,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 33 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(50,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 34 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(51,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 35 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(52,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 36 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(56,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.                  
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 80 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(88,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 81 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(89,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 84 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(90,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 85 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(91,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 87 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(92,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 88 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(93,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 89 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(94,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 90 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(95,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 91 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(96,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE IF  oversatt137 = TRUE AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137" AND THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 92 THEN DO:            
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "137" + STRING(97,"99"),INPUT "C" + STRING(sterad1),INPUT "C" + STRING(slerad1),TRUE).         
                  END.
                  ELSE DO:
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).                  
                  END.   
               END.           
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
               END.   
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
                  /*traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).*/                   
               END.                
            END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B84","B108",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.            
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.            
                                      
                  subqH:GET-NEXT(NO-LOCK).
               END.          
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(804 + INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                       
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(3).
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B18","B42",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                           
                  END.
                  IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                          
                  END.
                  subqH:GET-NEXT(NO-LOCK).           
               END.            
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).
               IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN traffcol = "C" + STRING(872 + INTEGER(SUBSTRING(traffcol,2))).
               ELSE traffcol = "C" + STRING(873 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).            
            END.
         END.
         ELSE DO:                    
            /*20112011 och framåt om ej annat framkommer */
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).           
            IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "C" + STRING(sterad2),"C" + STRING(slerad2),TRUE).
            END.
            ELSE DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
            END.        
            IF traffcol = ? OR traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
            END.   
            IF traffcol = ? OR traffcol = "" THEN DO:                                    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").               
            END.   
            ELSE DO:
               IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
                  traffcol = REPLACE(traffcol,"$","").            
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                     
               END.
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").            
                  traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
                  traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               END.
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".      
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2011" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013" THEN DO:
                     IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                        traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN DO:     
                        traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                        traffcol = "R" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                        traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                        traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                     IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                     END.
                     IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                        traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                  END.               
                  ELSE IF  SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"  OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:   
                     IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN DO:     
                        traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                        traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                        traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                     IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                        traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                     END.
                     IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                        traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                  END.
                  ELSE DO:
                    IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "MATERIEL" THEN DO:     
                        traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                        traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                     END.                                    
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                        traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                        traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                     IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                        traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                     END.
                     IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                        traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                     END.
                  END.   
                        
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
   
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).                     
               IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112011" THEN DO:
                  /*startrad för "FRI"-koder*/                           
                  traffcol = "C" + STRING(1012 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               END.
               ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20112012" THEN DO:
                  /*startrad för "FRI"-koder*/               
                  traffcol = "C" + STRING(1013 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).               
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2012" OR THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE BEGINS "2013"  THEN DO:
                  /*startrad för "FRI"-koder*/               
                  traffcol = "C" + STRING(1013 + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).               
               END.            
               ELSE  DO:            
                  /*startrad för "FRI"-koder 1037 + 20 = 1057*/
                  traffcol = "D" + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
                  traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                  
               END.   
            END.         
         END.                
         traffcol = "".
         qH:GET-NEXT().        
      END.   
DEBUGGER:SET-BREAK().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'EON'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2009" THEN     DO:   
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(2).
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B27","B31",TRUE).                    
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            IF traffcol NE "" THEN DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(3).
               traffcol = "C" + STRING(806 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                          
            END.         
         END.
         ELSE IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) = "20102010" THEN   DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(3).                
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"B4","B13",TRUE).
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").              
            END.
            ELSE 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).
               traffcol = "C" + STRING(879 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                             
            END.
         END.
         ELSE  DO:
      
            /*20112011 och framåt om ej annat framkommer*/
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad3),INPUT "C" + STRING(slerad3),TRUE).       
            IF traffcol = ? OR traffcol = "" THEN 
            DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE 
            DO:
               traffcol = REPLACE(traffcol,"$","").
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:         
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF pristypvar = "MATERIEL" THEN 
                  DO:     
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN 
                  DO:     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  subqH:GET-NEXT(NO-LOCK).           
               END. 
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
               traffcol = "C" + STRING(1019 + INTEGER(SUBSTRING(traffcol,2))).     
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).                          
            END.
         END.                  
         traffcol = "".    
         qH:GET-NEXT().    
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
   
   END METHOD.
   METHOD PUBLIC VOID VattenAvtal(INPUT ExelKommando AS CHARACTER):
      IF ExelKommando MATCHES "*EON*" THEN THIS-OBJECT:VattenAvtalEON(). 
      IF ExelKommando MATCHES "*Vattenfall*" THEN THIS-OBJECT:VattenAvtalVatt().
      IF ExelKommando MATCHES "*Ellevio*" THEN THIS-OBJECT:VattenAvtalEllevio().
   END METHOD.
   
   METHOD PUBLIC VOID VattenAvtalVatt():
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE filend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO.       
      DEFINE VARIABLE sterad AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad  AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad3 AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE slerad2  AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad3  AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE xrak AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE egakostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmtimvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egokostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egsumkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmtrlkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egkostov AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egenradhj AS INTEGER NO-UNDO.
      /*DEFINE VARIABLE xegennamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE xegenrak   AS INTEGER NO-UNDO.*/
      DEBUGGER:SET-BREAK().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VattenAvtal(INPUT-OUTPUT sterad ,INPUT-OUTPUT slerad ,OUTPUT sterad2 ,INPUT-OUTPUT slerad2 ,INPUT-OUTPUT sterad3 ,
                                INPUT-OUTPUT slerad3 ,INPUT-OUTPUT flik1 ,INPUT-OUTPUT flik2 ,
                                INPUT-OUTPUT beredarvar ,INPUT-OUTPUT utfardatvar  ,INPUT-OUTPUT refvar ,INPUT-OUTPUT ortnamn ,
                                INPUT-OUTPUT kontakt ,INPUT-OUTPUT arbannsv ).
         
      egenradhj = 4. 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J2", STRING(TODAY,"9999-99-99")).
      IF Guru.Konstanter:globforetag = "VAST"  THEN  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Vattenfall Service").
      IF Guru.Konstanter:globforetag = "SKOK" THEN  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Skogs och Kraftkonsulterna i Norr AB").
      IF Guru.Konstanter:globforetag = "WSP" THEN  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","WSP Sverige AB").
      IF Guru.Konstanter:globforetag = "ENKA" THEN  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Elnätkonsult i norr AB").
      IF Guru.Konstanter:globforetag = "KEKR" THEN  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","KE Kraft AB").
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar).            
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F6",refvar).
      
      xrak = 1.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).   
            /*egen kod vatt -sök x01 osv cccc*/            
            IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0  THEN.
            ELSE DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "X" + STRING(xrak,"99"),"A" + STRING(sterad),"A" + STRING(slerad),TRUE).
               /*traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "X" + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"A" + STRING(sterad),"A" + STRING(slerad),TRUE).*/
            END.
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("X" + " " + STRING(xrak,"99") + " Kod finns ej!").
               /*THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("X" + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").*/             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               traffcol = "B" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).        
               traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
               /*traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).*/
               ASSIGN 
               egakostvar = 0
               egmtimvar = 0
               egmkostvar = 0
               egokostvar = 0
               egsumkostvar = 0.
               egmtrlkostvar = 0.
               egkostov = 0.               
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:
                  
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE NE 0 THEN DO:
                     IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        egakostvar = egakostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/                        
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        egakostvar = egakostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                        /*traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).*/
                        egmtimvar = egmtimvar + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE. 
                        egmkostvar = egmkostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                        /*traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).*/
                        egmtimvar = egmtimvar + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.               
                        egmkostvar = egmkostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                                               
                     END.                                 
                  END.                  
                  /*IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE NE 0 THEN DO:*/
                  /*ta med både tim * pris och kostnader för så gör den i kalkyl Lena20161205*/
                     IF pristypvar = "MATERIEL" THEN DO:                             
                        /*traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).*/                        
                        egmtrlkostvar = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egsumkostvar = egsumkostvar + egmtrlkostvar.                                                
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD"  THEN DO:     
                        /*traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).*/
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END.  
                     IF pristypvar = "ÖVRIGKOSTNADEA"  THEN DO:     
                        /*traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).*/
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END.                                    
                     IF pristypvar = "ENTREP" THEN DO:     
                        /*traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).                       
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).*/
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END. 
                     IF pristypvar = "UTRUSTNING" THEN DO:
                        /*traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).*/
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END. 
                  /*END.*/
                  subqH:GET-NEXT(NO-LOCK).
               END.
               egsumkostvar = egsumkostvar + egakostvar + egmkostvar .
               /*sumMASKINtim */ 
               traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmtimvar)).
               /*sumarbetskostnad */ 
               traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egakostvar)).
               /*sumMASKINkost */ 
               traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmkostvar)).
               /*sumMTRLkost */
               traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmtrlkostvar)).
               
               
               /*sumOVRIGTkost */ 
               traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egokostvar)).
               /*sumtotkost */ 
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egsumkostvar)).

            END.
            DEBUGGER:SET-BREAK().
            IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2015"
            OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2016" or SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,10) =  "20172017Ka" THEN DO:
            END.
            ELSE DO:            
               
               /* nytt från början av 2018 med KLG1 2017*/   
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
               /*benämningar på egen kod i flik7. Först på rad C4 nästa på C24 osv */
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "X" + STRING(xrak,"99"),"A" + STRING(4),"A" + STRING(443),TRUE).
               /*traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "X" + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"A" + STRING(4),"A" + STRING(443),TRUE).*/
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               traffcol = "B" + STRING(INTEGER(SUBSTRING(traffcol,2)) + 1).
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).
               
               /* funkar inte med att räkna upp om de har tagit bort en egen kod i mitten
               /* nytt från början av 2018 med KLG1 2017*/   
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
               /*benämningar på egen kod i flik7. Först på rad C4 nästa på C24 osv */            
               traffcol = "C" + STRING(egenradhj).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).
               traffcol = "B" + STRING(egenradhj + 1).
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE)).                 
               egenradhj = egenradhj + 20.*/
                
            END.
            xrak = xrak + 1.                    
         END.   
         ELSE DO:
           THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
           IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "SKA" THEN DO:
              /*mellanslag i mallen "SKA 201". men testa även utan mellanslag*/
              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"A8","A1010",TRUE).
              IF traffcol = ? OR traffcol = "" THEN DO:
                 traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)  + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"A8","A1010",TRUE).
              END.   
           END.
           ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "137G"  THEN DO:
              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,1,3)) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"A8","A1010",TRUE).               
           END.
           ELSE   
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"A8","A1010",TRUE).
           
           IF traffcol = ? OR traffcol = "" THEN DO:               
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            END.                
         END.
            
         traffcol = "".
         qH:GET-NEXT().
      END.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1). 

   END METHOD.   
   METHOD PUBLIC VOID VattenAvtalEON():
      
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE filend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO.       
      DEFINE VARIABLE sterad AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad  AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad3 AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE slerad2  AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad3  AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE egkostov AS DECIMAL NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VattenEonAvtal(INPUT-OUTPUT sterad ,INPUT-OUTPUT slerad ,OUTPUT sterad2 ,INPUT-OUTPUT slerad2 ,INPUT-OUTPUT sterad3 ,
                                INPUT-OUTPUT slerad3 ,INPUT-OUTPUT flik1 ,INPUT-OUTPUT flik2 ,
                                INPUT-OUTPUT beredarvar ,INPUT-OUTPUT utfardatvar  ,INPUT-OUTPUT refvar ,INPUT-OUTPUT ortnamn ,
                                INPUT-OUTPUT kontakt ,INPUT-OUTPUT arbannsv ).
         
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I9",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",beredarvar).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("O3",refvar).      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:    
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " +  
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
         END.
         traffcol = "".
         qH:GET-NEXT().     
      END.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0  THEN.
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"D" + STRING(sterad),"D" + STRING(slerad),TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).        
            traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE NE 0 THEN DO:
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.                                 
               END.
               
               /*IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE NE 0 THEN DO:*/
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ) )).
                  END.
               
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                                                 
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).                 
                         
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE ,traffcol,STRING(egkostov)).
                     /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(egkostov)).*/
                  END.                                    
                  IF pristypvar = "ENTREP" THEN DO:     
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE ,traffcol,STRING(egkostov)).
                  END. 
                  IF pristypvar = "UTRUSTNING" THEN DO:
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).     
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(egkostov)).
                  END. 
               /*END.*/
               subqH:GET-NEXT(NO-LOCK).
            END.
         END.
         traffcol = "".
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
   END METHOD.
   
   METHOD PUBLIC VOID VattenAvtalEllevio():
      
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE filend AS CHARACTER NO-UNDO.
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO.       
      DEFINE VARIABLE sterad AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad  AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE sterad3 AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE slerad2  AS INTEGER NO-UNDO.
      DEFINE VARIABLE slerad3  AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE flik2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sokhjalp AS CHARACTER NO-UNDO.
      DEFINE VARIABLE egkostov AS DECIMAL NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VattenEllevioAvtal(INPUT-OUTPUT sterad ,INPUT-OUTPUT slerad ,OUTPUT sterad2 ,INPUT-OUTPUT slerad2 ,INPUT-OUTPUT sterad3 ,
                                INPUT-OUTPUT slerad3 ,INPUT-OUTPUT flik1 ,INPUT-OUTPUT flik2 ,
                                INPUT-OUTPUT beredarvar ,INPUT-OUTPUT utfardatvar  ,INPUT-OUTPUT refvar ,INPUT-OUTPUT ortnamn ,
                                INPUT-OUTPUT kontakt ,INPUT-OUTPUT arbannsv ).
         
      /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J32",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("U32",ortnamn).*/
     /* THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("x20",beredarvar).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("x17",refvar).
      MESSAGE utfardatvar beredarvar refvar kontakt
      VIEW-AS ALERT-BOX.*/
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:    
         
         /*IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + 
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.*/
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.30" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.31" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.32" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.33" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.34" OR
         THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TJ30.35" THEN DO:
            sokhjalp = SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,1,2) + "." + SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,3,5) + "."
            + SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,1,3) + "." + SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,4,2).
            
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(sokhjalp),"D" + STRING(sterad),"D" + STRING(slerad),TRUE).
            /*traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " +  
            STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).*/
         END.
         /*ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"c" + STRING(sterad),"c" + STRING(slerad),TRUE).
         END.*/
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99999") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            /*traffcol = REPLACE(traffcol,"$","").
            traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).*/           
         END.
         traffcol = "".
         qH:GET-NEXT().     
      END.
      /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0  THEN.
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"D" + STRING(sterad),"D" + STRING(slerad),TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).        
            traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
            traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE NE 0 THEN DO:
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                      
                  END.                                 
               END.
               
               /*IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE NE 0 THEN DO:*/
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ) )).
                  END.
               
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                                                 
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).                 
                         
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE ,traffcol,STRING(egkostov)).
                     /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(egkostov)).*/
                  END.                                    
                  IF pristypvar = "ENTREP" THEN DO:     
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE ,traffcol,STRING(egkostov)).
                  END. 
                  IF pristypvar = "UTRUSTNING" THEN DO:
                     egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).     
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,traffcol,STRING(egkostov)).
                  END. 
               /*END.*/
               subqH:GET-NEXT(NO-LOCK).
            END.
         END.
         traffcol = "".
         qH:GET-NEXT(NO-LOCK).
      END.*/
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
   END METHOD.
   
   
   METHOD PUBLIC VOID  SwecoAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE egakostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmtimvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egokostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egsumkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egmtrlkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE egkostov AS DECIMAL NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:SwecoAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
     /*Anders Olsson Elpool i Umeå AB  24 feb 2015 09:37:50 
     nytt avtal
     */
     /*
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(2).
      */
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ViewType(INPUT 1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 2.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J2",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Sweco Energuide AB").
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J5",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         IF STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN. 
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "A8", "A1032",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE  DO:   
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                   
            END.
            traffcol = "".
         END.   
         qH:GET-NEXT().
      END.
      

      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            
            /*egen kod vatt -sök x01 osv cccc*/            
            IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0  THEN.
            ELSE DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT "X" + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"A8","A1032",TRUE).
            END.
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt("X" + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               traffcol = "B" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)).        
               traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).        
               /*traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).*/
               ASSIGN 
               egakostvar = 0
               egmkostvar = 0
               egokostvar = 0
               egsumkostvar = 0.
               egmtrlkostvar = 0.
               egkostov = 0.
               
               queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
               subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
               subqH:GET-FIRST().   
               DO WHILE subqH:QUERY-OFF-END = FALSE:
                  
                  pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
                  IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE NE 0 THEN DO:
                     IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                        traffcol = "H" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        egakostvar = egakostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MONTÖR" THEN DO:     /*F2*/                        
                        traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                        THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                        egakostvar = egakostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MASKIN1" THEN DO: /*F3*/                         
                        egmtimvar = egmtimvar + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE. 
                        egmkostvar = egmkostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                        
                     END.
                     IF pristypvar = "MASKIN2" THEN DO:  /*F4*/                         
                        egmtimvar = egmtimvar + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.               
                        egmkostvar = egmkostvar + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).                                               
                     END.                                 
                  END.                  
                  /*IF THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE NE 0 THEN DO:*/
                  /*ta med både tim * pris och kostnader för så gör den i kalkyl Lena20161205*/
                     IF pristypvar = "MATERIEL" THEN DO:                                                                             
                        egmtrlkostvar = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egsumkostvar = egsumkostvar + egmtrlkostvar.                                                
                     END.
                     IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:                             
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END.                                    
                     IF pristypvar = "ENTREP" THEN DO:                             
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END. 
                     IF pristypvar = "UTRUSTNING" THEN DO:                 
                        egkostov = THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + (THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE ).
                        egokostvar = egokostvar + egkostov.
                        egsumkostvar = egsumkostvar + egkostov.
                     END. 
                  /*END.*/
                  subqH:GET-NEXT(NO-LOCK).
               END.
               egsumkostvar = egsumkostvar + egakostvar + egmkostvar .
               /*sumMASKINtim */ 
               traffcol = "J" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmtimvar)).
               /*sumarbetskostnad */ 
               traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egakostvar)).
               /*sumMASKINkost */ 
               traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmkostvar)).
               /*sumMTRLkost */
               traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egmtrlkostvar)).               
               /*sumOVRIGTkost */ 
               traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egokostvar)).
               /*sumtotkost */ 
               traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(egsumkostvar)).

            END.

      /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 875.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE ARBKOD = 'EGEN'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad + 1.    
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).             
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).                           
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
                  
            
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE NE 0 THEN DO:
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE  BEGINS "BEREDARE" THEN DO: /*F1*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().      
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MONTÖR" THEN DO:     /*F2*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO: /*F3*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
              
            END.*/
            
            subqH:GET-NEXT(NO-LOCK).
         END.  
         /*queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:  
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE NE 0 THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE )).
            END.    
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE NE 0 THEN DO:
               
               IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ARBETE" THEN DO:   
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                  
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGKOSTNAD" OR THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "UTRUSTNING" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.                                    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.*/
         qH:GET-NEXT().
      END. 
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).  
     
    
   END METHOD.
 
   METHOD PUBLIC VOID WigaAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      
      THIS-OBJECT:KalkdbControl:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkdbControl:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LappAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
     /*Anders Olsson Elpool i Umeå AB  24 feb 2015 09:37:50 
     nytt avtal
     */
     /*
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(2).
      */
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ViewType(INPUT 1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 2.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J2",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Lapplands Elnät AB").
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J5",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         IF STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN. 
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "A8", "A1032",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE  DO:   
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                   
            END.
            traffcol = "".
         END.   
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 868.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE ARBKOD = 'EGEN'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad + 1.    
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).             
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).                           
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE NE 0 THEN DO:
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE  BEGINS "BEREDARE" THEN DO: /*F1*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().      
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MONTÖR" THEN DO:     /*F2*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO: /*F3*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
            END.
            
            subqH:GET-NEXT(NO-LOCK).
         END.  
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:  
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE NE 0 THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            END.    
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE NE 0 THEN DO:
               
               IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ARBETE" THEN DO:   
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                  
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGKOSTNAD" OR THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "UTRUSTNING" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.                                    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).  
     
    
   END METHOD.
   METHOD PUBLIC VOID  LappAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:KalkdbControl:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkdbControl:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LappAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
     /*Anders Olsson Elpool i Umeå AB  24 feb 2015 09:37:50 
     nytt avtal
     */
     /*
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(2).
      */
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ViewType(INPUT 1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 2.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J2",STRING(TODAY,"9999-99-99")).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J3","Lapplands Elnät AB").
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J4",beredarvar). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("J5",ortnamn).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         IF STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN. 
         ELSE DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "A8", "A1032",TRUE).
            IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
            END.
            ELSE  DO:   
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "C" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
                                   
            END.
            traffcol = "".
         END.   
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 868.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE ARBKOD = 'EGEN'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad + 1.    
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().   
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99")).             
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight().
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE)). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColRight(). 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE)).                           
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE NE 0 THEN DO:
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE  BEGINS "BEREDARE" THEN DO: /*F1*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().      
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MONTÖR" THEN DO:     /*F2*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO: /*F3*/ 
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().  
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
            END.
            
            subqH:GET-NEXT(NO-LOCK).
         END.  
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:  
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "H".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "I".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "J".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(0)).
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE NE 0 THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "K".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            END.    
            IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE NE 0 THEN DO:
               
               IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ARBETE" THEN DO:   
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "L".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "M".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
                  
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE BEGINS "MASKIN" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "N".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGKOSTNAD" OR THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "UTRUSTNING" THEN DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.
               ELSE DO:     
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "O".
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.                                    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "P".
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().    
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOutKrBort(TRUE,STRING(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE / THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
            END.
            subqH:GET-NEXT(NO-LOCK).
         END.
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).  
     
    
   END METHOD.
   METHOD PUBLIC VOID  LimoAvtal():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:LimoAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).                             
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 1.    
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A3",refvar + " " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("A5",STRING(TODAY,"9999-99-99")). 
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C3",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C5",beredarvar).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:                 
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).         
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "TKOD"  THEN 
         DO: 
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE,1,6),INPUT "B10", "B950",TRUE).                    
         END.      
         ELSE traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),INPUT "B10", "B950",TRUE).
         IF traffcol = ? OR traffcol = "" THEN 
         DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99") + " Kod finns ej!").
         END.
         ELSE 
         DO:   
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(4).                 
            traffcol = REPLACE(traffcol,"$","").
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                         
            IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:AddKom(INPUT traffcol,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).                    
         END.
         traffcol = "".
         qH:GET-NEXT().
      END.   
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   METHOD PUBLIC VOID  KraftringenAvtal(INPUT ExelKommando AS CHARACTER):
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      THIS-OBJECT:KalkdbControl:kalkantalTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:kalkantalTTh.
      THIS-OBJECT:KalkdbControl:KalkkostnadTTh = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkkostnadTTh.
     
      
                                  
      IF ExelKommando MATCHES "*mot EON*" THEN THIS-OBJECT:KraftringenEON(ExelKommando).
      IF ExelKommando MATCHES "*Kraftringen EBR kostnadskalkyl KLG*" THEN THIS-OBJECT:KraftringenEON(ExelKommando).        
      /*IF ExelKommando MATCHES "*faktor till UE*" THEN THIS-OBJECT:KraftringenFaktorUE().
      IF ExelKommando MATCHES "*KNAB_Kostnads*" THEN THIS-OBJECT:KraftringenTillaggsKoder().  
      IF ExelKommando MATCHES "*UE MALL*" THEN THIS-OBJECT:KraftringenUEMall().
      IF ExelKommando MATCHES "*-EBR20*" THEN THIS-OBJECT:KraftringenEBRUE().
      IF ExelKommando MATCHES "*Referensmall*" THEN THIS-OBJECT:KraftringenReferens().  */
      
      
      
   END METHOD.
   
   METHOD PUBLIC VOID KraftringenReferens():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE btimmarvar AS DECIMAL NO-UNDO. 
      DEFINE VARIABLE mtimmarvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE masktimmarkabvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE masktimmarledvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE mtrlvarkost AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ovrvarkost AS DECIMAL NO-UNDO.
      
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
     
      THIS-OBJECT:KalkdbControl:KringAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
     
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("B1","Projekt: " + " " + Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1]) + " " + ortnamn).           
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("P1",STRING(TODAY)).
      THIS-OBJECT:KalkdbControl:KatalogDeltth:FIND-FIRST("WHERE BENAMNING BEGINS 'EBR KLG1'  ") NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:KatalogDeltth:AVAILABLE THEN DO:       
          THIS-OBJECT:KalkdbControl:PriserTTh:FIND-FIRST("WHERE KLOGSUBID = " + THIS-OBJECT:KalkdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE  +  " AND SOKBENAMNING = " + QUOTER('MONTÖR')) NO-ERROR.
          IF THIS-OBJECT:KalkdbControl:PriserTTh:AVAILABLE THEN DO:
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N27",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q21",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).                          
          END.   
          THIS-OBJECT:KalkdbControl:PriserTTh:FIND-FIRST("WHERE KLOGSUBID = " + THIS-OBJECT:KalkdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE  +  " AND SOKBENAMNING = " + QUOTER('BEREDARE')) NO-ERROR.
          IF THIS-OBJECT:KalkdbControl:PriserTTh:AVAILABLE THEN DO:
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N28",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q22",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).                          
          END.
          /*KABELSCHAKT MASKIN*/
          THIS-OBJECT:KalkdbControl:PriserTTh:FIND-FIRST("WHERE KLOGSUBID = " + THIS-OBJECT:KalkdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE  +  " AND SOKBENAMNING = " + QUOTER('MASKIN2')) NO-ERROR.
          IF THIS-OBJECT:KalkdbControl:PriserTTh:AVAILABLE THEN DO:
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N29",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q23",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).                          
          END.
          /*LEDNINGSBYGGNADS MASKIN*/
          THIS-OBJECT:KalkdbControl:PriserTTh:FIND-FIRST("WHERE KLOGSUBID = " + THIS-OBJECT:KalkdbControl:KatalogDeltth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE  +  " AND SOKBENAMNING = " + QUOTER('MASKIN1')) NO-ERROR.
          IF THIS-OBJECT:KalkdbControl:PriserTTh:AVAILABLE THEN DO:
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N30",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).
             THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q24",STRING(THIS-OBJECT:KalkdbControl:PriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE)).                          
          END.
       END.                 
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).
      qH:GET-FIRST().  
      REPEAT:
         IF THIS-OBJECT:KalkdbControl:kalkantalTTh:AVAILABLE THEN.
         ELSE LEAVE .         
         THIS-OBJECT:KalkdbControl:AvtalKodertth:FIND-FIRST("WHERE  ") NO-ERROR. 
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:AVAILABLE THEN DO:           
            pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).                                  
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Beredare" THEN DO:
               btimmarvar = btimmarvar + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE.
            END.
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Montör" THEN DO:            
               mtimmarvar = mtimmarvar + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE.
            END.
            IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Maskin" THEN DO:
               IF pristypvar = "MASKIN1" THEN DO: 
                  masktimmarledvar = masktimmarledvar + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE.
               END.
               ELSE IF pristypvar = "MASKIN2" THEN DO: 
                  masktimmarkabvar = masktimmarkabvar + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE.
               END.  
            END.
         END.            
         qH:GET-NEXT().
         IF THIS-OBJECT:KalkdbControl:kalkantalTTh:AVAILABLE THEN.
         ELSE LEAVE.
      END.         
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N6",STRING(btimmarvar)).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N7",STRING(mtimmarvar)).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N8",STRING(masktimmarkabvar)).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N9",STRING(masktimmarledvar)).
      THIS-OBJECT:KalkdbControl:kalkantalTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:kalkantalTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N4","nr:" + STRING(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)).
      END.   
            
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
      qH:GET-FIRST().  
      REPEAT:
         IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:AVAILABLE THEN.
         ELSE LEAVE .                          
         IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "MATERIEL" THEN DO:
            mtrlvarkost = mtrlvarkost + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
         END.         
         IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "ÖVRIGT" THEN DO:
            ovrvarkost = ovrvarkost + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
         END.
         qH:GET-NEXT().
         IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:AVAILABLE THEN.
         ELSE LEAVE.
      END.
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N10",STRING(mtrlvarkost)).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("N11",STRING(ovrvarkost)).
            
      THIS-OBJECT:KraftringenReferensUE(). 
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(1).
   END METHOD.
   
   METHOD PUBLIC VOID KraftringenReferensUE():
      DEFINE VARIABLE btimUE AS DECIMAL NO-UNDO. 
      DEFINE VARIABLE mtimUE AS DECIMAL NO-UNDO.
      DEFINE VARIABLE kabtimUE AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ledtimUE AS DECIMAL NO-UNDO.
      DEFINE VARIABLE mtrlUE AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ovrUE AS DECIMAL NO-UNDO.
      DEFINE VARIABLE uenr AS INTEGER NO-UNDO.
      ASSIGN        
      btimUE = 0 mtimUE = 0  kabtimUE = 0 ledtimUE = 0 mtrlUE = 0 ovrUE = 0.
      THIS-OBJECT:KalkdbControl:ReferensUE(OUTPUT btimUE,OUTPUT mtimUE,OUTPUT kabtimUE,OUTPUT ledtimUE,OUTPUT mtrlUE,OUTPUT ovrUE, OUTPUT uenr).
      
      IF btimUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q7",STRING(btimUE)).
      IF mtimUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q8",STRING(mtimUE)).
      IF kabtimUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q9",STRING(kabtimUE)).
      IF ledtimUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q10",STRING(ledtimUE)).
      IF mtrlUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q11",STRING(mtrlUE)).
      IF ovrUE > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q12",STRING(ovrUE)).
      IF uenr > 0 THEN THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("Q4","nr:" + STRING(uenr)).
            
   END METHOD.
   
   METHOD PUBLIC VOID KraftringenUEMall():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KringAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C2",ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C3",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1]) + ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C4",arbannsv).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C6",STRING(TODAY)). 
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE DO:
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B12","B151",TRUE).
           IF traffcol = ? OR traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B12","B151",TRUE).
           END.                  
           IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            END.                
         END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PUBLIC VOID KraftringenEBRUE():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.

      /*THIS-OBJECT:Root:DatabaseManager:Kalkyl:KringAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C2",ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C3",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1]) + ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C4",arbannsv).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C6",STRING(TODAY)).*/
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(1).
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE BEGINS "E" THEN.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE BEGINS "K" THEN.
         ELSE DO:
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),"C17","C893",TRUE).
           /*IF traffcol = ? OR traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B12","B151",TRUE).
           END.*/                  
           IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " Kod finns ej!").             
            END.   
            ELSE DO:                            
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">9")),traffcol,"D893",TRUE).
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">9") + " Kod finns ej!").             
               END.
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "A" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
               END.           
            END.                
         END.   
         traffcol = "".
         qH:GET-NEXT().
      END. 
      /*queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE DO:
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B12","B151",TRUE).
           IF traffcol = ? OR traffcol = "" THEN DO:
               traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B12","B151",TRUE).
           END.                  
           IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            END.                
         END.   
         traffcol = "".
         qH:GET-NEXT().
      END.*/
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   
   METHOD PUBLIC VOID KraftringenTillaggsKoder():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KringAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C4",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1]) + ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C6",refvar).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("K4",STRING(TODAY)). 
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "KSJ" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "KS" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN DO:
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " "  + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"A16","B156",TRUE).
           /*
           IF traffcol = ? OR traffcol = "" THEN DO:
              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"999"),"A16","B156",TRUE).
           END.
           */   
           IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            END.                
         END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   METHOD PUBLIC VOID KraftringenFaktorUE():
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KringAvtal(INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C3",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C2",ortnamn).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C4",arbannsv).      
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("C6",STRING(TODAY)). 
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE DO:
           traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B11","B163",TRUE).
           IF traffcol = ? OR traffcol = "" THEN DO:
              traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"B11","B163",TRUE).
           END.   
           IF traffcol = ? OR traffcol = "" THEN DO:
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
            END.   
            ELSE DO:
               traffcol = REPLACE(traffcol,"$","").
               traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
               THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
            END.                
         END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = 1 THEN DO: 
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">9"),INPUT "B168","B175",TRUE).
         END.
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>9") + " Kod finns ej!").               
         END.   
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").            
            traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).                      
               
         END.         
         traffcol = "".
         qH:GET-NEXT().        
      END.
     
     
     
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
   METHOD PUBLIC VOID KraftringenEON(INPUT ExelKommando AS CHARACTER):
      DEFINE VARIABLE avtalvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sterad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad1     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad2     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE sterad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE slerad3     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE frist       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik1       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE flik2       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE beredarvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE refvar      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ortnamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kontakt     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE arbannsv    AS CHARACTER NO-UNDO.    
      DEFINE VARIABLE traffcol    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH          AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE oversatt137  AS LOGICAL NO-UNDO.
      avtalvar = THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:InfraAvtal(INPUT-OUTPUT avtalvar,INPUT-OUTPUT sterad1,INPUT-OUTPUT slerad1,OUTPUT sterad2,INPUT-OUTPUT slerad2,INPUT-OUTPUT sterad3,
      INPUT-OUTPUT slerad3,INPUT-OUTPUT frist,INPUT-OUTPUT flik1,INPUT-OUTPUT flik2,
      INPUT-OUTPUT beredarvar,INPUT-OUTPUT utfardatvar ,INPUT-OUTPUT refvar,INPUT-OUTPUT ortnamn,
      INPUT-OUTPUT kontakt,INPUT-OUTPUT arbannsv, INPUT-OUTPUT oversatt137, INPUT-OUTPUT ExelKommando).
             
             
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
      IF ExelKommando MATCHES "*mot EON*" THEN DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I9",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",arbannsv).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).               
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).      
          
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F11",kontakt).
      END.   
      ELSE  IF ExelKommando MATCHES "*Kraftringen EBR kostnadskalkyl KLG*" THEN DO:
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I9",Guru.Konstanter:plusaonr + " " + STRING(Guru.Konstanter:plusdnr,Guru.Konstanter:varforetypchar[1])).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I10",arbannsv).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("I11",beredarvar).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F7",ortnamn).
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F9",kontakt).      
         /*THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F8",refvar).      
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut("F9",refvar).*/ 
       
      END.   
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:        
         IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "FRI" THEN.
         ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN.
         ELSE DO:
               IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EJK" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "ELL" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EUH" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EAF" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EON" THEN DO:
                   traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).               
               END.
               ELSE IF THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE P" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE T" OR THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "HE U"  THEN DO:
                  /*MESSAGE  "CCC" THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE
                  VIEW-AS ALERT-BOX.*/                         
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).
                  /*IF traffcol = ? OR traffcol = "" THEN DO:
                     traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM( "'" + SUBSTRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,4,1)) +  STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"9999"),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).
                  END.*/                 
               END.                   
               ELSE DO:
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).                  
               END.   
   
               IF traffcol = ? OR traffcol = "" THEN DO:
                  traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99")),"C" + STRING(sterad1),"C" + STRING(slerad1),TRUE).
               END.           
               IF traffcol = ? OR traffcol = "" THEN DO:
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").             
               END.   
               ELSE DO:
                  traffcol = REPLACE(traffcol,"$","").
                  traffcol = "D" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).        
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).                   
               END.                
            END.   
         traffcol = "".
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'FRI'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).           
         traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).
         IF traffcol = ? OR traffcol = "" THEN DO:
            traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + TRIM(STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99")),INPUT "D" + STRING(sterad2),"D" + STRING(slerad2),TRUE).   
         END.   
         IF traffcol = ? OR traffcol = "" THEN DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">99") + " Kod finns ej!").               
         END.               
         ELSE DO:
            traffcol = REPLACE(traffcol,"$","").            
            traffcol = "F" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ENHET"):BUFFER-VALUE).                          
            traffcol = "E" + STRING(INTEGER(SUBSTRING(traffcol,2))).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".      
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               IF SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) = "2013" OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,4) =  "2014"
               OR SUBSTRING(THIS-OBJECT:KalkdbControl:Avtalskalktth:BUFFER-FIELD("AVTALTXTHELA"):BUFFER-VALUE,1,8) =  "20152015" THEN DO:
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "S" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "K" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                  END.
                  IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                  END.
               END.
               ELSE DO:
                  IF pristypvar BEGINS "BEREDARE" THEN DO: /*F1*/     
                     traffcol = "G" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MONTÖR" THEN DO:     /*F2*/ 
                     traffcol = "I" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "MATERIEL" THEN DO:     
                     traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.
                  IF pristypvar = "ÖVRIGKOSTNAD" THEN DO:     
                     traffcol = "T" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
                  END.                                    
                  IF pristypvar = "MASKIN1" THEN DO: /*F3*/ 
                     traffcol = "M" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                
                  END.
                  IF pristypvar = "MASKIN2" THEN DO:  /*F4*/ 
                     traffcol = "L" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                  END.
                  IF pristypvar = "MASKIN3" THEN DO:  /*F5*/ 
                     traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                        
                  END.
                  IF pristypvar = "MASKIN4" THEN DO: /*F6*/ 
                     traffcol = "O" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                     THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE)).                                                       
                  END.
            
               END.      
               
               
               subqH:GET-NEXT(NO-LOCK).           
            END. 
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).                     
         
            /*startrad för "FRI"-koder 1037 + 20 = 1057*/
            traffcol = "D" + STRING(frist + INTEGER(SUBSTRING(traffcol,2))).     
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
            traffcol = "Q" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE).                  
               
         END.         
         traffcol = "".
         qH:GET-NEXT().        
      END.   
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:AvtalKodertth:TABLE + " WHERE AvtalKodertt.ARBKOD = 'EON'".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:AvtalKodertth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         /*20112011 och framåt om ej annat framkommer*/
         THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik1).
         traffcol = THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SokIExcel(INPUT TRIM(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,"99"),"C" + STRING(sterad3),INPUT "C" + STRING(slerad3),TRUE).       
         IF traffcol = ? OR traffcol = "" THEN 
         DO:
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:felexcel = FALSE.
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:TidutHmt(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>99") + " Kod finns ej!").
         END.
         ELSE  DO:
            traffcol = REPLACE(traffcol,"$","").
            queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE + " WHERE kalknumsubtt.NUM = " + STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
            subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
            subqH:GET-FIRST().   
            DO WHILE subqH:QUERY-OFF-END = FALSE:         
               pristypvar = THIS-OBJECT:Root:DatabaseManager:KALKYL:PrisTyp(INPUT THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KPID"):BUFFER-VALUE,INPUT THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE).
               IF pristypvar = "MATERIEL" THEN 
               DO:     
                  traffcol = "N" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.
               IF pristypvar = "ÖVRIGKOSTNAD" THEN 
               DO:     
                  traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).
                  THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE)).
               END.                                    
               subqH:GET-NEXT(NO-LOCK).           
            END. 
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).
            traffcol = "C" + STRING(1019 + INTEGER(SUBSTRING(traffcol,2))).     
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,STRING(THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).     
            traffcol = "P" + STRING(INTEGER(SUBSTRING(traffcol,2))).           
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:DataOut(traffcol,THIS-OBJECT:KalkdbControl:AvtalKodertth:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
            THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:ValjBlad(flik2).                          
         END.
                           
         traffcol = "".    
         qH:GET-NEXT().    
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      THIS-OBJECT:ControlShell:KalkVisaControl:VisaExcel:SlutExcel(3).
   END METHOD.
	DESTRUCTOR PUBLIC KalkAvtalControl ( ):
    /*  {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.
