 
 /*------------------------------------------------------------------------
    File        : ArendeJoin
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu Jan 29 15:44:18 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Arende.ArendeJoin INHERITS UserControl: 
	{ARENDEKATH.i}
	{ARENDEKAT.I}
	DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin             AS Controls.GridRubrikListaJoin                                      NO-UNDO.
	{GridRubrikListaJoinVarExtent.i}
	DEFINE PRIVATE VARIABLE bindingSourceVisaNU AS Progress.Data.BindingSource NO-UNDO.
	DEFINE VARIABLE Visasuperdsh AS HANDLE NO-UNDO.
	DEFINE DATASET Visasuperds FOR earendehuvtt,earendenumtt,earendenumsubtt,earendemtrlTT
   DATA-RELATION visaDR FOR earendehuvtt, earendenumtt RELATION-FIELDS (earendehuvtt.ARENDENR,earendenumtt.ARENDENR)
   DATA-RELATION visaDR FOR earendenumtt, earendenumsubtt RELATION-FIELDS (earendenumtt.ARENDENR,earendenumsubtt.ARENDENR,earendenumtt.NUM,earendenumsubtt.NUM)
   DATA-RELATION visaDR FOR earendehuvtt, earendemtrlTT RELATION-FIELDS (earendehuvtt.ARENDENR,earendemtrlTT.ARENDENR).
   
   /*
   DEFINE DATASET Visasuperds FOR earendehuvtt,earendenumtt
   DATA-RELATION visaDR FOR earendehuvtt, earendenumtt RELATION-FIELDS (earendehuvtt.ARENDENR,earendenumtt.ARENDENR).
   */
   DEFINE VARIABLE qArbetskoderTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qLopposterTTh AS HANDLE NO-UNDO.
   
   DEFINE VARIABLE qHuvTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qNumTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qNumSubTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qMtrlTTh AS HANDLE NO-UNDO.
   
   
   
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   /*
   DEFINE PRIVATE VARIABLE GridJoinVisa AS Controls.GridJoin NO-UNDO.
   */
   DEFINE PRIVATE VARIABLE GridJoinVisa AS Controls.GridJoin NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Arende.ArendeShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
		
	CONSTRUCTOR PUBLIC ArendeJoin ( INPUT r AS Guru.Root ):
		SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Arende:ControlShell.
     
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      
      {ARENDEKATDBH.i Root:DatabaseManager:Arende}
      
      THIS-OBJECT:qArbetskoderTTh = TEMP-TABLE ekalkylarbkodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qLopposterTTh = TEMP-TABLE ekalkylloppostertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qHuvTTh = TEMP-TABLE earendehuvtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qNumTTh = TEMP-TABLE earendenumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qNumSubTTh = TEMP-TABLE earendenumsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qMtrlTTh = TEMP-TABLE earendemtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      
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
      resources = NEW Progress.Util.ResourceManager("Modules.Arende.ArendeJoin").
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
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridJoinVisa = NEW Controls.GridJoin().
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:GridJoinVisa, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:GridJoinVisa).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainer1.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainer1:SplitterDistance = 644.
      THIS-OBJECT:splitContainer1:TabIndex = 0.
      /*  */
      /* GridJoinVisa */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridJoinVisa:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridJoinVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridJoinVisa:GuruContext = ?.
      THIS-OBJECT:GridJoinVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridJoinVisa:Name = "GridJoinVisa".
      THIS-OBJECT:GridJoinVisa:Size = NEW System.Drawing.Size(644, 546).
      THIS-OBJECT:GridJoinVisa:TabIndex = 0.
      THIS-OBJECT:GridJoinVisa:Text = "gridJoin1".
      /*  */
      /* ArendeJoin */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:Name = "ArendeJoin".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridJoinVisa, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      
      THIS-OBJECT:CreateGridJoinVisa().
     
      
   END METHOD.
   METHOD PUBLIC VOID StartJoin():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      qHuvTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      qNumTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      qNumSubTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      qMtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      queryvar =  "FOR EACH " + THIS-OBJECT:HuvudTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:HuvudTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qHuvTTh:BUFFER-CREATE().
         qHuvTTh:BUFFER-COPY(HuvudTTh).
         qH:GET-NEXT().   
      END.  
      queryvar =  "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qNumTTh:BUFFER-CREATE().
         qNumTTh:BUFFER-COPY(KoderTTh).
         qH:GET-NEXT().   
      END. 
      queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qNumSubTTh:BUFFER-CREATE().
         qNumSubTTh:BUFFER-COPY(ValdaPriserTTh).
         qH:GET-NEXT().   
      END. 
      queryvar =  "FOR EACH " + THIS-OBJECT:arendemtrlTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:arendemtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qMtrlTTh:BUFFER-CREATE().
         qMtrlTTh:BUFFER-COPY(arendemtrlTTh).
         qH:GET-NEXT().   
      END. 
      THIS-OBJECT:HuvudTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      THIS-OBJECT:qHuvTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      THIS-OBJECT:qNumTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      /*
      IF THIS-OBJECT:bindingSourceVisaNU = ? THEN DO:  
         THIS-OBJECT:bindingSourceVisaNU = NEW Progress.Data.BindingSource().
         bindingSourceVisaNU:HANDLE = DATASET Visasuperds:HANDLE.
         THIS-OBJECT:GridJoinVisa:DataSource = bindingSourceVisaNU.
         THIS-OBJECT:GridJoinVisa:GridBind(bindingSourceVisaNU).
      END.
      ELSE bindingSourceVisaNU:HANDLE = DATASET Visasuperds:HANDLE.
      */
      /*
      THIS-OBJECT:GridJoinVisa:GuruInitDBS(DATASET-handle Visasuperds:HANDLE).
      */
     

      
     THIS-OBJECT:GridJoinVisa:GridData(DATASET-HANDLE Visasuperdsh).
     
     
   END METHOD.
   METHOD PUBLIC VOID CreateGridJoinVisa():
      {GridRubrikListaVarExtent.i}
         
      
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[1] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[2] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[3] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[4] = NEW Controls.GridRubrikListaJoin().
                  
      rubrikergrid[1] = NEW Controls.GridRubrik("ARENDENR","Ärende",TRUE,10).
      rubrikergrid[2] = NEW Controls.GridRubrik("OMRADE",Guru.Konstanter:gomrk,TRUE,500).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,10).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT 0,INPUT rubrikergrid,INPUT TRUE,INPUT 3,INPUT THIS-OBJECT:qHuvTTh).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,40).
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Lopnr",TRUE,80).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benamning",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[2] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT 1,INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:qNumTTh).
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benamning",TRUE,200).
      rubrikergrid[2] = NEW Controls.GridRubrik("TIMMAR","Timmar",TRUE,60).
      rubrikergrid[3] = NEW Controls.GridRubrik("PRIS","Pris",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("KOSTNAD","Kostnad",TRUE,50).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[3] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT 2,INPUT rubrikergrid,INPUT TRUE,INPUT 4, INPUT THIS-OBJECT:qNumSubTTh).
      
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benamning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("Enr","Enr",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[4] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT 3,INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:qMtrlTTh).
      
      Visasuperdsh = DATASET Visasuperds:HANDLE.
      THIS-OBJECT:GridJoinVisa:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,4,DATASET-HANDLE Visasuperdsh).
      
      THIS-OBJECT:GridJoinVisa:SetViewStyleBand(TRUE).
      
   END METHOD.
   
   
   
	DESTRUCTOR PUBLIC ArendeJoin ( ):

	END DESTRUCTOR.

END CLASS.