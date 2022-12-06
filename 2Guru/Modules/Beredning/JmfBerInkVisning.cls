 
 /*------------------------------------------------------------------------
    File        : JmfBerInkVisning
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 29 13:18:10 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
USING Progress.Lang.*.



CLASS Modules.Beredning.JmfBerInkVisning INHERITS Form   : 
   
  
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridJmfBerInk AS Controls.GridJoin NO-UNDO.
   DEFINE VARIABLE antalDStabeller AS INTEGER NO-UNDO.
   DEFINE VARIABLE flerbertempbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE perflerberbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE summberinkmtrbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berinkmtrbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE sumkbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE topbuffh AS HANDLE NO-UNDO.
   
   DEFINE VARIABLE antalProtTabeller AS INTEGER NO-UNDO.
   
   DEFINE PRIVATE VARIABLE splitContainerMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   DEFINE VARIABLE jmfBerInkDSh AS HANDLE NO-UNDO.
   
   CONSTRUCTOR PUBLIC JmfBerInkVisning ():
       
      SUPER().
                 
      InitializeComponent().
      THIS-OBJECT:SIZE = NEW System.Drawing.Size(Guru.Konstanter:globstorb, Guru.Konstanter:globstorh).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Close().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridJmfBerInk_VisibleChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF Guru.GlobalaVariabler:fonstercol > 0 THEN THIS-OBJECT:LEFT = Guru.GlobalaVariabler:fonstercol.
      IF Guru.GlobalaVariabler:fonsterrow > 0 THEN THIS-OBJECT:TOP = Guru.GlobalaVariabler:fonsterrow.
      THIS-OBJECT:GridJmfBerInk:GuruExpand(TRUE).
		RETURN.
	END METHOD.

   METHOD PUBLIC VOID InitiateJmBerInkVis():
      THIS-OBJECT:CreateGridJmfBerInk().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID ConnectDataSet(DATASET-HANDLE jmfBerInkDSh BIND, inantalDStabeller AS INTEGER):
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 1.
      DEFINE VARIABLE iCount2 AS INTEGER NO-UNDO INITIAL 1.
      
      THIS-OBJECT:jmfBerInkDSh:EMPTY-DATASET() NO-ERROR.
      THIS-OBJECT:antalDStabeller = inantalDStabeller.
      
      
      IF VALID-HANDLE(jmfBerInkDSh) THEN DO:
         topbuffh = jmfBerInkDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         flerbertempbuffh = jmfBerInkDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         summberinkmtrbuffh = jmfBerInkDSh:GET-BUFFER-HANDLE(iCount). 
         iCount = iCount + 1.
         perflerberbuffh = jmfBerInkDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         berinkmtrbuffh = jmfBerInkDSh:GET-BUFFER-HANDLE(iCount).
      END. 
      
      THIS-OBJECT:jmfBerInkDSh = jmfBerInkDSh.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridJmfBerInk():
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE decCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE hideBands AS INTEGER EXTENT 10 NO-UNDO.
      DEFINE VARIABLE sortBands AS INTEGER EXTENT 50 NO-UNDO.
      DEFINE VARIABLE sortCount AS INTEGER NO-UNDO INITIAL 1.
      
      
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      
      
      
      iCount = 1.
       
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
    
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
    
      THIS-OBJECT:GridJmfBerInk:GuruGridRadOption(TRUE).
      CellFont = NEW System.Drawing.Font("Arial Black", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).   
                                             /*styrfält , värde ,   färg ,                          font, alla col, lista col  0  bgcolor 1 = fg color 2 font*/
                                                                                                                          /*  ska avslutas med,*/ 
      THIS-OBJECT:GridJmfBerInk:GuruAddRadOption("TYPSNITT", "1", Guru.StaticMethods:DotNetColor(12),CellFont,TRUE,"",2,0).
      
      
      /*start band*/        
      iband = 0.
      /*topbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("NAMN","Innehåll",TRUE,150).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:topbuffh).
      iband = iband + 1.
      
    
      rubrikergrid[1] = NEW Controls.GridRubrik("AONR",Guru.Konstanter:gaoK,TRUE,60).
      rubrikergrid[2] = NEW Controls.GridRubrik("DELNR",Guru.Konstanter:gdelnrk,TRUE,40,"DELNR").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,200).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 3,INPUT THIS-OBJECT:flerbertempbuffh).
      iband = iband + 1.
     
      rubrikergrid[1] = NEW Controls.GridRubrik("ENRBER",Guru.Konstanter:genk,TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNINGBER","Benämning",TRUE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("LEVNAMNBER","Leverantör",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTALBER","Antal",TRUE,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("ENRINK",Guru.Konstanter:genk,TRUE,75).
      rubrikergrid[6] = NEW Controls.GridRubrik("BENAMNINGINK","Benämning",TRUE,200).
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVNAMNINK","Leverantör",TRUE,100).
      rubrikergrid[8] = NEW Controls.GridRubrik("ANTALINK","Antal",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 8,INPUT THIS-OBJECT:summberinkmtrbuffh).
     
      iband = iband + 1.
      rubrikergrid[1] = NEW Controls.GridRubrik("AONR",Guru.Konstanter:gaoK,TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("DELNR",Guru.Konstanter:gdelnrk,TRUE,40,"DELNR").
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 3,INPUT THIS-OBJECT:perflerberbuffh).
      iband = iband + 1.
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ENRBER",Guru.Konstanter:genk,TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNINGBER","Benämning",TRUE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("LEVNAMNBER","Leverantör",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANTALBER","Antal",TRUE,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("ENRINK",Guru.Konstanter:genk,TRUE,75).
      rubrikergrid[6] = NEW Controls.GridRubrik("BENAMNINGINK","Benämning",TRUE,200).
      rubrikergrid[7] = NEW Controls.GridRubrik("LEVNAMNINK","Leverantör",TRUE,100).
      rubrikergrid[8] = NEW Controls.GridRubrik("ANTALINK","Antal",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 8,INPUT THIS-OBJECT:berinkmtrbuffh).
      
      THIS-OBJECT:GridJmfBerInk:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband + 1,DATASET-HANDLE jmfBerInkDSh).
      THIS-OBJECT:GridJmfBerInk:Text = "Jämför Beredning mot Inköp". 
      
      THIS-OBJECT:SkapaGrupper(2).
      THIS-OBJECT:SkapaGrupper(4).
      
   END METHOD.
   METHOD PUBLIC VOID SkapaGrupper(gruppband AS INTEGER ):
      THIS-OBJECT:GridJmfBerInk:SkapaGrupp(gruppband, "grupp1", "Beredning").
      
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "ANTALBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "LEVNAMNBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "BENAMNINGBER").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp1", "ENRBER").
      
      THIS-OBJECT:GridJmfBerInk:SkapaGrupp(gruppband, "grupp2", "Inköp").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "ANTALINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "LEVNAMNINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "BENAMNINGINK").
      THIS-OBJECT:GridJmfBerInk:SetGroup(gruppband, "grupp2", "ENRINK").
      
   END METHOD.   

   METHOD PRIVATE VOID InitializeComponent ( ):
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Beredning.JmfBerInkVisning").
      THIS-OBJECT:splitContainerMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridJmfBerInk = NEW Controls.GridJoin().
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMain:SuspendLayout().
      CAST(THIS-OBJECT:GridJmfBerInk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerMain */
      /*  */
      THIS-OBJECT:splitContainerMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMain:Name = "splitContainerMain".
      THIS-OBJECT:splitContainerMain:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel1:Controls:Add(THIS-OBJECT:GridJmfBerInk).
      /*  */
      /* splitContainerMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel2:Controls:Add(THIS-OBJECT:ButtonOK).
      THIS-OBJECT:splitContainerMain:Size = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:splitContainerMain:SplitterDistance = 818.
      THIS-OBJECT:splitContainerMain:TabIndex = 0.
      /*  */
      /* GridJmfBerInk */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridJmfBerInk:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridJmfBerInk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridJmfBerInk:GuruContext = ?.
      THIS-OBJECT:GridJmfBerInk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridJmfBerInk:Name = "GridJmfBerInk".
      THIS-OBJECT:GridJmfBerInk:Size = NEW System.Drawing.Size(1184, 818).
      THIS-OBJECT:GridJmfBerInk:TabIndex = 1.
      THIS-OBJECT:GridJmfBerInk:Text = "gridJoin1".
      THIS-OBJECT:GridJmfBerInk:VisibleChanged:Subscribe(THIS-OBJECT:GridJmfBerInk_VisibleChanged).
      /*  */
      /* ButtonOK */
      /*  */
      THIS-OBJECT:ButtonOK:Anchor = System.Windows.Forms.AnchorStyles:Right.
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance13.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(1062, 5).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 3.
      THIS-OBJECT:ButtonOK:TabStop = FALSE.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* JmfBerInkVisning */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerMain).
      THIS-OBJECT:Name = "JmfBerInkVisning".
      THIS-OBJECT:Text = "Jämför Beredning mot Inköp".
      THIS-OBJECT:splitContainerMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMain:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridJmfBerInk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.



END CLASS.