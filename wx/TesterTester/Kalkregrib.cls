
 /*------------------------------------------------------------------------
    File        : Kalkreg
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed Oct 26 16:06:07 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

USING GuruStart.*.

CLASS Kalk.Kalkreg INHERITS Form: 
	CONSTRUCTOR PUBLIC Kalkreg (kalknrVar AS INTEGER,omradeVar AS CHARACTER  ):
	   SUPER().
      InitializeComponent().
      
      CATCH e AS Progress.Lang.Error:
         
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   
   CONSTRUCTOR PUBLIC Kalkreg ( ):
          
      SUPER().
      InitializeComponent().
      CATCH e AS Progress.Lang.Error:         
         UNDO, THROW e.
      END CATCH.
END CONSTRUCTOR.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:egnapriser).
      THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3".
      THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* egnapriser */
      /*  */
      appearance402:BackColor = System.Drawing.SystemColors:Window.
      appearance402:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:egnapriser:DisplayLayout:Appearance = appearance402.
      THIS-OBJECT:egnapriser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:egnapriser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance403:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance403:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance403:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance403:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnapriser:DisplayLayout:GroupByBox:Appearance = appearance403.
      appearance404:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnapriser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance404.
      THIS-OBJECT:egnapriser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance405:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance405:BackColor2 = System.Drawing.SystemColors:Control.
      appearance405:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance405:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnapriser:DisplayLayout:GroupByBox:PromptAppearance = appearance405.
      THIS-OBJECT:egnapriser:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:egnapriser:DisplayLayout:MaxRowScrollRegions = 1.
      appearance406:BackColor = System.Drawing.SystemColors:Window.
      appearance406:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:ActiveCellAppearance = appearance406.
      appearance407:BackColor = System.Drawing.SystemColors:Highlight.
      appearance407:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:ActiveRowAppearance = appearance407.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance408:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:CardAreaAppearance = appearance408.
      appearance409:BorderColor = System.Drawing.Color:Silver.
      appearance409:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:CellAppearance = appearance409.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:CellPadding = 0.
      appearance410:BackColor = System.Drawing.SystemColors:Control.
      appearance410:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance410:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance410:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance410:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:GroupByRowAppearance = appearance410.
      appearance411:TextHAlignAsString = "Left".
      THIS-OBJECT:egnapriser:DisplayLayout:Override:HeaderAppearance = appearance411.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance412:BackColor = System.Drawing.SystemColors:Window.
      appearance412:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:RowAppearance = appearance412.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance413:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:egnapriser:DisplayLayout:Override:TemplateAddRowAppearance = appearance413.
      THIS-OBJECT:egnapriser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:egnapriser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:egnapriser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:egnapriser:GuruContext = ?.
      THIS-OBJECT:egnapriser:Location = NEW System.Drawing.Point(74, 50).
      THIS-OBJECT:egnapriser:Name = "egnapriser".
      THIS-OBJECT:egnapriser:Size = NEW System.Drawing.Size(550, 291).
      THIS-OBJECT:egnapriser:TabIndex = 0.
      THIS-OBJECT:egnapriser:Text = "grid1".
      /*  */
      /* ultraTabPageControl4 */
      /*  */
      THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:btnBeredningKalkyl).
      THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:buttonBackMarkMtrl).
      THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:buttonOverMarkMtrl).
      THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:egetmtrl).
      THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:valmtrl).
      THIS-OBJECT:ultraTabPageControl4:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4".
      THIS-OBJECT:ultraTabPageControl4:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* btnBeredningKalkyl */
      /*  */
      THIS-OBJECT:btnBeredningKalkyl:Location = NEW System.Drawing.Point(775, 78).
      THIS-OBJECT:btnBeredningKalkyl:Name = "btnBeredningKalkyl".
      THIS-OBJECT:btnBeredningKalkyl:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:btnBeredningKalkyl:TabIndex = 12.
      THIS-OBJECT:btnBeredningKalkyl:Text = "Beredning-Kalkyl".
      /*  */
      /* buttonBackMarkMtrl */
      /*  */
      THIS-OBJECT:buttonBackMarkMtrl:Location = NEW System.Drawing.Point(309, 229).
      THIS-OBJECT:buttonBackMarkMtrl:Name = "buttonBackMarkMtrl".
      THIS-OBJECT:buttonBackMarkMtrl:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackMarkMtrl:TabIndex = 11.
      THIS-OBJECT:buttonBackMarkMtrl:Text = "Markerade bort".
      /*  */
      /* buttonOverMarkMtrl */
      /*  */
      THIS-OBJECT:buttonOverMarkMtrl:Location = NEW System.Drawing.Point(309, 191).
      THIS-OBJECT:buttonOverMarkMtrl:Name = "buttonOverMarkMtrl".
      THIS-OBJECT:buttonOverMarkMtrl:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverMarkMtrl:TabIndex = 10.
      THIS-OBJECT:buttonOverMarkMtrl:Text = "Markerade väljs".
      /*  */
      /* egetmtrl */
      /*  */
      appearance414:BackColor = System.Drawing.SystemColors:Window.
      appearance414:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:egetmtrl:DisplayLayout:Appearance = appearance414.
      THIS-OBJECT:egetmtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:egetmtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance415:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance415:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance415:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance415:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egetmtrl:DisplayLayout:GroupByBox:Appearance = appearance415.
      appearance416:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egetmtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance416.
      THIS-OBJECT:egetmtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance417:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance417:BackColor2 = System.Drawing.SystemColors:Control.
      appearance417:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance417:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egetmtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance417.
      THIS-OBJECT:egetmtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:egetmtrl:DisplayLayout:MaxRowScrollRegions = 1.
      appearance418:BackColor = System.Drawing.SystemColors:Window.
      appearance418:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:ActiveCellAppearance = appearance418.
      appearance419:BackColor = System.Drawing.SystemColors:Highlight.
      appearance419:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:ActiveRowAppearance = appearance419.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance420:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:CardAreaAppearance = appearance420.
      appearance421:BorderColor = System.Drawing.Color:Silver.
      appearance421:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:CellAppearance = appearance421.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:CellPadding = 0.
      appearance422:BackColor = System.Drawing.SystemColors:Control.
      appearance422:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance422:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance422:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance422:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:GroupByRowAppearance = appearance422.
      appearance423:TextHAlignAsString = "Left".
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:HeaderAppearance = appearance423.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance424:BackColor = System.Drawing.SystemColors:Window.
      appearance424:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:RowAppearance = appearance424.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance425:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:egetmtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance425.
      THIS-OBJECT:egetmtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:egetmtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:egetmtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:egetmtrl:GuruContext = ?.
      THIS-OBJECT:egetmtrl:Location = NEW System.Drawing.Point(335, 30).
      THIS-OBJECT:egetmtrl:Name = "egetmtrl".
      THIS-OBJECT:egetmtrl:Size = NEW System.Drawing.Size(289, 373).
      THIS-OBJECT:egetmtrl:TabIndex = 1.
      THIS-OBJECT:egetmtrl:Text = "grid1".
      /*  */
      /* valmtrl */
      /*  */
      appearance426:BackColor = System.Drawing.SystemColors:Window.
      appearance426:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:valmtrl:DisplayLayout:Appearance = appearance426.
      THIS-OBJECT:valmtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:valmtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance427:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance427:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance427:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance427:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valmtrl:DisplayLayout:GroupByBox:Appearance = appearance427.
      appearance428:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valmtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance428.
      THIS-OBJECT:valmtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance429:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance429:BackColor2 = System.Drawing.SystemColors:Control.
      appearance429:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance429:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valmtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance429.
      THIS-OBJECT:valmtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:valmtrl:DisplayLayout:MaxRowScrollRegions = 1.
      appearance430:BackColor = System.Drawing.SystemColors:Window.
      appearance430:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:ActiveCellAppearance = appearance430.
      appearance431:BackColor = System.Drawing.SystemColors:Highlight.
      appearance431:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:ActiveRowAppearance = appearance431.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance432:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:CardAreaAppearance = appearance432.
      appearance433:BorderColor = System.Drawing.Color:Silver.
      appearance433:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:CellAppearance = appearance433.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:CellPadding = 0.
      appearance434:BackColor = System.Drawing.SystemColors:Control.
      appearance434:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance434:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance434:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance434:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:GroupByRowAppearance = appearance434.
      appearance435:TextHAlignAsString = "Left".
      THIS-OBJECT:valmtrl:DisplayLayout:Override:HeaderAppearance = appearance435.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance436:BackColor = System.Drawing.SystemColors:Window.
      appearance436:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:RowAppearance = appearance436.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance437:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:valmtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance437.
      THIS-OBJECT:valmtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:valmtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:valmtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:valmtrl:GuruContext = ?.
      THIS-OBJECT:valmtrl:Location = NEW System.Drawing.Point(17, 30).
      THIS-OBJECT:valmtrl:Name = "valmtrl".
      THIS-OBJECT:valmtrl:Size = NEW System.Drawing.Size(289, 373).
      THIS-OBJECT:valmtrl:TabIndex = 0.
      THIS-OBJECT:valmtrl:Text = "grid1".
      /*  */
      /* ultraTabPageControl5 */
      /*  */
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:buttonBackAllaMallar).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:buttonBackMarkMallar).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:buttonOverAllaMallar).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:buttonOverMarkMallar).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:mallarkoder).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:valdamallar).
      THIS-OBJECT:ultraTabPageControl5:Controls:Add(THIS-OBJECT:mallar).
      THIS-OBJECT:ultraTabPageControl5:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl5:Name = "ultraTabPageControl5".
      THIS-OBJECT:ultraTabPageControl5:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* buttonBackAllaMallar */
      /*  */
      THIS-OBJECT:buttonBackAllaMallar:Location = NEW System.Drawing.Point(255, 233).
      THIS-OBJECT:buttonBackAllaMallar:Name = "buttonBackAllaMallar".
      THIS-OBJECT:buttonBackAllaMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackAllaMallar:TabIndex = 14.
      THIS-OBJECT:buttonBackAllaMallar:Text = "Alla bort".
      /*  */
      /* buttonBackMarkMallar */
      /*  */
      THIS-OBJECT:buttonBackMarkMallar:Location = NEW System.Drawing.Point(255, 195).
      THIS-OBJECT:buttonBackMarkMallar:Name = "buttonBackMarkMallar".
      THIS-OBJECT:buttonBackMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackMarkMallar:TabIndex = 13.
      THIS-OBJECT:buttonBackMarkMallar:Text = "Markerade bort".
      /*  */
      /* buttonOverAllaMallar */
      /*  */
      THIS-OBJECT:buttonOverAllaMallar:Location = NEW System.Drawing.Point(255, 119).
      THIS-OBJECT:buttonOverAllaMallar:Name = "buttonOverAllaMallar".
      THIS-OBJECT:buttonOverAllaMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverAllaMallar:TabIndex = 12.
      THIS-OBJECT:buttonOverAllaMallar:Text = "Alla väljs".
      /*  */
      /* buttonOverMarkMallar */
      /*  */
      THIS-OBJECT:buttonOverMarkMallar:Location = NEW System.Drawing.Point(255, 157).
      THIS-OBJECT:buttonOverMarkMallar:Name = "buttonOverMarkMallar".
      THIS-OBJECT:buttonOverMarkMallar:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverMarkMallar:TabIndex = 11.
      THIS-OBJECT:buttonOverMarkMallar:Text = "Markerade väljs".
      /*  */
      /* mallarkoder */
      /*  */
      appearance489:BackColor = System.Drawing.SystemColors:Window.
      appearance489:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:mallarkoder:DisplayLayout:Appearance = appearance489.
      THIS-OBJECT:mallarkoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:mallarkoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance490:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance490:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance490:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance490:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallarkoder:DisplayLayout:GroupByBox:Appearance = appearance490.
      appearance491:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:mallarkoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance491.
      THIS-OBJECT:mallarkoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance492:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance492:BackColor2 = System.Drawing.SystemColors:Control.
      appearance492:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance492:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:mallarkoder:DisplayLayout:GroupByBox:PromptAppearance = appearance492.
      THIS-OBJECT:mallarkoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:mallarkoder:DisplayLayout:MaxRowScrollRegions = 1.
      appearance493:BackColor = System.Drawing.SystemColors:Window.
      appearance493:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:ActiveCellAppearance = appearance493.
      appearance494:BackColor = System.Drawing.SystemColors:Highlight.
      appearance494:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:ActiveRowAppearance = appearance494.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance495:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:CardAreaAppearance = appearance495.
      appearance496:BorderColor = System.Drawing.Color:Silver.
      appearance496:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:CellAppearance = appearance496.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:CellPadding = 0.
      appearance497:BackColor = System.Drawing.SystemColors:Control.
      appearance497:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance497:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance497:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance497:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:GroupByRowAppearance = appearance497.
      appearance498:TextHAlignAsString = "Left".
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:HeaderAppearance = appearance498.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance499:BackColor = System.Drawing.SystemColors:Window.
      appearance499:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:RowAppearance = appearance499.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance500:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance500.
      THIS-OBJECT:mallarkoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:mallarkoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:mallarkoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:mallarkoder:GuruContext = ?.
      THIS-OBJECT:mallarkoder:Location = NEW System.Drawing.Point(589, 43).
      THIS-OBJECT:mallarkoder:Name = "mallarkoder".
      THIS-OBJECT:mallarkoder:Size = NEW System.Drawing.Size(222, 277).
      THIS-OBJECT:mallarkoder:TabIndex = 2.
      THIS-OBJECT:mallarkoder:Text = "grid1".
      /*  */
      /* valdamallar */
      /*  */
      appearance501:BackColor = System.Drawing.SystemColors:Window.
      appearance501:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:valdamallar:DisplayLayout:Appearance = appearance501.
      THIS-OBJECT:valdamallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:valdamallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance502:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance502:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance502:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance502:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdamallar:DisplayLayout:GroupByBox:Appearance = appearance502.
      appearance503:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valdamallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance503.
      THIS-OBJECT:valdamallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance504:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance504:BackColor2 = System.Drawing.SystemColors:Control.
      appearance504:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance504:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valdamallar:DisplayLayout:GroupByBox:PromptAppearance = appearance504.
      THIS-OBJECT:valdamallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:valdamallar:DisplayLayout:MaxRowScrollRegions = 1.
      appearance505:BackColor = System.Drawing.SystemColors:Window.
      appearance505:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:ActiveCellAppearance = appearance505.
      appearance506:BackColor = System.Drawing.SystemColors:Highlight.
      appearance506:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:ActiveRowAppearance = appearance506.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance507:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:CardAreaAppearance = appearance507.
      appearance508:BorderColor = System.Drawing.Color:Silver.
      appearance508:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:CellAppearance = appearance508.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:CellPadding = 0.
      appearance509:BackColor = System.Drawing.SystemColors:Control.
      appearance509:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance509:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance509:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance509:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:GroupByRowAppearance = appearance509.
      appearance510:TextHAlignAsString = "Left".
      THIS-OBJECT:valdamallar:DisplayLayout:Override:HeaderAppearance = appearance510.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance511:BackColor = System.Drawing.SystemColors:Window.
      appearance511:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:RowAppearance = appearance511.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance512:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:valdamallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance512.
      THIS-OBJECT:valdamallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:valdamallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:valdamallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:valdamallar:GuruContext = ?.
      THIS-OBJECT:valdamallar:Location = NEW System.Drawing.Point(289, 43).
      THIS-OBJECT:valdamallar:Name = "valdamallar".
      THIS-OBJECT:valdamallar:Size = NEW System.Drawing.Size(222, 277).
      THIS-OBJECT:valdamallar:TabIndex = 1.
      THIS-OBJECT:valdamallar:Text = "grid1".
      /*  */
      /* mallar */
      /*  */
      appearance513:BackColor = System.Drawing.SystemColors:Window.
      appearance513:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:mallar:DisplayLayout:Appearance = appearance513.
      THIS-OBJECT:mallar:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:mallar:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance514:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance514:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance514:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance514:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallar:DisplayLayout:GroupByBox:Appearance = appearance514.
      appearance515:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:mallar:DisplayLayout:GroupByBox:BandLabelAppearance = appearance515.
      THIS-OBJECT:mallar:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance516:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance516:BackColor2 = System.Drawing.SystemColors:Control.
      appearance516:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance516:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:mallar:DisplayLayout:GroupByBox:PromptAppearance = appearance516.
      THIS-OBJECT:mallar:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:mallar:DisplayLayout:MaxRowScrollRegions = 1.
      appearance517:BackColor = System.Drawing.SystemColors:Window.
      appearance517:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:mallar:DisplayLayout:Override:ActiveCellAppearance = appearance517.
      appearance518:BackColor = System.Drawing.SystemColors:Highlight.
      appearance518:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:mallar:DisplayLayout:Override:ActiveRowAppearance = appearance518.
      THIS-OBJECT:mallar:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:mallar:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance519:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallar:DisplayLayout:Override:CardAreaAppearance = appearance519.
      appearance520:BorderColor = System.Drawing.Color:Silver.
      appearance520:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:mallar:DisplayLayout:Override:CellAppearance = appearance520.
      THIS-OBJECT:mallar:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:mallar:DisplayLayout:Override:CellPadding = 0.
      appearance521:BackColor = System.Drawing.SystemColors:Control.
      appearance521:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance521:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance521:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance521:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:mallar:DisplayLayout:Override:GroupByRowAppearance = appearance521.
      appearance522:TextHAlignAsString = "Left".
      THIS-OBJECT:mallar:DisplayLayout:Override:HeaderAppearance = appearance522.
      THIS-OBJECT:mallar:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:mallar:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance523:BackColor = System.Drawing.SystemColors:Window.
      appearance523:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:mallar:DisplayLayout:Override:RowAppearance = appearance523.
      THIS-OBJECT:mallar:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance524:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:mallar:DisplayLayout:Override:TemplateAddRowAppearance = appearance524.
      THIS-OBJECT:mallar:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:mallar:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:mallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:mallar:GuruContext = ?.
      THIS-OBJECT:mallar:Location = NEW System.Drawing.Point(19, 43).
      THIS-OBJECT:mallar:Name = "mallar".
      THIS-OBJECT:mallar:Size = NEW System.Drawing.Size(222, 277).
      THIS-OBJECT:mallar:TabIndex = 0.
      THIS-OBJECT:mallar:Text = "grid1".
      /*  */
      /* ultraTabPageControl12 */
      /*  */
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:LabelAvtal).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:cmb_avtal).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:Labelartalskj).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:cmb_artalavtal).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:ultraTextEditor2).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:avtaltext).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:Labelklgartal).
      THIS-OBJECT:ultraTabPageControl12:Controls:Add(THIS-OBJECT:cmb_KLGartal).
      THIS-OBJECT:ultraTabPageControl12:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl12:Name = "ultraTabPageControl12".
      THIS-OBJECT:ultraTabPageControl12:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* LabelAvtal */
      /*  */
      appearance438:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelAvtal:Appearance = appearance438.
      THIS-OBJECT:LabelAvtal:Location = NEW System.Drawing.Point(567, 124).
      THIS-OBJECT:LabelAvtal:Name = "LabelAvtal".
      THIS-OBJECT:LabelAvtal:Size = NEW System.Drawing.Size(85, 23).
      THIS-OBJECT:LabelAvtal:TabIndex = 10.
      THIS-OBJECT:LabelAvtal:Text = "Avtal".
      /*  */
      /* cmb_avtal */
      /*  */
      THIS-OBJECT:cmb_avtal:CheckedListSettings:CheckStateMember = "".
      appearance439:BackColor = System.Drawing.SystemColors:Window.
      appearance439:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Appearance = appearance439.
      THIS-OBJECT:cmb_avtal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:cmb_avtal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance440:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance440:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance440:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance440:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_avtal:DisplayLayout:GroupByBox:Appearance = appearance440.
      appearance441:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_avtal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance441.
      THIS-OBJECT:cmb_avtal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance442:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance442:BackColor2 = System.Drawing.SystemColors:Control.
      appearance442:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance442:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_avtal:DisplayLayout:GroupByBox:PromptAppearance = appearance442.
      THIS-OBJECT:cmb_avtal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:cmb_avtal:DisplayLayout:MaxRowScrollRegions = 1.
      appearance443:BackColor = System.Drawing.SystemColors:Window.
      appearance443:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:ActiveCellAppearance = appearance443.
      appearance444:BackColor = System.Drawing.SystemColors:Highlight.
      appearance444:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:ActiveRowAppearance = appearance444.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance445:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:CardAreaAppearance = appearance445.
      appearance446:BorderColor = System.Drawing.Color:Silver.
      appearance446:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:CellAppearance = appearance446.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:CellPadding = 0.
      appearance447:BackColor = System.Drawing.SystemColors:Control.
      appearance447:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance447:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance447:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance447:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:GroupByRowAppearance = appearance447.
      appearance448:TextHAlignAsString = "Left".
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:HeaderAppearance = appearance448.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance449:BackColor = System.Drawing.SystemColors:Window.
      appearance449:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:RowAppearance = appearance449.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance450:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:cmb_avtal:DisplayLayout:Override:TemplateAddRowAppearance = appearance450.
      THIS-OBJECT:cmb_avtal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:cmb_avtal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:cmb_avtal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:cmb_avtal:Location = NEW System.Drawing.Point(658, 124).
      THIS-OBJECT:cmb_avtal:Name = "cmb_avtal".
      THIS-OBJECT:cmb_avtal:Size = NEW System.Drawing.Size(273, 22).
      THIS-OBJECT:cmb_avtal:TabIndex = 9.
      /*  */
      /* Labelartalskj */
      /*  */
      appearance451:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelartalskj:Appearance = appearance451.
      THIS-OBJECT:Labelartalskj:Location = NEW System.Drawing.Point(356, 124).
      THIS-OBJECT:Labelartalskj:Name = "Labelartalskj".
      THIS-OBJECT:Labelartalskj:Size = NEW System.Drawing.Size(85, 23).
      THIS-OBJECT:Labelartalskj:TabIndex = 8.
      THIS-OBJECT:Labelartalskj:Text = "Årtal för avtal".
      /*  */
      /* cmb_artalavtal */
      /*  */
      THIS-OBJECT:cmb_artalavtal:CheckedListSettings:CheckStateMember = "".
      appearance452:BackColor = System.Drawing.SystemColors:Window.
      appearance452:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Appearance = appearance452.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance453:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance453:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance453:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance453:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:GroupByBox:Appearance = appearance453.
      appearance454:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance454.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance455:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance455:BackColor2 = System.Drawing.SystemColors:Control.
      appearance455:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance455:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:GroupByBox:PromptAppearance = appearance455.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:MaxRowScrollRegions = 1.
      appearance456:BackColor = System.Drawing.SystemColors:Window.
      appearance456:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:ActiveCellAppearance = appearance456.
      appearance457:BackColor = System.Drawing.SystemColors:Highlight.
      appearance457:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:ActiveRowAppearance = appearance457.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance458:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:CardAreaAppearance = appearance458.
      appearance459:BorderColor = System.Drawing.Color:Silver.
      appearance459:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:CellAppearance = appearance459.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:CellPadding = 0.
      appearance460:BackColor = System.Drawing.SystemColors:Control.
      appearance460:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance460:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance460:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance460:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:GroupByRowAppearance = appearance460.
      appearance461:TextHAlignAsString = "Left".
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:HeaderAppearance = appearance461.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance462:BackColor = System.Drawing.SystemColors:Window.
      appearance462:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:RowAppearance = appearance462.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance463:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:Override:TemplateAddRowAppearance = appearance463.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:cmb_artalavtal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:cmb_artalavtal:Location = NEW System.Drawing.Point(466, 124).
      THIS-OBJECT:cmb_artalavtal:Name = "cmb_artalavtal".
      THIS-OBJECT:cmb_artalavtal:Size = NEW System.Drawing.Size(100, 22).
      THIS-OBJECT:cmb_artalavtal:TabIndex = 7.
      /*  */
      /* ultraTextEditor2 */
      /*  */
      THIS-OBJECT:ultraTextEditor2:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 15, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:ultraTextEditor2:Location = NEW System.Drawing.Point(62, 49).
      THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2".
      THIS-OBJECT:ultraTextEditor2:Size = NEW System.Drawing.Size(601, 33).
      THIS-OBJECT:ultraTextEditor2:TabIndex = 6.
      THIS-OBJECT:ultraTextEditor2:Text = "Mallarna ska ge dig en hjälp att få med alla grundkalkylkoder!".
      /*  */
      /* avtaltext */
      /*  */
      THIS-OBJECT:avtaltext:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 15, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:avtaltext:Location = NEW System.Drawing.Point(62, 13).
      THIS-OBJECT:avtaltext:Name = "avtaltext".
      THIS-OBJECT:avtaltext:Size = NEW System.Drawing.Size(601, 33).
      THIS-OBJECT:avtaltext:TabIndex = 5.
      THIS-OBJECT:avtaltext:Text = "Har du tagit del av de kalkylmallar som finns tillgängliga?".
      /*  */
      /* Labelklgartal */
      /*  */
      appearance464:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelklgartal:Appearance = appearance464.
      THIS-OBJECT:Labelklgartal:Location = NEW System.Drawing.Point(87, 124).
      THIS-OBJECT:Labelklgartal:Name = "Labelklgartal".
      THIS-OBJECT:Labelklgartal:Size = NEW System.Drawing.Size(85, 23).
      THIS-OBJECT:Labelklgartal:TabIndex = 4.
      THIS-OBJECT:Labelklgartal:Text = "Årtal för KLG".
      /*  */
      /* cmb_KLGartal */
      /*  */
      THIS-OBJECT:cmb_KLGartal:CheckedListSettings:CheckStateMember = "".
      appearance465:BackColor = System.Drawing.SystemColors:Window.
      appearance465:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Appearance = appearance465.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance466:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance466:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance466:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance466:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:GroupByBox:Appearance = appearance466.
      appearance467:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance467.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance468:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance468:BackColor2 = System.Drawing.SystemColors:Control.
      appearance468:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance468:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:GroupByBox:PromptAppearance = appearance468.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:MaxRowScrollRegions = 1.
      appearance469:BackColor = System.Drawing.SystemColors:Window.
      appearance469:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:ActiveCellAppearance = appearance469.
      appearance470:BackColor = System.Drawing.SystemColors:Highlight.
      appearance470:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:ActiveRowAppearance = appearance470.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance471:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:CardAreaAppearance = appearance471.
      appearance472:BorderColor = System.Drawing.Color:Silver.
      appearance472:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:CellAppearance = appearance472.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:CellPadding = 0.
      appearance473:BackColor = System.Drawing.SystemColors:Control.
      appearance473:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance473:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance473:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance473:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:GroupByRowAppearance = appearance473.
      appearance474:TextHAlignAsString = "Left".
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:HeaderAppearance = appearance474.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance475:BackColor = System.Drawing.SystemColors:Window.
      appearance475:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:RowAppearance = appearance475.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance476:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:Override:TemplateAddRowAppearance = appearance476.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:cmb_KLGartal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:cmb_KLGartal:Location = NEW System.Drawing.Point(201, 124).
      THIS-OBJECT:cmb_KLGartal:Name = "cmb_KLGartal".
      THIS-OBJECT:cmb_KLGartal:Size = NEW System.Drawing.Size(100, 22).
      THIS-OBJECT:cmb_KLGartal:TabIndex = 3.
      /*  */
      /* ultraTabPageControl7 */
      /*  */
      THIS-OBJECT:ultraTabPageControl7:Controls:Add(THIS-OBJECT:frikalk).
      THIS-OBJECT:ultraTabPageControl7:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl7:Name = "ultraTabPageControl7".
      THIS-OBJECT:ultraTabPageControl7:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* frikalk */
      /*  */
      appearance477:BackColor = System.Drawing.SystemColors:Window.
      appearance477:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:frikalk:DisplayLayout:Appearance = appearance477.
      THIS-OBJECT:frikalk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:frikalk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance478:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance478:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance478:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance478:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:frikalk:DisplayLayout:GroupByBox:Appearance = appearance478.
      appearance479:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:frikalk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance479.
      THIS-OBJECT:frikalk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance480:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance480:BackColor2 = System.Drawing.SystemColors:Control.
      appearance480:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance480:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:frikalk:DisplayLayout:GroupByBox:PromptAppearance = appearance480.
      THIS-OBJECT:frikalk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:frikalk:DisplayLayout:MaxRowScrollRegions = 1.
      appearance481:BackColor = System.Drawing.SystemColors:Window.
      appearance481:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:frikalk:DisplayLayout:Override:ActiveCellAppearance = appearance481.
      appearance482:BackColor = System.Drawing.SystemColors:Highlight.
      appearance482:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:frikalk:DisplayLayout:Override:ActiveRowAppearance = appearance482.
      THIS-OBJECT:frikalk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:frikalk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance483:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:frikalk:DisplayLayout:Override:CardAreaAppearance = appearance483.
      appearance484:BorderColor = System.Drawing.Color:Silver.
      appearance484:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:frikalk:DisplayLayout:Override:CellAppearance = appearance484.
      THIS-OBJECT:frikalk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:frikalk:DisplayLayout:Override:CellPadding = 0.
      appearance485:BackColor = System.Drawing.SystemColors:Control.
      appearance485:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance485:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance485:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance485:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:frikalk:DisplayLayout:Override:GroupByRowAppearance = appearance485.
      appearance486:TextHAlignAsString = "Left".
      THIS-OBJECT:frikalk:DisplayLayout:Override:HeaderAppearance = appearance486.
      THIS-OBJECT:frikalk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:frikalk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance487:BackColor = System.Drawing.SystemColors:Window.
      appearance487:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:frikalk:DisplayLayout:Override:RowAppearance = appearance487.
      THIS-OBJECT:frikalk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance488:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:frikalk:DisplayLayout:Override:TemplateAddRowAppearance = appearance488.
      THIS-OBJECT:frikalk:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:frikalk:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:frikalk:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:frikalk:GuruContext = ?.
      THIS-OBJECT:frikalk:Location = NEW System.Drawing.Point(31, 63).
      THIS-OBJECT:frikalk:Name = "frikalk".
      THIS-OBJECT:frikalk:Size = NEW System.Drawing.Size(884, 344).
      THIS-OBJECT:frikalk:TabIndex = 0.
      THIS-OBJECT:frikalk:Text = "grid1".
      /*  */
      /* ultraTabPageControl6 */
      /*  */
      THIS-OBJECT:ultraTabPageControl6:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl6:Name = "ultraTabPageControl6".
      THIS-OBJECT:ultraTabPageControl6:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* ultraTabPageControl8 */
      /*  */
      THIS-OBJECT:ultraTabPageControl8:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl8:Name = "ultraTabPageControl8".
      THIS-OBJECT:ultraTabPageControl8:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* Kalkreg_Fill_Panel */
      /*  */
      THIS-OBJECT:Kalkreg_Fill_Panel:Controls:Add(THIS-OBJECT:Kalkyltab).
      THIS-OBJECT:Kalkreg_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:Kalkreg_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Kalkreg_Fill_Panel:Location = NEW System.Drawing.Point(8, 158).
      THIS-OBJECT:Kalkreg_Fill_Panel:Name = "Kalkreg_Fill_Panel".
      THIS-OBJECT:Kalkreg_Fill_Panel:Size = NEW System.Drawing.Size(1184, 496).
      THIS-OBJECT:Kalkreg_Fill_Panel:TabIndex = 0.
      /*  */
      /* Kalkyltab */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:Kalkyltab:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance2:BackColor = System.Drawing.SystemColors:ActiveCaption.
      THIS-OBJECT:Kalkyltab:Appearance = appearance2.
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl4).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl5).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl6).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl7).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl8).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl9).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl10).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl11).
      THIS-OBJECT:Kalkyltab:Controls:Add(THIS-OBJECT:ultraTabPageControl12).
      THIS-OBJECT:Kalkyltab:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:Kalkyltab:Name = "Kalkyltab".
      THIS-OBJECT:Kalkyltab:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:Kalkyltab:Size = NEW System.Drawing.Size(1177, 489).
      THIS-OBJECT:Kalkyltab:TabIndex = 0.
      THIS-OBJECT:Kalkyltab:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:ultraTabPageControl9.
      ultraTab13:Text = "Kalkylhuvud".
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:ultraTabPageControl1.
      ultraTab14:Text = "Kalkyl".
      ultraTab15:Key = "2".
      ultraTab15:TabPage = THIS-OBJECT:ultraTabPageControl10.
      ultraTab15:Text = "Egna koder".
      ultraTab16:TabPage = THIS-OBJECT:ultraTabPageControl11.
      ultraTab16:Text = "Visa kalkyl".
      ultraTab17:TabPage = THIS-OBJECT:ultraTabPageControl2.
      ultraTab17:Text = "Faktorer".
      ultraTab18:TabPage = THIS-OBJECT:ultraTabPageControl3.
      ultraTab18:Text = "Priser".
      ultraTab19:TabPage = THIS-OBJECT:ultraTabPageControl4.
      ultraTab19:Text = "Materiel".
      ultraTab22:TabPage = THIS-OBJECT:ultraTabPageControl5.
      ultraTab22:Text = "Import/Mallar".
      ultraTab20:TabPage = THIS-OBJECT:ultraTabPageControl12.
      ultraTab20:Text = "Avtalskalkyl".
      ultraTab21:TabPage = THIS-OBJECT:ultraTabPageControl7.
      ultraTab21:Text = "Frikalkyl".
      ultraTab23:TabPage = THIS-OBJECT:ultraTabPageControl6.
      ultraTab23:Text = "Fördelning".
      ultraTab24:TabPage = THIS-OBJECT:ultraTabPageControl8.
      ultraTab24:Text = "Volymberäkning".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 12 NO-UNDO.
      arrayvar3[1] = ultraTab13.
      arrayvar3[2] = ultraTab14.
      arrayvar3[3] = ultraTab15.
      arrayvar3[4] = ultraTab16.
      arrayvar3[5] = ultraTab17.
      arrayvar3[6] = ultraTab18.
      arrayvar3[7] = ultraTab19.
      arrayvar3[8] = ultraTab22.
      arrayvar3[9] = ultraTab20.
      arrayvar3[10] = ultraTab21.
      arrayvar3[11] = ultraTab23.
      arrayvar3[12] = ultraTab24.
      THIS-OBJECT:Kalkyltab:Tabs:AddRange(arrayvar3).
      THIS-OBJECT:Kalkyltab:Click:Subscribe(THIS-OBJECT:Kalkyltab_Click).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* KalkylToolbarsManager */
      /*  */
      THIS-OBJECT:KalkylToolbarsManager:DesignerFlags = 1.
      THIS-OBJECT:KalkylToolbarsManager:DockWithinContainer = THIS-OBJECT.
      THIS-OBJECT:KalkylToolbarsManager:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
      THIS-OBJECT:KalkylToolbarsManager:FormDisplayStyle = Infragistics.Win.UltraWinToolbars.FormDisplayStyle:RoundedSizable.
      ribbonTab1:Caption = "Start".
      ribbonGroup1:Caption = "Kalkylinfo".
      textBoxTool1:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      textBoxTool3:InstanceProps:MinimumSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      textBoxTool3:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar4[1] = textBoxTool1.
      arrayvar4[2] = textBoxTool3.
      ribbonGroup1:Tools:AddRange(arrayvar4).
      ribbonGroup2:Caption = "Visa/Skriv ut".
      ribbonGroup3:Caption = "Uppföljning".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 3 NO-UNDO.
      arrayvar5[1] = ribbonGroup1.
      arrayvar5[2] = ribbonGroup2.
      arrayvar5[3] = ribbonGroup3.
      ribbonTab1:Groups:AddRange(arrayvar5).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
      arrayvar6[1] = ribbonTab1.
      THIS-OBJECT:KalkylToolbarsManager:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar6).
      THIS-OBJECT:KalkylToolbarsManager:Ribbon:Visible = TRUE.
      THIS-OBJECT:KalkylToolbarsManager:ShowFullMenusDelay = 500.
      textBoxTool2:SharedPropsInternal:Caption = "ben".
      textBoxTool4:SharedPropsInternal:Caption = "kalknr".
      textBoxTool4:SharedPropsInternal:CustomizerCaption = "kalknr".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar7[1] = textBoxTool2.
      arrayvar7[2] = textBoxTool4.
      THIS-OBJECT:KalkylToolbarsManager:Tools:AddRange(arrayvar7).
      /*  */
      /* m_Kalkreg_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 158).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:Name = "m_Kalkreg_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 496).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:KalkylToolbarsManager.
      /*  */
      /* m_Kalkreg_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(1192, 158).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:Name = "m_Kalkreg_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 496).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:KalkylToolbarsManager.
      /*  */
      /* m_Kalkreg_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:Name = "m_Kalkreg_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(1200, 158).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:KalkylToolbarsManager.
      /*  */
      /* m_Kalkreg_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 654).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:Name = "m_Kalkreg_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(1200, 8).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:KalkylToolbarsManager.
      /*  */
      /* ultraTextEditor3 */
      /*  */
      THIS-OBJECT:ultraTextEditor3:Location = NEW System.Drawing.Point(131, 207).
      THIS-OBJECT:ultraTextEditor3:Name = "ultraTextEditor3".
      THIS-OBJECT:ultraTextEditor3:Size = NEW System.Drawing.Size(100, 21).
      THIS-OBJECT:ultraTextEditor3:TabIndex = 12.
      THIS-OBJECT:ultraTextEditor3:Text = " ".
      /*  */
      /* ultraTabControl1 */
      /*  */
      DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1".
      THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage2.
      THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(200, 100).
      THIS-OBJECT:ultraTabControl1:TabIndex = 0.
      /*  */
      /* ultraTabSharedControlsPage2 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage2:Location = NEW System.Drawing.Point(1, 20).
      THIS-OBJECT:ultraTabSharedControlsPage2:Name = "ultraTabSharedControlsPage2".
      THIS-OBJECT:ultraTabSharedControlsPage2:Size = NEW System.Drawing.Size(196, 77).
      /*  */
      /* Kalkreg */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1200, 662).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:Kalkreg_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Name = "Kalkreg".
      THIS-OBJECT:Text = "KALKYL".
      THIS-OBJECT:Load:Subscribe(THIS-OBJECT:Kalkreg_Load).
      THIS-OBJECT:VisaExcelTab:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl16:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl17:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl9:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl9:PerformLayout().
      CAST(THIS-OBJECT:cmbKata, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmbUtf, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmbOmr, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:personalkalkyl, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:anm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl1:PerformLayout().
      CAST(THIS-OBJECT:Decimaltest2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:lopkoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:arbkoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:kalkylkoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl10:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel1:PerformLayout().
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:PerformLayout().
      CAST(THIS-OBJECT:egnakoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer2:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:prisposter, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer3:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer3:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer3:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:valdaprisposter, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl11:ResumeLayout(FALSE).
      THIS-OBJECT:VisaDividerX:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:VisaDividerX:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:VisaDividerX:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabControl2:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelSettings:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelSettings:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:VisaGroupBoxTyp, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:VisaGroupBoxTyp:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:VisaKalkylTyp, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaInklGroupBox, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:VisaInklGroupBox:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:VisaInklOFakt, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaInklOEP, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaInklKAnm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaInklAnm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaInklHuvud, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:VisaFunkGrupp, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaFunkSum, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:egnafaktorer, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:egnapriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl4:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:egetmtrl, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:valmtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl5:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mallarkoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:valdamallar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:mallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl12:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl12:PerformLayout().
      CAST(THIS-OBJECT:cmb_avtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmb_artalavtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:avtaltext, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmb_KLGartal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabPageControl7:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:frikalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:Kalkreg_Fill_Panel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Kalkyltab, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:Kalkyltab:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:KalkylToolbarsManager, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTextEditor3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID Kalkreg_Load( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.




	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Kalkyltab_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	    
	   kalkyltabkontroll ().
	
		RETURN. 

	END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID kalkyltabkontroll( ):
      DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
      DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
      
      IF frantab = "Kalkylhuvud" THEN DO:
         felvar = FALSE.
         ASSIGN 
         kalkhuvtt.BENAMNING = fillinCharKalkBen:GuruText:Text
         kalkhuvtt.KALKNR = INTEGER(fillinIntegerKalknr:GuruText:Text)
         kalkhuvtt.KALKANV = fillinCharAnsvarig:GuruText:Text
         kalkhuvtt.TYPKALK = cmbTyp:GuruCombo:VALUE 
         kalkhuvtt.OMRADE = cmbOmr:VALUE
         kalkhuvtt.KLOGID = cmbKata:VALUE  
         kalkhuvtt.ANVANDARE = cmbUtf:VALUE.
         RUN sparakalkhuv_UI IN kalkberh (OUTPUT felvar,OUTPUT felmedd, INPUT-OUTPUT TABLE  kalkhuvtt). 
         FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.               
         IF felvar = TRUE THEN DO:
             ASSIGN
             kalkyltab:Tabs[frantab]:Selected = TRUE          
             frantab = STRING(Kalkyltab:ActiveTab)
             felvar = FALSE.
             MESSAGE felmedd  VIEW-AS ALERT-BOX.
             RETURN. 
          END.
          ASSIGN 
          fillinIntegerKalknr:GuruText:Text = STRING(kalkhuvtt.KALKNR)
          fillinIntegerKalknr:VISIBLE = TRUE 
          frantab = Kalkyltab:ActiveTab:TEXT.          
            
      END. 
      IF Kalkyltab:ActiveTab:TEXT = "Kalkyl"  THEN DO:
         
         hamtatabstart( ).
      END.   
      IF Kalkyltab:ActiveTab:TEXT = "Visa kalkyl"  THEN DO:
         IF excelprog = ? THEN DO:
            excelprog = NEW GuruStart.Excelfunc().
            THIS-OBJECT:VisaExcel:Initialize(). /* <-- Initierar Excelobjektet.*/ 
         END.   
      
      END.
      IF Kalkyltab:ActiveTab:TEXT = "Egna koder"  THEN DO:        
         IF AVAILABLE kalknumtt THEN DO:
            egnavaluech(?,?).               
         END.                      
      END.   
      frantab = Kalkyltab:ActiveTab:TEXT.  
   END METHOD.
   
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID hamtatabstart( ):
      FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE kalkhuvtt THEN DO:
         kalknrvar = kalkhuvtt.KALKNR.
         omradevar = kalkhuvtt.OMRADE. 
      END.
      FIND FIRST kalkylarbkodertt WHERE NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE kalkylarbkodertt THEN DO:         
         RUN kathmt_UI IN kalkberh (INPUT-OUTPUT kalknrvar,INPUT omradevar,OUTPUT TABLE kalkylarbkodertt,OUTPUT TABLE kalkylloppostertt, 
         OUTPUT TABLE kalkhuvtt, OUTPUT TABLE kalknumtt, OUTPUT TABLE kalkylkatalogtt).
         FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR. 
         arbkodgrid ().
         lopnrgrid ().
         kalkkodergrid ().
         egnakodergrid ().          
         arbkoder:GurufirstRow().                             
      END.
      MESSAGE "2"
      VIEW-AS ALERT-BOX.
      FOR EACH kalkylloppostertt WHERE TRIM(kalkylloppostertt.ARBKOD) = "15":
         DELETE kalkylloppostertt.
      END.   
      FIND FIRST kalkylprisertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylprisertt THEN DO:
         RUN kphmt IN kalkberh (INPUT kalkhuvtt.KLOGID, OUTPUT TABLE kalkylprisertt).
         kalkprisergrid ().
         prisposter:GuruFirstrow().
      END.
      
      /* Hämta kalknumsubbarna om det behövs och initiera grid */
      FIND FIRST kalknumsubtt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalknumsubtt THEN  DO:
         RUN snhmt IN kalkberh (OUTPUT TABLE kalknumsubtt, INPUT kalknrvar, INPUT omradevar).
         valdaprisergrid ().
         valdaprisposter:GuruFirstrow().         
      END.
      
      buttonOverStart ().  
                                  
      ASSIGN
      cmbKata:ENABLED = FALSE
      cmbOmr:ENABLED  = FALSE
      cmbTyp:ENABLED  = FALSE.
      
   END METHOD.
	
	
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraTabPageControl9_Paint( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.PaintEventArgs ):
		
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID allstart ():        
	           
     IF GuruStart.Konstanter:appcon THEN DO:
         RUN KALKYLBERAPP.P PERSISTENT SET kalkberh ON GuruStart.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN KALKYLBERAPP.P PERSISTENT SET kalkberh.
      END.        
               
   END METHOD.
   METHOD PRIVATE VOID mainblock ():
       
      decimalprog = NEW Controls.FillinDecimal().
 
      /*def av grid*/ 
      FIND LAST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = kalkhuvtt.KLOGID NO-LOCK NO-ERROR. 
             
      personalgrid ().
      Typcmb ().
      Omrcmb ().
      Katalogcmb ().        
      Utfcmb (). 
      
      fillinCharKalkBen:GuruText:GuruKontrollStart = TRUE.
      fillinCharKalkBen:GuruText:GuruKontrollText = TRUE.
/*ccc
      ASSIGN
      Labelkoppling:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 14, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
 */
 
      ASSIGN
      Labelomr:TEXT = GuruStart.Konstanter:gomrk
      
      fillinCharKalkBen:GuruLabel:Text = "Benämning"
      fillinCharKalkBen:GuruText:Text = kalkhuvtt.BENAMNING
      
      fillinIntegerKalknr:GuruLabel:Text = "Kalkyl nr"
      fillinIntegerKalknr:GuruText:Text = STRING(kalkhuvtt.KALKNR)
      fillinIntegerKalknr:GuruText:Enabled = FALSE          
      
      cmbTyp:GuruLabel:Text = "Kalkyl Typ"
      
      fillinCharAnsvarig:GuruLabel:Text = "Kalkyl ansvarig"
      fillinCharAnsvarig:GuruText:Text = kalkhuvtt.KALKANV.
     
      
   /*
      
      toolz = NEW System.Windows.Forms.ToolTip().      
      toolz:AutoPopDelay = 5000.
      toolz:InitialDelay = 1000.
      toolz:ReshowDelay = 500.
      toolz:ShowAlways = TRUE.
      toolz:SetToolTip(THIS-OBJECT:fillinCharKalkBen:GuruText, "Hej hej").
   */
      
      
      
   END METHOD.
	METHOD PRIVATE VOID Typcmb  ():
	   DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK AS Controls.GridRubrik NO-UNDO.
          
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).                    
      
      IF GuruStart.Konstanter:varforetypval[1] = 1 OR GuruStart.Konstanter:varforetypval[1] = 3 THEN DO:
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 1
         typtt.TYPC = "Typ 1".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 2
         typtt.TYPC = "Typ 2".
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 3
         typtt.TYPC = "Typ 3".           
      END.
      /*kalkfor*/
      IF GuruStart.Konstanter:globforetag = "ELPA" OR GuruStart.Konstanter:globforetag = "GRAN" OR GuruStart.Konstanter:globforetag = "VAST" THEN DO: 
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 5
         typtt.TYPC = "Sam B/F".
         
      END.
      IF GuruStart.Konstanter:varforetypval[41] = 1 THEN DO:  
         CREATE typtt.
         ASSIGN 
         typtt.TYPKALK = 7
         typtt.TYPC = "Nätreg N1".      
      END.
     
     /*
      cmbTyp:GuruCombo:GuruInitCombo(TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE, "TYPKALK", "TYPC").
      */
      cmbTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
      cmbTyp:GuruCombo:VALUE = kalkhuvtt.TYPKALK.
   END METHOD.
   METHOD PRIVATE VOID Omrcmb ():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE omrnamnK AS Controls.GridRubrik NO-UNDO.

      GuruStart.TempTabeller:OmrTemp().
      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = GuruStart.TempTabeller:omrttH.
      IF GuruStart.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = GuruStart.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  

      cmbOmr:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").

      cmbOmr:VALUE = kalkhuvtt.OMRADE.

   END METHOD.
   METHOD PRIVATE VOID katalogcmb ():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK AS Controls.GridRubrik NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = TEMP-TABLE kalkylkatalogtt:DEFAULT-BUFFER-HANDLE.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      /*             
      cmbKata:GuruInitCombo(cmbkataloglist, "KLOGID", "BENAMNING").
      */
      cmbKata:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").           
      cmbKata:VALUE = kalkhuvtt.KLOGID.
   END METHOD.
   
   METHOD PRIVATE VOID valuech (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/  
      MESSAGE "1"
      VIEW-AS ALERT-BOX.  
     lopkoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + kalkylarbkodertt.ARBKOD + "'").
     lopkoder:GuruFirstrow().
               
   END METHOD.
   METHOD PRIVATE VOID valuechpers (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/    
     fillinCharAnsvarig:GuruText:Text = GuruStart.TempTabeller:persttH:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
           
   END METHOD.
   METHOD PRIVATE VOID valuechkalkkoder (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/ 
      DEFINE VARIABLE exrid AS RECID NO-UNDO.  
      IF kalknumtt.ARBKOD = "EGEN" THEN DO: 
         exrid = kalknumtt.TTRECID.           
         egnakoder:GuruRepositionTo(ROWID(kalknumtt)).
         FIND FIRST kalknumtt WHERE kalknumtt.TTRECID = exrid NO-LOCK NO-ERROR.
         egnavaluech(?,?).  
      END. 
           
   END METHOD.
   
   METHOD PRIVATE VOID egnavaluech (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/    
          /*
      valdaprisposter:GuruFiltrera( "NUM = " + INTEGER egnakoder:ActiveRow:Cells["NUM"]:Text).
      */
      valdaprisposter:GuruFiltrera( "NUM = " + STRING(kalknumtt.NUM)).
      valdaprisposter:GuruFirstrow().
   END METHOD.
   
   METHOD PRIVATE VOID Utfcmb ():
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cmbUtflist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE UtfK AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE UtfnamnK AS Controls.GridRubrik NO-UNDO.
      GuruStart.TempTabeller:AnvTemp().
      cmbUtflist = NEW Controls.GridRubrikLista().
      cmbUtflist:ttBufferHandle = GuruStart.TempTabeller:anvttH.
      cmbUtflist:filter = "anvandartemp.AV-lEVEL > 0".
      
      UtfK = NEW Controls.GridRubrik().
      UtfK:Falt = "ANVANDARE".
      UtfK:Rubrik = "Användare".
      cmbUtflist:addRubrik(UtfK).
      UtfnamnK = NEW Controls.GridRubrik().
      UtfnamnK:Falt = "AV-NAMN".
      UtfnamnK:Rubrik = "Namn".
      cmbUtflist:addRubrik(UtfnamnK).       
      cmbUtf:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
      
      villkor = "WHERE ANVANDARE = '" + kalkhuvtt.ANVANDARE + "'".                 
      GuruStart.TempTabeller:anvttH:FIND-FIRST(villkor). 
      IF GuruStart.TempTabeller:anvttH:BUFFER-FIELD("AV-LEVEL"):BUFFER-VALUE = 0 THEN DO:
         villkor = "WHERE AV-LEVEL > 0".             
         GuruStart.TempTabeller:anvttH:FIND-FIRST(villkor).
      END.    
      cmbUtf:VALUE = GuruStart.TempTabeller:anvttH:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      
   END METHOD.
  
   METHOD PRIVATE VOID skapkalkposter ():
      DEFINE VARIABLE ttrec AS RECID NO-UNDO.
      DEFINE VARIABLE ttrow AS ROWID NO-UNDO.
      DEFINE VARIABLE rhand AS HANDLE NO-UNDO.
      DEFINE VARIABLE rCells AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
      DEFINE VARIABLE ttr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.           
      DEFINE VARIABLE rCurrentRow AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO.
      DEFINE VARIABLE radkalkyltyp AS INTEGER NO-UNDO.
      EMPTY TEMP-TABLE ekalknumtt NO-ERROR.  
       /*
      {WALLMAN\foreach.i System.Object oObject in kalkylkoder:Selected:Rows}      
         rCurrentRow = CAST (oObject, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .             
         rCells = rCurrentRow:Cells.      
         ttr = rCells["TTRECID"]:VALUE.
         villkor = "WHERE " + "TTRECID = " + ttr.                              
         kalkylkoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor).                                                  
         RUN sistanum_UI IN kalkberh (OUTPUT kalknumtt.NUM).  
         ASSIGN
         kalknumtt.KALKNR = kalkhuvtt.KALKNR
         kalknumtt.OMRADE = kalkhuvtt.OMRADE
         kalknumtt.TYPKALK = radkalkyltyp
         kalknumtt.MATRIS = 1
         kalknumtt.ANTAL = 1.
         
         CREATE ekalknumtt.
         BUFFER-COPY kalknumtt TO ekalknumtt.
        
         RUN sparanum_UI IN kalkberh (INPUT TABLE ekalknumtt, INPUT kalkylloppostertt.KLOGSUBID, OUTPUT kalknumtt.TOTKOST ).
         EMPTY TEMP-TABLE ekalknumtt NO-ERROR.   
      END. 
      */
      
      FOR EACH kalknumtt WHERE kalknumtt.MATRIS = 0 NO-LOCK:                                                   
         RUN sistanum_UI IN kalkberh (OUTPUT kalknumtt.NUM).  
         ASSIGN
         kalknumtt.KALKNR = kalkhuvtt.KALKNR
         kalknumtt.OMRADE = kalkhuvtt.OMRADE
         kalknumtt.TYPKALK = radkalkyltyp
         kalknumtt.MATRIS = 1
         kalknumtt.ANTAL = 1.
         
         CREATE ekalknumtt.
         BUFFER-COPY kalknumtt TO ekalknumtt.
         ttrec = kalknumtt.TTRECID.
         ttrow = ROWID(kalknumtt).
         RUN sparanum_UI IN kalkberh (INPUT TABLE ekalknumtt, INPUT kalkylloppostertt.KLOGSUBID, OUTPUT kalknumtt.TOTKOST ).
         EMPTY TEMP-TABLE ekalknumtt NO-ERROR.                 
      END.
      
      /*foreach end*/
      kalkylkoder:GuruReopen(). 
      buttonOverMark:GuruRadvaljareU(ttrow).
      kalkylkoder:GuruReopen().                    
   END METHOD.
   
   
   METHOD PRIVATE VOID skapegnaposter ():
      DEFINE VARIABLE ttrec          AS RECID NO-UNDO.
      DEFINE VARIABLE ttrow          AS ROWID NO-UNDO.
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(egnakoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR.
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = 0 NO-LOCK:                                                   
         ASSIGN
         kalknumsubtt.KALKNR = kalknumtt.KALKNR
         kalknumsubtt.OMRADE = kalknumtt.OMRADE
         kalknumsubtt.NUM =  kalknumtt.NUM.
         ttrec = kalknumsubtt.TTRECID.
         ttrow = ROWID(kalknumsubtt).
         IF kalknumsubtt.PRIS NE 0 THEN kalknumsubtt.TIMMAR = 1.
                
         CREATE ekalknumsubtt.
         BUFFER-COPY kalknumsubtt TO ekalknumsubtt.
         RUN sparanumsub_UI IN kalkberh (INPUT TABLE ekalknumsubtt, OUTPUT kalknumtt.TOTKOST  ).
         
         
         EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
         
      END.
      egnakoder:GuruReopen().
      valdaprisposter:GuruReopen().
     
      buttonOverMarkPriser:GuruRadvaljareU(ttrow).
      kalkylkoder:GuruReopen().
      /*
      FIND FIRST kalknumsubtt  WHERE kalknumsubtt.TTRECID = ttrec  NO-LOCK NO-ERROR.
      valdaprisposter:GuruRepositionTo(ROWID(kalknumsubtt)).
      */
            
      /*
      DEFINE VARIABLE rhand        AS HANDLE                                        NO-UNDO.
      DEFINE VARIABLE rCells       AS Infragistics.Win.UltraWinGrid.CellsCollection NO-UNDO.
      DEFINE VARIABLE ttr          AS CHARACTER                                     NO-UNDO.
      DEFINE VARIABLE villkor      AS CHARACTER                                     NO-UNDO.           
      DEFINE VARIABLE rCurrentRow  AS Infragistics.Win.UltraWinGrid.UltraGridRow    NO-UNDO.
      DEFINE VARIABLE radkalkyltyp AS INTEGER                                       NO-UNDO.
      EMPTY TEMP-TABLE ekalknumtt NO-ERROR.
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      {WALLMAN\foreach.i System.Object oObject in valdaprisposter:Selected:Rows}      
         rCurrentRow = CAST (oObject, Infragistics.Win.UltraWinGrid.UltraGridRow) NO-ERROR .             
         rCells = rCurrentRow:Cells.      
         ttr = rCells["TTRECID"]:VALUE.
         villkor = "WHERE " + "TTRECID = " + ttr.                              
         valdaprisposter:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor).  
         hittnum =  INTEGER(egnakoder:ActiveRow:Cells["NUM"]:TEXT).
         FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR.                                               
         ASSIGN
         kalknumsubtt.KALKNR = kalknumtt.KALKNR
         kalknumsubtt.OMRADE = kalknumtt.OMRADE
         kalknumsubtt.NUM =  kalknumtt.NUM.
         IF kalknumsubtt.PRIS NE 0 THEN kalknumsubtt.TIMMAR = 1.
                
         CREATE ekalknumsubtt.
         BUFFER-COPY kalknumsubtt TO ekalknumsubtt.
.
         RUN sparanumsub_UI IN kalkberh (INPUT TABLE ekalknumsubtt, OUTPUT kalknumtt.TOTKOST  ).
         
         
         EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
            
      END. /*foreach end*/
      
      */
                      
   END METHOD.
   
   
   METHOD PRIVATE VOID bortkalkpostermark ():                    
      EMPTY TEMP-TABLE ekalknumtt NO-ERROR.        
       {WALLMAN\foreachultra.i System.Object oObject in kalkylkoder:Selected:Rows}                                         
         bortkalkposterrader (villkor).
   END. /*foreach end*/     
   END METHOD.
   
   
   METHOD PRIVATE VOID bortkalkprisermark ():                    
      DEBUGGER:SET-BREAK().
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.     
      {WALLMAN\foreachultra.i System.Object oObject in valdaprisposter:Selected:Rows}                                         
         bortkalkprisrader (villkor).
      END. /*foreach end*/     
   END METHOD.
   
   METHOD PRIVATE VOID bortkalkpriseralla ():                    
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
       {WALLMAN\foreachultra.i System.Object oObject in valdaprisposter:Rows}                                         
         bortkalkprisrader (villkor).
      END. /*foreach end*/     
   END METHOD.
   
   METHOD PRIVATE VOID bortkalkposteralla ():                    
      EMPTY TEMP-TABLE ekalknumtt NO-ERROR.  
       {WALLMAN\foreachultra.i System.Object oObject in kalkylkoder:Rows}                                         
         bortkalkposterrader (villkor).
      END. /*foreach end*/     
   END METHOD.

   METHOD PRIVATE VOID bortkalkposterrader (villkor AS CHARACTER):
      kalkylkoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor).
      CREATE ekalknumtt.
      BUFFER-COPY kalknumtt TO ekalknumtt.
      RUN bortnumclient_UI IN kalkberh (INPUT TABLE ekalknumtt). 
      EMPTY TEMP-TABLE ekalknumtt NO-ERROR.      
   END METHOD.
   
   METHOD PRIVATE VOID bortkalkprisrader (villkor AS CHARACTER):
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(egnakoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR. 
      valdaprisposter:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor).
      CREATE ekalknumsubtt.
      BUFFER-COPY kalknumsubtt TO ekalknumsubtt.
      RUN bortnumsubclient_UI IN kalkberh (INPUT TABLE ekalknumsubtt, OUTPUT kalknumtt.TOTKOST). 
      
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
         
   END METHOD.
   
   
	DESTRUCTOR PUBLIC Kalkreg ( ):
	   IF VALID-HANDLE(kalkberh) THEN DELETE PROCEDURE kalkberh.
      IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	METHOD PRIVATE VOID personalgrid ():
	   DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      DEFINE VARIABLE personallist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE personalPKOD AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE personalFNAMN AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE personalENAMN AS Controls.GridRubrik NO-UNDO. 
      
      GuruStart.TempTabeller:PersTemp().                             
      personallist = NEW Controls.GridRubrikLista().
      personallist:ttBufferHandle = GuruStart.TempTabeller:persttH.           
      personalPKOD = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      personallist:addRubrik(personalPKOD).
      personalFNAMN = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      personallist:addRubrik(personalFNAMN).
      personalENAMN = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      personallist:addRubrik(personalENAMN).
      /*init av grid eg sparar alla egenskaper mm*/   
      personalkalkyl:guruInitGrid(personallist).  
      personalkalkyl:Text = "Välj Kalkyl Ansvarig!".
       /*
      personalkalkyl:GurufirstRow().
      */ 
      villkor = "WHERE PERSONALKOD = '" + kalkhuvtt.KALKANV + "'".                 
      GuruStart.TempTabeller:persttH:FIND-FIRST(villkor). 
      IF GuruStart.TempTabeller:persttH:AVAILABLE THEN DO:                   
         personalkalkyl:GuruRepositionTo(GuruStart.TempTabeller:persttH:rowid).              
      END.   
       /*egna event*/
      personalkalkyl:tBS:PositionChanged:Subscribe(valuechpers).
      
   END METHOD.
   METHOD PRIVATE VOID arbkodgrid ():
      DEFINE VARIABLE arbkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE arbkoderArb AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE arbkoderBen AS Controls.GridRubrik NO-UNDO.
                              
      arbkoderlist = NEW Controls.GridRubrikLista().
      arbkoderlist:ttBufferHandle = TEMP-TABLE kalkylarbkodertt:DEFAULT-BUFFER-HANDLE.
      arbkoderlist:filter = "kalkylarbkodertt.TYPKALK = " + STRING(kalkhuvtt.TYPKALK).
      
      arbkoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,35).
      arbkoderlist:addRubrik(arbkoderArb).
      arbkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      arbkoderlist:addRubrik(arbkoderBen).
      /*init av grid eg sparar alla egenskaper mm*/   
      arbkoder:guruInitGrid(arbkoderlist).  
      arbkoder:TEXT = "Välj Arbetskoder!".      
       /*egna event*/
      arbkoder:tBS:PositionChanged:Subscribe(valuech).
   END METHOD.
   METHOD PRIVATE VOID lopnrgrid ():
      DEFINE VARIABLE lopkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE lopkoderLop AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE lopkoderEnh AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE lopkoderBen AS Controls.GridRubrik NO-UNDO.
      
      lopkoderlist = NEW Controls.GridRubrikLista().
      lopkoderlist:ttBufferHandle = TEMP-TABLE kalkylloppostertt:DEFAULT-BUFFER-HANDLE.
      lopkoderlist:multiselect = TRUE.   
      
      lopkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35).
      lopkoderlist:addRubrik(lopkoderLop).
      lopkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      lopkoderlist:addRubrik(lopkoderBen).
      lopkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      lopkoderlist:addRubrik(lopkoderEnh).
      /*init av grid*/            
      lopkoder:guruInitGrid(lopkoderlist). 
      lopkoder:TEXT = "Välj Löpnummer!".  
      /*default filter  sortering*/      
      lopkoder:GuruOrderby("BY ARBKOD BY LOPNR").  
      lopkoder:GuruFirstrow().
     
   END METHOD.
   METHOD PRIVATE VOID kalkkodergrid ():
      DEFINE VARIABLE kalkylkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb AS Controls.GridRubrik NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kalkylkoderMat AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE kalkylkoderSum AS Controls.GridRubrik NO-UNDO.
      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = TEMP-TABLE kalknumtt:DEFAULT-BUFFER-HANDLE.
      kalkylkoderlist:multiselect = TRUE.  
      kalkylkoderMat = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35).
      
      kalkylkoderlist:addRubrik(kalkylkoderMat). 
      kalkylkoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,55).      
      kalkylkoderlist:addRubrik(kalkylkoderArb).
      kalkylkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      kalkylkoderlist:addRubrik(kalkylkoderLop).
      kalkylkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      kalkylkoderlist:addRubrik(kalkylkoderEnh).
      
      kalkylkoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45,"DEC").

    
      
      /*
      kalkylkoderAnt:dectyp().
           kalkylkoderAnt:Gurudec:visaDEC ( ).
      kalkylkoderAnt:GuruText:TextDecimal().
      kalkylkoderAnt:GuruText:GuruTextRensa().
      */
/*
      kalkylkoderAnt:Controls:TextDecimal().
      */
      kalkylkoderlist:addRubrik(kalkylkoderAnt).
      kalkylkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      kalkylkoderlist:addRubrik(kalkylkoderBen).
      kalkylkoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      kalkylkoderlist:addRubrik(kalkylkoderSum).
       /*init av grid*/
      kalkylkoder:guruInitGrid(kalkylkoderlist). 
      kalkylkoder:TEXT = "Valda Kalkylkoder!".     
      /*default filter  sortering*/     
      /*
      kalkylkoder:GuruOrderby("BY MATRIS BY ARBKOD BY LOPNR BY NUM").
      */
      kalkylkoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      kalkylkoder:tBS:PositionChanged:Subscribe(valuechkalkkoder).
      kalkylkoder:GuruFirstrow().
   END METHOD.
   
   
   METHOD PRIVATE VOID egnakodergrid():
      DEFINE VARIABLE egnakoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE egnakoderArb  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderNum  AS Controls.GridRubrik      NO-UNDO.      
      DEFINE VARIABLE egnakoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderBen  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderMat  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderAnt  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE egnakoderSum  AS Controls.GridRubrik      NO-UNDO.
      egnakoderlist = NEW Controls.GridRubrikLista().
      egnakoderlist:ttBufferHandle = TEMP-TABLE kalknumtt:DEFAULT-BUFFER-HANDLE.
      egnakoderlist:multiselect = TRUE.  
      egnakoderlist:filter = "kalknumtt.ARBKOD = 'EGEN'".
      egnakoderNum = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      egnakoderlist:addRubrik(egnakoderNum). 
      egnakoderMat = NEW Controls.GridRubrik("MATRIS","Matris",FALSE,35).
      egnakoderlist:addRubrik(egnakoderMat). 
      egnakoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,55).      
      egnakoderlist:addRubrik(egnakoderArb).
      egnakoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      egnakoderlist:addRubrik(egnakoderLop).
      egnakoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      egnakoderlist:addRubrik(egnakoderEnh).
      egnakoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45).
      egnakoderlist:addRubrik(egnakoderAnt).
      egnakoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      egnakoderlist:addRubrik(egnakoderBen).
      egnakoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      egnakoderlist:addRubrik(egnakoderSum).
      /*init av grid*/
      egnakoder:guruInitGrid(egnakoderlist). 
      egnakoder:TEXT = "Egna koder!".     
      /*default filter  sortering*/     
      egnakoder:GuruOrderby("BY MATRIS BY ARBKOD BY LOPNR BY NUM").
      egnakoder:tBS:PositionChanged:Subscribe(egnavaluech).
      egnakoder:GuruFirstrow().
      RETURN.   
   END METHOD.
   
   METHOD PRIVATE VOID kalkprisergrid():
      DEFINE VARIABLE kalkpriserlist     AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkpriserKPID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserKVID     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserSokB     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserBenm     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserPris     AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkpriserEgenPris AS Controls.GridRubrik      NO-UNDO.
      
      kalkpriserlist = NEW Controls.GridRubrikLista().
      kalkpriserlist:ttBufferHandle = TEMP-TABLE kalkylprisertt:DEFAULT-BUFFER-HANDLE.
      kalkpriserlist:multiselect = TRUE.
      kalkpriserlist:filter = "EGENKODUPP = TRUE".
      kalkpriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 150).
      kalkpriserlist:addRubrik(kalkpriserBenm).
      
      kalkpriserPris = NEW Controls.GridRubrik("PRIS", "Pris", TRUE, 35).
      kalkpriserlist:addRubrik(kalkpriserPris).
      prisposter:guruInitGrid(kalkpriserlist).
      prisposter:TEXT = "Kalkylpriser".
      prisposter:GuruOrderby("BY BENAMNING").
      prisposter:GuruFirstrow().
      RETURN.
   END METHOD.
   
   METHOD PRIVATE VOID valdaprisergrid():
      DEFINE VARIABLE valdapriserlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE valdapriserBenm    AS Controls.GridRubrik      NO-UNDO.
      
      DEFINE VARIABLE valdapriserTimmar  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserPris    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserKostnad AS Controls.GridRubrik      NO-UNDO.
      valdapriserlist = NEW Controls.GridRubrikLista().
      valdapriserlist:ttBufferHandle = TEMP-TABLE kalknumsubtt:DEFAULT-BUFFER-HANDLE.
      valdapriserlist:multiselect = TRUE.
      valdapriserlist:filter = "".
      /*
      DEFINE VARIABLE valdapriserNum     AS Controls.GridRubrik      NO-UNDO.
      valdapriserNum = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      valdapriserlist:AddRubrik(valdapriserNUM).
      */
      DEFINE VARIABLE valdapriserNum     AS Controls.GridRubrik      NO-UNDO.
      valdapriserNum = NEW Controls.GridRubrik("NUM", "#", TRUE ,35).
      valdapriserlist:AddRubrik(valdapriserNUM).
      valdapriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,150).
      valdapriserlist:AddRubrik(valdapriserBenm).
      
      valdapriserTimmar = NEW Controls.GridRubrik("TIMMAR", "Antal", FALSE ,35).
      valdapriserlist:AddRubrik(valdapriserTimmar).
      valdapriserPris = NEW Controls.GridRubrik("PRIS", "Pris", FALSE ,35).
      valdapriserlist:AddRubrik(valdapriserPris).
      valdapriserKostnad = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", TRUE ,100).
      valdapriserlist:AddRubrik(valdapriserKostnad).
      valdaprisposter:guruInitGrid(valdapriserlist).
      valdaprisposter:TEXT = "Valda priser!".
      valdaprisposter:GuruOrderby("BY BENAMNING").
      valdaprisposter:GuruFirstrow().
      RETURN.
   END METHOD.
   
   METHOD PRIVATE VOID  buttonOverStart ():
      buttonOvermark:GuruKoppla(lopkoder,kalkylkoder).
      buttonOvermark:GuruDublett = TRUE.
      buttonOverAlla:GuruKoppla(lopkoder,kalkylkoder).
      buttonOverAlla:GuruDublett = TRUE.
      buttonOverAlla:GuruAllaover = TRUE.
      
      buttonBackMark:GuruKoppla(kalkylkoder,lopkoder).
      buttonBackMark:GuruRemoveFrom = TRUE.  
      buttonBackAlla:GuruKoppla(kalkylkoder,lopkoder).              
      buttonBackAlla:GuruRemoveFrom = TRUE.
      buttonBackAlla:GuruAllaover = TRUE.
           
           
      /* Egna koder börjar här*/
      buttonOvermarkPriser:GuruKoppla(prisposter,valdaprisposter).
      buttonOvermarkPriser:GuruDublett = FALSE.
      buttonOverAllaPriser:GuruKoppla(prisposter, valdaprisposter).
      buttonOverAllaPriser:GuruDublett = FALSE.
      buttonOverAllaPriser:GuruAllaover = TRUE.
      
      buttonBackMarkPriser:GuruKoppla(valdaprisposter, prisposter).
      buttonBackMarkPriser:GuruRemoveFrom = TRUE.
      buttonBackAllaPriser:GuruKoppla(valdaprisposter, prisposter).
      buttonBackAllaPriser:GuruRemoveFrom = TRUE.
      buttonBackAllaPriser:GuruAllaover = TRUE.
      
   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID VisaGenerate_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		VisaExcel:OpenFileWithDialog().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID VisaReset_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      VisaPDFBrowser:Navigate("D:\Test.pdf").
      VisaIEBrowser:Navigate("D:\Test.html").
      VisaExcel:OpenFile("D:\Test.xlsx").
		RETURN.

	END METHOD.

END CLASS.