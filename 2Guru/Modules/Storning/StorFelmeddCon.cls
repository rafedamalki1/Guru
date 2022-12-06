 
 /*------------------------------------------------------------------------
    File        : KalkFelmedd
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 19:01:24 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Storning.StorFelmeddCon INHERITS UserControl: 
	
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerFelMedd AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFelMedd AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE PUBLIC VARIABLE ControlDataDb                AS Modules.Storning.StorningDb NO-UNDO.	
	DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
	CONSTRUCTOR PUBLIC StorFelmeddCon (INPUT r AS Guru.Root   ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:Storning.
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:CreateForm().  
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      
   END METHOD.
    METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridFelMedd().
   END METHOD. 
   METHOD PUBLIC VOID CreateGridFelMedd():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("SMEDD","Meddelande",TRUE,600).
      rubrikergrid[2] = NEW Controls.GridRubrik("LAGGTILL","Uppdatera/Lägg till",False,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("SKRIVOVER","Radera gamla poster och skriv över",False,200).
      rubrikergrid[4] = NEW Controls.GridRubrik("TABORT","Läs inte in denna post",False,200).
      THIS-OBJECT:GridFelMedd:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:ControlDataDb:Storfelmedtth)).
      THIS-OBJECT:GridFelMedd:Text = "Fel vid inläsning!".
      THIS-OBJECT:GridFelMedd:GuruGridReadOnlyCell(TRUE).        /*5 = MINDR ÄN VÄRDET 5*/ 
      THIS-OBJECT:GridFelMedd:GuruAddConditionalColor("ORDNING", 5, "5","SKRIVOVER", Guru.StaticMethods:DotNetColor(0)).    
      /*AOf*/
      THIS-OBJECT:ControlDataDb:Storfelmedtth:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFelMedd_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString(). 
        
      IF kolnamn = "LAGGTILL" THEN DO:
         
      END.
      IF kolnamn = "SKRIVOVER" THEN DO:
         
      END.
      IF kolnamn = "TABORT" THEN DO:
          
      END.
      THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde. 
      IF beforekolvarde = "FALSE" THEN DO:
         IF beforekolnamn = "LAGGTILL" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("SKRIVOVER"):BUFFER-VALUE  = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("TABORT"):BUFFER-VALUE  = FALSE.
         END.
         IF beforekolnamn = "SKRIVOVER" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("LAGGTILL"):BUFFER-VALUE  = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("TABORT"):BUFFER-VALUE  = FALSE.
            
         END.
         IF beforekolnamn = "TABORT" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("LAGGTILL"):BUFFER-VALUE  = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = FALSE.
            THIS-OBJECT:ControlDataDb:Storfelmedtth:BUFFER-FIELD("SKRIVOVER"):BUFFER-VALUE  = FALSE.
         END.
      END.
      
      /*
      MESSAGE 
      THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE
      THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE
      THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE
      VIEW-AS ALERT-BOX.
      */
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFelMedd_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFelMedd_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString()  NO-ERROR.
      IF kolnamn = "LAGGTILL" THEN DO:
          IF kolvarde = "FALSE" THEN THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = TRUE.
          ELSE THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = FALSE.
      END.
      IF kolnamn = "SKRIVOVER" THEN DO:
         IF kolvarde = "FALSE" THEN THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = TRUE.
          ELSE THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = FALSE.
         
      END.
      IF kolnamn = "TABORT" THEN DO:
         IF kolvarde = "FALSE" THEN THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = TRUE.
         ELSE THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = FALSE.
      END.
      /*    
      IF beforekolvarde = "FALSE" THEN DO:
         IF beforekolnamn = "LAGGTILL" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = FALSE.
         END.
         IF beforekolnamn = "SKRIVOVER" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["TABORT"]:VALUE = FALSE.
            
         END.
         IF beforekolnamn = "TABORT" THEN DO:
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["LAGGTILL"]:VALUE = FALSE.
            THIS-OBJECT:GridFelMedd:ActiveRow:Cells["SKRIVOVER"]:VALUE = FALSE.
         END.
      END.
      */       
 		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Storning.StorFelmeddCon").
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
      THIS-OBJECT:splitContainerFelMedd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFelMedd = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerFelMedd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFelMedd:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFelMedd:SuspendLayout().
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerFelMedd */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFelMedd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFelMedd:Name = "splitContainerFelMedd".
      /*  */
      /* splitContainerFelMedd.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Panel1:Controls:Add(THIS-OBJECT:GridFelMedd).
      /*  */
      /* splitContainerFelMedd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerFelMedd.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerFelMedd:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerFelMedd:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerFelMedd:SplitterDistance = 888.
      THIS-OBJECT:splitContainerFelMedd:TabIndex = 2.
      /*  */
      /* GridFelMedd */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFelMedd:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFelMedd:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFelMedd:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFelMedd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFelMedd:GuruContext = ?.
      THIS-OBJECT:GridFelMedd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFelMedd:Name = "GridFelMedd".
      THIS-OBJECT:GridFelMedd:Size = NEW System.Drawing.Size(888, 562).
      THIS-OBJECT:GridFelMedd:TabIndex = 0.
      THIS-OBJECT:GridFelMedd:Text = "grid1".
      THIS-OBJECT:GridFelMedd:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFelMedd_AfterCellUpdate).
      THIS-OBJECT:GridFelMedd:CellChange:Subscribe(THIS-OBJECT:GridFelMedd_CellChange).
      THIS-OBJECT:GridFelMedd:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridFelMedd_BeforeCellUpdate).
      /*  */
      /* StorFelmeddCon */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerFelMedd).
      THIS-OBJECT:Name = "StorFelmeddCon".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerFelMedd:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFelMedd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFelMedd:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFelMedd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC StorFelmeddCon ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.