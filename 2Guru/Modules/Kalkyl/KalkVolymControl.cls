 
 /*------------------------------------------------------------------------
    File        : KalkVolymControl
    Purpose     : FRÅN KALKYL
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 19:13:06 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/
/*
DEFINE DATASET Visasuperds FOR ekalkhuvtt,ekalknumtt,ekalknumsubtt,ekalktmtrlTT
   DATA-RELATION visaDR FOR ekalkhuvtt, ekalknumtt RELATION-FIELDS (ekalkhuvtt.KALKNR,ekalknumtt.KALKNR)
   DATA-RELATION visaDR FOR ekalknumtt, ekalknumsubtt RELATION-FIELDS (ekalknumtt.KALKNR,ekalknumsubtt.KALKNR,ekalknumtt.NUM,ekalknumsubtt.NUM)
   DATA-RELATION visaDR FOR ekalkhuvtt, ekalktmtrlTT RELATION-FIELDS (ekalkhuvtt.KALKNR,ekalktmtrlTT.KALKNR).
 
   DEFINE PRIVATE VARIABLE bindingSourceVisaNU AS Progress.Data.BindingSource NO-UNDO.
   THIS-OBJECT:bindingSourceVisaNU = NEW Progress.Data.BindingSource().
   bindingSourceVisaNU:HANDLE = DATASET Visasuperds:HANDLE.
    
      THIS-OBJECT:VisaGridNu:DataSource = bindingSourceVisaNU.   
   
   
   */



USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkVolymControl INHERITS UserControl: 
	{KALKYLKAT.I}
	
	
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
	DEFINE PUBLIC VARIABLE CellChvar AS LOGICAL NO-UNDO.
   DEFINE PRIVATE VARIABLE bindingSourceVisaNU AS Progress.Data.BindingSource NO-UNDO.
	/*ultraGridGroup3:Key = "This group" + chr(10) + "contains all Long columns".
	rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning" + chr(10) + "kk" ,TRUE,325).
      THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:ColHeaderLines = 5.
	
	
	*/
	DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE DATASET Gridsuperds FOR ekalkylarbkodertt,ekalkylloppostertt
   DATA-RELATION LopSubsDR FOR ekalkylarbkodertt, ekalkylloppostertt RELATION-FIELDS (ekalkylarbkodertt.ARBKOD,ekalkylloppostertt.ARBKOD).
   
   DEFINE DATASET Visasuperds FOR ekalkhuvtt,ekalknumtt,ekalknumsubtt,ekalktmtrlTT
   DATA-RELATION visaDR FOR ekalkhuvtt, ekalknumtt RELATION-FIELDS (ekalkhuvtt.KALKNR,ekalknumtt.KALKNR)
   DATA-RELATION visaDR FOR ekalknumtt, ekalknumsubtt RELATION-FIELDS (ekalknumtt.KALKNR,ekalknumsubtt.KALKNR,ekalknumtt.NUM,ekalknumsubtt.NUM)
   DATA-RELATION visaDR FOR ekalkhuvtt, ekalktmtrlTT RELATION-FIELDS (ekalkhuvtt.KALKNR,ekalktmtrlTT.KALKNR).
   
   DEFINE PRIVATE VARIABLE bindingSourceArbLop AS Progress.Data.BindingSource NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainergGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVolym AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridVolymBer AS Controls.Grid NO-UNDO.
   DEFINE VARIABLE qArbetskoderTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qLopposterTTh AS HANDLE NO-UNDO.
	
	DEFINE VARIABLE qHuvTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qNumTTh AS HANDLE NO-UNDO.
	DEFINE VARIABLE qNumSubTTh AS HANDLE NO-UNDO.
	DEFINE VARIABLE qMtrlTTh AS HANDLE NO-UNDO.
	
	CONSTRUCTOR PUBLIC KalkVolymControl (INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      
      THIS-OBJECT:qArbetskoderTTh = TEMP-TABLE ekalkylarbkodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qLopposterTTh = TEMP-TABLE ekalkylloppostertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qHuvTTh = TEMP-TABLE ekalkhuvtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qNumTTh = TEMP-TABLE ekalknumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qNumSubTTh = TEMP-TABLE ekalknumsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:qMtrlTTh = TEMP-TABLE ekalktmtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      
      THIS-OBJECT:CreateForm().      
     /*
      THIS-OBJECT:Gridtest:Visible = FALSE. 
      */
      THIS-OBJECT:splitContainergGrund:Panel2Collapsed = TRUE.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	

   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().

   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalVolymDSLadda().
      THIS-OBJECT:CreateGridVolymBer().
      /*
      THIS-OBJECT:CreateGridGuru().
      */
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridVolymBer():
      {GridRubrikListaVarExtent.i}
      
      rubrikergrid[1] = NEW Controls.GridRubrik("STRACKA","Sträcka",FALSE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("SCHAKTLANGD","Schaktlängd i meter",FALSE,100,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("SCHAKTBREDD","Schaktbredd i centimeter",FALSE,140,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("SCHAKTDJUP","Schaktdjup i centimeter",FALSE,135,"DEC", "KALKYL").
      rubrikergrid[5] = NEW Controls.GridRubrik("SVARMARK","Svår mark",FALSE,70,"LOG").
      rubrikergrid[6] = NEW Controls.GridRubrik("RORFOR","Rörförläggning",FALSE,90,"LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("TOTVOLYM","Totalt m3",TRUE,70,"DEC", "KALKYL").
      rubrikergrid[8] = NEW Controls.GridRubrik("TILLAGGVOLYM","Tilläggsfyllning m3",TRUE,110,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("SKYDDFYLLNING","Skyddsfyllning m3",TRUE,105,"DEC", "KALKYL").
      rubrikergrid[10] = NEW Controls.GridRubrik("YTBELAGGD","Ytbeläggning m2",TRUE,100,"DEC", "KALKYL").
      THIS-OBJECT:GridVolymBer:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KalkdbControl:VolymberTTbuffh)).  
      THIS-OBJECT:GridVolymBer:Text = "Volymberäkningar för Kalkyl. OBS! Beräkningarna sparas!".
   END METHOD.
   METHOD PUBLIC VOID CreateGridGuru():
       DEFINE VARIABLE comboList AS Infragistics.Win.ValueList NO-UNDO.
       DEFINE VARIABLE comboList2 AS Infragistics.Win.ValueList NO-UNDO.
       comboList = NEW Infragistics.Win.ValueList().
       comboList2 = NEW Infragistics.Win.ValueList().
       comboList:ValueListItems:Add(1, "Alla").
       comboList:ValueListItems:Add(2, "NYAAlla").
       comboList:ValueListItems:Add(3, "MERAAlla").
       
       comboList2:ValueListItems:Add(1, "mAlla").
       comboList2:ValueListItems:Add(2, "mNYAAlla").
       comboList2:ValueListItems:Add(3, "mMERAAlla").
       
       DEFINE VARIABLE griddsh AS HANDLE NO-UNDO.
       griddsh = DATASET Gridsuperds:HANDLE.
       
       
   END METHOD.
   
	
	
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkVolymControl").
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
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("ekalkylarbkodertt").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("ekalkylloppostertt").
      THIS-OBJECT:splitContainerVolym = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridVolymBer = NEW Controls.Grid().
      THIS-OBJECT:bindingSourceArbLop = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:splitContainergGrund = NEW System.Windows.Forms.SplitContainer().
      CAST(THIS-OBJECT:splitContainerVolym, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVolym:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVolym:SuspendLayout().
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSourceArbLop, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainergGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainergGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainergGrund:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerVolym */
      /*  */
      THIS-OBJECT:splitContainerVolym:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVolym:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVolym:Name = "splitContainerVolym".
      /*  */
      /* splitContainerVolym.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVolym:Panel1:Controls:Add(THIS-OBJECT:GridVolymBer).
      /*  */
      /* splitContainerVolym.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVolym:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerVolym.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerVolym:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerVolym:Size = NEW System.Drawing.Size(500, 546).
      THIS-OBJECT:splitContainerVolym:SplitterDistance = 416.
      THIS-OBJECT:splitContainerVolym:TabIndex = 2.
      /*  */
      /* GridVolymBer */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridVolymBer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridVolymBer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridVolymBer:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridVolymBer:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridVolymBer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridVolymBer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridVolymBer:GuruContext = ?.
      THIS-OBJECT:GridVolymBer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridVolymBer:Name = "GridVolymBer".
      THIS-OBJECT:GridVolymBer:Size = NEW System.Drawing.Size(416, 546).
      THIS-OBJECT:GridVolymBer:TabIndex = 0.
      THIS-OBJECT:GridVolymBer:Text = "grid1".
      THIS-OBJECT:GridVolymBer:AfterCellUpdate:Subscribe(THIS-OBJECT:GridVolymBer_AfterCellUpdate).
      THIS-OBJECT:GridVolymBer:CellChange:Subscribe(THIS-OBJECT:GridVolymBer_CellChange).
      /*  */
      /* bindingSourceArbLop */
      /*  */
      THIS-OBJECT:bindingSourceArbLop:MaxDataGuess = 0.
      THIS-OBJECT:bindingSourceArbLop:NoLOBs = FALSE.
      THIS-OBJECT:bindingSourceArbLop:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
      arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("ARBKOD", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("LOPNR", "", Progress.Data.DataType:INTEGER).
      arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "egen ben", Progress.Data.DataType:CHARACTER).
      arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("KOMMENTAR", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("MARKNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("MARKSUB", "", Progress.Data.DataType:CHARACTER).
      arrayvar1[8] = NEW Progress.Data.ColumnPropDesc("TKOST", "", Progress.Data.DataType:DECIMAL).
      tableDesc2:Columns = arrayvar1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
      arrayvar2[1] = tableDesc2.
      tableDesc1:ChildTables = arrayvar2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("ARBKOD", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("REGION", "", Progress.Data.DataType:LOGICAL).
      tableDesc1:Columns = arrayvar3.
      THIS-OBJECT:bindingSourceArbLop:TableSchema = tableDesc1.
      /*  */
      /* splitContainergGrund */
      /*  */
      THIS-OBJECT:splitContainergGrund:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainergGrund:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainergGrund:Name = "splitContainergGrund".
      /*  */
      /* splitContainergGrund.Panel1 */
      /*  */
      THIS-OBJECT:splitContainergGrund:Panel1:Controls:Add(THIS-OBJECT:splitContainerVolym).
      THIS-OBJECT:splitContainergGrund:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainergGrund:SplitterDistance = 500.
      THIS-OBJECT:splitContainergGrund:TabIndex = 0.
      /*  */
      /* KalkVolymControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainergGrund).
      THIS-OBJECT:Name = "KalkVolymControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVolym:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVolym, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVolym:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSourceArbLop, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainergGrund:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainergGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainergGrund:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
	METHOD PUBLIC VOID AddAlla():
   END METHOD.
   METHOD PUBLIC VOID AddNoll():
   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridVolymBer_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:CellChvar = FALSE THEN DO:
         THIS-OBJECT:GridVolymBerUpp(kolnamn,kolvarde).
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridVolymBer_CellChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      /*
      IF THIS-OBJECT:GridVolymBer:Guruegenskap:GetGridCol(kolnamn):TYPVAR = "LOG" THEN.
      ELSE RETURN. 
      */
      kolvarde = e:Cell:VALUE:ToString() NO-ERROR.
      THIS-OBJECT:CellChvar = FALSE.
      IF kolnamn = "SVARMARK" OR kolnamn = "RORFOR" THEN DO:
         IF kolvarde = "TRUE" THEN kolvarde = "FALSE".
         ELSE kolvarde = "TRUE".
         THIS-OBJECT:GridVolymBerUpp(kolnamn,kolvarde). 
         THIS-OBJECT:CellChvar = TRUE.
      END. 
             
      RETURN.

   END METHOD.
    METHOD PUBLIC VOID GridVolymBerUpp(INPUT kolnamn  AS CHARACTER, kolvarde  AS CHARACTER):
      THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBer().
      /*
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBerKalk().
      */
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TOTVOLYM"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TILLAGGVOLYM"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["SKYDDFYLLNING"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["YTBELAGGD"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("YTBELAGGD"):BUFFER-VALUE.
             
      RETURN.

   END METHOD. 











	
	DESTRUCTOR PUBLIC KalkVolymControl ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
      THIS-OBJECT:KalkdbControl:VolymSpar().
	END DESTRUCTOR.

END CLASS.