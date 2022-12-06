 
 /*------------------------------------------------------------------------
    File        : KalkVolymSCControl
    Purpose     : FRÅN SCHKAKT
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 19:13:06 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkVolymSCControl INHERITS UserControl: 
	{KALKYLKAT.I}
	DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   
   /* Ribbongrupper */
   DEFINE         VARIABLE RibbonTabAdd               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   /*ribbon ellement*/
   DEFINE PUBLIC  VARIABLE RibbonToolAddAlla         AS Infragistics.Win.UltraWinToolbars.ButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAddNoll         AS Infragistics.Win.UltraWinToolbars.ButtonTool              NO-UNDO.
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
   DEFINE PRIVATE VARIABLE gridguru AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE Gridtest AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainergGrund AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainervisar AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVolym AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridVolymBer AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGridArbLopSuper AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
   DEFINE VARIABLE qArbetskoderTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qLopposterTTh AS HANDLE NO-UNDO.
	
	DEFINE VARIABLE qHuvTTh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qNumTTh AS HANDLE NO-UNDO.
	DEFINE VARIABLE qNumSubTTh AS HANDLE NO-UNDO.
	DEFINE VARIABLE qMtrlTTh AS HANDLE NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaGridNu AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
	
	CONSTRUCTOR PUBLIC KalkVolymSCControl (INPUT r AS Guru.Root  ):
		
		
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
      THIS-OBJECT:CreateRibbon().
   END METHOD.
   METHOD PUBLIC VOID CreateRibbon():
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule("Volym").
      THIS-OBJECT:RibbonTabAdd = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Adda").
      THIS-OBJECT:RibbonTabAdd:Caption = "Hämta data".
      THIS-OBJECT:RibbonTabAdd:Visible = TRUE.
      
      
      THIS-OBJECT:RibbonToolAddAlla = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AddAlla").
      THIS-OBJECT:RibbonToolAddAlla:SharedPropsInternal:Caption = "Hämta alla längder från Schaktprotokollet".
      
      THIS-OBJECT:RibbonToolAddAlla:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:RibbonToolAddAlla:SharedPropsInternal:ToolTipText = "Skriver över alla längder med data från Schaktprotokollet".
     
      THIS-OBJECT:RibbonToolAddNoll = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("AddNoll").
      THIS-OBJECT:RibbonToolAddNoll:SharedPropsInternal:Caption = "Uppdatera poster med noll i meter från Schaktprotokollet".
      
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolAddAlla,INPUT "bilder/xbtn_alldown.gif").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolAddNoll,INPUT "bilder/xbtn_downarrow.gif").
      
      THIS-OBJECT:RibbonToolAddNoll:SharedPropsInternal:Visible = TRUE.
      THIS-OBJECT:RibbonToolAddNoll:SharedPropsInternal:ToolTipText =  THIS-OBJECT:RibbonToolAddNoll:SharedPropsInternal:Caption.
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAddAlla,THIS-OBJECT:RibbonTabAdd).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAddNoll,THIS-OBJECT:RibbonTabAdd).
      
      THIS-OBJECT:RibbonToolAddAlla:ToolClick:Subscribe(THIS-OBJECT:AddAlla). 
      THIS-OBJECT:RibbonToolAddNoll:ToolClick:Subscribe(THIS-OBJECT:AddNoll).
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymDSladda().
      THIS-OBJECT:CreateGridVolymBer().
      THIS-OBJECT:CreateGridGuru().
   END METHOD.
   METHOD PUBLIC VOID CreateGridVolymBer():
      {GridRubrikListaVarExtent.i}
     
      rubrikergrid[1] = NEW Controls.GridRubrik("STRACKA","Sträcka",FALSE,250).
      rubrikergrid[2] = NEW Controls.GridRubrik("SCHAKTLANGD","Schaktlängd i meter",FALSE,100,"DEC", "KALKYL").
      rubrikergrid[3] = NEW Controls.GridRubrik("SCHAKTBREDD","Schaktbredd i centimeter",FALSE,140,"DEC", "INT").
      rubrikergrid[4] = NEW Controls.GridRubrik("SCHAKTDJUP","Schaktdjup i centimeter",FALSE,135,"DEC", "INT").
      rubrikergrid[5] = NEW Controls.GridRubrik("SVARMARK","Svår mark",FALSE,70,"LOG").
      rubrikergrid[6] = NEW Controls.GridRubrik("RORFOR","Rörförläggning",FALSE,90,"LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("TOTVOLYM","Totalt m3",TRUE,70,"DEC", "KALKYL").
      rubrikergrid[8] = NEW Controls.GridRubrik("TILLAGGVOLYM","Tilläggsfyllning m3",TRUE,110,"DEC", "KALKYL").
      rubrikergrid[9] = NEW Controls.GridRubrik("SKYDDFYLLNING","Skyddsfyllning m3",TRUE,105,"DEC", "KALKYL").
      rubrikergrid[10] = NEW Controls.GridRubrik("YTBELAGGD","Ytbeläggning m2",TRUE,100,"DEC", "KALKYL").
      THIS-OBJECT:GridVolymBer:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:KalkdbControl:VolymberTTbuffh)).  
     
      THIS-OBJECT:GridVolymBer:Text = "Volymberäkningar för Schakt. Beräkningarna sparas!".
      THIS-OBJECT:GridVolymBer:GuruOrderbyDefault("BY KLID ").
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
       

       THIS-OBJECT:GridGuru:DisplayLayout:Bands[1]:Columns["benamning"]:ValueList = comboList NO-ERROR.
       THIS-OBJECT:GridGuru:DisplayLayout:Bands[1]:Columns["MARKNING"]:ValueList = comboList2 NO-ERROR.
       /*
       THIS-OBJECT:GridGuru:DisplayLayout:ValueLists:Add("sample").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueLists:Add("Msample").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("sample"):ValueListItems:Clear().       
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("sample"):ValueListItems:Add("Alla").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("sample"):ValueListItems:Add("NYAAlla").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("sample"):ValueListItems:Add("MERAAlla").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("Msample"):ValueListItems:Clear().
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("Msample"):ValueListItems:Add("MAlla").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("Msample"):ValueListItems:Add("MNYAAlla").
       THIS-OBJECT:GridGuru:DisplayLayout:ValueList("Msample"):ValueListItems:Add("MMERAAlla").
       THIS-OBJECT:GridGuru:Bands[1]:Columns("benamning"):ValueList = THIS-OBJECT:GridGuru:DisplayLayout:ValueLists("sample").
       THIS-OBJECT:GridGuru:Bands[1]:Columns("MARKNING"):ValueList = THIS-OBJECT:GridGuru:DisplayLayout:ValueLists("Msample").
         */
       
   END METHOD.
   
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridVolymBer_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
		IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN RETURN.
       /*
       THIS-OBJECT:Gridtest:Visible = TRUE.
       THIS-OBJECT:splitContainerVolym:Visible = FALSE.
       */
       
       THIS-OBJECT:splitContainergGrund:Panel1Collapsed = TRUE.
       THIS-OBJECT:splitContainergGrund:Panel2Collapsed = FALSE.
     
       FIND FIRST ekalkylloppostertt WHERE NO-LOCK NO-ERROR.
       IF NOT AVAILABLE ekalkylloppostertt THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ArbetskoderTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ArbetskoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            qArbetskoderTTh:BUFFER-CREATE().
            qArbetskoderTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:ArbetskoderTTh).
            qH:GET-NEXT().   
         END.  
         
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:LopposterTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:LopposterTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            qLopposterTTh:BUFFER-CREATE().
            qLopposterTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:LopposterTTh).
            qH:GET-NEXT().   
         END.  
                    
    
        
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:HuvudTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:HuvudTTh,queryvar).
      qH:GET-FIRST().
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qHuvTTh:BUFFER-CREATE().
         qHuvTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:HuvudTTh).
         qH:GET-NEXT().   
      END.  
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
      qH:GET-FIRST().
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qNumTTh:BUFFER-CREATE().
         qNumTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KoderTTh).
         qH:GET-NEXT().   
      END. 
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:ValdaPriserTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:ValdaPriserTTh,queryvar).
      qH:GET-FIRST().
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qNumSubTTh:BUFFER-CREATE().
         qNumSubTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:ValdaPriserTTh).
         qH:GET-NEXT().   
      END. 
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalktmtrlTTh,queryvar).
      qH:GET-FIRST().
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qMtrlTTh:BUFFER-CREATE().
         qMtrlTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:kalktmtrlTTh).
         qH:GET-NEXT().   
      END. 
      
    END.   
    THIS-OBJECT:bindingSourceVisaNU = NEW Progress.Data.BindingSource().
    
    
    THIS-OBJECT:KalkdbControl:HuvudTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      
   
     bindingSourceArbLop:HANDLE = DATASET Gridsuperds:HANDLE.

     bindingSourceVisaNU:HANDLE = DATASET Visasuperds:HANDLE.
    
      THIS-OBJECT:VisaGridNu:DataSource = bindingSourceVisaNU.
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkVolymSCControl").
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
      DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ekalkylarbkodertt", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKOD", -1, ?, 33990672, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 33990673, 0, 0, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Descending, FALSE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
      appearance17 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("REGION", -1, ?, 33990672, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ekalkylloppostertt").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup1 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("region", 33990672).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup2 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup2 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Koder", 33990673).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
      appearance18 = NEW Infragistics.Win.Appearance("Super spec").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ekalkylloppostertt", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKOD", -1, ?, 33990673, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance(" ").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LOPNR", -1, ?, 33990672, 1, 1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 33990673, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET", -1, ?, 33990672, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KOMMENTAR", -1, ?, 33990673, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKNING", -1, ?, 33990673, 4, 1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKSUB", -1, ?, 33990673, 5, 1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TKOST", -1, ?, 33990673, 6, 1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("kom + ben", 0, ?, 33990673, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup3 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup3 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("This group contains all Long columns", 33990672).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance("kommer detta").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup4 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup4 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("övriga", 33990673).
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("ekalkylarbkodertt").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("ekalkylloppostertt").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand3 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand3 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ekalkylarbkodertt", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKOD", -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Descending, FALSE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("REGION").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ekalkylloppostertt").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand4 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand4 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ekalkylloppostertt", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKOD", -1, ?, 33990672, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LOPNR", -1, ?, 33990672, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 33990672, 0, 0, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET", -1, ?, 33990672, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KOMMENTAR", -1, ?, 33990673, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKNING", -1, ?, 33990673, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKSUB", -1, ?, 33990673, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TKOST", -1, ?, 33990673, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup5 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup5 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Group1", 33990672).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup6 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup6 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Group2", 33990673).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
      appearance34 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
      appearance35 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
      appearance36 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
      appearance37 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
      appearance38 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
      appearance39 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
      appearance40 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
      appearance41 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
      appearance42 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
      appearance43 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
      appearance44 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
      appearance45 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
      appearance46 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
      appearance47 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
      appearance48 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
      appearance49 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
      appearance55 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
      appearance56 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerVolym = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridVolymBer = NEW Controls.Grid().
      THIS-OBJECT:Gridtest = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ultraGridArbLopSuper = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
      THIS-OBJECT:bindingSourceArbLop = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:splitContainervisar = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:gridguru = NEW Controls.Grid().
      THIS-OBJECT:VisaGridNu = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
      THIS-OBJECT:splitContainergGrund = NEW System.Windows.Forms.SplitContainer().
      CAST(THIS-OBJECT:splitContainerVolym, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVolym:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVolym:SuspendLayout().
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:Gridtest, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:Gridtest:Panel1:SuspendLayout().
      THIS-OBJECT:Gridtest:Panel2:SuspendLayout().
      THIS-OBJECT:Gridtest:SuspendLayout().
      CAST(THIS-OBJECT:ultraGridArbLopSuper, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSourceArbLop, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainervisar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainervisar:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainervisar:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainervisar:SuspendLayout().
      CAST(THIS-OBJECT:gridguru, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaGridNu, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainergGrund, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainergGrund:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainergGrund:Panel2:SuspendLayout().
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
      THIS-OBJECT:splitContainerVolym:SplitterDistance = 471.
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
      THIS-OBJECT:GridVolymBer:Size = NEW System.Drawing.Size(471, 546).
      THIS-OBJECT:GridVolymBer:TabIndex = 0.
      THIS-OBJECT:GridVolymBer:Text = "grid1".
      THIS-OBJECT:GridVolymBer:AfterCellUpdate:Subscribe(THIS-OBJECT:GridVolymBer_AfterCellUpdate).
      THIS-OBJECT:GridVolymBer:CellChange:Subscribe(THIS-OBJECT:GridVolymBer_CellChange).
      THIS-OBJECT:GridVolymBer:DoubleClick:Subscribe(THIS-OBJECT:GridVolymBer_DoubleClick).
      /*  */
      /* Gridtest */
      /*  */
      THIS-OBJECT:Gridtest:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Gridtest:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Gridtest:Name = "Gridtest".
      /*  */
      /* Gridtest.Panel1 */
      /*  */
      THIS-OBJECT:Gridtest:Panel1:Controls:Add(THIS-OBJECT:ultraGridArbLopSuper).
      THIS-OBJECT:Gridtest:Panel1:DoubleClick:Subscribe(THIS-OBJECT:Panel1_DoubleClick_1).
      /*  */
      /* Gridtest.Panel2 */
      /*  */
      THIS-OBJECT:Gridtest:Panel2:Controls:Add(THIS-OBJECT:splitContainervisar).
      THIS-OBJECT:Gridtest:Size = NEW System.Drawing.Size(607, 546).
      THIS-OBJECT:Gridtest:SplitterDistance = 300.
      THIS-OBJECT:Gridtest:TabIndex = 0.
      /*  */
      /* ultraGridArbLopSuper */
      /*  */
      THIS-OBJECT:ultraGridArbLopSuper:DataSource = THIS-OBJECT:bindingSourceArbLop.
      appearance13:BackColor = System.Drawing.Color:Silver.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:AddNewBox:Appearance = appearance13.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Appearance = appearance14.
      ultraGridBand1:ColHeaderLines = 5.
      ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:Disabled.
      appearance15:BackColor = System.Drawing.Color:Red.
      appearance15:ForeColor = System.Drawing.Color:Transparent.
      ultraGridColumn1:CellAppearance = appearance15.
      ultraGridColumn1:CellMultiLine = Infragistics.Win.DefaultableBoolean:True.
      appearance16:BackColor = System.Drawing.Color:Red.
      appearance16:BorderColor = System.Drawing.Color:Red.
      ultraGridColumn1:GroupByRowConnectorAppearance = appearance16.
      ultraGridColumn1:Width = 33.
      ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
      ultraGridColumn2:CellMultiLine = Infragistics.Win.DefaultableBoolean:True.
      appearance17:ImageBackground = CAST(resources:GetObject("appearance17.ImageBackground"), System.Drawing.Image).
      appearance17:ImageBackgroundStyle = Infragistics.Win.ImageBackgroundStyle:Stretched.
      appearance17:TextTrimming = Infragistics.Win.TextTrimming:WordWithLineLimit.
      ultraGridColumn2:Header:Appearance = appearance17.
      ultraGridColumn2:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
      ultraGridColumn3:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDownList.
      ultraGridColumn9:Header:VisiblePosition = 3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 4 NO-UNDO.
      arrayvar0[1] = ultraGridColumn1.
      arrayvar0[2] = ultraGridColumn2.
      arrayvar0[3] = ultraGridColumn3.
      arrayvar0[4] = ultraGridColumn9.
      ultraGridBand1:Columns:AddRange(arrayvar0).
      ultraGridGroup1:Key = "region".
      ultraGridGroup1:RowLayoutGroupInfo:LabelSpan = 1.
      ultraGridGroup2:Key = "Koder".
      ultraGridGroup2:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 2 NO-UNDO.
      arrayvar1[1] = ultraGridGroup1.
      arrayvar1[2] = ultraGridGroup2.
      ultraGridBand1:Groups:AddRange(arrayvar1).
      appearance18:FontData:BoldAsString = "True".
      appearance18:ForeColor = System.Drawing.Color:Blue.
      appearance18:Image = CAST(resources:GetObject("appearance18.Image"), System.Object).
      ultraGridBand1:Header:Appearance = appearance18.
      ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
      appearance19:FontData:BoldAsString = "True".
      appearance19:ForeColor = System.Drawing.Color:Blue.
      appearance19:Image = CAST(resources:GetObject("appearance19.Image"), System.Object).
      ultraGridColumn10:Header:Appearance = appearance19.
      ultraGridColumn10:Header:Caption = "Ö2".
      ultraGridColumn11:Header:Caption = "Ö1".
      ultraGridColumn13:Header:Caption = "VAD KOMMER HÄR".
      ultraGridColumn14:Header:Caption = "Ö4".
      ultraGridColumn20:Header:Caption = "L1Ö1".
      ultraGridColumn21:Header:Caption = "L1Ö2".
      ultraGridColumn22:Header:Caption = "L1Ö3".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Object EXTENT 9 NO-UNDO.
      arrayvar2[1] = ultraGridColumn10.
      arrayvar2[2] = ultraGridColumn11.
      arrayvar2[3] = ultraGridColumn12.
      arrayvar2[4] = ultraGridColumn13.
      arrayvar2[5] = ultraGridColumn14.
      arrayvar2[6] = ultraGridColumn20.
      arrayvar2[7] = ultraGridColumn21.
      arrayvar2[8] = ultraGridColumn22.
      arrayvar2[9] = ultraGridColumn19.
      ultraGridBand2:Columns:AddRange(arrayvar2).
      ultraGridBand2:GroupHeaderLines = 2.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:Word.
      ultraGridGroup3:Header:Appearance = appearance20.
      ultraGridGroup3:Header:Caption = "GRUND GRUPP".
      ultraGridGroup3:Key = "This group contains all Long columns".
      ultraGridGroup3:RowLayoutGroupInfo:LabelPosition = Infragistics.Win.UltraWinGrid.LabelPosition:Left.
      ultraGridGroup3:RowLayoutGroupInfo:LabelSpan = 1.
      ultraGridGroup4:Key = "övriga".
      ultraGridGroup4:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 2 NO-UNDO.
      arrayvar3[1] = ultraGridGroup3.
      arrayvar3[2] = ultraGridGroup4.
      ultraGridBand2:Groups:AddRange(arrayvar3).
      ultraGridBand2:LevelCount = 2.
      ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance21:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:GroupByBox:Appearance = appearance21.
      appearance22:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:GroupByBox:BandLabelAppearance = appearance22.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance23:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance23:BackColor2 = System.Drawing.SystemColors:Control.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:GroupByBox:PromptAppearance = appearance23.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:MaxRowScrollRegions = 1.
      appearance24:BackColor = System.Drawing.SystemColors:Window.
      appearance24:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:ActiveCellAppearance = appearance24.
      appearance25:BackColor = System.Drawing.SystemColors:Highlight.
      appearance25:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:ActiveRowAppearance = appearance25.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance26:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:CardAreaAppearance = appearance26.
      appearance27:BorderColor = System.Drawing.Color:Silver.
      appearance27:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:CellAppearance = appearance27.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:CellPadding = 0.
      appearance28:BackColor = System.Drawing.SystemColors:Control.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:GroupByRowAppearance = appearance28.
      appearance29:TextHAlignAsString = "Left".
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:HeaderAppearance = appearance29.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance30:BackColor = System.Drawing.SystemColors:Window.
      appearance30:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:RowAppearance = appearance30.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance31:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:Override:TemplateAddRowAppearance = appearance31.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:ultraGridArbLopSuper:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:ultraGridArbLopSuper:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ultraGridArbLopSuper:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:ultraGridArbLopSuper:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraGridArbLopSuper:Name = "ultraGridArbLopSuper".
      THIS-OBJECT:ultraGridArbLopSuper:Size = NEW System.Drawing.Size(300, 546).
      THIS-OBJECT:ultraGridArbLopSuper:TabIndex = 0.
      THIS-OBJECT:ultraGridArbLopSuper:Text = "ultraGrid1".
      THIS-OBJECT:ultraGridArbLopSuper:InitializeLayout:Subscribe(THIS-OBJECT:ultraGridArbLopSuper_InitializeLayout).
      THIS-OBJECT:ultraGridArbLopSuper:DoubleClick:Subscribe(THIS-OBJECT:ultraGridArbLopSuper_DoubleClick).
      /*  */
      /* bindingSourceArbLop */
      /*  */
      THIS-OBJECT:bindingSourceArbLop:MaxDataGuess = 0.
      THIS-OBJECT:bindingSourceArbLop:NoLOBs = FALSE.
      THIS-OBJECT:bindingSourceArbLop:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar4 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar4.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
      arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("ARBKOD", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("LOPNR", "", Progress.Data.DataType:INTEGER).
      arrayvar5[3] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "egen ben", Progress.Data.DataType:CHARACTER).
      arrayvar5[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[5] = NEW Progress.Data.ColumnPropDesc("KOMMENTAR", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[6] = NEW Progress.Data.ColumnPropDesc("MARKNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[7] = NEW Progress.Data.ColumnPropDesc("MARKSUB", "", Progress.Data.DataType:CHARACTER).
      arrayvar5[8] = NEW Progress.Data.ColumnPropDesc("TKOST", "", Progress.Data.DataType:DECIMAL).
      tableDesc2:Columns = arrayvar5.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS Progress.Data.TableDesc EXTENT 1 NO-UNDO.
      arrayvar6[1] = tableDesc2.
      tableDesc1:ChildTables = arrayvar6.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar7[1] = NEW Progress.Data.ColumnPropDesc("ARBKOD", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar7[3] = NEW Progress.Data.ColumnPropDesc("REGION", "", Progress.Data.DataType:LOGICAL).
      tableDesc1:Columns = arrayvar7.
      THIS-OBJECT:bindingSourceArbLop:TableSchema = tableDesc1.
      /*  */
      /* splitContainervisar */
      /*  */
      THIS-OBJECT:splitContainervisar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainervisar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainervisar:Name = "splitContainervisar".
      /*  */
      /* splitContainervisar.Panel1 */
      /*  */
      THIS-OBJECT:splitContainervisar:Panel1:Controls:Add(THIS-OBJECT:gridguru).
      /*  */
      /* splitContainervisar.Panel2 */
      /*  */
      THIS-OBJECT:splitContainervisar:Panel2:Controls:Add(THIS-OBJECT:VisaGridNu).
      THIS-OBJECT:splitContainervisar:Size = NEW System.Drawing.Size(303, 546).
      THIS-OBJECT:splitContainervisar:SplitterDistance = 200.
      THIS-OBJECT:splitContainervisar:TabIndex = 0.
      /*  */
      /* gridguru */
      /*  */
      THIS-OBJECT:gridguru:DataSource = THIS-OBJECT:bindingSourceArbLop.
      appearance32:BackColor = System.Drawing.SystemColors:Window.
      appearance32:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridguru:DisplayLayout:Appearance = appearance32.
      ultraGridBand3:ColHeaderLines = 5.
      ultraGridColumn4:Header:VisiblePosition = 0.
      ultraGridColumn5:CellMultiLine = Infragistics.Win.DefaultableBoolean:True.
      ultraGridColumn5:Header:VisiblePosition = 1.
      ultraGridColumn6:Header:VisiblePosition = 2.
      ultraGridColumn7:Header:VisiblePosition = 3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar8 AS System.Object EXTENT 4 NO-UNDO.
      arrayvar8[1] = ultraGridColumn4.
      arrayvar8[2] = ultraGridColumn5.
      arrayvar8[3] = ultraGridColumn6.
      arrayvar8[4] = ultraGridColumn7.
      ultraGridBand3:Columns:AddRange(arrayvar8).
      ultraGridBand3:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
      ultraGridColumn16:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
      appearance33:FontData:BoldAsString = "True".
      ultraGridColumn17:Header:Appearance = appearance33.
      ultraGridColumn23:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDownList.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar9 AS System.Object EXTENT 8 NO-UNDO.
      arrayvar9[1] = ultraGridColumn8.
      arrayvar9[2] = ultraGridColumn15.
      arrayvar9[3] = ultraGridColumn16.
      arrayvar9[4] = ultraGridColumn17.
      arrayvar9[5] = ultraGridColumn18.
      arrayvar9[6] = ultraGridColumn23.
      arrayvar9[7] = ultraGridColumn24.
      arrayvar9[8] = ultraGridColumn25.
      ultraGridBand4:Columns:AddRange(arrayvar9).
      ultraGridGroup5:Key = "Group1".
      ultraGridGroup6:Key = "Group2".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar10 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 2 NO-UNDO.
      arrayvar10[1] = ultraGridGroup5.
      arrayvar10[2] = ultraGridGroup6.
      ultraGridBand4:Groups:AddRange(arrayvar10).
      ultraGridBand4:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
      THIS-OBJECT:gridguru:DisplayLayout:BandsSerializer:Add(ultraGridBand3).
      THIS-OBJECT:gridguru:DisplayLayout:BandsSerializer:Add(ultraGridBand4).
      THIS-OBJECT:gridguru:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridguru:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance34:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance34:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance34:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance34:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridguru:DisplayLayout:GroupByBox:Appearance = appearance34.
      appearance35:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridguru:DisplayLayout:GroupByBox:BandLabelAppearance = appearance35.
      THIS-OBJECT:gridguru:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance36:BackColor2 = System.Drawing.SystemColors:Control.
      appearance36:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance36:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridguru:DisplayLayout:GroupByBox:PromptAppearance = appearance36.
      THIS-OBJECT:gridguru:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridguru:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridguru:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridguru:DisplayLayout:Override:ActiveCellAppearance = appearance37.
      appearance38:BackColor = System.Drawing.SystemColors:Highlight.
      appearance38:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridguru:DisplayLayout:Override:ActiveRowAppearance = appearance38.
      THIS-OBJECT:gridguru:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridguru:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridguru:DisplayLayout:Override:CardAreaAppearance = appearance39.
      appearance40:BorderColor = System.Drawing.Color:Silver.
      appearance40:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridguru:DisplayLayout:Override:CellAppearance = appearance40.
      THIS-OBJECT:gridguru:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridguru:DisplayLayout:Override:CellPadding = 0.
      appearance41:BackColor = System.Drawing.SystemColors:Control.
      appearance41:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance41:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance41:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridguru:DisplayLayout:Override:GroupByRowAppearance = appearance41.
      appearance42:TextHAlignAsString = "Left".
      THIS-OBJECT:gridguru:DisplayLayout:Override:HeaderAppearance = appearance42.
      THIS-OBJECT:gridguru:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridguru:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridguru:DisplayLayout:Override:RowAppearance = appearance43.
      THIS-OBJECT:gridguru:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance44:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridguru:DisplayLayout:Override:TemplateAddRowAppearance = appearance44.
      THIS-OBJECT:gridguru:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridguru:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridguru:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridguru:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridguru:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridguru:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridguru:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridguru:GuruContext = ?.
      THIS-OBJECT:gridguru:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridguru:Name = "gridguru".
      THIS-OBJECT:gridguru:Size = NEW System.Drawing.Size(200, 546).
      THIS-OBJECT:gridguru:TabIndex = 0.
      THIS-OBJECT:gridguru:Text = "grid1".
      /*  */
      /* VisaGridNu */
      /*  */
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      appearance45:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Appearance = appearance45.
      THIS-OBJECT:VisaGridNu:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:VisaGridNu:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance46:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance46:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance46:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance46:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:VisaGridNu:DisplayLayout:GroupByBox:Appearance = appearance46.
      appearance47:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:VisaGridNu:DisplayLayout:GroupByBox:BandLabelAppearance = appearance47.
      THIS-OBJECT:VisaGridNu:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance48:BackColor2 = System.Drawing.SystemColors:Control.
      appearance48:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance48:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:VisaGridNu:DisplayLayout:GroupByBox:PromptAppearance = appearance48.
      THIS-OBJECT:VisaGridNu:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:VisaGridNu:DisplayLayout:MaxRowScrollRegions = 1.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:ActiveCellAppearance = appearance49.
      appearance50:BackColor = System.Drawing.SystemColors:Highlight.
      appearance50:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:ActiveRowAppearance = appearance50.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:CardAreaAppearance = appearance51.
      appearance52:BorderColor = System.Drawing.Color:Silver.
      appearance52:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:CellAppearance = appearance52.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:CellPadding = 0.
      appearance53:BackColor = System.Drawing.SystemColors:Control.
      appearance53:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance53:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance53:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance53:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:GroupByRowAppearance = appearance53.
      appearance54:TextHAlignAsString = "Left".
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:HeaderAppearance = appearance54.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:RowAppearance = appearance55.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance56:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:VisaGridNu:DisplayLayout:Override:TemplateAddRowAppearance = appearance56.
      THIS-OBJECT:VisaGridNu:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:VisaGridNu:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:VisaGridNu:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:VisaGridNu:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaGridNu:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaGridNu:Name = "VisaGridNu".
      THIS-OBJECT:VisaGridNu:Size = NEW System.Drawing.Size(99, 546).
      THIS-OBJECT:VisaGridNu:TabIndex = 0.
      THIS-OBJECT:VisaGridNu:Text = "ultraGrid1".
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
      /*  */
      /* splitContainergGrund.Panel2 */
      /*  */
      THIS-OBJECT:splitContainergGrund:Panel2:Controls:Add(THIS-OBJECT:Gridtest).
      THIS-OBJECT:splitContainergGrund:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainergGrund:SplitterDistance = 500.
      THIS-OBJECT:splitContainergGrund:TabIndex = 0.
      /*  */
      /* KalkVolymSCControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainergGrund).
      THIS-OBJECT:Name = "KalkVolymSCControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVolym:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVolym, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVolym:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridVolymBer, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:Gridtest:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:Gridtest:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Gridtest, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:Gridtest:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraGridArbLopSuper, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSourceArbLop, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainervisar:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainervisar:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainervisar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainervisar:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridguru, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:VisaGridNu, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainergGrund:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainergGrund:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainergGrund, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainergGrund:ResumeLayout(FALSE).
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
   
   METHOD PUBLIC VOID AddAlla(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:UppdatLangd(1).
   END METHOD.
   METHOD PUBLIC VOID AddNoll(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:UppdatLangd(2).
      
   END METHOD.
   METHOD PUBLIC VOID UppdatLangd(INPUT allanoll AS INTEGER ):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:ttBuffHandleVallistaDyn,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkdbControl:VolymberTTbuffh:FIND-FIRST("WHERE SID = " + STRING(Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:BUFFER-FIELD("SID"):BUFFER-VALUE) +
         " AND KLID = " + STRING(Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:BUFFER-FIELD("KLID"):BUFFER-VALUE) ,NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:KalkdbControl:VolymberTTbuffh:AVAILABLE THEN DO:
            IF allanoll = 1 THEN DO:
               THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE = Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE.
               THIS-OBJECT:GridVolymBerUpp("SCHAKTLANGD", STRING(THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE)).
            END.
            ELSE IF allanoll = 2 THEN DO:
               IF THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE = 0 THEN DO:
                  THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE = Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE.
                  THIS-OBJECT:GridVolymBerUpp("SCHAKTLANGD", STRING(THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE)).
               END.
            END.
         END.      
         qH:GET-NEXT().   
      END.
      THIS-OBJECT:GridVolymBer:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID GridVolymBerUpp(INPUT kolnamn  AS CHARACTER, kolvarde  AS CHARACTER):
        
      THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:VolymBer().
      
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TOTVOLYM"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["TILLAGGVOLYM"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["SKYDDFYLLNING"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE.
      THIS-OBJECT:GridVolymBer:ActiveRow:Cells["YTBELAGGD"]:VALUE = THIS-OBJECT:KalkdbControl:VolymberTTbuffh:BUFFER-FIELD("YTBELAGGD"):BUFFER-VALUE.
      
      
             
      RETURN.

   END METHOD. 







	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID Panel1_DoubleClick_1( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN RETURN.
      THIS-OBJECT:splitContainergGrund:Panel1Collapsed = FALSE.
       THIS-OBJECT:splitContainergGrund:Panel2Collapsed = TRUE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraGridArbLopSuper_DoubleClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN RETURN.
      THIS-OBJECT:splitContainergGrund:Panel1Collapsed = FALSE.
       THIS-OBJECT:splitContainergGrund:Panel2Collapsed = TRUE.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraGridArbLopSuper_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
		
		RETURN.

	END METHOD.


	
	DESTRUCTOR PUBLIC KalkVolymSCControl ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
      THIS-OBJECT:KalkdbControl:VolymSpar().
	END DESTRUCTOR.

END CLASS.