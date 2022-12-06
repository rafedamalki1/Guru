 
 /*------------------------------------------------------------------------
    File        : KalkylControl
    Purpose     : helst inga mera grids i denna
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 11:23:05 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkylControl INHERITS UserControl: 
   DEFINE PUBLIC VARIABLE ButtonNy AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE AddRemoveStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE AddButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE GridEnrFran AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
	DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE flowLayoutPanel1 AS System.Windows.Forms.FlowLayoutPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE GridBerVal AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridBerMtrl AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSchaktVal AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE KodSokInput AS System.Windows.Forms.TextBox NO-UNDO.
   DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PUBLIC VARIABLE panelKodSok AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE panelFriIn AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE RemoveButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PUBLIC VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE RubrikKom AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerAllaAndFree AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerAndMTRL AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerBerKalkMtrl AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerFrekAnmKom AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerGrundKalk AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkBer AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKoderMtrl AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerLopInAntal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkKodoFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkFri AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalkArbkoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKalk AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInLop AS Controls.FillinMask NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInKod AS Controls.FillinMask NO-UNDO.
   DEFINE PUBLIC VARIABLE fillinFriInAntal AS Controls.FillinDecimal NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMtrlAnd AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNyBort AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerSchaktBer AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonAllaKoder AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripAllaKoder AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonHmtGrund AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonTomKonst AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripFriKalk AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonFriAter AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ValdKodKommentar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE Rubrikanm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylensKoderAnmarkning AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylButtonOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE KalkylButtonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKalkylKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFriKalk AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridFrekvens AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbetsKoder AS Controls.Grid NO-UNDO.
   DEFINE VARIABLE kolnamnRiskVinst  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kolvardeRiskVinst AS CHARACTER NO-UNDO.
   DEFINE VARIABLE DPIcheck  AS Helpers.DPIcheck              NO-UNDO.
   
	CONSTRUCTOR PUBLIC KalkylControl (INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ControlShell.
      
      InitializeComponent().
      THIS-OBJECT:buttonNy:visible = FALSE. 
      THIS-OBJECT:DPIcheck = NEW Helpers.DPIcheck().
      /*
      THIS-OBJECT:DPIcheck:yDPI
      */
       
     /*Anders Olsson Elpool i Umeå AB  25 mar 2015 10:48:44 
     Om man ska skapa en totalkalkyl av alla koder endast elpao!!!!
     */ 
      THIS-OBJECT:splitContainerGrundKalk:Panel1Collapsed = TRUE.
      IF Guru.Konstanter:globanv = {setuser.I} THEN DO:
          THIS-OBJECT:splitContainerGrundKalk:Panel1Collapsed = FALSE.
          THIS-OBJECT:toolStripAllaKoder:VISIBLE = TRUE. 
          THIS-OBJECT:toolStripFriKalk:VISIBLE = FALSE.
          IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
             THIS-OBJECT:toolStripButtonTomKonst:VISIBLE = TRUE.
             THIS-OBJECT:toolStripButtonHmtGrund:VISIBLE = TRUE.
          END.   
          ELSE DO:
             THIS-OBJECT:toolStripButtonTomKonst:VISIBLE = FALSE.
             THIS-OBJECT:toolStripButtonHmtGrund:VISIBLE = FALSE.
          END.   
      END.
      ELSE DO:
         THIS-OBJECT:toolStripAllaKoder:VISIBLE = FALSE.
         THIS-OBJECT:toolStripFriKalk:VISIBLE = FALSE.
         IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:splitContainerGrundKalk:Panel1Collapsed = FALSE.
            THIS-OBJECT:toolStripAllaKoder:VISIBLE = TRUE.
            THIS-OBJECT:toolStripButtonTomKonst:VISIBLE = TRUE.
            THIS-OBJECT:toolStripButtonAllaKoder:VISIBLE = FALSE.
            THIS-OBJECT:toolStripButtonHmtGrund:VISIBLE = TRUE.
         END.
      END.       
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:KalkdbControl = THIS-OBJECT:Root:DatabaseManager:Kalkyl.
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PUBLIC VOID GridBerMtrl_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString(). 
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridBerMtrl:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      THIS-OBJECT:KalkdbControl:Bermtrlbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.   
      THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = TRUE. 
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridBerMtrl_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
		
		RETURN.

	END METHOD.
		
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKalkylKoder_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
		   IF THIS-OBJECT:ControlShell:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:ControlShell:ValueKalkKoderCh(?,?). 
         END.
         ELSE IF THIS-OBJECT:ControlShell:RibbonToolKatalogKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:ControlShell:ValueKalkKoderCh(?,?).
         END.
      END.      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKalkylKoder_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		
		THIS-OBJECT:ButtonBackMark_Click(?,?).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopKoder_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		IF THIS-OBJECT:KalkdbControl:LopposterTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:ControlShell:RibbonToolFrekvensKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:ControlShell:ValueLopnrCh(?,?). 
         END.
         ELSE IF THIS-OBJECT:ControlShell:RibbonToolKatalogKom:Checked EQ TRUE THEN DO:
            THIS-OBJECT:ControlShell:ValueLopnrCh(?,?).
         END.
      END.  
		

		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridLopKoder_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
		THIS-OBJECT:ButtonOvermark_Click(1).
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkylControl").
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
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
      appearance57 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
      appearance58 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance59 AS Infragistics.Win.Appearance NO-UNDO.
      appearance59 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance60 AS Infragistics.Win.Appearance NO-UNDO.
      appearance60 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance61 AS Infragistics.Win.Appearance NO-UNDO.
      appearance61 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
      appearance62 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
      appearance63 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
      appearance64 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
      appearance66 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
      appearance67 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
      appearance68 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
      appearance69 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
      appearance70 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
      appearance71 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
      appearance72 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
      appearance73 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
      appearance74 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
      appearance75 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
      appearance76 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance77 AS Infragistics.Win.Appearance NO-UNDO.
      appearance77 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
      appearance78 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
      appearance79 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance80 AS Infragistics.Win.Appearance NO-UNDO.
      appearance80 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance81 AS Infragistics.Win.Appearance NO-UNDO.
      appearance81 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance82 AS Infragistics.Win.Appearance NO-UNDO.
      appearance82 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance83 AS Infragistics.Win.Appearance NO-UNDO.
      appearance83 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance84 AS Infragistics.Win.Appearance NO-UNDO.
      appearance84 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance85 AS Infragistics.Win.Appearance NO-UNDO.
      appearance85 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance86 AS Infragistics.Win.Appearance NO-UNDO.
      appearance86 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance87 AS Infragistics.Win.Appearance NO-UNDO.
      appearance87 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance88 AS Infragistics.Win.Appearance NO-UNDO.
      appearance88 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance89 AS Infragistics.Win.Appearance NO-UNDO.
      appearance89 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance90 AS Infragistics.Win.Appearance NO-UNDO.
      appearance90 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance91 AS Infragistics.Win.Appearance NO-UNDO.
      appearance91 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance92 AS Infragistics.Win.Appearance NO-UNDO.
      appearance92 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance93 AS Infragistics.Win.Appearance NO-UNDO.
      appearance93 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance94 AS Infragistics.Win.Appearance NO-UNDO.
      appearance94 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance95 AS Infragistics.Win.Appearance NO-UNDO.
      appearance95 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance96 AS Infragistics.Win.Appearance NO-UNDO.
      appearance96 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance97 AS Infragistics.Win.Appearance NO-UNDO.
      appearance97 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance98 AS Infragistics.Win.Appearance NO-UNDO.
      appearance98 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance99 AS Infragistics.Win.Appearance NO-UNDO.
      appearance99 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance100 AS Infragistics.Win.Appearance NO-UNDO.
      appearance100 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance101 AS Infragistics.Win.Appearance NO-UNDO.
      appearance101 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance102 AS Infragistics.Win.Appearance NO-UNDO.
      appearance102 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance103 AS Infragistics.Win.Appearance NO-UNDO.
      appearance103 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance104 AS Infragistics.Win.Appearance NO-UNDO.
      appearance104 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance105 AS Infragistics.Win.Appearance NO-UNDO.
      appearance105 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance106 AS Infragistics.Win.Appearance NO-UNDO.
      appearance106 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance107 AS Infragistics.Win.Appearance NO-UNDO.
      appearance107 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance108 AS Infragistics.Win.Appearance NO-UNDO.
      appearance108 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance109 AS Infragistics.Win.Appearance NO-UNDO.
      appearance109 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance110 AS Infragistics.Win.Appearance NO-UNDO.
      appearance110 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkArbkoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbetsKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopInAntal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridLopKoder = NEW Controls.Grid().
      THIS-OBJECT:KalkylButtonOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylButtonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:panelKodSok = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:flowLayoutPanel1 = NEW System.Windows.Forms.FlowLayoutPanel().
      THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
      THIS-OBJECT:KodSokInput = NEW System.Windows.Forms.TextBox().
      THIS-OBJECT:panelFriIn = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:fillinFriInKod = NEW Controls.FillinMask().
      THIS-OBJECT:fillinFriInAntal = NEW Controls.FillinDecimal().
      THIS-OBJECT:fillinFriInLop = NEW Controls.FillinMask().
      THIS-OBJECT:splitContainerKalkFri = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKodoFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkBer = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKoderMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSchaktBer = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerBerKalkMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridBerVal = NEW Controls.Grid().
      THIS-OBJECT:GridBerMtrl = NEW Controls.Grid().
      THIS-OBJECT:GridSchaktVal = NEW Controls.Grid().
      THIS-OBJECT:splitContainerAndMTRL = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyBort = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonNy = NEW Controls.ButtonOver().
      THIS-OBJECT:AddRemoveStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:RemoveButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:AddButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerMtrlAnd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridEnrFran = NEW Controls.Grid().
      THIS-OBJECT:GridKalkylKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerFrekAnmKom = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFrekvens = NEW Controls.Grid().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:RubrikKom = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:Rubrikanm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:KalkylensKoderAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ValdKodKommentar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:GridFriKalk = NEW Controls.Grid().
      THIS-OBJECT:splitContainerGrundKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerAllaAndFree = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripAllaKoder = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonAllaKoder = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonTomKonst = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonHmtGrund = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripFriKalk = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonFriAter = NEW System.Windows.Forms.ToolStripButton().
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalk:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkArbkoder:SuspendLayout().
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerLopInAntal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopInAntal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopInAntal:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopInAntal:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkLopnr:SuspendLayout().
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:panelKodSok:SuspendLayout().
      THIS-OBJECT:flowLayoutPanel1:SuspendLayout().
      THIS-OBJECT:panelFriIn:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkFri:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkFri:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKodoFrek:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKalkBer, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkBer:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBer:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBer:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKoderMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKoderMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKoderMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKoderMtrl:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSchaktBer, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSchaktBer:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSchaktBer:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSchaktBer:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBerKalkMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBerKalkMtrl:SuspendLayout().
      CAST(THIS-OBJECT:GridBerVal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridBerMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridSchaktVal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAndMTRL, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAndMTRL:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAndMTRL:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAndMTRL:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyBort:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:SuspendLayout().
      THIS-OBJECT:AddRemoveStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMtrlAnd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMtrlAnd:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMtrlAnd:SuspendLayout().
      CAST(THIS-OBJECT:GridEnrFran, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkBild:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerFrekAnmKom, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFrekAnmKom:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFrekAnmKom:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFrekAnmKom:SuspendLayout().
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      CAST(THIS-OBJECT:RubrikKom, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerGrundKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerGrundKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKalk:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAllaAndFree, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAllaAndFree:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAllaAndFree:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAllaAndFree:SuspendLayout().
      THIS-OBJECT:toolStripAllaKoder:SuspendLayout().
      THIS-OBJECT:toolStripFriKalk:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKalk */
      /*  */
      THIS-OBJECT:splitContainerKalk:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainerKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalk:Name = "splitContainerKalk".
      /*  */
      /* splitContainerKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkArbkoder).
      /*  */
      /* splitContainerKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalk:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkFri).
      THIS-OBJECT:splitContainerKalk:Size = NEW System.Drawing.Size(1111, 517).
      THIS-OBJECT:splitContainerKalk:SplitterDistance = 291.
      THIS-OBJECT:splitContainerKalk:TabIndex = 12.
      /*  */
      /* splitContainerKalkArbkoder */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkArbkoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkArbkoder:Name = "splitContainerKalkArbkoder".
      THIS-OBJECT:splitContainerKalkArbkoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkArbkoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:Controls:Add(THIS-OBJECT:GridArbetsKoder).
      /*  */
      /* splitContainerKalkArbkoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopInAntal).
      THIS-OBJECT:splitContainerKalkArbkoder:Size = NEW System.Drawing.Size(289, 515).
      THIS-OBJECT:splitContainerKalkArbkoder:SplitterDistance = 233.
      THIS-OBJECT:splitContainerKalkArbkoder:TabIndex = 0.
      /*  */
      /* GridArbetsKoder */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbetsKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbetsKoder:GuruContext = ?.
      THIS-OBJECT:GridArbetsKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbetsKoder:Name = "GridArbetsKoder".
      THIS-OBJECT:GridArbetsKoder:Size = NEW System.Drawing.Size(289, 233).
      THIS-OBJECT:GridArbetsKoder:TabIndex = 4.
      THIS-OBJECT:GridArbetsKoder:Text = "grid1".
      /*  */
      /* splitContainerLopInAntal */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopInAntal:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerLopInAntal:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerLopInAntal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopInAntal:Name = "splitContainerLopInAntal".
      THIS-OBJECT:splitContainerLopInAntal:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLopInAntal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkLopnr).
      /*  */
      /* splitContainerLopInAntal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopInAntal:Panel2:Controls:Add(THIS-OBJECT:panelKodSok).
      THIS-OBJECT:splitContainerLopInAntal:Panel2:Controls:Add(THIS-OBJECT:panelFriIn).
      THIS-OBJECT:splitContainerLopInAntal:Size = NEW System.Drawing.Size(289, 278).
      THIS-OBJECT:splitContainerLopInAntal:SplitterDistance = 203.
      THIS-OBJECT:splitContainerLopInAntal:TabIndex = 1.
      /*  */
      /* splitContainerKalkLopnr */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkLopnr:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerKalkLopnr:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKalkLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkLopnr:Name = "splitContainerKalkLopnr".
      /*  */
      /* splitContainerKalkLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:Controls:Add(THIS-OBJECT:GridLopKoder).
      /*  */
      /* splitContainerKalkLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonOverMark).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:Controls:Add(THIS-OBJECT:KalkylButtonBackMark).
      THIS-OBJECT:splitContainerKalkLopnr:Size = NEW System.Drawing.Size(289, 203).
      THIS-OBJECT:splitContainerKalkLopnr:SplitterDistance = 251.
      THIS-OBJECT:splitContainerKalkLopnr:TabIndex = 1.
      /*  */
      /* GridLopKoder */
      /*  */
      THIS-OBJECT:GridLopKoder:AllowDrop = TRUE.
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridLopKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopKoder:GuruContext = ?.
      THIS-OBJECT:GridLopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopKoder:Name = "GridLopKoder".
      THIS-OBJECT:GridLopKoder:Size = NEW System.Drawing.Size(251, 203).
      THIS-OBJECT:GridLopKoder:TabIndex = 5.
      THIS-OBJECT:GridLopKoder:Text = "grid2".
      THIS-OBJECT:GridLopKoder:DoubleClickRow:Subscribe(THIS-OBJECT:GridLopKoder_DoubleClickRow).
      THIS-OBJECT:GridLopKoder:Click:Subscribe(THIS-OBJECT:GridLopKoder_Click).
      /*  */
      /* KalkylButtonOverMark */
      /*  */
      appearance25:Image = CAST(resources:GetObject("appearance25.Image"), System.Object).
      THIS-OBJECT:KalkylButtonOverMark:Appearance = appearance25.
      THIS-OBJECT:KalkylButtonOverMark:Location = NEW System.Drawing.Point(5, 5).
      THIS-OBJECT:KalkylButtonOverMark:Name = "KalkylButtonOverMark".
      THIS-OBJECT:KalkylButtonOverMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:KalkylButtonOverMark:TabIndex = 7.
      THIS-OBJECT:KalkylButtonOverMark:Click:Subscribe(THIS-OBJECT:ButtonOvermark_ClickE).
      /*  */
      /* KalkylButtonBackMark */
      /*  */
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      THIS-OBJECT:KalkylButtonBackMark:Appearance = appearance26.
      THIS-OBJECT:KalkylButtonBackMark:Location = NEW System.Drawing.Point(5, 50).
      THIS-OBJECT:KalkylButtonBackMark:Name = "KalkylButtonBackMark".
      THIS-OBJECT:KalkylButtonBackMark:Size = NEW System.Drawing.Size(25, 23).
      THIS-OBJECT:KalkylButtonBackMark:TabIndex = 9.
      THIS-OBJECT:KalkylButtonBackMark:Click:Subscribe(THIS-OBJECT:ButtonBackMark_Click).
      /*  */
      /* panelKodSok */
      /*  */
      THIS-OBJECT:panelKodSok:BackColor = System.Drawing.Color:AliceBlue.
      THIS-OBJECT:panelKodSok:Controls:Add(THIS-OBJECT:flowLayoutPanel1).
      THIS-OBJECT:panelKodSok:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:panelKodSok:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:panelKodSok:Name = "panelKodSok".
      THIS-OBJECT:panelKodSok:Size = NEW System.Drawing.Size(289, 71).
      THIS-OBJECT:panelKodSok:TabIndex = 4.
      THIS-OBJECT:panelKodSok:Visible = FALSE.
      /*  */
      /* flowLayoutPanel1 */
      /*  */
      THIS-OBJECT:flowLayoutPanel1:Controls:Add(THIS-OBJECT:label1).
      THIS-OBJECT:flowLayoutPanel1:Controls:Add(THIS-OBJECT:KodSokInput).
      THIS-OBJECT:flowLayoutPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:flowLayoutPanel1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:flowLayoutPanel1:Name = "flowLayoutPanel1".
      THIS-OBJECT:flowLayoutPanel1:Size = NEW System.Drawing.Size(289, 71).
      THIS-OBJECT:flowLayoutPanel1:TabIndex = 1.
      /*  */
      /* label1 */
      /*  */
      THIS-OBJECT:label1:AutoSize = TRUE.
      THIS-OBJECT:label1:Location = NEW System.Drawing.Point(3, 0).
      THIS-OBJECT:label1:Name = "label1".
      THIS-OBJECT:label1:Size = NEW System.Drawing.Size(84, 17).
      THIS-OBJECT:label1:TabIndex = 1.
      THIS-OBJECT:label1:Text = "Sök Benämning".
      THIS-OBJECT:label1:TextAlign = System.Drawing.ContentAlignment:BottomCenter.
      THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
      /*  */
      /* KodSokInput */
      /*  */
      THIS-OBJECT:KodSokInput:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KodSokInput:Location = NEW System.Drawing.Point(3, 20).
      THIS-OBJECT:KodSokInput:Name = "KodSokInput".
      THIS-OBJECT:KodSokInput:Size = NEW System.Drawing.Size(248, 20).
      THIS-OBJECT:KodSokInput:TabIndex = 0.
      THIS-OBJECT:KodSokInput:KeyDown:Subscribe(THIS-OBJECT:KodSokInput_KeyDown).
      /*  */
      /* panelFriIn */
      /*  */
      THIS-OBJECT:panelFriIn:BackColor = System.Drawing.Color:AliceBlue.
      THIS-OBJECT:panelFriIn:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:panelFriIn:Controls:Add(THIS-OBJECT:fillinFriInKod).
      THIS-OBJECT:panelFriIn:Controls:Add(THIS-OBJECT:fillinFriInAntal).
      THIS-OBJECT:panelFriIn:Controls:Add(THIS-OBJECT:fillinFriInLop).
      THIS-OBJECT:panelFriIn:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:panelFriIn:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:panelFriIn:Name = "panelFriIn".
      THIS-OBJECT:panelFriIn:Size = NEW System.Drawing.Size(289, 71).
      THIS-OBJECT:panelFriIn:TabIndex = 3.
      THIS-OBJECT:panelFriIn:Visible = FALSE.
      /*  */
      /* fillinFriInKod */
      /*  */
      THIS-OBJECT:fillinFriInKod:Location = NEW System.Drawing.Point(20, 5).
      THIS-OBJECT:fillinFriInKod:Name = "fillinFriInKod".
      THIS-OBJECT:fillinFriInKod:Size = NEW System.Drawing.Size(120, 28).
      THIS-OBJECT:fillinFriInKod:TabIndex = 0.
      /*  */
      /* fillinFriInAntal */
      /*  */
      THIS-OBJECT:fillinFriInAntal:Location = NEW System.Drawing.Point(20, 34).
      THIS-OBJECT:fillinFriInAntal:Name = "fillinFriInAntal".
      THIS-OBJECT:fillinFriInAntal:Size = NEW System.Drawing.Size(261, 28).
      THIS-OBJECT:fillinFriInAntal:TabIndex = 2.
      /*  */
      /* fillinFriInLop */
      /*  */
      THIS-OBJECT:fillinFriInLop:Location = NEW System.Drawing.Point(146, 5).
      THIS-OBJECT:fillinFriInLop:Name = "fillinFriInLop".
      THIS-OBJECT:fillinFriInLop:Size = NEW System.Drawing.Size(136, 28).
      THIS-OBJECT:fillinFriInLop:TabIndex = 1.
      /*  */
      /* splitContainerKalkFri */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkFri:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkFri:Name = "splitContainerKalkFri".
      /*  */
      /* splitContainerKalkFri.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkKodoFrek).
      /*  */
      /* splitContainerKalkFri.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkFri:Panel2:Controls:Add(THIS-OBJECT:GridFriKalk).
      THIS-OBJECT:splitContainerKalkFri:Size = NEW System.Drawing.Size(814, 515).
      THIS-OBJECT:splitContainerKalkFri:SplitterDistance = 509.
      THIS-OBJECT:splitContainerKalkFri:TabIndex = 4.
      /*  */
      /* splitContainerKalkKodoFrek */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKodoFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKodoFrek:Name = "splitContainerKalkKodoFrek".
      /*  */
      /* splitContainerKalkKodoFrek.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:Controls:Add(THIS-OBJECT:splitContainerKalkBer).
      /*  */
      /* splitContainerKalkKodoFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkBild).
      THIS-OBJECT:splitContainerKalkKodoFrek:Size = NEW System.Drawing.Size(509, 515).
      THIS-OBJECT:splitContainerKalkKodoFrek:SplitterDistance = 381.
      THIS-OBJECT:splitContainerKalkKodoFrek:TabIndex = 36.
      /*  */
      /* splitContainerKalkBer */
      /*  */
      THIS-OBJECT:splitContainerKalkBer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkBer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkBer:Name = "splitContainerKalkBer".
      THIS-OBJECT:splitContainerKalkBer:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkBer.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkBer:Panel1:Controls:Add(THIS-OBJECT:splitContainerKoderMtrl).
      /*  */
      /* splitContainerKalkBer.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkBer:Panel2:Controls:Add(THIS-OBJECT:GridKalkylKoder).
      THIS-OBJECT:splitContainerKalkBer:Size = NEW System.Drawing.Size(381, 515).
      THIS-OBJECT:splitContainerKalkBer:SplitterDistance = 200.
      THIS-OBJECT:splitContainerKalkBer:SplitterWidth = 20.
      THIS-OBJECT:splitContainerKalkBer:TabIndex = 4.
      /*  */
      /* splitContainerKoderMtrl */
      /*  */
      THIS-OBJECT:splitContainerKoderMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKoderMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKoderMtrl:Name = "splitContainerKoderMtrl".
      THIS-OBJECT:splitContainerKoderMtrl:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKoderMtrl.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKoderMtrl:Panel1:Controls:Add(THIS-OBJECT:splitContainerSchaktBer).
      /*  */
      /* splitContainerKoderMtrl.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKoderMtrl:Panel2:Controls:Add(THIS-OBJECT:splitContainerAndMTRL).
      THIS-OBJECT:splitContainerKoderMtrl:Size = NEW System.Drawing.Size(381, 200).
      THIS-OBJECT:splitContainerKoderMtrl:SplitterDistance = 84.
      THIS-OBJECT:splitContainerKoderMtrl:TabIndex = 1.
      /*  */
      /* splitContainerSchaktBer */
      /*  */
      THIS-OBJECT:splitContainerSchaktBer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSchaktBer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSchaktBer:Name = "splitContainerSchaktBer".
      /*  */
      /* splitContainerSchaktBer.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSchaktBer:Panel1:Controls:Add(THIS-OBJECT:splitContainerBerKalkMtrl).
      /*  */
      /* splitContainerSchaktBer.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSchaktBer:Panel2:Controls:Add(THIS-OBJECT:GridSchaktVal).
      THIS-OBJECT:splitContainerSchaktBer:Size = NEW System.Drawing.Size(381, 84).
      THIS-OBJECT:splitContainerSchaktBer:SplitterDistance = 290.
      THIS-OBJECT:splitContainerSchaktBer:TabIndex = 0.
      /*  */
      /* splitContainerBerKalkMtrl */
      /*  */
      THIS-OBJECT:splitContainerBerKalkMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBerKalkMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBerKalkMtrl:Name = "splitContainerBerKalkMtrl".
      /*  */
      /* splitContainerBerKalkMtrl.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel1:Controls:Add(THIS-OBJECT:GridBerVal).
      /*  */
      /* splitContainerBerKalkMtrl.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel2:Controls:Add(THIS-OBJECT:GridBerMtrl).
      THIS-OBJECT:splitContainerBerKalkMtrl:Size = NEW System.Drawing.Size(290, 84).
      THIS-OBJECT:splitContainerBerKalkMtrl:SplitterDistance = 110.
      THIS-OBJECT:splitContainerBerKalkMtrl:TabIndex = 0.
      /*  */
      /* GridBerVal */
      /*  */
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridBerVal:DisplayLayout:Appearance = appearance27.
      THIS-OBJECT:GridBerVal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridBerVal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance28:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance28:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerVal:DisplayLayout:GroupByBox:Appearance = appearance28.
      appearance29:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerVal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance29.
      THIS-OBJECT:GridBerVal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance30:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance30:BackColor2 = System.Drawing.SystemColors:Control.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance30:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerVal:DisplayLayout:GroupByBox:PromptAppearance = appearance30.
      THIS-OBJECT:GridBerVal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridBerVal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      appearance31:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:ActiveCellAppearance = appearance31.
      appearance32:BackColor = System.Drawing.SystemColors:Highlight.
      appearance32:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:ActiveRowAppearance = appearance32.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:CardAreaAppearance = appearance33.
      appearance34:BorderColor = System.Drawing.Color:Silver.
      appearance34:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:CellAppearance = appearance34.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:CellPadding = 0.
      appearance35:BackColor = System.Drawing.SystemColors:Control.
      appearance35:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance35:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance35:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance35:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:GroupByRowAppearance = appearance35.
      appearance36:TextHAlignAsString = "Left".
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:HeaderAppearance = appearance36.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:RowAppearance = appearance37.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridBerVal:DisplayLayout:Override:TemplateAddRowAppearance = appearance38.
      THIS-OBJECT:GridBerVal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridBerVal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridBerVal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridBerVal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridBerVal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridBerVal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridBerVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridBerVal:GuruContext = ?.
      THIS-OBJECT:GridBerVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridBerVal:Name = "GridBerVal".
      THIS-OBJECT:GridBerVal:Size = NEW System.Drawing.Size(110, 84).
      THIS-OBJECT:GridBerVal:TabIndex = 0.
      THIS-OBJECT:GridBerVal:Text = "grid1".
      /*  */
      /* GridBerMtrl */
      /*  */
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridBerMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridBerMtrl:GuruContext = ?.
      THIS-OBJECT:GridBerMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridBerMtrl:Name = "GridBerMtrl".
      THIS-OBJECT:GridBerMtrl:Size = NEW System.Drawing.Size(176, 84).
      THIS-OBJECT:GridBerMtrl:TabIndex = 0.
      THIS-OBJECT:GridBerMtrl:Text = "grid1".
      THIS-OBJECT:GridBerMtrl:AfterCellUpdate:Subscribe(THIS-OBJECT:GridBerMtrl_AfterCellUpdate).
      THIS-OBJECT:GridBerMtrl:AfterSelectChange:Subscribe(THIS-OBJECT:GridBerMtrl_AfterSelectChange).
      /*  */
      /* GridSchaktVal */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSchaktVal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSchaktVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSchaktVal:GuruContext = ?.
      THIS-OBJECT:GridSchaktVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSchaktVal:Name = "GridSchaktVal".
      THIS-OBJECT:GridSchaktVal:Size = NEW System.Drawing.Size(87, 84).
      THIS-OBJECT:GridSchaktVal:TabIndex = 0.
      THIS-OBJECT:GridSchaktVal:Text = "grid1".
      /*  */
      /* splitContainerAndMTRL */
      /*  */
      THIS-OBJECT:splitContainerAndMTRL:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAndMTRL:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerAndMTRL:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAndMTRL:Name = "splitContainerAndMTRL".
      THIS-OBJECT:splitContainerAndMTRL:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerAndMTRL.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAndMTRL:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyBort).
      /*  */
      /* splitContainerAndMTRL.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAndMTRL:Panel2:Controls:Add(THIS-OBJECT:splitContainerMtrlAnd).
      THIS-OBJECT:splitContainerAndMTRL:Size = NEW System.Drawing.Size(381, 112).
      THIS-OBJECT:splitContainerAndMTRL:SplitterDistance = 25.
      THIS-OBJECT:splitContainerAndMTRL:TabIndex = 0.
      /*  */
      /* splitContainerNyBort */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyBort:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyBort:Name = "splitContainerNyBort".
      /*  */
      /* splitContainerNyBort.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel1:Controls:Add(THIS-OBJECT:ButtonNy).
      /*  */
      /* splitContainerNyBort.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel2:Controls:Add(THIS-OBJECT:AddRemoveStrip).
      THIS-OBJECT:splitContainerNyBort:Size = NEW System.Drawing.Size(381, 25).
      THIS-OBJECT:splitContainerNyBort:SplitterDistance = 238.
      THIS-OBJECT:splitContainerNyBort:TabIndex = 7.
      /*  */
      /* ButtonNy */
      /*  */
      THIS-OBJECT:ButtonNy:GuruTooltip = "Lägg till post".
      THIS-OBJECT:ButtonNy:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ButtonNy:Name = "ButtonNy".
      THIS-OBJECT:ButtonNy:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonNy:TabIndex = 5.
      THIS-OBJECT:ButtonNy:Text = " ".
      /*  */
      /* AddRemoveStrip */
      /*  */
      THIS-OBJECT:AddRemoveStrip:Dock = System.Windows.Forms.DockStyle:None.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:RemoveButton.
      arrayvar0[2] = THIS-OBJECT:AddButton.
      THIS-OBJECT:AddRemoveStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:AddRemoveStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:AddRemoveStrip:Name = "AddRemoveStrip".
      THIS-OBJECT:AddRemoveStrip:Size = NEW System.Drawing.Size(253, 25).
      THIS-OBJECT:AddRemoveStrip:TabIndex = 5.
      THIS-OBJECT:AddRemoveStrip:Text = "toolStrip1".
      /*  */
      /* RemoveButton */
      /*  */
      THIS-OBJECT:RemoveButton:Image = CAST(resources:GetObject("RemoveButton.Image"), System.Drawing.Image).
      THIS-OBJECT:RemoveButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:RemoveButton:Name = "RemoveButton".
      THIS-OBJECT:RemoveButton:Size = NEW System.Drawing.Size(133, 22).
      THIS-OBJECT:RemoveButton:Text = "Ta bort valda poster!".
      /*  */
      /* AddButton */
      /*  */
      THIS-OBJECT:AddButton:Image = CAST(resources:GetObject("AddButton.Image"), System.Drawing.Image).
      THIS-OBJECT:AddButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AddButton:Name = "AddButton".
      THIS-OBJECT:AddButton:Size = NEW System.Drawing.Size(108, 22).
      THIS-OBJECT:AddButton:Text = "Lägg till poster!".
      /*  */
      /* splitContainerMtrlAnd */
      /*  */
      THIS-OBJECT:splitContainerMtrlAnd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMtrlAnd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMtrlAnd:Name = "splitContainerMtrlAnd".
      /*  */
      /* splitContainerMtrlAnd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMtrlAnd:Panel2:Controls:Add(THIS-OBJECT:GridEnrFran).
      THIS-OBJECT:splitContainerMtrlAnd:Size = NEW System.Drawing.Size(381, 83).
      THIS-OBJECT:splitContainerMtrlAnd:SplitterDistance = 190.
      THIS-OBJECT:splitContainerMtrlAnd:TabIndex = 0.
      /*  */
      /* GridEnrFran */
      /*  */
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Appearance = appearance63.
      THIS-OBJECT:GridEnrFran:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEnrFran:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance64:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance64:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance64:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:Appearance = appearance64.
      appearance65:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:BandLabelAppearance = appearance65.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance66:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance66:BackColor2 = System.Drawing.SystemColors:Control.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:GroupByBox:PromptAppearance = appearance66.
      THIS-OBJECT:GridEnrFran:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEnrFran:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance67:BackColor = System.Drawing.SystemColors:Window.
      appearance67:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveCellAppearance = appearance67.
      appearance68:BackColor = System.Drawing.SystemColors:Highlight.
      appearance68:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:ActiveRowAppearance = appearance68.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CardAreaAppearance = appearance69.
      appearance70:BorderColor = System.Drawing.Color:Silver.
      appearance70:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellAppearance = appearance70.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:CellPadding = 0.
      appearance71:BackColor = System.Drawing.SystemColors:Control.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:GroupByRowAppearance = appearance71.
      appearance72:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderAppearance = appearance72.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance73:BackColor = System.Drawing.SystemColors:Window.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:RowAppearance = appearance73.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance74:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEnrFran:DisplayLayout:Override:TemplateAddRowAppearance = appearance74.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEnrFran:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEnrFran:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEnrFran:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEnrFran:GuruContext = ?.
      THIS-OBJECT:GridEnrFran:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEnrFran:Name = "GridEnrFran".
      THIS-OBJECT:GridEnrFran:Size = NEW System.Drawing.Size(187, 83).
      THIS-OBJECT:GridEnrFran:TabIndex = 16.
      THIS-OBJECT:GridEnrFran:Text = "Från".
      /*  */
      /* GridKalkylKoder */
      /*  */
      THIS-OBJECT:GridKalkylKoder:AllowDrop = TRUE.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Appearance = appearance75.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance76:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance76:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance76:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:Appearance = appearance76.
      appearance77:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance77.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance78:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance78:BackColor2 = System.Drawing.SystemColors:Control.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance78:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance78.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance79:BackColor = System.Drawing.SystemColors:Window.
      appearance79:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveCellAppearance = appearance79.
      appearance80:BackColor = System.Drawing.SystemColors:Highlight.
      appearance80:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveRowAppearance = appearance80.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance81:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CardAreaAppearance = appearance81.
      appearance82:BorderColor = System.Drawing.Color:Silver.
      appearance82:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellAppearance = appearance82.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellPadding = 0.
      appearance83:BackColor = System.Drawing.SystemColors:Control.
      appearance83:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance83:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance83:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance83:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:GroupByRowAppearance = appearance83.
      appearance84:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderAppearance = appearance84.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance85:BackColor = System.Drawing.SystemColors:Window.
      appearance85:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowAppearance = appearance85.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance86:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance86.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKalkylKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKalkylKoder:GuruContext = ?.
      THIS-OBJECT:GridKalkylKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridKalkylKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridKalkylKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKalkylKoder:Name = "GridKalkylKoder".
      THIS-OBJECT:GridKalkylKoder:Size = NEW System.Drawing.Size(381, 295).
      THIS-OBJECT:GridKalkylKoder:TabIndex = 3.
      THIS-OBJECT:GridKalkylKoder:Text = "grid3".
      THIS-OBJECT:GridKalkylKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKalkylKoder_GuruAfterDropped).
      THIS-OBJECT:GridKalkylKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterCellUpdate).
      THIS-OBJECT:GridKalkylKoder:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_BeforeCellUpdate).
      THIS-OBJECT:GridKalkylKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterSelectChange).
      THIS-OBJECT:GridKalkylKoder:DoubleClickRow:Subscribe(THIS-OBJECT:GridKalkylKoder_DoubleClickRow).
      THIS-OBJECT:GridKalkylKoder:Click:Subscribe(THIS-OBJECT:GridKalkylKoder_Click).
      /*  */
      /* splitContainerKalkBild */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkBild:Name = "splitContainerKalkBild".
      /*  */
      /* splitContainerKalkBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerFrekAnmKom).
      /*  */
      /* splitContainerKalkBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKalkBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKalkBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
      THIS-OBJECT:splitContainerKalkBild:Size = NEW System.Drawing.Size(124, 515).
      THIS-OBJECT:splitContainerKalkBild:SplitterDistance = 51.
      THIS-OBJECT:splitContainerKalkBild:TabIndex = 0.
      /*  */
      /* splitContainerFrekAnmKom */
      /*  */
      THIS-OBJECT:splitContainerFrekAnmKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFrekAnmKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFrekAnmKom:Name = "splitContainerFrekAnmKom".
      /*  */
      /* splitContainerFrekAnmKom.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFrekAnmKom:Panel1:Controls:Add(THIS-OBJECT:GridFrekvens).
      /*  */
      /* splitContainerFrekAnmKom.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFrekAnmKom:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      THIS-OBJECT:splitContainerFrekAnmKom:Size = NEW System.Drawing.Size(51, 515).
      THIS-OBJECT:splitContainerFrekAnmKom:SplitterDistance = 25.
      THIS-OBJECT:splitContainerFrekAnmKom:TabIndex = 1.
      /*  */
      /* GridFrekvens */
      /*  */
      appearance87:BackColor = System.Drawing.SystemColors:Window.
      appearance87:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Appearance = appearance87.
      THIS-OBJECT:GridFrekvens:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFrekvens:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance88:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance88:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance88:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance88:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:Appearance = appearance88.
      appearance89:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BandLabelAppearance = appearance89.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance90:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance90:BackColor2 = System.Drawing.SystemColors:Control.
      appearance90:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance90:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:GroupByBox:PromptAppearance = appearance90.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance91:BackColor = System.Drawing.SystemColors:Window.
      appearance91:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveCellAppearance = appearance91.
      appearance92:BackColor = System.Drawing.SystemColors:Highlight.
      appearance92:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:ActiveRowAppearance = appearance92.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance93:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CardAreaAppearance = appearance93.
      appearance94:BorderColor = System.Drawing.Color:Silver.
      appearance94:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellAppearance = appearance94.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:CellPadding = 0.
      appearance95:BackColor = System.Drawing.SystemColors:Control.
      appearance95:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance95:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance95:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance95:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:GroupByRowAppearance = appearance95.
      appearance96:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderAppearance = appearance96.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance97:BackColor = System.Drawing.SystemColors:Window.
      appearance97:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowAppearance = appearance97.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance98:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:Override:TemplateAddRowAppearance = appearance98.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFrekvens:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFrekvens:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFrekvens:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFrekvens:GuruContext = ?.
      THIS-OBJECT:GridFrekvens:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFrekvens:Name = "GridFrekvens".
      THIS-OBJECT:GridFrekvens:Size = NEW System.Drawing.Size(25, 515).
      THIS-OBJECT:GridFrekvens:TabIndex = 37.
      THIS-OBJECT:GridFrekvens:Text = "grid1".
      /*  */
      /* splitContainerKalkKomAnm */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKomAnm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKomAnm:Name = "splitContainerKalkKomAnm".
      THIS-OBJECT:splitContainerKalkKomAnm:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKalkKomAnm.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:Controls:Add(THIS-OBJECT:RubrikKom).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:Controls:Add(THIS-OBJECT:Rubrikanm).
      /*  */
      /* splitContainerKalkKomAnm.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:KalkylensKoderAnmarkning).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:Controls:Add(THIS-OBJECT:ValdKodKommentar).
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(25, 515).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 33.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* RubrikKom */
      /*  */
      THIS-OBJECT:RubrikKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:RubrikKom:Enabled = FALSE.
      THIS-OBJECT:RubrikKom:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:RubrikKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:RubrikKom:Name = "RubrikKom".
      THIS-OBJECT:RubrikKom:Size = NEW System.Drawing.Size(25, 33).
      THIS-OBJECT:RubrikKom:TabIndex = 1.
      THIS-OBJECT:RubrikKom:Text = "Anmärkning".
      /*  */
      /* Rubrikanm */
      /*  */
      THIS-OBJECT:Rubrikanm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Rubrikanm:Enabled = FALSE.
      THIS-OBJECT:Rubrikanm:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold).
      THIS-OBJECT:Rubrikanm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:Rubrikanm:Name = "Rubrikanm".
      THIS-OBJECT:Rubrikanm:Size = NEW System.Drawing.Size(25, 33).
      THIS-OBJECT:Rubrikanm:TabIndex = 0.
      THIS-OBJECT:Rubrikanm:Text = "Anmärkning".
      /*  */
      /* KalkylensKoderAnmarkning */
      /*  */
      THIS-OBJECT:KalkylensKoderAnmarkning:AcceptsReturn = TRUE.
      THIS-OBJECT:KalkylensKoderAnmarkning:AcceptsTab = TRUE.
      THIS-OBJECT:KalkylensKoderAnmarkning:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:KalkylensKoderAnmarkning:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:KalkylensKoderAnmarkning:Multiline = TRUE.
      THIS-OBJECT:KalkylensKoderAnmarkning:Name = "KalkylensKoderAnmarkning".
      THIS-OBJECT:KalkylensKoderAnmarkning:Size = NEW System.Drawing.Size(25, 478).
      THIS-OBJECT:KalkylensKoderAnmarkning:TabIndex = 37.
      THIS-OBJECT:KalkylensKoderAnmarkning:Text = " ".
      THIS-OBJECT:KalkylensKoderAnmarkning:AfterExitEditMode:Subscribe(THIS-OBJECT:KalkylensKoderAnmarkning_AfterExitEditMode).
      /*  */
      /* ValdKodKommentar */
      /*  */
      THIS-OBJECT:ValdKodKommentar:AcceptsReturn = TRUE.
      THIS-OBJECT:ValdKodKommentar:AcceptsTab = TRUE.
      THIS-OBJECT:ValdKodKommentar:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ValdKodKommentar:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ValdKodKommentar:Multiline = TRUE.
      THIS-OBJECT:ValdKodKommentar:Name = "ValdKodKommentar".
      THIS-OBJECT:ValdKodKommentar:ReadOnly = TRUE.
      THIS-OBJECT:ValdKodKommentar:Size = NEW System.Drawing.Size(25, 478).
      THIS-OBJECT:ValdKodKommentar:TabIndex = 36.
      THIS-OBJECT:ValdKodKommentar:Text = " ".
      /*  */
      /* GridFriKalk */
      /*  */
      appearance99:BackColor = System.Drawing.SystemColors:Window.
      appearance99:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Appearance = appearance99.
      THIS-OBJECT:GridFriKalk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFriKalk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance100:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance100:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance100:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance100:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:Appearance = appearance100.
      appearance101:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance101.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance102:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance102:BackColor2 = System.Drawing.SystemColors:Control.
      appearance102:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance102:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:PromptAppearance = appearance102.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance103:BackColor = System.Drawing.SystemColors:Window.
      appearance103:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveCellAppearance = appearance103.
      appearance104:BackColor = System.Drawing.SystemColors:Highlight.
      appearance104:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveRowAppearance = appearance104.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance105:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CardAreaAppearance = appearance105.
      appearance106:BorderColor = System.Drawing.Color:Silver.
      appearance106:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellAppearance = appearance106.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellPadding = 0.
      appearance107:BackColor = System.Drawing.SystemColors:Control.
      appearance107:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance107:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance107:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance107:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:GroupByRowAppearance = appearance107.
      appearance108:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderAppearance = appearance108.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance109:BackColor = System.Drawing.SystemColors:Window.
      appearance109:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowAppearance = appearance109.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance110:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:TemplateAddRowAppearance = appearance110.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFriKalk:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFriKalk:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFriKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFriKalk:GuruContext = ?.
      THIS-OBJECT:GridFriKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFriKalk:Name = "GridFriKalk".
      THIS-OBJECT:GridFriKalk:Size = NEW System.Drawing.Size(301, 515).
      THIS-OBJECT:GridFriKalk:TabIndex = 0.
      THIS-OBJECT:GridFriKalk:Text = "grid1".
      THIS-OBJECT:GridFriKalk:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFriKalk_AfterCellUpdate).
      /*  */
      /* splitContainerGrundKalk */
      /*  */
      THIS-OBJECT:splitContainerGrundKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundKalk:Name = "splitContainerGrundKalk".
      THIS-OBJECT:splitContainerGrundKalk:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerGrundKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundKalk:Panel1:Controls:Add(THIS-OBJECT:splitContainerAllaAndFree).
      /*  */
      /* splitContainerGrundKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundKalk:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalk).
      THIS-OBJECT:splitContainerGrundKalk:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerGrundKalk:SplitterDistance = 25.
      THIS-OBJECT:splitContainerGrundKalk:TabIndex = 0.
      /*  */
      /* splitContainerAllaAndFree */
      /*  */
      THIS-OBJECT:splitContainerAllaAndFree:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAllaAndFree:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAllaAndFree:Name = "splitContainerAllaAndFree".
      /*  */
      /* splitContainerAllaAndFree.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAllaAndFree:Panel1:Controls:Add(THIS-OBJECT:toolStripAllaKoder).
      /*  */
      /* splitContainerAllaAndFree.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAllaAndFree:Panel2:Controls:Add(THIS-OBJECT:toolStripFriKalk).
      THIS-OBJECT:splitContainerAllaAndFree:Size = NEW System.Drawing.Size(1111, 25).
      THIS-OBJECT:splitContainerAllaAndFree:SplitterDistance = 800.
      THIS-OBJECT:splitContainerAllaAndFree:TabIndex = 1.
      /*  */
      /* toolStripAllaKoder */
      /*  */
      THIS-OBJECT:toolStripAllaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:toolStripButtonAllaKoder.
      arrayvar1[2] = THIS-OBJECT:toolStripButtonTomKonst.
      arrayvar1[3] = THIS-OBJECT:toolStripButtonHmtGrund.
      THIS-OBJECT:toolStripAllaKoder:Items:AddRange(arrayvar1).
      THIS-OBJECT:toolStripAllaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripAllaKoder:Name = "toolStripAllaKoder".
      THIS-OBJECT:toolStripAllaKoder:Size = NEW System.Drawing.Size(800, 25).
      THIS-OBJECT:toolStripAllaKoder:TabIndex = 0.
      THIS-OBJECT:toolStripAllaKoder:Text = "toolStrip1".
      /*  */
      /* toolStripButtonAllaKoder */
      /*  */
      THIS-OBJECT:toolStripButtonAllaKoder:Image = CAST(resources:GetObject("toolStripButtonAllaKoder.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonAllaKoder:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonAllaKoder:Name = "toolStripButtonAllaKoder".
      THIS-OBJECT:toolStripButtonAllaKoder:Size = NEW System.Drawing.Size(99, 22).
      THIS-OBJECT:toolStripButtonAllaKoder:Text = "Välj alla koder".
      THIS-OBJECT:toolStripButtonAllaKoder:Click:Subscribe(THIS-OBJECT:toolStripButtonAllaKoder_Click).
      /*  */
      /* toolStripButtonTomKonst */
      /*  */
      THIS-OBJECT:toolStripButtonTomKonst:Image = CAST(resources:GetObject("toolStripButtonTomKonst.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonTomKonst:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonTomKonst:Name = "toolStripButtonTomKonst".
      THIS-OBJECT:toolStripButtonTomKonst:Size = NEW System.Drawing.Size(267, 22).
      THIS-OBJECT:toolStripButtonTomKonst:Text = "Ta bort alla koder på vald konstruktion/schakt".
      THIS-OBJECT:toolStripButtonTomKonst:Click:Subscribe(THIS-OBJECT:toolStripButtonTomKonst_Click).
      /*  */
      /* toolStripButtonHmtGrund */
      /*  */
      THIS-OBJECT:toolStripButtonHmtGrund:Image = CAST(resources:GetObject("toolStripButtonHmtGrund.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonHmtGrund:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonHmtGrund:Name = "toolStripButtonHmtGrund".
      THIS-OBJECT:toolStripButtonHmtGrund:Size = NEW System.Drawing.Size(269, 22).
      THIS-OBJECT:toolStripButtonHmtGrund:Text = "Hämta grundupplägg för konstruktion/schakt".
      THIS-OBJECT:toolStripButtonHmtGrund:Click:Subscribe(THIS-OBJECT:toolStripButtonHmtGrund_Click).
      /*  */
      /* toolStripFriKalk */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:toolStripButtonFriAter.
      THIS-OBJECT:toolStripFriKalk:Items:AddRange(arrayvar2).
      THIS-OBJECT:toolStripFriKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripFriKalk:Name = "toolStripFriKalk".
      THIS-OBJECT:toolStripFriKalk:Size = NEW System.Drawing.Size(307, 25).
      THIS-OBJECT:toolStripFriKalk:TabIndex = 0.
      THIS-OBJECT:toolStripFriKalk:Text = "toolStrip1".
      /*  */
      /* toolStripButtonFriAter */
      /*  */
      THIS-OBJECT:toolStripButtonFriAter:Image = CAST(resources:GetObject("toolStripButtonFriAter.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonFriAter:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonFriAter:Name = "toolStripButtonFriAter".
      THIS-OBJECT:toolStripButtonFriAter:Size = NEW System.Drawing.Size(116, 22).
      THIS-OBJECT:toolStripButtonFriAter:Text = "Återställ Frikalkyl".
      THIS-OBJECT:toolStripButtonFriAter:Click:Subscribe(THIS-OBJECT:toolStripButtonFriAter_Click).
      /*  */
      /* KalkylControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerGrundKalk).
      THIS-OBJECT:Name = "KalkylControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkArbkoder:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkArbkoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkArbkoder:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopInAntal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopInAntal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLopInAntal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopInAntal:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkLopnr:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkLopnr, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkLopnr:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:panelKodSok:ResumeLayout(FALSE).
      THIS-OBJECT:flowLayoutPanel1:ResumeLayout(FALSE).
      THIS-OBJECT:flowLayoutPanel1:PerformLayout().
      THIS-OBJECT:panelFriIn:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkFri:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkFri, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkFri:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKodoFrek:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkKodoFrek, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKodoFrek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkBer:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkBer:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkBer, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBer:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKoderMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKoderMtrl:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKoderMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKoderMtrl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSchaktBer:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSchaktBer:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSchaktBer, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSchaktBer:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBerKalkMtrl:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBerKalkMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBerKalkMtrl:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridBerVal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridBerMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridSchaktVal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAndMTRL:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAndMTRL:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAndMTRL, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAndMTRL:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyBort:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:PerformLayout().
      THIS-OBJECT:splitContainerMtrlAnd:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMtrlAnd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMtrlAnd:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridEnrFran, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKalkBild, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFrekAnmKom:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFrekAnmKom:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFrekAnmKom, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFrekAnmKom:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFrekvens, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerKalkKomAnm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:RubrikKom, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:Rubrikanm, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:KalkylensKoderAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ValdKodKommentar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKalk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerGrundKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundKalk:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAllaAndFree:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAllaAndFree:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerAllaAndFree:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAllaAndFree:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerAllaAndFree, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAllaAndFree:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripAllaKoder:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripAllaKoder:PerformLayout().
      THIS-OBJECT:toolStripFriKalk:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripFriKalk:PerformLayout().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
	
	METHOD PUBLIC VOID ButtonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderSelected().
         
         THIS-OBJECT:KalkylButtonBackMark:GuruOver().
        
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
     
         
         THIS-OBJECT:ControlShell:DontCheck = FALSE.
       
         IF THIS-OBJECT:GridKalkylKoder:Posn() = -1 AND Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:KalkdbControl:Berkalkstopp(INPUT "STOPP",INPUT TRUE).
         END.
       
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
   END METHOD.
	
	
	METHOD PRIVATE VOID ButtonOvermark_ClickE( INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:ButtonOvermark_Click(1).
      RETURN.
   END METHOD.
   

   METHOD PRIVATE VOID ButtonOvermark_Click(INPUT antalvar AS DECIMAL):
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:KalkylButtonOverMark:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:KalkylButtonOverMark:GuruOver().
         
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKoderRows(antalvar).
         /* FIXAR OM NY VÄRDEN*/
         THIS-OBJECT:KalkylButtonOverMark:GuruOverefteruppdat().
         
         IF THIS-OBJECT:GridKalkylKoder:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:KalkylButtonOverMark:GuruTogrid:GuruCellvaljare("antal").
         
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
      RETURN.
   END METHOD.
	
	
	/*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridFriKalk_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridFriKalk:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
     
      THIS-OBJECT:GridFriKalk:SyncMan().  
      THIS-OBJECT:KalkdbControl:ValdaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKodSub().
      THIS-OBJECT:ControlShell:BerKalkKod().
      RETURN.

   END METHOD.
	
	/*------------------------------------------------------------------------------
         Purpose:    skickar data till db efter cell uppdat                                                
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   
   
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      
      
      THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE ), NO-LOCK) NO-ERROR.
      THIS-OBJECT:GridKalkylKoder:ExtraToolStripMenuItem:ENABLED = TRUE.
      
      kolnamnRiskVinst = e:Cell:Column:ToString(). 
      kolvardeRiskVinst = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKalkylKoder:Guruegenskap:getRubrik(kolnamnRiskVinst):Readonly = TRUE THEN RETURN.
      
      IF kolnamnRiskVinst = "MATRIS" AND kolvardeRiskVinst =  "0" THEN DO:
         kolvardeRiskVinst = "1".
         THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamnRiskVinst):BUFFER-VALUE = kolvardeRiskVinst.
         THIS-OBJECT:GridKalkylKoder:GuruReopen().
      END.   
      ELSE THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamnRiskVinst):BUFFER-VALUE = kolvardeRiskVinst.   
            
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().
      THIS-OBJECT:ControlShell:BerKalkKod().
      
      RETURN.

   END METHOD.
   METHOD PRIVATE VOID GridKalkylKoder_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
     
      kolnamnRiskVinst = e:Cell:Column:ToString().        
      kolvardeRiskVinst = e:Cell:VALUE:ToString() NO-ERROR.
      RETURN.
   END METHOD.
   
   
	METHOD PUBLIC VOID SammaRiskVinst(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      IF kolnamnRiskVinst = "RISK" OR   kolnamnRiskVinst = "VINST" THEN DO:
         kolvardeRiskVinst = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamnRiskVinst ):BUFFER-VALUE().
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE + " WHERE " + " NO-LOCK".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().   
         DO WHILE qH:QUERY-OFF-END = FALSE:         
            THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD(kolnamnRiskVinst):BUFFER-VALUE = kolvardeRiskVinst.  
            qH:GET-NEXT(NO-LOCK).
         END.
         THIS-OBJECT:KalkdbControl:KalkSpara().
      END.
      THIS-OBJECT:GridKalkylKoder:GuruReOpen().
      
   END METHOD.
   
	
	
	
	/*------------------------------------------------------------------------------
         Purpose:    gör då samma som buttenover                                                  
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopKoder THEN DO:
         THIS-OBJECT:ButtonOvermark_ClickE(?,?).
      END.
      RETURN.

   END METHOD.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PUBLIC VOID GridKalkylKoder_AfterSelectChange ( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN DO:
         IF THIS-OBJECT:GridFriKalk:GuruEgenskap NE ? THEN DO:
            IF THIS-OBJECT:GridKalkylKoder:ActiveRow = ? THEN DO:
               THIS-OBJECT:GridKalkylKoder:GuruFirstRow().              
            END.   
            IF THIS-OBJECT:GridKalkylKoder:ActiveRow NE ? THEN DO:
               IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells NE ? THEN DO:
                  num = THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:VALUE.
                  /*Anders Olsson Elpool i Umeå AB  18 maj 2015 16:09:29 
                  THIS-OBJECT:GridFriKalk:GuruFiltrera("NUM = " + STRING(num) + " AND EGENKODUPP = TRUE").
                  BARA FAKT OCH EGNA PRISER 
                  */
                  
                 
                  THIS-OBJECT:GridFriKalk:GuruFiltrera("NUM = " + STRING(num) ).
                  
               END.
            END.
         END.
      END.
      RETURN.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KalkylensKoderAnmarkning_AfterExitEditMode( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:   
         THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkylensKoderAnmarkning:Text).
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
      END.
		RETURN.

	END METHOD.

    METHOD PUBLIC VOID KodSokInput_KeyDown(INPUT sender AS System.Object , INPUT e AS System.Windows.Forms.KeyEventArgs ):
        IF e:KeyCode:ToString() EQ "Return" THEN DO:
           THIS-OBJECT:ControlShell:RefreshArbKoder(?,?).
        END.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonAllaKoder_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Root:DatabaseManager:Kalkyl:AllaKoder().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonFriAter_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlShell:RestoreFriKoder(?,?).
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonHmtGrund_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
    
      IF THIS-OBJECT:GridKalkylKoder:Posn() NE -1 THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(228),"",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN.
      END.   
		THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerkalkHmtgrund().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonTomKonst_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlShell:DontCheck = TRUE.
         THIS-OBJECT:GridKalkylKoder:MarkAll().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderSelected().
         THIS-OBJECT:KalkylButtonBackMark:GuruOver().
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
         THIS-OBJECT:ControlShell:DontCheck = FALSE.
         
         /*
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateTomEgen().
         */
         IF THIS-OBJECT:GridKalkylKoder:Posn() = -1 AND Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
            THIS-OBJECT:KalkdbControl:Berkalkstopp(INPUT "STOPP",INPUT TRUE).
         END.
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSpara().
         THIS-OBJECT:GridKalkylKoder:GuruReopen().
         
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
      RETURN.
     

	END METHOD.
	
	DESTRUCTOR PUBLIC KalkylControl ( ):
      /*{KALKYLCONTROLLDELETE.i}*/
   END DESTRUCTOR.

END CLASS.