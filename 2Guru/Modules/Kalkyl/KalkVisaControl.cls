 
 /*------------------------------------------------------------------------
    File        : KalkVisaControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu May 08 19:28:38 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Kalkyl.KalkVisaControl INHERITS UserControl: 
	{ARBKALK.I}
	{ARBKALKtth.I}
	DEFINE PUBLIC VARIABLE KalkdbControl  AS Modules.Kalkyl.Kalkyldb NO-UNDO.
	DEFINE PUBLIC VARIABLE toolStripButtonTillExcel AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE VARIABLE utrustrubrik AS LOGICAL NO-UNDO.
	DEFINE VARIABLE ordernrvar               AS INTEGER  NO-UNDO.
	DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE btimmarvar AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE mtimmarvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE masktimmarvar AS DECIMAL NO-UNDO.
   DEFINE PRIVATE VARIABLE tBs AS Progress.Data.BindingSource NO-UNDO.
   DEFINE VARIABLE utimmarvar AS DECIMAL NO-UNDO.
   
   DEFINE VARIABLE eavar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE arbkostvar AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE maskkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE mtrlkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ukostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE okostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE radsummavar AS DECIMAL NO-UNDO.
	DEFINE VARIABLE matrisiband AS INTEGER NO-UNDO.   
     
   DEFINE PUBLIC VARIABLE ControlShell                       AS Modules.Kalkyl.KalkylShell                                       NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbVisa AS Controls.GridJoin NO-UNDO.
	DEFINE PUBLIC  VARIABLE splitArbExcelKalk AS System.Windows.Forms.SplitContainer NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE vantapakalkyl AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripVisa AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonSkrivUt AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonGenerera AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonExporteraExcel AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVisa AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaPanelWaiting AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaPanelEmpty AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PUBLIC VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.
   DEFINE VARIABLE GridArbVisaSkap AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin             AS Controls.GridRubrikListaJoin                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoinTotal        AS Controls.GridRubrikListaJoin      {GridRubrikListaExtent.i}       NO-UNDO.
   DEFINE PUBLIC VARIABLE ArbKalkDSh AS HANDLE NO-UNDO.
   DEFINE DATASET ArbKalkDS FOR ArbKalkTT,ArbKalkhuvTT,ArbKalkKoderTT,ArbKalkAnmTT,ArbEgnaPriserTT,ArbFaktorerTT,ArbMtrlTT,ArbBerMtrlTT,ArbFrekTT,ArbKomTT
      DATA-RELATION visaDR FOR ArbKalkTT, ArbKalkhuvTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbKalkhuvTT.KALKNR)  
      DATA-RELATION visaAK FOR ArbKalkTT, ArbKalkKoderTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbKalkKoderTT.KALKNR) 
      DATA-RELATION visaDR FOR ArbKalkTT, ArbKalkAnmTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbKalkAnmTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbEgnaPriserTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbEgnaPriserTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbFaktorerTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbFaktorerTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbMtrlTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbMtrlTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbBerMtrlTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbBerMtrlTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbFrekTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbFrekTT.KALKNR)  
      DATA-RELATION visaDR FOR ArbKalkTT, ArbKomTT RELATION-FIELDS (ArbKalkTT.KALKNR,ArbKomTT.KALKNR)  .
      
      /*
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbKalkhuvTT. PARENT-ID-FIELD RID
      PARENT-ID-RELATION visaDR FOR ArbKalkTT,ArbKalkKoderTT PARENT-ID-FIELD RID 
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbKalkAnmTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbEgnaPriserTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbFaktorerTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbMtrlTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbBerMtrlTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbFrekTT PARENT-ID-FIELD RID  
      PARENT-ID-RELATION visaDR FOR ArbKalkTT, ArbKomTT PARENT-ID-FIELD RID.
      */
      
      
  
      
   
		
		
		
		
	CONSTRUCTOR PUBLIC KalkVisaControl ( INPUT r AS Guru.Root  ):
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

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.KalkVisaControl").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbKalkTT", -1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KALKNR", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbKalkhuvTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbKalkKoderTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbKalkAnmTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn125 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn125 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbEgnaPriserTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn126 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn126 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbFaktorerTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn127 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn127 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbMtrlTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn128 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn128 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbBerMtrlTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn129 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn129 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbFrekTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn130 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn130 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ArbKomTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup1 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup1 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Klicka på +", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbKalkhuvTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn131 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn131 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UPPGIFT", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn132 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn132 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TEXTER", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup2 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup2 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Kalkyil huvud", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand3 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand3 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbKalkKoderTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn133 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn133 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKODLOPNR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn134 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn134 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn135 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn135 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANTAL").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn136 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn136 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn137 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn137 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BTIMMAR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn138 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn138 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MTIMMAR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn139 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn139 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MASKTIMMAR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn140 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn140 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UTIMMAR").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn141 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn141 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn142 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn142 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn143 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn143 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MASKKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn144 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn144 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MTRLKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn145 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn145 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("OVRIGKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn146 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn146 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("UTRKOST").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn147 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn147 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("STSUMMA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn148 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn148 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RADSUMMA").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TTRECID").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKNING", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MARKSUB", 1).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand4 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand4 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbKalkAnmTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn149 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn149 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKODLOPNR", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn150 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn150 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn151 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn151 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANMARKNING", -1, ?, 686872683, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup3 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup3 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Registreade anmärkningar ", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand5 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand5 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbEgnaPriserTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn152 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn152 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn153 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn153 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PRIS", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup4 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup4 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Egna", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand6 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand6 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbFaktorerTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn154 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn154 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn155 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn155 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FAKTOR", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup5 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup5 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Egna Faktorer", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand7 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand7 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbMtrlTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn156 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn156 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENR", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn157 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn157 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn158 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn158 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANTAL", -1, ?, 686872683, 4, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn159 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn159 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET", -1, ?, 686872683, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn160 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn160 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("MATRIS", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn161 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn161 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PRIS", -1, ?, 686872683, 5, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn162 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn162 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SUMMA", -1, ?, 686872683, 6, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup6 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup6 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Materiel Spec.", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand8 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand8 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbBerMtrlTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn163 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn163 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENR", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn164 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn164 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn165 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn165 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANTAL", -1, ?, 686872683, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn166 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn166 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET", -1, ?, 686872683, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn167 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn167 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PRIS", -1, ?, 686872683, 4, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn168 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn168 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("SUMMA", -1, ?, 686872683, 5, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup7 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup7 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Materiel hämtad från beredningen", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand9 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand9 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbFrekTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn169 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn169 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKODLOPNR", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn170 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn170 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FREKODLOPNR", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn171 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn171 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ANTAL", -1, ?, 686872683, 2, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn172 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn172 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ENHET", -1, ?, 686872683, 4, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn173 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn173 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("BENAMNING", -1, ?, 686872683, 3, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup8 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup8 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Frekvens kod till nästa nivå", 686872683).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridBand10 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
      ultraGridBand10 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ArbKomTT", 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn174 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn174 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ARBKODLOPNR", -1, ?, 686872683, 0, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn175 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn175 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("KOMMENTAR", -1, ?, 686872683, 1, 0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
      ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TYPSNITT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraGridGroup9 AS Infragistics.Win.UltraWinGrid.UltraGridGroup NO-UNDO.
      ultraGridGroup9 = NEW Infragistics.Win.UltraWinGrid.UltraGridGroup("Kommentarer från vald katalog", 686872683).
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
      tableDesc1 = NEW Progress.Data.TableDesc("ArbKalkTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc2 = NEW Progress.Data.TableDesc("ArbKalkhuvTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc3 = NEW Progress.Data.TableDesc("ArbKalkKoderTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc4 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc4 = NEW Progress.Data.TableDesc("ArbKalkAnmTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc5 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc5 = NEW Progress.Data.TableDesc("ArbEgnaPriserTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc6 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc6 = NEW Progress.Data.TableDesc("ArbFaktorerTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc7 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc7 = NEW Progress.Data.TableDesc("ArbMtrlTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc8 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc8 = NEW Progress.Data.TableDesc("ArbBerMtrlTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc9 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc9 = NEW Progress.Data.TableDesc("ArbFrekTT").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc10 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc10 = NEW Progress.Data.TableDesc("ArbKomTT").
      THIS-OBJECT:splitContainerVisa = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripVisa = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonGenerera = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonExporteraExcel = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonSkrivUt = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonTillExcel = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitArbExcelKalk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:VisaPanelEmpty = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:VisaPanelWaiting = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:vantapakalkyl = NEW System.Windows.Forms.Label().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:GridArbVisa = NEW Controls.GridJoin().
      THIS-OBJECT:tBs = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      CAST(THIS-OBJECT:splitContainerVisa, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisa:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisa:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVisa:SuspendLayout().
      THIS-OBJECT:toolStripVisa:SuspendLayout().
      CAST(THIS-OBJECT:splitArbExcelKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitArbExcelKalk:Panel1:SuspendLayout().
      THIS-OBJECT:splitArbExcelKalk:Panel2:SuspendLayout().
      THIS-OBJECT:splitArbExcelKalk:SuspendLayout().
      THIS-OBJECT:VisaPanelWaiting:SuspendLayout().
      CAST(THIS-OBJECT:GridArbVisa, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:tBs, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerVisa */
      /*  */
      THIS-OBJECT:splitContainerVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisa:Name = "splitContainerVisa".
      THIS-OBJECT:splitContainerVisa:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerVisa.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisa:Panel1:Controls:Add(THIS-OBJECT:toolStripVisa).
      /*  */
      /* splitContainerVisa.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisa:Panel2:Controls:Add(THIS-OBJECT:splitArbExcelKalk).
      THIS-OBJECT:splitContainerVisa:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVisa:SplitterDistance = 25.
      THIS-OBJECT:splitContainerVisa:TabIndex = 4.
      /*  */
      /* toolStripVisa */
      /*  */
      THIS-OBJECT:toolStripVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripButtonGenerera.
      arrayvar0[2] = THIS-OBJECT:toolStripButtonExporteraExcel.
      arrayvar0[3] = THIS-OBJECT:toolStripButtonSkrivUt.
      arrayvar0[4] = THIS-OBJECT:toolStripButtonTillExcel.
      THIS-OBJECT:toolStripVisa:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripVisa:Name = "toolStripVisa".
      THIS-OBJECT:toolStripVisa:Size = NEW System.Drawing.Size(1111, 25).
      THIS-OBJECT:toolStripVisa:TabIndex = 0.
      THIS-OBJECT:toolStripVisa:Text = "toolStrip1".
      /*  */
      /* toolStripButtonGenerera */
      /*  */
      THIS-OBJECT:toolStripButtonGenerera:Image = CAST(resources:GetObject("toolStripButtonGenerera.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonGenerera:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonGenerera:Name = "toolStripButtonGenerera".
      THIS-OBJECT:toolStripButtonGenerera:Size = NEW System.Drawing.Size(107, 22).
      THIS-OBJECT:toolStripButtonGenerera:Text = "Generera kalkyl".
      THIS-OBJECT:toolStripButtonGenerera:Click:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      /*  */
      /* toolStripButtonExporteraExcel */
      /*  */
      THIS-OBJECT:toolStripButtonExporteraExcel:Image = CAST(resources:GetObject("toolStripButtonExporteraExcel.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonExporteraExcel:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonExporteraExcel:Name = "toolStripButtonExporteraExcel".
      THIS-OBJECT:toolStripButtonExporteraExcel:Size = NEW System.Drawing.Size(95, 22).
      THIS-OBJECT:toolStripButtonExporteraExcel:Text = "Spara excelfil".
      THIS-OBJECT:toolStripButtonExporteraExcel:Click:Subscribe(THIS-OBJECT:SparaExcelE).
      /*  */
      /* toolStripButtonSkrivUt */
      /*  */
      THIS-OBJECT:toolStripButtonSkrivUt:Image = CAST(resources:GetObject("toolStripButtonSkrivUt.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonSkrivUt:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonSkrivUt:Name = "toolStripButtonSkrivUt".
      THIS-OBJECT:toolStripButtonSkrivUt:Size = NEW System.Drawing.Size(66, 22).
      THIS-OBJECT:toolStripButtonSkrivUt:Text = "Skriv ut".
      THIS-OBJECT:toolStripButtonSkrivUt:Click:Subscribe(THIS-OBJECT:SkrivUtE).
      /*  */
      /* toolStripButtonTillExcel */
      /*  */
      THIS-OBJECT:toolStripButtonTillExcel:Image = CAST(resources:GetObject("toolStripButtonTillExcel.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonTillExcel:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonTillExcel:Name = "toolStripButtonTillExcel".
      THIS-OBJECT:toolStripButtonTillExcel:Size = NEW System.Drawing.Size(167, 22).
      THIS-OBJECT:toolStripButtonTillExcel:Text = "Exportera kalkylen till Excel".
      THIS-OBJECT:toolStripButtonTillExcel:Click:Subscribe(THIS-OBJECT:toolStripButtonTillExcel_Click).
      /*  */
      /* splitArbExcelKalk */
      /*  */
      THIS-OBJECT:splitArbExcelKalk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitArbExcelKalk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitArbExcelKalk:Name = "splitArbExcelKalk".
      /*  */
      /* splitArbExcelKalk.Panel1 */
      /*  */
      THIS-OBJECT:splitArbExcelKalk:Panel1:Controls:Add(THIS-OBJECT:VisaPanelEmpty).
      THIS-OBJECT:splitArbExcelKalk:Panel1:Controls:Add(THIS-OBJECT:VisaPanelWaiting).
      THIS-OBJECT:splitArbExcelKalk:Panel1:Controls:Add(THIS-OBJECT:VisaExcel).
      /*  */
      /* splitArbExcelKalk.Panel2 */
      /*  */
      THIS-OBJECT:splitArbExcelKalk:Panel2:Controls:Add(THIS-OBJECT:GridArbVisa).
      THIS-OBJECT:splitArbExcelKalk:Size = NEW System.Drawing.Size(1111, 517).
      THIS-OBJECT:splitArbExcelKalk:SplitterDistance = 500.
      THIS-OBJECT:splitArbExcelKalk:TabIndex = 0.
      /*  */
      /* VisaPanelEmpty */
      /*  */
      THIS-OBJECT:VisaPanelEmpty:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaPanelEmpty:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPanelEmpty:Name = "VisaPanelEmpty".
      THIS-OBJECT:VisaPanelEmpty:Size = NEW System.Drawing.Size(500, 517).
      THIS-OBJECT:VisaPanelEmpty:TabIndex = 2.
      /*  */
      /* VisaPanelWaiting */
      /*  */
      THIS-OBJECT:VisaPanelWaiting:BackColor = System.Drawing.SystemColors:Info.
      THIS-OBJECT:VisaPanelWaiting:Controls:Add(THIS-OBJECT:vantapakalkyl).
      THIS-OBJECT:VisaPanelWaiting:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPanelWaiting:Name = "VisaPanelWaiting".
      THIS-OBJECT:VisaPanelWaiting:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:VisaPanelWaiting:TabIndex = 1.
      /*  */
      /* vantapakalkyl */
      /*  */
      THIS-OBJECT:vantapakalkyl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:vantapakalkyl:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Italic, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:vantapakalkyl:Image = CAST(resources:GetObject("vantapakalkyl.Image"), System.Drawing.Image).
      THIS-OBJECT:vantapakalkyl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:vantapakalkyl:Name = "vantapakalkyl".
      THIS-OBJECT:vantapakalkyl:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:vantapakalkyl:TabIndex = 0.
      THIS-OBJECT:vantapakalkyl:Text = "Genererar kalkyl, var god vänta..".
      THIS-OBJECT:vantapakalkyl:TextAlign = System.Drawing.ContentAlignment:MiddleCenter.
      THIS-OBJECT:vantapakalkyl:UseCompatibleTextRendering = TRUE.
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:direkticell = FALSE.
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(522, 330).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      THIS-OBJECT:VisaExcel:UseMsInterop = FALSE.
      /*  */
      /* GridArbVisa */
      /*  */
      THIS-OBJECT:GridArbVisa:DataSource = THIS-OBJECT:tBs.
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Appearance = appearance1.
      ultraGridColumn1:Header:VisiblePosition = 2.
      ultraGridColumn9:Header:VisiblePosition = 8.
      ultraGridColumn10:Header:VisiblePosition = 9.
      ultraGridColumn11:Header:VisiblePosition = 7.
      ultraGridColumn125:Header:VisiblePosition = 4.
      ultraGridColumn126:Header:VisiblePosition = 5.
      ultraGridColumn127:Header:VisiblePosition = 11.
      ultraGridColumn128:Header:VisiblePosition = 3.
      ultraGridColumn129:Header:VisiblePosition = 6.
      ultraGridColumn130:Header:VisiblePosition = 10.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 12 NO-UNDO.
      arrayvar1[1] = ultraGridColumn7.
      arrayvar1[2] = ultraGridColumn8.
      arrayvar1[3] = ultraGridColumn1.
      arrayvar1[4] = ultraGridColumn9.
      arrayvar1[5] = ultraGridColumn10.
      arrayvar1[6] = ultraGridColumn11.
      arrayvar1[7] = ultraGridColumn125.
      arrayvar1[8] = ultraGridColumn126.
      arrayvar1[9] = ultraGridColumn127.
      arrayvar1[10] = ultraGridColumn128.
      arrayvar1[11] = ultraGridColumn129.
      arrayvar1[12] = ultraGridColumn130.
      ultraGridBand1:Columns:AddRange(arrayvar1).
      ultraGridGroup1:Key = "Klicka på +".
      ultraGridGroup1:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar2[1] = ultraGridGroup1.
      ultraGridBand1:Groups:AddRange(arrayvar2).
      ultraGridColumn2:Header:VisiblePosition = 2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Object EXTENT 3 NO-UNDO.
      arrayvar3[1] = ultraGridColumn131.
      arrayvar3[2] = ultraGridColumn132.
      arrayvar3[3] = ultraGridColumn2.
      ultraGridBand2:Columns:AddRange(arrayvar3).
      ultraGridGroup2:Header:Caption = "Kalkyl huvud".
      ultraGridGroup2:Key = "Kalkyil huvud".
      ultraGridGroup2:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar4[1] = ultraGridGroup2.
      ultraGridBand2:Groups:AddRange(arrayvar4).
      ultraGridColumn133:Header:VisiblePosition = 13.
      ultraGridColumn134:Header:VisiblePosition = 14.
      ultraGridColumn135:Header:VisiblePosition = 15.
      ultraGridColumn136:Header:VisiblePosition = 16.
      ultraGridColumn137:Header:VisiblePosition = 1.
      ultraGridColumn138:Header:VisiblePosition = 2.
      ultraGridColumn139:Header:VisiblePosition = 3.
      ultraGridColumn140:Header:VisiblePosition = 4.
      ultraGridColumn141:Header:VisiblePosition = 5.
      ultraGridColumn142:Header:VisiblePosition = 6.
      ultraGridColumn143:Header:VisiblePosition = 7.
      ultraGridColumn144:Header:VisiblePosition = 8.
      ultraGridColumn145:Header:VisiblePosition = 9.
      ultraGridColumn146:Header:VisiblePosition = 10.
      ultraGridColumn147:Header:VisiblePosition = 11.
      ultraGridColumn148:Header:VisiblePosition = 12.
      ultraGridColumn3:Header:VisiblePosition = 0.
      ultraGridColumn16:Header:Caption = "".
      ultraGridColumn16:Header:VisiblePosition = 17.
      ultraGridColumn17:Header:Caption = "".
      ultraGridColumn17:Header:VisiblePosition = 18.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Object EXTENT 19 NO-UNDO.
      arrayvar5[1] = ultraGridColumn133.
      arrayvar5[2] = ultraGridColumn134.
      arrayvar5[3] = ultraGridColumn135.
      arrayvar5[4] = ultraGridColumn136.
      arrayvar5[5] = ultraGridColumn137.
      arrayvar5[6] = ultraGridColumn138.
      arrayvar5[7] = ultraGridColumn139.
      arrayvar5[8] = ultraGridColumn140.
      arrayvar5[9] = ultraGridColumn141.
      arrayvar5[10] = ultraGridColumn142.
      arrayvar5[11] = ultraGridColumn143.
      arrayvar5[12] = ultraGridColumn144.
      arrayvar5[13] = ultraGridColumn145.
      arrayvar5[14] = ultraGridColumn146.
      arrayvar5[15] = ultraGridColumn147.
      arrayvar5[16] = ultraGridColumn148.
      arrayvar5[17] = ultraGridColumn3.
      arrayvar5[18] = ultraGridColumn16.
      arrayvar5[19] = ultraGridColumn17.
      ultraGridBand3:Columns:AddRange(arrayvar5).
      ultraGridColumn4:Header:VisiblePosition = 3.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar6 AS System.Object EXTENT 4 NO-UNDO.
      arrayvar6[1] = ultraGridColumn149.
      arrayvar6[2] = ultraGridColumn150.
      arrayvar6[3] = ultraGridColumn151.
      arrayvar6[4] = ultraGridColumn4.
      ultraGridBand4:Columns:AddRange(arrayvar6).
      ultraGridGroup3:Header:Caption = "Registrerade anmärkningar ".
      ultraGridGroup3:Key = "Registreade anmärkningar ".
      ultraGridGroup3:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar7[1] = ultraGridGroup3.
      ultraGridBand4:Groups:AddRange(arrayvar7).
      ultraGridColumn5:Header:VisiblePosition = 2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar8 AS System.Object EXTENT 3 NO-UNDO.
      arrayvar8[1] = ultraGridColumn152.
      arrayvar8[2] = ultraGridColumn153.
      arrayvar8[3] = ultraGridColumn5.
      ultraGridBand5:Columns:AddRange(arrayvar8).
      ultraGridGroup4:Header:Caption = "Egna Priser".
      ultraGridGroup4:Key = "Egna".
      ultraGridGroup4:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar9 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar9[1] = ultraGridGroup4.
      ultraGridBand5:Groups:AddRange(arrayvar9).
      ultraGridColumn6:Header:VisiblePosition = 2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar10 AS System.Object EXTENT 3 NO-UNDO.
      arrayvar10[1] = ultraGridColumn154.
      arrayvar10[2] = ultraGridColumn155.
      arrayvar10[3] = ultraGridColumn6.
      ultraGridBand6:Columns:AddRange(arrayvar10).
      ultraGridGroup5:Key = "Egna Faktorer".
      ultraGridGroup5:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar11 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar11[1] = ultraGridGroup5.
      ultraGridBand6:Groups:AddRange(arrayvar11).
      ultraGridColumn12:Header:VisiblePosition = 7.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar12 AS System.Object EXTENT 8 NO-UNDO.
      arrayvar12[1] = ultraGridColumn156.
      arrayvar12[2] = ultraGridColumn157.
      arrayvar12[3] = ultraGridColumn158.
      arrayvar12[4] = ultraGridColumn159.
      arrayvar12[5] = ultraGridColumn160.
      arrayvar12[6] = ultraGridColumn161.
      arrayvar12[7] = ultraGridColumn162.
      arrayvar12[8] = ultraGridColumn12.
      ultraGridBand7:Columns:AddRange(arrayvar12).
      ultraGridGroup6:Key = "Materiel Spec.".
      ultraGridGroup6:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar13 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar13[1] = ultraGridGroup6.
      ultraGridBand7:Groups:AddRange(arrayvar13).
      ultraGridColumn13:Header:VisiblePosition = 6.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar14 AS System.Object EXTENT 7 NO-UNDO.
      arrayvar14[1] = ultraGridColumn163.
      arrayvar14[2] = ultraGridColumn164.
      arrayvar14[3] = ultraGridColumn165.
      arrayvar14[4] = ultraGridColumn166.
      arrayvar14[5] = ultraGridColumn167.
      arrayvar14[6] = ultraGridColumn168.
      arrayvar14[7] = ultraGridColumn13.
      ultraGridBand8:Columns:AddRange(arrayvar14).
      ultraGridGroup7:Key = "Materiel hämtad från beredningen".
      ultraGridGroup7:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar15 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar15[1] = ultraGridGroup7.
      ultraGridBand8:Groups:AddRange(arrayvar15).
      ultraGridColumn14:Header:VisiblePosition = 5.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar16 AS System.Object EXTENT 6 NO-UNDO.
      arrayvar16[1] = ultraGridColumn169.
      arrayvar16[2] = ultraGridColumn170.
      arrayvar16[3] = ultraGridColumn171.
      arrayvar16[4] = ultraGridColumn172.
      arrayvar16[5] = ultraGridColumn173.
      arrayvar16[6] = ultraGridColumn14.
      ultraGridBand9:Columns:AddRange(arrayvar16).
      ultraGridGroup8:Header:Caption = "Frekvenskoder till nästa nivå".
      ultraGridGroup8:Key = "Frekvens kod till nästa nivå".
      ultraGridGroup8:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar17 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar17[1] = ultraGridGroup8.
      ultraGridBand9:Groups:AddRange(arrayvar17).
      ultraGridColumn15:Header:VisiblePosition = 2.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar18 AS System.Object EXTENT 3 NO-UNDO.
      arrayvar18[1] = ultraGridColumn174.
      arrayvar18[2] = ultraGridColumn175.
      arrayvar18[3] = ultraGridColumn15.
      ultraGridBand10:Columns:AddRange(arrayvar18).
      ultraGridGroup9:Key = "Kommentarer från vald katalog".
      ultraGridGroup9:RowLayoutGroupInfo:LabelSpan = 1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar19 AS Infragistics.Win.UltraWinGrid.UltraGridGroup EXTENT 1 NO-UNDO.
      arrayvar19[1] = ultraGridGroup9.
      ultraGridBand10:Groups:AddRange(arrayvar19).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand3).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand4).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand5).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand6).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand7).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand8).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand9).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BandsSerializer:Add(ultraGridBand10).
      THIS-OBJECT:GridArbVisa:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbVisa:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbVisa:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbVisa:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridArbVisa:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbVisa:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridArbVisa:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbVisa:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbVisa:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridArbVisa:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbVisa:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbVisa:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbVisa:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbVisa:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbVisa:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbVisa:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbVisa:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:GridArbVisa:GuruContext = ?.
      THIS-OBJECT:GridArbVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbVisa:Name = "GridArbVisa".
      THIS-OBJECT:GridArbVisa:Size = NEW System.Drawing.Size(607, 517).
      THIS-OBJECT:GridArbVisa:TabIndex = 0.
      THIS-OBJECT:GridArbVisa:Text = "gridJoin1".
      /*  */
      /* tBs */
      /*  */
      THIS-OBJECT:tBs:MaxDataGuess = 0.
      THIS-OBJECT:tBs:NoLOBs = FALSE.
      THIS-OBJECT:tBs:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar20 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar20 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc2:ChildTables = arrayvar20.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar21 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar21[1] = NEW Progress.Data.ColumnPropDesc("UPPGIFT", "", Progress.Data.DataType:CHARACTER).
      arrayvar21[2] = NEW Progress.Data.ColumnPropDesc("TEXTER", "", Progress.Data.DataType:CHARACTER).
      arrayvar21[3] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc2:Columns = arrayvar21.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar22 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar22 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc3:ChildTables = arrayvar22.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar23 AS Progress.Data.ColumnPropDesc EXTENT 17 NO-UNDO.
      arrayvar23[1] = NEW Progress.Data.ColumnPropDesc("ARBKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar23[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar23[3] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar23[5] = NEW Progress.Data.ColumnPropDesc("BTIMMAR", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[6] = NEW Progress.Data.ColumnPropDesc("MTIMMAR", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[7] = NEW Progress.Data.ColumnPropDesc("MASKTIMMAR", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[8] = NEW Progress.Data.ColumnPropDesc("UTIMMAR", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[9] = NEW Progress.Data.ColumnPropDesc("EA", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[10] = NEW Progress.Data.ColumnPropDesc("ARBKOST", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[11] = NEW Progress.Data.ColumnPropDesc("MASKKOST", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[12] = NEW Progress.Data.ColumnPropDesc("MTRLKOST", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[13] = NEW Progress.Data.ColumnPropDesc("OVRIGKOST", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[14] = NEW Progress.Data.ColumnPropDesc("UTRKOST", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[15] = NEW Progress.Data.ColumnPropDesc("STSUMMA", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[16] = NEW Progress.Data.ColumnPropDesc("RADSUMMA", "", Progress.Data.DataType:DECIMAL).
      arrayvar23[17] = NEW Progress.Data.ColumnPropDesc("TTRECID", "", Progress.Data.DataType:RECID).
      tableDesc3:Columns = arrayvar23.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar24 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar24 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc4:ChildTables = arrayvar24.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar25 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
      arrayvar25[1] = NEW Progress.Data.ColumnPropDesc("ARBKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar25[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar25[3] = NEW Progress.Data.ColumnPropDesc("ANMARKNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar25[4] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc4:Columns = arrayvar25.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar26 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar26 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc5:ChildTables = arrayvar26.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar27 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar27[1] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar27[2] = NEW Progress.Data.ColumnPropDesc("PRIS", "", Progress.Data.DataType:DECIMAL).
      arrayvar27[3] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc5:Columns = arrayvar27.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar28 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar28 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc6:ChildTables = arrayvar28.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar29 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar29[1] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar29[2] = NEW Progress.Data.ColumnPropDesc("FAKTOR", "", Progress.Data.DataType:DECIMAL).
      arrayvar29[3] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc6:Columns = arrayvar29.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar30 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar30 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc7:ChildTables = arrayvar30.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar31 AS Progress.Data.ColumnPropDesc EXTENT 8 NO-UNDO.
      arrayvar31[1] = NEW Progress.Data.ColumnPropDesc("ENR", "", Progress.Data.DataType:CHARACTER).
      arrayvar31[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar31[3] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:DECIMAL).
      arrayvar31[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar31[5] = NEW Progress.Data.ColumnPropDesc("MATRIS", "", Progress.Data.DataType:INTEGER).
      arrayvar31[6] = NEW Progress.Data.ColumnPropDesc("PRIS", "", Progress.Data.DataType:DECIMAL).
      arrayvar31[7] = NEW Progress.Data.ColumnPropDesc("SUMMA", "", Progress.Data.DataType:DECIMAL).
      arrayvar31[8] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc7:Columns = arrayvar31.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar32 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar32 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc8:ChildTables = arrayvar32.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar33 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
      arrayvar33[1] = NEW Progress.Data.ColumnPropDesc("ENR", "", Progress.Data.DataType:CHARACTER).
      arrayvar33[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar33[3] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:DECIMAL).
      arrayvar33[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar33[5] = NEW Progress.Data.ColumnPropDesc("PRIS", "", Progress.Data.DataType:DECIMAL).
      arrayvar33[6] = NEW Progress.Data.ColumnPropDesc("SUMMA", "", Progress.Data.DataType:DECIMAL).
      arrayvar33[7] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc8:Columns = arrayvar33.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar34 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar34 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc9:ChildTables = arrayvar34.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar35 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
      arrayvar35[1] = NEW Progress.Data.ColumnPropDesc("ARBKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar35[2] = NEW Progress.Data.ColumnPropDesc("FREKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar35[3] = NEW Progress.Data.ColumnPropDesc("ANTAL", "", Progress.Data.DataType:DECIMAL).
      arrayvar35[4] = NEW Progress.Data.ColumnPropDesc("ENHET", "", Progress.Data.DataType:CHARACTER).
      arrayvar35[5] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar35[6] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc9:Columns = arrayvar35.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar36 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar36 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc10:ChildTables = arrayvar36.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar37 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar37[1] = NEW Progress.Data.ColumnPropDesc("ARBKODLOPNR", "", Progress.Data.DataType:CHARACTER).
      arrayvar37[2] = NEW Progress.Data.ColumnPropDesc("KOMMENTAR", "", Progress.Data.DataType:CHARACTER).
      arrayvar37[3] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc10:Columns = arrayvar37.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar38 AS Progress.Data.TableDesc EXTENT 9 NO-UNDO.
      arrayvar38[1] = tableDesc2.
      arrayvar38[2] = tableDesc3.
      arrayvar38[3] = tableDesc4.
      arrayvar38[4] = tableDesc5.
      arrayvar38[5] = tableDesc6.
      arrayvar38[6] = tableDesc7.
      arrayvar38[7] = tableDesc8.
      arrayvar38[8] = tableDesc9.
      arrayvar38[9] = tableDesc10.
      tableDesc1:ChildTables = arrayvar38.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar39 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
      arrayvar39[1] = NEW Progress.Data.ColumnPropDesc("KALKNR", "", Progress.Data.DataType:INTEGER).
      arrayvar39[2] = NEW Progress.Data.ColumnPropDesc("BENAMNING", "", Progress.Data.DataType:CHARACTER).
      arrayvar39[3] = NEW Progress.Data.ColumnPropDesc("TYPSNITT", "", Progress.Data.DataType:INTEGER).
      tableDesc1:Columns = arrayvar39.
      THIS-OBJECT:tBs:TableSchema = tableDesc1.
      /*  */
      /* KalkVisaControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerVisa).
      THIS-OBJECT:Name = "KalkVisaControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1111, 546).
      THIS-OBJECT:splitContainerVisa:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisa:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerVisa:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisa, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:PerformLayout().
      THIS-OBJECT:splitArbExcelKalk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitArbExcelKalk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitArbExcelKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitArbExcelKalk:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelWaiting:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbVisa, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:tBs, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE path  AS CHARACTER NO-UNDO. 
      
      IF THIS-OBJECT:VisaExcel:Visible EQ TRUE THEN DO:
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         IF res:ToString()  EQ "Yes" THEN DO:
             
            THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).     
            THIS-OBJECT:GenereraArbKalkylVisning().   
         END.
      END.
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  11 feb 2015 10:23:15 
          VisningKalkIni skapar excel
          GenereraKalkylVisning skapar visning
         */
       
         THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
         THIS-OBJECT:GenereraArbKalkylVisning().
         
      END.
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
   END METHOD.
   METHOD PUBLIC VOID PostRound():
       DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
       DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
    
      queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("STSUMMA"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("STSUMMA"):BUFFER-VALUE,0).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE,0).
         qH:GET-NEXT().  
      END.
   
   END METHOD.
   METHOD PUBLIC VOID CreateGridArbVisa():
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      
      THIS-OBJECT:PostRound().
      IF GridArbVisaSkap = TRUE THEN RETURN.
      GridArbVisaSkap = TRUE. 
      
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[1] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[2] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[3] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[4] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[5] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[6] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[7] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[8] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[9] = NEW Controls.GridRubrikListaJoin().
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[10] = NEW Controls.GridRubrikListaJoin().
      
      /*start band*/     
     
    
      iband = 0.
     
      rubrikergrid[1] = NEW Controls.GridRubrik("KALKNR","Kalkyl",TRUE,50).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:ArbKalkTTh).
      iband = iband + 1.
      /*kalk huv*/
      rubrikergrid[1] = NEW Controls.GridRubrik("UPPGIFT","Kalkyl info",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("TEXTER","Text",TRUE,250).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2,INPUT THIS-OBJECT:ArbKalkhuvTTh).
      THIS-OBJECT:GridArbVisa:GuruGridRadOption(TRUE).
     
      CellFont = NEW System.Drawing.Font("Arial Black", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).   
                                             /*styrfält , värde ,   färg ,                          font, alla col, lista col  0  bgcolor 1 = fg color 2 font*/
                                                                                                                          /*  ska avslutas med,*/ 
      THIS-OBJECT:GridArbVisa:GuruAddRadOption("TYPSNITT", "1", Guru.StaticMethods:DotNetColor(12),CellFont,TRUE,"",2,0).
    
       
      iband = iband + 1.
      /*koder*/
      irubrik = 0.
      queryvar = "FOR EACH " + THIS-OBJECT:ArbRubrikTTh:TABLE. 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbRubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         irubrik = irubrik + 1.
         rubrikergrid[irubrik] = NEW Controls.GridRubrik(THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE,TRUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE).
         qH:GET-NEXT().  
      END.
     
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,125).
 /*
      irubrik = irubrik + 1. 
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SID","",TRUE,125).     
      irubrik = irubrik + 1. 
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("BYGGNR","",TRUE,125).     
      irubrik = irubrik + 1. 
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("BERNUM","",TRUE,125). 
      irubrik = irubrik + 1. 
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("ORDNING","",TRUE,125). 
      irubrik = irubrik + 1. 
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SUBORDNING","",TRUE,125).     
   */    
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik, INPUT THIS-OBJECT:ArbKalkKoderTTh).
     
      THIS-OBJECT:SkapaGrupper().
     
      iband = iband + 1.
      /*anmärkning*/
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKODLOPNR","Kod",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANMARKNING","Anmärkning",TRUE,350).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:ArbKalkAnmTTh).
      iband = iband + 1.
      /*egna priser*/
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("PRIS", "Pris", TRUE ,100,"DEC", "KALKYL").
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:ArbEgnaPriserTTh).
      iband = iband + 1.
      /*egna faktorer*/
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("FAKTOR", "Faktor", TRUE ,100,"DEC", "KALKYL").
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:ArbFaktorerTTh).
      iband = iband + 1.
       /* mtrl*/    
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning", TRUE ,300).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANTAL", "Antal", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh", TRUE ,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("MATRIS",THIS-OBJECT:ControlShell:GetMatrisName(),TRUE,50,"INT","INT").
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS", "Pris", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[7] = NEW Controls.GridRubrik("SUMMA", "Summa", TRUE ,100,"DEC", "KALKYL").
      
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 7, INPUT THIS-OBJECT:ArbMtrlTTh).
      
      matrisiband = iband.
      
      iband = iband + 1.
      /*mtrl ber*/
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning", TRUE ,300).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANTAL", "Antal", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh", TRUE ,50).
      rubrikergrid[5] = NEW Controls.GridRubrik("PRIS", "Pris", TRUE ,100,"DEC", "KALKYL").
      rubrikergrid[6] = NEW Controls.GridRubrik("SUMMA", "Summa", TRUE ,100,"DEC", "KALKYL").
      
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ArbBerMtrlTTh).
      iband = iband + 1.
      /*frekvens*/
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKODLOPNR","Kod",TRUE,55).
      rubrikergrid[2] = NEW Controls.GridRubrik("FREKODLOPNR","FrekvensKod",TRUE,55).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANTAL","Frekvens",TRUE,100,"DEC", "KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,55).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,300).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ArbFrekTTh).
      iband = iband + 1.
      /*kom*/
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKODLOPNR","Kod",TRUE,75).
      rubrikergrid[2] = NEW Controls.GridRubrik("KOMMENTAR","Kommentarer",TRUE,350).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:ArbKomTTh).
      iband = iband + 1.
      ArbKalkDSh = DATASET ArbKalkDS:HANDLE.
      
      THIS-OBJECT:GridArbVisa:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband,DATASET-HANDLE ArbKalkDSh).
      THIS-OBJECT:GridArbVisa:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridArbVisa:Text = "Visa Kalkyl". 
     
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.

   METHOD PUBLIC VOID MatrisGom(INPUT gomma AS LOGICAL):
      THIS-OBJECT:GridArbVisa:GuruHidden(matrisiband + 1,"MATRIS", gomma).
   END METHOD.
   METHOD PUBLIC VOID ArbKalkTTSkap():
      ordernrvar = ordernrvar + 1.
      THIS-OBJECT:ArbKalkTTh:BUFFER-CREATE().
      THIS-OBJECT:ArbKalkTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      THIS-OBJECT:ArbKalkTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbKalkTTh:RECID.
      
   END METHOD.
   METHOD PUBLIC VOID ArbKalkhuvTTSkap():
      ordernrvar = ordernrvar + 1.
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-CREATE(). 
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordernrvar.
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbKalkhuvTTh:RECID.
     
   END METHOD.
   METHOD PUBLIC VOID ArbRubrikTTSkap():
      ordernrvar = ordernrvar + 1.
      THIS-OBJECT:ArbRubrikTTh:BUFFER-CREATE().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordernrvar.
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbRubrikTTh:RECID.
   END METHOD.
   METHOD PUBLIC VOID ArbKalkKoderTidTThSkap(kalknrvar AS INTEGER):
      THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-CREATE().
      THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTidTTh:RECID.
   END METHOD.
   METHOD PUBLIC VOID ArbKalkKoderKostTTSkap(kalknrvar AS INTEGER):
      THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-CREATE().
      THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:RECID.
   END METHOD.
   METHOD PUBLIC VOID ArbKalkKoderTThSkap(kalknrvar AS INTEGER):
      ordernrvar = ordernrvar + 1.
      THIS-OBJECT:ArbKalkKoderTTh:BUFFER-CREATE().
      THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = kalknrvar.
      THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordernrvar.
      THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = 1.
      THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:RECID.
   END METHOD.
   METHOD PUBLIC VOID ArbBilagTThSkap(inTTh AS HANDLE):
      ordernrvar = ordernrvar + 1.
      inTTh:BUFFER-CREATE().
      inTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordernrvar.
      inTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = 1.
      inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      inTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = inTTh:RECID.
   END METHOD.
   METHOD PRIVATE VOID ArbGruppKoder():
      
      IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
         THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Egna Koder".
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN".
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = 0.
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
         
      END.   
      ELSE DO:
         THIS-OBJECT:KalkdbControl:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'") NO-ERROR.
         THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE.
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + " " + THIS-OBJECT:KalkdbControl:ArbetskoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = 0.
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
         
      END.
   END METHOD. 
   
   METHOD PRIVATE VOID ArbHuv():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      DEFINE VARIABLE koppladbest AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE helptext AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisHuvud") = "yes" THEN  DO:
         THIS-OBJECT:KalkdbControl:KatalogTTh:FIND-FIRST("WHERE kalkylkatalogtt.KLOGID = " + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK) NO-ERROR.
         Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(THIS-OBJECT:ControlShell:Omradevar) + "'") NO-ERROR.
         Guru.Konstanter:PersonalTTh:FIND-FIRST("WHERE PERSONALKOD = '" + THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'") NO-ERROR.
         
         THIS-OBJECT:ArbKalkhuvTTSkap().
         THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Katalog".
         THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KatalogTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         IF THIS-OBJECT:ControlShell:vismulti = TRUE THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:HuvudTTh:TABLE. 
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:HuvudTTh,queryvar).
            qH:GET-FIRST().
            helptext = Guru.GlobalaVariabler:KalkArendeText + "-nr".
            DO WHILE qH:QUERY-OFF-END = FALSE:
               THIS-OBJECT:ArbKalkhuvTTSkap().
               koppladtill = THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonrMulti(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
               THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = helptext.
               THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " " + koppladtill. 
               IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
                  THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkkopplabuffh:FIND-FIRST("WHERE  KALKNR = " + STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)) NO-ERROR.
                  
                  IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkkopplabuffh:AVAILABLE THEN                         
                  THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE  + " Ber nr" + STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkkopplabuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE). 
               END.
               
               helptext = "".
               qH:GET-NEXT().  
            END.   
            THIS-OBJECT:KalkdbControl:HuvudTTh:FIND-FIRST("WHERE  ") NO-ERROR.
         END.
         ELSE DO:   
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:PlnrAonr(OUTPUT koppladtill,OUTPUT koppladbest).
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = Guru.GlobalaVariabler:KalkArendeText + "-nr".
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = STRING(THIS-OBJECT:ControlShell:KalkNrvar) + " " + koppladtill. 
            IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkkopplabuffh:FIND-FIRST("WHERE  ") NO-ERROR.
               THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE  + " Ber nr" + STRING(THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkkopplabuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE). 
            END.
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = Guru.Konstanter:gbestk.
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = koppladbest.
         END.
         IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN. 
         ELSE DO:
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE  + " " +
            THIS-OBJECT:ControlShell:GetMatrisName() + " " + STRING(THIS-OBJECT:ControlShell:VisMatris()).
         END. 
         
         THIS-OBJECT:ArbKalkhuvTTSkap().
         THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Kalkylsumma".
         IF THIS-OBJECT:ControlShell:vismulti = TRUE THEN DO:
         END.
         ELSE DO: 
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Kalkyltyp".
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ControlShell:RibbonToolInfTyp:Text.
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = Guru.Konstanter:gomrl.
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ControlShell:Omradevar + " " + Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE.   
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Kalkylansv.".
            IF Guru.Konstanter:PersonalTTh:AVAILABLE THEN DO:  
               THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = Guru.Konstanter:PersonalTTh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + Guru.Konstanter:PersonalTTh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
            END.
         END.   
      END.
   END METHOD.
   METHOD PRIVATE VOID ArbAnm():
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE helptext AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmarkning") = "yes" THEN DO:
         IF THIS-OBJECT:ControlShell:vismulti = TRUE THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:HuvudTTh:TABLE. 
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:HuvudTTh,queryvar).
            qH:GET-FIRST().
            helptext = "Anmärkning".
            DO WHILE qH:QUERY-OFF-END = FALSE:
               IF THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE NE "" THEN DO: 
                  THIS-OBJECT:ArbKalkhuvTTSkap().
                  THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = helptext.
                  THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = STRING(THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE). 
                  helptext = "".
               END.   
               qH:GET-NEXT().  
            END.   
            THIS-OBJECT:KalkdbControl:HuvudTTh:FIND-FIRST("WHERE  ") NO-ERROR.
         END.
         ELSE DO:
            THIS-OBJECT:ArbKalkhuvTTSkap().
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Anmärkning".
            THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:HuvudTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
         END.   
      END.
   END METHOD.
   METHOD PUBLIC VOID ArbNoll():
     ASSIGN 
     antalvar = 0
     btimmarvar = 0 
     mtimmarvar = 0
     masktimmarvar = 0
     utimmarvar = 0
     eavar = 0
     arbkostvar = 0 
     maskkostvar = 0
     mtrlkostvar = 0
     ukostvar = 0
     okostvar = 0
     radsummavar = 0.
   END METHOD.
   METHOD PUBLIC CHARACTER ArbKalkKoderWhere(inTTh AS HANDLE):
     DEFINE VARIABLE whererqueryvar AS CHARACTER NO-UNDO.
     IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND BERNUM = "  + STRING(inTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE) +
        " AND SID = "  + STRING(inTTh:BUFFER-FIELD("SID"):BUFFER-VALUE) + " AND NUM = " + STRING(inTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
        IF THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE THEN DO :
           IF inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
              whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
           END.
           ELSE DO:    
              whererqueryvar = " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
           END.  
        END.
        
        RETURN whererqueryvar.
     END.        
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "no" THEN DO :
        whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND MATRIS = " + 
        STRING(inTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + " AND NUM = " + STRING(inTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
     END.
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes" THEN DO :
        IF inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
           whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND MATRIS = " + STRING(inTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) +  
           " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
        END.
        ELSE DO:   
           whererqueryvar = " AND MATRIS = " + STRING(inTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) +  " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE +
           "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
        END.   
     END.   
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "no" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes" THEN DO :
        IF inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
           whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
        END.
        ELSE DO:    
           whererqueryvar = " AND ARBKOD = '" + inTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "' AND LOPNR = " + STRING(inTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
        END.  
     END.   
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "no" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "no"
     THEN DO :
        whererqueryvar = " AND KALKNR = " + STRING(inTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND NUM = "  + STRING(inTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
     END.
     RETURN whererqueryvar.
  END METHOD.
     
  METHOD PUBLIC VOID ArbKalkKoderBreakBy(OUTPUT breakbyqueryvar AS CHARACTER, OUTPUT antalbreakby  AS INTEGER):
     IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        breakbyqueryvar = " BREAK BY KALKNR BY SID BY BERNUM BY NUM ".
        antalbreakby = 4. 
        IF THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE THEN DO :
           breakbyqueryvar = " BREAK BY KALKNR BY ARBKOD BY LOPNR".
           antalbreakby = 3.
        END.   
        RETURN. 
     END.
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "no"
     THEN DO :
        breakbyqueryvar = " BREAK BY KALKNR BY MATRIS BY NUM ".
        antalbreakby = 3. 
     END.
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes"
     THEN DO :
        breakbyqueryvar = " BREAK BY KALKNR BY MATRIS BY ARBKOD BY LOPNR".
        antalbreakby = 4.
     END.   
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "no" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "yes"
     THEN DO :
        breakbyqueryvar = " BREAK BY KALKNR BY ARBKOD BY LOPNR".
        antalbreakby = 3.
     END.   
     ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "no" AND 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisSummera") = "no"
     THEN DO :
        breakbyqueryvar = " BREAK BY KALKNR BY NUM".
        antalbreakby = 2.
     END.
     
  END METHOD.
  METHOD PUBLIC VOID MtrlBreakBy(OUTPUT breakbyqueryvar AS CHARACTER, OUTPUT antalbreakby  AS INTEGER):
     
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
        breakbyqueryvar = "BREAK BY MATRIS BY ENR BY LEVKOD". 
        antalbreakby = 3.
     END.
     ELSE IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN DO:
        breakbyqueryvar = "BREAK BY ENR BY LEVKOD".
        antalbreakby = 2.
     END.   
     ELSE DO:
        breakbyqueryvar = "BREAK BY ENR BY LEVKOD".
        antalbreakby = 2.
     END.
     
  END METHOD.
  METHOD PUBLIC VOID ArbKalkSumV():
     IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN.
     ELSE RETURN.
     btimmarvar     =  btimmarvar    + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE    . 
     mtimmarvar     =  mtimmarvar    + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE    . 
     masktimmarvar  =  masktimmarvar + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE . 
     utimmarvar     =  utimmarvar    + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE    . 
     eavar          =  eavar         + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE         . 
     arbkostvar     =  arbkostvar    + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE    . 
     maskkostvar    =  maskkostvar   + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE   . 
     mtrlkostvar    =  mtrlkostvar   + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE   . 
     ukostvar       =  ukostvar      + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE    . 
     okostvar       =  okostvar      + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE  . 
     radsummavar    =  radsummavar   + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE   .
  END METHOD.
  
  METHOD PUBLIC VOID ArbKalkSumTot():
     
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE =  btimmarvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE = mtimmarvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE = masktimmarvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE = utimmarvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = eavar.   
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE = arbkostvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE = maskkostvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = mtrlkostvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE = ukostvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE = okostvar.
     THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE  = radsummavar.
     
  END METHOD.
  METHOD PUBLIC VOID ArbMtrlSum():
     DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
     DEFINE VARIABLE breakbyqueryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE antalbreakby AS INTEGER NO-UNDO.
     DEFINE VARIABLE bermtrlvar AS DECIMAL NO-UNDO.
     DEFINE VARIABLE matrisvar AS INTEGER NO-UNDO.
     IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN RETURN.
     
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
     THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2"
     THEN DO:
        /*EGET MATRIEL SUMMA matris*/
        IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
           THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-FIRST("WHERE  ") NO-ERROR.
           REPEAT:
              IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
                 matrisvar = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE.  
              END.
              ELSE LEAVE.   
              THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
              IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:  
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-LAST("WHERE  MATRIS = " + STRING(matrisvar) + " USE-INDEX ORDNING") NO-ERROR.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Materielkostnad från eget materiel".
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE.
              END.
              THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-FIRST("WHERE  MATRIS > " + STRING(matrisvar)) NO-ERROR.
           END.
           
        END.
        ELSE DO:
           
           THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-LAST("WHERE  ") NO-ERROR.
           IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
              THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
              IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:  
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Materielkostnad från eget materiel".
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:MtrlKost(?).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE.
              END.    
           END.
           
        END.
        
     END.    
       /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
     /*Berednings mtrl*/   
     IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
        THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-LAST("WHERE  ") NO-ERROR.
        IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
           THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
           IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:  
              THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Materielkostnad från beredning".
              THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlHmt(OUTPUT bermtrlvar).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE =  bermtrlvar.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = bermtrlvar.
           END.    
        END.
     END.
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3" THEN DO:
        THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-LAST("WHERE  ") NO-ERROR.
        IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
           THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST('WHERE KOSTTIMM = TRUE  AND BENAMNING = "Materiel"') NO-ERROR.
           IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:  
              THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Materielkostnad från beredning".
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerMtrlPris().
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE.
           END.    
        END.
        
     END.   
  END METHOD.
   
  METHOD PUBLIC VOID ArbKalkSum():
     DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
     DEFINE VARIABLE queryvar2          AS CHARACTER NO-UNDO.
     DEFINE VARIABLE qH2                AS HANDLE    NO-UNDO.
     DEFINE VARIABLE breakbyqueryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE antalbreakby AS INTEGER NO-UNDO.
     DEFINE VARIABLE whererqueryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE matrisvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE sordvar AS INTEGER NO-UNDO.
     ordernrvar = 0.
     THIS-OBJECT:ArbNoll().
  
     IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Allt" THEN queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTidTTh:TABLE. 
        IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Beredning" THEN DO:
           queryvar = " WHERE ArbKalkKoderTidTT.SID = 0 ".
           queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTidTTh:TABLE + queryvar. 
        END.   
        IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Schakt" THEN DO:
           queryvar = " WHERE ArbKalkKoderTidTT.BERNUM = 0 ".
           queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTidTTh:TABLE + queryvar.
        END.
     END.
     ELSE DO:
        IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTidTTh:TABLE. 
        ELSE queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTidTTh:TABLE + " WHERE ArbKalkKoderTidTT.MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).
     END.         
     THIS-OBJECT:ArbKalkKoderBreakBy(OUTPUT breakbyqueryvar, OUTPUT antalbreakby).      
     
     
     
     /*Summerar timmar*/
     
     queryvar = queryvar + breakbyqueryvar + " BY TIMTYP".
     
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderTidTTh,queryvar).
     qH:GET-FIRST().  
     REPEAT:
        IF THIS-OBJECT:ArbKalkKoderTidTTh:AVAILABLE THEN.
        ELSE LEAVE .
        antalvar = antalvar + THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.   
       
        IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Beredare" THEN DO:
           btimmarvar = btimmarvar + THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Montör" THEN DO:
           mtimmarvar = mtimmarvar + THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Maskin" THEN DO:
           masktimmarvar = masktimmarvar + THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Utrustning" THEN DO:
           utimmarvar = utimmarvar + THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE.
        END.
       
        IF qH:LAST-OF(antalbreakby + 1) = TRUE THEN DO:
           whererqueryvar = "WHERE " + SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTidTTh),5).
           
           THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST(whererqueryvar) NO-ERROR.
           IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN.
           ELSE DO:
              THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-COPY(THIS-OBJECT:ArbKalkKoderTidTTh).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99"). 
           END.
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE + antalvar.   
           IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Beredare" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE + btimmarvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Montör" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE + mtimmarvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Maskin" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE + masktimmarvar.
           END.
           
           IF THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = "Utrustning" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTIMMAR"):BUFFER-VALUE + utimmarvar.
              
           END.
           THIS-OBJECT:ArbNoll().
        END.
        qH:GET-NEXT().
        IF THIS-OBJECT:ArbKalkKoderTidTTh:AVAILABLE THEN.
        ELSE LEAVE.
     END.   
     IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderKostTTh:TABLE. 
     ELSE queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderKostTTh:TABLE + " WHERE ArbKalkKoderKostTT.MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).      
     THIS-OBJECT:ArbKalkKoderBreakBy(OUTPUT breakbyqueryvar, OUTPUT antalbreakby).      
     /*summerar kostnad*/
     queryvar = queryvar + breakbyqueryvar + " BY KOSTTYP".
     
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderKostTTh,queryvar).
     qH:GET-FIRST().  
     REPEAT:
        IF THIS-OBJECT:ArbKalkKoderKostTTh:AVAILABLE THEN.
        ELSE LEAVE.
        eavar = eavar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("EA"):BUFFER-VALUE.   
        IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Arbete" THEN DO:
           arbkostvar = arbkostvar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Maskin" THEN DO:
           maskkostvar = maskkostvar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Materiel" THEN DO:
           mtrlkostvar = mtrlkostvar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Utrustning" THEN DO:
           ukostvar = ukostvar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE.
        END.
        IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Övrigt" THEN DO:
           okostvar = okostvar + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE.
        END.
        IF qH:LAST-OF(antalbreakby + 1) = TRUE THEN DO:
           whererqueryvar = "WHERE " + SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderKostTTh),5).
          
           THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST(whererqueryvar) NO-ERROR.
           IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN.
           ELSE DO:
              THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-COPY(THIS-OBJECT:ArbKalkKoderKostTTh).
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = 0.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99").
           END.
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("EA"):BUFFER-VALUE + eavar.   
           IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Arbete" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE + arbkostvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Maskin" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE + maskkostvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Materiel" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE + mtrlkostvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Utrustning" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE + ukostvar.
           END.
           IF THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = "Övrigt" THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE + okostvar.
           END.
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE =
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOST"):BUFFER-VALUE + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE +
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("UTRKOST"):BUFFER-VALUE + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("OVRIGKOST"):BUFFER-VALUE.
           THIS-OBJECT:ArbNoll().
        END.
        qH:GET-NEXT().
        IF THIS-OBJECT:ArbKalkKoderKostTTh:AVAILABLE THEN.
        ELSE LEAVE.
     END.
     ordernrvar = 1.
     queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE.
     IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE.
     ELSE queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE + " WHERE MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).       
     
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
        queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
     END.
     ELSE DO:
        queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
     END.
     IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
        IF THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE THEN queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE + " BY ARBKOD BY LOPNR".
        ELSE DO:
           IF Guru.Konstanter:varforetypval[32] = 1 THEN DO:
              queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:Bervallbuffh:TABLE. 
              qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:Bervallbuffh,queryvar2).
              qH:GET-FIRST().
              DO WHILE qH:QUERY-OFF-END = FALSE:
                 queryvar2 = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE + " WHERE BERNUM = " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE). 
                 qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderTTh,queryvar2).
                 qH2:GET-FIRST().
                 DO WHILE qH2:QUERY-OFF-END = FALSE:
                    THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE.
                    qH2:GET-NEXT().
                 END.
                 qH:GET-NEXT().
              END.
           END.   
           queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". 
        END.     
     END.
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderTTh,queryvar).
     qH:GET-FIRST().
     
     DO WHILE qH:QUERY-OFF-END = FALSE:
        
        IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
           THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE KALKNR = " + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + 
           " AND  ARBKOD = '" +  THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + 
           "' AND LOPNR = " +  THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) NO-ERROR.
          
           IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE =  THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE =  THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
             
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE =  THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99").
           END.
        END.
        ELSE DO:
           IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
              IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Allt" THEN whererqueryvar =  "WHERE " + SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh),5).
              IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Beredning" THEN DO:
                 whererqueryvar =  "WHERE SID = 0 " + THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh). 
              END.   
              IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Schakt" THEN DO:
                 whererqueryvar =  "WHERE BERNUM = 0 " + THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh). 
              END.
           END.
           ELSE DO:
              IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN whererqueryvar =  "WHERE " + SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh),5).
              ELSE whererqueryvar = " WHERE MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()) + " " + THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh).
           END.
           queryvar2 = "FOR EACH " + THIS-OBJECT:KalkdbControl:kodantaltempTTh:TABLE + " " +  whererqueryvar.
           qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kodantaltempTTh,queryvar2).
           qH2:GET-FIRST().
           DO WHILE qH2:QUERY-OFF-END = FALSE:
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE =  THIS-OBJECT:KalkdbControl:kodantaltempTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE =  THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:kodantaltempTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
              
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE =  THIS-OBJECT:KalkdbControl:kodantaltempTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
              THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99").
              qH2:GET-NEXT().
           END.
           
        END.      
        THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordernrvar.
        IF THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE THEN DO:
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = 0.
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = 0.
        END.
        ordernrvar = ordernrvar + 1.
        qH:GET-NEXT().
     END.
     
     /*nu finns timmar och kostnad på rätt rad för varje kod/matris mm*/
     /*mtrl*/
      
     THIS-OBJECT:ArbMtrlSum().
     /* mtrl*/
     /* TOTAL SUMMA per rad */
     THIS-OBJECT:ArbNoll().
     queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE. 
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(ArbKalkKoderTTh,queryvar).
     qH:GET-FIRST().  
     REPEAT:
        THIS-OBJECT:ArbKalkSumV().
        qH:GET-NEXT().
        IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN.
        ELSE DO:
           THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
           THIS-OBJECT:ArbKalkSumTot().
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Totalt".
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
           THIS-OBJECT:ArbKalkhuvTTh:FIND-FIRST("WHERE UPPGIFT = 'Kalkylsumma'") NO-ERROR.
           IF THIS-OBJECT:ArbKalkhuvTTh:AVAILABLE THEN DO:
              THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = STRING(ROUND(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE,0)).
              THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
              
           END.
           THIS-OBJECT:ArbNoll().
           LEAVE.
        END.    
     END.
    /*MATRIS SUMMA */
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
        queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTbuffh:TABLE + " WHERE MATRIS > 0 AND MED = FALSE". 
        breakbyqueryvar = " BREAK BY MATRIS".
        antalbreakby = 1.
        queryvar = queryvar + breakbyqueryvar.
       
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(ArbKalkKoderTTbuffh,queryvar).
        qH:GET-FIRST().
        
        IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
           REPEAT:
              THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST("WHERE TTRECID = " +  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE) NO-ERROR.
              THIS-OBJECT:ArbKalkSumV().
              IF qH:FIRST-OF(antalbreakby) = TRUE THEN DO:
                 matrisvar = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-FIRST("WHERE MATRIS = " + STRING(matrisvar) + " USE-INDEX ORDNING")  NO-ERROR.
                 
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MED"):BUFFER-VALUE = TRUE. 
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = matrisvar.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE - 1.
               
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:ControlShell:GetMatrisName() + " " +  STRING(matrisvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
                 
              END.   
              IF qH:LAST-OF(antalbreakby) = TRUE THEN DO:
                 matrisvar = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-LAST("WHERE MATRIS = " + STRING(matrisvar) + " USE-INDEX ORDNING")  NO-ERROR.
                 
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MED"):BUFFER-VALUE = TRUE. 
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = matrisvar.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
               
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Summa " + THIS-OBJECT:ControlShell:GetMatrisName() + " " +  STRING(matrisvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
                 THIS-OBJECT:ArbKalkSumTot().
                 THIS-OBJECT:ArbNoll().
              END. 
              qH:GET-NEXT().
              IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN.
              ELSE LEAVE.  
           END.
        END.   
     END.
     /*GRUPPERA KODER*/
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppera") = "yes" THEN DO:
        queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE.
        IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
           breakbyqueryvar = " BREAK BY MATRIS BY ARBKOD".
           antalbreakby = 2.
        END.
        ELSE DO:
           breakbyqueryvar = " BREAK BY ARBKOD".
           antalbreakby = 1.
        END.      
        queryvar = queryvar + breakbyqueryvar.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(ArbKalkKoderTTh,queryvar).
        qH:GET-FIRST().  
        REPEAT:
           IF qH:FIRST-OF(antalbreakby) = TRUE THEN DO:
              IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE NE "" THEN DO: 
                 IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppera") = "yes" THEN DO:
                    THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-FIRST("WHERE MATRIS = " + STRING(THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE) + " AND ARBKOD = '" + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'")  NO-ERROR.
                 END.
                 ELSE DO:
                    THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-FIRST("WHERE ARBKOD = '" + THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'")  NO-ERROR.
                 END.   
                 THIS-OBJECT:ArbGruppKoder().
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = 0.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE.
              END.      
           END. 
           qH:GET-NEXT().
           IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN.
           ELSE LEAVE.  
        END.
        
     END. 
    
     /*SUMMA KONSTRUKTION*/
     DEFINE VARIABLE asid AS INTEGER NO-UNDO.
     DEFINE VARIABLE abernum AS INTEGER NO-UNDO.
     IF THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE THEN.
     ELSE IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTbuffh:TABLE + " WHERE MATRIS > 0 AND MED = FALSE". 
        breakbyqueryvar = " BREAK BY SID BY BERNUM".  /* BREAK BY KALKNR BY SID BY BERNUM KK*/
        antalbreakby = 2.
        queryvar = queryvar + breakbyqueryvar.
        qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(ArbKalkKoderTTbuffh,queryvar).
        qH:GET-FIRST().
        
        IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
           REPEAT:
              THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST("WHERE TTRECID = " +  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE) NO-ERROR.
              THIS-OBJECT:ArbKalkSumV().
              IF qH:FIRST-OF(antalbreakby) = TRUE THEN DO:
                 asid     = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE.
                 abernum = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.
                 IF asid NE 0 THEN THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-FIRST("WHERE SID = " + STRING(asid) + " USE-INDEX ORDNING")  NO-ERROR.
                 IF abernum NE 0 THEN THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-FIRST("WHERE BERNUM = " + STRING(abernum) + " USE-INDEX ORDNING")  NO-ERROR.
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MED"):BUFFER-VALUE = TRUE. 
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = asid.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = abernum.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE - 1.
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MED"):BUFFER-VALUE = TRUE. 
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = asid.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = abernum.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE - 1.
                 IF asid NE 0 THEN DO:
                    /*HITTA VIA KALKNR KK*/
                    THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE SID = " + STRING(asid),NO-LOCK) NO-ERROR.
                    IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN DO:
                       THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = " " + STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE) + " " + 
                       STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                    END.
                 END.
                 IF abernum NE 0 THEN DO:
                    THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE NUM = " + STRING(abernum) ,NO-LOCK) NO-ERROR.
                    IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
                       /*HITTA VIA KALKNR KK*/
                       THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = " " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE) + " " + 
                       STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE).
                    END.
                 END.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
                  
              END.
              IF qH:LAST-OF(antalbreakby) = TRUE THEN DO:
                 asid     = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE.
                 abernum = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.
                 IF asid NE 0 THEN THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-LAST("WHERE SID = " + STRING(asid) + " USE-INDEX ORDNING")  NO-ERROR.
                 IF abernum NE 0 THEN THIS-OBJECT:ArbKalkKoderTTbuff2h:FIND-LAST("WHERE BERNUM = " + STRING(abernum) + " USE-INDEX ORDNING")  NO-ERROR.
                 THIS-OBJECT:ArbKalkKoderTThSkap(THIS-OBJECT:ControlShell:KalkNrvar).
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MED"):BUFFER-VALUE = TRUE. 
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = asid.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = abernum.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("BYGGNR"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuff2h:BUFFER-FIELD("SUBORDNING"):BUFFER-VALUE + 1.
                 IF asid NE 0 THEN DO:
                    /*HITTA VIA KALKNR KK*/
                    THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-FIRST("WHERE SID = " + STRING(asid),NO-LOCK) NO-ERROR.
                    IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN DO:
                       THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Summa " + STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE) + " " + 
                       STRING(THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
                    END.
                 END.
                 IF abernum NE 0 THEN DO:
                    THIS-OBJECT:KalkdbControl:Bervallbuffh:FIND-FIRST("WHERE NUM = " + STRING(abernum) ,NO-LOCK) NO-ERROR.
                    IF THIS-OBJECT:KalkdbControl:Bervallbuffh:AVAILABLE THEN DO:
                       /*HITTA VIA KALKNR KK*/
                       THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Summa " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE) + " " + 
                       STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkdbControl:Bervallbuffh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE).
                    END.
                 END.
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("TYPSNITT"):BUFFER-VALUE = 1.
                 THIS-OBJECT:ArbKalkSumTot().
                 THIS-OBJECT:ArbNoll().
              END. 
              qH:GET-NEXT().
              IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN.
              ELSE LEAVE.  
           END.
        END.   
     END.   
     /*SUMMA KONSTRUKTION*/
     /*SLUT SUMMA per kolumn*/
     THIS-OBJECT:ArbKalkKoderTTh:FIND-LAST("WHERE BENAMNING = 'Totalt'") NO-ERROR.
     IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN DO:
        THIS-OBJECT:ArbKalkKoderTTbuffh:FIND-LAST("WHERE USE-INDEX ORDNING") NO-ERROR.
        IF THIS-OBJECT:ArbKalkKoderTTbuffh:AVAILABLE THEN DO:
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Totalt".
           THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE + 1.
           THIS-OBJECT:KalkdbControl:HdSchaktbuffh:FIND-LAST("WHERE ",NO-LOCK) NO-ERROR.
           IF VALID-HANDLE(THIS-OBJECT:KalkdbControl:HdSchaktbuffh) THEN DO:
              IF THIS-OBJECT:KalkdbControl:HdSchaktbuffh:AVAILABLE THEN DO:
                 THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:HdSchaktbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE + 1.
              END.
              ELSE THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = 1.      
           END.
        END.   
     END.

  END METHOD.
   
   
   METHOD PUBLIC VOID GenereraArbKalkylVisning():

      THIS-OBJECT:splitArbExcelKalk:Panel1Collapsed = TRUE.
      THIS-OBJECT:splitArbExcelKalk:Panel2Collapsed = FALSE.
      THIS-OBJECT:ControlShell:ArbetsKalkylGom(?,?).
      DEFINE VARIABLE marksubordning AS INTEGER NO-UNDO.
      DEFINE VARIABLE totbermtrl        AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE findfel           AS LOGICAL   NO-UNDO.
      
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH             AS HANDLE    NO-UNDO.
      DEFINE VARIABLE whererqueryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE link              AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rubrikrad         AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE radefterrubrikrad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE matrisrader       AS CHARACTER NO-UNDO.   
      DEFINE VARIABLE styckpris         AS DECIMAL NO-UNDO.
      DEFINE VARIABLE kostnadstartcol   AS CHARACTER NO-UNDO.      
      
      DEFINE VARIABLE sustcol AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vsumma AS DECIMAL NO-UNDO.
      DEFINE VARIABLE hsumma AS DECIMAL NO-UNDO.
      THIS-OBJECT:ControlShell:cSenasteArbKod     = "".
      THIS-OBJECT:ControlShell:iSenasteLopNr      = 0.
       /*BERÄKNA KALYLEN*/
       
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
      /*DESSA FINNS BARA I DENNA CLASS*/
     
      
      THIS-OBJECT:ArbKalkhuvTTh = TEMP-TABLE ArbKalkhuvTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbRubrikTTh = TEMP-TABLE ArbRubrikTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKalkKoderTidTTh = TEMP-TABLE ArbKalkKoderTidTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKalkKoderKostTTh = TEMP-TABLE ArbKalkKoderKostTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKalkKoderTTh = TEMP-TABLE ArbKalkKoderTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      CREATE BUFFER ArbKalkKoderTTbuffh FOR TABLE ArbKalkKoderTTh:NAME.
      CREATE BUFFER ArbKalkKoderTTbuff2h FOR TABLE ArbKalkKoderTTh:NAME.
      THIS-OBJECT:ArbKalkAnmTTh = TEMP-TABLE ArbKalkAnmTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbEgnaPriserTTh = TEMP-TABLE ArbEgnaPriserTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbFaktorerTTh = TEMP-TABLE ArbFaktorerTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbMtrlTTh = TEMP-TABLE ArbMtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbBerMtrlTTh = TEMP-TABLE ArbBerMtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbFrekTTh = TEMP-TABLE ArbFrekTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKomTTh = TEMP-TABLE ArbKomTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKalkTTh = TEMP-TABLE ArbKalkTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ArbKalkhuvTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbRubrikTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbKalkKoderTidTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbKalkKoderKostTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbKalkKoderTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbKalkAnmTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbEgnaPriserTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbFaktorerTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbMtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbBerMtrlTTh:EMPTY-TEMP-TABLE() NO-ERROR.    
      THIS-OBJECT:ArbFrekTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:ArbKomTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:ArbKalkTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:ArbKalkTTSkap().
      
      /*INTE GRUPPERA OM BARA EN MATRIS*/
      
      IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN.
      ELSE DO:
         THIS-OBJECT:ControlShell:RibbonToolVisGroupMatris:CHECKED = FALSE.
      END.   
     
      ordernrvar = 0. 
      {LOGGORc.I}
      
      Guru.Konstanter:JurAvdTTh:FIND-FIRST("WHERE AVDELNINGNR = " + Guru.Konstanter:OmradeTTh:BUFFER-FIELD("AVDELNINGNR"):BUFFER-VALUE) NO-ERROR.
      IF Guru.Konstanter:JurAvdTTh:AVAILABLE THEN DO:
         Guru.Konstanter:JurPersTTh:FIND-FIRST("WHERE JUDID = '" + STRING(Guru.Konstanter:JurAvdTTh:BUFFER-FIELD("JUDID"):BUFFER-VALUE) + "'") NO-ERROR.         
      END.
      
      IF Guru.Konstanter:JurPersTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ArbKalkhuvTTSkap().
         THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Företag".
         THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE =                              
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + CHR(10) +
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("GATUADR"):BUFFER-VALUE) + "  " + CHR(10) +
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTNR"):BUFFER-VALUE,"xxx xx") + " " + 
         STRING(Guru.Konstanter:JurPersTTh:BUFFER-FIELD("POSTADR"):BUFFER-VALUE) + CHR(10) + 
         "Telefon:" + Guru.Konstanter:JurPersTTh:BUFFER-FIELD("TELVXL"):BUFFER-VALUE + CHR(10).
         
      END. 
      THIS-OBJECT:ArbKalkhuvTTSkap().
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("UPPGIFT"):BUFFER-VALUE = "Kalkyl".
      THIS-OBJECT:ArbKalkhuvTTh:BUFFER-FIELD("TEXTER"):BUFFER-VALUE = THIS-OBJECT:ControlShell:RibbonToolVisVisa:Text.
      
      /* Skapa huvud */
      THIS-OBJECT:ArbHuv().    
      /*ANMÄRKNING*/
      THIS-OBJECT:ArbAnm().
      /* --- Börja göra listan här --- */
      /* Skapa rubrikerna */
      ordernrvar = 0. 
      
     
      
      THIS-OBJECT:ArbRubrikTTSkap().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Kod".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "ARBKODLOPNR".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 75.
      THIS-OBJECT:ArbRubrikTTSkap().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Benämning".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "BENAMNING".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 250.
    
      THIS-OBJECT:ArbRubrikTTSkap().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Antal".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "ANTAL".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 75.
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE = "DEC".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE = "KALKYL".
      THIS-OBJECT:ArbRubrikTTSkap().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Enh".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "ENHET".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 50.
      
      /*LÄGG UT RUBRIKER FÖR TIMMAR*/
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = FALSE NO-LOCK BY ORDNING". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ArbRubrikTTSkap().
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Beredare" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "BTIMMAR".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Mont" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "MTIMMAR".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Maskin" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "MASKTIMMAR".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Utrustning" THEN DO:
             THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "UTIMMAR".
         END.   
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 70.
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE = "DEC".
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE = "DEC0".
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("TIMMCOL"):BUFFER-VALUE = STRING(ordernrvar).      
         qH:GET-NEXT().
      END.
        
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      /*LÄGG UT RUBRIKER FÖR KOSTNAD*/
      queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkrubrikTTh:TABLE + " WHERE KOSTTIMM = TRUE NO-LOCK BY ORDNING".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkrubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:ArbRubrikTTSkap().
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "EA" THEN DO:
            THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "EA".
            THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE = 70.
         END.   
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Arbete" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "ARBKOST".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Maskin" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "MASKKOST".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Mate" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "MTRLKOST".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Övrig" THEN THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "OVRIGKOST".
         IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE BEGINS "Utrustning" THEN DO:
            THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "UTRKOST".
         END.   
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE = "DEC".
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE = "DEC0".
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = STRING(ordernrvar).
         qH:GET-NEXT(). 
      END.
      DEFINE VARIABLE rubrikergridadd     AS Controls.GridRubrik      NO-UNDO EXTENT 2. 
      
       /*ny koll om utrustning*/
      
      THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST("WHERE BENAMNING BEGINS 'Utrustning' ") NO-ERROR.
      IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
         IF GridArbVisaSkap = TRUE AND utrustrubrik = FALSE THEN DO:
            THIS-OBJECT:ArbRubrikTTh:FIND-FIRST("WHERE FALT = 'UTIMMAR' ") NO-ERROR.
            IF THIS-OBJECT:ArbRubrikTTh:AVAILABLE THEN DO:
               rubrikergridadd[1] = NEW Controls.GridRubrik(THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE,TRUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE).
               rubrikergridadd[1]:Ordning = 3.
               THIS-OBJECT:GridArbVisa:GuruAddColumn(2,rubrikergridadd[1]).
               
            END.
            THIS-OBJECT:ArbRubrikTTh:FIND-FIRST("WHERE FALT = 'UTRKOST' ") NO-ERROR.
            IF THIS-OBJECT:ArbRubrikTTh:AVAILABLE THEN DO:
               rubrikergridadd[2] = NEW Controls.GridRubrik(THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE,TRUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("BREDD"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE,THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE).
               rubrikergridadd[2]:Ordning = 5.
               THIS-OBJECT:GridArbVisa:GuruAddColumn(2,rubrikergridadd[2]).
               
            END.
         END.   
         utrustrubrik = TRUE.
      END.  
      
      THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-CREATE().
      THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = STRING(ordernrvar).
      THIS-OBJECT:ArbRubrikTTSkap().
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Summa".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "RADSUMMA".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE = "DEC".
      THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE = "DEC0".
      IF Guru.Konstanter:varforetypchar[8] = "1"  THEN DO:     
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-CREATE().
         THIS-OBJECT:KalkdbControl:kalkrubrikTTh:BUFFER-FIELD("KOSTCOL"):BUFFER-VALUE = STRING(ordernrvar).
         THIS-OBJECT:ArbRubrikTTSkap().
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE = "Summa/st".  
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("FALT"):BUFFER-VALUE = "STSUMMA".
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("TYPVAR"):BUFFER-VALUE = "DEC".
         THIS-OBJECT:ArbRubrikTTh:BUFFER-FIELD("DECFORMAT"):BUFFER-VALUE = "DEC0". 
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:Utsmetkost().
      END.   
      /*OM MATRIS*/
     
      IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE. 
      ELSE queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " WHERE kalkantal.MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).      
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR". 
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
        
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Allt" THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + " NO-LOCK  BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Beredning" THEN DO:
            queryvar = " WHERE kalkantal.SID = 0 ".
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + queryvar + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         END.   
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Schakt" THEN DO:
            queryvar = " WHERE kalkantal.BERNUM = 0 ".
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalkantalTTh:TABLE + queryvar + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         END.
      END.        
      
      /*timmar*/ 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalkantalTTh,queryvar).    
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         /*
         THIS-OBJECT:ControlShell:KalkNrvar = THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
         */
         IF THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.
         ELSE DO:
            THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = FALSE  AND BENAMNING = '" + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArbKalkKoderTidTTh:FIND-FIRST("WHERE TIMTYP = '" + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE + "'" + 
               THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:KalkdbControl:kalkantalTTh)) NO-ERROR.
               IF THIS-OBJECT:ArbKalkKoderTidTTh:AVAILABLE THEN. 
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderTidTThSkap(THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:kalkantalTTh).
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0.
               END.
               IF INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 1 OR 
               INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 4 OR 
               INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE = 
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE.                 
               END.
               ELSE DO:   
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE = 
                  THIS-OBJECT:ArbKalkKoderTidTTh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:kalkantalTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE.                                
               END.
              
            END.
              
            ArbKalkKoderTidTTh:BUFFER-RELEASE().
         END.
         qH:GET-NEXT().
      END.
      
      /*kostnad*/
      IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE. 
      ELSE queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " WHERE kalkkostnad.MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).      
      
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO :
         queryvar = queryvar + " NO-LOCK BY MATRIS BY ARBKOD BY LOPNR".
      END.
      ELSE DO:
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
      END.
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Allt" THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Beredning" THEN DO:
            queryvar = " WHERE kalkkostnad.SID = 0 ".
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + queryvar + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         END.   
         IF THIS-OBJECT:ControlShell:RibbonToolVisBerSch:Text = "Schakt" THEN DO:
            queryvar = " WHERE kalkkostnad.BERNUM = 0 ".
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:TABLE + queryvar + " NO-LOCK BY BERNUM BY SID BY ARBKOD BY LOPNR". /*BREAK BY KALKNR KK*/
         END.
         
         
      END.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KalkkostnadTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.
         ELSE DO:
            THIS-OBJECT:KalkdbControl:kalkrubrikTTh:FIND-FIRST("WHERE KOSTTIMM = TRUE  AND BENAMNING = '" + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'") NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:kalkrubrikTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArbKalkKoderKostTTh:FIND-FIRST("WHERE KOSTTYP = '" + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE + "'" + 
               THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:KalkdbControl:KalkkostnadTTh)) NO-ERROR.
               IF THIS-OBJECT:ArbKalkKoderKostTTh:AVAILABLE THEN.     
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderKostTTSkap(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KalkkostnadTTh).
               END.
               IF INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.                  
               END.
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
               END.
               THIS-OBJECT:ArbKalkKoderKostTTh:FIND-FIRST("WHERE  KOSTTYP = 'EA'" + THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:KalkdbControl:KalkkostnadTTh)) NO-ERROR.
               IF THIS-OBJECT:ArbKalkKoderKostTTh:AVAILABLE THEN.
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderKostTTSkap(THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KalkkostnadTTh).
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = 'EA'.
               END.   
               IF INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 1 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 4 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("EA"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("FRIEAMANGD"):BUFFER-VALUE.
               END.
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("EA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("EA"):BUFFER-VALUE + THIS-OBJECT:KalkdbControl:KalkkostnadTTh:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE.                  
               END.
              
            END.
            ArbKalkKoderKostTTh:BUFFER-RELEASE() NO-ERROR.
         END. 
         qH:GET-NEXT().
      END.
      /*risk vinst måste ske innan koderna slås ihop*/
      queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderKostTTh:TABLE + " NO-LOCK". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderKostTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE KALKNR = " + STRING(ArbKalkKoderKostTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE) + " AND NUM = " + THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE) NO-ERROR.
         /*RISKALKYL EV KOLL OM FAKTOR*/
         IF INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 2 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 4  THEN DO:          
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE * (1 + (THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("RISK"):BUFFER-VALUE) / 100).
            END.   
         END.   
         /*VINSTKALKYL*/
         IF INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 3 OR INTEGER(THIS-OBJECT:ControlShell:RibbonToolVisVisa:SelectedIndex) = 5 THEN DO:          
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE = 0 THEN.
            ELSE DO:
               THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderKostTTh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE * (1 + (THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("VINST"):BUFFER-VALUE) / 100).
            END.   
         END.
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:ArbKalkSum().
      EMPTY TEMP-TABLE EgenExtraTT NO-ERROR. 
      queryvar = "FOR EACH " + THIS-OBJECT:ArbKalkKoderTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ArbKalkKoderTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         /*Anders Olsson Elpool i Umeå AB  14 jan 2019 16:49:39 
         LADDAR ALLTID INTERN PÅ EGENKOD 
         */
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"MarkSubMark") = "yes" THEN DO:
            whererqueryvar = SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh),5).
           
            IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN.
            ELSE whererqueryvar = whererqueryvar + " AND MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()). 
            
            THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE  " + whererqueryvar) NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE = "" THEN THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
               /*
               IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = "" THEN THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE.
               */
             
               IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = "" THEN THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE.
               ELSE THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE + " " + THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE. 
            END.
         END.
         ELSE DO:
            IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
               whererqueryvar = SUBSTRING(THIS-OBJECT:ArbKalkKoderWhere(THIS-OBJECT:ArbKalkKoderTTh),5).
               THIS-OBJECT:KalkdbControl:KoderTTh:FIND-FIRST("WHERE  " + whererqueryvar) NO-ERROR.
               IF THIS-OBJECT:KalkdbControl:KoderTTh:AVAILABLE THEN DO:
                  IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = "" THEN THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE.
               END.    
            END.
         END.
         /*Anders Olsson Elpool i Umeå AB  15 jan 2019 11:51:34 
         För sorteringa av egna koder 
         */ 
         IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Egna Koder" THEN.
         ELSE IF THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN DO:
            IF marksubordning = 0 THEN marksubordning = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE. 
            CREATE EgenExtraTT.
            BUFFER EgenExtraTT:HANDLE:BUFFER-COPY(THIS-OBJECT:ArbKalkKoderTTh).
         END.    
            
         THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE =  THIS-OBJECT:ControlShell:KalkNrvar.
         qH:GET-NEXT().
      END.
      
      /*bilagOR*/
      IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE. 
      ELSE DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:KoderTTh:TABLE + " WHERE kalknumtt.MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).      
      END.
      /*bilaga anm kod*/    
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisAnmUtf") = "yes" THEN DO:
         THIS-OBJECT:ArbBilagor(7, queryvar).
      END.
      /*BILAGA EGNA PRISER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:
         THIS-OBJECT:ArbBilagor(1, queryvar).
      END.
      /*BILAGA EGNA FAKTORER*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO: 
         THIS-OBJECT:ArbBilagor(2, queryvar).       
      END.  
      /*BILAGA EGNet mtrl*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisMatSpec") = "yes" THEN DO:
         THIS-OBJECT:ArbBilagor(3, queryvar).      
      END.   
      /*BILAGA mtrl från ber*/
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BeredningMtrlFinns() = TRUE THEN DO:
         THIS-OBJECT:ArbBilagor(4, queryvar).      
      END.
      /*BILAGA frekvens*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFrekvens") = "yes" THEN DO:
         THIS-OBJECT:ArbBilagor(5, queryvar).      
      END.   
      /*BILAGA kommentarer*/
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisKommentar") = "yes" THEN DO:
         THIS-OBJECT:ArbBilagor(6, queryvar).      
      END.
     /*
      DATASET ArbKalkDS:WRITE-XML("FILE", "C:\TEMP\ARBKALK.xml").
      */
     Guru.Konstanter:EuropeanAmericanReset().
     
     /*Anders Olsson Elpool i Umeå AB  15 jan 2019 11:51:34 
         För sorteringa av egna koder 
         */ 
      DEFINE VARIABLE matrisnr AS INTEGER NO-UNDO.   
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
         FOR EACH EgenExtraTT WHERE USE-INDEX MATRIS NO-LOCK:
            IF matrisnr NE  EgenExtraTT.MATRIS THEN DO:
               
               THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST("WHERE MATRIS = " + STRING(EgenExtraTT.MATRIS) + " AND ARBKOD = 'EGEN' AND BENAMNING NE 'Egna Koder'" + " USE-INDEX MATRISARB" ,NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN DO:
                   marksubordning = THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE. 
                   matrisnr = EgenExtraTT.MATRIS.
               END.
               ELSE NEXT.     
            END.   
            
            THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST("WHERE ARBKOD = 'EGEN' AND LOPNR = " + STRING(EgenExtraTT.LOPNR) ,NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = marksubordning.
            END.
            marksubordning = marksubordning + 1.
         END.
      END.
      ELSE DO:
         FOR EACH EgenExtraTT WHERE USE-INDEX MARKSUB NO-LOCK:
            THIS-OBJECT:ArbKalkKoderTTh:FIND-FIRST("WHERE ARBKOD = 'EGEN' AND LOPNR = " + STRING(EgenExtraTT.LOPNR) ,NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN DO:
               THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = marksubordning.
            END.
            marksubordning = marksubordning + 1.
         END.
      END.
   
      DEFINE VARIABLE rabattvar AS INTEGER NO-UNDO.
      
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"EndFakSum") = "yes" THEN DO:
         THIS-OBJECT:ArbKalkKoderTTh:FIND-LAST("WHERE BENAMNING = 'Totalt'") NO-ERROR.
         IF THIS-OBJECT:ArbKalkKoderTTh:AVAILABLE THEN DO:
            
            THIS-OBJECT:KalkdbControl:FaktorerTTh:FIND-FIRST("WHERE FAKTOR NE 1 ",NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:KalkdbControl:FaktorerTTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE = 1 THEN.
               ELSE DO:
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-CREATE().
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-COPY(THIS-OBJECT:ArbKalkKoderTTh).
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE + 1.
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = "%".
                  /*EV KOLL OM FAKTOR*/
                  IF THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE > 1 THEN DO:
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = (THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE * 100 - 100). 
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Tillägg + " + 
                     STRING(THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE) +
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.  
                  END.
                  /*EV KOLL OM FAKTOR*/
                  IF THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE < 1 THEN DO:
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = -1 * (100 - THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE * 100). 
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Rabatt  " + 
                     STRING(THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE) + 
                     THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ENHET"):BUFFER-VALUE. 
                  END.
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * THIS-OBJECT:ArbKalkKoderTTh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE / 100.
                  
                  rabattvar = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE .
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-CREATE().
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-COPY(THIS-OBJECT:ArbKalkKoderTTh).
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE + 2. 
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Slutsumma".
                  THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE = THIS-OBJECT:ArbKalkKoderTTbuffh:BUFFER-FIELD("RADSUMMA"):BUFFER-VALUE + rabattvar. 
               END.
            END.
         END.   
      END.
       
      EMPTY TEMP-TABLE EgenExtraTT NO-ERROR.
     
      THIS-OBJECT:CreateGridArbVisa().
      THIS-OBJECT:GridArbVisa:GridData(DATASET-HANDLE ArbKalkDSh).
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN THIS-OBJECT:MatrisGom(FALSE).
      ELSE IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN THIS-OBJECT:MatrisGom(TRUE).
      ELSE THIS-OBJECT:MatrisGom(FALSE).  
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"MarkSubMark") = "yes" THEN DO:
         THIS-OBJECT:GridArbVisa:GuruHidden(3,"MARKNING", TRUE).
         THIS-OBJECT:GridArbVisa:GuruHidden(3,"MARKSUB", FALSE).
      END.
      ELSE DO:   
         THIS-OBJECT:GridArbVisa:GuruHidden(3,"MARKNING", TRUE).
         THIS-OBJECT:GridArbVisa:GuruHidden(3,"MARKSUB", TRUE).
      END.  
      THIS-OBJECT:GridArbVisa:VisibleGroup(2,"Timmar", FALSE).
      THIS-OBJECT:GridArbVisa:VisibleGroup(2,"Kostnad", FALSE).
      
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"EndastSumma") = "yes" THEN DO:
         THIS-OBJECT:GridArbVisa:VisibleGroup(2,"Timmar", TRUE).
         THIS-OBJECT:GridArbVisa:VisibleGroup(2,"Kostnad", TRUE).
      END.
      
   END METHOD.
   
   METHOD PRIVATE VOID ArbBilagor(vad AS INTEGER, kalkylqueryvar AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE subqH    AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE breakbyqueryvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE antalbreakby AS INTEGER NO-UNDO.
      ordernrvar = 0.
      /*EGNA PRISER*/
      IF vad = 1 THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:EgnaPriserTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:EgnaPriserTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ArbBilagTThSkap(ArbEgnaPriserTTh).
            THIS-OBJECT:ArbEgnaPriserTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
            THIS-OBJECT:ArbEgnaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:EgnaPriserTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE.   
            subqH:GET-NEXT(NO-LOCK).
         END.       
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
      /*FAKTORER*/
      IF vad = 2 THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:KalkdbControl:FaktorerTTh:TABLE.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:FaktorerTTh,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ArbBilagTThSkap(ArbFaktorerTTh).
            THIS-OBJECT:ArbFaktorerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
            THIS-OBJECT:ArbFaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:FaktorerTTh:BUFFER-FIELD("FAKTOR"):BUFFER-VALUE.   
            subqH:GET-NEXT(NO-LOCK).
         END.       
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
     /*MTRL SPEC*/
      IF vad = 3 THEN DO:
         /*
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " BY MATRIS BY ENR". 
         END.
         ELSE IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " BY ENR".
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " WHERE MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()) + " BY ENR".
         */
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE. 
         END.
         ELSE IF THIS-OBJECT:ControlShell:VisMatris() = 0 THEN queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE.
         ELSE queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:TABLE + " WHERE MATRIS = " + STRING(THIS-OBJECT:ControlShell:VisMatris()).
         THIS-OBJECT:MtrlBreakBy(OUTPUT breakbyqueryvar, OUTPUT antalbreakby).
         queryvar = queryvar + " " + breakbyqueryvar.
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:kalktmtrlTTh,queryvar).
         subqH:GET-FIRST(). 
         antalvar = 0.
         REPEAT:
            IF THIS-OBJECT:KalkdbControl:kalktmtrlTTh:AVAILABLE THEN.
            ELSE LEAVE.
            antalvar = antalvar + THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("BERKVANT"):BUFFER-VALUE.
            IF subqH:LAST-OF(antalbreakby) = TRUE THEN DO:
               THIS-OBJECT:ArbBilagTThSkap(ArbMtrlTTh).
               THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE.
               THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE. 
               THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = antalvar.         
               THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.   
               IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisGruppMatris") = "yes" THEN     
               THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
                    
               IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
               ELSE DO:
                  THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE.
                  THIS-OBJECT:ArbMtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:kalktmtrlTTh:BUFFER-FIELD("NPRIS"):BUFFER-VALUE * antalvar.
                  
               END.
               antalvar = 0.
               
            END.      
            subqH:GET-NEXT(NO-LOCK).
            /*
            IF THIS-OBJECT:KalkdbControl:kalktmtrlTTh:AVAILABLE THEN.
            ELSE LEAVE.
            */
         END.       
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END. 
      /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
      /*MTRL BER*/
      IF vad = 4 THEN DO:
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:berkalkmtrlttH:TABLE + " BY ENR".
         subqH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:berkalkmtrlttH,queryvar).
         subqH:GET-FIRST().   
         DO WHILE subqH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ArbBilagTThSkap(ArbBerMtrlTTh).
            THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("ENR"):BUFFER-VALUE.
            THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE. 
            THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.         
            THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("ENHET"):BUFFER-VALUE.        
            IF Guru.konstanter:mtrlsekvar[6] = TRUE THEN.
            ELSE DO:
               THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("PRIS"):BUFFER-VALUE.
               THIS-OBJECT:ArbBerMtrlTTh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:berkalkmtrlttH:BUFFER-FIELD("TOTPRIS"):BUFFER-VALUE.            
            END. 
            subqH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(subqH).
      END.
     /*FREKVENS*/
      IF vad = 5 THEN DO:
         ASSIGN 
         THIS-OBJECT:ControlShell:cSenasteArbKod     = ""
         THIS-OBJECT:ControlShell:iSenasteLopNr      = 0.
         THIS-OBJECT:KalkdbControl:FrekvensTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:   
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN. 
            ELSE DO: 
               IF THIS-OBJECT:ControlShell:VisSummera(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
               THIS-OBJECT:Root:DatabaseManager:Kalkyl:FrekvensHmtAppend(INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE,
                                                                         INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE,
                                                                         INPUT THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
               END.
            END.
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
         queryvar = "FOR EACH " + THIS-OBJECT:KalkdbControl:FrekvensTTh:TABLE. 
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR BY FREKOD BY FREKNR ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:FrekvensTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            THIS-OBJECT:ArbBilagTThSkap(ArbFrekTTh).
            THIS-OBJECT:ArbFrekTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:FrekvensTTh).  
            THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99").
            THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("FREKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("FREKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbFrekTTh:BUFFER-FIELD("FREKNR"):BUFFER-VALUE,">>>99").
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      END.
      /*Kommentarer från vald Katalog*/
      IF vad = 6 THEN DO:
         ASSIGN 
         THIS-OBJECT:ControlShell:cSenasteArbKod     = ""
         THIS-OBJECT:ControlShell:iSenasteLopNr      = 0.
          queryvar = kalkylqueryvar.
         queryvar = queryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:    
            /*inga dubbletter*/
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = "EGEN" THEN.
            ELSE IF THIS-OBJECT:ControlShell:VisSummera(STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE),THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) = FALSE THEN DO:
               THIS-OBJECT:ArbBilagTThSkap(ArbKomTTh).
               THIS-OBJECT:ArbKomTTh:BUFFER-COPY(THIS-OBJECT:KalkdbControl:KoderTTh). 
               THIS-OBJECT:ArbKomTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKomTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKomTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99"). 
            END.
            qH:GET-NEXT(NO-LOCK).
         END.          
         THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      END.
      /*KOD ANM*/
      IF vad = 7 THEN DO:
         kalkylqueryvar = kalkylqueryvar + " NO-LOCK BY ARBKOD BY LOPNR".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkdbControl:KoderTTh,kalkylqueryvar).
         qH:GET-FIRST().
     
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0 THEN.           
            ELSE IF STRING(THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE) NE "" THEN DO:
               THIS-OBJECT:ArbBilagTThSkap(ArbKalkAnmTTh).
               THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE.
               THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE.
               THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
               THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = THIS-OBJECT:KalkdbControl:KoderTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
               THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("ARBKODLOPNR"):BUFFER-VALUE = THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + STRING(THIS-OBJECT:ArbKalkAnmTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE,">>>>99").
            END.  
            qH:GET-NEXT(NO-LOCK).
         END.          
         
        
      END.
     
   END METHOD.
   
   METHOD PUBLIC VOID SkapaGrupper():
     THIS-OBJECT:GridArbVisa:SkapaGrupp(2,"Kod info","").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kod info","ARBKODLOPNR").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kod info","BENAMNING").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kod info","ANTAL").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kod info","ENHET").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kod info","MARKSUB").
     
     THIS-OBJECT:GridArbVisa:SkapaGrupp(2,"Timmar","Timmar").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Timmar","BTIMMAR").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Timmar","MTIMMAR").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Timmar","MASKTIMMAR").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Timmar","UTIMMAR").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Timmar","EA").
     
     THIS-OBJECT:GridArbVisa:SkapaGrupp(2,"Kostnad","Kostnad").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","ARBKOST").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","MASKKOST").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","MTRLKOST").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","OVRIGKOST").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","UTRKOST").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","STSUMMA").
     
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","TTRECID").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Kostnad","MARKNING").
     
     THIS-OBJECT:GridArbVisa:SkapaGrupp(2,"Summa","").
     THIS-OBJECT:GridArbVisa:SetGroup(2,"Summa","RADSUMMA").
       
   END METHOD.   
    /* EXPRIMENT SLUT*/
    
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID SparaExcelE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:SaveFileWithDialog().
   END METHOD.
   

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonTillExcel_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE path AS CHARACTER.
	   THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
	   THIS-OBJECT:ControlShell:VisningKalkIni(OUTPUT path).
	   
      THIS-OBJECT:ControlShell:GenereraKalkylVisning().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC KalkVisaControl ( ):
    /*  {KALKYLCONTROLLDELETE.i}*/
	END DESTRUCTOR.

END CLASS.