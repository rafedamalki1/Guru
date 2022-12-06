
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
   	/*egna def*/ 
	{KALKYLKAT.I}
   DEFINE PRIVATE VARIABLE DECIMALTEST AS Controls.FillinDecimal NO-UNDO.
   DEFINE PRIVATE VARIABLE Decimaltest2 AS Controls.TextDecimal NO-UNDO.
	DEFINE VARIABLE kalkberh AS HANDLE NO-UNDO.
   DEFINE VARIABLE excelprog AS GuruStart.Excelfunc NO-UNDO.
   DEFINE VARIABLE filtervar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE decimalprog AS Controls.FillinDecimal NO-UNDO.
   
   DEFINE PRIVATE VARIABLE toolStrip1 AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer3 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TaBortEgen AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE SkapaNyEgen AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE VARIABLE toolz AS System.Windows.Forms.ToolTip NO-UNDO.
   DEFINE VARIABLE currdir AS CHARACTER NO-UNDO.
   DEFINE TEMP-TABLE typtt NO-UNDO
      FIELD ATAT AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC AS CHARACTER LABEL "Kalkyltyp".
   DEFINE VARIABLE frantab AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tilltab AS CHARACTER NO-UNDO.
   
   DEFINE PRIVATE VARIABLE cmbKata AS Controls.ComboSimple NO-UNDO.
   DEFINE PRIVATE VARIABLE cmbOmr AS Controls.Combo NO-UNDO.
   DEFINE PRIVATE VARIABLE cmbUtf AS Controls.Combo NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinCharAnsvarig AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelomr AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE personalkalkyl AS Controls.Grid NO-UNDO.
		
   DEFINE PRIVATE VARIABLE cmbTyp AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinCharKalkBen AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinIntegerKalknr AS Controls.FillinInteger NO-UNDO.
    
   DEFINE PRIVATE VARIABLE anm AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverAllaMallar AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackMarkMallar AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackAllaMallar AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE avtaltext AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverAllaPriser AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackMarkPriser AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackMarkMtrl AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackAllaPriser AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE btnBeredningKalkyl AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOvermark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverAlla AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackAlla AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE arbkoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverMarkMallar AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverMarkPriser AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverMarkMtrl AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE cmb_avtal AS Infragistics.Win.UltraWinGrid.UltraCombo NO-UNDO.
   DEFINE PRIVATE VARIABLE cmb_artalavtal AS Infragistics.Win.UltraWinGrid.UltraCombo NO-UNDO.
   DEFINE PRIVATE VARIABLE cmb_KLGartal AS Infragistics.Win.UltraWinGrid.UltraCombo NO-UNDO.
   DEFINE PRIVATE VARIABLE egnapriser AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE egnakoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE egnafaktorer AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE egetmtrl AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE frikalk AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE kalkylkoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE Kalkyltab AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelartalskj AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE LabelAvtal AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelklgartal AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelkatalog AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelanm AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE Labelutf AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE lopkoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkreg_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkreg_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkreg_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Kalkreg_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
      
   DEFINE PRIVATE VARIABLE Kalkreg_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PRIVATE VARIABLE KalkylToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   DEFINE PRIVATE VARIABLE mallarkoder AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE mallar AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE prisposter AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl12 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl11 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl10 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl17 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl16 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl8 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl7 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl6 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl5 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabPageControl9 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage2 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage3 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTextEditor3 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE valdamallar AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE valmtrl AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE valdaprisposter AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC PROPERTY kalknrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
  
   DEFINE PRIVATE VARIABLE VisaReset AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelSettings AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPDFBrowser AS System.Windows.Forms.WebBrowser NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaKalkylTyp AS Infragistics.Win.UltraWinEditors.UltraOptionSet NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklOFakt AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklOEP AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklKAnm AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklHuvud AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklGroupBox AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaInklAnm AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaIEBrowser AS System.Windows.Forms.WebBrowser NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaGroupBoxTyp AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaGenerate AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaFunkSum AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaFunkGrupp AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaExcelTab AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaExcel AS Controls.GuruExcelEmbed NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaDividerX AS System.Windows.Forms.SplitContainer NO-UNDO.
	CONSTRUCTOR PUBLIC Kalkreg (kalknrVar AS INTEGER,omradeVar AS CHARACTER  ):
	   SUPER().
      InitializeComponent().
      THIS-OBJECT:kalknrvar = kalknrVar.
      THIS-OBJECT:omradevar = omradeVar. 
      allstart ().
      hamtatabstart( ).       
      Kalkyltab:Tabs["Kalkyl"]:Selected = TRUE.
      frantab = Kalkyltab:ActiveTab:TEXT.      
      mainblock ().  
      
      CATCH e AS Progress.Lang.Error:
         
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   
   CONSTRUCTOR PUBLIC Kalkreg ( ):
          
      SUPER().
      InitializeComponent().
      allstart (). 
                 
      IF GuruStart.Konstanter:varforetypchar[3] = "" THEN  
      RUN startny_UI IN kalkberh (INPUT GuruStart.Konstanter:globomr,INPUT GuruStart.Konstanter:globanv,OUTPUT TABLE kalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
      ELSE 
      RUN startny_UI IN kalkberh (INPUT GuruStart.Konstanter:varforetypchar[3],INPUT GuruStart.Konstanter:globanv,OUTPUT TABLE kalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
       
      FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
      ASSIGN 
      kalkhuvtt.ANVANDARE = GuruStart.Konstanter:globanv
      kalkhuvtt.KALKANV = GuruStart.Konstanter:globanvpkod
      Kalkyltab:Tabs["Kalkylhuvud"]:Selected = TRUE      
      frantab = Kalkyltab:ActiveTab:TEXT. 
      
      mainblock ().
      fillinIntegerKalknr:VISIBLE = FALSE.                                    
      CATCH e AS Progress.Lang.Error:         
         UNDO, THROW e.
      END CATCH.
END CONSTRUCTOR.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonBackAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
      bortkalkposteralla().      
      buttonBackAlla:GuruOver().
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      bortkalkpriseralla().
      buttonBackAllaPriser:GuruOver().
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		bortkalkpostermark().
      buttonBackMark:GuruOver().
		RETURN.

	END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      bortkalkprisermark().
      buttonBackMarkPriser:GuruOver().
      egnakoder:GuruReopen().  
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		buttonOverAlla:GuruOver().
      skapkalkposter().
		RETURN.

	END METHOD.


   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      buttonOverAllaPriser:GuruOver().
      skapegnaposter().
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOvermark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   buttonOvermark:GuruOver().
		skapkalkposter().
		RETURN.

	END METHOD.

	

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      buttonOverMarkPriser:GuruOver().
      skapegnaposter().
      RETURN.

   END METHOD.



	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID cmbOmr_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID cmbOmr_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF GuruStart.Konstanter:varforetypchar[3] = "" THEN DO: 
   		RUN omradekoll_UI IN kalkberh (INPUT STRING(cmbOmr:VALUE), OUTPUT kalkhuvtt.KALKNR).
         IF kalkhuvtt.KALKNR = ? THEN DO:
            MESSAGE GuruStart.Konstanter:gomrl " är inte valbart!"
            VIEW-AS ALERT-BOX.
         END.   
         fillinIntegerKalknr:GuruText:Text = STRING(kalkhuvtt.KALKNR).
      END.   
		RETURN.

	END METHOD.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID grid1_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
      
      RETURN.

   END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
      appearance42 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
      appearance40 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
      appearance41 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance45 AS Infragistics.Win.Appearance NO-UNDO.
      appearance45 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance44 AS Infragistics.Win.Appearance NO-UNDO.
      appearance44 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance43 AS Infragistics.Win.Appearance NO-UNDO.
      appearance43 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance47 AS Infragistics.Win.Appearance NO-UNDO.
      appearance47 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
      appearance49 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance48 AS Infragistics.Win.Appearance NO-UNDO.
      appearance48 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance46 AS Infragistics.Win.Appearance NO-UNDO.
      appearance46 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance280 AS Infragistics.Win.Appearance NO-UNDO.
      appearance280 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
      appearance6 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
      appearance7 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance268 AS Infragistics.Win.Appearance NO-UNDO.
      appearance268 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance269 AS Infragistics.Win.Appearance NO-UNDO.
      appearance269 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance270 AS Infragistics.Win.Appearance NO-UNDO.
      appearance270 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance271 AS Infragistics.Win.Appearance NO-UNDO.
      appearance271 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance272 AS Infragistics.Win.Appearance NO-UNDO.
      appearance272 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance273 AS Infragistics.Win.Appearance NO-UNDO.
      appearance273 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance274 AS Infragistics.Win.Appearance NO-UNDO.
      appearance274 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance275 AS Infragistics.Win.Appearance NO-UNDO.
      appearance275 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance276 AS Infragistics.Win.Appearance NO-UNDO.
      appearance276 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance277 AS Infragistics.Win.Appearance NO-UNDO.
      appearance277 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance278 AS Infragistics.Win.Appearance NO-UNDO.
      appearance278 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance279 AS Infragistics.Win.Appearance NO-UNDO.
      appearance279 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance318 AS Infragistics.Win.Appearance NO-UNDO.
      appearance318 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance319 AS Infragistics.Win.Appearance NO-UNDO.
      appearance319 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance320 AS Infragistics.Win.Appearance NO-UNDO.
      appearance320 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance321 AS Infragistics.Win.Appearance NO-UNDO.
      appearance321 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance322 AS Infragistics.Win.Appearance NO-UNDO.
      appearance322 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance323 AS Infragistics.Win.Appearance NO-UNDO.
      appearance323 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance324 AS Infragistics.Win.Appearance NO-UNDO.
      appearance324 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance325 AS Infragistics.Win.Appearance NO-UNDO.
      appearance325 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance326 AS Infragistics.Win.Appearance NO-UNDO.
      appearance326 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance327 AS Infragistics.Win.Appearance NO-UNDO.
      appearance327 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance328 AS Infragistics.Win.Appearance NO-UNDO.
      appearance328 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance329 AS Infragistics.Win.Appearance NO-UNDO.
      appearance329 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance330 AS Infragistics.Win.Appearance NO-UNDO.
      appearance330 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance331 AS Infragistics.Win.Appearance NO-UNDO.
      appearance331 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance332 AS Infragistics.Win.Appearance NO-UNDO.
      appearance332 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance333 AS Infragistics.Win.Appearance NO-UNDO.
      appearance333 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance334 AS Infragistics.Win.Appearance NO-UNDO.
      appearance334 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance335 AS Infragistics.Win.Appearance NO-UNDO.
      appearance335 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance336 AS Infragistics.Win.Appearance NO-UNDO.
      appearance336 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance337 AS Infragistics.Win.Appearance NO-UNDO.
      appearance337 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance338 AS Infragistics.Win.Appearance NO-UNDO.
      appearance338 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance339 AS Infragistics.Win.Appearance NO-UNDO.
      appearance339 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance340 AS Infragistics.Win.Appearance NO-UNDO.
      appearance340 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance341 AS Infragistics.Win.Appearance NO-UNDO.
      appearance341 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance342 AS Infragistics.Win.Appearance NO-UNDO.
      appearance342 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance343 AS Infragistics.Win.Appearance NO-UNDO.
      appearance343 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance344 AS Infragistics.Win.Appearance NO-UNDO.
      appearance344 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance345 AS Infragistics.Win.Appearance NO-UNDO.
      appearance345 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance346 AS Infragistics.Win.Appearance NO-UNDO.
      appearance346 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance347 AS Infragistics.Win.Appearance NO-UNDO.
      appearance347 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance348 AS Infragistics.Win.Appearance NO-UNDO.
      appearance348 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance349 AS Infragistics.Win.Appearance NO-UNDO.
      appearance349 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance350 AS Infragistics.Win.Appearance NO-UNDO.
      appearance350 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance351 AS Infragistics.Win.Appearance NO-UNDO.
      appearance351 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance352 AS Infragistics.Win.Appearance NO-UNDO.
      appearance352 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance353 AS Infragistics.Win.Appearance NO-UNDO.
      appearance353 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Kalk.Kalkreg").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance354 AS Infragistics.Win.Appearance NO-UNDO.
      appearance354 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance355 AS Infragistics.Win.Appearance NO-UNDO.
      appearance355 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance356 AS Infragistics.Win.Appearance NO-UNDO.
      appearance356 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance357 AS Infragistics.Win.Appearance NO-UNDO.
      appearance357 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance358 AS Infragistics.Win.Appearance NO-UNDO.
      appearance358 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance359 AS Infragistics.Win.Appearance NO-UNDO.
      appearance359 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance360 AS Infragistics.Win.Appearance NO-UNDO.
      appearance360 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance361 AS Infragistics.Win.Appearance NO-UNDO.
      appearance361 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance362 AS Infragistics.Win.Appearance NO-UNDO.
      appearance362 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance363 AS Infragistics.Win.Appearance NO-UNDO.
      appearance363 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance364 AS Infragistics.Win.Appearance NO-UNDO.
      appearance364 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance365 AS Infragistics.Win.Appearance NO-UNDO.
      appearance365 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance378 AS Infragistics.Win.Appearance NO-UNDO.
      appearance378 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance379 AS Infragistics.Win.Appearance NO-UNDO.
      appearance379 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance380 AS Infragistics.Win.Appearance NO-UNDO.
      appearance380 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance381 AS Infragistics.Win.Appearance NO-UNDO.
      appearance381 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance382 AS Infragistics.Win.Appearance NO-UNDO.
      appearance382 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance383 AS Infragistics.Win.Appearance NO-UNDO.
      appearance383 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance384 AS Infragistics.Win.Appearance NO-UNDO.
      appearance384 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance385 AS Infragistics.Win.Appearance NO-UNDO.
      appearance385 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance386 AS Infragistics.Win.Appearance NO-UNDO.
      appearance386 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance387 AS Infragistics.Win.Appearance NO-UNDO.
      appearance387 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance388 AS Infragistics.Win.Appearance NO-UNDO.
      appearance388 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance389 AS Infragistics.Win.Appearance NO-UNDO.
      appearance389 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance366 AS Infragistics.Win.Appearance NO-UNDO.
      appearance366 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance367 AS Infragistics.Win.Appearance NO-UNDO.
      appearance367 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance368 AS Infragistics.Win.Appearance NO-UNDO.
      appearance368 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance369 AS Infragistics.Win.Appearance NO-UNDO.
      appearance369 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance370 AS Infragistics.Win.Appearance NO-UNDO.
      appearance370 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance371 AS Infragistics.Win.Appearance NO-UNDO.
      appearance371 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance372 AS Infragistics.Win.Appearance NO-UNDO.
      appearance372 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance373 AS Infragistics.Win.Appearance NO-UNDO.
      appearance373 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance374 AS Infragistics.Win.Appearance NO-UNDO.
      appearance374 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance375 AS Infragistics.Win.Appearance NO-UNDO.
      appearance375 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance376 AS Infragistics.Win.Appearance NO-UNDO.
      appearance376 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance377 AS Infragistics.Win.Appearance NO-UNDO.
      appearance377 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab7 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab7 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab8 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab8 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueListItem6 AS Infragistics.Win.ValueListItem NO-UNDO.
      valueListItem6 = NEW Infragistics.Win.ValueListItem().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueListItem7 AS Infragistics.Win.ValueListItem NO-UNDO.
      valueListItem7 = NEW Infragistics.Win.ValueListItem().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance390 AS Infragistics.Win.Appearance NO-UNDO.
      appearance390 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance391 AS Infragistics.Win.Appearance NO-UNDO.
      appearance391 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance392 AS Infragistics.Win.Appearance NO-UNDO.
      appearance392 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance393 AS Infragistics.Win.Appearance NO-UNDO.
      appearance393 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance394 AS Infragistics.Win.Appearance NO-UNDO.
      appearance394 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance395 AS Infragistics.Win.Appearance NO-UNDO.
      appearance395 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance396 AS Infragistics.Win.Appearance NO-UNDO.
      appearance396 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance397 AS Infragistics.Win.Appearance NO-UNDO.
      appearance397 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance398 AS Infragistics.Win.Appearance NO-UNDO.
      appearance398 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance399 AS Infragistics.Win.Appearance NO-UNDO.
      appearance399 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance400 AS Infragistics.Win.Appearance NO-UNDO.
      appearance400 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance401 AS Infragistics.Win.Appearance NO-UNDO.
      appearance401 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance402 AS Infragistics.Win.Appearance NO-UNDO.
      appearance402 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance403 AS Infragistics.Win.Appearance NO-UNDO.
      appearance403 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance404 AS Infragistics.Win.Appearance NO-UNDO.
      appearance404 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance405 AS Infragistics.Win.Appearance NO-UNDO.
      appearance405 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance406 AS Infragistics.Win.Appearance NO-UNDO.
      appearance406 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance407 AS Infragistics.Win.Appearance NO-UNDO.
      appearance407 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance408 AS Infragistics.Win.Appearance NO-UNDO.
      appearance408 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance409 AS Infragistics.Win.Appearance NO-UNDO.
      appearance409 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance410 AS Infragistics.Win.Appearance NO-UNDO.
      appearance410 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance411 AS Infragistics.Win.Appearance NO-UNDO.
      appearance411 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance412 AS Infragistics.Win.Appearance NO-UNDO.
      appearance412 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance413 AS Infragistics.Win.Appearance NO-UNDO.
      appearance413 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance414 AS Infragistics.Win.Appearance NO-UNDO.
      appearance414 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance415 AS Infragistics.Win.Appearance NO-UNDO.
      appearance415 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance416 AS Infragistics.Win.Appearance NO-UNDO.
      appearance416 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance417 AS Infragistics.Win.Appearance NO-UNDO.
      appearance417 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance418 AS Infragistics.Win.Appearance NO-UNDO.
      appearance418 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance419 AS Infragistics.Win.Appearance NO-UNDO.
      appearance419 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance420 AS Infragistics.Win.Appearance NO-UNDO.
      appearance420 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance421 AS Infragistics.Win.Appearance NO-UNDO.
      appearance421 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance422 AS Infragistics.Win.Appearance NO-UNDO.
      appearance422 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance423 AS Infragistics.Win.Appearance NO-UNDO.
      appearance423 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance424 AS Infragistics.Win.Appearance NO-UNDO.
      appearance424 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance425 AS Infragistics.Win.Appearance NO-UNDO.
      appearance425 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance426 AS Infragistics.Win.Appearance NO-UNDO.
      appearance426 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance427 AS Infragistics.Win.Appearance NO-UNDO.
      appearance427 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance428 AS Infragistics.Win.Appearance NO-UNDO.
      appearance428 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance429 AS Infragistics.Win.Appearance NO-UNDO.
      appearance429 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance430 AS Infragistics.Win.Appearance NO-UNDO.
      appearance430 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance431 AS Infragistics.Win.Appearance NO-UNDO.
      appearance431 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance432 AS Infragistics.Win.Appearance NO-UNDO.
      appearance432 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance433 AS Infragistics.Win.Appearance NO-UNDO.
      appearance433 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance434 AS Infragistics.Win.Appearance NO-UNDO.
      appearance434 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance435 AS Infragistics.Win.Appearance NO-UNDO.
      appearance435 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance436 AS Infragistics.Win.Appearance NO-UNDO.
      appearance436 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance437 AS Infragistics.Win.Appearance NO-UNDO.
      appearance437 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance489 AS Infragistics.Win.Appearance NO-UNDO.
      appearance489 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance490 AS Infragistics.Win.Appearance NO-UNDO.
      appearance490 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance491 AS Infragistics.Win.Appearance NO-UNDO.
      appearance491 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance492 AS Infragistics.Win.Appearance NO-UNDO.
      appearance492 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance493 AS Infragistics.Win.Appearance NO-UNDO.
      appearance493 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance494 AS Infragistics.Win.Appearance NO-UNDO.
      appearance494 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance495 AS Infragistics.Win.Appearance NO-UNDO.
      appearance495 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance496 AS Infragistics.Win.Appearance NO-UNDO.
      appearance496 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance497 AS Infragistics.Win.Appearance NO-UNDO.
      appearance497 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance498 AS Infragistics.Win.Appearance NO-UNDO.
      appearance498 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance499 AS Infragistics.Win.Appearance NO-UNDO.
      appearance499 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance500 AS Infragistics.Win.Appearance NO-UNDO.
      appearance500 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance501 AS Infragistics.Win.Appearance NO-UNDO.
      appearance501 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance502 AS Infragistics.Win.Appearance NO-UNDO.
      appearance502 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance503 AS Infragistics.Win.Appearance NO-UNDO.
      appearance503 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance504 AS Infragistics.Win.Appearance NO-UNDO.
      appearance504 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance505 AS Infragistics.Win.Appearance NO-UNDO.
      appearance505 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance506 AS Infragistics.Win.Appearance NO-UNDO.
      appearance506 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance507 AS Infragistics.Win.Appearance NO-UNDO.
      appearance507 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance508 AS Infragistics.Win.Appearance NO-UNDO.
      appearance508 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance509 AS Infragistics.Win.Appearance NO-UNDO.
      appearance509 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance510 AS Infragistics.Win.Appearance NO-UNDO.
      appearance510 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance511 AS Infragistics.Win.Appearance NO-UNDO.
      appearance511 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance512 AS Infragistics.Win.Appearance NO-UNDO.
      appearance512 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance513 AS Infragistics.Win.Appearance NO-UNDO.
      appearance513 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance514 AS Infragistics.Win.Appearance NO-UNDO.
      appearance514 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance515 AS Infragistics.Win.Appearance NO-UNDO.
      appearance515 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance516 AS Infragistics.Win.Appearance NO-UNDO.
      appearance516 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance517 AS Infragistics.Win.Appearance NO-UNDO.
      appearance517 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance518 AS Infragistics.Win.Appearance NO-UNDO.
      appearance518 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance519 AS Infragistics.Win.Appearance NO-UNDO.
      appearance519 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance520 AS Infragistics.Win.Appearance NO-UNDO.
      appearance520 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance521 AS Infragistics.Win.Appearance NO-UNDO.
      appearance521 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance522 AS Infragistics.Win.Appearance NO-UNDO.
      appearance522 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance523 AS Infragistics.Win.Appearance NO-UNDO.
      appearance523 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance524 AS Infragistics.Win.Appearance NO-UNDO.
      appearance524 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance438 AS Infragistics.Win.Appearance NO-UNDO.
      appearance438 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance439 AS Infragistics.Win.Appearance NO-UNDO.
      appearance439 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance440 AS Infragistics.Win.Appearance NO-UNDO.
      appearance440 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance441 AS Infragistics.Win.Appearance NO-UNDO.
      appearance441 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance442 AS Infragistics.Win.Appearance NO-UNDO.
      appearance442 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance443 AS Infragistics.Win.Appearance NO-UNDO.
      appearance443 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance444 AS Infragistics.Win.Appearance NO-UNDO.
      appearance444 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance445 AS Infragistics.Win.Appearance NO-UNDO.
      appearance445 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance446 AS Infragistics.Win.Appearance NO-UNDO.
      appearance446 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance447 AS Infragistics.Win.Appearance NO-UNDO.
      appearance447 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance448 AS Infragistics.Win.Appearance NO-UNDO.
      appearance448 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance449 AS Infragistics.Win.Appearance NO-UNDO.
      appearance449 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance450 AS Infragistics.Win.Appearance NO-UNDO.
      appearance450 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance451 AS Infragistics.Win.Appearance NO-UNDO.
      appearance451 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance452 AS Infragistics.Win.Appearance NO-UNDO.
      appearance452 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance453 AS Infragistics.Win.Appearance NO-UNDO.
      appearance453 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance454 AS Infragistics.Win.Appearance NO-UNDO.
      appearance454 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance455 AS Infragistics.Win.Appearance NO-UNDO.
      appearance455 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance456 AS Infragistics.Win.Appearance NO-UNDO.
      appearance456 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance457 AS Infragistics.Win.Appearance NO-UNDO.
      appearance457 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance458 AS Infragistics.Win.Appearance NO-UNDO.
      appearance458 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance459 AS Infragistics.Win.Appearance NO-UNDO.
      appearance459 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance460 AS Infragistics.Win.Appearance NO-UNDO.
      appearance460 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance461 AS Infragistics.Win.Appearance NO-UNDO.
      appearance461 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance462 AS Infragistics.Win.Appearance NO-UNDO.
      appearance462 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance463 AS Infragistics.Win.Appearance NO-UNDO.
      appearance463 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance464 AS Infragistics.Win.Appearance NO-UNDO.
      appearance464 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance465 AS Infragistics.Win.Appearance NO-UNDO.
      appearance465 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance466 AS Infragistics.Win.Appearance NO-UNDO.
      appearance466 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance467 AS Infragistics.Win.Appearance NO-UNDO.
      appearance467 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance468 AS Infragistics.Win.Appearance NO-UNDO.
      appearance468 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance469 AS Infragistics.Win.Appearance NO-UNDO.
      appearance469 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance470 AS Infragistics.Win.Appearance NO-UNDO.
      appearance470 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance471 AS Infragistics.Win.Appearance NO-UNDO.
      appearance471 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance472 AS Infragistics.Win.Appearance NO-UNDO.
      appearance472 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance473 AS Infragistics.Win.Appearance NO-UNDO.
      appearance473 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance474 AS Infragistics.Win.Appearance NO-UNDO.
      appearance474 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance475 AS Infragistics.Win.Appearance NO-UNDO.
      appearance475 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance476 AS Infragistics.Win.Appearance NO-UNDO.
      appearance476 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance477 AS Infragistics.Win.Appearance NO-UNDO.
      appearance477 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance478 AS Infragistics.Win.Appearance NO-UNDO.
      appearance478 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance479 AS Infragistics.Win.Appearance NO-UNDO.
      appearance479 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance480 AS Infragistics.Win.Appearance NO-UNDO.
      appearance480 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance481 AS Infragistics.Win.Appearance NO-UNDO.
      appearance481 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance482 AS Infragistics.Win.Appearance NO-UNDO.
      appearance482 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance483 AS Infragistics.Win.Appearance NO-UNDO.
      appearance483 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance484 AS Infragistics.Win.Appearance NO-UNDO.
      appearance484 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance485 AS Infragistics.Win.Appearance NO-UNDO.
      appearance485 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance486 AS Infragistics.Win.Appearance NO-UNDO.
      appearance486 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance487 AS Infragistics.Win.Appearance NO-UNDO.
      appearance487 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance488 AS Infragistics.Win.Appearance NO-UNDO.
      appearance488 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
      appearance2 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab13 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab13 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab15 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab15 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab18 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab18 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab19 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab19 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab22 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab22 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab20 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab20 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab21 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab21 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab23 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab23 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab24 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab24 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("Start").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Kalkylinfo").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool1 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Benämning").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool3 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool3 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("ee").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup2 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup2 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Visa").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup3 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup3 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("Uppföljning").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool2 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("Benämning").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE textBoxTool4 AS Infragistics.Win.UltraWinToolbars.TextBoxTool NO-UNDO.
      textBoxTool4 = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("ee").
      THIS-OBJECT:VisaExcelTab = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:ultraTabPageControl16 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaIEBrowser = NEW System.Windows.Forms.WebBrowser().
      THIS-OBJECT:ultraTabPageControl17 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaPDFBrowser = NEW System.Windows.Forms.WebBrowser().
      THIS-OBJECT:ultraTabPageControl9 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:cmbKata = NEW Controls.ComboSimple().
      THIS-OBJECT:cmbUtf = NEW Controls.Combo().
      THIS-OBJECT:fillinCharAnsvarig = NEW Controls.FillinChar().
      THIS-OBJECT:fillinIntegerKalknr = NEW Controls.FillinInteger().
      THIS-OBJECT:Labelomr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmbOmr = NEW Controls.Combo().
      THIS-OBJECT:cmbTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:fillinCharKalkBen = NEW Controls.FillinChar().
      THIS-OBJECT:personalkalkyl = NEW Controls.Grid().
      THIS-OBJECT:Labelanm = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:anm = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:Labelkatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:Labelutf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:Decimaltest2 = NEW Controls.TextDecimal().
      THIS-OBJECT:DECIMALTEST = NEW Controls.FillinDecimal().
      THIS-OBJECT:buttonBackAlla = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverAlla = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOvermark = NEW Controls.ButtonOver().
      THIS-OBJECT:lopkoder = NEW Controls.Grid().
      THIS-OBJECT:arbkoder = NEW Controls.Grid().
      THIS-OBJECT:kalkylkoder = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl10 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStrip1 = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:SkapaNyEgen = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:TaBortEgen = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:egnakoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainer2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:prisposter = NEW Controls.Grid().
      THIS-OBJECT:splitContainer3 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:buttonOverAllaPriser = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonBackAllaPriser = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverMarkPriser = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonBackMarkPriser = NEW Controls.ButtonOver().
      THIS-OBJECT:valdaprisposter = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl11 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaDividerX = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ultraTabControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:VisaPanelSettings = NEW Infragistics.Win.Misc.UltraPanel().
      THIS-OBJECT:VisaReset = NEW Controls.Button().
      THIS-OBJECT:VisaGroupBoxTyp = NEW Infragistics.Win.Misc.UltraGroupBox().
      THIS-OBJECT:VisaKalkylTyp = NEW Infragistics.Win.UltraWinEditors.UltraOptionSet().
      THIS-OBJECT:VisaGenerate = NEW Controls.Button().
      THIS-OBJECT:VisaInklGroupBox = NEW Infragistics.Win.Misc.UltraGroupBox().
      THIS-OBJECT:VisaInklOFakt = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:VisaInklOEP = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:VisaInklKAnm = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:VisaInklAnm = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:VisaInklHuvud = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
      THIS-OBJECT:VisaFunkGrupp = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:VisaFunkSum = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
      THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:egnafaktorer = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:egnapriser = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:btnBeredningKalkyl = NEW Controls.Button().
      THIS-OBJECT:buttonBackMarkMtrl = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverMarkMtrl = NEW Controls.ButtonOver().
      THIS-OBJECT:egetmtrl = NEW Controls.Grid().
      THIS-OBJECT:valmtrl = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl5 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:buttonBackAllaMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonBackMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverAllaMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:mallarkoder = NEW Controls.Grid().
      THIS-OBJECT:valdamallar = NEW Controls.Grid().
      THIS-OBJECT:mallar = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl12 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:LabelAvtal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_avtal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:Labelartalskj = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_artalavtal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:avtaltext = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:Labelklgartal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_KLGartal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:ultraTabPageControl7 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:frikalk = NEW Controls.Grid().
      THIS-OBJECT:ultraTabPageControl6 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabPageControl8 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:Kalkreg_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:Kalkyltab = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:KalkylToolbarsManager = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Kalkreg_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:ultraTextEditor3 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:VisaExcelTab:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl16:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl17:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl9:SuspendLayout().
      CAST(THIS-OBJECT:cmbKata, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmbUtf, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmbOmr, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:personalkalkyl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:anm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
      CAST(THIS-OBJECT:Decimaltest2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:lopkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:arbkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:kalkylkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl10:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      THIS-OBJECT:toolStrip1:SuspendLayout().
      CAST(THIS-OBJECT:egnakoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer2:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer2:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer2:SuspendLayout().
      CAST(THIS-OBJECT:prisposter, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer3:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer3:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer3:SuspendLayout().
      CAST(THIS-OBJECT:valdaprisposter, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl11:SuspendLayout().
      THIS-OBJECT:VisaDividerX:Panel1:SuspendLayout().
      THIS-OBJECT:VisaDividerX:Panel2:SuspendLayout().
      THIS-OBJECT:VisaDividerX:SuspendLayout().
      CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabControl2:SuspendLayout().
      THIS-OBJECT:VisaPanelSettings:ClientArea:SuspendLayout().
      THIS-OBJECT:VisaPanelSettings:SuspendLayout().
      CAST(THIS-OBJECT:VisaGroupBoxTyp, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:VisaGroupBoxTyp:SuspendLayout().
      CAST(THIS-OBJECT:VisaKalkylTyp, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaInklGroupBox, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:VisaInklGroupBox:SuspendLayout().
      CAST(THIS-OBJECT:VisaInklOFakt, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaInklOEP, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaInklKAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaInklAnm, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaInklHuvud, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraGroupBox1:SuspendLayout().
      CAST(THIS-OBJECT:VisaFunkGrupp, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:VisaFunkSum, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
      CAST(THIS-OBJECT:egnafaktorer, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
      CAST(THIS-OBJECT:egnapriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl4:SuspendLayout().
      CAST(THIS-OBJECT:egetmtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:valmtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl5:SuspendLayout().
      CAST(THIS-OBJECT:mallarkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:valdamallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:mallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl12:SuspendLayout().
      CAST(THIS-OBJECT:cmb_avtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmb_artalavtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:avtaltext, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmb_KLGartal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabPageControl7:SuspendLayout().
      CAST(THIS-OBJECT:frikalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:Kalkreg_Fill_Panel:SuspendLayout().
      CAST(THIS-OBJECT:Kalkyltab, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:Kalkyltab:SuspendLayout().
      CAST(THIS-OBJECT:KalkylToolbarsManager, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTextEditor3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* VisaExcelTab */
      /*  */
      THIS-OBJECT:VisaExcelTab:Controls:Add(THIS-OBJECT:VisaExcel).
      THIS-OBJECT:VisaExcelTab:Location = NEW System.Drawing.Point(1, 23).
      THIS-OBJECT:VisaExcelTab:Name = "VisaExcelTab".
      THIS-OBJECT:VisaExcelTab:Size = NEW System.Drawing.Size(1019, 437).
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(1019, 437).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      /*  */
      /* ultraTabPageControl16 */
      /*  */
      THIS-OBJECT:ultraTabPageControl16:Controls:Add(THIS-OBJECT:VisaIEBrowser).
      THIS-OBJECT:ultraTabPageControl16:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl16:Name = "ultraTabPageControl16".
      THIS-OBJECT:ultraTabPageControl16:Size = NEW System.Drawing.Size(1016, 437).
      /*  */
      /* VisaIEBrowser */
      /*  */
      THIS-OBJECT:VisaIEBrowser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaIEBrowser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaIEBrowser:MinimumSize = NEW System.Drawing.Size(20, 20).
      THIS-OBJECT:VisaIEBrowser:Name = "VisaIEBrowser".
      THIS-OBJECT:VisaIEBrowser:Size = NEW System.Drawing.Size(1016, 437).
      THIS-OBJECT:VisaIEBrowser:TabIndex = 0.
      /*  */
      /* ultraTabPageControl17 */
      /*  */
      THIS-OBJECT:ultraTabPageControl17:Controls:Add(THIS-OBJECT:VisaPDFBrowser).
      THIS-OBJECT:ultraTabPageControl17:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl17:Name = "ultraTabPageControl17".
      THIS-OBJECT:ultraTabPageControl17:Size = NEW System.Drawing.Size(1016, 437).
      /*  */
      /* VisaPDFBrowser */
      /*  */
      THIS-OBJECT:VisaPDFBrowser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaPDFBrowser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPDFBrowser:MinimumSize = NEW System.Drawing.Size(20, 20).
      THIS-OBJECT:VisaPDFBrowser:Name = "VisaPDFBrowser".
      THIS-OBJECT:VisaPDFBrowser:Size = NEW System.Drawing.Size(1016, 437).
      THIS-OBJECT:VisaPDFBrowser:TabIndex = 0.
      /*  */
      /* ultraTabPageControl9 */
      /*  */
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:cmbKata).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:cmbUtf).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:fillinCharAnsvarig).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:fillinIntegerKalknr).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:Labelomr).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:cmbOmr).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:cmbTyp).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:fillinCharKalkBen).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:personalkalkyl).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:Labelanm).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:anm).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:Labelkatalog).
      THIS-OBJECT:ultraTabPageControl9:Controls:Add(THIS-OBJECT:Labelutf).
      THIS-OBJECT:ultraTabPageControl9:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl9:Name = "ultraTabPageControl9".
      THIS-OBJECT:ultraTabPageControl9:Size = NEW System.Drawing.Size(1173, 463).
      THIS-OBJECT:ultraTabPageControl9:Paint:Subscribe(THIS-OBJECT:ultraTabPageControl9_Paint).
      /*  */
      /* cmbKata */
      /*  */
      THIS-OBJECT:cmbKata:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:cmbKata:Location = NEW System.Drawing.Point(115, 102).
      THIS-OBJECT:cmbKata:Name = "cmbKata".
      THIS-OBJECT:cmbKata:Size = NEW System.Drawing.Size(208, 21).
      THIS-OBJECT:cmbKata:TabIndex = 29.
      /*  */
      /* cmbUtf */
      /*  */
      THIS-OBJECT:cmbUtf:CheckedListSettings:CheckStateMember = "".
      appearance42:BackColor = System.Drawing.SystemColors:Window.
      appearance42:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:cmbUtf:DisplayLayout:Appearance = appearance42.
      THIS-OBJECT:cmbUtf:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:cmbUtf:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance1:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance1:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance1:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance1:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbUtf:DisplayLayout:GroupByBox:Appearance = appearance1.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmbUtf:DisplayLayout:GroupByBox:BandLabelAppearance = appearance40.
      THIS-OBJECT:cmbUtf:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance41:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance41:BackColor2 = System.Drawing.SystemColors:Control.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmbUtf:DisplayLayout:GroupByBox:PromptAppearance = appearance41.
      THIS-OBJECT:cmbUtf:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:cmbUtf:DisplayLayout:MaxRowScrollRegions = 1.
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:ActiveCellAppearance = appearance50.
      appearance45:BackColor = System.Drawing.SystemColors:Highlight.
      appearance45:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:ActiveRowAppearance = appearance45.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance44:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:CardAreaAppearance = appearance44.
      appearance43:BorderColor = System.Drawing.Color:Silver.
      appearance43:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:CellAppearance = appearance43.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance49:TextHAlignAsString = "Left".
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:HeaderAppearance = appearance49.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance48:BackColor = System.Drawing.SystemColors:Window.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:RowAppearance = appearance48.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance46:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:cmbUtf:DisplayLayout:Override:TemplateAddRowAppearance = appearance46.
      THIS-OBJECT:cmbUtf:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:cmbUtf:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:cmbUtf:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:cmbUtf:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:cmbUtf:Location = NEW System.Drawing.Point(131, 183).
      THIS-OBJECT:cmbUtf:Name = "cmbUtf".
      THIS-OBJECT:cmbUtf:Size = NEW System.Drawing.Size(184, 22).
      THIS-OBJECT:cmbUtf:TabIndex = 28.
      /*  */
      /* fillinCharAnsvarig */
      /*  */
      THIS-OBJECT:fillinCharAnsvarig:Location = NEW System.Drawing.Point(44, 212).
      THIS-OBJECT:fillinCharAnsvarig:Name = "fillinCharAnsvarig".
      THIS-OBJECT:fillinCharAnsvarig:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinCharAnsvarig:TabIndex = 27.
      /*  */
      /* fillinIntegerKalknr */
      /*  */
      THIS-OBJECT:fillinIntegerKalknr:Location = NEW System.Drawing.Point(44, 4).
      THIS-OBJECT:fillinIntegerKalknr:Name = "fillinIntegerKalknr".
      THIS-OBJECT:fillinIntegerKalknr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinIntegerKalknr:TabIndex = 26.
      /*  */
      /* Labelomr */
      /*  */
      appearance280:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelomr:Appearance = appearance280.
      THIS-OBJECT:Labelomr:Location = NEW System.Drawing.Point(44, 37).
      THIS-OBJECT:Labelomr:Name = "Labelomr".
      THIS-OBJECT:Labelomr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelomr:TabIndex = 23.
      THIS-OBJECT:Labelomr:Text = "Område".
      /*  */
      /* cmbOmr */
      /*  */
      THIS-OBJECT:cmbOmr:CheckedListSettings:CheckStateMember = "".
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:cmbOmr:DisplayLayout:Appearance = appearance6.
      THIS-OBJECT:cmbOmr:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:cmbOmr:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance3:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance3:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance3:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance3:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbOmr:DisplayLayout:GroupByBox:Appearance = appearance3.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmbOmr:DisplayLayout:GroupByBox:BandLabelAppearance = appearance4.
      THIS-OBJECT:cmbOmr:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance5:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance5:BackColor2 = System.Drawing.SystemColors:Control.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:cmbOmr:DisplayLayout:GroupByBox:PromptAppearance = appearance5.
      THIS-OBJECT:cmbOmr:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:cmbOmr:DisplayLayout:MaxRowScrollRegions = 1.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:ActiveCellAppearance = appearance14.
      appearance9:BackColor = System.Drawing.SystemColors:Highlight.
      appearance9:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:ActiveRowAppearance = appearance9.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:CardAreaAppearance = appearance8.
      appearance7:BorderColor = System.Drawing.Color:Silver.
      appearance7:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:CellAppearance = appearance7.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:CellPadding = 0.
      appearance11:BackColor = System.Drawing.SystemColors:Control.
      appearance11:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance11:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance11:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance11:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:GroupByRowAppearance = appearance11.
      appearance13:TextHAlignAsString = "Left".
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:HeaderAppearance = appearance13.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      appearance12:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:RowAppearance = appearance12.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance10:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:cmbOmr:DisplayLayout:Override:TemplateAddRowAppearance = appearance10.
      THIS-OBJECT:cmbOmr:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:cmbOmr:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:cmbOmr:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:cmbOmr:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:cmbOmr:Location = NEW System.Drawing.Point(127, 38).
      THIS-OBJECT:cmbOmr:Name = "cmbOmr".
      THIS-OBJECT:cmbOmr:Size = NEW System.Drawing.Size(157, 22).
      THIS-OBJECT:cmbOmr:TabIndex = 22.
      THIS-OBJECT:cmbOmr:ValueChanged:Subscribe(THIS-OBJECT:cmbOmr_ValueChanged).
      THIS-OBJECT:cmbOmr:InitializeLayout:Subscribe(THIS-OBJECT:cmbOmr_InitializeLayout).
      /*  */
      /* cmbTyp */
      /*  */
      THIS-OBJECT:cmbTyp:Location = NEW System.Drawing.Point(44, 67).
      THIS-OBJECT:cmbTyp:Name = "cmbTyp".
      THIS-OBJECT:cmbTyp:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:cmbTyp:TabIndex = 20.
      /*  */
      /* fillinCharKalkBen */
      /*  */
      THIS-OBJECT:fillinCharKalkBen:Location = NEW System.Drawing.Point(44, 129).
      THIS-OBJECT:fillinCharKalkBen:Name = "fillinCharKalkBen".
      THIS-OBJECT:fillinCharKalkBen:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinCharKalkBen:TabIndex = 17.
      THIS-OBJECT:fillinCharKalkBen:Tag = "".
      /*  */
      /* personalkalkyl */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
      THIS-OBJECT:personalkalkyl:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance268:BackColor = System.Drawing.SystemColors:Window.
      appearance268:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Appearance = appearance268.
      THIS-OBJECT:personalkalkyl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:personalkalkyl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance269:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance269:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance269:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance269:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:personalkalkyl:DisplayLayout:GroupByBox:Appearance = appearance269.
      appearance270:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:personalkalkyl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance270.
      THIS-OBJECT:personalkalkyl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance271:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance271:BackColor2 = System.Drawing.SystemColors:Control.
      appearance271:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance271:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:personalkalkyl:DisplayLayout:GroupByBox:PromptAppearance = appearance271.
      THIS-OBJECT:personalkalkyl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:personalkalkyl:DisplayLayout:MaxRowScrollRegions = 1.
      appearance272:BackColor = System.Drawing.SystemColors:Window.
      appearance272:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:ActiveCellAppearance = appearance272.
      appearance273:BackColor = System.Drawing.SystemColors:Highlight.
      appearance273:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:ActiveRowAppearance = appearance273.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance274:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:CardAreaAppearance = appearance274.
      appearance275:BorderColor = System.Drawing.Color:Silver.
      appearance275:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:CellAppearance = appearance275.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:CellPadding = 0.
      appearance276:BackColor = System.Drawing.SystemColors:Control.
      appearance276:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance276:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance276:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance276:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:GroupByRowAppearance = appearance276.
      appearance277:TextHAlignAsString = "Left".
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:HeaderAppearance = appearance277.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance278:BackColor = System.Drawing.SystemColors:Window.
      appearance278:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:RowAppearance = appearance278.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance279:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:personalkalkyl:DisplayLayout:Override:TemplateAddRowAppearance = appearance279.
      THIS-OBJECT:personalkalkyl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:personalkalkyl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:personalkalkyl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:personalkalkyl:GuruContext = ?.
      THIS-OBJECT:personalkalkyl:Location = NEW System.Drawing.Point(437, 52).
      THIS-OBJECT:personalkalkyl:Name = "personalkalkyl".
      THIS-OBJECT:personalkalkyl:Size = NEW System.Drawing.Size(634, 322).
      THIS-OBJECT:personalkalkyl:TabIndex = 16.
      THIS-OBJECT:personalkalkyl:Text = "grid1".
      /*  */
      /* Labelanm */
      /*  */
      THIS-OBJECT:Labelanm:Location = NEW System.Drawing.Point(22, 355).
      THIS-OBJECT:Labelanm:Name = "Labelanm".
      THIS-OBJECT:Labelanm:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelanm:TabIndex = 15.
      THIS-OBJECT:Labelanm:Text = "Anmärkning".
      /*  */
      /* anm */
      /*  */
      THIS-OBJECT:anm:Location = NEW System.Drawing.Point(93, 246).
      THIS-OBJECT:anm:Multiline = TRUE.
      THIS-OBJECT:anm:Name = "anm".
      THIS-OBJECT:anm:Size = NEW System.Drawing.Size(300, 126).
      THIS-OBJECT:anm:TabIndex = 14.
      THIS-OBJECT:anm:Text = " ".
      /*  */
      /* Labelkatalog */
      /*  */
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelkatalog:Appearance = appearance15.
      THIS-OBJECT:Labelkatalog:Location = NEW System.Drawing.Point(44, 100).
      THIS-OBJECT:Labelkatalog:Name = "Labelkatalog".
      THIS-OBJECT:Labelkatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelkatalog:TabIndex = 11.
      THIS-OBJECT:Labelkatalog:Text = "Katalog".
      /*  */
      /* Labelutf */
      /*  */
      THIS-OBJECT:Labelutf:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:Labelutf:Location = NEW System.Drawing.Point(44, 183).
      THIS-OBJECT:Labelutf:Name = "Labelutf".
      THIS-OBJECT:Labelutf:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:Labelutf:TabIndex = 9.
      THIS-OBJECT:Labelutf:Text = "Utfärdare".
      /*  */
      /* ultraTabPageControl1 */
      /*  */
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:Decimaltest2).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:DECIMALTEST).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:buttonBackAlla).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:buttonBackMark).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:buttonOverAlla).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:buttonOvermark).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:lopkoder).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:arbkoder).
      THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:kalkylkoder).
      THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1".
      THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* Decimaltest2 */
      /*  */
      THIS-OBJECT:Decimaltest2:Location = NEW System.Drawing.Point(995, 131).
      THIS-OBJECT:Decimaltest2:Name = "Decimaltest2".
      THIS-OBJECT:Decimaltest2:Size = NEW System.Drawing.Size(100, 21).
      THIS-OBJECT:Decimaltest2:TabIndex = 12.
      THIS-OBJECT:Decimaltest2:Text = "textDecimal1".
      /*  */
      /* DECIMALTEST */
      /*  */
      THIS-OBJECT:DECIMALTEST:Location = NEW System.Drawing.Point(959, 186).
      THIS-OBJECT:DECIMALTEST:Name = "DECIMALTEST".
      THIS-OBJECT:DECIMALTEST:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:DECIMALTEST:TabIndex = 11.
      /*  */
      /* buttonBackAlla */
      /*  */
      THIS-OBJECT:buttonBackAlla:Location = NEW System.Drawing.Point(424, 411).
      THIS-OBJECT:buttonBackAlla:Name = "buttonBackAlla".
      THIS-OBJECT:buttonBackAlla:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackAlla:TabIndex = 10.
      THIS-OBJECT:buttonBackAlla:Text = "Alla bort".
      THIS-OBJECT:buttonBackAlla:Click:Subscribe(THIS-OBJECT:buttonBackAlla_Click).
      /*  */
      /* buttonBackMark */
      /*  */
      THIS-OBJECT:buttonBackMark:Location = NEW System.Drawing.Point(424, 373).
      THIS-OBJECT:buttonBackMark:Name = "buttonBackMark".
      THIS-OBJECT:buttonBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackMark:TabIndex = 9.
      THIS-OBJECT:buttonBackMark:Text = "Markerade bort".
      THIS-OBJECT:buttonBackMark:Click:Subscribe(THIS-OBJECT:buttonBackMark_Click).
      /*  */
      /* buttonOverAlla */
      /*  */
      THIS-OBJECT:buttonOverAlla:Location = NEW System.Drawing.Point(424, 297).
      THIS-OBJECT:buttonOverAlla:Name = "buttonOverAlla".
      THIS-OBJECT:buttonOverAlla:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverAlla:TabIndex = 8.
      THIS-OBJECT:buttonOverAlla:Text = "Alla väljs".
      THIS-OBJECT:buttonOverAlla:Click:Subscribe(THIS-OBJECT:buttonOverAlla_Click).
      /*  */
      /* buttonOvermark */
      /*  */
      THIS-OBJECT:buttonOvermark:Location = NEW System.Drawing.Point(424, 335).
      THIS-OBJECT:buttonOvermark:Name = "buttonOvermark".
      THIS-OBJECT:buttonOvermark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOvermark:TabIndex = 7.
      THIS-OBJECT:buttonOvermark:Text = "Markerade väljs".
      THIS-OBJECT:buttonOvermark:Click:Subscribe(THIS-OBJECT:buttonOvermark_Click).
      /*  */
      /* lopkoder */
      /*  */
      appearance318:BackColor = System.Drawing.SystemColors:Window.
      appearance318:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:lopkoder:DisplayLayout:Appearance = appearance318.
      THIS-OBJECT:lopkoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:lopkoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance319:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance319:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance319:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance319:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:lopkoder:DisplayLayout:GroupByBox:Appearance = appearance319.
      appearance320:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:lopkoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance320.
      THIS-OBJECT:lopkoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance321:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance321:BackColor2 = System.Drawing.SystemColors:Control.
      appearance321:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance321:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:lopkoder:DisplayLayout:GroupByBox:PromptAppearance = appearance321.
      THIS-OBJECT:lopkoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:lopkoder:DisplayLayout:MaxRowScrollRegions = 1.
      appearance322:BackColor = System.Drawing.SystemColors:Window.
      appearance322:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:ActiveCellAppearance = appearance322.
      appearance323:BackColor = System.Drawing.SystemColors:Highlight.
      appearance323:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:ActiveRowAppearance = appearance323.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance324:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:CardAreaAppearance = appearance324.
      appearance325:BorderColor = System.Drawing.Color:Silver.
      appearance325:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:CellAppearance = appearance325.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:CellPadding = 0.
      appearance326:BackColor = System.Drawing.SystemColors:Control.
      appearance326:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance326:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance326:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance326:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:GroupByRowAppearance = appearance326.
      appearance327:TextHAlignAsString = "Left".
      THIS-OBJECT:lopkoder:DisplayLayout:Override:HeaderAppearance = appearance327.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance328:BackColor = System.Drawing.SystemColors:Window.
      appearance328:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:RowAppearance = appearance328.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance329:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:lopkoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance329.
      THIS-OBJECT:lopkoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:lopkoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:lopkoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:lopkoder:GuruContext = ?.
      THIS-OBJECT:lopkoder:Location = NEW System.Drawing.Point(3, 235).
      THIS-OBJECT:lopkoder:Name = "lopkoder".
      THIS-OBJECT:lopkoder:Size = NEW System.Drawing.Size(414, 223).
      THIS-OBJECT:lopkoder:TabIndex = 5.
      THIS-OBJECT:lopkoder:Text = "grid2".
      /*  */
      /* arbkoder */
      /*  */
      appearance330:BackColor = System.Drawing.SystemColors:Window.
      appearance330:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:arbkoder:DisplayLayout:Appearance = appearance330.
      THIS-OBJECT:arbkoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:arbkoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance331:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance331:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance331:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance331:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:arbkoder:DisplayLayout:GroupByBox:Appearance = appearance331.
      appearance332:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:arbkoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance332.
      THIS-OBJECT:arbkoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance333:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance333:BackColor2 = System.Drawing.SystemColors:Control.
      appearance333:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance333:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:arbkoder:DisplayLayout:GroupByBox:PromptAppearance = appearance333.
      THIS-OBJECT:arbkoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:arbkoder:DisplayLayout:MaxRowScrollRegions = 1.
      appearance334:BackColor = System.Drawing.SystemColors:Window.
      appearance334:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:ActiveCellAppearance = appearance334.
      appearance335:BackColor = System.Drawing.SystemColors:Highlight.
      appearance335:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:ActiveRowAppearance = appearance335.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance336:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:CardAreaAppearance = appearance336.
      appearance337:BorderColor = System.Drawing.Color:Silver.
      appearance337:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:CellAppearance = appearance337.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:CellPadding = 0.
      appearance338:BackColor = System.Drawing.SystemColors:Control.
      appearance338:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance338:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance338:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance338:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:GroupByRowAppearance = appearance338.
      appearance339:TextHAlignAsString = "Left".
      THIS-OBJECT:arbkoder:DisplayLayout:Override:HeaderAppearance = appearance339.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance340:BackColor = System.Drawing.SystemColors:Window.
      appearance340:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:RowAppearance = appearance340.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance341:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:arbkoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance341.
      THIS-OBJECT:arbkoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:arbkoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:arbkoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:arbkoder:GuruContext = ?.
      THIS-OBJECT:arbkoder:Location = NEW System.Drawing.Point(3, 4).
      THIS-OBJECT:arbkoder:Name = "arbkoder".
      THIS-OBJECT:arbkoder:Size = NEW System.Drawing.Size(414, 211).
      THIS-OBJECT:arbkoder:TabIndex = 4.
      THIS-OBJECT:arbkoder:Text = "grid1".
      /*  */
      /* kalkylkoder */
      /*  */
      appearance342:BackColor = System.Drawing.SystemColors:Window.
      appearance342:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Appearance = appearance342.
      THIS-OBJECT:kalkylkoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:kalkylkoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance343:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance343:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance343:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance343:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:kalkylkoder:DisplayLayout:GroupByBox:Appearance = appearance343.
      appearance344:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:kalkylkoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance344.
      THIS-OBJECT:kalkylkoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance345:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance345:BackColor2 = System.Drawing.SystemColors:Control.
      appearance345:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance345:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:kalkylkoder:DisplayLayout:GroupByBox:PromptAppearance = appearance345.
      THIS-OBJECT:kalkylkoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:kalkylkoder:DisplayLayout:MaxRowScrollRegions = 1.
      appearance346:BackColor = System.Drawing.SystemColors:Window.
      appearance346:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:ActiveCellAppearance = appearance346.
      appearance347:BackColor = System.Drawing.SystemColors:Highlight.
      appearance347:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:ActiveRowAppearance = appearance347.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance348:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:CardAreaAppearance = appearance348.
      appearance349:BorderColor = System.Drawing.Color:Silver.
      appearance349:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:CellAppearance = appearance349.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:CellPadding = 0.
      appearance350:BackColor = System.Drawing.SystemColors:Control.
      appearance350:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance350:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance350:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance350:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:GroupByRowAppearance = appearance350.
      appearance351:TextHAlignAsString = "Left".
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:HeaderAppearance = appearance351.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance352:BackColor = System.Drawing.SystemColors:Window.
      appearance352:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:RowAppearance = appearance352.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance353:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:kalkylkoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance353.
      THIS-OBJECT:kalkylkoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:kalkylkoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:kalkylkoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:kalkylkoder:GuruContext = ?.
      THIS-OBJECT:kalkylkoder:Location = NEW System.Drawing.Point(458, 4).
      THIS-OBJECT:kalkylkoder:Name = "kalkylkoder".
      THIS-OBJECT:kalkylkoder:Size = NEW System.Drawing.Size(495, 454).
      THIS-OBJECT:kalkylkoder:TabIndex = 3.
      THIS-OBJECT:kalkylkoder:Text = "grid3".
      /*  */
      /* ultraTabPageControl10 */
      /*  */
      THIS-OBJECT:ultraTabPageControl10:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:ultraTabPageControl10:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:ultraTabPageControl10:Name = "ultraTabPageControl10".
      THIS-OBJECT:ultraTabPageControl10:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      THIS-OBJECT:splitContainer1:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:toolStrip1).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:egnakoder).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:splitContainer2).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(1173, 463).
      THIS-OBJECT:splitContainer1:SplitterDistance = 231.
      THIS-OBJECT:splitContainer1:TabIndex = 18.
      /*  */
      /* toolStrip1 */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:SkapaNyEgen.
      arrayvar0[2] = THIS-OBJECT:TaBortEgen.
      THIS-OBJECT:toolStrip1:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStrip1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStrip1:Name = "toolStrip1".
      THIS-OBJECT:toolStrip1:Size = NEW System.Drawing.Size(1173, 25).
      THIS-OBJECT:toolStrip1:TabIndex = 13.
      THIS-OBJECT:toolStrip1:Text = "toolStrip1".
      /*  */
      /* SkapaNyEgen */
      /*  */
      THIS-OBJECT:SkapaNyEgen:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
      THIS-OBJECT:SkapaNyEgen:Image = CAST(resources:GetObject("SkapaNyEgen.Image"), System.Drawing.Image).
      THIS-OBJECT:SkapaNyEgen:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:SkapaNyEgen:Name = "SkapaNyEgen".
      THIS-OBJECT:SkapaNyEgen:Size = NEW System.Drawing.Size(23, 22).
      THIS-OBJECT:SkapaNyEgen:Text = "Lägg till rad".
      /*  */
      /* TaBortEgen */
      /*  */
      THIS-OBJECT:TaBortEgen:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
      THIS-OBJECT:TaBortEgen:Image = CAST(resources:GetObject("TaBortEgen.Image"), System.Drawing.Image).
      THIS-OBJECT:TaBortEgen:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:TaBortEgen:Name = "TaBortEgen".
      THIS-OBJECT:TaBortEgen:Size = NEW System.Drawing.Size(23, 22).
      THIS-OBJECT:TaBortEgen:Text = "Ta bort markerade".
      /*  */
      /* egnakoder */
      /*  */
      appearance354:BackColor = System.Drawing.SystemColors:Window.
      appearance354:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:egnakoder:DisplayLayout:Appearance = appearance354.
      THIS-OBJECT:egnakoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:egnakoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance355:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance355:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance355:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance355:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnakoder:DisplayLayout:GroupByBox:Appearance = appearance355.
      appearance356:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnakoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance356.
      THIS-OBJECT:egnakoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance357:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance357:BackColor2 = System.Drawing.SystemColors:Control.
      appearance357:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance357:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnakoder:DisplayLayout:GroupByBox:PromptAppearance = appearance357.
      THIS-OBJECT:egnakoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:egnakoder:DisplayLayout:MaxRowScrollRegions = 1.
      appearance358:BackColor = System.Drawing.SystemColors:Window.
      appearance358:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:ActiveCellAppearance = appearance358.
      appearance359:BackColor = System.Drawing.SystemColors:Highlight.
      appearance359:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:ActiveRowAppearance = appearance359.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance360:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:CardAreaAppearance = appearance360.
      appearance361:BorderColor = System.Drawing.Color:Silver.
      appearance361:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:CellAppearance = appearance361.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:CellPadding = 0.
      appearance362:BackColor = System.Drawing.SystemColors:Control.
      appearance362:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance362:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance362:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance362:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:GroupByRowAppearance = appearance362.
      appearance363:TextHAlignAsString = "Left".
      THIS-OBJECT:egnakoder:DisplayLayout:Override:HeaderAppearance = appearance363.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance364:BackColor = System.Drawing.SystemColors:Window.
      appearance364:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:RowAppearance = appearance364.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance365:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:egnakoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance365.
      THIS-OBJECT:egnakoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:egnakoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:egnakoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:egnakoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:egnakoder:GuruContext = ?.
      THIS-OBJECT:egnakoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:egnakoder:Name = "egnakoder".
      THIS-OBJECT:egnakoder:Size = NEW System.Drawing.Size(1173, 231).
      THIS-OBJECT:egnakoder:TabIndex = 12.
      THIS-OBJECT:egnakoder:Text = "grid1".
      /*  */
      /* splitContainer2 */
      /*  */
      THIS-OBJECT:splitContainer2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer2:Name = "splitContainer2".
      /*  */
      /* splitContainer2.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer2:Panel1:Controls:Add(THIS-OBJECT:prisposter).
      /*  */
      /* splitContainer2.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer2:Panel2:Controls:Add(THIS-OBJECT:splitContainer3).
      THIS-OBJECT:splitContainer2:Size = NEW System.Drawing.Size(1173, 228).
      THIS-OBJECT:splitContainer2:SplitterDistance = 391.
      THIS-OBJECT:splitContainer2:TabIndex = 0.
      /*  */
      /* prisposter */
      /*  */
      appearance378:BackColor = System.Drawing.SystemColors:Window.
      appearance378:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:prisposter:DisplayLayout:Appearance = appearance378.
      THIS-OBJECT:prisposter:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:prisposter:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance379:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance379:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance379:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance379:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:prisposter:DisplayLayout:GroupByBox:Appearance = appearance379.
      appearance380:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:prisposter:DisplayLayout:GroupByBox:BandLabelAppearance = appearance380.
      THIS-OBJECT:prisposter:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance381:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance381:BackColor2 = System.Drawing.SystemColors:Control.
      appearance381:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance381:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:prisposter:DisplayLayout:GroupByBox:PromptAppearance = appearance381.
      THIS-OBJECT:prisposter:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:prisposter:DisplayLayout:MaxRowScrollRegions = 1.
      appearance382:BackColor = System.Drawing.SystemColors:Window.
      appearance382:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:prisposter:DisplayLayout:Override:ActiveCellAppearance = appearance382.
      appearance383:BackColor = System.Drawing.SystemColors:Highlight.
      appearance383:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:prisposter:DisplayLayout:Override:ActiveRowAppearance = appearance383.
      THIS-OBJECT:prisposter:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:prisposter:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance384:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:prisposter:DisplayLayout:Override:CardAreaAppearance = appearance384.
      appearance385:BorderColor = System.Drawing.Color:Silver.
      appearance385:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:prisposter:DisplayLayout:Override:CellAppearance = appearance385.
      THIS-OBJECT:prisposter:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:prisposter:DisplayLayout:Override:CellPadding = 0.
      appearance386:BackColor = System.Drawing.SystemColors:Control.
      appearance386:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance386:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance386:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance386:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:prisposter:DisplayLayout:Override:GroupByRowAppearance = appearance386.
      appearance387:TextHAlignAsString = "Left".
      THIS-OBJECT:prisposter:DisplayLayout:Override:HeaderAppearance = appearance387.
      THIS-OBJECT:prisposter:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:prisposter:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance388:BackColor = System.Drawing.SystemColors:Window.
      appearance388:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:prisposter:DisplayLayout:Override:RowAppearance = appearance388.
      THIS-OBJECT:prisposter:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance389:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:prisposter:DisplayLayout:Override:TemplateAddRowAppearance = appearance389.
      THIS-OBJECT:prisposter:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:prisposter:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:prisposter:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:prisposter:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:prisposter:GuruContext = ?.
      THIS-OBJECT:prisposter:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:prisposter:Name = "prisposter".
      THIS-OBJECT:prisposter:Size = NEW System.Drawing.Size(391, 228).
      THIS-OBJECT:prisposter:TabIndex = 10.
      THIS-OBJECT:prisposter:Text = "grid1".
      /*  */
      /* splitContainer3 */
      /*  */
      THIS-OBJECT:splitContainer3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer3:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer3:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer3:Name = "splitContainer3".
      /*  */
      /* splitContainer3.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:buttonOverAllaPriser).
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:buttonBackAllaPriser).
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:buttonOverMarkPriser).
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:buttonBackMarkPriser).
      /*  */
      /* splitContainer3.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel2:Controls:Add(THIS-OBJECT:valdaprisposter).
      THIS-OBJECT:splitContainer3:Size = NEW System.Drawing.Size(778, 228).
      THIS-OBJECT:splitContainer3:SplitterDistance = 30.
      THIS-OBJECT:splitContainer3:TabIndex = 0.
      /*  */
      /* buttonOverAllaPriser */
      /*  */
      THIS-OBJECT:buttonOverAllaPriser:Location = NEW System.Drawing.Point(2, 3).
      THIS-OBJECT:buttonOverAllaPriser:Name = "buttonOverAllaPriser".
      THIS-OBJECT:buttonOverAllaPriser:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverAllaPriser:TabIndex = 14.
      THIS-OBJECT:buttonOverAllaPriser:Text = "Alla väljs".
      THIS-OBJECT:buttonOverAllaPriser:Click:Subscribe(THIS-OBJECT:buttonOverAllaPriser_Click).
      /*  */
      /* buttonBackAllaPriser */
      /*  */
      THIS-OBJECT:buttonBackAllaPriser:Location = NEW System.Drawing.Point(3, 96).
      THIS-OBJECT:buttonBackAllaPriser:Name = "buttonBackAllaPriser".
      THIS-OBJECT:buttonBackAllaPriser:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackAllaPriser:TabIndex = 16.
      THIS-OBJECT:buttonBackAllaPriser:Text = "Alla bort".
      THIS-OBJECT:buttonBackAllaPriser:Click:Subscribe(THIS-OBJECT:buttonBackAllaPriser_Click).
      /*  */
      /* buttonOverMarkPriser */
      /*  */
      THIS-OBJECT:buttonOverMarkPriser:Location = NEW System.Drawing.Point(2, 34).
      THIS-OBJECT:buttonOverMarkPriser:Name = "buttonOverMarkPriser".
      THIS-OBJECT:buttonOverMarkPriser:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverMarkPriser:TabIndex = 13.
      THIS-OBJECT:buttonOverMarkPriser:Text = "Markerade väljs".
      THIS-OBJECT:buttonOverMarkPriser:Click:Subscribe(THIS-OBJECT:buttonOverMarkPriser_Click).
      /*  */
      /* buttonBackMarkPriser */
      /*  */
      THIS-OBJECT:buttonBackMarkPriser:Location = NEW System.Drawing.Point(3, 65).
      THIS-OBJECT:buttonBackMarkPriser:Name = "buttonBackMarkPriser".
      THIS-OBJECT:buttonBackMarkPriser:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonBackMarkPriser:TabIndex = 15.
      THIS-OBJECT:buttonBackMarkPriser:Text = "Markerade bort".
      THIS-OBJECT:buttonBackMarkPriser:Click:Subscribe(THIS-OBJECT:buttonBackMarkPriser_Click).
      /*  */
      /* valdaprisposter */
      /*  */
      appearance366:BackColor = System.Drawing.SystemColors:Window.
      appearance366:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Appearance = appearance366.
      THIS-OBJECT:valdaprisposter:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:valdaprisposter:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance367:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance367:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance367:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance367:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdaprisposter:DisplayLayout:GroupByBox:Appearance = appearance367.
      appearance368:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valdaprisposter:DisplayLayout:GroupByBox:BandLabelAppearance = appearance368.
      THIS-OBJECT:valdaprisposter:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance369:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance369:BackColor2 = System.Drawing.SystemColors:Control.
      appearance369:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance369:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:valdaprisposter:DisplayLayout:GroupByBox:PromptAppearance = appearance369.
      THIS-OBJECT:valdaprisposter:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:valdaprisposter:DisplayLayout:MaxRowScrollRegions = 1.
      appearance370:BackColor = System.Drawing.SystemColors:Window.
      appearance370:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:ActiveCellAppearance = appearance370.
      appearance371:BackColor = System.Drawing.SystemColors:Highlight.
      appearance371:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:ActiveRowAppearance = appearance371.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance372:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:CardAreaAppearance = appearance372.
      appearance373:BorderColor = System.Drawing.Color:Silver.
      appearance373:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:CellAppearance = appearance373.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:CellPadding = 0.
      appearance374:BackColor = System.Drawing.SystemColors:Control.
      appearance374:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance374:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance374:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance374:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:GroupByRowAppearance = appearance374.
      appearance375:TextHAlignAsString = "Left".
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:HeaderAppearance = appearance375.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance376:BackColor = System.Drawing.SystemColors:Window.
      appearance376:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:RowAppearance = appearance376.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance377:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:valdaprisposter:DisplayLayout:Override:TemplateAddRowAppearance = appearance377.
      THIS-OBJECT:valdaprisposter:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:valdaprisposter:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:valdaprisposter:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:valdaprisposter:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:valdaprisposter:GuruContext = ?.
      THIS-OBJECT:valdaprisposter:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:valdaprisposter:Name = "valdaprisposter".
      THIS-OBJECT:valdaprisposter:Size = NEW System.Drawing.Size(744, 228).
      THIS-OBJECT:valdaprisposter:TabIndex = 11.
      THIS-OBJECT:valdaprisposter:Text = "grid1".
      /*  */
      /* ultraTabPageControl11 */
      /*  */
      THIS-OBJECT:ultraTabPageControl11:Controls:Add(THIS-OBJECT:VisaDividerX).
      THIS-OBJECT:ultraTabPageControl11:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl11:Name = "ultraTabPageControl11".
      THIS-OBJECT:ultraTabPageControl11:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* VisaDividerX */
      /*  */
      THIS-OBJECT:VisaDividerX:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaDividerX:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:VisaDividerX:IsSplitterFixed = TRUE.
      THIS-OBJECT:VisaDividerX:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaDividerX:Margin = NEW System.Windows.Forms.Padding(0).
      THIS-OBJECT:VisaDividerX:Name = "VisaDividerX".
      /*  */
      /* VisaDividerX.Panel1 */
      /*  */
      THIS-OBJECT:VisaDividerX:Panel1:Controls:Add(THIS-OBJECT:ultraTabControl2).
      /*  */
      /* VisaDividerX.Panel2 */
      /*  */
      THIS-OBJECT:VisaDividerX:Panel2:Controls:Add(THIS-OBJECT:VisaPanelSettings).
      THIS-OBJECT:VisaDividerX:Size = NEW System.Drawing.Size(1173, 463).
      THIS-OBJECT:VisaDividerX:SplitterDistance = 1023.
      THIS-OBJECT:VisaDividerX:SplitterWidth = 1.
      THIS-OBJECT:VisaDividerX:TabIndex = 1.
      /*  */
      /* ultraTabControl2 */
      /*  */
      THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage3).
      THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:VisaExcelTab).
      THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabPageControl16).
      THIS-OBJECT:ultraTabControl2:Controls:Add(THIS-OBJECT:ultraTabPageControl17).
      THIS-OBJECT:ultraTabControl2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ultraTabControl2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ultraTabControl2:Name = "ultraTabControl2".
      THIS-OBJECT:ultraTabControl2:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage3.
      THIS-OBJECT:ultraTabControl2:Size = NEW System.Drawing.Size(1023, 463).
      THIS-OBJECT:ultraTabControl2:TabIndex = 0.
      ultraTab6:TabPage = THIS-OBJECT:VisaExcelTab.
      ultraTab6:Text = "Excel".
      ultraTab7:TabPage = THIS-OBJECT:ultraTabPageControl16.
      ultraTab7:Text = "Internet Explorer".
      ultraTab8:TabPage = THIS-OBJECT:ultraTabPageControl17.
      ultraTab8:Text = "PDF".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar1[1] = ultraTab6.
      arrayvar1[2] = ultraTab7.
      arrayvar1[3] = ultraTab8.
      THIS-OBJECT:ultraTabControl2:Tabs:AddRange(arrayvar1).
      /*  */
      /* ultraTabSharedControlsPage3 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage3:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage3:Name = "ultraTabSharedControlsPage3".
      THIS-OBJECT:ultraTabSharedControlsPage3:Size = NEW System.Drawing.Size(1019, 437).
      /*  */
      /* VisaPanelSettings */
      /*  */
      appearance19:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(241)), System.Convert:ToInt32(System.Convert:ToByte(237)), System.Convert:ToInt32(System.Convert:ToByte(237))).
      appearance19:BackColorAlpha = Infragistics.Win.Alpha:Opaque.
      THIS-OBJECT:VisaPanelSettings:Appearance = appearance19.
      /*  */
      /* VisaPanelSettings.ClientArea */
      /*  */
      THIS-OBJECT:VisaPanelSettings:ClientArea:Controls:Add(THIS-OBJECT:VisaReset).
      THIS-OBJECT:VisaPanelSettings:ClientArea:Controls:Add(THIS-OBJECT:VisaGroupBoxTyp).
      THIS-OBJECT:VisaPanelSettings:ClientArea:Controls:Add(THIS-OBJECT:VisaGenerate).
      THIS-OBJECT:VisaPanelSettings:ClientArea:Controls:Add(THIS-OBJECT:VisaInklGroupBox).
      THIS-OBJECT:VisaPanelSettings:ClientArea:Controls:Add(THIS-OBJECT:ultraGroupBox1).
      THIS-OBJECT:VisaPanelSettings:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaPanelSettings:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPanelSettings:Name = "VisaPanelSettings".
      THIS-OBJECT:VisaPanelSettings:Size = NEW System.Drawing.Size(149, 463).
      THIS-OBJECT:VisaPanelSettings:TabIndex = 0.
      THIS-OBJECT:VisaPanelSettings:UseFlatMode = Infragistics.Win.DefaultableBoolean:False.
      /*  */
      /* VisaReset */
      /*  */
      THIS-OBJECT:VisaReset:Location = NEW System.Drawing.Point(3, 290).
      THIS-OBJECT:VisaReset:Name = "VisaReset".
      THIS-OBJECT:VisaReset:Size = NEW System.Drawing.Size(84, 23).
      THIS-OBJECT:VisaReset:TabIndex = 9.
      THIS-OBJECT:VisaReset:Text = "Återställ".
      THIS-OBJECT:VisaReset:Click:Subscribe(THIS-OBJECT:VisaReset_Click).
      /*  */
      /* VisaGroupBoxTyp */
      /*  */
      THIS-OBJECT:VisaGroupBoxTyp:Controls:Add(THIS-OBJECT:VisaKalkylTyp).
      THIS-OBJECT:VisaGroupBoxTyp:Location = NEW System.Drawing.Point(3, 7).
      THIS-OBJECT:VisaGroupBoxTyp:Name = "VisaGroupBoxTyp".
      THIS-OBJECT:VisaGroupBoxTyp:Size = NEW System.Drawing.Size(180, 44).
      THIS-OBJECT:VisaGroupBoxTyp:TabIndex = 2.
      THIS-OBJECT:VisaGroupBoxTyp:Text = "Kalkyltyp".
      /*  */
      /* VisaKalkylTyp */
      /*  */
      THIS-OBJECT:VisaKalkylTyp:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
      THIS-OBJECT:VisaKalkylTyp:Dock = System.Windows.Forms.DockStyle:Fill.
      valueListItem6:DataValue = System.Convert:ToInt16(0).
      valueListItem6:DisplayText = "Grund".
      valueListItem7:DataValue = System.Convert:ToInt16(1).
      valueListItem7:DisplayText = "Offert".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = valueListItem6.
      arrayvar2[2] = valueListItem7.
      THIS-OBJECT:VisaKalkylTyp:Items:AddRange(arrayvar2).
      THIS-OBJECT:VisaKalkylTyp:ItemSpacingHorizontal = 16.
      THIS-OBJECT:VisaKalkylTyp:ItemSpacingVertical = 8.
      THIS-OBJECT:VisaKalkylTyp:Location = NEW System.Drawing.Point(3, 16).
      THIS-OBJECT:VisaKalkylTyp:Margin = NEW System.Windows.Forms.Padding(16, 8, 8, 8).
      THIS-OBJECT:VisaKalkylTyp:Name = "VisaKalkylTyp".
      THIS-OBJECT:VisaKalkylTyp:RightToLeft = System.Windows.Forms.RightToLeft:No.
      THIS-OBJECT:VisaKalkylTyp:Size = NEW System.Drawing.Size(174, 25).
      THIS-OBJECT:VisaKalkylTyp:TabIndex = 1.
      THIS-OBJECT:VisaKalkylTyp:TextIndentation = 4.
      THIS-OBJECT:VisaKalkylTyp:UseFlatMode = Infragistics.Win.DefaultableBoolean:True.
      /*  */
      /* VisaGenerate */
      /*  */
      THIS-OBJECT:VisaGenerate:Location = NEW System.Drawing.Point(93, 290).
      THIS-OBJECT:VisaGenerate:Name = "VisaGenerate".
      THIS-OBJECT:VisaGenerate:Size = NEW System.Drawing.Size(87, 23).
      THIS-OBJECT:VisaGenerate:TabIndex = 8.
      THIS-OBJECT:VisaGenerate:Text = "Generera".
      THIS-OBJECT:VisaGenerate:Click:Subscribe(THIS-OBJECT:VisaGenerate_Click).
      /*  */
      /* VisaInklGroupBox */
      /*  */
      THIS-OBJECT:VisaInklGroupBox:Controls:Add(THIS-OBJECT:VisaInklOFakt).
      THIS-OBJECT:VisaInklGroupBox:Controls:Add(THIS-OBJECT:VisaInklOEP).
      THIS-OBJECT:VisaInklGroupBox:Controls:Add(THIS-OBJECT:VisaInklKAnm).
      THIS-OBJECT:VisaInklGroupBox:Controls:Add(THIS-OBJECT:VisaInklAnm).
      THIS-OBJECT:VisaInklGroupBox:Controls:Add(THIS-OBJECT:VisaInklHuvud).
      THIS-OBJECT:VisaInklGroupBox:Location = NEW System.Drawing.Point(5, 57).
      THIS-OBJECT:VisaInklGroupBox:Name = "VisaInklGroupBox".
      THIS-OBJECT:VisaInklGroupBox:Size = NEW System.Drawing.Size(177, 148).
      THIS-OBJECT:VisaInklGroupBox:TabIndex = 3.
      THIS-OBJECT:VisaInklGroupBox:Text = "Inkludera".
      /*  */
      /* VisaInklOFakt */
      /*  */
      THIS-OBJECT:VisaInklOFakt:Location = NEW System.Drawing.Point(6, 123).
      THIS-OBJECT:VisaInklOFakt:Name = "VisaInklOFakt".
      THIS-OBJECT:VisaInklOFakt:Size = NEW System.Drawing.Size(120, 20).
      THIS-OBJECT:VisaInklOFakt:TabIndex = 4.
      THIS-OBJECT:VisaInklOFakt:Text = "Faktorer".
      /*  */
      /* VisaInklOEP */
      /*  */
      THIS-OBJECT:VisaInklOEP:Location = NEW System.Drawing.Point(6, 97).
      THIS-OBJECT:VisaInklOEP:Name = "VisaInklOEP".
      THIS-OBJECT:VisaInklOEP:Size = NEW System.Drawing.Size(120, 20).
      THIS-OBJECT:VisaInklOEP:TabIndex = 3.
      THIS-OBJECT:VisaInklOEP:Text = "Egna Priser".
      /*  */
      /* VisaInklKAnm */
      /*  */
      THIS-OBJECT:VisaInklKAnm:Location = NEW System.Drawing.Point(6, 71).
      THIS-OBJECT:VisaInklKAnm:Name = "VisaInklKAnm".
      THIS-OBJECT:VisaInklKAnm:Size = NEW System.Drawing.Size(120, 20).
      THIS-OBJECT:VisaInklKAnm:TabIndex = 2.
      THIS-OBJECT:VisaInklKAnm:Text = "Kodanmärkningar".
      /*  */
      /* VisaInklAnm */
      /*  */
      THIS-OBJECT:VisaInklAnm:Location = NEW System.Drawing.Point(6, 45).
      THIS-OBJECT:VisaInklAnm:Name = "VisaInklAnm".
      THIS-OBJECT:VisaInklAnm:Size = NEW System.Drawing.Size(120, 20).
      THIS-OBJECT:VisaInklAnm:TabIndex = 1.
      THIS-OBJECT:VisaInklAnm:Text = "Kalkylanmärkning".
      /*  */
      /* VisaInklHuvud */
      /*  */
      THIS-OBJECT:VisaInklHuvud:Location = NEW System.Drawing.Point(6, 19).
      THIS-OBJECT:VisaInklHuvud:Name = "VisaInklHuvud".
      THIS-OBJECT:VisaInklHuvud:Size = NEW System.Drawing.Size(120, 20).
      THIS-OBJECT:VisaInklHuvud:TabIndex = 0.
      THIS-OBJECT:VisaInklHuvud:Text = "Kalkylhuvud".
      /*  */
      /* ultraGroupBox1 */
      /*  */
      THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:VisaFunkGrupp).
      THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:VisaFunkSum).
      THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(5, 209).
      THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1".
      THIS-OBJECT:ultraGroupBox1:Size = NEW System.Drawing.Size(177, 75).
      THIS-OBJECT:ultraGroupBox1:TabIndex = 7.
      THIS-OBJECT:ultraGroupBox1:Text = "Funktioner".
      /*  */
      /* VisaFunkGrupp */
      /*  */
      THIS-OBJECT:VisaFunkGrupp:Location = NEW System.Drawing.Point(6, 19).
      THIS-OBJECT:VisaFunkGrupp:Name = "VisaFunkGrupp".
      THIS-OBJECT:VisaFunkGrupp:Size = NEW System.Drawing.Size(165, 20).
      THIS-OBJECT:VisaFunkGrupp:TabIndex = 5.
      THIS-OBJECT:VisaFunkGrupp:Text = "Gruppera Arbetskoder".
      /*  */
      /* VisaFunkSum */
      /*  */
      THIS-OBJECT:VisaFunkSum:Location = NEW System.Drawing.Point(6, 45).
      THIS-OBJECT:VisaFunkSum:Name = "VisaFunkSum".
      THIS-OBJECT:VisaFunkSum:Size = NEW System.Drawing.Size(165, 20).
      THIS-OBJECT:VisaFunkSum:TabIndex = 6.
      THIS-OBJECT:VisaFunkSum:Text = "Summera koder vid visning".
      /*  */
      /* ultraTabPageControl2 */
      /*  */
      THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:egnafaktorer).
      THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2".
      THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(1173, 463).
      /*  */
      /* egnafaktorer */
      /*  */
      appearance390:BackColor = System.Drawing.SystemColors:Window.
      appearance390:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Appearance = appearance390.
      THIS-OBJECT:egnafaktorer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:egnafaktorer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance391:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance391:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance391:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance391:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnafaktorer:DisplayLayout:GroupByBox:Appearance = appearance391.
      appearance392:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnafaktorer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance392.
      THIS-OBJECT:egnafaktorer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance393:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance393:BackColor2 = System.Drawing.SystemColors:Control.
      appearance393:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance393:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:egnafaktorer:DisplayLayout:GroupByBox:PromptAppearance = appearance393.
      THIS-OBJECT:egnafaktorer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:egnafaktorer:DisplayLayout:MaxRowScrollRegions = 1.
      appearance394:BackColor = System.Drawing.SystemColors:Window.
      appearance394:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:ActiveCellAppearance = appearance394.
      appearance395:BackColor = System.Drawing.SystemColors:Highlight.
      appearance395:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:ActiveRowAppearance = appearance395.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance396:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:CardAreaAppearance = appearance396.
      appearance397:BorderColor = System.Drawing.Color:Silver.
      appearance397:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:CellAppearance = appearance397.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:CellPadding = 0.
      appearance398:BackColor = System.Drawing.SystemColors:Control.
      appearance398:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance398:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance398:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance398:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:GroupByRowAppearance = appearance398.
      appearance399:TextHAlignAsString = "Left".
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:HeaderAppearance = appearance399.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance400:BackColor = System.Drawing.SystemColors:Window.
      appearance400:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:RowAppearance = appearance400.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance401:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:egnafaktorer:DisplayLayout:Override:TemplateAddRowAppearance = appearance401.
      THIS-OBJECT:egnafaktorer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:egnafaktorer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:egnafaktorer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:egnafaktorer:GuruContext = ?.
      THIS-OBJECT:egnafaktorer:Location = NEW System.Drawing.Point(41, 43).
      THIS-OBJECT:egnafaktorer:Name = "egnafaktorer".
      THIS-OBJECT:egnafaktorer:Size = NEW System.Drawing.Size(550, 368).
      THIS-OBJECT:egnafaktorer:TabIndex = 0.
      THIS-OBJECT:egnafaktorer:Text = "grid1".
      /*  */
      /* ultraTabPageControl3 */
      /*  */
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