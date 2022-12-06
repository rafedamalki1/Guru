 
 /*------------------------------------------------------------------------
    File        : EkgP2Kom
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Nov 18 12:55:16 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.EkgData.EkgP2Kom INHERITS UserControl: 
	{EKGTH.I}
	DEFINE VARIABLE arblopkomval AS INTEGER NO-UNDO.
   DEFINE PRIVATE VARIABLE ArbLopStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonDownKom AS Controls.ButtonUpDown NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonDownKopp AS Controls.ButtonUpDown NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverArbKomDirekt AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverKomBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverKom AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverArbKomBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverArbKom AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonUpKom AS Controls.ButtonUpDown NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonUpKopp AS Controls.ButtonUpDown NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKomVal AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridP2KomResult AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridSvepKomKoll AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE KomLop AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE KomBefSvep AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE KomArbkod AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE p2KomStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillP2kom AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE SkapaNyttSvep AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE SammaSomSvep AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE SammaSomArbkod AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerArbLopKom AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerGrundKom AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerAllt AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKomTillFran AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKomTillBort AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKopplaArb AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKoppladeKom AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKom AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridArbLop AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerKoppladeSvep AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResultatAter AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerValdaKom AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerVisaKontroll AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortP2kom AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonAter AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonP2KomKoll AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonReadOnly AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonVisa AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButtonVisaResult AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripComboBox1 AS System.Windows.Forms.ToolStripComboBox NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripComboBox2 AS System.Windows.Forms.ToolStripComboBox NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripDropDownButtonSvep AS System.Windows.Forms.ToolStripDropDownButton NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripTextArbLop AS System.Windows.Forms.ToolStripTextBox NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripVisa AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE UtokaBefSvep AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
   
      	
	CONSTRUCTOR PUBLIC EkgP2Kom ( INPUT r AS Guru.Root ):
		 SUPER().
		THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      {EKGDBH.i ControlDataDb}
		THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
		THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.



	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonDownKom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		  THIS-OBJECT:buttonDownKom:GuruUpDown(THIS-OBJECT:GridKomVal:tBS:Count,"").
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonDownKopp_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
	  IF THIS-OBJECT:buttonDownKopp:RadFromGrid() = TRUE THEN DO:
		   IF THIS-OBJECT:KollUpDown(FALSE) = TRUE THEN DO: 
   		   villkor = " P2ARBKOD = '" + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND SVEPLOPNR = '" + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE + "'".		   
            
            THIS-OBJECT:buttonDownKopp:GuruUpDown(0,villkor).
            THIS-OBJECT:ControlWinLg:AndringP2kom().
         END.    
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverArbKom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		/*VILKEN TYP AV KOM*/
		IF arblopkomval = 0 OR arblopkomval = 1 OR arblopkomval = 2 OR arblopkomval = 4 THEN DO:
         /*NÅGRA VALDA?*/
         IF THIS-OBJECT:GridKomVal:RadInGrid() = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(176), "").
            RETURN. 
         END.   
		END.
		
		IF THIS-OBJECT:buttonOverArbKom:RadFromGrid() = TRUE THEN DO:
		   IF THIS-OBJECT:SkapaKom() = TRUE THEN THIS-OBJECT:buttonOverArbKom:GuruOverefteruppdat().
		   ELSE DO:
		      THIS-OBJECT:GridKoppladeKom:GuruReopen().
		   END.   
		   THIS-OBJECT:ControlWinLg:AndringP2kom().
		   eekgP2ValkomTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         eekgKoppArbSvepTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
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
	METHOD PRIVATE VOID buttonOverArbKomBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		DEFINE VARIABLE varp2arb AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2lop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2ordning AS INTEGER NO-UNDO.
		IF THIS-OBJECT:buttonOverArbKomBort:RadFromGrid() = TRUE THEN DO:
		   varp2arb = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["P2ARBKOD"]:VALUE. 
         varp2lop = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["SVEPLOPNR"]:VALUE. 
         THIS-OBJECT:buttonOverArbKomBort:GuruOver().
         THIS-OBJECT:NyOrdning(varp2arb,varp2lop).
         THIS-OBJECT:buttonOverArbKomBort:GuruOverefteruppdat().
         THIS-OBJECT:ControlWinLg:AndringP2kom(). 
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").
      END.
		RETURN.

	END METHOD.
   METHOD PUBLIC VOID NyOrdning(INPUT varp2arb AS CHARACTER, varp2lop AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2ordning AS INTEGER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE P2ARBKOD = '" + varp2arb + "'AND SVEPLOPNR = '" + varp2lop + "' BY  ORDNING ".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
      qH:GET-FIRST().
      varp2ordning = 0.
      DO WHILE qH:QUERY-OFF-END = FALSE:
         varp2ordning = varp2ordning + 1.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = varp2ordning.
         qH:GET-NEXT().
      END. 
      THIS-OBJECT:GridKoppladeKom:GuruReopen().
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonOverArbKomDirekt_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		IF arblopkomval = 0 OR arblopkomval = 1 OR arblopkomval = 2 OR arblopkomval = 4 THEN DO:
         IF THIS-OBJECT:GridKomVal:RadInGrid() = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(176), "").
            RETURN. 
         END.   
      END.
		IF THIS-OBJECT:buttonOverArbKomDirekt:RadFromGrid() = TRUE THEN DO:
         IF THIS-OBJECT:SkapaKom() = TRUE THEN THIS-OBJECT:buttonOverArbKomDirekt:GuruOverefteruppdat().
         ELSE DO:
            THIS-OBJECT:GridKoppladeKom:GuruReopen().
         END.   
         THIS-OBJECT:ControlWinLg:AndringP2kom(). 
         eekgP2ValkomTTh:EMPTY-TEMP-TABLE() NO-ERROR.
         eekgKoppArbSvepTTh:EMPTY-TEMP-TABLE() NO-ERROR.
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
	METHOD PRIVATE VOID buttonOverKom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		DEFINE VARIABLE ordvar  AS INTEGER NO-UNDO. 
		DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
		
		IF THIS-OBJECT:buttonOverKom:RadFromGrid() = TRUE THEN DO:
         THIS-OBJECT:buttonOverKom:GuruOver().
         THIS-OBJECT:ekgP2ValkomTTh:FIND-LAST() NO-ERROR.
         IF THIS-OBJECT:ekgP2ValkomTTh:AVAILABLE THEN ordvar = THIS-OBJECT:ekgP2ValkomTTh:BUFFER-FIELD("OORDNING"):BUFFER-VALUE.
         ELSE ordvar = 0.
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgP2ValkomTTh:TABLE + " WHERE OORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2ValkomTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            ordvar = ordvar + 1.
            THIS-OBJECT:ekgP2ValkomTTh:BUFFER-FIELD("OORDNING"):BUFFER-VALUE = ordvar.
            qH:GET-NEXT().         
         END.
         
         THIS-OBJECT:buttonOverKom:GuruOverefteruppdat().
         
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
	METHOD PRIVATE VOID buttonOverKomBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2ordning AS INTEGER NO-UNDO.
		IF THIS-OBJECT:buttonOverKomBort:RadFromGrid() = TRUE THEN DO:
		   THIS-OBJECT:buttonOverKomBort:GuruOver().
		   queryvar =  "FOR EACH " + THIS-OBJECT:ekgP2ValkomTTh:TABLE + " BY  OORDNING ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2ValkomTTh,queryvar).
         qH:GET-FIRST().
         varp2ordning = 0.
         DO WHILE qH:QUERY-OFF-END = FALSE:
            varp2ordning = varp2ordning + 1.
            THIS-OBJECT:ekgP2ValkomTTh:BUFFER-FIELD("OORDNING"):BUFFER-VALUE = varp2ordning.
            qH:GET-NEXT().
         END. 
         THIS-OBJECT:GridKomVal:GuruReopen().
		   THIS-OBJECT:buttonOverKomBort:GuruOverefteruppdat().
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
	METHOD PRIVATE VOID buttonUpKom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:buttonUpKom:RadFromGrid() = TRUE THEN DO:
		   THIS-OBJECT:buttonUpKom:GuruUpDown(THIS-OBJECT:GridKomVal:tBS:Count,""). 
		END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonUpKopp_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE villkor AS CHARACTER NO-UNDO.
      
		IF THIS-OBJECT:buttonUpKopp:RadFromGrid() = TRUE THEN DO:
         IF THIS-OBJECT:KollUpDown(TRUE) = TRUE THEN DO: 
            villkor = " P2ARBKOD = '" + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "' AND SVEPLOPNR = '" + 
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE + "'".
            THIS-OBJECT:buttonUpKopp:GuruUpDown(0,villkor).
            THIS-OBJECT:ControlWinLg:AndringP2kom().  
         END.
      END.
		RETURN.

	END METHOD.
   METHOD PUBLIC LOGICAL KollUpDown(INPUT updown AS LOGICAL):
      DEFINE VARIABLE komidvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE rid AS RECID NO-UNDO.
      DEFINE VARIABLE akod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE svnr AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE komidvartopp AS INTEGER NO-UNDO.
      DEFINE VARIABLE ridtopp AS RECID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN.
      ELSE RETURN FALSE. 
      komidvar = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE.
      rid = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
      akod = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE.
      svnr = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE.
      /*UPP*/
      IF updown = TRUE THEN DO:
         IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 2 THEN DO:
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE ORDNING = 1 AND P2ARBKOD = '" + akod + "' AND SVEPLOPNR = '" + svnr + "'") NO-ERROR.
            komidvartopp = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE.
            ridtopp = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE KOMID = " + STRING(komidvartopp) + 
        /*    " AND P2ARBKOD = '" + akod + "' AND SVEPLOPNR = '" + svnr + "' AND TTRECID NE " + STRING(ridtopp)) NO-ERROR.*/
            " AND P2ARBKOD = '" + akod + "' AND SVEPLOPNR = '" + svnr + "' AND ORDNING NE " + STRING(1)) NO-ERROR.
            IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(184), "").
               THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE TTRECID = " + STRING(rid)) NO-ERROR.
               RETURN FALSE.
            END.
         END.
      END.
      ELSE DO:
         IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 1 THEN DO:
          
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE KOMID = " + STRING(komidvar) + 
           /*
            " AND P2ARBKOD = '" + akod + "' AND SVEPLOPNR = '" + svnr + "' AND TTRECID NE " + STRING(rid)) NO-ERROR.
            */
            " AND P2ARBKOD = '" + akod + "' AND SVEPLOPNR = '" + svnr + "' AND ORDNING NE " + STRING(1)) NO-ERROR.
            IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
               rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(184), "").
               THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE TTRECID = " + STRING(rid)) NO-ERROR.
               
               RETURN FALSE.
            END.
         END.
      END.      
      THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE TTRECID = " + STRING(rid)) NO-ERROR.
      RETURN TRUE.
      
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKom_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.   
      THIS-OBJECT:ControlWinLg:AndringP2kom().
      THIS-OBJECT:ControlDataDb:AnmarkFix(THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE).
      THIS-OBJECT:GridKoppladeKom:GuruReopen(). 
      THIS-OBJECT:GridKomVal:GuruReopen().       
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKoppladeKom_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF kolvarde NE "" THEN kolvarde = "X".
      /*Anders Olsson Elpool i Umeå AB  8 mar 2016 17:07:58 
      Första raden på ett svep är en rubrikrad och kan inte ha ingår ej 
      */
      IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 1 THEN DO:
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = "".
      END.
      ELSE THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = CAPS(kolvarde).
     
      THIS-OBJECT:ControlWinLg:AndringP2kom(). 
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgP2Kom").
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
      THIS-OBJECT:GridKom = NEW Controls.Grid().
      THIS-OBJECT:buttonDownKom = NEW Controls.ButtonUpDown().
      THIS-OBJECT:buttonUpKom = NEW Controls.ButtonUpDown().
      THIS-OBJECT:buttonOverKomBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverKom = NEW Controls.ButtonOver().
      THIS-OBJECT:GridKomVal = NEW Controls.Grid().
      THIS-OBJECT:GridArbLop = NEW Controls.Grid().
      THIS-OBJECT:buttonDownKopp = NEW Controls.ButtonUpDown().
      THIS-OBJECT:buttonUpKopp = NEW Controls.ButtonUpDown().
      THIS-OBJECT:buttonOverArbKomDirekt = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverArbKomBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverArbKom = NEW Controls.ButtonOver().
      THIS-OBJECT:GridKoppladeKom = NEW Controls.Grid().
      THIS-OBJECT:GridP2KomResult = NEW Controls.Grid().
      THIS-OBJECT:p2KomStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillP2kom = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortP2kom = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonReadOnly = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ArbLopStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripDropDownButtonSvep = NEW System.Windows.Forms.ToolStripDropDownButton().
      THIS-OBJECT:KomLop = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:KomArbkod = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:SkapaNyttSvep = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:UtokaBefSvep = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:KomBefSvep = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:SammaSomSvep = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:SammaSomArbkod = NEW System.Windows.Forms.ToolStripMenuItem().
      THIS-OBJECT:toolStripTextArbLop = NEW System.Windows.Forms.ToolStripTextBox().
      THIS-OBJECT:toolStripButtonVisa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButtonP2KomKoll = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripVisa = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonAter = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripComboBox1 = NEW System.Windows.Forms.ToolStripComboBox().
      THIS-OBJECT:toolStripComboBox2 = NEW System.Windows.Forms.ToolStripComboBox().
      THIS-OBJECT:splitContainerAllt = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerGrundKom = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKomTillBort = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKomTillFran = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerValdaKom = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerArbLopKom = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKopplaArb = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKoppladeSvep = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerResultatAter = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerVisaKontroll = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridSvepKomKoll = NEW Controls.Grid().
      CAST(THIS-OBJECT:GridKom, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridKomVal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridArbLop, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridKoppladeKom, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridP2KomResult, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:p2KomStrip:SuspendLayout().
      THIS-OBJECT:ArbLopStrip:SuspendLayout().
      THIS-OBJECT:toolStripVisa:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAllt, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAllt:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAllt:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAllt:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerGrundKom, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerGrundKom:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKom:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKom:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKomTillBort, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKomTillBort:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKomTillBort:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKomTillBort:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKomTillFran, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKomTillFran:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKomTillFran:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKomTillFran:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdaKom, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdaKom:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdaKom:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerValdaKom:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerArbLopKom, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerArbLopKom:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerArbLopKom:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerArbLopKom:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKopplaArb, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKopplaArb:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKopplaArb:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKopplaArb:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerKoppladeSvep, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKoppladeSvep:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKoppladeSvep:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResultatAter, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResultatAter:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResultatAter:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResultatAter:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerVisaKontroll, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVisaKontroll:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVisaKontroll:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVisaKontroll:SuspendLayout().
      CAST(THIS-OBJECT:GridSvepKomKoll, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* GridKom */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKom:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridKom:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKom:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKom:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKom:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridKom:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKom:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridKom:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKom:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKom:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKom:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKom:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridKom:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKom:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKom:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKom:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridKom:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKom:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKom:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKom:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridKom:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKom:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKom:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridKom:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKom:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridKom:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKom:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKom:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKom:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKom:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKom:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKom:GuruContext = ?.
      THIS-OBJECT:GridKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKom:Name = "GridKom".
      THIS-OBJECT:GridKom:Size = NEW System.Drawing.Size(220, 556).
      THIS-OBJECT:GridKom:TabIndex = 0.
      THIS-OBJECT:GridKom:Text = "grid1".
      THIS-OBJECT:GridKom:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKom_AfterCellUpdate).
      /*  */
      /* buttonDownKom */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:buttonDownKom:Appearance = appearance13.
      THIS-OBJECT:buttonDownKom:Location = NEW System.Drawing.Point(3, 62).
      THIS-OBJECT:buttonDownKom:Name = "buttonDownKom".
      THIS-OBJECT:buttonDownKom:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonDownKom:TabIndex = 5.
      THIS-OBJECT:buttonDownKom:Text = "buttonUpDown1".
      THIS-OBJECT:buttonDownKom:Click:Subscribe(THIS-OBJECT:buttonDownKom_Click).
      /*  */
      /* buttonUpKom */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:buttonUpKom:Appearance = appearance14.
      THIS-OBJECT:buttonUpKom:Location = NEW System.Drawing.Point(3, 31).
      THIS-OBJECT:buttonUpKom:Name = "buttonUpKom".
      THIS-OBJECT:buttonUpKom:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonUpKom:TabIndex = 4.
      THIS-OBJECT:buttonUpKom:Text = "buttonUpDown1".
      THIS-OBJECT:buttonUpKom:Click:Subscribe(THIS-OBJECT:buttonUpKom_Click).
      /*  */
      /* buttonOverKomBort */
      /*  */
      appearance15:Image = CAST(resources:GetObject("appearance15.Image"), System.Object).
      THIS-OBJECT:buttonOverKomBort:Appearance = appearance15.
      THIS-OBJECT:buttonOverKomBort:Location = NEW System.Drawing.Point(3, 124).
      THIS-OBJECT:buttonOverKomBort:Name = "buttonOverKomBort".
      THIS-OBJECT:buttonOverKomBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverKomBort:TabIndex = 3.
      THIS-OBJECT:buttonOverKomBort:Click:Subscribe(THIS-OBJECT:buttonOverKomBort_Click).
      /*  */
      /* buttonOverKom */
      /*  */
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      THIS-OBJECT:buttonOverKom:Appearance = appearance16.
      THIS-OBJECT:buttonOverKom:Location = NEW System.Drawing.Point(3, 93).
      THIS-OBJECT:buttonOverKom:Name = "buttonOverKom".
      THIS-OBJECT:buttonOverKom:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverKom:TabIndex = 2.
      THIS-OBJECT:buttonOverKom:Click:Subscribe(THIS-OBJECT:buttonOverKom_Click).
      /*  */
      /* GridKomVal */
      /*  */
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKomVal:DisplayLayout:Appearance = appearance17.
      THIS-OBJECT:GridKomVal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKomVal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance18:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance18:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance18:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKomVal:DisplayLayout:GroupByBox:Appearance = appearance18.
      appearance19:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKomVal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance19.
      THIS-OBJECT:GridKomVal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance20:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance20:BackColor2 = System.Drawing.SystemColors:Control.
      appearance20:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance20:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKomVal:DisplayLayout:GroupByBox:PromptAppearance = appearance20.
      THIS-OBJECT:GridKomVal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKomVal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      appearance21:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:ActiveCellAppearance = appearance21.
      appearance22:BackColor = System.Drawing.SystemColors:Highlight.
      appearance22:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:ActiveRowAppearance = appearance22.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:CardAreaAppearance = appearance23.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      appearance24:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:CellAppearance = appearance24.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:CellPadding = 0.
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance25:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:GroupByRowAppearance = appearance25.
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:HeaderAppearance = appearance26.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:RowAppearance = appearance27.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKomVal:DisplayLayout:Override:TemplateAddRowAppearance = appearance28.
      THIS-OBJECT:GridKomVal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKomVal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKomVal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKomVal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKomVal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKomVal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKomVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKomVal:GuruContext = ?.
      THIS-OBJECT:GridKomVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKomVal:Name = "GridKomVal".
      THIS-OBJECT:GridKomVal:Size = NEW System.Drawing.Size(232, 556).
      THIS-OBJECT:GridKomVal:TabIndex = 0.
      THIS-OBJECT:GridKomVal:Text = "grid1".
      /*  */
      /* GridArbLop */
      /*  */
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbLop:DisplayLayout:Appearance = appearance29.
      THIS-OBJECT:GridArbLop:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbLop:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance30:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance30:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance30:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbLop:DisplayLayout:GroupByBox:Appearance = appearance30.
      appearance31:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbLop:DisplayLayout:GroupByBox:BandLabelAppearance = appearance31.
      THIS-OBJECT:GridArbLop:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance32:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance32:BackColor2 = System.Drawing.SystemColors:Control.
      appearance32:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbLop:DisplayLayout:GroupByBox:PromptAppearance = appearance32.
      THIS-OBJECT:GridArbLop:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbLop:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance33:BackColor = System.Drawing.SystemColors:Window.
      appearance33:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:ActiveCellAppearance = appearance33.
      appearance34:BackColor = System.Drawing.SystemColors:Highlight.
      appearance34:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:ActiveRowAppearance = appearance34.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:CardAreaAppearance = appearance35.
      appearance36:BorderColor = System.Drawing.Color:Silver.
      appearance36:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:CellAppearance = appearance36.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:CellPadding = 0.
      appearance37:BackColor = System.Drawing.SystemColors:Control.
      appearance37:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance37:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance37:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance37:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:GroupByRowAppearance = appearance37.
      appearance38:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:HeaderAppearance = appearance38.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:RowAppearance = appearance39.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbLop:DisplayLayout:Override:TemplateAddRowAppearance = appearance40.
      THIS-OBJECT:GridArbLop:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbLop:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbLop:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbLop:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbLop:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbLop:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbLop:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbLop:GuruContext = ?.
      THIS-OBJECT:GridArbLop:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbLop:Name = "GridArbLop".
      THIS-OBJECT:GridArbLop:Size = NEW System.Drawing.Size(215, 556).
      THIS-OBJECT:GridArbLop:TabIndex = 1.
      THIS-OBJECT:GridArbLop:Text = "grid1".
      /*  */
      /* buttonDownKopp */
      /*  */
      appearance41:Image = CAST(resources:GetObject("appearance41.Image"), System.Object).
      THIS-OBJECT:buttonDownKopp:Appearance = appearance41.
      THIS-OBJECT:buttonDownKopp:Location = NEW System.Drawing.Point(3, 62).
      THIS-OBJECT:buttonDownKopp:Name = "buttonDownKopp".
      THIS-OBJECT:buttonDownKopp:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonDownKopp:TabIndex = 8.
      THIS-OBJECT:buttonDownKopp:Text = "buttonUpDown1".
      THIS-OBJECT:buttonDownKopp:Click:Subscribe(THIS-OBJECT:buttonDownKopp_Click).
      /*  */
      /* buttonUpKopp */
      /*  */
      appearance42:Image = CAST(resources:GetObject("appearance42.Image"), System.Object).
      THIS-OBJECT:buttonUpKopp:Appearance = appearance42.
      THIS-OBJECT:buttonUpKopp:Location = NEW System.Drawing.Point(3, 31).
      THIS-OBJECT:buttonUpKopp:Name = "buttonUpKopp".
      THIS-OBJECT:buttonUpKopp:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonUpKopp:TabIndex = 7.
      THIS-OBJECT:buttonUpKopp:Text = "buttonUpDown1".
      THIS-OBJECT:buttonUpKopp:Click:Subscribe(THIS-OBJECT:buttonUpKopp_Click).
      /*  */
      /* buttonOverArbKomDirekt */
      /*  */
      appearance43:Image = CAST(resources:GetObject("appearance43.Image"), System.Object).
      THIS-OBJECT:buttonOverArbKomDirekt:Appearance = appearance43.
      THIS-OBJECT:buttonOverArbKomDirekt:Location = NEW System.Drawing.Point(3, 93).
      THIS-OBJECT:buttonOverArbKomDirekt:Name = "buttonOverArbKomDirekt".
      THIS-OBJECT:buttonOverArbKomDirekt:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverArbKomDirekt:TabIndex = 6.
      THIS-OBJECT:buttonOverArbKomDirekt:Click:Subscribe(THIS-OBJECT:buttonOverArbKomDirekt_Click).
      /*  */
      /* buttonOverArbKomBort */
      /*  */
      appearance44:Image = CAST(resources:GetObject("appearance44.Image"), System.Object).
      THIS-OBJECT:buttonOverArbKomBort:Appearance = appearance44.
      THIS-OBJECT:buttonOverArbKomBort:Location = NEW System.Drawing.Point(3, 124).
      THIS-OBJECT:buttonOverArbKomBort:Name = "buttonOverArbKomBort".
      THIS-OBJECT:buttonOverArbKomBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverArbKomBort:TabIndex = 5.
      THIS-OBJECT:buttonOverArbKomBort:Click:Subscribe(THIS-OBJECT:buttonOverArbKomBort_Click).
      /*  */
      /* buttonOverArbKom */
      /*  */
      appearance45:Image = CAST(resources:GetObject("appearance45.Image"), System.Object).
      THIS-OBJECT:buttonOverArbKom:Appearance = appearance45.
      THIS-OBJECT:buttonOverArbKom:Location = NEW System.Drawing.Point(2, 93).
      THIS-OBJECT:buttonOverArbKom:Name = "buttonOverArbKom".
      THIS-OBJECT:buttonOverArbKom:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverArbKom:TabIndex = 4.
      THIS-OBJECT:buttonOverArbKom:Click:Subscribe(THIS-OBJECT:buttonOverArbKom_Click).
      /*  */
      /* GridKoppladeKom */
      /*  */
      appearance46:BackColor = System.Drawing.SystemColors:Window.
      appearance46:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Appearance = appearance46.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance47:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:GroupByBox:Appearance = appearance47.
      appearance48:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:GroupByBox:BandLabelAppearance = appearance48.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance49:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance49:BackColor2 = System.Drawing.SystemColors:Control.
      appearance49:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance49:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:GroupByBox:PromptAppearance = appearance49.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance50:BackColor = System.Drawing.SystemColors:Window.
      appearance50:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:ActiveCellAppearance = appearance50.
      appearance51:BackColor = System.Drawing.SystemColors:Highlight.
      appearance51:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:ActiveRowAppearance = appearance51.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance52:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:CardAreaAppearance = appearance52.
      appearance53:BorderColor = System.Drawing.Color:Silver.
      appearance53:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:CellAppearance = appearance53.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:CellPadding = 0.
      appearance54:BackColor = System.Drawing.SystemColors:Control.
      appearance54:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance54:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:GroupByRowAppearance = appearance54.
      appearance55:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:HeaderAppearance = appearance55.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance56:BackColor = System.Drawing.SystemColors:Window.
      appearance56:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:RowAppearance = appearance56.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance57:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Override:TemplateAddRowAppearance = appearance57.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKoppladeKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKoppladeKom:GuruContext = ?.
      THIS-OBJECT:GridKoppladeKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKoppladeKom:Name = "GridKoppladeKom".
      THIS-OBJECT:GridKoppladeKom:Size = NEW System.Drawing.Size(393, 556).
      THIS-OBJECT:GridKoppladeKom:TabIndex = 0.
      THIS-OBJECT:GridKoppladeKom:Text = "grid2".
      THIS-OBJECT:GridKoppladeKom:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKoppladeKom_AfterCellUpdate).
      /*  */
      /* GridP2KomResult */
      /*  */
      appearance58:BackColor = System.Drawing.SystemColors:Window.
      appearance58:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Appearance = appearance58.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance59:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:GroupByBox:Appearance = appearance59.
      appearance60:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:GroupByBox:BandLabelAppearance = appearance60.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance61:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance61:BackColor2 = System.Drawing.SystemColors:Control.
      appearance61:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance61:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:GroupByBox:PromptAppearance = appearance61.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance62:BackColor = System.Drawing.SystemColors:Window.
      appearance62:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:ActiveCellAppearance = appearance62.
      appearance63:BackColor = System.Drawing.SystemColors:Highlight.
      appearance63:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:ActiveRowAppearance = appearance63.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance64:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:CardAreaAppearance = appearance64.
      appearance65:BorderColor = System.Drawing.Color:Silver.
      appearance65:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:CellAppearance = appearance65.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:CellPadding = 0.
      appearance66:BackColor = System.Drawing.SystemColors:Control.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:GroupByRowAppearance = appearance66.
      appearance67:TextHAlignAsString = "Left".
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:HeaderAppearance = appearance67.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance68:BackColor = System.Drawing.SystemColors:Window.
      appearance68:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:RowAppearance = appearance68.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance69:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Override:TemplateAddRowAppearance = appearance69.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridP2KomResult:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridP2KomResult:GuruContext = ?.
      THIS-OBJECT:GridP2KomResult:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridP2KomResult:Name = "GridP2KomResult".
      THIS-OBJECT:GridP2KomResult:Size = NEW System.Drawing.Size(25, 556).
      THIS-OBJECT:GridP2KomResult:TabIndex = 0.
      THIS-OBJECT:GridP2KomResult:Text = "grid1".
      /*  */
      /* p2KomStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillP2kom.
      arrayvar0[2] = THIS-OBJECT:taBortP2kom.
      arrayvar0[3] = THIS-OBJECT:toolStripButtonReadOnly.
      THIS-OBJECT:p2KomStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:p2KomStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:p2KomStrip:Name = "p2KomStrip".
      THIS-OBJECT:p2KomStrip:Size = NEW System.Drawing.Size(490, 25).
      THIS-OBJECT:p2KomStrip:TabIndex = 2.
      THIS-OBJECT:p2KomStrip:Text = "toolStrip1".
      /*  */
      /* laggTillP2kom */
      /*  */
      THIS-OBJECT:laggTillP2kom:Image = CAST(resources:GetObject("laggTillP2kom.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillP2kom:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillP2kom:Name = "laggTillP2kom".
      THIS-OBJECT:laggTillP2kom:Size = NEW System.Drawing.Size(153, 22).
      THIS-OBJECT:laggTillP2kom:Text = "Lägg till P2-Kommentar".
      THIS-OBJECT:laggTillP2kom:Click:Subscribe(THIS-OBJECT:laggTillP2kom_Click).
      /*  */
      /* taBortP2kom */
      /*  */
      THIS-OBJECT:taBortP2kom:Image = CAST(resources:GetObject("taBortP2kom.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortP2kom:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortP2kom:Name = "taBortP2kom".
      THIS-OBJECT:taBortP2kom:Size = NEW System.Drawing.Size(141, 22).
      THIS-OBJECT:taBortP2kom:Text = "Ta bort P2- Komentar".
      THIS-OBJECT:taBortP2kom:ToolTipText = "Tar bort markerade P2- Komentar".
      THIS-OBJECT:taBortP2kom:Click:Subscribe(THIS-OBJECT:taBortP2kom_Click).
      /*  */
      /* toolStripButtonReadOnly */
      /*  */
      THIS-OBJECT:toolStripButtonReadOnly:Image = CAST(resources:GetObject("toolStripButtonReadOnly.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonReadOnly:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonReadOnly:Name = "toolStripButtonReadOnly".
      THIS-OBJECT:toolStripButtonReadOnly:Size = NEW System.Drawing.Size(108, 22).
      THIS-OBJECT:toolStripButtonReadOnly:Text = "Skrivskydd Bort".
      THIS-OBJECT:toolStripButtonReadOnly:ToolTipText = "Ändra på skrivskydd.".
      THIS-OBJECT:toolStripButtonReadOnly:Click:Subscribe(THIS-OBJECT:toolStripButtonReadOnly_Click).
      /*  */
      /* ArbLopStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:toolStripDropDownButtonSvep.
      arrayvar1[2] = THIS-OBJECT:toolStripTextArbLop.
      arrayvar1[3] = THIS-OBJECT:toolStripButtonVisa.
      arrayvar1[4] = THIS-OBJECT:toolStripButtonP2KomKoll.
      THIS-OBJECT:ArbLopStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:ArbLopStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ArbLopStrip:Name = "ArbLopStrip".
      THIS-OBJECT:ArbLopStrip:Size = NEW System.Drawing.Size(646, 25).
      THIS-OBJECT:ArbLopStrip:TabIndex = 0.
      THIS-OBJECT:ArbLopStrip:Text = "toolStrip1".
      /*  */
      /* toolStripDropDownButtonSvep */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 7 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:KomLop.
      arrayvar2[2] = THIS-OBJECT:KomArbkod.
      arrayvar2[3] = THIS-OBJECT:SkapaNyttSvep.
      arrayvar2[4] = THIS-OBJECT:UtokaBefSvep.
      arrayvar2[5] = THIS-OBJECT:KomBefSvep.
      arrayvar2[6] = THIS-OBJECT:SammaSomSvep.
      arrayvar2[7] = THIS-OBJECT:SammaSomArbkod.
      THIS-OBJECT:toolStripDropDownButtonSvep:DropDownItems:AddRange(arrayvar2).
      THIS-OBJECT:toolStripDropDownButtonSvep:Image = CAST(resources:GetObject("toolStripDropDownButtonSvep.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripDropDownButtonSvep:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripDropDownButtonSvep:Name = "toolStripDropDownButtonSvep".
      THIS-OBJECT:toolStripDropDownButtonSvep:Size = NEW System.Drawing.Size(153, 22).
      THIS-OBJECT:toolStripDropDownButtonSvep:Text = "Kommentarer på Svep".
      /*  */
      /* KomLop */
      /*  */
      THIS-OBJECT:KomLop:Name = "KomLop".
      THIS-OBJECT:KomLop:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:KomLop:Text = "Kommentarer på Löpnummer        ".
      THIS-OBJECT:KomLop:Click:Subscribe(THIS-OBJECT:KomLop_Click).
      /*  */
      /* KomArbkod */
      /*  */
      THIS-OBJECT:KomArbkod:Name = "KomArbkod".
      THIS-OBJECT:KomArbkod:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:KomArbkod:Text = "Kommentarer på Arbetskoder ".
      THIS-OBJECT:KomArbkod:Click:Subscribe(THIS-OBJECT:KomArbkod_Click).
      /*  */
      /* SkapaNyttSvep */
      /*  */
      THIS-OBJECT:SkapaNyttSvep:Name = "SkapaNyttSvep".
      THIS-OBJECT:SkapaNyttSvep:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:SkapaNyttSvep:Text = "Skapa Svep                      ".
      THIS-OBJECT:SkapaNyttSvep:Click:Subscribe(THIS-OBJECT:SkapaNyttSvep_Click).
      /*  */
      /* UtokaBefSvep */
      /*  */
      THIS-OBJECT:UtokaBefSvep:Name = "UtokaBefSvep".
      THIS-OBJECT:UtokaBefSvep:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:UtokaBefSvep:Text = "Utöka befintligt Svep            ".
      THIS-OBJECT:UtokaBefSvep:Click:Subscribe(THIS-OBJECT:UtokaBefSvep_Click).
      /*  */
      /* KomBefSvep */
      /*  */
      THIS-OBJECT:KomBefSvep:Name = "KomBefSvep".
      THIS-OBJECT:KomBefSvep:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:KomBefSvep:Text = "Kommentarer på befintligt Svep".
      THIS-OBJECT:KomBefSvep:Click:Subscribe(THIS-OBJECT:KomBefSvep_Click).
      /*  */
      /* SammaSomSvep */
      /*  */
      THIS-OBJECT:SammaSomSvep:Name = "SammaSomSvep".
      THIS-OBJECT:SammaSomSvep:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:SammaSomSvep:Text = "Skapa Svep med samma Kommentarer    ".
      THIS-OBJECT:SammaSomSvep:Click:Subscribe(THIS-OBJECT:SammaSomSvep_Click).
      /*  */
      /* SammaSomArbkod */
      /*  */
      THIS-OBJECT:SammaSomArbkod:Name = "SammaSomArbkod".
      THIS-OBJECT:SammaSomArbkod:Size = NEW System.Drawing.Size(364, 22).
      THIS-OBJECT:SammaSomArbkod:Text = "Skapa Svep med samma Kommentarer som Arbetskod ".
      THIS-OBJECT:SammaSomArbkod:Click:Subscribe(THIS-OBJECT:SammaSomArbkod_Click).
      /*  */
      /* toolStripTextArbLop */
      /*  */
      THIS-OBJECT:toolStripTextArbLop:ForeColor = System.Drawing.Color:Green.
      THIS-OBJECT:toolStripTextArbLop:Name = "toolStripTextArbLop".
      THIS-OBJECT:toolStripTextArbLop:Size = NEW System.Drawing.Size(400, 25).
      /*  */
      /* toolStripButtonVisa */
      /*  */
      THIS-OBJECT:toolStripButtonVisa:Image = CAST(resources:GetObject("toolStripButtonVisa.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonVisa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonVisa:Name = "toolStripButtonVisa".
      THIS-OBJECT:toolStripButtonVisa:Size = NEW System.Drawing.Size(48, 22).
      THIS-OBJECT:toolStripButtonVisa:Text = "Visa".
      THIS-OBJECT:toolStripButtonVisa:Click:Subscribe(THIS-OBJECT:toolStripButtonVisa_Click).
      /*  */
      /* toolStripButtonP2KomKoll */
      /*  */
      THIS-OBJECT:toolStripButtonP2KomKoll:Image = CAST(resources:GetObject("toolStripButtonP2KomKoll.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonP2KomKoll:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonP2KomKoll:Name = "toolStripButtonP2KomKoll".
      THIS-OBJECT:toolStripButtonP2KomKoll:Size = NEW System.Drawing.Size(159, 20).
      THIS-OBJECT:toolStripButtonP2KomKoll:Text = "Kontroll av kommentarer".
      THIS-OBJECT:toolStripButtonP2KomKoll:Click:Subscribe(THIS-OBJECT:toolStripButtonP2KomKoll_Click).
      /*  */
      /* toolStripVisa */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:toolStripButtonAter.
      THIS-OBJECT:toolStripVisa:Items:AddRange(arrayvar3).
      THIS-OBJECT:toolStripVisa:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripVisa:Name = "toolStripVisa".
      THIS-OBJECT:toolStripVisa:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:toolStripVisa:TabIndex = 0.
      THIS-OBJECT:toolStripVisa:Text = "toolStrip1".
      /*  */
      /* toolStripButtonAter */
      /*  */
      THIS-OBJECT:toolStripButtonAter:Image = CAST(resources:GetObject("toolStripButtonAter.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonAter:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonAter:Name = "toolStripButtonAter".
      THIS-OBJECT:toolStripButtonAter:Size = NEW System.Drawing.Size(141, 20).
      THIS-OBJECT:toolStripButtonAter:Text = "Åter till Kommentarer".
      THIS-OBJECT:toolStripButtonAter:Click:Subscribe(THIS-OBJECT:toolStripButtonAter_Click).
      /*  */
      /* toolStripComboBox1 */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true", Type="System.String").
      DEFINE VARIABLE arrayvar4 AS CHARACTER EXTENT 7 NO-UNDO.
      arrayvar4[1] = "Kommentarer på Arbetskoder".
      arrayvar4[2] = "Kommentarer på Löpnummer  ".
      arrayvar4[3] = "Kommentarer på befintligt Svep".
      arrayvar4[4] = "Utöka befintligt Svep".
      arrayvar4[5] = "Skapa nytt Svep ".
      arrayvar4[6] = "Skapa nytt Svep med samma Kommentarer         ".
      arrayvar4[7] = "Skapa nytt Svep med samma som Arbetskod            ".
      THIS-OBJECT:toolStripComboBox1:AutoCompleteCustomSource:AddRange(arrayvar4).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS System.Object EXTENT 7 NO-UNDO.
      arrayvar5[1] = "Kommentarer på Arbetskoder".
      arrayvar5[2] = "Kommentarer på Löpnummer".
      arrayvar5[3] = "Kommentarer på befinligt Svep".
      arrayvar5[4] = "Utöka befintligt  Svep".
      arrayvar5[5] = "Skapa nytt Svep".
      arrayvar5[6] = "Skapa nytt Svep med samma Kommentarer ".
      arrayvar5[7] = "Skapa nytt Svep med samma som Arbetskod".
      THIS-OBJECT:toolStripComboBox1:Items:AddRange(arrayvar5).
      THIS-OBJECT:toolStripComboBox1:Name = "toolStripComboBox1".
      THIS-OBJECT:toolStripComboBox1:Size = NEW System.Drawing.Size(250, 23).
      THIS-OBJECT:toolStripComboBox1:ToolTipText = "Visa Arbetskoder eller Löpnummer".
      /*  */
      /* toolStripComboBox2 */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true", Type="System.String").
      DEFINE VARIABLE arrayvar6 AS CHARACTER EXTENT 7 NO-UNDO.
      arrayvar6[1] = "Kommentarer på Arbetskoder".
      arrayvar6[2] = "Kommentarer på Löpnummer  ".
      arrayvar6[3] = "Kommentarer på befintligt Svep".
      arrayvar6[4] = "Utöka befintligt Svep".
      arrayvar6[5] = "Skapa nytt Svep ".
      arrayvar6[6] = "Skapa nytt Svep med samma Kommentarer         ".
      arrayvar6[7] = "Skapa nytt Svep med samma som Arbetskod            ".
      THIS-OBJECT:toolStripComboBox2:AutoCompleteCustomSource:AddRange(arrayvar6).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar7 AS System.Object EXTENT 7 NO-UNDO.
      arrayvar7[1] = "Kommentarer på Arbetskoder".
      arrayvar7[2] = "Kommentarer på Löpnummer".
      arrayvar7[3] = "Kommentarer på befinligt Svep".
      arrayvar7[4] = "Utöka befintligt  Svep".
      arrayvar7[5] = "Skapa nytt Svep".
      arrayvar7[6] = "Skapa nytt Svep med samma Kommentarer ".
      arrayvar7[7] = "Skapa nytt Svep med samma som Arbetskod".
      THIS-OBJECT:toolStripComboBox2:Items:AddRange(arrayvar7).
      THIS-OBJECT:toolStripComboBox2:Name = "toolStripComboBox2".
      THIS-OBJECT:toolStripComboBox2:Size = NEW System.Drawing.Size(250, 23).
      THIS-OBJECT:toolStripComboBox2:ToolTipText = "Visa Arbetskoder eller Löpnummer".
      /*  */
      /* splitContainerAllt */
      /*  */
      THIS-OBJECT:splitContainerAllt:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAllt:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAllt:Name = "splitContainerAllt".
      /*  */
      /* splitContainerAllt.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAllt:Panel1:Controls:Add(THIS-OBJECT:splitContainerGrundKom).
      /*  */
      /* splitContainerAllt.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAllt:Panel2:Controls:Add(THIS-OBJECT:splitContainerResultatAter).
      THIS-OBJECT:splitContainerAllt:Size = NEW System.Drawing.Size(1169, 585).
      THIS-OBJECT:splitContainerAllt:SplitterDistance = 1140.
      THIS-OBJECT:splitContainerAllt:TabIndex = 2.
      /*  */
      /* splitContainerGrundKom */
      /*  */
      THIS-OBJECT:splitContainerGrundKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundKom:Name = "splitContainerGrundKom".
      /*  */
      /* splitContainerGrundKom.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundKom:Panel1:Controls:Add(THIS-OBJECT:splitContainerKomTillBort).
      /*  */
      /* splitContainerGrundKom.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundKom:Panel2:Controls:Add(THIS-OBJECT:splitContainerArbLopKom).
      THIS-OBJECT:splitContainerGrundKom:Size = NEW System.Drawing.Size(1140, 585).
      THIS-OBJECT:splitContainerGrundKom:SplitterDistance = 490.
      THIS-OBJECT:splitContainerGrundKom:TabIndex = 0.
      /*  */
      /* splitContainerKomTillBort */
      /*  */
      THIS-OBJECT:splitContainerKomTillBort:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKomTillBort:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKomTillBort:Name = "splitContainerKomTillBort".
      THIS-OBJECT:splitContainerKomTillBort:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKomTillBort.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKomTillBort:Panel1:Controls:Add(THIS-OBJECT:p2KomStrip).
      /*  */
      /* splitContainerKomTillBort.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKomTillBort:Panel2:Controls:Add(THIS-OBJECT:splitContainerKomTillFran).
      THIS-OBJECT:splitContainerKomTillBort:Size = NEW System.Drawing.Size(490, 585).
      THIS-OBJECT:splitContainerKomTillBort:SplitterDistance = 25.
      THIS-OBJECT:splitContainerKomTillBort:TabIndex = 0.
      /*  */
      /* splitContainerKomTillFran */
      /*  */
      THIS-OBJECT:splitContainerKomTillFran:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKomTillFran:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKomTillFran:Name = "splitContainerKomTillFran".
      /*  */
      /* splitContainerKomTillFran.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKomTillFran:Panel1:Controls:Add(THIS-OBJECT:GridKom).
      /*  */
      /* splitContainerKomTillFran.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKomTillFran:Panel2:Controls:Add(THIS-OBJECT:splitContainerValdaKom).
      THIS-OBJECT:splitContainerKomTillFran:Size = NEW System.Drawing.Size(490, 556).
      THIS-OBJECT:splitContainerKomTillFran:SplitterDistance = 220.
      THIS-OBJECT:splitContainerKomTillFran:TabIndex = 0.
      /*  */
      /* splitContainerValdaKom */
      /*  */
      THIS-OBJECT:splitContainerValdaKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdaKom:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerValdaKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdaKom:Name = "splitContainerValdaKom".
      /*  */
      /* splitContainerValdaKom.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdaKom:Panel1:Controls:Add(THIS-OBJECT:buttonUpKom).
      THIS-OBJECT:splitContainerValdaKom:Panel1:Controls:Add(THIS-OBJECT:buttonDownKom).
      THIS-OBJECT:splitContainerValdaKom:Panel1:Controls:Add(THIS-OBJECT:buttonOverKom).
      THIS-OBJECT:splitContainerValdaKom:Panel1:Controls:Add(THIS-OBJECT:buttonOverKomBort).
      /*  */
      /* splitContainerValdaKom.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdaKom:Panel2:Controls:Add(THIS-OBJECT:GridKomVal).
      THIS-OBJECT:splitContainerValdaKom:Size = NEW System.Drawing.Size(266, 556).
      THIS-OBJECT:splitContainerValdaKom:SplitterDistance = 30.
      THIS-OBJECT:splitContainerValdaKom:TabIndex = 6.
      /*  */
      /* splitContainerArbLopKom */
      /*  */
      THIS-OBJECT:splitContainerArbLopKom:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerArbLopKom:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerArbLopKom:Name = "splitContainerArbLopKom".
      THIS-OBJECT:splitContainerArbLopKom:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerArbLopKom.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerArbLopKom:Panel1:Controls:Add(THIS-OBJECT:ArbLopStrip).
      /*  */
      /* splitContainerArbLopKom.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerArbLopKom:Panel2:Controls:Add(THIS-OBJECT:splitContainerKopplaArb).
      THIS-OBJECT:splitContainerArbLopKom:Size = NEW System.Drawing.Size(646, 585).
      THIS-OBJECT:splitContainerArbLopKom:SplitterDistance = 25.
      THIS-OBJECT:splitContainerArbLopKom:TabIndex = 0.
      /*  */
      /* splitContainerKopplaArb */
      /*  */
      THIS-OBJECT:splitContainerKopplaArb:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKopplaArb:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKopplaArb:Name = "splitContainerKopplaArb".
      /*  */
      /* splitContainerKopplaArb.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKopplaArb:Panel1:Controls:Add(THIS-OBJECT:GridArbLop).
      /*  */
      /* splitContainerKopplaArb.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKopplaArb:Panel2:Controls:Add(THIS-OBJECT:splitContainerKoppladeSvep).
      THIS-OBJECT:splitContainerKopplaArb:Size = NEW System.Drawing.Size(646, 556).
      THIS-OBJECT:splitContainerKopplaArb:SplitterDistance = 215.
      THIS-OBJECT:splitContainerKopplaArb:TabIndex = 0.
      /*  */
      /* splitContainerKoppladeSvep */
      /*  */
      THIS-OBJECT:splitContainerKoppladeSvep:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKoppladeSvep:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerKoppladeSvep:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKoppladeSvep:Name = "splitContainerKoppladeSvep".
      /*  */
      /* splitContainerKoppladeSvep.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:Controls:Add(THIS-OBJECT:buttonOverArbKomBort).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:Controls:Add(THIS-OBJECT:buttonOverArbKomDirekt).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:Controls:Add(THIS-OBJECT:buttonOverArbKom).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:Controls:Add(THIS-OBJECT:buttonDownKopp).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:Controls:Add(THIS-OBJECT:buttonUpKopp).
      /*  */
      /* splitContainerKoppladeSvep.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKoppladeSvep:Panel2:Controls:Add(THIS-OBJECT:GridKoppladeKom).
      THIS-OBJECT:splitContainerKoppladeSvep:Size = NEW System.Drawing.Size(427, 556).
      THIS-OBJECT:splitContainerKoppladeSvep:SplitterDistance = 30.
      THIS-OBJECT:splitContainerKoppladeSvep:TabIndex = 0.
      /*  */
      /* splitContainerResultatAter */
      /*  */
      THIS-OBJECT:splitContainerResultatAter:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResultatAter:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResultatAter:Name = "splitContainerResultatAter".
      THIS-OBJECT:splitContainerResultatAter:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerResultatAter.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResultatAter:Panel1:Controls:Add(THIS-OBJECT:toolStripVisa).
      /*  */
      /* splitContainerResultatAter.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResultatAter:Panel2:Controls:Add(THIS-OBJECT:splitContainerVisaKontroll).
      THIS-OBJECT:splitContainerResultatAter:Size = NEW System.Drawing.Size(25, 585).
      THIS-OBJECT:splitContainerResultatAter:SplitterDistance = 25.
      THIS-OBJECT:splitContainerResultatAter:TabIndex = 0.
      /*  */
      /* splitContainerVisaKontroll */
      /*  */
      THIS-OBJECT:splitContainerVisaKontroll:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisaKontroll:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisaKontroll:Name = "splitContainerVisaKontroll".
      /*  */
      /* splitContainerVisaKontroll.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVisaKontroll:Panel1:Controls:Add(THIS-OBJECT:GridP2KomResult).
      /*  */
      /* splitContainerVisaKontroll.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisaKontroll:Panel2:Controls:Add(THIS-OBJECT:GridSvepKomKoll).
      THIS-OBJECT:splitContainerVisaKontroll:Size = NEW System.Drawing.Size(25, 556).
      THIS-OBJECT:splitContainerVisaKontroll:SplitterDistance = 25.
      THIS-OBJECT:splitContainerVisaKontroll:TabIndex = 10.
      /*  */
      /* GridSvepKomKoll */
      /*  */
      appearance70:BackColor = System.Drawing.SystemColors:Window.
      appearance70:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Appearance = appearance70.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance71:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance71:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance71:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance71:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:GroupByBox:Appearance = appearance71.
      appearance72:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:GroupByBox:BandLabelAppearance = appearance72.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance73:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance73:BackColor2 = System.Drawing.SystemColors:Control.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:GroupByBox:PromptAppearance = appearance73.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance74:BackColor = System.Drawing.SystemColors:Window.
      appearance74:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:ActiveCellAppearance = appearance74.
      appearance75:BackColor = System.Drawing.SystemColors:Highlight.
      appearance75:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:ActiveRowAppearance = appearance75.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance76:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:CardAreaAppearance = appearance76.
      appearance77:BorderColor = System.Drawing.Color:Silver.
      appearance77:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:CellAppearance = appearance77.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:CellPadding = 0.
      appearance78:BackColor = System.Drawing.SystemColors:Control.
      appearance78:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance78:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance78:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance78:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:GroupByRowAppearance = appearance78.
      appearance79:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:HeaderAppearance = appearance79.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance80:BackColor = System.Drawing.SystemColors:Window.
      appearance80:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:RowAppearance = appearance80.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance81:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:Override:TemplateAddRowAppearance = appearance81.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSvepKomKoll:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSvepKomKoll:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSvepKomKoll:GuruContext = ?.
      THIS-OBJECT:GridSvepKomKoll:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSvepKomKoll:Name = "GridSvepKomKoll".
      THIS-OBJECT:GridSvepKomKoll:Size = NEW System.Drawing.Size(25, 556).
      THIS-OBJECT:GridSvepKomKoll:TabIndex = 0.
      THIS-OBJECT:GridSvepKomKoll:Text = "grid1".
      /*  */
      /* EkgP2Kom */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerAllt).
      THIS-OBJECT:Name = "EkgP2Kom".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1169, 585).
      CAST(THIS-OBJECT:GridKom, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridKomVal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridArbLop, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridKoppladeKom, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridP2KomResult, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:p2KomStrip:ResumeLayout(FALSE).
      THIS-OBJECT:p2KomStrip:PerformLayout().
      THIS-OBJECT:ArbLopStrip:ResumeLayout(FALSE).
      THIS-OBJECT:ArbLopStrip:PerformLayout().
      THIS-OBJECT:toolStripVisa:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripVisa:PerformLayout().
      THIS-OBJECT:splitContainerAllt:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAllt:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAllt, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAllt:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKom:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKom:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerGrundKom, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundKom:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKomTillBort:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKomTillBort:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerKomTillBort:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKomTillBort, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKomTillBort:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKomTillFran:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKomTillFran:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKomTillFran, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKomTillFran:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdaKom:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdaKom:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValdaKom, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdaKom:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbLopKom:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerArbLopKom:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerArbLopKom:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerArbLopKom, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerArbLopKom:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopplaArb:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKopplaArb:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKopplaArb, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKopplaArb:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKoppladeSvep:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKoppladeSvep, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKoppladeSvep:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResultatAter:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResultatAter:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerResultatAter:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResultatAter, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResultatAter:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisaKontroll:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisaKontroll:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVisaKontroll, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVisaKontroll:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSvepKomKoll, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      arblopkomval = 1.
      THIS-OBJECT:splitContainerAllt:Panel2Collapsed = TRUE.
   END METHOD.
   METHOD PUBLIC CHARACTER ArbLopFilter():
      THIS-OBJECT:splitContainerGrundKom:Panel1Collapsed = FALSE.
      THIS-OBJECT:splitContainerGrundKom:Panel2Collapsed = FALSE.
      THIS-OBJECT:splitContainerKopplaArb:Panel1Collapsed = FALSE.
      THIS-OBJECT:buttonOverArbKomDirekt:VISIBLE = FALSE.
      THIS-OBJECT:buttonOverArbKom:VISIBLE = TRUE.

      IF arblopkomval = 0 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:KomArbkod:Text. 
         THIS-OBJECT:toolStripTextArbLop:Text = "Nya Kommentarer som ska visas per Arbetskod".
         RETURN "SVEPLOPNR = ''".
      END.   
      IF arblopkomval = 1 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:KomLop:Text.
         THIS-OBJECT:toolStripTextArbLop:Text = "Nya Kommentarer som ska visas per Löpnummer".
         RETURN "SVEPLOPNR NE ''".
      END.   
      IF arblopkomval = 2 THEN DO:                    
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:KomBefSvep:Text.
         THIS-OBJECT:toolStripTextArbLop:Text = "Nya Kommentarer till ett befintligt Svep".
         THIS-OBJECT:splitContainerKopplaArb:Panel1Collapsed = TRUE.
         THIS-OBJECT:buttonOverArbKomDirekt:VISIBLE = TRUE.
         THIS-OBJECT:buttonOverArbKom:VISIBLE = FALSE.   
         RETURN "SVEPLOPNR NE ''".
      END.
      IF arblopkomval = 3 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:UtokaBefSvep:Text.
         THIS-OBJECT:toolStripTextArbLop:Text = "Utökar Svepet med ytterligare Löpnummer".
         THIS-OBJECT:splitContainerGrundKom:Panel1Collapsed = TRUE.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE NE "" THEN. 
            ELSE THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE SVEPLOPNR NE '' ") NO-ERROR.
         END.
         ELSE THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE SVEPLOPNR NE '' ") NO-ERROR.  
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN RETURN "SVEPLOPNR NE '' AND  P2ARBKOD = '" +  THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "'".
         ELSE RETURN "SVEPLOPNR NE '' ".
      END.
      IF arblopkomval = 4 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:SkapaNyttSvep:Text.
         THIS-OBJECT:toolStripTextArbLop:Text = "Skapar nya Svep av valda Löpnummer och Kommentarer".
         RETURN "SVEPLOPNR NE ''".
      END.
      IF arblopkomval = 5 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:SammaSomSvep:Text.
         THIS-OBJECT:splitContainerGrundKom:Panel1Collapsed = TRUE.
         THIS-OBJECT:toolStripTextArbLop:Text = "Skapar nytt Svep med samma Kommentarer som valt Svep".
         RETURN "".
      END.
      IF arblopkomval = 6 THEN DO:
         THIS-OBJECT:toolStripDropDownButtonSvep:Text = THIS-OBJECT:SammaSomArbkod:Text.
         THIS-OBJECT:splitContainerGrundKom:Panel1Collapsed = TRUE.
         THIS-OBJECT:toolStripTextArbLop:Text = "Skapar nytt Svep med samma Kommentarer som vald ArbetsKod".
         RETURN "".
      END.
 
      
        
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      /*P2kom*/
      THIS-OBJECT:CreateGridP2KomResult().
      THIS-OBJECT:CreateGridKoppladeKom().
      THIS-OBJECT:CreateGridKom().
      THIS-OBJECT:CreateGridArbLop().
      THIS-OBJECT:CreateGridSvepKomKoll().
      
   END METHOD.
   METHOD PUBLIC VOID CreateGridSvepKomKoll():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,300).
      rubrikergrid[2] = NEW Controls.GridRubrik("SVEPLOPNR","",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("FELTYP","",TRUE,200).
      THIS-OBJECT:GridSvepKomKoll:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:SvepKollTTh)).
      THIS-OBJECT:GridSvepKomKoll:Text = "P2-Kommentarer kontroll".
      THIS-OBJECT:GridSvepKomKoll:GuruUpdateTitle().
   END METHOD.
   METHOD PUBLIC VOID CreateGridP2KomResult():
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("P2KOD","Kod",TRUE,300).
      rubrikergrid[2] = NEW Controls.GridRubrik("KOMID","",TRUE,50).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,800).
      rubrikergrid[4] = NEW Controls.GridRubrik("INGAREJ","I kostnaden" + chr(10) + "ingår ej",TRUE,200).
      rubrikergrid[5] = NEW Controls.GridRubrik("ORDNING","",TRUE,70).
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Bands[0]:ColHeaderLines = 2.
      /*ultraGridGroup3:Key = "This group" + chr(10) + "contains all Long columns".
   rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","Benämning" + chr(10) + "kk" ,TRUE,325).
      THIS-OBJECT:KalkControl:GridLopKoder:DisplayLayout:Bands[0]:ColHeaderLines = 5.
   */
      THIS-OBJECT:GridP2KomResult:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ekgP2KomResultTTh)).
      THIS-OBJECT:GridP2KomResult:Text = "P2 Kommentarer Resultat".
      THIS-OBJECT:GridP2KomResult:GuruUpdateTitle().
      THIS-OBJECT:GridP2KomResult:GuruOrderby("BY P2ARBKOD BY SVEPLOPNR BY ORDNING" ).
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Bands[0]:Columns["KOMID"]:Hidden = TRUE.
      THIS-OBJECT:GridP2KomResult:DisplayLayout:Bands[0]:Columns["ORDNING"]:Hidden = TRUE. 
    
      THIS-OBJECT:GridP2KomResult:GuruGridRadOption(TRUE).
     
      CellFont = NEW System.Drawing.Font("Arial Black", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).   
                                             /*styrfält  värde    färg                           font alla col, lista col  0  bgcolor 1 = fg color 2 font*/
                                                                                                                          /*  ska avslutas med,*/ 
      THIS-OBJECT:GridP2KomResult:GuruAddRadOption("ORDNING", "1", Guru.StaticMethods:DotNetColor(12),CellFont,false,"P2KOD,ANMARKNING,",2).
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKoppladeKom():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("SVEPLOPNR","",TRUE,150).
      rubrikergrid[3] = NEW Controls.GridRubrik("KOMID","",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,350).
      rubrikergrid[5] = NEW Controls.GridRubrik("INGAREJ","Ej",FALSE,30).
      rubrikergrid[6] = NEW Controls.GridRubrik("ORDNING","Ordning",TRUE,55).
      /*
      rubrikergrid[7] = NEW Controls.GridRubrik("EKGSUBID","s",TRUE,55).
      rubrikergrid[8] = NEW Controls.GridRubrik("EBRKAT","kat",TRUE,55).
      */
      THIS-OBJECT:GridKoppladeKom:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 6, INPUT THIS-OBJECT:ekgKoppArbSvepTTh)).
      THIS-OBJECT:GridKoppladeKom:Text = "P2-Kommentarer".
      THIS-OBJECT:GridKoppladeKom:GuruUpdateTitle().
      THIS-OBJECT:GridKoppladeKom:tBS:PositionChanged:Subscribe(ValueGridKoppladeKomCh).
      THIS-OBJECT:GridKoppladeKom:DisplayLayout:Bands[0]:Columns["KOMID"]:Hidden = TRUE.
      
      THIS-OBJECT:buttonUpKopp:GuruKoppla(THIS-OBJECT:GridKoppladeKom,"ORDNING",TRUE).
      THIS-OBJECT:buttonDownKopp:GuruKoppla(THIS-OBJECT:GridKoppladeKom,"ORDNING",FALSE).
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKom():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("KOMID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,340).
      THIS-OBJECT:GridKom:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:ekgP2komTTh)).
      THIS-OBJECT:GridKom:Text = "Kommentarer".
      THIS-OBJECT:GridKom:GuruUpdateTitle().
      THIS-OBJECT:GridKom:DisplayLayout:Bands[0]:Columns["KOMID"]:Hidden = TRUE.
      THIS-OBJECT:GridKom:GuruOrderby("BY ANMARKNING").
      rubrikergrid[1] = NEW Controls.GridRubrik("KOMID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,350).
      rubrikergrid[3] = NEW Controls.GridRubrik("OORDNING","Ordning",TRUE,55).
      THIS-OBJECT:GridKomVal:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:ekgP2ValkomTTh)).
      THIS-OBJECT:GridKomVal:Text = "Valda Kommentarer".
      THIS-OBJECT:GridKomVal:GuruUpdateTitle().
      THIS-OBJECT:GridKomVal:DisplayLayout:Bands[0]:Columns["KOMID"]:Hidden = TRUE.
      THIS-OBJECT:buttonUpKom:GuruKoppla(THIS-OBJECT:GridKomVal,"OORDNING",TRUE).
      THIS-OBJECT:buttonDownKom:GuruKoppla(THIS-OBJECT:GridKomVal,"OORDNING",FALSE).

   END METHOD.
   
   METHOD PUBLIC VOID CreateGridArbLop():
      {GridRubrikListaVarExtent.i} 
      rubrikergrid[1] = NEW Controls.GridRubrik("P2ARBKOD","",TRUE,50).
      rubrikergrid[2] = NEW Controls.GridRubrik("SVEPLOPNR","",TRUE,40).
      rubrikergrid[3] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      THIS-OBJECT:GridArbLop:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 3, INPUT THIS-OBJECT:ekgArbLopKodTTh)).
      THIS-OBJECT:GridArbLop:Text = "P2-Koder".
      THIS-OBJECT:GridArbLop:GuruUpdateTitle().
      
      
   END METHOD.
   
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KomArbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 0.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KomBefSvep_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 2.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KomLop_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 1.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillP2kom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE komidvar AS INTEGER NO-UNDO.
		DEFINE VARIABLE rid AS ROWID NO-UNDO.
		THIS-OBJECT:ekgP2komTTh:FIND-LAST("WHERE  USE-INDEX KOMID" ) NO-ERROR.
      IF THIS-OBJECT:ekgP2komTTh:AVAILABLE THEN DO:
         komidvar = THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE + 1. 
      END.
      ELSE komidvar = 1.
          
      THIS-OBJECT:ekgP2komTTh:BUFFER-CREATE().
      ASSIGN 
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("EKGSUBID"):BUFFER-VALUE = THIS-OBJECT:ControlWinLg:ekgsubidvar.
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("EBRKAT"):BUFFER-VALUE = THIS-OBJECT:ControlWinLg:ebrkatvar.
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE = komidvar.
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "< Ny kommentar >".
      THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgP2komTTh:RECID.
      THIS-OBJECT:GridKom:GuruReadOnly(FALSE,"ANMARKNING").
      THIS-OBJECT:toolStripButtonReadOnly:Text = "Skrivskydd".
      rid = THIS-OBJECT:ekgP2komTTh:ROWID. 
      THIS-OBJECT:GridKom:GuruReopen().
      
      THIS-OBJECT:GridKom:GuruRepositionTo(rid).
      THIS-OBJECT:ControlWinLg:AndringP2kom(). 
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SammaSomArbkod_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 6.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SammaSomSvep_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 5.
		THIS-OBJECT:StripDropDownButtonSvep(). 
		RETURN.

	END METHOD.

   METHOD PUBLIC LOGICAL SkapaKom():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
      DEFINE VARIABLE fardigtsvep AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sammap2arb AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sammap2lop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2arb AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2lop AS CHARACTER NO-UNDO.
      DEFINE VARIABLE varp2ordning AS INTEGER NO-UNDO.
      DEFINE VARIABLE ordvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      eekgP2ValkomTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      eekgKoppArbSvepTTh:EMPTY-TEMP-TABLE() NO-ERROR.
     /*Arbetskod ELLER  Löpnummer*/
      IF arblopkomval = 0 OR arblopkomval = 1 THEN DO:
         THIS-OBJECT:buttonOverArbKom:GuruOver().
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         queryvar2 =  "FOR EACH " + THIS-OBJECT:ekgP2ValkomTTh:TABLE.
         q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2ValkomTTh,queryvar2).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            q2H:GET-FIRST().
            DO WHILE q2H:QUERY-OFF-END = FALSE:
               eekgKoppArbSvepTTh:BUFFER-CREATE().
               eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
               eekgKoppArbSvepTTh:BUFFER-COPY(ekgP2ValkomTTh).
               THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID . 
               THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE = 
               TRIM(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
               q2H:GET-NEXT().                       
            END.
            /*AUTOPOST BORT*/
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().   
         END.   
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         varp2arb = TRIM(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
         varp2lop = TRIM(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD = '" + STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) + 
            "' AND SVEPLOPNR = '" +  STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE) + "'" + 
            " AND KOMID = " + STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE) +
            " AND ORDNING NE 1") NO-ERROR.
            /*INGA DUBLETTER SKAPAS*/
            IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN eekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR.
            ELSE DO:    
               THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE) +
               "' AND SVEPLOPNR = '" +  STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE) + "' USE-INDEX ORDNING") NO-ERROR.
               IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN ordvar = 1 + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
               ELSE ordvar = 1.
               ekgKoppArbSvepTTh:BUFFER-CREATE().
               ekgKoppArbSvepTTh:BUFFER-COPY(eekgKoppArbSvepTTh).
               THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:RECID . 
               THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordvar.
               varp2arb = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
               varp2lop = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
               varp2ordning = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
            END.
            qH:GET-NEXT().         
         END.
          /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         
         THIS-OBJECT:GridKoppladeKom:GuruReopen().
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  varp2lop + "'" + " AND ORDNING = " + STRING(varp2ordning)) NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
         ELSE DO:
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  varp2lop + "'") NO-ERROR.
            IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
               THIS-OBJECT:buttonOverArbKom:TomMark().
               THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
            END.
         END.   
      END.
      /* Nya Kommentarer till ett befintligt Svep */
      IF arblopkomval = 2 THEN DO:
         ASSIGN 
         varp2arb = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         varp2lop = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["SVEPLOPNR"]:VALUE. 
         varp2arb = TRIM(varp2arb).
         varp2lop = TRIM(varp2lop).
         THIS-OBJECT:buttonOverArbKomDirekt:GuruOver().
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            eekgKoppArbSvepTTh:BUFFER-CREATE().
            eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID .
            ASSIGN
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE = varp2arb
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE = varp2lop.
            /*AUTOPOST BORT*/
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().         
         END.
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  varp2lop + "'" + 
            " AND KOMID = " + STRING(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE) + 
             " AND ORDNING NE 1") NO-ERROR.
              /*INGA DUBLETTER SKAPAS*/
            IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN eekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR.
            ELSE DO:   
               THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  varp2lop + "' USE-INDEX ORDNING") NO-ERROR.
               IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN ordvar = 1 + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
               ELSE ordvar = 1.
               ekgKoppArbSvepTTh:BUFFER-CREATE().
               ekgKoppArbSvepTTh:BUFFER-COPY(eekgKoppArbSvepTTh).
               THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:RECID .
               THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = ordvar.
            END.
            qH:GET-NEXT().         
         END.
         /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         THIS-OBJECT:GridKoppladeKom:GuruReopen().
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  varp2lop + "'") NO-ERROR.
         
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKomDirekt:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
      END. 
      /*Utökar Svepet med ytterligare Löpnummer*/
      IF arblopkomval = 3 THEN DO:
         
         ASSIGN 
         sammap2arb = THIS-OBJECT:GridArbLop:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         sammap2lop = THIS-OBJECT:GridArbLop:ActiveRow:Cells["SVEPLOPNR"]:VALUE.
         ASSIGN 
         varp2arb = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         varp2lop = THIS-OBJECT:GridKoppladeKom:ActiveRow:Cells["SVEPLOPNR"]:VALUE.
         varp2arb = TRIM(varp2arb).
         varp2lop = TRIM(varp2lop).
         THIS-OBJECT:buttonOverArbKom:GuruOver().
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF varp2arb = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE THEN DO:
               eekgKoppArbSvepTTh:BUFFER-CREATE().
               eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
               THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID .
            END.  
            /*AUTOPOST BORT*/ 
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().   
         END.
         
         THIS-OBJECT:ControlDataDb:SvepLopNrStart(varp2lop).  
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlDataDb:SvepLopNr(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
            qH:GET-NEXT().   
         END.
         
         fardigtsvep = THIS-OBJECT:ControlDataDb:SvepLopNrSlut().
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" + varp2lop + "'".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE =  fardigtsvep.
            qH:GET-NEXT().   
         END.
         /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         THIS-OBJECT:GridKoppladeKom:GuruReopen().
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  fardigtsvep + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
         THIS-OBJECT:ekgArbLopKodTTh:FIND-first("WHERE P2ARBKOD = '" + sammap2arb + "' AND SVEPLOPNR = '" +  sammap2lop + "'") NO-ERROR.
         THIS-OBJECT:GridArbLop:GuruRepositionTo(THIS-OBJECT:ekgArbLopKodTTh:ROWID).
         
      END.
      /*SKAPA SVEP*/
      IF arblopkomval = 4 THEN DO:
         ASSIGN 
         varp2arb = THIS-OBJECT:GridArbLop:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         varp2lop = THIS-OBJECT:GridArbLop:ActiveRow:Cells["SVEPLOPNR"]:VALUE.
         varp2arb = TRIM(varp2arb).
         varp2lop = TRIM(varp2lop).
         THIS-OBJECT:buttonOverArbKom:GuruOver().
         /*MINST TVÅ POSTER FÖR ETT SVEP*/
         IF THIS-OBJECT:buttonOverArbKom:AntalMarkerade() < 2 THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*AUTOPOST BORT*/
               ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
               qH:GET-NEXT().   
            END.
            THIS-OBJECT:GridKoppladeKom:GuruFirstrow().
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(174), "").
            RETURN FALSE. 
         END.
         /*ALLA POSTER MÅSTE HA SAMMA ARBKOD*/
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE ORDNING = 0 AND P2ARBKOD NE '" + varp2arb + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               /*AUTOPOST BORT*/
               ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
               qH:GET-NEXT().   
            END.
            THIS-OBJECT:GridKoppladeKom:GuruFirstrow().
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(175), "").
            RETURN FALSE. 
         END.
               
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF varp2arb = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE THEN DO:
               eekgKoppArbSvepTTh:BUFFER-CREATE().
               eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
               THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID .
            END.   
            /*AUTOPOST BORT*/
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().   
         END.
        
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            /*SKAPAR RÄTT SVEP LOPNR*/
            THIS-OBJECT:ControlDataDb:SvepLopNr(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
            qH:GET-NEXT().   
         END.
        
         fardigtsvep = THIS-OBJECT:ControlDataDb:SvepLopNrSlut().
         /*FINNS SVEPET SEN TIDIGARE*/
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" + fardigtsvep + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:GridKoppladeKom:GuruFirstrow().
            eekgKoppArbSvepTTh:EMPTY-TEMP-TABLE() NO-ERROR.
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(177), "").
            RETURN FALSE.
         END.
         qH:GET-FIRST().
         /*SKAPAR SVEP OCH ORDNING*/
         queryvar2 =  "FOR EACH " + THIS-OBJECT:ekgP2ValkomTTh:TABLE + " BY OORDNING".
         q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgP2ValkomTTh,queryvar2).
         q2H:GET-FIRST().     
         varp2ordning = 0.
         DO WHILE q2H:QUERY-OFF-END = FALSE:
            varp2ordning = varp2ordning + 1.
            ekgKoppArbSvepTTh:BUFFER-CREATE().
            ekgKoppArbSvepTTh:BUFFER-COPY(eekgKoppArbSvepTTh).
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:RECID .
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE =  fardigtsvep.
            ekgKoppArbSvepTTh:BUFFER-COPY(ekgP2ValkomTTh). 
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = varp2ordning.
            
            q2H:GET-NEXT().                       
         END.
         /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         THIS-OBJECT:GridKoppladeKom:GuruReopen().
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  fardigtsvep + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
      END.
      /*"Skapar nytt Svep med samma Kommentarer som valt Svep"*/
      IF arblopkomval = 5 THEN DO:
         ASSIGN 
         varp2arb = THIS-OBJECT:GridArbLop:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         varp2lop = THIS-OBJECT:GridArbLop:ActiveRow:Cells["SVEPLOPNR"]:VALUE.
         ASSIGN 
         varp2arb = TRIM(varp2arb)
         varp2lop = TRIM(varp2lop)
         sammap2arb = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)
         sammap2lop = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
         THIS-OBJECT:buttonOverArbKom:GuruOver().
         
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE ORDNING = 0 AND P2ARBKOD NE '" + varp2arb + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
               qH:GET-NEXT().   
            END.
            THIS-OBJECT:GridKoppladeKom:GuruReopen().
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(175), "").
            RETURN FALSE. 
         END.
               
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         
         DO WHILE qH:QUERY-OFF-END = FALSE:
            eekgKoppArbSvepTTh:BUFFER-CREATE().
            eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID .
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().   
         END.
          
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlDataDb:SvepLopNr(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 1.
            qH:GET-NEXT().   
         END.
         fardigtsvep = THIS-OBJECT:ControlDataDb:SvepLopNrSlut().
         qH:GET-FIRST().
         ekgKoppArbSvepTTh:BUFFER-CREATE().
         ekgKoppArbSvepTTh:BUFFER-COPY(eekgKoppArbSvepTTh).
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:RECID .
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE =  fardigtsvep.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE = -1.
         IF sammap2lop = "" THEN THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "Se " + sammap2arb.
         ELSE THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "Se " + sammap2arb + " . " + sammap2lop.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SEP2ARBKOD"):BUFFER-VALUE = sammap2arb.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SESVEPLOPNR"):BUFFER-VALUE = sammap2lop.
         /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  fardigtsvep + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
           THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
      END.
      /*Skapar nytt Svep med samma Kommentarer som vald ArbetsKod*/
      IF arblopkomval = 6 THEN DO:
         ASSIGN 
         varp2arb = THIS-OBJECT:GridArbLop:ActiveRow:Cells["P2ARBKOD"]:VALUE 
         varp2lop = THIS-OBJECT:GridArbLop:ActiveRow:Cells["SVEPLOPNR"]:VALUE.
         ASSIGN 
         varp2arb = TRIM(varp2arb)
         varp2lop = TRIM(varp2lop)
         sammap2arb = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE)
         sammap2lop = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
         THIS-OBJECT:buttonOverArbKom:GuruOver().
         
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE ORDNING = 0 AND P2ARBKOD NE '" + varp2arb + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
            qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
               qH:GET-NEXT().   
            END.
            THIS-OBJECT:GridKoppladeKom:GuruReopen().
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(175), "").
            RETURN FALSE. 
         END.
               
         queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE + " WHERE ORDNING = 0 ".
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         
         DO WHILE qH:QUERY-OFF-END = FALSE:
            eekgKoppArbSvepTTh:BUFFER-CREATE().
            eekgKoppArbSvepTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:eekgKoppArbSvepTTh:RECID .
            ekgKoppArbSvepTTh:BUFFER-DELETE() NO-ERROR. 
            qH:GET-NEXT().   
         END.
          
         queryvar =  "FOR EACH " + THIS-OBJECT:eekgKoppArbSvepTTh:TABLE.
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:eekgKoppArbSvepTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:ControlDataDb:SvepLopNr(THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
            THIS-OBJECT:eekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 1.
            qH:GET-NEXT().   
         END.
         fardigtsvep = THIS-OBJECT:ControlDataDb:SvepLopNrSlut().
         qH:GET-FIRST().
         ekgKoppArbSvepTTh:BUFFER-CREATE().
         ekgKoppArbSvepTTh:BUFFER-COPY(eekgKoppArbSvepTTh).
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:RECID .
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE =  fardigtsvep.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE = -1.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = "Se " + sammap2arb.
         THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SEP2ARBKOD"):BUFFER-VALUE = sammap2arb.
         /*Anders Olsson Elpool i Umeå AB  26 nov 2014 19:18:52 
         pLACERA MARKÖREN 
         */
         THIS-OBJECT:ekgKoppArbSvepTTh:FIND-LAST("WHERE P2ARBKOD = '" + varp2arb + "' AND SVEPLOPNR = '" +  fardigtsvep + "'") NO-ERROR.
         IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
            THIS-OBJECT:buttonOverArbKom:TomMark().
            THIS-OBJECT:buttonOverArbKom:GuruRadvaljareU(ekgKoppArbSvepTTh:ROWID).
         END.
      END.
      
      IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
         THIS-OBJECT:NyOrdning(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE,THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
      END.
      eekgP2ValkomTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      eekgKoppArbSvepTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      RETURN TRUE.
   END METHOD. 
   
   
   METHOD PUBLIC VOID SkapaKomResultat():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
      ekgP2KomResultTTh:EMPTY-TEMP-TABLE() NO-ERROR.
      
      queryvar =  "FOR EACH " + THIS-OBJECT:ekgKoppArbSvepTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ekgKoppArbSvepTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         ekgP2KomResultTTh:BUFFER-CREATE().
         ekgP2KomResultTTh:BUFFER-COPY(ekgKoppArbSvepTTh).
         /*
         THIS-OBJECT:ekgP2komTTh:FIND-FIRST("WHERE  KOMID = " + STRING(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE)) NO-ERROR.
         IF THIS-OBJECT:ekgP2komTTh:AVAILABLE THEN DO:
            THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
            THIS-OBJECT:ekgP2KomResultTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE.
         END.
          */     
         IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = 1 THEN DO:
            IF THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE = "" THEN DO:
               THIS-OBJECT:ekgP2KomResultTTh:BUFFER-FIELD("P2KOD"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE.
            END.   
            ELSE DO:
               THIS-OBJECT:ekgP2KomResultTTh:BUFFER-FIELD("P2KOD"):BUFFER-VALUE = THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + "     ." +
               THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE.
            END.   
         END.   
         qH:GET-NEXT().   
      END.   
      
      THIS-OBJECT:GridP2KomResult:GuruReopen().
      THIS-OBJECT:GridP2KomResult:GuruFirstrow().
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID SkapaNyttSvep_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 4.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	METHOD PUBLIC VOID StripDropDownButtonSvep():
	   IF THIS-OBJECT:GridArbLop:GuruFiltreraGet() = THIS-OBJECT:ArbLopFilter() THEN.
      ELSE THIS-OBJECT:GridArbLop:GuruFiltrera(THIS-OBJECT:ArbLopFilter()).
      IF arblopkomval = 3 THEN THIS-OBJECT:GridKoppladeKom:GuruFiltrera("SVEPLOPNR NE ''").
      ELSE DO:
         IF THIS-OBJECT:GridKoppladeKom:GuruFiltreraGet() NE "" THEN THIS-OBJECT:GridKoppladeKom:GuruFiltrera("").           
      END.   
   END METHOD.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID taBortP2kom_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		THIS-OBJECT:GridKom:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridKom:ActiveRow NE ? THEN DO:
         IF THIS-OBJECT:GridKom:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.
            
            {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:GridKom:Selected:Rows}
               THIS-OBJECT:ekgP2komTTh:FIND-FIRST(villkor) NO-ERROR.
               THIS-OBJECT:ekgKoppArbSvepTTh:FIND-FIRST("WHERE KOMID = " + STRING(THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE) ) NO-ERROR.
               IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN DO:
                  rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE + " " + THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE + " " + 
                  THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(178), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(179)).
                  RETURN.
               END.
               THIS-OBJECT:ekgP2ValkomTTh:FIND-FIRST("WHERE KOMID = " + STRING(THIS-OBJECT:ekgP2komTTh:BUFFER-FIELD("KOMID"):BUFFER-VALUE) ) NO-ERROR.
               IF THIS-OBJECT:ekgP2ValkomTTh:AVAILABLE THEN DO:
                  ekgP2ValkomTTh:BUFFER-DELETE().
               END.
                     
            END.   
            THIS-OBJECT:GridKom:GuruBortrader(FALSE).
            THIS-OBJECT:GridKomVal:GuruReopen(). 
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(73), "").  
      END.
		
		
		THIS-OBJECT:ControlWinLg:AndringP2kom().  
		RETURN.

	END METHOD.


	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonAter_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:splitContainerAllt:Panel1Collapsed = FALSE.
      THIS-OBJECT:splitContainerAllt:Panel2Collapsed = TRUE.
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonP2KomKoll_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO. 
		rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(201),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN DO:  
         THIS-OBJECT:ControlWinLg:SparaTab("p2Kom").                           
      END.         
      IF rrr:ToString() = "No" THEN DO:              
         THIS-OBJECT:ControlDataDb:P2KomKontroll().
         THIS-OBJECT:splitContainerAllt:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerAllt:Panel2Collapsed = FALSE.
         THIS-OBJECT:splitContainerVisaKontroll:Panel1Collapsed = TRUE.
         THIS-OBJECT:splitContainerVisaKontroll:Panel2Collapsed = FALSE.
         THIS-OBJECT:GridSvepKomKoll:GuruReopen().
         THIS-OBJECT:GridSvepKomKoll:GuruUpdateTitle().   
         RETURN.
      END.  
		
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonReadOnly_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		IF THIS-OBJECT:toolStripButtonReadOnly:Text = "Skrivskydd Bort" THEN DO:
		   THIS-OBJECT:GridKom:GuruReadOnly(FALSE,"ANMARKNING").
		   THIS-OBJECT:toolStripButtonReadOnly:Text = "Skrivskydd".
		END.   
		ELSE DO:
		   THIS-OBJECT:GridKom:GuruReadOnly(TRUE,"ANMARKNING").
         THIS-OBJECT:toolStripButtonReadOnly:Text = "Skrivskydd Bort".
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonVisa_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(TRUE).
		THIS-OBJECT:SkapaKomResultat().
      THIS-OBJECT:splitContainerAllt:Panel1Collapsed = TRUE.
      THIS-OBJECT:splitContainerAllt:Panel2Collapsed = FALSE.
      THIS-OBJECT:splitContainerVisaKontroll:Panel1Collapsed = FALSE.
      THIS-OBJECT:splitContainerVisaKontroll:Panel2Collapsed = TRUE.
      
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID UtokaBefSvep_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		arblopkomval = 3.
		THIS-OBJECT:StripDropDownButtonSvep().
		RETURN.

	END METHOD.

	METHOD PUBLIC VOID ValueGridKoppladeKomCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):
      
      IF THIS-OBJECT:ekgKoppArbSvepTTh:AVAILABLE THEN.
      ELSE RETURN.
            
	   IF arblopkomval = 3 THEN THIS-OBJECT:GridArbLop:GuruFiltrera(THIS-OBJECT:ArbLopFilter()).
	
	   THIS-OBJECT:ControlWinLg:RibbonToolAktivKod:Text = TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("P2ARBKOD"):BUFFER-VALUE).
	   /*
	   TRIM(THIS-OBJECT:ekgKoppArbSvepTTh:BUFFER-FIELD("SVEPLOPNR"):BUFFER-VALUE).
  */
   END METHOD.
	DESTRUCTOR PUBLIC EkgP2Kom ( ):

	END DESTRUCTOR.

END CLASS.