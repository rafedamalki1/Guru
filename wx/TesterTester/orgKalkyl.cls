 
 /*------------------------------------------------------------------------
    File        : Kalkyl.cls
    Purpose     : Make sure stuff works
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 12:57:57 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.
USING Microsoft.Office.Interop.Excel.*.

CLASS Modules.Kalkyl.Kalkyl INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE dontcheck AS LOGICAL INITIAL FALSE.
   DEFINE PUBLIC VARIABLE CurrentMatris AS INTEGER NO-UNDO.
   
   DEFINE PRIVATE VARIABLE GridPriser AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridFaktorer AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE ModuleHandle AS Guru.Module NO-UNDO.

   
   DEFINE PRIVATE VARIABLE GridFriKalk AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE Gr AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
   DEFINE PRIVATE VARIABLE matBtnOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE matBtnBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE GridMaterial AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridEgetMaterial AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE PassedOnMatris AS LOGICAL INITIAL FALSE NO-UNDO.
   
   DEFINE PUBLIC VARIABLE EgnaBtnOverMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaBtnOverAll AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaBtnBackMark AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgnaBtnBackAll AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenTaBort AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE EgenSkapa AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE egnakoderstrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridEgnaKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridLopKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKalkylKoder AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridArbetsKoder     AS Controls.Grid                                           NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridValdaUnderKoder AS Controls.Grid                                           NO-UNDO.
   DEFINE PUBLIC  VARIABLE GridUnderKoder      AS Controls.Grid                                           NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudUtfardare      AS Controls.Combo                                          NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudTyp            AS Controls.ComboSimpleLabel                               NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudOmrade         AS Controls.Combo                                          NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudKatalog        AS Controls.ComboSimple                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudKalknr         AS Controls.FillinInteger                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudBenamning      AS Controls.FillinChar                                     NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnsvarig       AS Controls.FillinChar                                     NO-UNDO.
   DEFINE PRIVATE VARIABLE HuvudAnmarkning     AS Infragistics.Win.UltraWinEditors.UltraTextEditor        NO-UNDO.
   DEFINE PRIVATE VARIABLE GridPersonal        AS Controls.Grid                                           NO-UNDO.
   /* ttHandles ifrån stuff */
   
   DEFINE         VARIABLE KalkDummy           AS LOGICAL                                                 INITIAL FALSE NO-UNDO.
   
   /* Kalkyl */
   DEFINE         VARIABLE HuvudTT             AS HANDLE                                                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE KalkylBtnOverMark   AS Controls.ButtonOver                                     NO-UNDO.
   DEFINE PUBLIC  VARIABLE KalkylBtnOverAll    AS Controls.ButtonOver                                     NO-UNDO.
   DEFINE PUBLIC  VARIABLE KalkylBtnBackMark   AS Controls.ButtonOver                                     NO-UNDO.
   DEFINE PUBLIC  VARIABLE KalkylBtnBackAll    AS Controls.ButtonOver                                     NO-UNDO.
   DEFINE         VARIABLE KatalogTT           AS HANDLE                                                  NO-UNDO.
   DEFINE         VARIABLE PriserTT            AS HANDLE                                                  NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer11 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer12 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer9     AS System.Windows.Forms.SplitContainer                     NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer10    AS System.Windows.Forms.SplitContainer                     NO-UNDO.
   DEFINE PRIVATE VARIABLE TabAvtalskalkyl     AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabEgna             AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFordelning       AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabImportMallar     AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFrikalkyl        AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFaktorer         AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkyl           AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager          AS Infragistics.Win.UltraWinTabControl.UltraTabControl     NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylHuvud      AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVolymber         AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa             AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabMateriel         AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripButton1 AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE         VARIABLE ValdaPriserTT       AS HANDLE                                                  NO-UNDO.
   DEFINE         VARIABLE LopposterTT         AS HANDLE                                                  NO-UNDO.
   DEFINE         VARIABLE ArbetskoderTT       AS HANDLE                                                  NO-UNDO.
   DEFINE         VARIABLE KoderTT             AS HANDLE                                                  NO-UNDO.
   /*DEFINE         VARIABLE FriKalkTT             AS HANDLE                                                  NO-UNDO.*/
   DEFINE         VARIABLE KalkSummaTT         AS HANDLE                                                  NO-UNDO.
   DEFINE TEMP-TABLE typtt NO-UNDO
      FIELD ATAT    AS CHARACTER
      FIELD TYPKALK AS INTEGER
      FIELD TYPC    AS CHARACTER LABEL "Kalkyltyp".
   
   
   /* Global */
   DEFINE         VARIABLE OmradeTT             AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE AnvandareTT          AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE PersonalTT           AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE LeverantorTT         AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE MaterialTT           AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE BestMaterialTT       AS HANDLE                                           NO-UNDO.
   DEFINE         VARIABLE MarkningTT           AS HANDLE                                           NO-UNDO.
   
   /* -- */
   DEFINE VARIABLE WasExcelShown AS LOGICAL INITIAL FALSE NO-UNDO.
   /* Drag & Drop*/
   DEFINE         VARIABLE DGKoder              AS Controls.Subclasses.DragDropGroup                NO-UNDO.
   DEFINE         VARIABLE DGEgna               AS Controls.Subclasses.DragDropGroup                NO-UNDO.
   DEFINE         VARIABLE DGMat                AS Controls.Subclasses.DragDropGroup                NO-UNDO.
   
   DEFINE PRIVATE VARIABLE cmb_avtal            AS Infragistics.Win.UltraWinGrid.UltraCombo         NO-UNDO.
   DEFINE PRIVATE VARIABLE cmb_artalavtal       AS Infragistics.Win.UltraWinGrid.UltraCombo         NO-UNDO.
   DEFINE PRIVATE VARIABLE cmb_KLGartal         AS Infragistics.Win.UltraWinGrid.UltraCombo         NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverMarkMallar AS Controls.ButtonOver                              NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonOverAllaMallar AS Controls.ButtonOver                              NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackMarkMallar AS Controls.ButtonOver                              NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBackAllaMallar AS Controls.ButtonOver                              NO-UNDO.
   DEFINE PRIVATE VARIABLE avtaltext            AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY kalknrvar AS INTEGER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY omradevar AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   DEFINE PUBLIC VARIABLE NyKalkyl AS LOGICAL NO-UNDO INITIAL FALSE.
   
   CONSTRUCTOR PUBLIC Kalkyl ( INPUT r AS Guru.Root ):
      SUPER ().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Root:Logger:WriteLine("Skapar kalkylobjekt..").
      InitializeComponent().
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Gör kopplingar till databasemanager..").
      
      THIS-OBJECT:PersonalTT = THIS-OBJECT:Root:DatabaseManager:Global:PersonalTT.
      THIS-OBJECT:AnvandareTT = THIS-OBJECT:Root:DatabaseManager:Global:AnvandareTT.
      THIS-OBJECT:OmradeTT = THIS-OBJECT:Root:DatabaseManager:Global:OmradeTT.
      THIS-OBJECT:LeverantorTT = THIS-OBJECT:Root:DatabaseManager:Global:LeverantorTT.
      THIS-OBJECT:MaterialTT = THIS-OBJECT:Root:DatabaseManager:Global:MaterialTT.
      
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC Kalkyl ( ):

   END DESTRUCTOR.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      /*THIS-OBJECT:bortkalkposteralla().*/      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(43), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderAll().
      THIS-OBJECT:KalkylBtnBackAll:GuruOver().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:FilterUnderKoder().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(43),THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveValdaAlla().
      THIS-OBJECT:EgnaBtnBackAll:GuruOver().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      /*THIS-OBJECT:bortkalkpostermark().*/
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      
      THIS-OBJECT:dontcheck = TRUE.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveKoderSelected().
      THIS-OBJECT:KalkylBtnBackMark:GuruOver().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridValdaUnderKoder:GuruReopen().
      THIS-OBJECT:dontcheck = FALSE.
      /*THIS-OBJECT:GridKalkylKoder:GuruReopen().*/
      /*
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      */
            RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonBackMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
      
      DEFINE VARIABLE tmp AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmp =  THIS-OBJECT:GridEgnaKoder:ActiveRow.
      
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridValdaUnderKoder:ActiveRow.
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveValdaSelected().
      THIS-OBJECT:EgnaBtnBackMark:GuruOver().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridValdaUnderKoder:GuruReopen().
      /*
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      IF tmp NE ? THEN 
      DO:
         THIS-OBJECT:GridEgnaKoder:GuruClearSelection().
         tmp:SELECTED = TRUE.
         tmp:Activate().
      END.
      IF tmpe NE ? THEN 
      DO:
         /*THIS-OBJECT:valdaprisposter:GuruClearSelection().*/
         tmpe:SELECTED = TRUE.
         tmpe:Activate().
      END.
      */
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverAlla_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      THIS-OBJECT:KalkylBtnOverAll:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKoderRows().
      THIS-OBJECT:KalkylBtnOverAll:GuruOverefteruppdat().
     
      /*
       THIS-OBJECT:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:GridKalkylKoder:GuruClearSelection().
      THIS-OBJECT:GridKalkylKoder:GuruSelectNewRows().
      */
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverAllaPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:EgnaBtnOverAll:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateValdaRows().
      THIS-OBJECT:EgnaBtnOverAll:GuruOverefteruppdat().
      THIS-OBJECT:GridValdaUnderKoder:GuruReopen().
      /*THIS-OBJECT:buttonOverMarkPriser:GuruRadvaljare().*/
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOvermark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:KalkylBtnOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKoderRows().
      THIS-OBJECT:KalkylBtnOverMark:GuruOverefteruppdat().
      
      RETURN.

   END METHOD.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverMarkPriser_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE tmpe AS Infragistics.Win.UltraWinGrid.UltraGridRow.
      tmpe =  THIS-OBJECT:GridEgnaKoder:ActiveRow.     
     
      THIS-OBJECT:EgnaBtnOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateValdaRows().
      THIS-OBJECT:EgnaBtnOverMark:GuruOverefteruppdat().     
      THIS-OBJECT:GridValdaUnderKoder:GuruReopen().      
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      RETURN.

   END METHOD.





   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgetMaterial_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridMaterial THEN 
      DO:
         THIS-OBJECT:matBtnOverMark_Click(?,?).
      END.
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridEgnaKoder_AfterCellActivate( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:GridEgnaKoder:ActiveRow = ? THEN RETURN.
		FilterUnderKoder().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridEgnaKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
         THIS-OBJECT:KoderTT:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:Value ), NO-LOCK).
         DEFINE VARIABLE kolnamn AS CHARACTER NO-UNDO.
         kolnamn = e:Cell:Column:ToString().
         
         DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
         kolvarde = e:Cell:Value:ToString().
         THIS-OBJECT:KoderTT:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         
         THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().
         THIS-OBJECT:GridEgnaKoder:GuruReopen().
         THIS-OBJECT:GridKalkylKoder:GuruReopen().
      RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterRowActivate( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      FilterUnderKoder().

   END METHOD.
   
   METHOD PUBLIC VOID FilterUnderKoder():
      /* Om det finns en aktiv row..*/
     
      IF THIS-OBJECT:GridEgnaKoder:tBS:Count > 0 AND THIS-OBJECT:GridEgnaKoder:ActiveRow NE ? THEN DO:

         /* Uppdatera valda underkoderna, och återställ kontrollerna*/
         IF TRIM(STRING(THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:Value)) = "" THEN RETURN.
         IF THIS-OBJECT:dontcheck = FALSE THEN DO:
         
            THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera( "NUM = " + STRING( THIS-OBJECT:GridEgnaKoder:ActiveRow:Cells["NUM"]:Value )).
            THIS-OBJECT:GridValdaUnderKoder:GuruFirstrow().
         
            THIS-OBJECT:GridValdaUnderKoder:Enabled = TRUE.
            THIS-OBJECT:EgnaBtnOverAll:Enabled = TRUE.
            THIS-OBJECT:EgnaBtnOverMark:Enabled = TRUE.
            THIS-OBJECT:EgnaBtnBackAll:Enabled = TRUE.
            THIS-OBJECT:EgnaBtnBackMark:Enabled = TRUE.
         END.  
      END.
      ELSE DO:
         /* Annars disablea kontrollerna och filtrera efter något dumt som döljer rubbet */
         THIS-OBJECT:GridValdaUnderKoder:GuruFiltrera("NUM = -1").
         THIS-OBJECT:GridValdaUnderKoder:Enabled = FALSE.
         THIS-OBJECT:EgnaBtnOverAll:Enabled = FALSE.
         THIS-OBJECT:EgnaBtnOverMark:Enabled = FALSE.
         THIS-OBJECT:EgnaBtnBackAll:Enabled = FALSE.
         THIS-OBJECT:EgnaBtnBackMark:Enabled = FALSE.
      END.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridEgnaKoder_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
      THIS-OBJECT:FilterUnderKoder().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridFriKalk_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolvarde = e:Cell:Value:ToString().

      THIS-OBJECT:ValdaPriserTT:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKodSub().    
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKalkylKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      THIS-OBJECT:KoderTT:FIND-FIRST("WHERE NUM = " + STRING( THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:Value ), NO-LOCK).
      DEFINE VARIABLE kolnamn AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolvarde = e:Cell:Value:ToString().

      THIS-OBJECT:KoderTT:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:UpdateKod().    
      RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKalkylKoder_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
		DEFINE VARIABLE num AS INTEGER NO-UNDO.
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN DO:
		   IF THIS-OBJECT:GridFriKalk:GuruEgenskap NE ? THEN DO:
		      IF THIS-OBJECT:GridKalkylKoder:ActiveRow NE ? THEN DO:
   		      IF THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells NE ? THEN DO:
                  num = THIS-OBJECT:GridKalkylKoder:ActiveRow:Cells["NUM"]:Value.
                  THIS-OBJECT:GridFriKalk:GuruFiltrera("NUM = " + STRING(num)).
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
   METHOD PRIVATE VOID GridKalkylKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridLopKoder THEN 
      DO:
         THIS-OBJECT:buttonOvermark_Click(?,?).
      END.

      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridLopKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridKalkylKoder THEN 
      DO:
         THIS-OBJECT:buttonBackmark_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridValdaUnderKoder THEN 
      DO:
         THIS-OBJECT:buttonBackmarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridValdaUnderKoder_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE ddArgs AS Controls.Subclasses.DragDropArgs NO-UNDO.
      ddArgs = CAST(e, Controls.Subclasses.DragDropArgs).
      IF ddArgs:Grid = THIS-OBJECT:GridUnderKoder THEN 
      DO:
         THIS-OBJECT:buttonOvermarkPriser_Click(?,?).
      END.
      RETURN.

   END METHOD.

	
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID HuvudTyp_GuruValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:UpdateRibbonShow().
      RETURN.

   END METHOD.

   METHOD PUBLIC LOGICAL Initialize():
      /*  THIS-OBJECT:VisaExcel:Initialize().*/
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Länkar control till Root.").
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:Control = THIS-OBJECT.
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Skapar ribbon.").
      THIS-OBJECT:CreateRibbon().

      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Uppdaterar typvisningar.").
      THIS-OBJECT:InitRibbonVisaTyp().
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Uppdaterar kodvisningar.").
      THIS-OBJECT:UpdateRibbonShow().
      
      RETURN TRUE.
   END METHOD.
   



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID matBtnBackMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:matBtnBackMark:GuruOver().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID matBtnOverMark_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      /*
      THIS-OBJECT:GridEgetMaterial:GuruSaveOldRows().
       */
      THIS-OBJECT:matBtnOverMark:GuruOver().
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateBestMaterial(THIS-OBJECT:kalknrvar).
      THIS-OBJECT:matBtnOverMark:GuruOverefteruppdat().
            /*
      THIS-OBJECT:GridMaterial:GuruReopen().
      */
      
  /*THIS-OBJECT:GridEgetMaterial:GuruReopen().
      THIS-OBJECT:GridEgetMaterial:GuruClearSelection().
      THIS-OBJECT:GridEgetMaterial:GuruSelectNewRows().
    */  
      RETURN.

   END METHOD.



	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButton1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ApplyLanguage().
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID UpdateMatris():
     
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
      mat = 1.
      IF THIS-OBJECT:RibbonToolMatris:Text = "Alla" THEN MAT = -1.
      IF THIS-OBJECT:RibbonToolMatris:Text = GetMatrisName() + " 1" THEN MAT = 1.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 2" THEN MAT = 2.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 3" THEN MAT = 3.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 4" THEN MAT = 4.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 5" THEN MAT = 5.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 6" THEN MAT = 6.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 7" THEN MAT = 7.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 8" THEN MAT = 8.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 9" THEN MAT = 9.
      IF THIS-OBJECT:RibbonToolMatris:TEXT = GetMatrisName() + " 10" THEN MAT = 10.
      THIS-OBJECT:CurrentMatris = mat.
      IF THIS-OBJECT:NyKalkyl = FALSE THEN 
      DO:
         IF MAT = -1 THEN 
         DO:
            
            /*CFREDRIK*/
            THIS-OBJECT:GridEgetMaterial:GuruFiltrera("").
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("").
   
         END.
         ELSE 
         DO:
            THIS-OBJECT:GridEgetMaterial:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            THIS-OBJECT:KalkylBtnOverAll:Enabled = TRUE.
            THIS-OBJECT:KalkylBtnOverMark:Enabled = TRUE.
            THIS-OBJECT:EgenSkapa:Enabled = TRUE.
            THIS-OBJECT:GridEgnaKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)+ " AND ARBKOD = 'EGEN'" ).
            THIS-OBJECT:GridKalkylKoder:GuruFiltrera("MATRIS = " + STRING(THIS-OBJECT:CurrentMatris)).
            
            
         END.
         
         THIS-OBJECT:GridEgnaKoder:GuruFirstRow().
         THIS-OBJECT:FilterUnderKoder().
      END.      
   END METHOD.
   
   DEFINE VARIABLE WasMatrisRibbonCreated AS LOGICAL INITIAL FALSE NO-UNDO.
   
   METHOD PUBLIC VOID CreateRibbonMatris():
      IF THIS-OBJECT:WasMatrisRibbonCreated THEN RETURN.
      THIS-OBJECT:RibbonToolMatris = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("KodMatris").
      THIS-OBJECT:RibbonToolMatris:SharedPropsInternal:Caption = GetMatrisName() + ":".
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add("Alla").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 1").
      THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 2").
      IF HuvudTT:Buffer-Field("TYPKALK"):BUFFER-VALUE NE 7 THEN 
      DO:
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 3").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 4").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 5").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 6").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 7").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 8").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 9").
         THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems:Add(GetMatrisName() + " 10").
      END.
      THIS-OBJECT:RibbonToolMatris:Value = THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems[0].
      THIS-OBJECT:RibbonToolMatris:Key = "KalkylRibbonMatris".
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolMatris).
      THIS-OBJECT:RibbonTabKoder:Tools:Add( THIS-OBJECT:RibbonToolMatris ).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolMatris).
      THIS-OBJECT:RibbonToolMatris:SelectedIndex = 0.
      THIS-OBJECT:CurrentMatris = -1.
      THIS-OBJECT:WasMatrisRibbonCreated = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID CreateRibbon():
      /* Skapa ribbongrupp i kalkyltaben */
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule(THIS-OBJECT:Text).
      THIS-OBJECT:RibbonTabKalkyl = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkyl " + STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)).
      THIS-OBJECT:RibbonTabKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Koder").
      THIS-OBJECT:RibbonTabMat = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Sök material").
      THIS-OBJECT:RibbonTabVisaKoder = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visning av koder").
      THIS-OBJECT:RibbonTabVisaKoder:Visible = FALSE.
      THIS-OBJECT:RibbonTabKalkVisning = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Kalkylvisning").
      
      
      /* Skapa verktyg*/

      /* kalkyl*/
      THIS-OBJECT:RibbonToolInfTyp = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKalkTyp").
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Caption = "Kalkyltyp:".
      /*THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Enabled = False.*/
      THIS-OBJECT:RibbonToolInfTyp:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      /*
      THIS-OBJECT:RibbonToolInfTyp:Text = THIS-OBJECT:HuvudTyp:GuruCombo:Text.
      */
      THIS-OBJECT:RibbonToolInfTyp:SharedPropsInternal:Width = 192.
      
      THIS-OBJECT:RibbonToolOmrade = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoOmrade").
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Caption = Guru.Konstanter:gomrk.
      /*THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Enabled = False.*/
      THIS-OBJECT:RibbonToolOmrade:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
      THIS-OBJECT:RibbonToolOmrade:SharedPropsInternal:Width = 192.
      
      THIS-OBJECT:RibbonToolKat = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoKatalog").
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Caption = "Katalog:".
      /*THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Enabled = False.*/
      THIS-OBJECT:RibbonToolKat:ToolKeyPress:Subscribe(THIS-OBJECT:Root:BlockInput).
      THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
      THIS-OBJECT:RibbonToolKat:SharedPropsInternal:Width = 192.
      
      /* Koder */


      /*THIS-OBJECT:RibbonToolOpto:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.*/
   
      

      
      
      THIS-OBJECT:RibbonToolFriKalk = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("FriKalkActive").
     /* THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:*/
      THIS-OBJECT:RibbonToolFriKalk:SharedPropsInternal:Caption = "Frikalkyl-läge".
      THIS-OBJECT:RibbonToolFriKalk:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      /* Visning */
      THIS-OBJECT:RibbonToolGenerera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaGen").
      THIS-OBJECT:RibbonToolGenerera:SharedPropsInternal:Caption = "Generera kalkyl".
      
      THIS-OBJECT:RibbonToolSkrivUt = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("VisaPrint").
      THIS-OBJECT:RibbonToolSkrivUt:SharedPropsInternal:Caption = "Skriv Ut".
      
      /* Material */
      THIS-OBJECT:RibbonToolLev = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("MatLever").
      THIS-OBJECT:RibbonToolLev:SharedPropsInternal:Caption = "Leverantör:".
      /* Använd detta för inladdning = THIS-OBJECT:RibbonToolMatris:ValueList:ValueListItems[1]:DataValue */
      THIS-OBJECT:RibbonToolEnr = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatEnr").
      THIS-OBJECT:RibbonToolEnr:SharedPropsInternal:Caption = Guru.Konstanter:genk.
      THIS-OBJECT:RibbonToolSok = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("MatSoek").
      THIS-OBJECT:RibbonToolSok:SharedPropsInternal:Caption = "Sök:".
      
      
      /* Förbered för ribbon */
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolInfTyp).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolOmrade).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolKat).           
      

                  
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolLev).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnr).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSok).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkrivUt).
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFriKalk).
      
      
      
      /* Lägg till verktygen i gruppen*/
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolInfTyp).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolOmrade).
      THIS-OBJECT:RibbonTabKalkyl:Tools:Add ( THIS-OBJECT:RibbonToolKat).
      

      THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFriKalk).
      
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolLev).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolEnr).
      THIS-OBJECT:RibbonTabMat:Tools:Add(THIS-OBJECT:RibbonToolSok).
      
      THIS-OBJECT:RibbonTabKalkVisning:Tools:Add(THIS-OBJECT:RibbonToolGenerera).
      THIS-OBJECT:RibbonTabKalkVisning:Tools:Add(THIS-OBJECT:RibbonToolSkrivUt).
      
      /* binda events*/
      
      THIS-OBJECT:RibbonToolSok:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).
      THIS-OBJECT:RibbonToolEnr:ToolKeyPress:Subscribe(THIS-OBJECT:RibbonSokUpdate).

      THIS-OBJECT:RibbonToolGenerera:ToolClick:Subscribe(THIS-OBJECT:GenereraKalkylVisningE).
      THIS-OBJECT:RibbonToolSkrivUt:ToolClick:Subscribe(THIS-OBJECT:SkrivUtE).
      
      THIS-OBJECT:RibbonToolFriKalk:ToolClick:Subscribe(THIS-OBJECT:FriKalkChanged).

      
      THIS-OBJECT:LoadRibbonLevs().

   END METHOD.

   /* Ribbongrupper */
   DEFINE         VARIABLE RibbonTabKalkyl      AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE         VARIABLE RibbonTabKoder       AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE         VARIABLE RibbonTabMat         AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE         VARIABLE RibbonTabVisaKoder   AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   DEFINE         VARIABLE RibbonTabKalkVisning AS Infragistics.Win.UltraWinToolbars.RibbonGroup     NO-UNDO.
   
   
   /* Ribbonkontroller */
   /* info */
   DEFINE         VARIABLE RibbonToolInfTyp     AS Infragistics.Win.UltraWinToolbars.TextBoxTool     NO-UNDO.
   DEFINE         VARIABLE RibbonToolOmrade     AS Infragistics.Win.UltraWinToolbars.TextBoxTool     NO-UNDO.
   DEFINE         VARIABLE RibbonToolKat        AS Infragistics.Win.UltraWinToolbars.TextBoxTool     NO-UNDO.
   /* Koder */
   DEFINE PUBLIC  VARIABLE RibbonToolMatris     AS Infragistics.Win.UltraWinToolbars.ComboBoxTool    NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolVisaTyp    AS Infragistics.Win.UltraWinToolbars.ComboBoxTool    NO-UNDO.
   DEFINE PUBLIC VARIABLE RibbonToolFilterTyp   AS Infragistics.Win.UltraWinToolbars.ComboBoxTool    NO-UNDO.

   DEFINE PUBLIC VARIABLE RibbonToolFriKalk     AS Infragistics.Win.UltraWinToolbars.StateButtonTool NO-UNDO.

   /* visning */
   DEFINE PUBLIC  VARIABLE RibbonToolGenerera   AS Infragistics.Win.UltraWinToolbars.ButtonTool      NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkrivUt    AS Infragistics.Win.UltraWinToolbars.ButtonTool      NO-UNDO.

   /* Material */
   DEFINE PUBLIC  VARIABLE RibbonToolLev        AS Infragistics.Win.UltraWinToolbars.ComboBoxTool    NO-UNDO.
   DEFINE         VARIABLE RibbonToolEnr        AS Infragistics.Win.UltraWinToolbars.TextBoxTool     NO-UNDO.
   DEFINE         VARIABLE RibbonToolSok        AS Infragistics.Win.UltraWinToolbars.TextBoxTool     NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelEmpty       AS System.Windows.Forms.Panel                        NO-UNDO.
   DEFINE PRIVATE VARIABLE VisaPanelWaiting     AS System.Windows.Forms.Panel                        NO-UNDO.
   DEFINE         VARIABLE WasVisaTypInit       AS LOGICAL                                           INITIAL FALSE NO-UNDO.

   
   METHOD PUBLIC VOID FriKalkChanged(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      
      GridKalkylKoder_AfterSelectChange(?,?).
      IF THIS-OBJECT:RibbonToolFriKalk:Checked EQ TRUE THEN 
      DO:
         THIS-OBJECT:splitContainer11:Panel2Collapsed = FALSE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = FALSE.
       THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = FALSE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = FALSE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:splitContainer11:Panel2Collapsed = TRUE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.
         THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
       THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID SkrivUtE(INPUT sender AS System.Object, INPUT args AS System.EventArgs):
      THIS-OBJECT:VisaExcel:Print().
   END METHOD.
   
   METHOD PUBLIC VOID InitRibbonVisaTyp():
      IF THIS-OBJECT:WasVisaTypInit = FALSE THEN DO:
         THIS-OBJECT:RibbonToolVisaTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("VisaTyp").
         THIS-OBJECT:RibbonToolVisaTyp:SharedPropsInternal:Caption = "Visa för typ:".
         THIS-OBJECT:RibbonToolVisaTyp:SharedProps:Visible = TRUE.
         THIS-OBJECT:RibbonToolVisaTyp:Key = "KalkylRibbonVisaTyp". 
         DEFINE VARIABLE wTemp AS Infragistics.Win.ValueListItem NO-UNDO.
         
         FOR EACH typtt:
            wTemp = NEW Infragistics.Win.ValueListItem().
            wTemp:DisplayText = typtt.TYPC.
            wTemp:DataValue = NEW Helpers.ABLDataContainer().
            CAST(wTemp:DataValue, Helpers.ABLDataContainer):CreateData("Typ", typtt.TYPKALK).
            THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Add(wTemp).
         END. 

         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolVisaTyp).
         THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolVisaTyp).
        
         THIS-OBJECT:RibbonToolVisaTyp:ToolValueChanged:Subscribe(RibbonUpdateTyp).
         
         
         THIS-OBJECT:RibbonToolFilterTyp = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("FilterTyp").
         THIS-OBJECT:RibbonToolFilterTyp:SharedPropsInternal:Caption = "Filtertyp".

         THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFilterTyp).
         THIS-OBJECT:RibbonTabKoder:Tools:Add(THIS-OBJECT:RibbonToolFilterTyp).
         
         THIS-OBJECT:RibbonToolFilterTyp:ToolValueChanged:Subscribe(RefreshArbKoder).
         
         
      END.
      THIS-OBJECT:WasVisaTypInit = TRUE.
   END METHOD.
   
   DEFINE PUBLIC VARIABLE WasMarkningMade AS LOGICAL INITIAL FALSE NO-UNDO.
   
   METHOD PUBLIC VOID RefreshMarkning():
        
      
      IF THIS-OBJECT:WasVisaTypInit = TRUE THEN DO:

         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Clear().
         
        
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Alla").
         THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add("Ej märkta").
         
         
         /*THIS-OBJECT:MarkningTT:FIND-FIRST().*/
         
         DEFINE VARIABLE mQ AS HANDLE    NO-UNDO.
         DEFINE VARIABLE Q  AS CHARACTER NO-UNDO.
         Q = "FOR EACH " + THIS-OBJECT:MarkningTT:TABLE + " WHERE TYP = " + STRING(THIS-OBJECT:GetVisaTyp()) + " NO-LOCK".
         CREATE QUERY mQ.
         mQ:SET-BUFFERS(THIS-OBJECT:MarkningTT).
         mQ:QUERY-PREPARE(Q).
         mQ:QUERY-OPEN().
         mQ:GET-FIRST().
         DO WHILE mQ:QUERY-OFF-END = FALSE:
            THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems:Add(THIS-OBJECT:MarkningTT:BUFFER-FIELD("MARKNING"):BUFFER-VALUE).
            mQ:GET-NEXT().            
         END.
         
         mQ:QUERY-CLOSE().
         THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[1].

         THIS-OBJECT:RefreshArbKoder(?,?).
         THIS-OBJECT:WasMarkningMade = TRUE.
      END.
      
   END METHOD.
   
   METHOD PUBLIC CHARACTER GetVisaMarkning():
      RETURN THIS-OBJECT:RibbonToolFilterTyp:Text.
   END METHOD.
   
   METHOD PUBLIC VOID RefreshArbKoder(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE Typ AS INTEGER NO-UNDO INITIAL 1.
      Typ = THIS-OBJECT:GetVisaTyp().
      
      DEFINE VARIABLE Markning AS CHARACTER NO-UNDO.
      Markning = TRIM(THIS-OBJECT:GetVisaMarkning()).
      
      DEFINE VARIABLE DoAll AS LOGICAL NO-UNDO.
      IF Markning EQ "Alla" OR Markning EQ "" THEN DoAll = TRUE.
      ELSE DoAll = FALSE.
      
      DEFINE VARIABLE DoUnmarked AS LOGICAL NO-UNDO.
      IF Markning EQ "Ej märkta" THEN DoUnmarked = TRUE.
      ELSE DoUnmarked = FALSE.
         
         
      DEFINE VARIABLE DoReturn AS LOGICAL INITIAL FALSE NO-UNDO.
      
      IF DoAll EQ TRUE THEN DO:
         THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ)).
         THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
      END.
      ELSE DO:
         IF DoUnmarked EQ TRUE THEN DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = ''").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         ELSE DO:
            THIS-OBJECT:GridArbetsKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND MARKNING = '" + Markning + "'").
            THIS-OBJECT:GridArbetsKoder:GuruFirstrow().
         END.
         
         IF THIS-OBJECT:WasMarkningMade EQ FALSE THEN DO:
            IF THIS-OBJECT:GridArbetsKoder:tBS:Count EQ 0 THEN DO:
               THIS-OBJECT:RibbonToolFilterTyp:SelectedItem = THIS-OBJECT:RibbonToolFilterTyp:ValueList:ValueListItems[0].
               DoReturn = TRUE.
            END.
         END.

      END.
      
      IF DoReturn EQ TRUE THEN RETURN.
      
      IF THIS-OBJECT:ArbetsKoderTT:AVAILABLE EQ TRUE THEN DO:
         
         THIS-OBJECT:LopposterTT:FIND-FIRST("WHERE TYPKALK = " + STRING(Typ)) NO-ERROR.
         THIS-OBJECT:GridLopKoder:GuruFiltrera("TYPKALK = " + STRING(Typ) + " AND ARBKOD = '" + STRING(THIS-OBJECT:ArbetskoderTT:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + "'").
         THIS-OBJECT:GridLopKoder:GuruReopen().
      END.
      ELSE DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("TYPKALK = ? AND ARBKOD = ?").
         THIS-OBJECT:GridLopKoder:GuruReopen().
         
         System.Windows.Forms.MessageBox:Show("Inga arbetskoder funna!").
      END.

   END METHOD.
   
   METHOD PUBLIC VOID UpdateRibbonShow():
     
      IF THIS-OBJECT:WasVisaTypInit THEN DO:
         THIS-OBJECT:RibbonToolInfTyp:Text = THIS-OBJECT:HuvudTyp:GuruCombo:Text.
     
     
         DEFINE VARIABLE i AS INTEGER NO-UNDO.
         i = 0.
        
         DO WHILE i LE THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count :
           
            IF  THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[i]:DisplayText = THIS-OBJECT:RibbonToolInfTyp:Text THEN 
            DO:
               THIS-OBJECT:RibbonToolVisaTyp:SelectedItem = THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[i].
               RETURN.
            END.
           
            i = i + 1.
         END.
      END.
     
   END METHOD.
   
   METHOD PUBLIC VOID LoadRibbonLevs():
      THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Clear().
      DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:LeverantorTT, "FOR EACH " + THIS-OBJECT:LeverantorTT:TABLE + " NO-LOCK").
      qH:GET-FIRST().
      DEFINE VARIABLE sw AS Integer NO-UNDO INITIAL 0.
      DEFINE VARIABLE ssw AS Integer NO-UNDO.
      DO WHILE qh:QUERY-OFF-END = FALSE:
         DEFINE VARIABLE w     AS Infragistics.Win.ValueListItem NO-UNDO.
         DEFINE VARIABLE wData AS Helpers.ABLDataContainer  NO-UNDO.

         w = NEW Infragistics.Win.ValueListItem().
         w:DisplayText = STRING(THIS-OBJECT:LeverantorTT:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE()).

         wData = NEW Helpers.ABLDataContainer().

         wData:CreateData("LEVKOD", STRING(THIS-OBJECT:LeverantorTT:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE())).

         w:DataValue = wData.
         IF STRING(THIS-OBJECT:LeverantorTT:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE()) = THIS-OBJECT:Root:DatabaseManager:Global:HuvudLeverantor THEN DO:
            wData:CreateData("ISHUV", TRUE).
            ssw = sw.
         END.
         ELSE DO:
            wData:CreateData("ISHUV", FALSE).
         END.
         
         
         THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems:Add(w).
         sw = sw + 1 . 
         qH:GET-NEXT(NO-LOCK).
      END.
      THIS-OBJECT:RibbonToolLev:SelectedIndex = ssw.
      THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
   END METHOD.
   
   /* Ribbon events*/
   METHOD PUBLIC VOID RibbonSokUpdate(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
      IF e:KeyChar  = CHR(13) THEN 
      DO:
         DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
         DEFINE VARIABLE enr AS CHARACTER NO-UNDO.
         DEFINE VARIABLE ben AS CHARACTER NO-UNDO.
         
         lev = STRING( CAST(THIS-OBJECT:RibbonToolLev:ValueList:ValueListItems[THIS-OBJECT:RibbonToolLev:SelectedIndex]:DataValue, Helpers.ABLDataContainer ):GetCharacter("LEVKOD")). /*THIS-OBJECT:MatLeverantor:GuruCombo:SelectedRow:GetCellValue("LEVKOD").*/
         enr = THIS-OBJECT:RibbonToolEnr:Text.
         ben = THIS-OBJECT:RibbonToolSok:Text.
         THIS-OBJECT:Root:DatabaseManager:Global:FetchMaterial(lev, enr, ben).
      
         THIS-OBJECT:GridMaterial:GuruReopen().
         e:Handled = TRUE.
      END.
   END METHOD.


   METHOD PUBLIC INTEGER GetVisaTyp():
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.
      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 AND THIS-OBJECT:WasVisaTypInit = TRUE THEN DO:
         RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex]:DataValue, Helpers.ABLDataContainer ):GetInteger("Typ").
      END.
      RETURN -1.
   END METHOD.

   METHOD PUBLIC VOID RibbonUpdateTyp(sender AS System.Object, e AS System.EventArgs):
      IF THIS-OBJECT:GridArbetsKoder:Initierad = FALSE THEN RETURN.
      IF THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex < 0  THEN THIS-OBJECT:RibbonToolVisaTyp:SelectedIndex = 0.

      IF THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems:Count > 0 AND THIS-OBJECT:WasVisaTypInit = TRUE THEN DO:
         THIS-OBJECT:RefreshMarkning().


      END.
      
   END METHOD.
     
      
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
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
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
      appearance8 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
      appearance19 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
      appearance10 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
      appearance9 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
      appearance15 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
      appearance14 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.Kalkyl").
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
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
      appearance4 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
      appearance3 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab16 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab16 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
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
      THIS-OBJECT:VisaExcelTab = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaPanelEmpty = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:VisaPanelWaiting = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
      THIS-OBJECT:VisaExcel = NEW Controls.GuruExcelEmbed().
      THIS-OBJECT:ultraTabPageControl16 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaIEBrowser = NEW System.Windows.Forms.WebBrowser().
      THIS-OBJECT:ultraTabPageControl17 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:VisaPDFBrowser = NEW System.Windows.Forms.WebBrowser().
      THIS-OBJECT:TabKalkylHuvud = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:HuvudKalknr = NEW Controls.FillinInteger().
      THIS-OBJECT:HuvudUtfardare = NEW Controls.Combo().
      THIS-OBJECT:HuvudAnmarkning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:Labelanm = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudAnsvarig = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudKatalog = NEW Controls.ComboSimple().
      THIS-OBJECT:Labelomr = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudBenamning = NEW Controls.FillinChar().
      THIS-OBJECT:HuvudOmrade = NEW Controls.Combo().
      THIS-OBJECT:Labelutf = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:HuvudTyp = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:Labelkatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:GridPersonal = NEW Controls.Grid().
      THIS-OBJECT:TabKalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer6 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainer7 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbetsKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainer8 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridLopKoder = NEW Controls.Grid().
      THIS-OBJECT:KalkylBtnOverAll = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylBtnBackAll = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylBtnOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:KalkylBtnBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainer11 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKalkylKoder = NEW Controls.Grid().
      THIS-OBJECT:GridFriKalk = NEW Controls.Grid().
      THIS-OBJECT:TabEgna = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainer5 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:egnakoderstrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:EgenSkapa = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:EgenTaBort = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripButton1 = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridEgnaKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainer3 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainer4 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:EgnaBtnOverAll = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaBtnBackAll = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaBtnOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:EgnaBtnBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridValdaUnderKoder = NEW Controls.Grid().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:ultraTabControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabFaktorer = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer12 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridFaktorer = NEW Controls.Grid().
      THIS-OBJECT:GridPriser = NEW Controls.Grid().
      THIS-OBJECT:TabMateriel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainer9 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainer10 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridMaterial = NEW Controls.Grid().
      THIS-OBJECT:matBtnOverMark = NEW Controls.ButtonOver().
      THIS-OBJECT:matBtnBackMark = NEW Controls.ButtonOver().
      THIS-OBJECT:GridEgetMaterial = NEW Controls.Grid().
      THIS-OBJECT:TabImportMallar = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:buttonBackAllaMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonBackMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverAllaMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverMarkMallar = NEW Controls.ButtonOver().
      THIS-OBJECT:mallarkoder = NEW Controls.Grid().
      THIS-OBJECT:valdamallar = NEW Controls.Grid().
      THIS-OBJECT:mallar = NEW Controls.Grid().
      THIS-OBJECT:TabAvtalskalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:LabelAvtal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_avtal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:Labelartalskj = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_artalavtal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:avtaltext = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:Labelklgartal = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:cmb_KLGartal = NEW Infragistics.Win.UltraWinGrid.UltraCombo().
      THIS-OBJECT:TabFrikalkyl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:Gr = NEW Controls.Grid().
      THIS-OBJECT:TabFordelning = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabVolymber = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:VisaExcelTab:SuspendLayout().
      THIS-OBJECT:VisaPanelWaiting:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl16:SuspendLayout().
      THIS-OBJECT:ultraTabPageControl17:SuspendLayout().
      THIS-OBJECT:TabKalkylHuvud:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkyl:SuspendLayout().
      THIS-OBJECT:splitContainer6:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer6:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer6:SuspendLayout().
      THIS-OBJECT:splitContainer7:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer7:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer7:SuspendLayout().
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer8:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer8:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer8:SuspendLayout().
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer11:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer11:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer11:SuspendLayout().
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabEgna:SuspendLayout().
      THIS-OBJECT:splitContainer2:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer2:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer2:SuspendLayout().
      THIS-OBJECT:splitContainer5:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer5:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer5:SuspendLayout().
      THIS-OBJECT:egnakoderstrip:SuspendLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer3:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer3:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer3:SuspendLayout().
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer4:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer4:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer4:SuspendLayout().
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabVisa:SuspendLayout().
      CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:ultraTabControl2:SuspendLayout().
      THIS-OBJECT:TabFaktorer:SuspendLayout().
      THIS-OBJECT:splitContainer12:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer12:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer12:SuspendLayout().
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabMateriel:SuspendLayout().
      THIS-OBJECT:splitContainer9:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer9:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer9:SuspendLayout().
      THIS-OBJECT:splitContainer10:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer10:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer10:SuspendLayout().
      CAST(THIS-OBJECT:GridMaterial, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridEgetMaterial, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabImportMallar:SuspendLayout().
      CAST(THIS-OBJECT:mallarkoder, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:valdamallar, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:mallar, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabAvtalskalkyl:SuspendLayout().
      CAST(THIS-OBJECT:cmb_avtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmb_artalavtal, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:avtaltext, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:cmb_KLGartal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabFrikalkyl:SuspendLayout().
      CAST(THIS-OBJECT:Gr, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* VisaExcelTab */
      /*  */
      THIS-OBJECT:VisaExcelTab:Controls:Add(THIS-OBJECT:VisaPanelEmpty).
      THIS-OBJECT:VisaExcelTab:Controls:Add(THIS-OBJECT:VisaPanelWaiting).
      THIS-OBJECT:VisaExcelTab:Controls:Add(THIS-OBJECT:VisaExcel).
      THIS-OBJECT:VisaExcelTab:Location = NEW System.Drawing.Point(1, 23).
      THIS-OBJECT:VisaExcelTab:Name = "VisaExcelTab".
      THIS-OBJECT:VisaExcelTab:Size = NEW System.Drawing.Size(1141, 551).
      /*  */
      /* VisaPanelEmpty */
      /*  */
      THIS-OBJECT:VisaPanelEmpty:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:VisaPanelEmpty:Location = NEW System.Drawing.Point(2, 160).
      THIS-OBJECT:VisaPanelEmpty:Name = "VisaPanelEmpty".
      THIS-OBJECT:VisaPanelEmpty:Size = NEW System.Drawing.Size(279, 221).
      THIS-OBJECT:VisaPanelEmpty:TabIndex = 2.
      /*  */
      /* VisaPanelWaiting */
      /*  */
      THIS-OBJECT:VisaPanelWaiting:BackColor = System.Drawing.SystemColors:Info.
      THIS-OBJECT:VisaPanelWaiting:Controls:Add(THIS-OBJECT:label1).
      THIS-OBJECT:VisaPanelWaiting:Location = NEW System.Drawing.Point(3, 3).
      THIS-OBJECT:VisaPanelWaiting:Name = "VisaPanelWaiting".
      THIS-OBJECT:VisaPanelWaiting:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:VisaPanelWaiting:TabIndex = 1.
      /*  */
      /* label1 */
      /*  */
      THIS-OBJECT:label1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:label1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 8.25, System.Drawing.FontStyle:Italic, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:label1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:label1:Name = "label1".
      THIS-OBJECT:label1:Size = NEW System.Drawing.Size(252, 150).
      THIS-OBJECT:label1:TabIndex = 0.
      THIS-OBJECT:label1:Text = "Genererar kalkyl, var god vänta..".
      THIS-OBJECT:label1:TextAlign = System.Drawing.ContentAlignment:MiddleCenter.
      THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
      /*  */
      /* VisaExcel */
      /*  */
      THIS-OBJECT:VisaExcel:Location = NEW System.Drawing.Point(324, 51).
      THIS-OBJECT:VisaExcel:Name = "VisaExcel".
      THIS-OBJECT:VisaExcel:Size = NEW System.Drawing.Size(522, 330).
      THIS-OBJECT:VisaExcel:TabIndex = 0.
      /*  */
      /* ultraTabPageControl16 */
      /*  */
      THIS-OBJECT:ultraTabPageControl16:Controls:Add(THIS-OBJECT:VisaIEBrowser).
      THIS-OBJECT:ultraTabPageControl16:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl16:Name = "ultraTabPageControl16".
      THIS-OBJECT:ultraTabPageControl16:Size = NEW System.Drawing.Size(1141, 551).
      /*  */
      /* VisaIEBrowser */
      /*  */
      THIS-OBJECT:VisaIEBrowser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaIEBrowser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaIEBrowser:MinimumSize = NEW System.Drawing.Size(20, 20).
      THIS-OBJECT:VisaIEBrowser:Name = "VisaIEBrowser".
      THIS-OBJECT:VisaIEBrowser:Size = NEW System.Drawing.Size(1141, 551).
      THIS-OBJECT:VisaIEBrowser:TabIndex = 0.
      /*  */
      /* ultraTabPageControl17 */
      /*  */
      THIS-OBJECT:ultraTabPageControl17:Controls:Add(THIS-OBJECT:VisaPDFBrowser).
      THIS-OBJECT:ultraTabPageControl17:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabPageControl17:Name = "ultraTabPageControl17".
      THIS-OBJECT:ultraTabPageControl17:Size = NEW System.Drawing.Size(1141, 551).
      /*  */
      /* VisaPDFBrowser */
      /*  */
      THIS-OBJECT:VisaPDFBrowser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:VisaPDFBrowser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:VisaPDFBrowser:MinimumSize = NEW System.Drawing.Size(20, 20).
      THIS-OBJECT:VisaPDFBrowser:Name = "VisaPDFBrowser".
      THIS-OBJECT:VisaPDFBrowser:Size = NEW System.Drawing.Size(1141, 551).
      THIS-OBJECT:VisaPDFBrowser:TabIndex = 0.
      /*  */
      /* TabKalkylHuvud */
      /*  */
      THIS-OBJECT:TabKalkylHuvud:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:TabKalkylHuvud:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKalkylHuvud:Name = "TabKalkylHuvud".
      THIS-OBJECT:TabKalkylHuvud:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer1:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      /*  */
      /* splitContainer1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudKalknr).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudUtfardare).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudAnmarkning).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:Labelanm).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudAnsvarig).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudKatalog).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:Labelomr).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudBenamning).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudOmrade).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:Labelutf).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:HuvudTyp).
      THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:Labelkatalog).
      /*  */
      /* splitContainer1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:GridPersonal).
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(1145, 577).
      THIS-OBJECT:splitContainer1:SplitterDistance = 383.
      THIS-OBJECT:splitContainer1:TabIndex = 43.
      /*  */
      /* HuvudKalknr */
      /*  */
      THIS-OBJECT:HuvudKalknr:Location = NEW System.Drawing.Point(13, 6).
      THIS-OBJECT:HuvudKalknr:Name = "HuvudKalknr".
      THIS-OBJECT:HuvudKalknr:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudKalknr:TabIndex = 39.
      /*  */
      /* HuvudUtfardare */
      /*  */
      THIS-OBJECT:HuvudUtfardare:CheckedListSettings:CheckStateMember = "".
      appearance16:BackColor = System.Drawing.SystemColors:Window.
      appearance16:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Appearance = appearance16.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance17:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance17:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance17:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance17:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:Appearance = appearance17.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BandLabelAppearance = appearance18.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance20:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance20:BackColor2 = System.Drawing.SystemColors:Control.
      appearance20:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance20:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:GroupByBox:PromptAppearance = appearance20.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:MaxRowScrollRegions = 1.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      appearance21:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveCellAppearance = appearance21.
      appearance22:BackColor = System.Drawing.SystemColors:Highlight.
      appearance22:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:ActiveRowAppearance = appearance22.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CardAreaAppearance = appearance23.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      appearance24:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellAppearance = appearance24.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:CellPadding = 0.
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance25:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:GroupByRowAppearance = appearance25.
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderAppearance = appearance26.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowAppearance = appearance27.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:Override:TemplateAddRowAppearance = appearance28.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudUtfardare:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudUtfardare:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudUtfardare:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudUtfardare:Location = NEW System.Drawing.Point(97, 171).
      THIS-OBJECT:HuvudUtfardare:Name = "HuvudUtfardare".
      THIS-OBJECT:HuvudUtfardare:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudUtfardare:TabIndex = 41.
      /*  */
      /* HuvudAnmarkning */
      /*  */
      THIS-OBJECT:HuvudAnmarkning:Location = NEW System.Drawing.Point(13, 284).
      THIS-OBJECT:HuvudAnmarkning:Multiline = TRUE.
      THIS-OBJECT:HuvudAnmarkning:Name = "HuvudAnmarkning".
      THIS-OBJECT:HuvudAnmarkning:Size = NEW System.Drawing.Size(361, 108).
      THIS-OBJECT:HuvudAnmarkning:TabIndex = 32.
      THIS-OBJECT:HuvudAnmarkning:Text = " ".
      /*  */
      /* Labelanm */
      /*  */
      THIS-OBJECT:Labelanm:Location = NEW System.Drawing.Point(13, 255).
      THIS-OBJECT:Labelanm:Name = "Labelanm".
      THIS-OBJECT:Labelanm:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelanm:TabIndex = 33.
      THIS-OBJECT:Labelanm:Text = "Anmärkning".
      /*  */
      /* HuvudAnsvarig */
      /*  */
      THIS-OBJECT:HuvudAnsvarig:Location = NEW System.Drawing.Point(13, 210).
      THIS-OBJECT:HuvudAnsvarig:Name = "HuvudAnsvarig".
      THIS-OBJECT:HuvudAnsvarig:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudAnsvarig:TabIndex = 40.
      /*  */
      /* HuvudKatalog */
      /*  */
      THIS-OBJECT:HuvudKatalog:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudKatalog:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:HuvudKatalog:Location = NEW System.Drawing.Point(97, 104).
      THIS-OBJECT:HuvudKatalog:Name = "HuvudKatalog".
      THIS-OBJECT:HuvudKatalog:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:HuvudKatalog:TabIndex = 42.
      /*  */
      /* Labelomr */
      /*  */
      appearance29:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelomr:Appearance = appearance29.
      THIS-OBJECT:Labelomr:Location = NEW System.Drawing.Point(13, 40).
      THIS-OBJECT:Labelomr:Name = "Labelomr".
      THIS-OBJECT:Labelomr:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelomr:TabIndex = 38.
      /*  */
      /* HuvudBenamning */
      /*  */
      THIS-OBJECT:HuvudBenamning:Location = NEW System.Drawing.Point(13, 137).
      THIS-OBJECT:HuvudBenamning:Name = "HuvudBenamning".
      THIS-OBJECT:HuvudBenamning:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudBenamning:TabIndex = 35.
      THIS-OBJECT:HuvudBenamning:Tag = "".
      /*  */
      /* HuvudOmrade */
      /*  */
      THIS-OBJECT:HuvudOmrade:CheckedListSettings:CheckStateMember = "".
      appearance30:BackColor = System.Drawing.SystemColors:Window.
      appearance30:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Appearance = appearance30.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance31:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance31:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance31:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance31:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:Appearance = appearance31.
      appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BandLabelAppearance = appearance32.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance33:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance33:BackColor2 = System.Drawing.SystemColors:Control.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:GroupByBox:PromptAppearance = appearance33.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:MaxRowScrollRegions = 1.
      appearance34:BackColor = System.Drawing.SystemColors:Window.
      appearance34:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveCellAppearance = appearance34.
      appearance35:BackColor = System.Drawing.SystemColors:Highlight.
      appearance35:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:ActiveRowAppearance = appearance35.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance36:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CardAreaAppearance = appearance36.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      appearance37:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellAppearance = appearance37.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:CellPadding = 0.
      appearance38:BackColor = System.Drawing.SystemColors:Control.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:GroupByRowAppearance = appearance38.
      appearance39:TextHAlignAsString = "Left".
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderAppearance = appearance39.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowAppearance = appearance51.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:Override:TemplateAddRowAppearance = appearance52.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:HuvudOmrade:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:HuvudOmrade:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:HuvudOmrade:DropDownStyle = Infragistics.Win.UltraWinGrid.UltraComboStyle:DropDownList.
      THIS-OBJECT:HuvudOmrade:Location = NEW System.Drawing.Point(97, 40).
      THIS-OBJECT:HuvudOmrade:Name = "HuvudOmrade".
      THIS-OBJECT:HuvudOmrade:Size = NEW System.Drawing.Size(277, 22).
      THIS-OBJECT:HuvudOmrade:TabIndex = 37.
      /*  */
      /* Labelutf */
      /*  */
      THIS-OBJECT:Labelutf:ImageTransparentColor = System.Drawing.Color:Red.
      THIS-OBJECT:Labelutf:Location = NEW System.Drawing.Point(13, 171).
      THIS-OBJECT:Labelutf:Name = "Labelutf".
      THIS-OBJECT:Labelutf:Size = NEW System.Drawing.Size(81, 23).
      THIS-OBJECT:Labelutf:TabIndex = 30.
      THIS-OBJECT:Labelutf:Text = "Utfärdare".
      /*  */
      /* HuvudTyp */
      /*  */
      THIS-OBJECT:HuvudTyp:Location = NEW System.Drawing.Point(13, 70).
      THIS-OBJECT:HuvudTyp:Name = "HuvudTyp".
      THIS-OBJECT:HuvudTyp:Size = NEW System.Drawing.Size(361, 28).
      THIS-OBJECT:HuvudTyp:TabIndex = 36.
      THIS-OBJECT:HuvudTyp:GuruValueChanged:Subscribe(THIS-OBJECT:HuvudTyp_GuruValueChanged).
      /*  */
      /* Labelkatalog */
      /*  */
      appearance65:TextHAlignAsString = "Left".
      THIS-OBJECT:Labelkatalog:Appearance = appearance65.
      THIS-OBJECT:Labelkatalog:Location = NEW System.Drawing.Point(13, 108).
      THIS-OBJECT:Labelkatalog:Name = "Labelkatalog".
      THIS-OBJECT:Labelkatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:Labelkatalog:TabIndex = 31.
      THIS-OBJECT:Labelkatalog:Text = "Katalog".
      /*  */
      /* GridPersonal */
      /*  */
      appearance53:BackColor = System.Drawing.SystemColors:Window.
      appearance53:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPersonal:DisplayLayout:Appearance = appearance53.
      THIS-OBJECT:GridPersonal:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPersonal:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance54:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance54:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance54:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:Appearance = appearance54.
      appearance55:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BandLabelAppearance = appearance55.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance56:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance56:BackColor2 = System.Drawing.SystemColors:Control.
      appearance56:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPersonal:DisplayLayout:GroupByBox:PromptAppearance = appearance56.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      appearance57:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveCellAppearance = appearance57.
      appearance58:BackColor = System.Drawing.SystemColors:Highlight.
      appearance58:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:ActiveRowAppearance = appearance58.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance59:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CardAreaAppearance = appearance59.
      appearance60:BorderColor = System.Drawing.Color:Silver.
      appearance60:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellAppearance = appearance60.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:CellPadding = 0.
      appearance61:BackColor = System.Drawing.SystemColors:Control.
      appearance61:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance61:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance61:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance61:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:GroupByRowAppearance = appearance61.
      appearance62:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderAppearance = appearance62.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance63:BackColor = System.Drawing.SystemColors:Window.
      appearance63:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowAppearance = appearance63.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance64:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPersonal:DisplayLayout:Override:TemplateAddRowAppearance = appearance64.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPersonal:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPersonal:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPersonal:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPersonal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridPersonal:GuruContext = ?.
      THIS-OBJECT:GridPersonal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPersonal:Name = "GridPersonal".
      THIS-OBJECT:GridPersonal:Size = NEW System.Drawing.Size(758, 577).
      THIS-OBJECT:GridPersonal:TabIndex = 34.
      THIS-OBJECT:GridPersonal:Text = "grid1".
      /*  */
      /* TabKalkyl */
      /*  */
      THIS-OBJECT:TabKalkyl:Controls:Add(THIS-OBJECT:splitContainer6).
      THIS-OBJECT:TabKalkyl:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabKalkyl:Name = "TabKalkyl".
      THIS-OBJECT:TabKalkyl:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* splitContainer6 */
      /*  */
      THIS-OBJECT:splitContainer6:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainer6:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer6:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer6:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer6:Name = "splitContainer6".
      /*  */
      /* splitContainer6.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer6:Panel1:Controls:Add(THIS-OBJECT:splitContainer7).
      /*  */
      /* splitContainer6.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer6:Panel2:Controls:Add(THIS-OBJECT:splitContainer11).
      THIS-OBJECT:splitContainer6:Size = NEW System.Drawing.Size(1145, 577).
      THIS-OBJECT:splitContainer6:SplitterDistance = 281.
      THIS-OBJECT:splitContainer6:TabIndex = 11.
      /*  */
      /* splitContainer7 */
      /*  */
      THIS-OBJECT:splitContainer7:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer7:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer7:Name = "splitContainer7".
      THIS-OBJECT:splitContainer7:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainer7.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer7:Panel1:Controls:Add(THIS-OBJECT:GridArbetsKoder).
      /*  */
      /* splitContainer7.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer7:Panel2:Controls:Add(THIS-OBJECT:splitContainer8).
      THIS-OBJECT:splitContainer7:Size = NEW System.Drawing.Size(279, 575).
      THIS-OBJECT:splitContainer7:SplitterDistance = 264.
      THIS-OBJECT:splitContainer7:TabIndex = 0.
      /*  */
      /* GridArbetsKoder */
      /*  */
      appearance330:BackColor = System.Drawing.SystemColors:Window.
      appearance330:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Appearance = appearance330.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance331:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance331:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance331:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance331:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:Appearance = appearance331.
      appearance332:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance332.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance333:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance333:BackColor2 = System.Drawing.SystemColors:Control.
      appearance333:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance333:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance333.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance334:BackColor = System.Drawing.SystemColors:Window.
      appearance334:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveCellAppearance = appearance334.
      appearance335:BackColor = System.Drawing.SystemColors:Highlight.
      appearance335:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:ActiveRowAppearance = appearance335.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance336:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CardAreaAppearance = appearance336.
      appearance337:BorderColor = System.Drawing.Color:Silver.
      appearance337:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellAppearance = appearance337.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:CellPadding = 0.
      appearance338:BackColor = System.Drawing.SystemColors:Control.
      appearance338:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance338:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance338:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance338:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:GroupByRowAppearance = appearance338.
      appearance339:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderAppearance = appearance339.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance340:BackColor = System.Drawing.SystemColors:Window.
      appearance340:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowAppearance = appearance340.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance341:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbetsKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance341.
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
      THIS-OBJECT:GridArbetsKoder:Size = NEW System.Drawing.Size(279, 264).
      THIS-OBJECT:GridArbetsKoder:TabIndex = 4.
      THIS-OBJECT:GridArbetsKoder:Text = "grid1".
      /*  */
      /* splitContainer8 */
      /*  */
      THIS-OBJECT:splitContainer8:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer8:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainer8:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer8:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer8:Name = "splitContainer8".
      /*  */
      /* splitContainer8.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer8:Panel1:Controls:Add(THIS-OBJECT:GridLopKoder).
      /*  */
      /* splitContainer8.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer8:Panel2:Controls:Add(THIS-OBJECT:KalkylBtnOverAll).
      THIS-OBJECT:splitContainer8:Panel2:Controls:Add(THIS-OBJECT:KalkylBtnBackAll).
      THIS-OBJECT:splitContainer8:Panel2:Controls:Add(THIS-OBJECT:KalkylBtnOverMark).
      THIS-OBJECT:splitContainer8:Panel2:Controls:Add(THIS-OBJECT:KalkylBtnBackMark).
      THIS-OBJECT:splitContainer8:Size = NEW System.Drawing.Size(279, 307).
      THIS-OBJECT:splitContainer8:SplitterDistance = 244.
      THIS-OBJECT:splitContainer8:TabIndex = 0.
      /*  */
      /* GridLopKoder */
      /*  */
      THIS-OBJECT:GridLopKoder:AllowDrop = TRUE.
      appearance318:BackColor = System.Drawing.SystemColors:Window.
      appearance318:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Appearance = appearance318.
      THIS-OBJECT:GridLopKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridLopKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance319:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance319:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance319:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance319:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:Appearance = appearance319.
      appearance320:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance320.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance321:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance321:BackColor2 = System.Drawing.SystemColors:Control.
      appearance321:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance321:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance321.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance322:BackColor = System.Drawing.SystemColors:Window.
      appearance322:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveCellAppearance = appearance322.
      appearance323:BackColor = System.Drawing.SystemColors:Highlight.
      appearance323:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:ActiveRowAppearance = appearance323.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance324:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CardAreaAppearance = appearance324.
      appearance325:BorderColor = System.Drawing.Color:Silver.
      appearance325:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellAppearance = appearance325.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:CellPadding = 0.
      appearance326:BackColor = System.Drawing.SystemColors:Control.
      appearance326:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance326:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance326:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance326:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:GroupByRowAppearance = appearance326.
      appearance327:TextHAlignAsString = "Left".
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderAppearance = appearance327.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance328:BackColor = System.Drawing.SystemColors:Window.
      appearance328:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowAppearance = appearance328.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance329:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance329.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridLopKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridLopKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridLopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridLopKoder:GuruContext = ?.
      THIS-OBJECT:GridLopKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridLopKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridLopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridLopKoder:Name = "GridLopKoder".
      THIS-OBJECT:GridLopKoder:Size = NEW System.Drawing.Size(244, 307).
      THIS-OBJECT:GridLopKoder:TabIndex = 5.
      THIS-OBJECT:GridLopKoder:Text = "grid2".
      THIS-OBJECT:GridLopKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridLopKoder_GuruAfterDropped).
      /*  */
      /* KalkylBtnOverAll */
      /*  */
      THIS-OBJECT:KalkylBtnOverAll:Location = NEW System.Drawing.Point(3, 3).
      THIS-OBJECT:KalkylBtnOverAll:Name = "KalkylBtnOverAll".
      THIS-OBJECT:KalkylBtnOverAll:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylBtnOverAll:TabIndex = 8.
      THIS-OBJECT:KalkylBtnOverAll:Text = "Alla väljs".
      THIS-OBJECT:KalkylBtnOverAll:Click:Subscribe(THIS-OBJECT:buttonOverAlla_Click).
      /*  */
      /* KalkylBtnBackAll */
      /*  */
      THIS-OBJECT:KalkylBtnBackAll:Location = NEW System.Drawing.Point(2, 96).
      THIS-OBJECT:KalkylBtnBackAll:Name = "KalkylBtnBackAll".
      THIS-OBJECT:KalkylBtnBackAll:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylBtnBackAll:TabIndex = 10.
      THIS-OBJECT:KalkylBtnBackAll:Text = "Alla bort".
      THIS-OBJECT:KalkylBtnBackAll:Click:Subscribe(THIS-OBJECT:buttonBackAlla_Click).
      /*  */
      /* KalkylBtnOverMark */
      /*  */
      THIS-OBJECT:KalkylBtnOverMark:Location = NEW System.Drawing.Point(2, 34).
      THIS-OBJECT:KalkylBtnOverMark:Name = "KalkylBtnOverMark".
      THIS-OBJECT:KalkylBtnOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylBtnOverMark:TabIndex = 7.
      THIS-OBJECT:KalkylBtnOverMark:Text = "Markerade väljs".
      THIS-OBJECT:KalkylBtnOverMark:Click:Subscribe(THIS-OBJECT:buttonOvermark_Click).
      /*  */
      /* KalkylBtnBackMark */
      /*  */
      THIS-OBJECT:KalkylBtnBackMark:Location = NEW System.Drawing.Point(2, 65).
      THIS-OBJECT:KalkylBtnBackMark:Name = "KalkylBtnBackMark".
      THIS-OBJECT:KalkylBtnBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:KalkylBtnBackMark:TabIndex = 9.
      THIS-OBJECT:KalkylBtnBackMark:Text = "Markerade bort".
      THIS-OBJECT:KalkylBtnBackMark:Click:Subscribe(THIS-OBJECT:buttonBackMark_Click).
      /*  */
      /* splitContainer11 */
      /*  */
      THIS-OBJECT:splitContainer11:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer11:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainer11:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer11:Name = "splitContainer11".
      /*  */
      /* splitContainer11.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer11:Panel1:Controls:Add(THIS-OBJECT:GridKalkylKoder).
      /*  */
      /* splitContainer11.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer11:Panel2:Controls:Add(THIS-OBJECT:GridFriKalk).
      THIS-OBJECT:splitContainer11:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainer11:Size = NEW System.Drawing.Size(858, 575).
      THIS-OBJECT:splitContainer11:SplitterDistance = 597.
      THIS-OBJECT:splitContainer11:TabIndex = 4.
      /*  */
      /* GridKalkylKoder */
      /*  */
      THIS-OBJECT:GridKalkylKoder:AllowDrop = TRUE.
      appearance342:BackColor = System.Drawing.SystemColors:Window.
      appearance342:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Appearance = appearance342.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance343:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance343:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance343:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance343:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:Appearance = appearance343.
      appearance344:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance344.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance345:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance345:BackColor2 = System.Drawing.SystemColors:Control.
      appearance345:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance345:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance345.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance346:BackColor = System.Drawing.SystemColors:Window.
      appearance346:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveCellAppearance = appearance346.
      appearance347:BackColor = System.Drawing.SystemColors:Highlight.
      appearance347:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:ActiveRowAppearance = appearance347.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance348:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CardAreaAppearance = appearance348.
      appearance349:BorderColor = System.Drawing.Color:Silver.
      appearance349:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellAppearance = appearance349.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:CellPadding = 0.
      appearance350:BackColor = System.Drawing.SystemColors:Control.
      appearance350:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance350:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance350:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance350:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:GroupByRowAppearance = appearance350.
      appearance351:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderAppearance = appearance351.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance352:BackColor = System.Drawing.SystemColors:Window.
      appearance352:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowAppearance = appearance352.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance353:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance353.
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
      THIS-OBJECT:GridKalkylKoder:Size = NEW System.Drawing.Size(858, 575).
      THIS-OBJECT:GridKalkylKoder:TabIndex = 3.
      THIS-OBJECT:GridKalkylKoder:Text = "grid3".
      THIS-OBJECT:GridKalkylKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterCellUpdate).
      THIS-OBJECT:GridKalkylKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridKalkylKoder_GuruAfterDropped).
      THIS-OBJECT:GridKalkylKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridKalkylKoder_AfterSelectChange).
      /*  */
      /* GridFriKalk */
      /*  */
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      appearance8:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Appearance = appearance8.
      THIS-OBJECT:GridFriKalk:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFriKalk:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance5:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:Appearance = appearance5.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance7:BackColor2 = System.Drawing.SystemColors:Control.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance11:BackColor = System.Drawing.SystemColors:Highlight.
      appearance11:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:ActiveRowAppearance = appearance11.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CardAreaAppearance = appearance10.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:CellPadding = 0.
      appearance13:BackColor = System.Drawing.SystemColors:Control.
      appearance13:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance13:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance13:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance13:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:GroupByRowAppearance = appearance13.
      appearance15:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderAppearance = appearance15.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowAppearance = appearance14.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFriKalk:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
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
      THIS-OBJECT:GridFriKalk:Size = NEW System.Drawing.Size(96, 100).
      THIS-OBJECT:GridFriKalk:TabIndex = 0.
      THIS-OBJECT:GridFriKalk:Text = "grid1".
      THIS-OBJECT:GridFriKalk:AfterCellUpdate:Subscribe(THIS-OBJECT:GridFriKalk_AfterCellUpdate).
      /*  */
      /* TabEgna */
      /*  */
      THIS-OBJECT:TabEgna:Controls:Add(THIS-OBJECT:splitContainer2).
      THIS-OBJECT:TabEgna:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabEgna:Name = "TabEgna".
      THIS-OBJECT:TabEgna:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* splitContainer2 */
      /*  */
      THIS-OBJECT:splitContainer2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer2:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainer2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer2:Name = "splitContainer2".
      THIS-OBJECT:splitContainer2:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainer2.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer2:Panel1:Controls:Add(THIS-OBJECT:splitContainer5).
      /*  */
      /* splitContainer2.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer2:Panel2:Controls:Add(THIS-OBJECT:splitContainer3).
      THIS-OBJECT:splitContainer2:Size = NEW System.Drawing.Size(1145, 577).
      THIS-OBJECT:splitContainer2:SplitterDistance = 345.
      THIS-OBJECT:splitContainer2:TabIndex = 18.
      /*  */
      /* splitContainer5 */
      /*  */
      THIS-OBJECT:splitContainer5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer5:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer5:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer5:Name = "splitContainer5".
      THIS-OBJECT:splitContainer5:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainer5.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer5:Panel1:Controls:Add(THIS-OBJECT:egnakoderstrip).
      /*  */
      /* splitContainer5.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer5:Panel2:Controls:Add(THIS-OBJECT:GridEgnaKoder).
      THIS-OBJECT:splitContainer5:Size = NEW System.Drawing.Size(1145, 345).
      THIS-OBJECT:splitContainer5:SplitterDistance = 25.
      THIS-OBJECT:splitContainer5:SplitterWidth = 1.
      THIS-OBJECT:splitContainer5:TabIndex = 14.
      /*  */
      /* egnakoderstrip */
      /*  */
      THIS-OBJECT:egnakoderstrip:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:EgenSkapa.
      arrayvar0[2] = THIS-OBJECT:EgenTaBort.
      arrayvar0[3] = THIS-OBJECT:toolStripButton1.
      THIS-OBJECT:egnakoderstrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:egnakoderstrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:egnakoderstrip:Name = "egnakoderstrip".
      THIS-OBJECT:egnakoderstrip:Size = NEW System.Drawing.Size(1145, 25).
      THIS-OBJECT:egnakoderstrip:TabIndex = 13.
      THIS-OBJECT:egnakoderstrip:Text = "toolStrip1".
      /*  */
      /* EgenSkapa */
      /*  */
      THIS-OBJECT:EgenSkapa:Image = CAST(resources:GetObject("EgenSkapa.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenSkapa:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenSkapa:Name = "EgenSkapa".
      THIS-OBJECT:EgenSkapa:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:EgenSkapa:Text = "Lägg till egen".
      THIS-OBJECT:EgenSkapa:Click:Subscribe(THIS-OBJECT:SkapaNyEgen_Click).
      /*  */
      /* EgenTaBort */
      /*  */
      THIS-OBJECT:EgenTaBort:Image = CAST(resources:GetObject("EgenTaBort.Image"), System.Drawing.Image).
      THIS-OBJECT:EgenTaBort:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:EgenTaBort:Name = "EgenTaBort".
      THIS-OBJECT:EgenTaBort:Size = NEW System.Drawing.Size(124, 22).
      THIS-OBJECT:EgenTaBort:Text = "Ta bort markerade".
      THIS-OBJECT:EgenTaBort:Click:Subscribe(THIS-OBJECT:TaBortEgen_Click).
      /*  */
      /* toolStripButton1 */
      /*  */
      THIS-OBJECT:toolStripButton1:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
      THIS-OBJECT:toolStripButton1:Image = CAST(resources:GetObject("toolStripButton1.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButton1:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButton1:Name = "toolStripButton1".
      THIS-OBJECT:toolStripButton1:Size = NEW System.Drawing.Size(23, 22).
      THIS-OBJECT:toolStripButton1:Text = "toolStripButton1".
      THIS-OBJECT:toolStripButton1:Click:Subscribe(THIS-OBJECT:toolStripButton1_Click).
      /*  */
      /* GridEgnaKoder */
      /*  */
      appearance354:BackColor = System.Drawing.SystemColors:Window.
      appearance354:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Appearance = appearance354.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance355:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance355:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance355:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance355:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:Appearance = appearance355.
      appearance356:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance356.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance357:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance357:BackColor2 = System.Drawing.SystemColors:Control.
      appearance357:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance357:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance357.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance358:BackColor = System.Drawing.SystemColors:Window.
      appearance358:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveCellAppearance = appearance358.
      appearance359:BackColor = System.Drawing.SystemColors:Highlight.
      appearance359:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:ActiveRowAppearance = appearance359.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance360:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CardAreaAppearance = appearance360.
      appearance361:BorderColor = System.Drawing.Color:Silver.
      appearance361:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellAppearance = appearance361.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:CellPadding = 0.
      appearance362:BackColor = System.Drawing.SystemColors:Control.
      appearance362:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance362:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance362:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance362:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:GroupByRowAppearance = appearance362.
      appearance363:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderAppearance = appearance363.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance364:BackColor = System.Drawing.SystemColors:Window.
      appearance364:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowAppearance = appearance364.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance365:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance365.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgnaKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgnaKoder:GuruContext = ?.
      THIS-OBJECT:GridEgnaKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgnaKoder:Name = "GridEgnaKoder".
      THIS-OBJECT:GridEgnaKoder:Size = NEW System.Drawing.Size(1145, 319).
      THIS-OBJECT:GridEgnaKoder:TabIndex = 12.
      THIS-OBJECT:GridEgnaKoder:Text = "grid1".
      THIS-OBJECT:GridEgnaKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellUpdate).
      THIS-OBJECT:GridEgnaKoder:AfterCellActivate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterCellActivate).
      THIS-OBJECT:GridEgnaKoder:AfterRowActivate:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterRowActivate).
      THIS-OBJECT:GridEgnaKoder:AfterSelectChange:Subscribe(THIS-OBJECT:GridEgnaKoder_AfterSelectChange).
      /*  */
      /* splitContainer3 */
      /*  */
      THIS-OBJECT:splitContainer3:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainer3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer3:Name = "splitContainer3".
      /*  */
      /* splitContainer3.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:GridUnderKoder).
      /*  */
      /* splitContainer3.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel2:Controls:Add(THIS-OBJECT:splitContainer4).
      THIS-OBJECT:splitContainer3:Size = NEW System.Drawing.Size(1145, 228).
      THIS-OBJECT:splitContainer3:SplitterDistance = 381.
      THIS-OBJECT:splitContainer3:TabIndex = 0.
      /*  */
      /* GridUnderKoder */
      /*  */
      THIS-OBJECT:GridUnderKoder:AllowDrop = TRUE.
      appearance378:BackColor = System.Drawing.SystemColors:Window.
      appearance378:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Appearance = appearance378.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance379:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance379:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance379:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance379:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance379.
      appearance380:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance380.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance381:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance381:BackColor2 = System.Drawing.SystemColors:Control.
      appearance381:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance381:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance381.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance382:BackColor = System.Drawing.SystemColors:Window.
      appearance382:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance382.
      appearance383:BackColor = System.Drawing.SystemColors:Highlight.
      appearance383:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance383.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance384:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance384.
      appearance385:BorderColor = System.Drawing.Color:Silver.
      appearance385:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellAppearance = appearance385.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance386:BackColor = System.Drawing.SystemColors:Control.
      appearance386:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance386:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance386:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance386:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance386.
      appearance387:TextHAlignAsString = "Left".
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance387.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance388:BackColor = System.Drawing.SystemColors:Window.
      appearance388:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowAppearance = appearance388.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance389:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance389.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridUnderKoder:Name = "GridUnderKoder".
      THIS-OBJECT:GridUnderKoder:Size = NEW System.Drawing.Size(379, 226).
      THIS-OBJECT:GridUnderKoder:TabIndex = 10.
      THIS-OBJECT:GridUnderKoder:Text = "grid1".
      THIS-OBJECT:GridUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridUnderKoder_GuruAfterDropped).
      /*  */
      /* splitContainer4 */
      /*  */
      THIS-OBJECT:splitContainer4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer4:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainer4:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer4:Name = "splitContainer4".
      /*  */
      /* splitContainer4.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer4:Panel1:Controls:Add(THIS-OBJECT:EgnaBtnOverAll).
      THIS-OBJECT:splitContainer4:Panel1:Controls:Add(THIS-OBJECT:EgnaBtnBackAll).
      THIS-OBJECT:splitContainer4:Panel1:Controls:Add(THIS-OBJECT:EgnaBtnOverMark).
      THIS-OBJECT:splitContainer4:Panel1:Controls:Add(THIS-OBJECT:EgnaBtnBackMark).
      /*  */
      /* splitContainer4.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer4:Panel2:Controls:Add(THIS-OBJECT:GridValdaUnderKoder).
      THIS-OBJECT:splitContainer4:Size = NEW System.Drawing.Size(758, 226).
      THIS-OBJECT:splitContainer4:SplitterDistance = 30.
      THIS-OBJECT:splitContainer4:TabIndex = 0.
      /*  */
      /* EgnaBtnOverAll */
      /*  */
      THIS-OBJECT:EgnaBtnOverAll:Location = NEW System.Drawing.Point(4, 3).
      THIS-OBJECT:EgnaBtnOverAll:Name = "EgnaBtnOverAll".
      THIS-OBJECT:EgnaBtnOverAll:Size = NEW System.Drawing.Size(24, 25).
      THIS-OBJECT:EgnaBtnOverAll:TabIndex = 14.
      THIS-OBJECT:EgnaBtnOverAll:Text = "Alla väljs".
      THIS-OBJECT:EgnaBtnOverAll:Click:Subscribe(THIS-OBJECT:buttonOverAllaPriser_Click).
      /*  */
      /* EgnaBtnBackAll */
      /*  */
      THIS-OBJECT:EgnaBtnBackAll:Location = NEW System.Drawing.Point(4, 94).
      THIS-OBJECT:EgnaBtnBackAll:Name = "EgnaBtnBackAll".
      THIS-OBJECT:EgnaBtnBackAll:Size = NEW System.Drawing.Size(23, 24).
      THIS-OBJECT:EgnaBtnBackAll:TabIndex = 16.
      THIS-OBJECT:EgnaBtnBackAll:Text = "Alla bort".
      THIS-OBJECT:EgnaBtnBackAll:Click:Subscribe(THIS-OBJECT:buttonBackAllaPriser_Click).
      /*  */
      /* EgnaBtnOverMark */
      /*  */
      THIS-OBJECT:EgnaBtnOverMark:Location = NEW System.Drawing.Point(4, 34).
      THIS-OBJECT:EgnaBtnOverMark:Name = "EgnaBtnOverMark".
      THIS-OBJECT:EgnaBtnOverMark:Size = NEW System.Drawing.Size(23, 25).
      THIS-OBJECT:EgnaBtnOverMark:TabIndex = 13.
      THIS-OBJECT:EgnaBtnOverMark:Text = "Markerade väljs".
      THIS-OBJECT:EgnaBtnOverMark:Click:Subscribe(THIS-OBJECT:buttonOverMarkPriser_Click).
      /*  */
      /* EgnaBtnBackMark */
      /*  */
      THIS-OBJECT:EgnaBtnBackMark:Location = NEW System.Drawing.Point(4, 65).
      THIS-OBJECT:EgnaBtnBackMark:Name = "EgnaBtnBackMark".
      THIS-OBJECT:EgnaBtnBackMark:Size = NEW System.Drawing.Size(23, 23).
      THIS-OBJECT:EgnaBtnBackMark:TabIndex = 15.
      THIS-OBJECT:EgnaBtnBackMark:Text = "Markerade bort".
      THIS-OBJECT:EgnaBtnBackMark:Click:Subscribe(THIS-OBJECT:buttonBackMarkPriser_Click).
      /*  */
      /* GridValdaUnderKoder */
      /*  */
      THIS-OBJECT:GridValdaUnderKoder:AllowDrop = TRUE.
      appearance366:BackColor = System.Drawing.SystemColors:Window.
      appearance366:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Appearance = appearance366.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance367:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance367:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance367:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance367:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:Appearance = appearance367.
      appearance368:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance368.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance369:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance369:BackColor2 = System.Drawing.SystemColors:Control.
      appearance369:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance369:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance369.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance370:BackColor = System.Drawing.SystemColors:Window.
      appearance370:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveCellAppearance = appearance370.
      appearance371:BackColor = System.Drawing.SystemColors:Highlight.
      appearance371:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:ActiveRowAppearance = appearance371.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance372:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CardAreaAppearance = appearance372.
      appearance373:BorderColor = System.Drawing.Color:Silver.
      appearance373:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellAppearance = appearance373.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:CellPadding = 0.
      appearance374:BackColor = System.Drawing.SystemColors:Control.
      appearance374:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance374:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance374:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance374:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:GroupByRowAppearance = appearance374.
      appearance375:TextHAlignAsString = "Left".
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderAppearance = appearance375.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance376:BackColor = System.Drawing.SystemColors:Window.
      appearance376:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowAppearance = appearance376.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance377:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance377.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridValdaUnderKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridValdaUnderKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridValdaUnderKoder:GuruContext = ?.
      THIS-OBJECT:GridValdaUnderKoder:GuruDraggable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = TRUE.
      THIS-OBJECT:GridValdaUnderKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridValdaUnderKoder:Name = "GridValdaUnderKoder".
      THIS-OBJECT:GridValdaUnderKoder:Size = NEW System.Drawing.Size(724, 226).
      THIS-OBJECT:GridValdaUnderKoder:TabIndex = 11.
      THIS-OBJECT:GridValdaUnderKoder:Text = "grid1".
      THIS-OBJECT:GridValdaUnderKoder:GuruAfterDropped:Subscribe(THIS-OBJECT:GridValdaUnderKoder_GuruAfterDropped).
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Controls:Add(THIS-OBJECT:ultraTabControl2).
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(1145, 577).
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
      THIS-OBJECT:ultraTabControl2:Size = NEW System.Drawing.Size(1145, 577).
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
      THIS-OBJECT:ultraTabSharedControlsPage3:Size = NEW System.Drawing.Size(1141, 551).
      /*  */
      /* TabFaktorer */
      /*  */
      THIS-OBJECT:TabFaktorer:Controls:Add(THIS-OBJECT:splitContainer12).
      THIS-OBJECT:TabFaktorer:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFaktorer:Name = "TabFaktorer".
      THIS-OBJECT:TabFaktorer:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* splitContainer12 */
      /*  */
      THIS-OBJECT:splitContainer12:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer12:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer12:Name = "splitContainer12".
      /*  */
      /* splitContainer12.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer12:Panel1:Controls:Add(THIS-OBJECT:GridFaktorer).
      /*  */
      /* splitContainer12.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer12:Panel2:Controls:Add(THIS-OBJECT:GridPriser).
      THIS-OBJECT:splitContainer12:Size = NEW System.Drawing.Size(1145, 577).
      THIS-OBJECT:splitContainer12:SplitterDistance = 597.
      THIS-OBJECT:splitContainer12:TabIndex = 2.
      /*  */
      /* GridFaktorer */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridFaktorer:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance4:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance4:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance4:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:Appearance = appearance4.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BandLabelAppearance = appearance40.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance41:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance41:BackColor2 = System.Drawing.SystemColors:Control.
      appearance41:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:GroupByBox:PromptAppearance = appearance41.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance42:BackColor = System.Drawing.SystemColors:Window.
      appearance42:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveCellAppearance = appearance42.
      appearance43:BackColor = System.Drawing.SystemColors:Highlight.
      appearance43:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:ActiveRowAppearance = appearance43.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance44:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CardAreaAppearance = appearance44.
      appearance45:BorderColor = System.Drawing.Color:Silver.
      appearance45:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellAppearance = appearance45.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:CellPadding = 0.
      appearance46:BackColor = System.Drawing.SystemColors:Control.
      appearance46:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance46:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance46:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance46:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:GroupByRowAppearance = appearance46.
      appearance47:TextHAlignAsString = "Left".
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderAppearance = appearance47.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance48:BackColor = System.Drawing.SystemColors:Window.
      appearance48:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowAppearance = appearance48.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance49:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:Override:TemplateAddRowAppearance = appearance49.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridFaktorer:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridFaktorer:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridFaktorer:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridFaktorer:GuruContext = ?.
      THIS-OBJECT:GridFaktorer:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridFaktorer:Name = "GridFaktorer".
      THIS-OBJECT:GridFaktorer:Size = NEW System.Drawing.Size(597, 577).
      THIS-OBJECT:GridFaktorer:TabIndex = 0.
      THIS-OBJECT:GridFaktorer:Text = "grid1".
      /*  */
      /* GridPriser */
      /*  */
      appearance390:BackColor = System.Drawing.SystemColors:Window.
      appearance390:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridPriser:DisplayLayout:Appearance = appearance390.
      THIS-OBJECT:GridPriser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridPriser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance391:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance391:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance391:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance391:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:Appearance = appearance391.
      appearance392:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance392.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance393:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance393:BackColor2 = System.Drawing.SystemColors:Control.
      appearance393:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance393:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridPriser:DisplayLayout:GroupByBox:PromptAppearance = appearance393.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance394:BackColor = System.Drawing.SystemColors:Window.
      appearance394:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveCellAppearance = appearance394.
      appearance395:BackColor = System.Drawing.SystemColors:Highlight.
      appearance395:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:ActiveRowAppearance = appearance395.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance396:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CardAreaAppearance = appearance396.
      appearance397:BorderColor = System.Drawing.Color:Silver.
      appearance397:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellAppearance = appearance397.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:CellPadding = 0.
      appearance398:BackColor = System.Drawing.SystemColors:Control.
      appearance398:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance398:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance398:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance398:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:GroupByRowAppearance = appearance398.
      appearance399:TextHAlignAsString = "Left".
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderAppearance = appearance399.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance400:BackColor = System.Drawing.SystemColors:Window.
      appearance400:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowAppearance = appearance400.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance401:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridPriser:DisplayLayout:Override:TemplateAddRowAppearance = appearance401.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridPriser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridPriser:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridPriser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridPriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridPriser:GuruContext = ?.
      THIS-OBJECT:GridPriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridPriser:Name = "GridPriser".
      THIS-OBJECT:GridPriser:Size = NEW System.Drawing.Size(544, 577).
      THIS-OBJECT:GridPriser:TabIndex = 1.
      THIS-OBJECT:GridPriser:Text = "grid1".
      /*  */
      /* TabMateriel */
      /*  */
      THIS-OBJECT:TabMateriel:Controls:Add(THIS-OBJECT:splitContainer9).
      THIS-OBJECT:TabMateriel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabMateriel:Name = "TabMateriel".
      THIS-OBJECT:TabMateriel:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* splitContainer9 */
      /*  */
      THIS-OBJECT:splitContainer9:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
      THIS-OBJECT:splitContainer9:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer9:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer9:Name = "splitContainer9".
      /*  */
      /* splitContainer9.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer9:Panel1:Controls:Add(THIS-OBJECT:splitContainer10).
      /*  */
      /* splitContainer9.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer9:Panel2:Controls:Add(THIS-OBJECT:GridEgetMaterial).
      THIS-OBJECT:splitContainer9:Size = NEW System.Drawing.Size(1145, 577).
      THIS-OBJECT:splitContainer9:SplitterDistance = 551.
      THIS-OBJECT:splitContainer9:TabIndex = 19.
      /*  */
      /* splitContainer10 */
      /*  */
      THIS-OBJECT:splitContainer10:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer10:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainer10:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainer10:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer10:Name = "splitContainer10".
      /*  */
      /* splitContainer10.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer10:Panel1:Controls:Add(THIS-OBJECT:GridMaterial).
      /*  */
      /* splitContainer10.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer10:Panel2:Controls:Add(THIS-OBJECT:matBtnOverMark).
      THIS-OBJECT:splitContainer10:Panel2:Controls:Add(THIS-OBJECT:matBtnBackMark).
      THIS-OBJECT:splitContainer10:Size = NEW System.Drawing.Size(549, 575).
      THIS-OBJECT:splitContainer10:SplitterDistance = 512.
      THIS-OBJECT:splitContainer10:TabIndex = 1.
      /*  */
      /* GridMaterial */
      /*  */
      THIS-OBJECT:GridMaterial:AllowDrop = TRUE.
      appearance426:BackColor = System.Drawing.SystemColors:Window.
      appearance426:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridMaterial:DisplayLayout:Appearance = appearance426.
      THIS-OBJECT:GridMaterial:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridMaterial:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance427:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance427:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance427:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance427:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:Appearance = appearance427.
      appearance428:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:BandLabelAppearance = appearance428.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance429:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance429:BackColor2 = System.Drawing.SystemColors:Control.
      appearance429:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance429:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridMaterial:DisplayLayout:GroupByBox:PromptAppearance = appearance429.
      THIS-OBJECT:GridMaterial:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridMaterial:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance430:BackColor = System.Drawing.SystemColors:Window.
      appearance430:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveCellAppearance = appearance430.
      appearance431:BackColor = System.Drawing.SystemColors:Highlight.
      appearance431:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:ActiveRowAppearance = appearance431.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance432:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CardAreaAppearance = appearance432.
      appearance433:BorderColor = System.Drawing.Color:Silver.
      appearance433:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellAppearance = appearance433.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:CellPadding = 0.
      appearance434:BackColor = System.Drawing.SystemColors:Control.
      appearance434:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance434:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance434:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance434:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:GroupByRowAppearance = appearance434.
      appearance435:TextHAlignAsString = "Left".
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderAppearance = appearance435.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance436:BackColor = System.Drawing.SystemColors:Window.
      appearance436:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:RowAppearance = appearance436.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance437:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridMaterial:DisplayLayout:Override:TemplateAddRowAppearance = appearance437.
      THIS-OBJECT:GridMaterial:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridMaterial:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridMaterial:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridMaterial:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridMaterial:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridMaterial:GuruContext = ?.
      THIS-OBJECT:GridMaterial:GuruDraggable = TRUE.
      THIS-OBJECT:GridMaterial:GuruDroppable = TRUE.
      THIS-OBJECT:GridMaterial:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridMaterial:Name = "GridMaterial".
      THIS-OBJECT:GridMaterial:Size = NEW System.Drawing.Size(512, 575).
      THIS-OBJECT:GridMaterial:TabIndex = 0.
      THIS-OBJECT:GridMaterial:Text = "grid1".
      /*  */
      /* matBtnOverMark */
      /*  */
      THIS-OBJECT:matBtnOverMark:Location = NEW System.Drawing.Point(2, 3).
      THIS-OBJECT:matBtnOverMark:Name = "matBtnOverMark".
      THIS-OBJECT:matBtnOverMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:matBtnOverMark:TabIndex = 10.
      THIS-OBJECT:matBtnOverMark:Text = "Markerade väljs".
      THIS-OBJECT:matBtnOverMark:Click:Subscribe(THIS-OBJECT:matBtnOverMark_Click).
      /*  */
      /* matBtnBackMark */
      /*  */
      THIS-OBJECT:matBtnBackMark:Location = NEW System.Drawing.Point(2, 34).
      THIS-OBJECT:matBtnBackMark:Name = "matBtnBackMark".
      THIS-OBJECT:matBtnBackMark:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:matBtnBackMark:TabIndex = 11.
      THIS-OBJECT:matBtnBackMark:Text = "Markerade bort".
      THIS-OBJECT:matBtnBackMark:Click:Subscribe(THIS-OBJECT:matBtnBackMark_Click).
      /*  */
      /* GridEgetMaterial */
      /*  */
      THIS-OBJECT:GridEgetMaterial:AllowDrop = TRUE.
      appearance414:BackColor = System.Drawing.SystemColors:Window.
      appearance414:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Appearance = appearance414.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance415:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance415:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance415:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance415:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:Appearance = appearance415.
      appearance416:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:BandLabelAppearance = appearance416.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance417:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance417:BackColor2 = System.Drawing.SystemColors:Control.
      appearance417:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance417:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:GroupByBox:PromptAppearance = appearance417.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance418:BackColor = System.Drawing.SystemColors:Window.
      appearance418:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveCellAppearance = appearance418.
      appearance419:BackColor = System.Drawing.SystemColors:Highlight.
      appearance419:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:ActiveRowAppearance = appearance419.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance420:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CardAreaAppearance = appearance420.
      appearance421:BorderColor = System.Drawing.Color:Silver.
      appearance421:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellAppearance = appearance421.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:CellPadding = 0.
      appearance422:BackColor = System.Drawing.SystemColors:Control.
      appearance422:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance422:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance422:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance422:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:GroupByRowAppearance = appearance422.
      appearance423:TextHAlignAsString = "Left".
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderAppearance = appearance423.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance424:BackColor = System.Drawing.SystemColors:Window.
      appearance424:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:RowAppearance = appearance424.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance425:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:Override:TemplateAddRowAppearance = appearance425.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridEgetMaterial:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridEgetMaterial:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridEgetMaterial:GuruContext = ?.
      THIS-OBJECT:GridEgetMaterial:GuruDraggable = TRUE.
      THIS-OBJECT:GridEgetMaterial:GuruDroppable = TRUE.
      THIS-OBJECT:GridEgetMaterial:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridEgetMaterial:Name = "GridEgetMaterial".
      THIS-OBJECT:GridEgetMaterial:Size = NEW System.Drawing.Size(588, 575).
      THIS-OBJECT:GridEgetMaterial:TabIndex = 1.
      THIS-OBJECT:GridEgetMaterial:Text = "grid1".
      THIS-OBJECT:GridEgetMaterial:GuruAfterDropped:Subscribe(THIS-OBJECT:GridEgetMaterial_GuruAfterDropped).
      /*  */
      /* TabImportMallar */
      /*  */
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:buttonBackAllaMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:buttonBackMarkMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:buttonOverAllaMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:buttonOverMarkMallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:mallarkoder).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:valdamallar).
      THIS-OBJECT:TabImportMallar:Controls:Add(THIS-OBJECT:mallar).
      THIS-OBJECT:TabImportMallar:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabImportMallar:Name = "TabImportMallar".
      THIS-OBJECT:TabImportMallar:Size = NEW System.Drawing.Size(1145, 577).
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
      THIS-OBJECT:mallarkoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
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
      THIS-OBJECT:mallarkoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:mallarkoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:mallarkoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
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
      THIS-OBJECT:valdamallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
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
      THIS-OBJECT:valdamallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:valdamallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:valdamallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
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
      THIS-OBJECT:mallar:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
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
      THIS-OBJECT:mallar:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:mallar:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:mallar:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:mallar:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:mallar:GuruContext = ?.
      THIS-OBJECT:mallar:Location = NEW System.Drawing.Point(19, 43).
      THIS-OBJECT:mallar:Name = "mallar".
      THIS-OBJECT:mallar:Size = NEW System.Drawing.Size(222, 277).
      THIS-OBJECT:mallar:TabIndex = 0.
      THIS-OBJECT:mallar:Text = "grid1".
      /*  */
      /* TabAvtalskalkyl */
      /*  */
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:LabelAvtal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:cmb_avtal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:Labelartalskj).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:cmb_artalavtal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:ultraTextEditor2).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:avtaltext).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:Labelklgartal).
      THIS-OBJECT:TabAvtalskalkyl:Controls:Add(THIS-OBJECT:cmb_KLGartal).
      THIS-OBJECT:TabAvtalskalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabAvtalskalkyl:Name = "TabAvtalskalkyl".
      THIS-OBJECT:TabAvtalskalkyl:Size = NEW System.Drawing.Size(1145, 577).
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
      /* TabFrikalkyl */
      /*  */
      THIS-OBJECT:TabFrikalkyl:Controls:Add(THIS-OBJECT:Gr).
      THIS-OBJECT:TabFrikalkyl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFrikalkyl:Name = "TabFrikalkyl".
      THIS-OBJECT:TabFrikalkyl:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* Gr */
      /*  */
      appearance477:BackColor = System.Drawing.SystemColors:Window.
      appearance477:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:Gr:DisplayLayout:Appearance = appearance477.
      THIS-OBJECT:Gr:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:Gr:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance478:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance478:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance478:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance478:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:Gr:DisplayLayout:GroupByBox:Appearance = appearance478.
      appearance479:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:Gr:DisplayLayout:GroupByBox:BandLabelAppearance = appearance479.
      THIS-OBJECT:Gr:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance480:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance480:BackColor2 = System.Drawing.SystemColors:Control.
      appearance480:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance480:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:Gr:DisplayLayout:GroupByBox:PromptAppearance = appearance480.
      THIS-OBJECT:Gr:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:Gr:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:Gr:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance481:BackColor = System.Drawing.SystemColors:Window.
      appearance481:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:Gr:DisplayLayout:Override:ActiveCellAppearance = appearance481.
      appearance482:BackColor = System.Drawing.SystemColors:Highlight.
      appearance482:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:Gr:DisplayLayout:Override:ActiveRowAppearance = appearance482.
      THIS-OBJECT:Gr:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:Gr:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance483:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:Gr:DisplayLayout:Override:CardAreaAppearance = appearance483.
      appearance484:BorderColor = System.Drawing.Color:Silver.
      appearance484:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:Gr:DisplayLayout:Override:CellAppearance = appearance484.
      THIS-OBJECT:Gr:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:Gr:DisplayLayout:Override:CellPadding = 0.
      appearance485:BackColor = System.Drawing.SystemColors:Control.
      appearance485:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance485:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance485:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance485:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:Gr:DisplayLayout:Override:GroupByRowAppearance = appearance485.
      appearance486:TextHAlignAsString = "Left".
      THIS-OBJECT:Gr:DisplayLayout:Override:HeaderAppearance = appearance486.
      THIS-OBJECT:Gr:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:Gr:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance487:BackColor = System.Drawing.SystemColors:Window.
      appearance487:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:Gr:DisplayLayout:Override:RowAppearance = appearance487.
      THIS-OBJECT:Gr:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance488:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:Gr:DisplayLayout:Override:TemplateAddRowAppearance = appearance488.
      THIS-OBJECT:Gr:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:Gr:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:Gr:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:Gr:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:Gr:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:Gr:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:Gr:GuruContext = ?.
      THIS-OBJECT:Gr:Location = NEW System.Drawing.Point(31, 63).
      THIS-OBJECT:Gr:Name = "Gr".
      THIS-OBJECT:Gr:Size = NEW System.Drawing.Size(884, 344).
      THIS-OBJECT:Gr:TabIndex = 0.
      THIS-OBJECT:Gr:Text = "grid1".
      /*  */
      /* TabFordelning */
      /*  */
      THIS-OBJECT:TabFordelning:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabFordelning:Name = "TabFordelning".
      THIS-OBJECT:TabFordelning:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* TabVolymber */
      /*  */
      THIS-OBJECT:TabVolymber:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVolymber:Name = "TabVolymber".
      THIS-OBJECT:TabVolymber:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* TabManager */
      /*  */
      appearance2:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance2.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFaktorer).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabMateriel).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabImportMallar).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFordelning).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFrikalkyl).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVolymber).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabKalkylHuvud).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabEgna).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabAvtalskalkyl).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabManager:TabIndex = 44.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      ultraTab13:Key = "Kalkylhuvud".
      ultraTab13:TabPage = THIS-OBJECT:TabKalkylHuvud.
      ultraTab13:Text = "Kalkylhuvud".
      ultraTab14:Key = "Kalkyl".
      ultraTab14:TabPage = THIS-OBJECT:TabKalkyl.
      ultraTab14:Text = "Kalkyl".
      appearance3:BackColor = System.Drawing.SystemColors:Control.
      ultraTab15:ClientAreaAppearance = appearance3.
      ultraTab15:Key = "Egna koder".
      ultraTab15:TabPage = THIS-OBJECT:TabEgna.
      ultraTab15:Text = "Egna koder".
      ultraTab16:Key = "Visa Kalkyl".
      ultraTab16:TabPage = THIS-OBJECT:TabVisa.
      ultraTab16:Text = "Visa kalkyl".
      ultraTab17:Key = "FaktorerPriser".
      ultraTab17:TabPage = THIS-OBJECT:TabFaktorer.
      ultraTab17:Text = "Faktorer/Priser".
      ultraTab19:Key = "Materiel".
      ultraTab19:TabPage = THIS-OBJECT:TabMateriel.
      ultraTab19:Text = "Materiel".
      ultraTab22:Key = "ImportMallar".
      ultraTab22:TabPage = THIS-OBJECT:TabImportMallar.
      ultraTab22:Text = "Import/Mallar".
      ultraTab20:Key = "Avtalskalkyl".
      ultraTab20:TabPage = THIS-OBJECT:TabAvtalskalkyl.
      ultraTab20:Text = "Avtalskalkyl".
      ultraTab21:Key = "Frikalkyl".
      ultraTab21:TabPage = THIS-OBJECT:TabFrikalkyl.
      ultraTab21:Text = "Frikalkyl".
      ultraTab23:Key = "Fordelning".
      ultraTab23:TabPage = THIS-OBJECT:TabFordelning.
      ultraTab23:Text = "Fördelning".
      ultraTab24:Key = "Volymber".
      ultraTab24:TabPage = THIS-OBJECT:TabVolymber.
      ultraTab24:Text = "Volymberäkning".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 11 NO-UNDO.
      arrayvar2[1] = ultraTab13.
      arrayvar2[2] = ultraTab14.
      arrayvar2[3] = ultraTab15.
      arrayvar2[4] = ultraTab16.
      arrayvar2[5] = ultraTab17.
      arrayvar2[6] = ultraTab19.
      arrayvar2[7] = ultraTab22.
      arrayvar2[8] = ultraTab20.
      arrayvar2[9] = ultraTab21.
      arrayvar2[10] = ultraTab23.
      arrayvar2[11] = ultraTab24.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar2).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:Kalkyltab_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 577).
      /*  */
      /* Kalkyl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "Kalkyl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:VisaExcelTab:ResumeLayout(FALSE).
      THIS-OBJECT:VisaPanelWaiting:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl16:ResumeLayout(FALSE).
      THIS-OBJECT:ultraTabPageControl17:ResumeLayout(FALSE).
      THIS-OBJECT:TabKalkylHuvud:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:Panel1:PerformLayout().
      THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:HuvudUtfardare, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudAnmarkning, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudKatalog, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:HuvudOmrade, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridPersonal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer6:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer6:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer6:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer7:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer7:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer7:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbetsKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer8:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer8:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer8:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridLopKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer11:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer11:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer11:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKalkylKoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridFriKalk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabEgna:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer2:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer5:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer5:Panel1:PerformLayout().
      THIS-OBJECT:splitContainer5:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer5:ResumeLayout(FALSE).
      THIS-OBJECT:egnakoderstrip:ResumeLayout(FALSE).
      THIS-OBJECT:egnakoderstrip:PerformLayout().
      CAST(THIS-OBJECT:GridEgnaKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer3:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer3:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer3:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer4:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer4:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer4:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridValdaUnderKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabVisa:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:ultraTabControl2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ultraTabControl2:ResumeLayout(FALSE).
      THIS-OBJECT:TabFaktorer:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer12:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer12:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer12:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridFaktorer, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridPriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabMateriel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer9:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer9:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer9:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer10:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer10:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer10:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridMaterial, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridEgetMaterial, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabImportMallar:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:mallarkoder, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:valdamallar, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:mallar, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabAvtalskalkyl:ResumeLayout(FALSE).
      THIS-OBJECT:TabAvtalskalkyl:PerformLayout().
      CAST(THIS-OBJECT:cmb_avtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmb_artalavtal, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:avtaltext, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:cmb_KLGartal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabFrikalkyl:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:Gr, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
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
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
      IF e:Tab:Text = "Kalkyl" THEN 
      DO:

      END.
      ELSE 
      DO:

      END.
      
      IF e:Tab:Text = "Visa Kalkyl" THEN 
      DO:

         IF THIS-OBJECT:WasExcelShown NE TRUE THEN DO:
            THIS-OBJECT:SetVisningsBild(0).
            THIS-OBJECT:VisaExcel:Initialize().
            THIS-OBJECT:VisaExcel:Fix().
            THIS-OBJECT:GenereraKalkylVisning().
            THIS-OBJECT:WasExcelShown = TRUE.
         END.
         
         THIS-OBJECT:RibbonTabKalkVisning:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabKalkVisning:Visible = FALSE.
      END.
      
      IF e:Tab:Text = "Faktorer" THEN 
      DO:

      END.
      
      IF e:Tab:Text = "Egna koder" OR e:Tab:Text = "Kalkyl" THEN 
      DO:
         THIS-OBJECT:RibbonTabKoder:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabKoder:Visible = FALSE.
      END.
      
      IF e:Tab:Text = "Materiel" THEN 
      DO:
         THIS-OBJECT:RibbonTabMat:Visible = TRUE.
      END.
      ELSE 
      DO:
         THIS-OBJECT:RibbonTabMat:Visible = FALSE.
      END.
      
      /* cfredrik; KalkDummy ser till att följande kod inte körs när man startar programmet, då SelectedTabChanged-eventet körs då också.*/
      IF THIS-OBJECT:KalkDummy = TRUE THEN 
      DO:
         DEFINE VARIABLE IsError   AS LOGICAL   INITIAL FALSE NO-UNDO.
         DEFINE VARIABLE ErrorText AS CHARACTER INITIAL "" NO-UNDO. 

         IF e:PreviousSelectedTab:Text = "Kalkylhuvud" THEN 
         DO:
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE  =     STRING(THIS-OBJECT:HuvudBenamning:GuruText:Text).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudKalknr:GuruText:Text).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKANV"):BUFFER-VALUE    =      STRING(THIS-OBJECT:PersonalTT:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE    =     STRING(THIS-OBJECT:HuvudTyp:GuruCombo:VALUE).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("OMRADE"):BUFFER-VALUE     =     STRING(THIS-OBJECT:HuvudOmrade:VALUE).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("KLOGID"):BUFFER-VALUE     =         STRING(THIS-OBJECT:HuvudKatalog:VALUE).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE  = STRING(THIS-OBJECT:HuvudUtfardare:VALUE).
            THIS-OBJECT:HuvudTT:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = STRING(THIS-OBJECT:HuvudAnmarkning:Text).
            THIS-OBJECT:Root:DatabaseManager:Kalkyl:SaveHuvud(OUTPUT ErrorText, OUTPUT IsError).
            IF IsError = TRUE THEN 
            DO:
               System.Windows.Forms.MessageBox:Show(ErrorText).
               IsError = FALSE.
               THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
               RETURN. 
            END.
            
            ASSIGN 
               THIS-OBJECT:HuvudKalknr:GuruText:Text = STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
               THIS-OBJECT:HuvudKalknr:VISIBLE       = TRUE. 
               
            IF THIS-OBJECT:NyKalkyl = TRUE THEN 
            DO:
               THIS-OBJECT:LaddaKalkyl(INTEGER(STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)),STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("OMRADE"):BUFFER-VALUE)).
               THIS-OBJECT:RibbonToolOmrade:Text = THIS-OBJECT:HuvudOmrade:Text.
               THIS-OBJECT:RibbonToolKat:Text = THIS-OBJECT:HuvudKatalog:Text.
               

   
               /* Sätt typ */
               THIS-OBJECT:UpdateRibbonShow().
               
               
               THIS-OBJECT:RibbonTabKalkyl:Caption = "Kalkyl " + STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE).

               THIS-OBJECT:NyKalkyl = FALSE.
               THIS-OBJECT:CreateRibbonMatris().
               THIS-OBJECT:RibbonUpdateTyp(?,?).
            END.
            
            THIS-OBJECT:UpdateRibbonShow().
            
         END.
         
         RETURN.
      END.
      THIS-OBJECT:KalkDummy = TRUE.
      
      IF THIS-OBJECT:NyKalkyl = FALSE THEN THIS-OBJECT:CreateRibbonMatris().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID Kalkyltab_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):

   END METHOD.

   METHOD PUBLIC VOID Kill().
      
   END METHOD.
   
   
   
   METHOD PUBLIC LOGICAL SkapaKalkyl():
      /* skapa kalkyl i databas */
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateKalkyl() = FALSE THEN RETURN FALSE.

      THIS-OBJECT:HuvudTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:HuvudTT.
      THIS-OBJECT:KatalogTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KatalogTT.


      /* Hitta rätt katalog*/
      KatalogTT:FIND-LAST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTT:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK). 

      THIS-OBJECT:NyKalkyl = TRUE.
      /* Initialisera form */      
      THIS-OBJECT:InitializeForm().        
      visatabs("ALLA", FALSE).
      visatabs(THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:TEXT, TRUE).
      visatabs(THIS-OBJECT:TabManager:Tabs["Kalkyl"]:TEXT, TRUE).
      THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:Selected = TRUE.
      
   
   END METHOD.
   
   METHOD PUBLIC LOGICAL LaddaKalkyl(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      /* Sätt kalkylvariablar*/
      THIS-OBJECT:NyKalkyl = FALSE.
      THIS-OBJECT:kalknrvar = kalknr.
      THIS-OBJECT:omradevar = omr.

      /* Hämta kalkyl från databas */
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Laddar: Hämtar kalkyl ifrån databas..").
      IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchKalkyl(THIS-OBJECT:kalknrvar, THIS-OBJECT:omradevar) = FALSE THEN RETURN FALSE.

      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Laddar: Skapar temp-table-handles..").

      THIS-OBJECT:MarkningTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:MarkningTT.
      THIS-OBJECT:HuvudTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:HuvudTT.
      THIS-OBJECT:KatalogTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KatalogTT.
      THIS-OBJECT:PriserTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:PriserTT.
      THIS-OBJECT:ValdaPriserTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ValdaPriserTT.
      THIS-OBJECT:ArbetskoderTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:ArbetskoderTT.
      THIS-OBJECT:LopposterTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:LopposterTT.
      THIS-OBJECT:KoderTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KoderTT.
      /*THIS-OBJECT:FriKalkTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:FriKalkTT.*/
      
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Laddar: Hämtar material ifrån databas..").
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:FetchBestMaterial(THIS-OBJECT:kalknrvar,THIS-OBJECT:omradevar ).
      THIS-OBJECT:BestMaterialTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BestMaterialTT.


      /* Hitta rätt katalog*/
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Laddar: Hämta katalog..").
      KatalogTT:FIND-LAST("WHERE kalkylkatalogtt.KLOGID = " + HuvudTT:BUFFER-FIELD("KLOGID"):BUFFER-VALUE, NO-LOCK). 

      /* Initialisera form */
      THIS-OBJECT:Root:Logger:WriteLine("Kalkyl: Laddar: Initializerar formen..").
      THIS-OBJECT:InitializeForm().
      visatabs("ALLA", TRUE).
      
      THIS-OBJECT:TabManager:Tabs["Kalkyl"]:Selected = TRUE.
      
      THIS-OBJECT:FilterUnderKoder().

      
   END METHOD.

   DEFINE PUBLIC VARIABLE NyKalkylCreated AS LOGICAL INITIAL FALSE NO-UNDO.

   METHOD PRIVATE VOID InitializeForm ():
     
      /* Kalkylhuvudtab*/
      IF NyKalkylCreated = FALSE THEN DO:
         personalgrid ().
         Typcmb ().
         Omrcmb ().
         Katalogcmb ().        
         Utfcmb (). 
      
         THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollStart = TRUE.
         THIS-OBJECT:HuvudBenamning:GuruText:GuruKontrollText = TRUE.
    
         ASSIGN
            THIS-OBJECT:Labelomr:TEXT                 = Guru.Konstanter:gomrk
   
            THIS-OBJECT:HuvudBenamning:GuruLabel:Text = "Benämning"
            THIS-OBJECT:HuvudBenamning:GuruText:Text  = THIS-OBJECT:HuvudTT:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE

            THIS-OBJECT:HuvudAnmarkning:Text          = THIS-OBJECT:HuvudTT:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE

            THIS-OBJECT:HuvudKalknr:GuruLabel:Text    = "Kalkylnr."
            THIS-OBJECT:HuvudKalknr:GuruText:Text     = STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKNR"):BUFFER-VALUE)
            THIS-OBJECT:HuvudKalknr:GuruText:Enabled  = FALSE          

            THIS-OBJECT:HuvudTyp:GuruLabel:Text       = "Kalkyltyp"

            THIS-OBJECT:HuvudAnsvarig:GuruLabel:Text  = "Ansvarig"
            THIS-OBJECT:HuvudAnsvarig:GuruText:Text   = THIS-OBJECT:PersonalTT:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:PersonalTT:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
            
         THIS-OBJECT:NyKalkylCreated = TRUE.
      END.

      IF THIS-OBJECT:NyKalkyl = FALSE THEN 
      DO:
         ASSIGN
            THIS-OBJECT:HuvudKatalog:ENABLED = FALSE
            THIS-OBJECT:HuvudOmrade:ENABLED  = FALSE
            THIS-OBJECT:HuvudTyp:ENABLED     = FALSE.
         /*kalkyltab */
         THIS-OBJECT:arbkodgrid().
         THIS-OBJECT:lopnrgrid().
         THIS-OBJECT:kalkkodergrid().
         THIS-OBJECT:frikalksubgrid().
         THIS-OBJECT:DGKoder = NEW Controls.Subclasses.DragDropGroup("Koder").
         THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridLopKoder).
         THIS-OBJECT:DGKoder:AddChild(THIS-OBJECT:GridKalkylKoder).
      

      
         /* egna kodertab */
         THIS-OBJECT:egnakodergrid().
         THIS-OBJECT:kalkprisergrid().
         THIS-OBJECT:valdaprisergrid().
      
         THIS-OBJECT:GridKalkylKoder:GuruFirstrow().
         THIS-OBJECT:GridValdaUnderKoder:GuruFirstRow().

         THIS-OBJECT:DGEgna = NEW Controls.Subclasses.DragDropGroup("Egna").
         THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridUnderKoder).
         THIS-OBJECT:DGEgna:AddChild(THIS-OBJECT:GridValdaUnderKoder).
         /* btnover*/
      
         THIS-OBJECT:KalkylBtnOverMark:GuruKoppla(THIS-OBJECT:GridLopKoder,THIS-OBJECT:GridKalkylKoder).
         THIS-OBJECT:KalkylBtnOverMark:GuruDublett = TRUE.
         THIS-OBJECT:KalkylBtnOverAll:GuruKoppla(THIS-OBJECT:GridLopKoder,THIS-OBJECT:GridKalkylKoder).
         THIS-OBJECT:KalkylBtnOverAll:GuruDublett = TRUE.
         THIS-OBJECT:KalkylBtnOverAll:GuruAllaover = TRUE.
      
         THIS-OBJECT:KalkylBtnBackMark:GuruKoppla(THIS-OBJECT:GridKalkylKoder,THIS-OBJECT:GridLopKoder).
         THIS-OBJECT:KalkylBtnBackMark:GuruRemoveFrom = TRUE.  
         THIS-OBJECT:KalkylBtnBackAll:GuruKoppla(THIS-OBJECT:GridKalkylKoder,THIS-OBJECT:GridLopKoder).              
         THIS-OBJECT:KalkylBtnBackAll:GuruRemoveFrom = TRUE.
         THIS-OBJECT:KalkylBtnBackAll:GuruAllaover = TRUE.
           
           
         /* Egna koder börjar här*/
         THIS-OBJECT:EgnaBtnOverMark:GuruKoppla(THIS-OBJECT:GridUnderKoder,THIS-OBJECT:GridValdaUnderKoder).
         THIS-OBJECT:EgnaBtnOverMark:GuruDublett = FALSE.
         THIS-OBJECT:EgnaBtnOverAll:GuruKoppla(THIS-OBJECT:GridUnderKoder, THIS-OBJECT:GridValdaUnderKoder).
         THIS-OBJECT:EgnaBtnOverAll:GuruDublett = FALSE.
         THIS-OBJECT:EgnaBtnOverAll:GuruAllaover = TRUE.
      
         THIS-OBJECT:EgnaBtnBackMark:GuruKoppla(THIS-OBJECT:GridValdaUnderKoder, THIS-OBJECT:GridUnderKoder).
         THIS-OBJECT:EgnaBtnBackMark:GuruRemoveFrom = TRUE.
         THIS-OBJECT:EgnaBtnBackAll:GuruKoppla(THIS-OBJECT:GridValdaUnderKoder, THIS-OBJECT:GridUnderKoder).
         THIS-OBJECT:EgnaBtnBackAll:GuruRemoveFrom = TRUE.
         THIS-OBJECT:EgnaBtnBackAll:GuruAllaover = TRUE.

         /*materialtab*/
      
         THIS-OBJECT:matBtnOverMark:GuruKoppla(THIS-OBJECT:GridMaterial,THIS-OBJECT:GridEgetMaterial).
         THIS-OBJECT:matBtnOverMark:GuruDublett = FALSE.
      
         THIS-OBJECT:matBtnBackMark:GuruKoppla(THIS-OBJECT:GridEgetMaterial, THIS-OBJECT:GridMaterial).
         THIS-OBJECT:matBtnBackMark:GuruRemoveFrom = TRUE.
      
         /* combo*/
      
         /* grid material */
         DEFINE VARIABLE matlist AS Controls.GridRubrikLista NO-UNDO.
         DEFINE VARIABLE matben  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE matenh  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE matenr  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE matnet  AS Controls.GridRubrik      NO-UNDO.
         matlist = NEW Controls.GridRubrikLista().
         matlist:ttBufferHandle = THIS-OBJECT:MaterialTT.
         matlist:multiselect = TRUE.
         matlist:filter = "".
         matenr = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk , TRUE ,75).
         matlist:AddRubrik(matenr).
         matben = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,150).
         matlist:AddRubrik(matben).
         matenh = NEW Controls.GridRubrik("ENHET", "st", TRUE ,50).
         matlist:AddRubrik(matenh).
         matnet = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,50).
         matlist:AddRubrik(matnet).
         
         THIS-OBJECT:GridMaterial:guruInitGrid(matlist).
         THIS-OBJECT:GridMaterial:Text = "Materiel".

         /* grid materialspec */
         DEFINE VARIABLE ematlist AS Controls.GridRubrikLista NO-UNDO.
         DEFINE VARIABLE ematben  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematenh  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematenr  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematnet  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematlev  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematkpr  AS Controls.GridRubrik      NO-UNDO.
         DEFINE VARIABLE ematmat AS Controls.GridRubrik      NO-UNDO.
         ematlist = NEW Controls.GridRubrikLista().
         ematlist:ttBufferHandle = THIS-OBJECT:BestMaterialTT.
         ematlist:multiselect = TRUE.
         ematlist:filter = "".
         
         ematenr = NEW Controls.GridRubrik("ENR", Guru.Konstanter:genk, TRUE ,75).
         ematlist:AddRubrik(ematenr).
         ematben = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE ,150).
         ematlist:AddRubrik(ematben).
         ematenh = NEW Controls.GridRubrik("ENHET", "st", TRUE ,50).
         ematlist:AddRubrik(ematenh).
         ematnet = NEW Controls.GridRubrik("NPRIS", "Nettopris", TRUE ,50).
         ematlist:AddRubrik(ematnet).
         ematkpr = NEW Controls.GridRubrik("KPRIS", "Kundpris", TRUE ,50).
         ematlist:AddRubrik(ematkpr).
         ematlev = NEW Controls.GridRubrik("LEVKOD", "Lev-id.", TRUE ,50).
         ematlist:AddRubrik(ematlev).
         
         
         ematmat = NEW Controls.GridRubrik("MATRIS", "Matris.", TRUE ,50).
         ematlist:AddRubrik(ematmat).
         
         
         
         /* +kundpris, +levid, +antal*/
         
         THIS-OBJECT:GridEgetMaterial:guruInitGrid(ematlist).
         THIS-OBJECT:GridEgetMaterial:Text = "Materiel-specifikation".

         THIS-OBJECT:DGMat = NEW Controls.Subclasses.DragDropGroup("Material").
         THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridMaterial).
         THIS-OBJECT:DGMat:AddChild(THIS-OBJECT:GridEgetMaterial).

      /* DEFINE VARIABLE colorEnr AS Controls.Subclasses.GuruGridColorFilter NO-UNDO.
       colorEnr = NEW Controls.Subclasses.GuruGridColorFilter(THIS-OBJECT:GridMaterial, true, "ENR", "KUND NE FALSE").
       colorEnr:BGCOLOR = System.Drawing.Color:Turquoise.
       colorEnr:Activated = TRUE.
       colorEnr:SetBg = TRUE.
       DEFINE VARIABLE colorNpris AS Controls.Subclasses.GuruGridColorFilter NO-UNDO.
       colorNpris = NEW Controls.Subclasses.GuruGridColorFilter(THIS-OBJECT:GridMaterial, true, "NPRIS", "KUND NE FALSE").
       colorNpris:BGCOLOR = System.Drawing.Color:Turquoise.
       colorNpris:Activated = TRUE.
       colorNpris:SetBg = TRUE.

       THIS-OBJECT:GridMaterial:AddColorFilter(colorEnr).
       THIS-OBJECT:GridMaterial:AddColorFilter(colorNpris).
*/


      END.
      

   END METHOD.



   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID SkapaNyEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.      
      THIS-OBJECT:dontcheck = TRUE.
      rid = THIS-OBJECT:Root:DatabaseManager:Kalkyl:CreateEgenKod().
      THIS-OBJECT:GridEgnaKoder:GuruReopen().
      THIS-OBJECT:GridEgnaKoder:GuruRepositionto(rid).
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:dontcheck= FALSE.

      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortEgen_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF rrr:ToString() NE "Yes" THEN RETURN.
               
      THIS-OBJECT:Root:DatabaseManager:Kalkyl:RemoveEgenKod().
      GridEgnaKoder:GuruBortrader(FALSE).       
      THIS-OBJECT:GridKalkylKoder:GuruReopen().
      THIS-OBJECT:GridValdaUnderKoder:GuruReopen().
      
      THIS-OBJECT:FilterUnderKoder().
      RETURN.
   
   END METHOD.

   METHOD PUBLIC VOID TabRefreshKalkylHuvud():
      
   END METHOD.

   METHOD PRIVATE VOID personalgrid ():
      DEFINE VARIABLE villkor       AS CHARACTER                NO-UNDO.
      DEFINE VARIABLE personallist  AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE personalPKOD  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalFNAMN AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE personalENAMN AS Controls.GridRubrik      NO-UNDO. 
          
      personallist = NEW Controls.GridRubrikLista().
      personallist:ttBufferHandle = THIS-OBJECT:Root:DatabaseManager:Global:PersonalTT. /* THIS-OBJECT:PersonalTT. */           
      personalPKOD = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sing",TRUE,75).
      personallist:addRubrik(personalPKOD).
      personalFNAMN = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,150).
      personallist:addRubrik(personalFNAMN).
      personalENAMN = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,150).
      personallist:addRubrik(personalENAMN).
      /*init av grid eg sparar alla egenskaper mm*/   
      THIS-OBJECT:GridPersonal:guruInitGrid(personallist).  
      THIS-OBJECT:GridPersonal:Text = "Kalkylansvarig".
      /*
     personalkalkyl:GurufirstRow().
     */ 
      villkor = "WHERE PERSONALKOD = '" + THIS-OBJECT:HuvudTT:BUFFER-FIELD("KALKANV"):BUFFER-VALUE + "'".                 
      THIS-OBJECT:PersonalTT:FIND-FIRST(villkor). 
      IF THIS-OBJECT:PersonalTT:AVAILABLE THEN 
      DO:                   
         THIS-OBJECT:GridPersonal:GuruRepositionTo(THIS-OBJECT:PersonalTT:ROWID).              
      END.   
      /*egna event*/
      THIS-OBJECT:GridPersonal:tBS:PositionChanged:Subscribe(valuechpers).
      
   END METHOD.

   METHOD PRIVATE VOID arbkodgrid ():

      DEFINE VARIABLE arbkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE arbkoderArb  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE arbkoderBen  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE arbkoderMark    AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE arbkoderMarksub AS Controls.GridRubrik NO-UNDO.
                              
      arbkoderlist = NEW Controls.GridRubrikLista().
      arbkoderlist:ttBufferHandle = THIS-OBJECT:ArbetskoderTT.
      arbkoderlist:filter = "kalkylarbkodertt.TYPKALK = " + STRING(THIS-OBJECT:HuvudTT:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE).
      
      arbkoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,48).
      arbkoderlist:addRubrik(arbkoderArb).
      
      arbkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      arbkoderlist:addRubrik(arbkoderBen).
      
      arbkoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",TRUE,64).
      arbkoderlist:addRubrik(arbkoderMark).
      
      arbkoderMarksub = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",TRUE,125).
      arbkoderlist:addRubrik(arbkoderMarksub).
      
      /*init av grid eg sparar alla egenskaper mm*/   
      GridArbetsKoder:guruInitGrid(arbkoderlist).  
      GridArbetsKoder:TEXT = "Arbetskoder".      
      /*egna event*/
      GridArbetsKoder:tBS:PositionChanged:Subscribe(valuech).
   END METHOD.

   METHOD PRIVATE VOID valuech (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      IF THIS-OBJECT:ArbetskoderTT:AVAILABLE  THEN 
      DO:
         THIS-OBJECT:GridLopKoder:GuruFiltrera("kalkylloppostertt.ARBKOD = " + "'" + STRING(THIS-OBJECT:ArbetskoderTT:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE)+ "'").
         THIS-OBJECT:GridLopKoder:GuruFirstrow().
      END.
   END METHOD.

   METHOD PRIVATE VOID frikalksubgrid():
      DEFINE VARIABLE frikalkList AS Controls.GridRubrikLista NO-UNDO.
      /*DEFINE VARIABLE frikalkTim AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE frikalkKost AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE frikalkPris AS Controls.GridRubrik NO-UNDO.*/
      DEFINE VARIABLE frikalkfTim  AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE frikalkfKost AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE frikalkfPris AS Controls.GridRubrik NO-UNDO.
      DEFINE VARIABLE frikalkfBen AS Controls.GridRubrik NO-UNDO.
      /*DEFINE VARIABLE frikalkBen AS Controls.GridRubrik NO-UNDO.*/
      frikalklist = NEW Controls.GridRubrikLista().
      frikalklist:ttBufferHandle = THIS-OBJECT:ValdaPriserTT.
      frikalklist:multiselect = FALSE.
      
      /*frikalkBen = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 100).
      frikalkList:addRubrik(frikalkBen).*/
      
      
      frikalkfBen = NEW Controls.GridRubrik("FRIBENAMNING", "Fri benämning", false, 100).
      frikalkList:addRubrik(frikalkfBen).

      /*frikalkTim = NEW Controls.GridRubrik("TIMMAR", "Timmar", true, 50).
      frikalkList:addRubrik(frikalkTim).*/
      
      frikalkfTim = NEW Controls.GridRubrik("FRITIMMAR", "Fri timmar", false, 50).
      frikalkList:addRubrik(frikalkfTim).
      
      
      /*frikalkKost = NEW Controls.GridRubrik("KOSTNAD", "Kostnad", true, 75).
      frikalkList:addRubrik(frikalkKost).*/
      
      frikalkfKost = NEW Controls.GridRubrik("FRIKOSTNAD", "Fri kostnad", false, 75).
      frikalkList:addRubrik(frikalkfKost).
      
      
      
      /*frikalkPris = NEW Controls.GridRubrik("PRIS", "Pris", true, 75).
      frikalkList:addRubrik(frikalkPris).*/
      
      frikalkfPris = NEW Controls.GridRubrik("FRIPRIS", "Fri pris", false, 75).
      frikalkList:addRubrik(frikalkfPris).
      

      
      THIS-OBJECT:GridFriKalk:guruInitGrid(frikalklist).
      THIS-OBJECT:GridFriKalk:Text = "Frikalkylkoder".
      THIS-OBJECT:GridFriKalk:GuruFirstrow().
      THIS-OBJECT:splitContainer6:SplitterDistance = THIS-OBJECT:ClientSize:Width * 0.35.
      THIS-OBJECT:splitContainer11:SplitterDistance =  THIS-OBJECT:splitContainer11:Width / 2.

      
   END METHOD.


   METHOD PRIVATE VOID lopnrgrid ():
      DEFINE VARIABLE lopkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE lopkoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE lopkoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE lopkoderBen  AS Controls.GridRubrik      NO-UNDO.
      
      lopkoderlist = NEW Controls.GridRubrikLista().
      lopkoderlist:ttBufferHandle = THIS-OBJECT:LopposterTT.
      lopkoderlist:multiselect = TRUE.   
      
      lopkoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,35).
      lopkoderlist:addRubrik(lopkoderLop).
      lopkoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,150).
      lopkoderlist:addRubrik(lopkoderBen).
      lopkoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      lopkoderlist:addRubrik(lopkoderEnh).
      /*init av grid*/            
      THIS-OBJECT:GridLopKoder:guruInitGrid(lopkoderlist). 
      THIS-OBJECT:GridLopKoder:TEXT = "Löpnummer".  
      /*default filter  sortering*/      
      THIS-OBJECT:GridLopKoder:GuruOrderby("BY ARBKOD BY LOPNR").  
      THIS-OBJECT:GridLopKoder:GuruFirstrow().
     
   END METHOD.


   METHOD PRIVATE INTEGER GetCurrentType():
      RETURN CAST(THIS-OBJECT:RibbonToolVisaTyp:ValueList:ValueListItems[THIS-OBJECT:RibbonToolVisaTyp:ValueList:SelectedIndex]:DataValue, Helpers.ABLDataContainer):GetInteger("Typ").
   END METHOD.

   METHOD PRIVATE CHARACTER GetMatrisName():
      DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:HuvudTT:Buffer-field("TYPKALK"):Buffer-Value = 7 THEN mName = "Period".
      ELSE mName = "Matris".
      RETURN mName.
   END METHOD.

   METHOD PRIVATE VOID kalkkodergrid ():
      DEFINE VARIABLE kalkylkoderlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE kalkylkoderArb  AS Controls.GridRubrik      NO-UNDO.   
      DEFINE VARIABLE kalkylkoderLop  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderEnh  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderBen  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderMat  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderAnt  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSum  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderFSum  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkodertt AS Controls.GridRubrik NO-UNDO.

      DEFINE VARIABLE kalkylkoderMark  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderSubmark  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE kalkylkoderRisk AS Controls.GridRubrik       NO-UNDO.
      DEFINE VARIABLE kalkylkoderVinst  AS Controls.GridRubrik      NO-UNDO.

      kalkylkoderlist = NEW Controls.GridRubrikLista().
      kalkylkoderlist:ttBufferHandle = THIS-OBJECT:KoderTT.
      kalkylkoderlist:multiselect = TRUE.  


      DEFINE VARIABLE egnakoderNum  AS Controls.GridRubrik      NO-UNDO.      
     
      egnakoderNum = NEW Controls.GridRubrik("NUM","#",TRUE,35).
         kalkylkoderlist:addRubrik(egnakoderNum). 

      kalkylkoderMat = NEW Controls.GridRubrik("MATRIS",GetMatrisName(),FALSE,35).
      
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
      
      
      kalkylkoderFSum = NEW Controls.GridRubrik("FRITOTKOST","Frikalk. Summa",TRUE,55).
      kalkylkoderlist:addRubrik(kalkylkoderFSum).
      
      kalkylkoderMark = NEW Controls.GridRubrik("MARKNING","Märkning",false,100).
      kalkylkoderlist:addRubrik(kalkylkoderMark).
      kalkylkoderSubmark = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",false,75).
      kalkylkoderlist:addRubrik(kalkylkoderSubmark).
      
      kalkylkoderRisk = NEW Controls.GridRubrik("RISK","Risk",false,75).
      kalkylkoderlist:addRubrik(kalkylkoderRisk).
      kalkylkoderVinst = NEW Controls.GridRubrik("VINST","Vinst",false,75).
      kalkylkoderlist:addRubrik(kalkylkoderVinst).
      
      /*init av grid*/
      THIS-OBJECT:GridKalkylKoder:guruInitGrid(kalkylkoderlist). 
      THIS-OBJECT:GridKalkylKoder:TEXT = "Kalkylkoder".  
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Hidden = TRUE.   
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["RISK"]:Hidden = TRUE.
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["VINST"]:Hidden = TRUE.
      
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      THIS-OBJECT:GridKalkylKoder:DisplayLayout:Bands[0]:Columns["FRITOTKOST"]:Format = "0".
      /*default filter  sortering*/     
      /*
      kalkylkoder:GuruOrderby("BY MATRIS BY ARBKOD BY LOPNR BY NUM").
      */
      THIS-OBJECT:GridKalkylKoder:GuruOrderby("BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridKalkylKoder:tBS:PositionChanged:Subscribe(valuechkalkkoder).
      THIS-OBJECT:GridKalkylKoder:GuruFirstrow().
      
      
      

      
   END METHOD.

   METHOD PRIVATE VOID valuechkalkkoder (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/ 
      IF THIS-OBJECT:GridKalkylKoder:tBS:Count > 0 THEN 
      DO:
         IF THIS-OBJECT:dontcheck = TRUE THEN 
         DO:
         END.
         ELSE 
         DO:
            DEFINE VARIABLE exrid AS RECID NO-UNDO.  
            IF STRING(THIS-OBJECT:KoderTT:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) = "EGEN" THEN 
            DO: 
               exrid = INTEGER(STRING(THIS-OBJECT:KoderTT:BUFFER-FIELD("TTRECID"):BUFFER-VALUE)).           
               THIS-OBJECT:GridEgnaKoder:GuruRepositionTo( THIS-OBJECT:KoderTT:ROWID /* ROWID(kalknumtt) */).
               DEFINE VARIABLE q AS CHARACTER NO-UNDO.
               q = STRING("WHERE kalknumtt.TTRECID = " + STRING(exrid)).
               THIS-OBJECT:KoderTT:FIND-FIRST(q, NO-LOCK).
               /*FIND FIRST kalknumtt WHERE kalknumtt.TTRECID = exrid NO-LOCK NO-ERROR.*/
               THIS-OBJECT:FilterUnderKoder().  
            END. 
         END.
      END.
   END METHOD.
   
   METHOD PRIVATE VOID egnavaluech (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      

   END METHOD.


   METHOD PRIVATE VOID egnakodergrid():
      THIS-OBJECT:dontcheck = TRUE.
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
      egnakoderlist:ttBufferHandle = THIS-OBJECT:KoderTT.
      egnakoderlist:multiselect = TRUE.  
      egnakoderlist:filter = "kalknumtt.ARBKOD = 'EGEN'".
      egnakoderNum = NEW Controls.GridRubrik("NUM","#",TRUE,35).
      egnakoderlist:addRubrik(egnakoderNum). 
      egnakoderMat = NEW Controls.GridRubrik("MATRIS",GetMatrisName(),FALSE,35).
      egnakoderlist:addRubrik(egnakoderMat). 
      egnakoderArb = NEW Controls.GridRubrik("ARBKOD","Arbkod",TRUE,55).      
      egnakoderlist:addRubrik(egnakoderArb).
      egnakoderLop = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,45).
      egnakoderlist:addRubrik(egnakoderLop).
      egnakoderEnh = NEW Controls.GridRubrik("ENHET","Enh",TRUE,35).
      egnakoderlist:addRubrik(egnakoderEnh).
      egnakoderAnt = NEW Controls.GridRubrik("ANTAL","Antal",FALSE,45).
      egnakoderlist:addRubrik(egnakoderAnt).
      egnakoderBen = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,150).
      egnakoderlist:addRubrik(egnakoderBen).
      egnakoderSum = NEW Controls.GridRubrik("TOTKOST","Summa",TRUE,55).
      egnakoderlist:addRubrik(egnakoderSum).
      /*init av grid*/
      THIS-OBJECT:GridEgnaKoder:guruInitGrid(egnakoderlist). 
      GridEgnaKoder:TEXT = "Egna koder".     
      /*default filter  sortering*/     
      
      THIS-OBJECT:GridEgnaKoder:DisplayLayout:Bands[0]:Columns["TOTKOST"]:Format = "0".
      
      THIS-OBJECT:GridEgnaKoder:GuruOrderby("BY MATRIS BY ARBKOD BY LOPNR BY NUM").
      THIS-OBJECT:GridEgnaKoder:tBS:PositionChanged:Subscribe(egnavaluech).
      THIS-OBJECT:GridEgnaKoder:GuruFirstrow().
      THIS-OBJECT:dontcheck = false.
      /*THIS-OBJECT:egnavaluech(?,?).*/
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
      kalkpriserlist:ttBufferHandle = THIS-OBJECT:PriserTT.
      kalkpriserlist:multiselect = TRUE.
      kalkpriserlist:filter = "EGENKODUPP = TRUE".
      kalkpriserBenm = NEW Controls.GridRubrik("BENAMNING", "Benämning", TRUE, 150).
      kalkpriserlist:addRubrik(kalkpriserBenm).
      
      kalkpriserPris = NEW Controls.GridRubrik("PRIS", "Pris", TRUE, 35).
      kalkpriserlist:addRubrik(kalkpriserPris).
      THIS-OBJECT:GridUnderKoder:guruInitGrid(kalkpriserlist).
      THIS-OBJECT:GridUnderKoder:TEXT = "Underkoder".
      THIS-OBJECT:GridUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridUnderKoder:GuruFirstrow().
      RETURN.
   END METHOD.
   
   METHOD PRIVATE VOID valdaprisergrid():
      DEFINE VARIABLE valdapriserlist    AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE valdapriserBenm    AS Controls.GridRubrik      NO-UNDO.
      
      DEFINE VARIABLE valdapriserTimmar  AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserPris    AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE valdapriserKostnad AS Controls.GridRubrik      NO-UNDO.
      valdapriserlist = NEW Controls.GridRubrikLista().
      valdapriserlist:ttBufferHandle = THIS-OBJECT:ValdaPriserTT.
      valdapriserlist:multiselect = TRUE.
      valdapriserlist:filter = "".
      DEFINE VARIABLE valdapriserNum AS Controls.GridRubrik NO-UNDO.
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
      THIS-OBJECT:GridValdaUnderKoder:guruInitGrid(valdapriserlist).
      THIS-OBJECT:GridValdaUnderKoder:TEXT = "Valda underkoder".
      THIS-OBJECT:GridValdaUnderKoder:GuruDroppable = tRUE.
      THIS-OBJECT:GridValdaUnderKoder:GuruOrderby("BY BENAMNING").
      THIS-OBJECT:GridValdaUnderKoder:GuruFirstrow().
      RETURN.
   END METHOD.





   METHOD PRIVATE VOID valuechpers (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/   
      
      /* HEHEHE NEDAN*/
      THIS-OBJECT:HuvudAnsvarig:GuruText:Text = THIS-OBJECT:PersonalTT:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:PersonalTT:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
           
   END METHOD.
  
  
   METHOD PRIVATE VOID Typcmb  ():
      DEFINE VARIABLE cmbTyplist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE typK       AS Controls.GridRubrik      NO-UNDO.
          
      cmbTyplist = NEW Controls.GridRubrikLista().
      cmbTyplist:ttBufferHandle = TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE.
      typK = NEW Controls.GridRubrik().
      typK:Falt = "TYPC".
      typK:Rubrik = "Kalkyltyp".
      cmbTyplist:addRubrik(typK).                    
      
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN 
      DO:
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 1
            typtt.TYPC    = "Typ 1".
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 2
            typtt.TYPC    = "Typ 2".
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 3
            typtt.TYPC    = "Typ 3".           
      END.
      /*kalkfor*/
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "VAST" THEN 
      DO: 
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 5
            typtt.TYPC    = "Sam B/F".        
      END.
      IF Guru.Konstanter:varforetypval[41] = 1 THEN 
      DO:  
         CREATE typtt.
         ASSIGN 
            typtt.TYPKALK = 7
            typtt.TYPC    = "Nätreg N1".      
      END.
     
      /*
       cmbTyp:GuruCombo:GuruInitCombo(TEMP-TABLE typtt:DEFAULT-BUFFER-HANDLE, "TYPKALK", "TYPC").
       */
      THIS-OBJECT:HuvudTyp:GuruCombo:GuruInitCombo(cmbTyplist:ttBufferHandle, "TYPKALK", "TYPC").
      THIS-OBJECT:HuvudTyp:GuruCombo:VALUE = THIS-OBJECT:HuvudTT:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE.
   END METHOD.
   METHOD PRIVATE VOID Omrcmb ():
      DEFINE VARIABLE cmbOmrlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE omrK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE omrnamnK   AS Controls.GridRubrik      NO-UNDO.

      cmbOmrlist = NEW Controls.GridRubrikLista().
      cmbOmrlist:ttBufferHandle = THIS-OBJECT:OmradeTT.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  cmbOmrlist:filter = "omrtemp.KALKYLINT2 >= omrtemp.KALKYLSIST".
      omrK = NEW Controls.GridRubrik().
      omrK:Falt = "OMRADE".
      omrK:Rubrik = Guru.Konstanter:gomrk.
      cmbOmrlist:addRubrik(omrK).
      omrnamnK = NEW Controls.GridRubrik().
      omrnamnK:Falt = "NAMN".
      omrnamnK:Rubrik = "Namn".
      cmbOmrlist:addRubrik(omrnamnK).  

      THIS-OBJECT:HuvudOmrade:GuruInitCombo(cmbOmrlist, "OMRADE", "NAMN").

      THIS-OBJECT:HuvudOmrade:VALUE = THIS-OBJECT:HuvudTT:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.

   END METHOD.
   METHOD PRIVATE VOID katalogcmb ():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = THIS-OBJECT:KatalogTT.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Katalog".
      cmbkataloglist:addRubrik(katanamnK).
      /*             
      cmbKata:GuruInitCombo(cmbkataloglist, "KLOGID", "BENAMNING").
      */
      THIS-OBJECT:HuvudKatalog:GuruInitCombo(cmbkataloglist:ttBufferHandle, "KLOGID", "BENAMNING").           
      THIS-OBJECT:HuvudKatalog:VALUE = THIS-OBJECT:HuvudTT:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.

   END METHOD.
   
   METHOD PRIVATE VOID Utfcmb ():
      DEFINE VARIABLE villkor    AS CHARACTER                NO-UNDO.
      DEFINE VARIABLE cmbUtflist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE UtfK       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE UtfnamnK   AS Controls.GridRubrik      NO-UNDO.

      cmbUtflist = NEW Controls.GridRubrikLista().
      cmbUtflist:ttBufferHandle = THIS-OBJECT:AnvandareTT.
      cmbUtflist:filter = "anvandartemp.AV-lEVEL > 0".
      
      UtfK = NEW Controls.GridRubrik().
      UtfK:Falt = "ANVANDARE".
      UtfK:Rubrik = "Användare".
      cmbUtflist:addRubrik(UtfK).
      UtfnamnK = NEW Controls.GridRubrik().
      UtfnamnK:Falt = "AV-NAMN".
      UtfnamnK:Rubrik = "Namn".
      cmbUtflist:addRubrik(UtfnamnK).       
      THIS-OBJECT:HuvudUtfardare:GuruInitCombo(cmbUtflist, "ANVANDARE", "AV-NAMN").
      
      villkor = "WHERE ANVANDARE = '" + THIS-OBJECT:HuvudTT:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "'".                 
      THIS-OBJECT:AnvandareTT:FIND-FIRST(villkor). 
      IF THIS-OBJECT:AnvandareTT:BUFFER-FIELD("AV-LEVEL"):BUFFER-VALUE = 0 THEN 
      DO:
         villkor = "WHERE AV-LEVEL > 0".             
         THIS-OBJECT:AnvandareTT:FIND-FIRST(villkor).
      END.    
      THIS-OBJECT:HuvudUtfardare:VALUE = THIS-OBJECT:AnvandareTT:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      
   END METHOD.
   METHOD PRIVATE VOID visatabs (INPUT vilka AS CHARACTER, INPUT visavar AS LOGICAL ):
      
      IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:TEXT = vilka THEN DO:
   THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = FALSE.
   IF Guru.konstanter:kalk2sekvar[2] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkylhuvud"]:VISIBLE   = visavar.
END.   
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Kalkyl"]:TEXT = vilka THEN DO:
THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = FALSE.
IF Guru.konstanter:kalk2sekvar[4] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Kalkyl"]:VISIBLE   = visavar.
END.   
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Egna koder"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["Egna koder"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:TEXT = vilka THEN DO:
THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = FALSE.
IF Guru.konstanter:kalk2sekvar[6] = TRUE THEN THIS-OBJECT:TabManager:Tabs["Visa Kalkyl"]:VISIBLE   = visavar.
END.   
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["FaktorerPriser"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Materiel"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["Materiel"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["ImportMallar"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["ImportMallar"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:TEXT = vilka THEN DO:
THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = FALSE.
IF Guru.konstanter:kalk2sekvar[11] = TRUE THEN 
DO: 
   IF STRING(THIS-OBJECT:HuvudTyp:GuruCombo:VALUE) = "2" THEN 
   DO:
      IF Guru.konstanter:globforetag = "ELPA" OR Guru.konstanter:globforetag = "GRAN" OR  Guru.konstanter:globforetag = "LIMO" THEN 
      DO:  
         THIS-OBJECT:TabManager:Tabs["Avtalskalkyl"]:VISIBLE   = visavar.
      END.
   END.      
END.   
END.   
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Frikalkyl"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["Frikalkyl"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Fordelning"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["Fordelning"]:VISIBLE   = visavar.
IF vilka = "ALLA" OR THIS-OBJECT:TabManager:Tabs["Volymber"]:TEXT = vilka THEN  THIS-OBJECT:TabManager:Tabs["Volymber"]:VISIBLE   = visavar.
    
 
            
END METHOD.   

METHOD PUBLIC VOID SetVisningsBild(i AS INTEGER):
   /* 
      0 = inget genererat än
      1 = genererar
      2 = resultat
   */
   IF i EQ 0 THEN 
   DO:
      THIS-OBJECT:VisaPanelEmpty:Visible = TRUE.
      THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:FILL.
         
      THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
      THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
         
      THIS-OBJECT:VisaExcel:Visible = FALSE.
      THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
         
   END.
   IF i EQ 1 THEN 
   DO:
      THIS-OBJECT:VisaPanelWaiting:Visible = TRUE.
      THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:FILL.
         
      THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
      THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         
      THIS-OBJECT:VisaExcel:Visible = FALSE.
      THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:NONE.
   END.
   IF i EQ 2 THEN 
   DO:
      THIS-OBJECT:VisaExcel:Visible = TRUE.
      THIS-OBJECT:VisaExcel:Dock = System.Windows.Forms.DockStyle:FILL.
         
      THIS-OBJECT:VisaPanelEmpty:Visible = FALSE.
      THIS-OBJECT:VisaPanelEmpty:Dock = System.Windows.Forms.DockStyle:NONE.
         
      THIS-OBJECT:VisaPanelWaiting:Visible = FALSE.
      THIS-OBJECT:VisaPanelWaiting:Dock = System.Windows.Forms.DockStyle:NONE.
   END.
END METHOD.

METHOD PUBLIC VOID GenereraKalkylVisningE(s AS System.Object, e AS System.EventArgs):
   IF THIS-OBJECT:VisaExcel:Visible EQ TRUE THEN 
   DO:
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(45), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      IF res:ToString()  EQ "Yes" THEN 
      DO:
         THIS-OBJECT:GenereraKalkylVisning().   
      END.
   END.
   ELSE 
   DO:
      THIS-OBJECT:GenereraKalkylVisning().
   END.
END METHOD.

METHOD PUBLIC VOID GenereraKalkylVisning():

   /*  /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i slutet också! */
   DEFINE VARIABLE TimerTest AS System.Diagnostics.Stopwatch NO-UNDO.
   TimerTest = NEW System.Diagnostics.Stopwatch().
   TimerTest:Reset().
   TimerTest:Start().
   */
   THIS-OBJECT:SetVisningsBild(1).
   THIS-OBJECT:VisaExcel:ExcelAppHandle:ScreenUpdating = FALSE.
   THIS-OBJECT:VisaExcel:Reload().
   
   DEFINE VARIABLE link AS CHARACTER NO-UNDO.
   {LOGGORc.I}
   
   IF link NE ? THEN DO:
      MESSAGE System.IO.Path:GetFullPath(link) link
      VIEW-AS ALERT-BOX.
      THIS-OBJECT:VisaExcel:GetWorkSheet():PageSetup:LeftHeaderPicture:Filename = System.IO.Path:GetFullPath(link).
   END.
      
   /* Skapa huvud */
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A5"):Formula = "Grundkalkyl".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A5"):Font:Bold = TRUE.

    
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A6"):Formula = "Kalkyl-nr.: ".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "C6"):Formula = STRING(THIS-OBJECT:kalknrvar).

   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A7"):Formula = "Kalkyltyp: ".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "C7"):Formula = STRING(THIS-OBJECT:RibbonToolInfTyp:Text).
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A8"):Formula = "Område: ".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "C8"):Formula = STRING(THIS-OBJECT:omradevar).
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A6", "A8"):FONT:Bold = TRUE.
      
   /* --- Börja göra listan här --- */
      
   /* Vilken rad ska listan börja på? Rubriker inkluderat */
   DEFINE VARIABLE ListBegin  AS INTEGER   INITIAL 9 NO-UNDO. 
   DEFINE VARIABLE ListBeginC AS CHARACTER NO-UNDO.
   ListBeginC = STRING(ListBegin). 
      
   /* Skapa rubrikerna */
   /* Beredare Montör   Maskin   EA Arbete   Materiel Mask  Övrigt   Summa
*/
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A" + ListBeginC):Formula = "Kod".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "B" + ListBeginC):Formula = "Benämning".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "D" + ListBeginC):Formula = "Antal".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "E" + ListBeginC):Formula = "Enh".
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "F" + ListBeginC):Formula = "Beredare".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "G" + ListBeginC):Formula = "Montör".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "H" + ListBeginC):Formula = "Maskin".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "I" + ListBeginC):Formula = "EA".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "J" + ListBeginC):Formula = "Arbete".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "K" + ListBeginC):Formula = "Materiel".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "L" + ListBeginC):Formula = "Mask".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "M" + ListBeginC):Formula = "Övrigt".
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "N" + ListBeginC):Formula = "Summa".

   /* Gör rubriker bold */
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ ListBeginC, "N"+ ListBeginC):FONT:Bold = TRUE.

   /* Håll koll på dom faktiska raderna */
   DEFINE VARIABLE rowCounter AS INTEGER NO-UNDO.
   rowCounter = ListBegin + 1.
     
   THIS-OBJECT:Root:DatabaseManager:Kalkyl:CalculateSum().
   THIS-OBJECT:KalkSummaTT = THIS-OBJECT:Root:DatabaseManager:Kalkyl:KalkSummaTT.
      
   DEFINE VARIABLE qH AS HANDLE NO-UNDO.
      
   qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KalkSummaTT, "FOR EACH " + THIS-OBJECT:KalkSummaTT:TABLE + " NO-LOCK").
   qH:GET-FIRST().

   DO WHILE qH:QUERY-OFF-END = FALSE:
        
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("LOPNR"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "B" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "D" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("ANTAL"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "E" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("ENHET"):BUFFER-VALUE).
         
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "F" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("BTIMMAR"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "G" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("MTIMMAR"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "H" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("MASKTIMMAR"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "I" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("EAMANGD"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "J" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("AKOST"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "K" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("MTRLKOST"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "L" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("MASKKOST"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "M" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("OVRIGTKOST"):BUFFER-VALUE).
      Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "N" + STRING(rowCounter)):Formula = STRING(THIS-OBJECT:KalkSummaTT:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE).
         
      rowCounter= rowCounter + 1.
      qH:GET-NEXT(NO-LOCK).
   END.
      
   THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "B" + ListBeginC, "C" + STRING(rowCounter - 1) ):Merge( true ).
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "C" + STRING(rowCounter) ):ColumnWidth = 45.
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "D" + STRING(rowCounter) ):ColumnWidth = 10.
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "E" + STRING(rowCounter) ):ColumnWidth = 10.
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "N" + STRING(rowCounter - 1) ):BorderAround(XlLineStyle:xlContinuous, XlBorderWeight:xlMedium, XlColorIndex:xlColorIndexAutomatic, System.Reflection.Missing:Value).
      
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "N" + STRING(rowCounter - 1) ), XlBordersIndex:xlInsideHorizontal):LineStyle = XlLineStyle:xlDot.
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "N" + STRING(rowCounter - 1) ), XlBordersIndex:xlInsideHorizontal):Weight = XlBorderWeight:xlThin.
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "N" + STRING(rowCounter - 1) ), XlBordersIndex:xlInsideVertical):LineStyle = XlLineStyle:xlDot.
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "N" + STRING(rowCounter - 1) ), XlBordersIndex:xlInsideVertical):Weight = XlBorderWeight:xlThin.
      
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A"+ STRING(ListBegin + 1), "E" + STRING(rowCounter - 1) ), XlBordersIndex:xlEdgeRight):Weight = XlBorderWeight:xlMedium.
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "F"+ STRING(ListBegin + 1), "M" + STRING(rowCounter - 1) ), XlBordersIndex:xlEdgeRight):Weight = XlBorderWeight:xlMedium.
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "F"+ STRING(ListBegin), "N" + STRING(rowCounter - 1) ):HorizontalAlignment = XlHAlign:xlHAlignRight.
      
   Elpool.Helpers:GetBorder(Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A" + ListBeginC, "N" + ListBeginC), XlBordersIndex:xlEdgeBottom):LineStyle = XlLineStyle:xlDouble.
      
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A1", "N" + STRING(rowCounter)):FONT:Size = 18.
   Elpool.Helpers:GetRange(THIS-OBJECT:VisaExcel:GetWorkSheet(), "A1", "N" + STRING(rowCounter)):FONT:Name = "Garamond".

      
   THIS-OBJECT:Root:DatabaseManager:Global:CloseCustomQuery(qH).
   THIS-OBJECT:VisaExcel:ExcelAppHandle:ScreenUpdating = TRUE.
   THIS-OBJECT:VisaExcel:ExcelAppHandle:Visible = TRUE.
   THIS-OBJECT:VisaExcel:Visible = TRUE.
   THIS-OBJECT:VisaExcel:ExcelAppHandle:ActiveWorkbook:Activate().
   THIS-OBJECT:VisaExcel:Fix().

   THIS-OBJECT:SetVisningsBild(2).

/*  /* Avkommentera detta för att klocka genereringstiden - se till att avmarkera det i början också! */
MESSAGE TimerTest:ElapsedMilliseconds
VIEW-AS ALERT-BOX. */

END METHOD.


METHOD PUBLIC VOID ApplyLanguage():
    THIS-OBJECT:EgenSkapa:Text = THIS-OBJECT:Root:LanguageManager:GetString(41).
END METHOD.

DEFINE PRIVATE VARIABLE mallarkoder                 AS Controls.Grid                                                  NO-UNDO.
DEFINE PRIVATE VARIABLE mallar                      AS Controls.Grid                                                  NO-UNDO.
DEFINE PRIVATE VARIABLE Labelklgartal               AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE Labelartalskj               AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE LabelAvtal                  AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PUBLIC  VARIABLE Root                        AS Guru.Root                                                      NO-UNDO.


   
DEFINE PRIVATE VARIABLE Labelutf                    AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE Labelomr                    AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE Labelkatalog                AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE Labelanm                    AS Infragistics.Win.Misc.UltraLabel                               NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer1             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer5             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer8             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer7             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer6             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE valdamallar                 AS Controls.Grid                                                  NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTextEditor2            AS Infragistics.Win.UltraWinEditors.UltraTextEditor               NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage3 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTabPageControl17       AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTabPageControl16       AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
DEFINE PRIVATE VARIABLE ultraTabControl2            AS Infragistics.Win.UltraWinTabControl.UltraTabControl            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer4             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer3             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE splitContainer2             AS System.Windows.Forms.SplitContainer                            NO-UNDO.
DEFINE PRIVATE VARIABLE VisaPDFBrowser              AS System.Windows.Forms.WebBrowser                                NO-UNDO.
DEFINE PRIVATE VARIABLE VisaIEBrowser               AS System.Windows.Forms.WebBrowser                                NO-UNDO.
DEFINE PRIVATE VARIABLE VisaExcelTab                AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl        NO-UNDO.
DEFINE PRIVATE VARIABLE VisaExcel                   AS Controls.GuruExcelEmbed                                        NO-UNDO.

      
END CLASS.
